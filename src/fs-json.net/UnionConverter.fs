(*-------------------------------------------------------------------------                                                      
Copyright (c) Paulmichael Blasucci.                                        
                                                                           
This source code is subject to terms and conditions of the Apache License, 
Version 2.0. A copy of the license can be found in the License.html file   
at the root of this distribution.                                          
                                                                           
By using this source code in any fashion, you are agreeing to be bound     
by the terms of the Apache License, Version 2.0.                           
                                                                           
You must not remove this notice, or any other, from this software.         
-------------------------------------------------------------------------*)
namespace Newtonsoft.Json.FSharp

open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System
open System.Reflection
open System.Text.RegularExpressions

/// <summary>
/// A JSON.NET converter which can serialize/deserialize a given F# union.
/// <para>
/// NOTE: This converter is not intended for F#'s option type. 
///       Please use OptionConverter instead.
/// </para>
/// </summary>
type UnionConverter<'u>() =
  inherit JsonConverter()

  let [<Literal>] FS_TAG = "Tag"

  let union     = typeof<'u>
  let unionCtor = FSharpValue.PreComputeUnionConstructor
  let isItem    = Regex(@"^Item(\d*)$",RegexOptions.Compiled)
  let getTag    = FSharpValue.PreComputeUnionTagReader(union)

  // metadata for the individual union cases
  let cases  = union |> FSharpType.GetUnionCases
  // metadata for the key/value pairs to be written to and read from JSON
  let props  = 
    // get the properties for the abstract base class for this union 
    let baseProps = union.GetProperties(BindingFlags.Public |||
                                        BindingFlags.Instance)
    let missing p' = baseProps |> Array.exists (fun p -> p' <> p)
    // for each union case, merge case-specific fields (as properties) 
    // with then properties of the abstract base class for this union
    cases 
      |> Array.map (fun u ->  u.GetFields() 
                                |> Array.filter missing // avoid duplicates
                                |> Array.append 
                                <| baseProps 
                                |> Array.sortWith (fun l r -> 
                                      // sort the properties such than
                                      // "Tag" comes before all others;
                                      // this ordering becomes important 
                                      // for guiding the deserialization
                                      // process (see ReadJson(...))
                                      let left,right = l.Name,r.Name
                                      if      left  = right  then  0
                                      elif    left  = FS_TAG then -1
                                      else (* left <> FS_TAG *)    1))
  // maps union-case tag to union-case constructor function
  let ctors  = cases 
              |> Array.map (fun u -> let ctr = unionCtor u in u.Tag,ctr)
              |> Map.ofSeq
  // compiler-generated sub-type names corresponding to union cases
  let names  = 
    // HACK: this name generation replies on an implementation detail of
    // this F# compiler; a future release could change it without warning!
    cases |> Array.map (fun c -> let nm = union.Name + "+" + c.Name
                                 union.FullName.Replace(union.Name,nm))
  // map union-case tag to metadata needed fro union-case constructor args
  let fields = cases 
                |> Seq.map (fun c -> c.Tag,c.GetFields())
                |> Map.ofSeq
   
  let tryGetField tag name = 
    fields.[tag] |> Array.tryFind (fun p -> p.Name = name)

  let prepValues tag convert =
    // strip out any values not needed to construct a union-case
    Seq.filter (fun (KeyValue(k,_)) -> isItem.IsMatch(k)) 
    // marshal each value to the appropriate data type based on metadata
    >> Seq.mapi (fun i (KeyValue(_,v:obj)) -> let t = fields.[tag].[i]
                                              coerceType t.PropertyType v)
    >> Array.ofSeq

  override __.CanRead  = true
  override __.CanWrite = true
  
  override __.CanConvert(vType) = 
    // check for base type and union-case type name
    (vType = union) || 
    (names |> Array.exists (fun n -> n = vType.FullName))
  
  override __.WriteJson(writer,value,serializer) = 
    match value with
    | null -> nullArg "value" // 'null' unions don't really make any sense!
    | data -> 
        writer.WriteStartObject()
        // emit "system" metadata, if necessary
        if serializer.IsTracking then 
          writer.WriteIndentity(serializer,value)
        
        props.[value |> getTag]
          // match name (from definition) with value (from instance)
          |> Array.map  (fun p -> p.Name,p.GetValue(value,null))
          |> Array.iter (fun (n,v) -> 
              // emit field name (from definition)
              writer.WritePropertyName(n)
              // emit value, or reference thereto, if necessary
              if v <> null && serializer.HasReference(v) 
                then writer.WriteReference(serializer,v)
                else serializer.Serialize(writer,v))
        
        writer.WriteEndObject()

  override __.ReadJson(reader,_,_,serializer) = 
    
    let decode,decode',advance,readName = makeHelpers reader serializer

    let readProperties () =
      
      let rec readProps pairs =
        match reader.TokenType with
        | JsonToken.EndObject -> pairs //no more pairs;return map
        | JsonToken.PropertyName ->
            // get the key of the next key/value pair
            let name = readName ()
            match name with
            //  for "system" metadata, process normally
            | JSON_ID 
            | JSON_REF ->  
                let value = decode()
                advance ()
                readProps (pairs |> Map.add name value)
            //  "Tag" indicates the start of union-specific pairs
            | FS_TAG -> 
                // read tag value
                let tag = decode() |> Convert.ToInt32
                advance()
                // add tag value to map, jump to union-specific processing
                readFields tag (pairs |> Map.add name (box tag))
            | _ -> reader |> invalidToken
        | _     -> reader |> invalidToken

      and readFields tag pairs =
        match reader.TokenType with
        | JsonToken.EndObject -> readProps pairs //no more pairs;return map
        | JsonToken.PropertyName ->
            // get the key of the next key/value pair
            let name = readName ()
            match tryGetField tag name with
            // pair IS needed for union-case constructor
            | Some(f) ->  // decode value using field type info
                          let value = decode' f.PropertyType
                          advance()
                          // add decoded pair to map, continue to next pair
                          readFields tag (pairs |> Map.add name value)
            // pair is NOT needed for union-case constructor
            | None    ->  advance()
                          // nothing to do, continue to next pair
                          readFields tag pairs
        | _ -> reader |> invalidToken

      advance ()
      readProps Map.empty

    match reader.TokenType with
    | JsonToken.StartObject ->
        // read all key/value pairs, reifying type info based on union tag
        match readProperties () with
        | Ref(trackingId) -> 
            // tuple value is a reference, de-reference to actual value
            serializer.GetReference(string trackingId)
        | Map(data) -> 
            // marshal to correct data types, based on union-case tag
            let tag = data.[FS_TAG] |> Convert.ToInt32
            let inputs = data |> prepValues tag serializer.Deserialize
            // create union-case instance
            let value = inputs |> ctors.[tag] 
            if serializer.IsTracking then 
              match data |> Map.tryFindKey (fun k _ -> k = JSON_ID) with
              // use existing "$id"
              | Some(k) -> serializer.AddReference(string data.[k],value)
              // make a new "$id"
              | None    -> serializer.MakeReference(value) |> ignore
            value
        | _ -> raise InvalidPropertySet
    | _ -> reader |> invalidToken
