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

//TODO: add caching

/// A JSON.NET converter which can serialize/deserialize F# option types.
type OptionConverter() =
  inherit JsonConverter()
  
  let [<Literal>] FS_VALUE = "Value"
 
  let option = typedefof<option<_>>
  
  // makes it easier to determine IsSome/IsNone reflectively
  let readTag = FSharpValue.PreComputeUnionTagReader(option)

  let (|Option|) t = 
    let cases = FSharpType.GetUnionCases(t)
    Option(cases.[0],cases.[1])
  
  let (|IsNone|IsSome|) o =
    let (Option(none,some)) = option
    if (readTag o) = some.Tag 
      then  let _,values = FSharpValue.GetUnionFields(o,o.GetType())
            IsSome (values.[0])
      else  IsNone
  
  override __.CanRead  = true
  override __.CanWrite = true
  
  override __.CanConvert(vType) = 
    let vType = if vType.IsGenericType 
                  then  vType.GetGenericTypeDefinition()
                  else  vType
    (vType = option)

  override __.WriteJson(writer,value,serializer) =
    match value with
    | IsNone    ->  writer.WriteNull() 
                    //TODO: investigate the relevance 
                    //      of JSON.NET's NullHandling options
    | IsSome(v) ->  writer.WriteStartObject()
              
                    // emit "system" metadata, if necessary
                    if serializer.IsTracking then 
                      writer.WriteIndentity(serializer,value)
              
                    writer.WritePropertyName(FS_VALUE)
                    // emit value, or reference thereto, if necessary
                    if serializer.HasReference(v) 
                      then  writer.WriteReference(serializer,v)
                      else  serializer.Serialize(writer,v)
              
                    writer.WriteEndObject()
    
  override __.ReadJson(reader,vType,_,serializer) = 
    let decode,decode',advance,readName = makeHelpers reader serializer
    
    // type of actual data "wrapped" by option type
    let innerType = vType.GetGenericArguments() |> Seq.head
    //NOTE: Reflection requires fully-reified generic types 
    //      in order to create late-bound union case instances
    let (Option(none,some)) = option.MakeGenericType([| innerType |])

    let readProperties () =
      let rec readProps pairs =
        match reader.TokenType with
        | JsonToken.EndObject     -> pairs // no more pairs, return map
        | JsonToken.PropertyName  ->
            // get the key of the next key/value pair
            let name  = readName ()
            let value = match name with
                        // for "system" metadata, process normally
                        | JSON_ID 
                        | JSON_REF -> decode()
                        // "Value" indicates option-type pair
                        | FS_VALUE -> decode' innerType
                        | _ -> reader |> invalidToken
            advance ()
            // add decoded key/value pair to map and continue to next pair
            readProps (pairs |> Map.add name value)
        | _ -> reader |> invalidToken
      advance ()
      readProps Map.empty

    match reader.TokenType with
    | JsonToken.Null        -> FSharpValue.MakeUnion(none,null)
    | JsonToken.StartObject ->
        // read all key/value pairs, reifying with tuple field types
        match readProperties() with
        | Ref(trackingId) -> 
            // tuple value is a reference, de-reference to actual value
            serializer.GetReference(string trackingId)
        | Map(data) -> 
            let inputs =
              data
                // strip away "system" meta data
                |> Seq.filter (fun (KeyValue(k,_)) -> k <> JSON_ID)
                // discard keys, retain values
                |> Seq.map (fun (KeyValue(_,v)) -> v)
                // marshal values to correct data types
                |> Seq.map (coerceType innerType)
                |> Seq.toArray
            // create option instance
            let value = FSharpValue.MakeUnion(some,inputs)
            if serializer.IsTracking then 
              match data |> Map.tryFindKey (fun k _ -> k = JSON_ID) with
              // use existing "$id"
              | Some(k) -> serializer.AddReference(string data.[k],value)
              // make a new "$id"
              | None    -> serializer.MakeReference(value) |> ignore  
            value
        | _ -> raise InvalidPropertySet
    | _ -> reader |> invalidToken