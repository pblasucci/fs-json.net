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
open System

type TupleConverter() =
  inherit JsonConverter()

  override __.CanRead  = true
  override __.CanWrite = true
  
  override __.CanConvert(vType) = vType |> FSharpType.IsTuple
  
  override __.WriteJson(writer,value,serializer) = 
    match value with
    | null -> nullArg "value" // a 'null' tuple doesn't make sense!
    | data -> 
        writer.WriteStartObject()
        
        let fields = value |> FSharpValue.GetTupleFields
        if fields.Length > 0 then
        
          if serializer.IsTracking then
            writer.WriteIndentity(serializer,value)
          
          fields |> Array.iteri (fun i v ->  
            let n = sprintf "Item%i" (i + 1)
            writer.WritePropertyName(n)
            if v <> null && serializer.HasReference(v)
              then writer.WriteReference(serializer,v)
              else serializer.Serialize(writer,v))
        
        writer.WriteEndObject()

  override __.ReadJson(reader,vType,_,serializer) = 
    
    let decode,decode',advance,readName = makeHelpers reader serializer

    let readProperties (fields:Type[]) =
      let rec readProps index pairs =
        match reader.TokenType with
        | JsonToken.EndObject     -> pairs // no more pairs, return map
        | JsonToken.PropertyName  ->
            // get the key of the next key/value pair
            let name = readName ()
            let value,index' =  match name with
                                //  for "system" metadata, process normally
                                | JSON_ID | JSON_REF -> decode (),index
                                //  for tuple data...
                                //    use type info for current field
                                //    bump offset to the next type info
                                | _ -> decode' fields.[index],index+1
            advance ()
            // add decoded key/value pair to map and continue to next pair
            readProps (index') (pairs |> Map.add name value)
        | _ -> reader |> invalidToken
      advance ()
      readProps 0 Map.empty

    match reader.TokenType with
    | JsonToken.StartObject ->
        let fields = vType |> FSharpType.GetTupleElements
        // read all key/value pairs, reifying with tuple field types
        match readProperties fields with
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
                // merge values with type info
                |> Seq.zip fields
                // marshal values to correct data types
                |> Seq.map (fun (t,v) -> v |> coerceType t)
                |> Seq.toArray
            // create tuple instance (and cache it if necessary)
            let value = FSharpValue.MakeTuple(inputs,vType)
            if serializer.IsTracking then 
              match data |> Map.tryFindKey (fun k _ -> k = JSON_ID) with
              // use existing "$id"
              | Some(k) -> serializer.AddReference(string data.[k],value)
              // make a new "$id"
              | None    -> serializer.MakeReference(value) |> ignore  
            value
        | _ -> raise InvalidPropertySet
    | _ -> reader |> invalidToken
