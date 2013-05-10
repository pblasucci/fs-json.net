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

open Newtonsoft.Json
open System

exception UnexpectedToken of JsonToken
exception InvalidPropertySet

[<AutoOpen>]
module internal Library =

  // in the JSON.NET library, "system" metadata names begin with a '$'
  let [<Literal>] JSON_ID   = "$id"   // uniquely identifies complex value
  let [<Literal>] JSON_REF  = "$ref"  // refers to unique identifier
    
  /// defines extensions which simplify using the JSON.NET serializer
  type Newtonsoft.Json.JsonSerializer with
    
    member self.IsTracking =
      self.PreserveReferencesHandling
          .HasFlag(PreserveReferencesHandling.Objects)
    
    member self.AddReference(name,value) =
        self.ReferenceResolver.AddReference(self,name,value)

    member self.HasReference(value) =
        self.ReferenceResolver.IsReferenced(self,value)

    member self.MakeReference(value) =
        self.ReferenceResolver.GetReference(self,value)

    member self.GetReference(name) =
        self.ReferenceResolver.ResolveReference(self,name)
    
  /// defines extensions which simplify using the JSON.NET writer
  type Newtonsoft.Json.JsonWriter with

    member self.WriteIndentity(serializer:JsonSerializer,value) =
      // { "$id" : <an-identity> }
      if serializer.IsTracking then
        let identity = serializer.MakeReference(value)
        self.WritePropertyName(JSON_ID)
        self.WriteValue(identity)

    member self.WriteReference(serializer:JsonSerializer,value) =
      // { "$ref" : <an-identity> }
      if serializer.HasReference(value) then  
        let identity = serializer.MakeReference(value)
        self.WriteStartObject()
        self.WritePropertyName(JSON_REF)
        self.WriteValue(identity)
        self.WriteEndObject()
  
  //  analyzes a key/value collection to determine its purpose...
  //    a 'Ref' contains a single key/value pair with a of key "$ref"
  //    a 'Map' has one or more key/value pairs, but NOT a "$ref" pair
  //    'Empty' contains no key/value pairs
  //    'Invalid' has a "$ref" pair AND one or more other pairs
  let (|Ref|Map|Empty|Invalid|) data = 
    match data |> Map.tryFindKey (fun k _ -> k = JSON_REF) with
    | Some(k) -> if data.Count = 1 then Ref(data.[k]) else Invalid
    | None    -> if data.Count > 0 then Map(data)     else Empty

  // since JSON only defines a single numeric data type, and JSON.NET
  // forces all numeric data to be either Int64 or Double, this function
  // attempts to coerce said Int64s and/or Doubles into the appropriate
  // integral, floating point, or enum value, based on supplied definition
  let coerceType (vType:Type) (value:obj) =
    if   vType.IsEnum            then Enum.ToObject (vType,value)
    elif vType = typeof<byte>    then Convert.ToByte    value |> box
    elif vType = typeof<uint16>  then Convert.ToUInt16  value |> box
    elif vType = typeof<uint32>  then Convert.ToUInt32  value |> box
    elif vType = typeof<sbyte>   then Convert.ToSByte   value |> box
    elif vType = typeof<int16>   then Convert.ToInt16   value |> box
    elif vType = typeof<int32>   then Convert.ToInt32   value |> box
    elif vType = typeof<float32> then Convert.ToSingle  value |> box
    elif vType = typeof<decimal> then Convert.ToDecimal value |> box
    else value // no type coercion required

  // simplifies raising UnexpectedToken exceptions
  let invalidToken (r:JsonReader) = raise <| UnexpectedToken(r.TokenType)

  //  given a JsonReader and a JsonSerializer, this function produces
  //  several helper functions which simplify working with the given
  //  reader and serializer. 
  let makeHelpers (reader:JsonReader) (serializer:JsonSerializer) = 
    let decode () = serializer.Deserialize(reader)
    let decode' t = serializer.Deserialize(reader,t)
    let advance = reader.Read >> ignore
    let readName () = let n = reader.Value |> string in advance (); n
    decode,decode',advance,readName
