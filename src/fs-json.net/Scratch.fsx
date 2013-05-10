(*-------------------------------------------------------------------------                                                      
Copyright (c) Paulmichael Blasucci.                                        
                                                                           
This source code is subject to terms and conditions of the Apache License, 
Version 2.0. A copy of the license can be found in the License.html file   
at the root of this distribution.                                          
                                                                           
By using this source code in any fashion, you are agreeing to be bound     
by the terms of the Apache License, Version 2.0.                           
                                                                           
You must not remove this notice, or any other, from this software.         
-------------------------------------------------------------------------*)
open System

#I @"..\..\packages\Newtonsoft.Json.5.0.5\lib\net40"
#r "Newtonsoft.Json.dll"
open Newtonsoft.Json

#load "Library.fs"
open Newtonsoft.Json.FSharp

#load "TupleConverter.fs"
open Newtonsoft.Json.FSharp

#load "UnionConverter.fs"
open Newtonsoft.Json.FSharp

#load "OptionConverter.fs"
open Newtonsoft.Json.FSharp

type Animal =
 | Dog    of name
 | Cat    of name * lives
 | Ferret of name * sex * years
 | Fish
and sex   = Female | Male
and lives = 
 | Nine  = 9 
 | Eight = 8
 | Seven = 7
 | Six   = 6
 | Five  = 5
 | Four  = 4
 | Three = 3
 | Two   = 2
 | One   = 1
and years = byte
and name  = string

//NOTE: custom equality is required by the test apparatus in this script
//      (i.e. not necessary when using fs-json.net)
[<CustomEquality; 
  NoComparison>]
type Person =
  { Name  : (string * string)
    DoB   : DateTime
    Pets  : ResizeArray<Animal> }

  override self.Equals(other) = 
    match other with
    | :? Person as other ->
        let {Person.Name=(fnm1,lnm1);DoB=dob1;Pets=pets1} = self
        let {Person.Name=(fnm2,lnm2);DoB=dob2;Pets=pets2} = other
        (fnm1 = fnm2) && (lnm1 = lnm2) && (dob1 = dob2) 
          && (Seq.forall2 (fun l r -> l = r) pets1 pets2)
    | _ -> false

  override self.GetHashCode() = 
    let {Person.Name=(fnm,lnm);DoB=dob;Pets=pets} = self
    (hash fnm) ^^^ (hash lnm) ^^^ (hash dob) ^^^ (hash pets)

//NOTE: custom equality is required by the test apparatus in this script
//      (i.e. not necessary when using fs-json.net)
[<CustomEquality;
  NoComparison>]
type Status<'t when 't : equality> =
  | Success of option<'t>
  | Warning of option<'t> * message
  | Failure of exn

  override self.Equals(other) = 
    match other with
    | :? Status<'t> as other ->
        match self,other with
        | Success(v1),Success(v2)       -> v1 = v2
        | Warning(v1,m1),Warning(v2,m2) -> v1 = v2 && m1 = m2
        | Failure(x1),Failure(x2)       -> x1.GetType() = x2.GetType()
        | _,_ -> false
    | _ -> false

  override self.GetHashCode() = 
    match self with
    | Success(v)    -> (hash "Success") ^^^ (hash v)
    | Warning(v,m)  -> (hash "Warning") ^^^ (hash v) ^^^ (hash m)
    | Failure(x)    -> (hash "Failure") ^^^ (hash x)

and message = string

let dt = System.DateTime.Parse
let converters : JsonConverter[] = [| TupleConverter()
                                      OptionConverter()
                                      UnionConverter<Animal>()
                                      UnionConverter<sex>()
                                      UnionConverter<Status<obj>>() |]

let settings = 
  JsonSerializerSettings(
    PreserveReferencesHandling  = PreserveReferencesHandling.All, 
    Converters                  = converters)

let toJSON v = 
  JsonConvert.SerializeObject(v,Formatting.Indented,converters)
let ofJSON (v) : 't = 
  JsonConvert.DeserializeObject<'t>(v,converters)

//NOTE: prime (') functions use JSON.NET with reference-tracking enabled
let toJSON' v = 
  JsonConvert.SerializeObject(v,Formatting.Indented,settings)
let ofJSON' (v) : 't = 
  JsonConvert.DeserializeObject<'t>(v,settings)

//NOTE: non-structurally-comparable types may require custom equality
//      for `runTest` to report 'ctrl = expr' (e.x. testing an Exception)
let runTest ``to`` ``of`` (v:'a) =
  printfn "ctrl: %A" v
  let j = v |> ``to`` // JSON
  printfn "json: %s" j
  let v' : 'a = j |> ``of`` // JSON
  printfn "expr: %A" v'
  let eq = 
    match (box v) with
    | :? seq<obj> as v1 -> 
      let v2 = (box v') :?> seq<obj> // HACK: !!! unsafe cast !!!
      if Seq.forall2 (fun a b -> a = b) v1 v2 then "=" else "<>"
    | _ -> if v = v' then "=" else "<>"
  printfn "ctrl %s expr" eq

let test  v = runTest toJSON  ofJSON  v
//NOTE: prime (') functions use JSON.NET with reference-tracking enabled
let test' v = runTest toJSON' ofJSON' v

(* ===================================================================== *)

let simplePair = ("Paul",32)
test simplePair

(* --------------------------------------------------------------------- *)

let john = { Name = ("john","smith")
             DoB  = dt "1977-05-05"
             Pets = ResizeArray() }
test john

(* --------------------------------------------------------------------- *)

let apollo      = Dog("Apollo")
let brooke      = Cat("Brooke",lives.Seven)
let hannah      = Ferret("Hannah",Female,4uy)
let kiki        = Ferret("Chiara",Female,4uy)
let huck        = Ferret("Huckleberry",Male,1uy)
let brandypaul  = Fish
let menagerie   = ResizeArray([ brandypaul
                                apollo
                                huck
                                hannah
                                brooke
                                kiki ])
test  menagerie
test' menagerie

(* --------------------------------------------------------------------- *)

let jane = { Name = ("jane","smith")
             DoB  = dt "1978-09-01"
             Pets = ResizeArray([hannah;kiki;huck;]) }
test  jane
test' jane

(* --------------------------------------------------------------------- *)

let series1 = ResizeArray([ (john,jane)
                            (john,jane)
                            (jane,john)
                            (jane,john) ])
let pair1,pair2 = (john,jane),(jane,john)
let series2 = ResizeArray([ pair1;pair1;pair2;pair2; ])
test' series1
test' series2

(* --------------------------------------------------------------------- *)

let x,y = Some(3L),Option<int64>.None
test  (x,y)
test' (x,y)

let good  = Success(Some(box 98.6))
let bad   = Warning(None,"Nothing to see here.")
let ugly  = Failure(Exception("Something Wicked"))
let perps = ResizeArray([ good;bad;ugly ])
test  perps
test' perps
