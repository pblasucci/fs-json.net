fs-json.net
======
----------------------------------------------------------------------------

### A library of tools to help F# and JSON.NET play nicely together

Thus far, fs-json.net contains two type converters:

  - **TupleConverter** ... can serialize/deserialize tuple values of any arity
  
  - **UnionConverter<'t>** ... can serialize/deserialize any values of the discriminated union given for 't

  - **OptionConverter** ... can serialize/deserialize option type instances *(NOTE: not fully tested yet)*

A complete example can be found in [Scratch.fsx](http://github.com/pblasucci/fs-json.net/blob/master/src/fs-json.net/Scratch.fsx), located along-side the source files

More information about JSON.NET can be found at: [http://json.codeplex.com](http://json.codeplex.com "JSON.NET")
                        
fs-json.net is distributed under the Apache License

----------------------------------------------------------------------------
###### Copyright &#169; Paulmichael Blasucci. All rights reserved.
