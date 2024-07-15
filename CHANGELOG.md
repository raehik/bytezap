## 1.3.1 (2024-07-15)
* fix building on GHC 9.8 and probably 9.10

## 1.3.0 (2024-04-13)
* move type-level byte stuff to another package

## 1.2.0 (2024-04-11)
* fix type-level generic blen type family (needed defunctionalization)
  * split out into its own even _more_ abstract library, generic-type-functions
* add missing instance `Integral (ByteOrdered end a)`
* add struct parser! design taken from flatparse. barebones combinators
  * add type-level byte parsing. hahahaha
* add `Bytezap.Poke.toStructPoke :: Poke s -> Struct.Poke s`

## 1.1.0 (2024-04-05)
* add struct serializer
* add type-level bytestring utilities (generalized from binrep)

## 1.0.0 (2024-03-17)
* initial release, finally its own package
