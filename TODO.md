## 2024-03-08
OK, so here's the deal:

* my original implementation is based on unaligned reads
* to support serializing to SBSs I need to use MutableByteArray primops, which
  don't allow unaligned reads (it's just not possible)
* those offsets in read/writeX primops? yeah they're dependent on what type
  you're read/writing, not just a byte count. enforced alignment :(

I _can_ do an efficient unaligned implementation for BSs using `Addr#`.
If I can write an aligned implementation, I can use it for both.
The type would something like this:

```haskell
type Poke# s (a :: TYPE levity) = a -> Int# -> State# s -> (# State# s, Int# #)
```

where `a` is instantiated as `Addr#` or `MutableByteArray#`.
I'd need to do some alignment bookkeeping, not sure how.
`mason`... does none. It will break on platforms without unaligned reads.
`store` does something I swear. Oh, `bytestring` itself does a bunch-- see e.g.
`Data.ByteString.Builder.Prim.Internal.storableToF`.
