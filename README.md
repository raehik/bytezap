# bytezap
Build known-length strict bytestrings with zero intermediate allocation.

## Design
Given an `Addr#` pointing to somewhere in some live pinned memory, we can
perform a 
The base serializer is a plain function holds an `Addr#` pointing somewhere in a pinned
`ByteString`


```haskell
newtype Poke s = Poke { unPoke :: Addr# -> State# s -> (# State# s, Addr# #) }
```

## Why?
Most binary serialization libraries tend towards a model where the serializer
itself handles allocation. In the plumbing, most serialization operations are
bracketed by a check to ensure the current buffer has enough space for the next
operation; if not, a new buffer is allocated and serialization continues. (I
sometimes refer to this as *buffered serialization*.)

This design is effective with lazy bytestrings, where old buffers may be
appended to the bytestring being built. For strict bytestrings, the old buffer
must be copied over to the new one. (Or you create a lazy bytestring and squash
it into a strict one afterwards, which is why many libraries only export a lazy
serializer.)

If we know the size of the serialized data before actually serializing it, we
may allocate a single buffer with the required size upfront. This removes lots
of the plumbing that otherwise goes on during buffered serialization. I tend to
call this *unbuffered serialization*.

This approach has advantages and disadvantages. The aim of bytezap is to find
out what exactly those are, and hopefully provide a performance boost for
serializing especially plain or small data.

## Why hasn't it been done already?
Most people performing data serialization in Haskell aren't usually interested
in cases where the final byte size is known upfront. It's too low level, too
specific, and too easy to get wrong.

Importantly, there is *no possible safe interface* for the basic poking.
I provide a separate, safer serializer which tracks its size, which is slightly
more general.

## Why the name?
We serialize with upfront allocation, which I sometimes think of as "static"
serialization. static -> electricity -> zap, which has a useful double meaning:
I think of bytezap as "zapping" your data into bytes, in a flash.

## Other bits
### Serializing to `ShortByteString`s
`ByteString`s are pinned, meaning GHC's garbage collector never moves them.
Given an `Addr#` that points to the contents stored by a `ByteString`, we can
safely increment this `Addr#` to get to the next byte.

`ShortByteString`s are not pinned. We can't grab its `Addr#` and increment it
willy nilly-- it may get moved in the meantime, and now our base address is
wrong. Instead, we need to store a constant `MutableByteArray#` and an `Int#`
offset. This means extra arithmetic operations, so nope :(

TODO actually, whether it means extra arithmetic depends on how `writeXOffAddr# 0` works. does it still do +0?

```haskell
type Poke# s = MutableByteArray# -> Int# -> State# -> (# State# s, Int# #)
```

WAIT DW ALL BETTER NOW. However, we can't make an "either one" serializer,
unless we want to box the address type. Or maybe, we can do it via some
judicious typeclass application...?
