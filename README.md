# bytezap
Build strict bytestrings with zero intermediate allocation.

You probably want [fumieval/mason](https://github.com/fumieval/mason), which is
much more flexible thanks to its buffered serialization design.

## Why?
Most binary serialization libraries tend towards a model where the serializer
itself handles allocation. In the plumbing, serialization operations are
bracketed by a check to ensure the current buffer has enough space for the next
operation; if not, we obtain more space in some way, and serialization
continues. This design is nice because we can _chunk_ the serializing:

* for writing to a lazy bytestring, we can emit a new chunk and clear our buffer
* for writing to a handle, we can write, flush and clear our buffer
* for writing to a strict bytestring, we must grow our current buffer

But if we know the size of the serialized data _before serializing it_, we don't
need those space checks, nor these intermediate steps. We may allocate a single
buffer with the required size upfront, then use that as we like.

This approach has advantages and disadvantages. The aim of bytezap is to find
out what exactly those are, and hopefully provide a performance boost for
serializing especially plain or small data.

## Why hasn't it been done already?
Most people performing data serialization in Haskell aren't usually interested
in cases where the final byte size is known upfront. It's too low level, too
specific, and too easy to get wrong. Better to give in and do the extra
bookkeeping every time, for a more general library, even if it does some wasted
work for certain operations.

## Pros
* Unnecessarily fast.
* Only one backend: serialize to bytestring. Since we don't chunk, we can't have
  specialized file handle or socket backends.
  * Slight fib: you may serialize to `ShortByteString` also.

## Cons
* Very limited: may only serialize types which we can know the serialized byte
  length of. This should be known statically, located in a field of the type, or
  cheap to compute.
* Lots of very unsafe code here that should scare you. (Certainly does me.)

## Problems
* Do we really have to write a bytestring every time? Isn't there some low-level
  write operation on handles that takes a `Ptr Word8 -> IO ()` and a length and
  does the write directly...?
  * I have a feeling there isn't, because it needs to be able to copy
    chunk-by-chunk. See `fdWrite`, which is almost the lowest level of handle
    copying.

## Non-features
### Serialize to `ByteString` (pinned byte arrays) only
No `ShortByteString`s, no writing directly to handles.

(One _may_ support writing to `ShortByteString`s (unpinned byte arrays) by doing
a bunch of class indirection. But it's a lot of extra work for a use case that I
don't see as very popular at all. Check the Git history for an early
implementation.)
