# bytezap
Build bytestrings with zero intermediate allocation.

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
