## C algorithm
* if   buffer has at least 2 bytes
  * if   first byte is ` `
    * if   second byte is ` `, continue +2, else fail
  * else parse both bytes as hex digits, continue +2
* else
  * if   buffer has 0 bytes
    * finalize
  * else buffer has 1 byte: assert ` `, finalize, else fail

We're assuming the out buffer can fit the maximum length.

I want the `finalize` action to be able to refill the buffer, so this can be
used 
