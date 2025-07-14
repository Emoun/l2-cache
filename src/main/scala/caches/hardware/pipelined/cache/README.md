## Cache request controller UART

The cache request controller accepts commands to execute serially over a UART.
The number of bits in a command must be a multiple of a byte size (i.e. 8 bits).
The length of the command depends on the configuration of the cache.
The command always contains the following fields `wdata` - write data (if any), `addr` - address to access, `coreid` - requesting cores ID, `reqid` - id of the request, and `rw` - a single bit value indicating if it is a read - 0 or write - 1.
For example, for a cache that returns a 32 bit line to the core, contains 16-bit addresses, has 4 cores, and has request ID width of 4, the command would look as follows:

```
wdata = 0xDEADBEEF
addr = 0x00CC // 255
coreidAndReqid = 0xE // reqid = 3, coreid = 2 
rw = 0x1 // write

command = 0x1E00CCDEADBEEF
```

Note, that if any of the input fields in the command are smaller or larger, the command would also get shorter or longer.
However, the length of the command changes based on byte alignment.
