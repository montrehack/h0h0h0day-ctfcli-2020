# G0h0h0pher

We have a statically linked 64bit ELF file. Looking through the program's strings, we have pretty good hint that it was compiled from a Golang program: `runtime.GOROOT`, `cgocheckdeadlockg`, ...

`main.checkPass` is a red herring, it XOR's the entered password with random bytes and prints it. This means that we want to get to `main.deriveKey`, so our password should be of length `2^5 (32)`. There is one useful bit of information in there though: a hint that gives us the payload's entry point: `0x464820`. Knowing that it was compiled identically as the dropper, we can use [Wikipedia's wonderful ELF header table](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#File_header) to figure out that the plaintext file header should be:

```
00000000: 7f45 4c46 0201 0100 0000 0000 0000 0000  .ELF............
00000010: 0200 3e00 0100 0000 2048 4600 0000 0000  ..>..... HF.....  # e_entry is from 0x18 to 0x20
00000020: 4000 0000 0000 0000 c801 0000 0000 0000  @...............
00000030: 0000 0000 4000 3800 0700 4000 1900 0300  ....@.8...@.....
```

`main.deriveKey` as the name suggests, uses a bunch of operations to derive the key from the entered password. Then, this key is passed to `main.decryptRsrc` which uses it as a repeating XOR key to decrypt a large buffer and write it to `./decrypted`. This buffer is our encrypted payload:

```
2d\x21\x0e\x2f\x63\x18\x2c\x11\x34\x51\x61\x69\x59\x73\x5d\x38\x31\x17\x5c\x66\x67\x6e\x47\x36\x4e\x75\x72\x6c\x30\x47\x37\x07...
```

At this point, we have both the plaintext and ciphertext. By the properties of XOR, if we xor them together, we get the key. Since the key is 32 bytes long, we only need the first 32bytes of each

```
7f454c4602010100000000000000000002003e00010000002048460000000000 #Plaintext
XOR
2d210e2f63182c113451616959735d3831175c66676e47364e75726c30473707 #Ciphertext
=
5264426961192d113451616959735d3833176266666e47366e3d346c30473707 #XOR key
```

From this key, we now have to work backwards through `main.deriveKey` to derive the original password. All the operations in that function are reversible, so we can write a script to do that.

```python
import binascii

key = binascii.unhexlify("5264426961192d113451616959735d3833176266666e47366e3d346c30473707")

passwd = [0] * 32 # Initialize array so we can index directly into it

# Start with the outright copies from one to another
for i in range(3):
    passwd[27+i] = key[1+i]
for i in range(4):
	passwd[14+i] = key[29-i]
passwd[26] =  key[16]
for i in range(7):
    if i % 2 == 0:
        val = key[24-i]
    else:
        val = key[18+i]
    passwd[i+18] = val ^ i
passwd[12] = key[30]
# Then the simple operations involving only one character from passwd
passwd[13] = key[0] ^ key[31] ^ key[25]
passwd[31] = key[0] ^ 33
passwd[0] = key[4] ^ 0x22
passwd[1] = key[5] ^ passwd[0]
passwd[2] = key[6] ^ passwd[1]
passwd[3] = key[7] ^ passwd[2]
for i in range(8):
    passwd[4+i] = key[i+8] - i
# And the rest
passwd[25] = key[17] ^ passwd[31]
passwd[30] = passwd[29] ^ key[31]

print("".join([chr(c) for c in passwd]))
```

## Flag

This script gives us the flag: **CZwf4P_fUnW17hG0l4ngEmb3dd3dBins**

If we give it as an argument to the executable, it drops another valid ELF file which prints an ASCII art holidays gopher.
