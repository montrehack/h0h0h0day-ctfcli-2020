#!/usr/bin/env python
# -*- encoding: utf-8 -*-

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
