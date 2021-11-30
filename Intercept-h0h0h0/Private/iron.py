#!/usr/bin/env python3
import sys
import struct

# USAGE:
# iron e /path/to/uncompressed/file /path/to/compressed/file.iron key [override unit size]
# iron d /path/to/compressed/file.iron /path/to/uncompressed/file key

from pprint import pprint

class Iron:
    def get_format(value):
        formats = {
            1: 'B',
            2: 'H',
            4: 'I'
        }

        size = 1
        while value >= 2 ** (size * 8):
            size *= 2

        return (size, formats[size])

    def encode(input_buf, key, size=0):
        positions = {}
        for i, char in enumerate(input_buf):
            if char not in positions.keys():
                positions[char] = []
            positions[char].append(i)

        size_format = Iron.get_format(max(len(input_buf), 2 ** (size * 8) - 1))
        output_buf = b'IRON' + struct.pack('>B' + size_format[1], size_format[0], len(input_buf))

        for char, char_positions in positions.items():
            struct_format = '>' + size_format[1] + 'B' + (size_format[1] * len(char_positions))

            encrypted_values = [x ^ key for x in [struct.calcsize(struct_format), char] + char_positions]
            output_buf += struct.pack(struct_format, *encrypted_values)

        return output_buf

    def decode(input_buf, key):
        size_format = Iron.get_format(2 ** (input_buf[4] * 8) - 1)

        output_buf = bytearray(struct.unpack('>' + size_format[1], input_buf[5:][:size_format[0]])[0])

        i = 5 + size_format[0]
        while i < len(input_buf):
            j = size_format[0] + 1
            while j < struct.unpack('>' + size_format[1], input_buf[i:][:size_format[0]])[0] ^ key:
                position = struct.unpack('>' + size_format[1], input_buf[i + j:][:size_format[0]])[0] ^ key
                output_buf[position] = input_buf[i + size_format[0]] ^ key
                j += size_format[0]
            i += struct.unpack('>' + size_format[1], input_buf[i:][:size_format[0]])[0] ^ key

        return output_buf

if __name__ == '__main__':
    if len(sys.argv) < 5:
        exit(1)

    with open(sys.argv[2], "rb") as f:
        input_buf = f.read()

    output_buf = b''

    if sys.argv[1] == 'e':
        size = int(sys.argv[5]) if len(sys.argv) == 6 else 0
        print(size)
        output_buf = Iron.encode(input_buf, int(sys.argv[4]), size)
    elif sys.argv[1] == 'd':
        output_buf = Iron.decode(input_buf, int(sys.argv[4]))
    else:
        exit(1)

    with open(sys.argv[3], "wb+") as f:
        f.write(output_buf)
