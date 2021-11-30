from random import randint, choice

ENCODED = ['ho', 'hu', 'ha', 'hi']


def encode_num(n, bits):
    # 0b010101
    #   hohaho
    encoded = ''
    while bits > 0:
        op = n & 0b11
        encoded = ENCODED[op] + encoded
        n >>= 2
        bits -= 2
    return encoded


def get_imm(imm, bits=8):
    if imm.startswith("0x"):
        imm = int(imm, 16)
    else:
        imm = int(imm)

    return encode_num(imm, bits)


def asm_push1(args):
    assert len(args) == 1
    arg = get_imm(args[0], 8)
    return arg


def asm_push8(args):
    assert len(args) == 1
    arg = get_imm(args[0], 64)
    return arg


def asm_add1(args):
    assert len(args) == 0
    return ''


def asm_add8(args):
    assert len(args) == 0
    return ''


def asm_div8(args):
    assert len(args) == 0
    return ''


def asm_mul8(args):
    assert len(args) == 0
    return ''


def asm_sub8(args):
    assert len(args) == 0
    return ''


def asm_sub1(args):
    assert len(args) == 0
    return ''


def asm_xor1(args):
    assert len(args) == 0
    return ''


def asm_xor8(args):
    assert len(args) == 0
    return ''


def asm_sym(args):
    assert len(args) == 0
    return ''


def asm_call(args):
    assert len(args) == 1
    return get_imm(args[0], 8)


def asm_load(args):
    assert len(args) == 0
    return ''


def asm_stor(args):
    assert len(args) == 0
    return ''


def asm_popsp(args):
    assert len(args) == 0
    return ''


def asm_popr0(args):
    assert len(args) == 0
    return ''


def asm_popr1(args):
    assert len(args) == 0
    return ''


def asm_popr2(args):
    assert len(args) == 0
    return ''


def asm_pushsp(args):
    assert len(args) == 0
    return ''


def asm_pushr0(args):
    assert len(args) == 0
    return ''


def asm_pushr1(args):
    assert len(args) == 0
    return ''


def asm_pushr2(args):
    assert len(args) == 0
    return ''


def asm_ret(args):
    assert len(args) == 0
    return ''


NULL = (0, 0)
OPCODES = [
    ("push1", asm_push1),
    ("push8", asm_push8),
    NULL, NULL, NULL,
    ("add1", asm_add1),
    NULL, NULL,
    ("div8", asm_div8),
    ("mul8", asm_mul8),
    NULL, NULL,
    ("add8", asm_add8),
    ("sub8", asm_sub8),
    ("sub1", asm_sub1),
    NULL,
    ("xor1", asm_xor1),
    ("xor8", asm_xor8),
    NULL, NULL,
    ("sym", asm_sym),
    ("call", asm_call),
    ("load", asm_load),
    ("stor", asm_stor),
    NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    ("popsp", asm_popsp),
    ("popr0", asm_popr0),
    ("popr1", asm_popr1),
    ("popr2", asm_popr2),
    NULL, NULL, NULL, NULL, NULL, NULL,
    ("pushsp", asm_pushsp),
    ("pushr0", asm_pushr0),
    ("pushr1", asm_pushr1),
    ("pushr2", asm_pushr2),
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL,
    ("ret", asm_ret),
]


def assemble(listing) -> bytes:
    lines = listing.split("\n")

    stream = ""

    for line in lines:
        # Strip comments
        comment = line.find(";")
        if comment >= 0:
            line = line[:comment]

        # Strip whitespace
        line = line.strip()
        if len(line) == 0:
            continue

        opidx = line.find(" ")
        opcode = line[:opidx].strip().lower() if opidx > 0 else line.lower()

        if opidx > 0:
            line = line[opidx:]
            operands = [o.strip() for o in line.split(",")]
        else:
            operands = []

        op = 0
        found = False
        for opname, handler in OPCODES:
            if opname == opcode:
                found = True
                break
            op += 1

        if not found:
            print(f'Unknown opcode: {opcode}')

        assert handler != 0

        stream += encode_num(op, 6) + handler(operands)

    return stream


def build_string(chars):
    """Build a string and place its address in r0"""
    out = (
        ';"{chars}"\n'
        'pushsp\n'
        'popr0\n'
    )
    for c in chars:
        out += f"push1 0x{ord(c):02X} ; '{c}'\n"

    out += 'push1 0x00\n'  # NULL terminate the string.
    return out


def o_add(b):
    """Split a value into an addition."""
    lhs = randint(0, b)
    rhs = b - lhs
    return lhs, rhs


def o_xor(b):
    """Split a value into a xoring operation."""
    k = randint(0x01, 0xFF)
    return b ^ k, k


def o_sub(b):
    """Split a value into a subtraction."""
    rhs = randint(0, b)
    lhs = rhs + b
    return lhs, rhs


def obfuscate(data, max=1):
    """Simple metamorphic string obfuscator."""
    if type(data) == str:
        data = bytes(data, encoding='ascii')
    ops = [('add1', o_add), ('sub1', o_sub), ('xor1', o_xor)]
    code = ''
    for b in data:
        passes = randint(1, max)
        if passes == 1:
            opname, fn = choice(ops)
            lhs, rhs = fn(b)
            code += f'push1 0x{rhs:02X}\npush1 0x{lhs:02X}\n{opname}\n'
        else:
            opname, fn = choice(ops)
            lhs, rhs = fn(b)
            code += obfuscate([rhs], passes - 1)
            code += obfuscate([lhs], passes - 1)

    return code


def build_challenge(passwd):
    passwd += '\0'
    strcmp = obfuscate('strcmp\0', 1)
    malloc =  obfuscate('malloc\0', 1)
    return f'''; Password validation routine
    ; r0 = user input

    ; Resolve malloc
    pushsp
    popr1
    {malloc}
    pushr1
    sym
    popr1 ; r1 = malloc

    ; Invoke malloc(len)
    push8 {len(passwd)}
    pushr1
    call 1
    popr2  ; r2 = dst buffer

    ; Build the password on the stack.
    {obfuscate(passwd)}

    ; Store password in buffer (will be stored backwards)
    push1 {len(passwd)}
    pushr2  ; r2 = dst buffer
    stor

    ; Load it back to flip its order
    push1 {len(passwd)}
    pushr2
    load

    ; Store it one last time so that it's in the same order as the user input.
    push1 {len(passwd)}
    pushr2  ; r2 = dst buffer
    stor

    ; Resolve strcmp
    pushsp
    popr1
    {strcmp}
    pushr1
    sym
    popr1 ; r1 = strcmp

    ; Call strcmp
    pushr2 ; arg2: expected password
    pushr0 ; arg1: user input
    pushr1 ; strcmp
    call 2

    popr0 ; r0 = comparison result
    ret
    '''


def main():
    print('PASSWORD CHECKER')
    print(assemble(build_challenge('CrinkleBell\n')))

    print('RCE')
    RCE = (  # Assemble payload to make the VM call system('/bin/bash')
        f"{build_string('system')}\n"  # r0 = &'system'
        "pushr0\nsym\npopr1\n"  # r1 = pSystem
        f"{build_string('/bin/bash')}"  # r0 = &'/bin/bash'
        'pushr0\n'
        'pushr1\n'
        'call 1\n'
    )
    print(assemble(RCE))
    return

    print(assemble('''

        ; printf
        push1 0x70
        push1 0x72
        push1 0x69
        push1 0x6e
        push1 0x74
        push1 0x66
        push1 0x00

        push8 15 ; 7 + 8 bytes for this value;
        pushsp
        sub8

        sym ;
        popr0 ; Store printf address in r0

        ; -----------

        ; FLAG ...
        push1 0x46
        push1 0x4c
        push1 0x41
        push1 0x47
        push1 0x2d
        push1 0x61
        push1 0x62
        push1 0x63
        push1 0x64
        push1 0x65
        push1 0x66

        push8 19 ;; 11 + 8 bytes for this value
        pushsp
        sub8 ; arg1: flag pointer

        pushr0 ; Put printf on stack
        call 0x1 ; Call with one argument
        ret'''))


if __name__ == '__main__':
    main()
