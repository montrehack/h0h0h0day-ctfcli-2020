#include "factory.h"
#include <string.h>

#ifdef DEBUG
#define P printf
#else
#define P(s, ...) // Strip
#endif

#define STACKSZ 0x1000
#define assert(cond, msg) if (!(cond)) {        \
    printf(msg); \
    P("---\nCPU:\n  pc=%p\n  sp=%p\n  bp=%p\n  r0=%p\n  r1=%p\n  r2=%p\n---\n", \
      vm->pc, vm->sp, vm->stack, vm->r0, vm->r1, vm->r2);                      \
    exit(2); \
    }


#define psh_chk(vm, req) assert(vm->sp + req < (vm->stack + STACKSZ), "Oh no, an ELF smashed your gift!\n")
#define pop_chk(vm, req) assert(((uint64_t)vm->sp - (uint64_t)vm->stack) >= req, "Oh no, an ELF smashed your gift!\n")

void* libc;
vm_t vm;

char* flage = "FLAG-StringsWontSaveYouThisTime";
char validate_passwd[] = "Haha! huha? hohu? HoHoHo, huhohohohohohohaha. Hihu...hohiha-- hohoho, hohahuhoho. Hoho, haho, huhu,hohihahoho. Hohuho, hihuho! Hohohahihahu; ho... hiha! hohoho hihi hi hi.\nHohoho, hahuho, hihuho. . . hohohohoho!!! haho? Hahohoho. hahu. hohu. hohi. ha! hohoho, huhaho, huhoho, hohohoho ha! hohuhu, hohoho ...\n... hohohohohohohohohohohoho ...\nhi... hahaha.\nhi? huhu. Ho... ha... ho... hu! hohohu, hohohohohohohohohohohohohohohohohohohohohohohohohohohohohoho,\nhihuha. hahihu, huhu hoho!  hohu haho hahohoho huhuho huhohoho hohuho hahu.\n\nhohoho! hohoho! huhohu! hohoho!\nhahoho hiho!\nhihaho,hohoho,hihahuhoho!\nhohohihoho,ho hu hu hohohohu, hohaha hohoho hohahu hohohuhuho hohohohi, hihuhohoho, hohahihahohu?huhoho! hohihohuhi... hohoho! hahaha! hihu. hohoho! hohoho! ho! huhu! hohoho! huhahoho...\n\nhohuhu & hohoho, hohihi: hu, hohoho, huhi hihi hohiha hohoho! Huhu, hahuhohoho, hohohi, hohohuhu, hoho hoho hoho huho hoho huhahi huhu hohohohohoho\n... ...hu...\n\nHohu, hoho hohu hihihu ho hi ha hohoho hihi ha huho hoho hihi :) hohi, hu ho ho ho hohohohohohohohohohohohohoho -- huhu -- hohohoho.\n\nhohihuha, hihohu huhihohohoho hohihuha hiho huhu ha, hohohohoho, hihu:\n\n    * ha,\n    * hi,\n    * ho,\n    * hu,\n\nhu hi ha ha hu ha ho hu ho ho ho ho ha ha ha ho ho ho hu hu ha hu hu ho ho ho ho ho hu \nho ho hu ho ho ho ha hi hu hu ho hi ha ho ho ho ho ha hu ho ho ho ho hu hu hu ha hu ho \nho ho ho ho ho hi hu hi ho ho ho ha hu ha ha ho hi ha ho ho ho hu ho hi ha ho ho ho ho \nha ho hi hu ho ho ho ho ho hu hu hu hu ho ho ho ho hu ha hi ho hu hu ho ho ho ho ho ho \nho ho ho ho ho ho ho ho ho hi ha ha ha hi hu hu ho ha ho hu ha hi ho ha ha ha ha ha hi \nhu hu hu ho ho ho ha ha ho ho \n\n... hihihi !!!\n\n:)";
char* flag = "FLAG-ZUHrTeZLIBbsKbgMv6BphauGxoUdXYBH";

inline uint64_t parse_symbol(vm_t* vm)
{
    assert(vm->pc >= vm->code && vm->pc <= (vm->code + vm->len), "Huhuhu! What a bad ELF!\n");
    assert(*vm->pc, "Hoho! The factory is closed today!\n");

    // Skip all characters until the next encoded byte (H<c> or h<c>).
    // A NULL byte indicates the end of the instruction stream.
    char op;
    while ((op = *(vm->pc++)) != '\0')
    {
        if (op != 'h' && op != 'H') continue;
        op = *vm->pc;
        break;
    }

    // P("parse_symbol> %02x (%c)\n", op, op);

    switch (op)
    {
    case 'o': return 0;
    case 'u': return 1;
    case 'a': return 2;
    case 'i': return 3;
    default:  assert(0, "Ho...hu? You speak in weird tongues...\n");
    }
}

inline uint64_t parse_int(vm_t* vm, size_t bits)
{
    assert(bits > 1 && bits <= 64, "Huuuu... No ELF is available right now.\n");

    uint64_t parsed = 0;
    size_t n = bits / 2;
    while (n--)
    {
        parsed |= parse_symbol(vm);
        // P("parsed: %llx\n", parsed);
        if (n) parsed <<= 2;
    }

    // P("parse_int> %0llx\n", parsed);
    return parsed;
}

// Helper function to pop N bytes from the stack.
inline uint64_t z_pop(vm_t* vm, size_t n)
{
    assert(n > 0 && n <= 8, "Bad ELF.\n");
    pop_chk(vm, n);
    uint64_t popped = 0;
    P("z_pop>\n");
    while (n--)
    {
        vm->sp--;
        uint8_t b = (uint8_t) *vm->sp;
        popped |= b;
        if (n) popped <<= 8;
    }
    P("z_pop> %llx\n", popped);
    return popped;
}

inline void z_push(vm_t* vm, uint64_t val, size_t n)
{

    P("z_push> %llx\n", val);
    assert(n > 0 && n <= 8, "Bad ELF.\n");
    psh_chk(vm, n);
    while (n--)
    {
        uint8_t b = val & 0xFF;
        *vm->sp++ = b;
        val >>= 8;
    }
}

// Push a 64 bit literal on the stack.
int vm_push8(vm_t* vm)
{
    uint64_t n = parse_int(vm, 64);
    z_push(vm, n, 8);
    return 0;
}

// Push a 8 bit literal on the stack.
int vm_push1(vm_t* vm)
{
    P("push1\n");
    uint64_t n = parse_int(vm, 8);
    z_push(vm, n, 1);
    return 0;
}

// Push a 64bit 0-constant on the stack.
int vm_push0(vm_t* vm)
{
    P("push0\n");
    z_push(vm, 0, 8);
    return 0;
}

#define pushop(reg) int vm_push##reg(vm_t* vm) \
{ \
    z_push(vm, (uint64_t)vm->reg, 8);                     \
    return 0; \
}

pushop(r0);
pushop(r1);
pushop(r2);
pushop(sp);


#define popop(reg) int vm_pop##reg(vm_t* vm) \
{ \
    vm->reg = z_pop(vm, 8);           \
    P("" #reg " = %0llx\n", vm->reg);         \
    return 0; \
}

popop(r0);
popop(r1);
popop(r2);

// Have to deal with popsp explicitly to avoid cast warning 2 error.
int vm_popsp(vm_t* vm)
{
    vm->sp = (char *) z_pop(vm, 8);
    P("popsp> %0llx\n", vm->sp);
    return 0;
}


#define binop(name, S, sz) int vm_##name##sz(vm_t* vm)   \
{ \
    uint64_t lhs = z_pop(vm, sz); \
    uint64_t rhs = z_pop(vm, sz); \
    uint64_t res = lhs S rhs; \
    P("%llx " #S " %llx = %llx\n", lhs, rhs, res); \
    z_push(vm, res, sz); \
    return 0; \
}

binop(add, +, 1);
binop(add, +, 8);
binop(sub, -, 1);
binop(sub, -, 8);
binop(xor, ^, 1);
binop(xor, ^, 8);

binop(and, &, 1);
binop(and, &, 8);

binop(div, /, 8);
binop(mul, *, 8);

int vm_sym(vm_t* vm)
{
    char* buf = (char*)z_pop(vm, 8);
    assert(buf != NULL, "Your gift was not found...\n");
    void* fn = dlsym(libc, buf);
    P("sym> %s @ %p\n", buf, fn);
    z_push(vm, (uint64_t)fn, 8);
    return 0;
}

typedef void*(*fn0)(void);
typedef void*(*fn1)(void*);
typedef void*(*fn2)(void*, void*);
typedef void*(*fn3)(void*, void*, void*);
typedef void*(*fn4)(void*, void*, void*, void*);

// Calls a given function with a number of parameters.
// @param function address : 8 bytes
// @param ...args : up to 4 arguments, 8 bytes each.
// @param nargs: enocded in instruction stream
int vm_call(vm_t* vm)
{
    void* f = (void *) z_pop(vm, 8);
    uint64_t nargs = parse_int(vm, 8);
    assert(nargs < 4, "There are too many boxes!\n");
    void *arg1, *arg2, *arg3, *arg4;
    if (nargs > 0) arg1 = (void *) z_pop(vm, 8);
    if (nargs > 1) arg2 = (void *) z_pop(vm, 8);
    if (nargs > 2) arg3 = (void *) z_pop(vm, 8);
    if (nargs > 3) arg4 = (void *) z_pop(vm, 8);
    void* res = NULL;
    if (nargs == 0) res = ((fn0)f)();
    if (nargs == 1) res = ((fn1)f)(arg1);
    if (nargs == 2) res = ((fn2)f)(arg1, arg2);
    if (nargs == 3) res = ((fn3)f)(arg1, arg2, arg3);
    if (nargs == 4) res = ((fn4)f)(arg1, arg2, arg3, arg4);
    z_push(vm, (uint64_t) res, 8);
    P("call> nargs=%d, arg1=%p, arg2=%p, arg3=%p, arg4=%p res=%p\n",
      nargs, arg1, arg2, arg3, arg4, res);
    return 0;
}

// Loads a specified number of bytes on the stack from a given memory location.
// @param src source address : 8 bytes
// @param n number of bytes : 1 byte
int vm_load(vm_t* vm)
{
    char* src = (char *)z_pop(vm, 8);
    uint64_t n = z_pop(vm, 1); // Number of bytes to load.
    P("load> src=%p len=%d\n", src, n);
    while (n--)
    {
        uint64_t b = *src++;
        z_push(vm, b, 1);
    }
}

// Stores bytes on the stack into a specified memory location.
// @param dst destination address : 8 bytes
// @param n  number of bytes to store : 1 byte
int vm_stor(vm_t* vm)
{
    char* dst = (char *)z_pop(vm, 8);
    uint64_t n = z_pop(vm, 1); // Number of bytes to store.
    P("stor> dst=%p len=%d\n", dst, n);
    while (n--)
    {
        uint64_t b = z_pop(vm, 1);
        // NULL dst -> discard
        if (dst) *dst++ = b & 0xFF;
    }
}

// Maybe for next year? :)
// int vm_cmp1(vm_t* vm)
// {
//     return 0;
// }

// int vm_cmp8(vm_t* vm)
// {
//     return 0;
// }

// int vm_jmp(vm_t* vm)
// {
// }

// int vm_jz(vm_t* vm)
// {
// }

// ret - Unconditionally exit vm execution without altering CPU state.
int vm_ret(vm_t* vm)
{
    P("ret\n");
    return 1;
}

// Dispatch table.
OPCODE ops[64] = {
    vm_push1,      // hohoho
    vm_push8,      // hohohu
    NULL,          // hohoha
    NULL,          // hohohi
    vm_push0,          // hohuho
    vm_add1,       // hohuhu
    NULL,          // hohuha
    NULL,          // hohuhi
    vm_div8,       // hohaho
    vm_mul8,       // hohahu
    NULL,          // hohaha
    NULL,          // hohahi
    vm_add8,       // hohiho
    vm_sub8,       // hohihu
    vm_sub1,       // hohiha
    NULL,          // hohihi
    vm_xor1,       // huhoho
    vm_xor8,       // huhohu
    NULL,          // huhoha
    NULL,          // huhohi
    vm_sym,        // huhuho
    vm_call,       // huhuhu
    vm_load,       // huhuha
    vm_stor,       // huhuhi
    NULL,          // huhaho
    NULL,          // huhahu
    NULL,          // huhaha
    NULL,          // huhahi
    NULL,          // huhiho
    NULL,          // huhihu
    NULL,          // huhiha
    vm_popsp,      // huhihi
    vm_popr0,      // hahoho
    vm_popr1,      // hahohu
    vm_popr2,      // hahoha
    NULL,          // hahohi
    NULL,          // hahuho
    NULL,          // hahuhu
    NULL,          // hahuha
    NULL,          // hahuhi
    NULL,          // hahaho
    vm_pushsp,     // hahahu
    vm_pushr0,     // hahaha
    vm_pushr1,     // hahahi
    vm_pushr2,     // hahiho
    NULL,          // hahihu
    NULL,          // hahiha
    NULL,          // hahihi
    NULL,          // hihoho
    NULL,          // hihohu
    NULL,          // hihoha
    NULL,          // hihohi
    NULL,          // hihuho
    NULL,          // hihuhu
    NULL,          // hihuha
    NULL,          // hihuhi
    NULL,          // hihaho
    NULL,          // hihahu
    NULL,          // hihaha
    NULL,          // hihahi
    NULL,          // hihiho
    NULL,          // hihihu
    NULL,          // hihiha
    vm_ret,        // hihihi
};

void vm_run(vm_t* vm, char* code, size_t len) {
    vm->sp = vm->stack;
    vm->code = code;
    vm->pc = code;
    vm->len = len;

    int done = 0;
    while (!done)
    {
        uint64_t i = parse_int(vm, 6);
        OPCODE p = ops[i];
        assert(p != NULL, "Hohoho, ELF couldn't build the gift!\n");
        done = p(vm);
    }
}

void elf_menu()
{
    printf(
        "[A C C E S S   G R A N T E D]\n"
        "Here's your first giftoh ho ho, hohoho! <%s>\n"
        "Speak the ELFen directives to create a wonderful gift!\n"
        "(Hint: There might be another flag somewhere in the factory...)\n\n"
        , flag);
    fflush(stdout);

    int done = 0;
    while (!done)
    {
        printf("Input directives> ");
        fflush(stdout);

        char* code = NULL;
        size_t len = 0;
        getline(&code, &len, stdin);

        done = len == 0 || strcmp("exit\n", code) == 0;

        if (!done) vm_run(&vm, code, len);
        free(code);
    }
}

int main(int argc, char** argv)
{
    libc = dlopen("libc", RTLD_NOLOAD);

    size_t len = 0;
    char* passwd = NULL;
    printf("Welcome to Santa's factory, where elves are hard at work building gifts\n"
           "for all the kind children of the world.\n\n"
           "Unfortunately, the elves only speak ELFish. Speak the secret code to enter.\n"
           "Ho? Hohoho! > ");
    fflush(stdout);

    getline(&passwd, &len, stdin);

    // Initialize the VM with the validation routine.
    vm.r0 = (uint64_t) passwd;
    vm_run(&vm, validate_passwd, sizeof(validate_passwd)); // Run the VM.
    free(passwd);

    if (vm.r0 != 0) // Check output of strcmp
    {
        printf("You don't look like an ELF! Git Out!\n");
        fflush(stdout);

        exit(1);
    }

    elf_menu();
}
