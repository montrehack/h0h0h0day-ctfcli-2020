#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct vm_
{
    char* code; size_t len;
    char* pc; char* sp;
    uint64_t r0, r1, r2;
    char stack[0x1000];
} vm_t;

typedef int(*OPCODE)(vm_t*);
uint64_t parse_symbol(vm_t* vm);
uint64_t parse_int(vm_t* vm, size_t bits);

// Helper function to pop N bytes from the stack.
uint64_t z_pop(vm_t* vm, size_t n);
void z_push(vm_t* vm, uint64_t val, size_t n);

int vm_push8(vm_t* vm);
int vm_push1(vm_t* vm);

#define pushop(reg) int vm_push##reg(vm_t* vm);
pushop(r0);
pushop(r1);
pushop(r2);
pushop(sp);
#undef pushop


#define popop(reg) int vm_pop##reg(vm_t* vm);
popop(r0);
popop(r1);
popop(r2);
popop(sp);
#undef popop

#define binop(name, S, sz) int vm_##name##sz(vm_t* vm);
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
#undef binop

int vm_sym(vm_t* vm);


typedef void*(*fn0)(void);
typedef void*(*fn1)(void*);
typedef void*(*fn2)(void*, void*);
typedef void*(*fn3)(void*, void*, void*);
typedef void*(*fn4)(void*, void*, void*, void*);
int vm_call(vm_t* vm);
int vm_load(vm_t* vm);
int vm_stor(vm_t* vm);
int vm_ret(vm_t* vm);
void vm_run(vm_t* vm, char* code, size_t len);
