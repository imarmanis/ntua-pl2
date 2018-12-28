#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <byteswap.h>
#include <time.h>
#include "gc.h"

#define enci(x) (((int32_t) (x)) << 1)
#define deci(x) (((int32_t) (x)) >> 1)
#define encp(x) ((((uint32_t) (x)) << 1) + 1)
#define decp(x) (((uint32_t) (x)) >> 1)
#define isp(x) ((x) & (1 << 31))

uint16_t get_2b(uint8_t *p)
{
   uint16_t res = *((uint16_t *) p);
#if __BYTE_ORDER == __BIG_ENDIAN
   return __bswap_16(p);
#else
   return res;
#endif
}

uint32_t get_4b(uint8_t *p)
{
   uint32_t res = *((uint32_t *) p);
#if __BYTE_ORDER == __BIG_ENDIAN
   return __bswap_32(p);
#else
   return res;
#endif
}

typedef struct cell {
    uint32_t hd;
    uint32_t tl;
} cell_t;

typedef struct heap {
    cell_t *start;
    cell_t *from;
    cell_t *to;
    cell_t *alloc;
    uint32_t half;
} heap_t;

uint8_t *prg;
int32_t *stack;
int32_t *sp;
heap_t heap;

unsigned char is_from(cell_t *ptr)
{
    return ((heap.from <= ptr) && (ptr < heap.from + heap.half)) ? 1 : 0;
}

uint32_t forward(uint32_t p)
{
    cell_t *ptr = &heap.start[decp(p)];
    if (!is_from(ptr))
        return p;

    if (!isp(ptr->hd) || is_from(&heap.start[decp(ptr->hd)])){
        heap.alloc->hd = ptr->hd;
        heap.alloc->tl = ptr->tl;
        ptr->hd = encp(heap.alloc++ - heap.start);
    }

    return ptr->hd;
}

void collect(int32_t *sp)
{
    heap.alloc = heap.to;

    int32_t *s_scan;
    for(s_scan = stack; s_scan < sp; ++s_scan) {
        if (isp(*s_scan))
            *s_scan = forward(*s_scan);
    }

    cell_t *h_scan;
    for(h_scan = heap.to; h_scan < heap.alloc; ++h_scan) {
        if (isp(h_scan->hd)) h_scan->hd = forward(h_scan->hd);
        if (isp(h_scan->tl)) h_scan->tl = forward(h_scan->tl);
    }

    cell_t *temp = heap.to;
    heap.to = heap.from;
    heap.from = temp;

}

cell_t *alloc(int32_t *sp)
{
    if (heap.alloc == heap.from + heap.half)
        collect(sp);
    if (heap.alloc == heap.from + heap.half){
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return heap.alloc++;
    //return encp(heap.alloc++ - heap.start);
}



int run()
{
    clock_t start = clock();
    uint8_t *pc = prg;
    sp = stack;
    for(;;) {
        switch(pc[0]) {
            case HALT:
                return 0;
            case JUMP:
                pc = &prg[get_2b(pc + 1)];
                break;
            case JNZ:
                pc = (deci(*--sp) != 0) ? &prg[get_2b(pc + 1)] : pc + 3;
                break;
            case DUP:
                {
                    int8_t ind = -pc[1];
                    int32_t d = sp[ind-1];
                    *sp++ = d;
                    pc += 2;
                    break;
                }
            case SWAP:
                {
                    int8_t ind = -pc[1];
                    int32_t d = sp[ind-1];
                    sp[ind-1] = sp[-1];
                    sp[-1] = d;
                    pc += 2;
                    break;
                }
            case DROP:
                sp--;
                pc++;
                break;
            case PUSH4:
                *sp++ = enci(get_4b(++pc));
                pc += 4;
                break;
            case PUSH2:
                *sp++ =  enci(get_2b(++pc));
                pc += 2;
                break;
            case PUSH1:
                *sp++ = enci(*++pc);
                pc++;
                break;
            case ADD:
                sp[-2] = enci(deci(sp[-2]) + deci(sp[-1]));
                sp--;
                pc++;
                break;
            case SUB:
                sp[-2] = enci(deci(sp[-2]) - deci(sp[-1]));
                sp--;
                pc++;
                break;
            case MUL:
                sp[-2] = enci(deci(sp[-2]) * deci(sp[-1]));
                sp--;
                pc++;
                break;
            case DIV:
                sp[-2] = enci(deci(sp[-2]) / deci(sp[-1]));
                sp--;
                pc++;
                break;
            case MOD:
                sp[-2] = enci(deci(sp[-2]) % deci(sp[-1]));
                sp--;
                pc++;
                break;
            case EQ:
                sp[-2] = (deci(sp[-2]) == deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case NE:
                sp[-2] = (deci(sp[-2]) != deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case LT:
                sp[-2] = (deci(sp[-2]) < deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case GT:
                sp[-2] = (deci(sp[-2]) > deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case LE:
                sp[-2] = (deci(sp[-2]) <= deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case GE:
                sp[-2] = (deci(sp[-2]) >= deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case NOT:
                sp[-1] = (deci(sp[-1]) == 0) ? enci(1) : enci(0);
                break;
            case AND:
                sp[-2] = (deci(sp[-2] != 0) && deci(sp[-1] != 0)) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case OR:
                sp[-2] = (deci(sp[-2] != 0) || deci(sp[-1] != 0)) ? enci(1) : enci(0);
                sp--;
                pc++;
                break;
            case INPUT:
                *sp++ = enci(getchar());
                pc++;
                break;
            case OUTPUT:
                putchar(deci(*--sp));
                pc++;
                break;
            case CONS:
                {
                    cell_t *c = alloc(sp);
                    c->tl = *--sp;
                    c->hd = *--sp;
                    *sp++ = encp(c - heap.start);
                    pc++;
                    break;
                }
            case HD:
                sp[-1] = heap.start[decp(sp[-1])].hd;
                pc++;
                break;
            case TL:
                sp[-1] = heap.start[decp(sp[-1])].tl;
                pc++;
                break;
            case CLOCK:
                {
                    clock_t end = clock();
                    printf("%0.6lf\n", (double)(end - start)/CLOCKS_PER_SEC);
                    pc++;
                    break;
                }
        }
    }

}

int main (int argc, char ** argv)
{
    if (argc < 2) {
        fprintf(stderr, "File arg missing\n");
        return 1;
    }

    FILE *f = fopen(argv[1], "r");
    prg = calloc(PROG_SIZE, sizeof(uint8_t));
    fread(prg, sizeof(uint8_t), PROG_SIZE, f);
    stack =  malloc(STACK_SIZE*sizeof(int32_t));
    heap.half = SEMI_SIZE;
    heap.alloc = heap.from = heap.start = malloc(2*heap.half*sizeof(cell_t));
    heap.to = heap.from + heap.half;
    return run();
}
