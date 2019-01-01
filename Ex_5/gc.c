#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <byteswap.h>
#include <time.h>
#include "gc.h"

#define enci(x) (((int32_t) (x)) << 1)
#define deci(x) (((int32_t) (x)) >> 1)
#define encp(x) ((((uint32_t) (x)) << 1) | 1)
#define decp(x) (((uint32_t) (x)) >> 1)
#define isp(x) ((x) & 1)

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
        *heap.alloc = *ptr;
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
    static void *label_tab[51] = {
        &&halt_label,
        &&jump_label,
        &&jnz_label,
        &&dup_label,
        &&swap_label,
        &&drop_label,
        &&push4_label,
        &&push2_label,
        &&push1_label,
        &&add_label,
        &&sub_label,
        &&mul_label,
        &&div_label,
        &&mod_label,
        &&eq_label,
        &&ne_label,
        &&lt_label,
        &&gt_label,
        &&le_label,
        &&ge_label,
        &&not_label,
        &&and_label,
        &&or_label,
        &&input_label,
        &&output_label
    };
    label_tab[CLOCK] = &&clock_label;
    label_tab[CONS] = &&cons_label;
    label_tab[HD] = &&hd_label;
    label_tab[TL] = &&tl_label;
#define NEXT_INSTR goto *(void *)(label_tab[*pc])
    for(;;) {
        switch(pc[0]) {
            case HALT:
halt_label:
                return 0;
            case JUMP:
jump_label:
                pc = &prg[get_2b(pc + 1)];
                NEXT_INSTR;
            case JNZ:
jnz_label:
                pc = (deci(*--sp) != 0) ? &prg[get_2b(pc + 1)] : pc + 3;
                NEXT_INSTR;
            case DUP:
dup_label:
                {
                    int8_t ind = -pc[1];
                    int32_t d = sp[ind-1];
                    *sp++ = d;
                    pc += 2;
                    NEXT_INSTR;
                }
            case SWAP:
swap_label:
                {
                    int8_t ind = -pc[1];
                    int32_t d = sp[ind-1];
                    sp[ind-1] = sp[-1];
                    sp[-1] = d;
                    pc += 2;
                    NEXT_INSTR;
                }
            case DROP:
drop_label:
                sp--;
                pc++;
                NEXT_INSTR;
            case PUSH4:
push4_label:
                *sp++ = enci(get_4b(++pc));
                pc += 4;
                NEXT_INSTR;
            case PUSH2:
push2_label:
                *sp++ =  enci(get_2b(++pc));
                pc += 2;
                NEXT_INSTR;
            case PUSH1:
push1_label:
                *sp++ = enci(*++pc);
                pc++;
                NEXT_INSTR;
            case ADD:
add_label:
                sp[-2] = enci(deci(sp[-2]) + deci(sp[-1]));
                sp--;
                pc++;
                NEXT_INSTR;
            case SUB:
sub_label:
                sp[-2] = enci(deci(sp[-2]) - deci(sp[-1]));
                sp--;
                pc++;
                NEXT_INSTR;
            case MUL:
mul_label:
                sp[-2] = enci(deci(sp[-2]) * deci(sp[-1]));
                sp--;
                pc++;
                NEXT_INSTR;
            case DIV:
div_label:
                sp[-2] = enci(deci(sp[-2]) / deci(sp[-1]));
                sp--;
                pc++;
                NEXT_INSTR;
            case MOD:
mod_label:
                sp[-2] = enci(deci(sp[-2]) % deci(sp[-1]));
                sp--;
                pc++;
                NEXT_INSTR;
            case EQ:
eq_label:
                sp[-2] = (deci(sp[-2]) == deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case NE:
ne_label:
                sp[-2] = (deci(sp[-2]) != deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case LT:
lt_label:
                sp[-2] = (deci(sp[-2]) < deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case GT:
gt_label:
                sp[-2] = (deci(sp[-2]) > deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case LE:
le_label:
                sp[-2] = (deci(sp[-2]) <= deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case GE:
ge_label:
                sp[-2] = (deci(sp[-2]) >= deci(sp[-1])) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case NOT:
not_label:
                sp[-1] = (deci(sp[-1]) == 0) ? enci(1) : enci(0);
                NEXT_INSTR;
            case AND:
and_label:
                sp[-2] = (deci(sp[-2] != 0) && deci(sp[-1] != 0)) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case OR:
or_label:
                sp[-2] = (deci(sp[-2] != 0) || deci(sp[-1] != 0)) ? enci(1) : enci(0);
                sp--;
                pc++;
                NEXT_INSTR;
            case INPUT:
input_label:
                *sp++ = enci(getchar());
                pc++;
                NEXT_INSTR;
            case OUTPUT:
output_label:
                putchar(deci(*--sp));
                pc++;
                NEXT_INSTR;
            case CONS:
cons_label:
                {
                    cell_t *c = alloc(sp);
                    c->tl = *--sp;
                    c->hd = *--sp;
                    *sp++ = encp(c - heap.start);
                    pc++;
                    NEXT_INSTR;
                }
            case HD:
hd_label:
                sp[-1] = heap.start[decp(sp[-1])].hd;
                pc++;
                NEXT_INSTR;
            case TL:
tl_label:
                sp[-1] = heap.start[decp(sp[-1])].tl;
                pc++;
                NEXT_INSTR;
            case CLOCK:
clock_label:
                {
                    clock_t end = clock();
                    printf("%0.6lf\n", (double)(end - start)/CLOCKS_PER_SEC);
                    pc++;
                    NEXT_INSTR;
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
