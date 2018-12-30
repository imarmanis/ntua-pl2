#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <byteswap.h>
#include <time.h>
#include "vm.h"

typedef uint8_t byte;
typedef int32_t word;

uint16_t get_2b(byte *p)
{
   uint16_t res = *((uint16_t *) p);
#if __BYTE_ORDER == __BIG_ENDIAN
   return __bswap_16(p);
#else
   return res;
#endif
}

uint32_t get_4b(byte *p)
{
   uint32_t res = *((uint32_t *) p);
#if __BYTE_ORDER == __BIG_ENDIAN
   return __bswap_32(p);
#else
   return res;
#endif
}

int run(byte *prg, word *stack)
{
    clock_t start = clock();
    byte *pc = prg;
    word *sp = stack;
    static void *label_tab[33] = {
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
#define NEXT_INSTR goto *(void *)(label_tab[*pc])
    for(;;) {
        byte opcode = pc[0];
        switch(opcode) {
            case HALT:
halt_label:
                return 0;
            case JUMP:
jump_label:
                pc = &prg[get_2b(pc + 1)];
                NEXT_INSTR;
            case JNZ:
jnz_label:
                pc = (*--sp != 0) ? &prg[get_2b(pc + 1)] : pc + 3;
                NEXT_INSTR;
            case DUP:
dup_label:
                {
                    int8_t ind = -pc[1];
                    word d = sp[ind-1];
                    *sp++ = d;
                    pc += 2;
                    NEXT_INSTR;
                }
            case SWAP:
swap_label:
                {
                    int8_t ind = -pc[1];
                    word d = sp[ind-1];
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
                *sp++ = get_4b(++pc);
                pc += 4;
                NEXT_INSTR;
            case PUSH2:
push2_label:
                *sp++ =  get_2b(++pc);
                pc += 2;
                NEXT_INSTR;
            case PUSH1:
push1_label:
                *sp++ = *++pc;
                pc++;
                NEXT_INSTR;
            case ADD:
add_label:
                sp[-2] += sp[-1];
                sp--;
                pc++;
                NEXT_INSTR;
            case SUB:
sub_label:
                sp[-2] -= sp[-1];
                sp--;
                pc++;
                NEXT_INSTR;
            case MUL:
mul_label:
                sp[-2] *= sp[-1];
                sp--;
                pc++;
                NEXT_INSTR;
            case DIV:
div_label:
                sp[-2] /= sp[-1];
                sp--;
                pc++;
                NEXT_INSTR;
            case MOD:
mod_label:
                sp[-2] %= sp[-1];
                sp--;
                pc++;
                NEXT_INSTR;
            case EQ:
eq_label:
                sp[-2] = (sp[-2] == sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case NE:
ne_label:
                sp[-2] = (sp[-2] != sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case LT:
lt_label:
                sp[-2] = (sp[-2] < sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case GT:
gt_label:
                sp[-2] = (sp[-2] > sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case LE:
le_label:
                sp[-2] = (sp[-2] <= sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case GE:
ge_label:
                sp[-2] = (sp[-2] >= sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case NOT:
not_label:
                sp[-1] = (sp[-1] == 0) ? 1 : 0;
                NEXT_INSTR;
            case AND:
and_label:
                sp[-2] = (sp[-2] != 0 && sp[-1] != 0) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case OR:
or_label:
                sp[-2] = (sp[-2] != 0 || sp[-1] != 0) ? 1 : 0;
                sp--;
                pc++;
                NEXT_INSTR;
            case INPUT:
input_label:
                *sp++ = getchar();
                pc++;
                NEXT_INSTR;
            case OUTPUT:
output_label:
                putchar(*--sp);
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
    byte *prg = calloc(PROG_SIZE, sizeof(byte));
    fread(prg, sizeof(byte), PROG_SIZE, f);
    word *stack =  malloc(STACK_SIZE*sizeof(word));
    return run(prg, stack);
}
