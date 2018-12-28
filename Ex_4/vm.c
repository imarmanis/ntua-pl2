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
    for(;;) {
        byte opcode = pc[0];
        switch(opcode) {
            case HALT:
                return 0;
            case JUMP:
                pc = &prg[get_2b(pc + 1)];
                break;
            case JNZ:
                pc = (*--sp != 0) ? &prg[get_2b(pc + 1)] : pc + 3;
                break;
            case DUP:
                {
                    int8_t ind = -pc[1];
                    word d = sp[ind-1];
                    *sp++ = d;
                    pc += 2;
                    break;
                }
            case SWAP:
                {
                    int8_t ind = -pc[1];
                    word d = sp[ind-1];
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
                *sp++ = get_4b(++pc);
                pc += 4;
                break;
            case PUSH2:
                *sp++ =  get_2b(++pc);
                pc += 2;
                break;
            case PUSH1:
                *sp++ = *++pc;
                pc++;
                break;
            case ADD:
                sp[-2] += sp[-1];
                sp--;
                pc++;
                break;
            case SUB:
                sp[-2] -= sp[-1];
                sp--;
                pc++;
                break;
            case MUL:
                sp[-2] *= sp[-1];
                sp--;
                pc++;
                break;
            case DIV:
                sp[-2] /= sp[-1];
                sp--;
                pc++;
                break;
            case MOD:
                sp[-2] %= sp[-1];
                sp--;
                pc++;
                break;
            case EQ:
                sp[-2] = (sp[-2] == sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                break;
            case NE:
                sp[-2] = (sp[-2] != sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                break;
            case LT:
                sp[-2] = (sp[-2] < sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                break;
            case GT:
                sp[-2] = (sp[-2] > sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                break;
            case LE:
                sp[-2] = (sp[-2] <= sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                break;
            case GE:
                sp[-2] = (sp[-2] >= sp[-1]) ? 1 : 0;
                sp--;
                pc++;
                break;
            case NOT:
                sp[-1] = (sp[-1] == 0) ? 1 : 0;
                break;
            case AND:
                sp[-2] = (sp[-2] != 0 && sp[-1] != 0) ? 1 : 0;
                sp--;
                pc++;
                break;
            case OR:
                sp[-2] = (sp[-2] != 0 || sp[-1] != 0) ? 1 : 0;
                sp--;
                pc++;
                break;
            case INPUT:
                *sp++ = getchar();
                pc++;
                break;
            case OUTPUT:
                putchar(*--sp);
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
    byte *prg = calloc(PROG_SIZE, sizeof(byte));
    fread(prg, sizeof(byte), PROG_SIZE, f);
    word *stack =  malloc(STACK_SIZE*sizeof(word));
    return run(prg, stack);
}
