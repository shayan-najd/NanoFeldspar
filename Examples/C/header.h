#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <complex.h>
#include <math.h>

#ifndef Feldspar_Header
#define Feldspar_Header

#define Wrd unsigned int
#define Bol bool

#define add(X,Y) (X + Y)
#define sub(X,Y) (X - Y)
#define mul(X,Y) (X * Y)
#define div(X,Y) (X / Y)
#define mod(X,Y) (X % Y)
#define eql(X,Y) (X == Y)
#define ltd(X,Y) (X < Y)

#define mem(X) (X)
#define sqrt(X) ((Wrd)sqrtf(X))


#define fst(X) (X.fst)
#define snd(X) (X.snd)
#define len(X) (X.size)
#define ind(X,Y) (X.elems[Y])
#define newTpl(X,Y,Z) (((Tpl##X) {.fst = Y , .snd = Z}))
#define newAry(X,Y) (((Ary##X) {.size = Y , .elems = malloc(Y * sizeof(X))}))
#define setAry(X,Y,Z) (({X.elems [Y] = Z;X;}))

typedef struct {Wrd size; Wrd* elems;} AryWrd;
#endif
