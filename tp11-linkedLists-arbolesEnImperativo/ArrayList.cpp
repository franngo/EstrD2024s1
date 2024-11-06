#include <iostream>
using namespace std;
#include "ArrayList.h"

/*
struct ArrayListSt {
int cantidad; // cantidad de elementos
int* elementos; // array de elementos
int capacidad; // tamaño del array
}
typedef ArrayListSt* ArrayList;
*/

ArrayList newArrayList() {
//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.
    ArrayListSt* a = new ArrayListSt;
    a->cantidad= 0;
    a->capacidad= 16;
    a->elementos = new int[a->capacidad];
    for (int i=0; i<a->capacidad; i++) {
        a->elementos[i]=0;
    }
    return a;
}

ArrayList newArrayListWith(int capacidad) {
//Crea una lista con 0 elementos y una capacidad dada por parámetro.
    ArrayListSt* a = new ArrayListSt;
    a->cantidad= 0;
    a->capacidad= capacidad;
    a->elementos = new int[a->capacidad];
    for (int i=0; i<a->capacidad; i++) {
        a->elementos[i]=0;
    }
    return a;
}

int lengthAL(ArrayList xs) {
//Devuelve la cantidad de elementos existentes.
    return xs->cantidad;
}

int get(int i, ArrayList xs) {
//Devuelve el iésimo elemento de la lista.
    if (i<=xs->cantidad && i>0) {
        return xs->elementos[--i]; 
    }
    return 0; //de todos modos, este compilador devuelve 0 al intentar acceder a un espacio en memoria que no le pertenece al array.
}

void set(int i, int x, ArrayList xs) {
//Reemplaza el iésimo elemento por otro dado.
    xs->elementos[--i]= x;
}

void resize(int capacidad, ArrayList xs) {
//Decrementa o aumenta la capacidad del array.
//Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
    int* is = new int[capacidad];
    for(int i=0; i<capacidad; i++) {
        if(i+1>xs->capacidad) {
            is[i] = 0;
        } else {
            is[i] = xs->elementos[i];
        }
    }
    xs->capacidad= capacidad;
    xs->elementos= is;
}

void add(int x, ArrayList xs) {
//Agrega un elemento al final de la lista.
    if(xs->cantidad<xs->capacidad) {
        xs->elementos[xs->cantidad++] = x;
    } else {
        resize((xs->capacidad)*2, xs);
        xs->elementos[xs->cantidad++] = x;
    }
}

void remove(ArrayList xs) {
//Borra el último elemento de la lista.
    if((xs->cantidad-1)==(xs->capacidad/2) && xs->capacidad>16) {
        resize((xs->capacidad)/2, xs); //acá directamente se desasigna el espacio en donde estaba el elemento.
        (xs->cantidad)--;
    } else {
        xs->elementos[--(xs->cantidad)]= 0;
    }
}