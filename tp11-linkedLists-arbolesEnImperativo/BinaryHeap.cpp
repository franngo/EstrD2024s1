#include <iostream>
using namespace std;
#include "BinaryHeap.h"

/*
INV. REP.: No puede darse que curSize sea igual o mayor a maxSize.
struct BinaryHeapHeaderSt {
    int curSize;
    int maxSize;
    int* elems;
};

//INV. REP.: El puntero no es NULL.
typedef BinaryHeapHeaderSt* BinaryHeap;
*/

BinaryHeap emptyHeap() {
    BinaryHeapHeaderSt* bh = new BinaryHeapHeaderSt;
    bh->curSize = 0;
    bh->maxSize = 16;
    bh->elems = new int[bh->maxSize];
    bh->elems[0] = INT_MIN; // Macro definida en limits.h
    for (int i=1; i<bh->maxSize; i++) {
        bh->elems[i] = 0;
    }
    return bh;
}

//subtarea de insertH
void DoubleSize(BinaryHeap bh) {
    bh->maxSize*=2;
    int* newArray = new int[bh->maxSize];
    bh->elems[0] = INT_MIN; // Macro definida en limits.h
    for(int i=1; i<bh->maxSize; i++) {
        if(i<=bh->curSize) {
            newArray[i]=bh->elems[i]; //caso posición ocupada en array anterior
        } else {                      //caso posición sin ocupar en array anterior (inicializo)
            newArray[i]=0;
        }
    }
    delete bh->elems;
    bh->elems = newArray;
}

//Eficiencia O(log n), siendo n la cantidad total de elementos del binary heap dado.
void insertH(int x, BinaryHeap bh) {
    if(++bh->curSize==bh->maxSize) { DoubleSize(bh); } //tener curSize==15 y maxSize==16 significa que el array YA ESTÁ LLENO por el INT_MIN
    int curNode = bh->curSize; //la "posición actual" en la que ubicamos a x
    while(x<bh->elems[curNode/2]) { //mientras x sea menor que el número de su actual nodo padre
        bh->elems[curNode] = bh->elems[curNode/2]; //hundo al nodo padre a la posición del nodo hijo
        curNode/=2; //"floto" al nodo hijo a la posición del nodo padre
    }
    bh->elems[curNode] = x;
}

bool isEmptyH(BinaryHeap bh) {
    return bh->curSize==0;
}

//PRECOND: Debe darse que bh->curSize > 0
int findMin(BinaryHeap bh) {
    return bh->elems[1]; //no 0 porque ahí está almacenado el centinela (INT_MIN)
}


void destroyH(BinaryHeap bh) {
    delete bh->elems;
    delete bh;
}

//subtarea de deleteMin
void SplitSize(BinaryHeap bh) {
    bh->maxSize/=2;
    int* newArray = new int[bh->maxSize];
    bh->elems[0] = INT_MIN; // Macro definida en limits.h
    for(int i=1; i<bh->maxSize; i++) {
        newArray[i]=bh->elems[i];
    }
    delete bh->elems;
    bh->elems = newArray;
}

//Eficiencia O(log n), siendo n la cantidad total de elementos del binary heap dado.
//PRECOND: Debe darse que bh->curSize > 0
void deleteMin(BinaryHeap bh) {
    int last = bh->elems[bh->curSize--]; //elem de posición de borrado que subimos a la raíz e iremos hundiendo y actualizo cant elementos
    int curNode; //la "posición actual" en la que ubicamos a last
    int child;
    for(curNode=1; curNode*2<=bh->curSize; curNode=child) { //si el nodo actual tiene un hijo... (no se tiene en cuenta nodo del borrado)
        child = curNode*2;
        if(child!=bh->curSize && bh->elems[child+1]<bh->elems[child]) { child++; } //si el hijo derecho es menor que el izquierdo, 
                                                                                   //nos quedamos con el izquierdo
        if(last>bh->elems[child]) { //si el nodo actual es mayor que su hijo menor
            bh->elems[curNode]=bh->elems[child]; //"hundo" al nodo actual a la posición de su hijo y floto al hijo a la pos del padre
        } else {
            break; //sino ROMPO el ciclo y salgo de este para ahorrarme poner esta misma condicion en la condición del for y tener que
                   //volver a evaluar condición. Me ahorro de evaluar condición una vez.
        }
    }
    if (bh->curSize==1) { //caso borde cuando hay solo un elemento
        bh->elems[bh->curSize+1]= 0; //el elemento de la posición de borrado lo dejo en 0
    } else {
        bh->elems[bh->curSize+1]= 0; //el elemento de la posición de borrado lo dejo en 0
        bh->elems[curNode] = last; //luego de terminado el ciclo, asigno efectivamente al nodo actual el elemento borrado que antes era el 
                                   //de la posición de borrado.
    }
    if((bh->curSize+1)*2==bh->maxSize) { SplitSize(bh); } //cuando tengo la mitad del array vacío después de eliminar, puedo dividir ese
                                                          //mismo array por la mitad para ahorro de espacio. Importante recordar que hago
                                                          //curSize+1 porque una posición del array ya me la ocupa el centinela INT_MIN
}

//Eficiencia O(N log N), siendo N la cantidad de elementos del array de Int dado. Esto es así ya que se ejecuta en N ocasiones la operación
//insertH, la cual tiene, en el peor de los casos, un costo logarítmico sobre N, dejando así un costo O(N log N).
//PRECOND: Los elementos del array de Int dado tienen que estar ubicados a partir de la primera posición del mismo y sin dejar espacios
//vacíos entremedio.
BinaryHeap crearHeap(int* elements, int cant) {
    BinaryHeapHeaderSt* bh = emptyHeap();
    for(int i=0; i<cant; i++) {
        insertH(elements[i], bh);
    }
    return bh;
}