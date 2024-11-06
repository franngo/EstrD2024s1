#include <iostream>
using namespace std;
#include "BinaryHeap.h"

//g++ -o prueba usuarioBinaryHeap.cpp BinaryHeap.cpp Tree.cpp

/*
-INTERFAZ DE BinaryHeap.h=

BinaryHeap emptyHeap();

void insertH(int x, BinaryHeap h);

bool isEmptyH(BinaryHeap h);

int findMin(BinaryHeap h);

void deleteMin(BinaryHeap h);

void destroyH(BinaryHeap bh);

BinaryHeap crearHeap(int* elements, int cant);
*/

//g++ -o prueba usuarioBinaryHeap.cpp BinaryHeap.cpp

int main() {
    int* arrayPrueba = new int[16];
    arrayPrueba[0]= 33;
    arrayPrueba[1]= 11;
    arrayPrueba[2]= 9;
    arrayPrueba[3]= 10;
    arrayPrueba[4]= 8;
    arrayPrueba[5]= 80;
    arrayPrueba[6]= 81;
    arrayPrueba[7]= 82;
    arrayPrueba[8]= 83;
    arrayPrueba[9]= 84;
    arrayPrueba[10]= 85;
    arrayPrueba[11]= 86;
    arrayPrueba[12]= 87;
    arrayPrueba[13]= 88;
    arrayPrueba[14]= 89;
    arrayPrueba[15]= 90;
    BinaryHeap bh = crearHeap(arrayPrueba, 16); //como añado 16 elementos, por el centinela, se va a duplicar el tamaño a 32
    for(int i=1; i<=19; i++) {
        cout << bh->elems[i] << endl;
    }
    cout << "El curSize es " <<  bh->curSize << endl;
    cout << "El maxSize es " <<  bh->maxSize << endl;
    destroyH(bh);
}