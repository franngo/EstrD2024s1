#include <iostream>
using namespace std;
#include <climits>

struct BinaryHeapHeaderSt {
    int curSize;
    int maxSize;
    int* elems;
};

//INV. REP.: El puntero no es NULL.
typedef BinaryHeapHeaderSt* BinaryHeap;

BinaryHeap emptyHeap();

void insertH(int x, BinaryHeap h);

bool isEmptyH(BinaryHeap h);

int findMin(BinaryHeap h);

void deleteMin(BinaryHeap h);

void destroyH(BinaryHeap bh);

BinaryHeap crearHeap(int* elements, int cant);