#include <iostream>
using namespace std;
#include "Tree.h"

/*
struct NodeT { 
int elem;
NodeT* left;
NodeT* right;
} //lo declaro acá en vez de en el .h porque tengo dos TADS que lo usan y que importo para usuarioTreeBFS y me lo toma como doble
  //definición del struct. Haciendolo así, no me pasa (supongo que la única definición que procesa el compilador es esta cuando pongo
  //Tree.cpp al compilar)

typedef NodeT* Tree;
*/

struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};

Tree emptyT() {
    NodeT* t = NULL;
    return t;
}

Tree nodeT(int elem, Tree left, Tree right) {
    NodeT* t = new NodeT;
    t->elem = elem;
    t->left = left;
    t->right = right;
    return t;
}

bool isEmptyT(Tree t) {
    return t==NULL;
}

//PRECOND: t no es un árbol vacío.
int rootT(Tree t) {
    return t->elem;
}

Tree left(Tree t) {
    return t->left;
}

Tree right(Tree t) {
    return t->right;
}