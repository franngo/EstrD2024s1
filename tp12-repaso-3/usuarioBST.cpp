#include <iostream>
using namespace std;
#include "BST.h" //que importa a su vez LinkedList.h, aunque acá solo somos usuarios del módulo BST.h

/*INV. REP. de BST: 
    *La raíz es mayor que todos los elementos del subtree izquierdo y es menor que todos los elementos del subtree derecho.
    *El anterior invariante también se cumple para el subtree izquierdo y el subtree derecho. 
*/

/*
INTERFAZ DE OPERACIONES DE BST.h =
BST emptyT();

BST nodeT(Elem x, BST left, BST right);

//PRECOND= t no puede ser un árbol vacío
Elem rootT(BST t);

bool belongsT(Elem x, BST t);

//OBSERVACIÓN= Si es un árbol vacío, retorna un árbol vacío
BST findMin(BST t); //devuelve el nodo con el menor elemento de todo el tree

//OBSERVACIÓN= Si es un árbol vacío, retorna un árbol vacío
BST findMax(BST t); //devuelve el nodo con el mayor elemento de todo el tree

//PRECOND= El BST dado NO puede ser nulo
//OBSERVACIÓN= Si el elemento ya está en el BST, no se ejecuta ninguna acción.
void InsertT(Elem x, BST t);

//OBSERVACIÓN= Si el elemento no está en el BST, no se ejecuta ninguna acción.
void RemoveT(Elem x, BST t);

void DestroyT(BST t); //versión con recorrido recursivo (MÁS COSTOSO EN MEMORIA)

void DestroyTV2(BST t); //versión con recorrido lineal (MÁS COSTOSO EN TIEMPO)

BST cloneT(BST t); //devuelve un clon del BST dado por parámetro. Versión con recorrido recursivo (MÁS COSTOSO EN MEMORIA)

BST cloneTV2(BST t); //devuelve un clon del BST dado por parámetro. Versión con recorrido lineal (MÁS COSTOSO EN TIEMPO)
*/

//g++ -o prueba usuarioBST.cpp BST.cpp LinkedList.cpp

int main() {
    BST t1 = nodeT(9, emptyT(), emptyT());
    InsertT(12, t1);
    InsertT(2, t1);
    RemoveT(9, t1);
    RemoveT(12, t1);
    InsertT(10, t1);
    BST t2 = cloneTV2(t1);
    cout << "La raíz de t2 es " << rootT(t2) << endl;
    cout << "El mínimo de t2 es " << rootT(findMin(t2)) << endl;
    cout << "El máximo de t2 es " << rootT(findMax(t2)) << endl;
    DestroyTV2(t1); 
    DestroyTV2(t2); 
}
