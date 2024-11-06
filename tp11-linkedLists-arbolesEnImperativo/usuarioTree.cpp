#include <iostream>
using namespace std;
#include "Tree.h"
#include "ArrayList.h"

/*
-INTERFAZ DE Tree.h=
Tree emptyT();

Tree nodeT(int elem, Tree left, Tree right);

bool isEmptyT(Tree t);

int rootT(Tree t);

Tree left(Tree t);

Tree right(Tree t);

-INTERFAZ DE ArrayList.h=
ArrayList newArrayList();
//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.

ArrayList newArrayListWith(int capacidad);
//Crea una lista con 0 elementos y una capacidad dada por parámetro.

int lengthAL(ArrayList xs);
//Devuelve la cantidad de elementos existentes.

int get(int i, ArrayList xs);
//Devuelve el iésimo elemento de la lista.

void set(int i, int x, ArrayList xs);
//Reemplaza el iésimo elemento por otro dado.

void resize(int capacidad, ArrayList xs);
//Decrementa o aumenta la capacidad del array.
//Nota: en caso de decrementarla, se pierden los elementos del final de la lista.

void add(int x, ArrayList xs);
//Agrega un elemento al final de la lista.

void remove(ArrayList xs);
//Borra el último elemento de la lista.
*/

//FUNCIONES IMPLEMENTADAS CON RECURSIÓN (manera no tan eficiente de implementarlas) =

//Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t) {
    if(!isEmptyT(t)) {
        return (rootT(t) + sumarT(left(t)) + sumarT(right(t)) );
    } else {
        return 0;
    }
}


//Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
//en inglés).
int sizeT(Tree t) {
    if(!isEmptyT(t)) {
        return ( 1 + sizeT(left(t)) + sizeT(right(t)) );
    } else {
        return 0;
    }
}

//Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
//árbol.
bool perteneceT(int e, Tree t) {
    if(!isEmptyT(t)) {
        return (rootT(t)==e) || perteneceT(e, left(t)) || perteneceT(e, right(t));
    } else {
        return false;
    }
}


//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
//iguales a e.
int aparicionesT(int e, Tree t) {
    if(!isEmptyT(t)) {
        if(rootT(t)==e) {
            return 1 + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        } else {
            return 0 + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
    } else {
        return 0;
    }
}

//Dado un árbol devuelve su altura.
int heightT(Tree t) {
    if(!isEmptyT(t)) {
        return 1 + max(heightT(left(t)), heightT(right(t)));
    } else {
        return 0;
    }
}

//subtarea de toList
void ToList2(ArrayList al, Tree t) {
    if(!isEmptyT(t)) {
        add(rootT(t), al);
        ToList2(al, left(t)); //acá no pasa nada con ir pasando como parámetros varias veces el ArrayList, ya que realmente solo es un 
        ToList2(al, right(t));//puntero, al igual que los trees pasados. Donde se da el gasto en memoria es en crear muchos stack frames,
    }                         //aún si no se da que cada uno almacena estructuras grandes sino solo punteros.
}

//Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    ArrayList al = newArrayList();
    ToList2(al, t);
    return al;
}

//subtarea de leaves
void Leaves2(ArrayList al, Tree t) {
    if(!isEmptyT(t)) {
        if(isEmptyT(left(t)) && isEmptyT(right(t))) {
            add(rootT(t), al);
        }  else {
            Leaves2(al, left(t)); 
            Leaves2(al, right(t));
        }
    }
}

//Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {
    ArrayList al = newArrayList();
    Leaves2(al, t);
    return al;
}

//subtarea de levelN
//Observación= Se comienza en el nivel 0 (raíz) y los niveles de los hijos son un número mayor, y así respectivamente.
void LevelN2(ArrayList al, int n, Tree t) {
    if(!isEmptyT(t)) {
        if(n==0) {
            add(rootT(t), al);
        } else {
            LevelN2(al, (n-1), left(t));
            LevelN2(al, (n-1), right(t));
        }
    }
}

//Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t) {
    ArrayList al = newArrayList();
    LevelN2(al, n, t);
    return al;
}

//g++ -o prueba usuarioTree.cpp Tree.cpp ArrayList.cpp

int main() {
    Tree et = emptyT();
    Tree t1 = nodeT(7, et, et);
    Tree t2 = nodeT(9, et, et);
    Tree t3 = nodeT(6, t1, t2);
    Tree t4 = nodeT(11, et, et);
    Tree t5 = nodeT(5, t3, t4);
    ArrayList al = levelN(1, t5);
    cout << "El largo del ArrayList es de " << lengthAL(al) << endl;
    cout << "A continuación, sus elementos: " << endl;
    for(int i=1; i<=6; i++) {
        cout << get(i, al) << endl;
    }
}