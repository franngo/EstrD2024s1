#include <iostream>
using namespace std;
#include "ArrayList.h"

/*
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

bool perteneceAL(int x, ArrayList xs);
//Retorna si x pertenece a xs o no.

void removeIf(int x, ArrayList xs);
//Borra la primera aparición de x en xs si el mismo se encuentra almacenado ahí
*/

int sumatoria(ArrayList xs) {
//Devuelve la suma de todos los elementos.
    int s = 0;
    for(int i=0; i<lengthAL(xs); i++) {
        s+=get((i+1),xs);
    }
    return s;
}

void sucesores(ArrayList xs) {
//Incrementa en uno todos los elementos.
    for(int i=0; i<lengthAL(xs); i++) {
        int x= get((i+1),xs);
        set((i+1), ++x, xs);
    }
}

bool pertenece(int x, ArrayList xs) {
//Indica si el elemento pertenece a la lista.
    for(int i=0; i<lengthAL(xs); i++) {
        if (x==get((i+1),xs)) {
            return true;
        }
    }
    return false;
}

int apariciones(int x, ArrayList xs) {
//Indica la cantidad de elementos iguales a x.
    int c = 0;
    for(int i=0; i<lengthAL(xs); i++) {
        if (x==get((i+1),xs)) {
            c++;
        }
    }
    return c;
}

ArrayList append(ArrayList xs, ArrayList ys) {
//Crea una nueva lista a partir de la primera y la segunda (en ese orden).
    for(int i=0; i<lengthAL(ys); i++) {
        add(get((i+1),ys), xs);
    }
    return xs;
}

int minimo(ArrayList xs) {
//Devuelve el elemento más chico de la lista.
    int m = get((1),xs);
    for(int i=0; i<lengthAL(xs); i++) {
        if (get((i+1),xs)<m) {
            m = get((i+1),xs);
        }
    }
    return m;
}

int main () {
    ArrayList a = newArrayList();
    add(4, a);
    add(9, a);
    add(94, a);
    add(65, a);
    add(3, a);
    add(23, a);
    add(56, a);
    removeIf(65,a);
    cout << "Cantidad de elementos: " << lengthAL(a) << endl;
    cout << "Capacidad del array: " << a->capacidad << endl; //trampa!!
    for(int i=0; i<9; i++) {
        cout << get(i+1,a) << endl;
    }
    Destruir(a);
}