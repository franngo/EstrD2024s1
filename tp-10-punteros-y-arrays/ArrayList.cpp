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
    if (i<=xs->cantidad) {
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
//A TENER EN CUENTA= Si es usada por el usuario (y no por otra de interfaz) y este decrementa a menos de la cantidad de elementos que
//tiene el arraylist, el campo de cantidad queda desfasado ya que acá NO se actualiza (hay que ver como se maneja ese dato en las
//de interfaz para no hacer quilombo)
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

bool perteneceAL(int x, ArrayList xs) {
//Retorna si x pertenece a xs o no.    
    for(int i=0; i<xs->cantidad; i++) {
        if(xs->elementos[i]==x) {
            return true;
        }
    }
    return false;
}

//subtarea de removeIf
//OBSERVACIÓN= Si no está el elemento, no se ejecuta ninguna acción, por lo que no se requiere precondición.
//Eficiencia O(n), siendo n la cantidad de elementos que tiene el arraylist dado (no capacidad, sino cantidad). Esto es así ya que,
//en todos los casos, se ejecutan en n ocasiones una serie de operaciones constantes. En el peor de los casos, esta serie de operaciones
//incluye hacer una asignación en el array de elementos del arraylist dado.
void borrarYOrdenarSi(int x, ArrayList xs) {
    for(int i=0; i<xs->cantidad; i++) {
        if(xs->elementos[i]==x) { 
            for(; i<xs->cantidad-1; i++) { //como ya tengo i creada y con un valor, no hace falta inicializar nada. Se pone solo ;
                xs->elementos[i]=xs->elementos[i+1]; //desplazo al de la derecha a pos actual a la izquierda y sobrescribo. así borro a x.
            }
            xs->elementos[i]=0; //a la última posición ocupada la dejo en 0, que es con lo que inicializo las celdas en newArrayList.
            (xs->cantidad)--; //actualizo cantidad
            break; //rompo el ciclo así no se ejecuta nada más innecesariamente
        }
    }
}

void removeIf(int x, ArrayList xs) {
//Borra la primera aparición de x en xs si el mismo se encuentra almacenado ahí
    borrarYOrdenarSi(x, xs); //si no está el elemento, no se ejecuta ninguna acción
    if((xs->cantidad)==(xs->capacidad/2) && xs->capacidad>16) { //hacemos resize en caso de que la nueva cantidad sea mitad de capacidad.
        resize((xs->capacidad)/2, xs);
    }
}

void Destruir(ArrayList xs) {
    delete xs->elementos;
    delete xs;
}
