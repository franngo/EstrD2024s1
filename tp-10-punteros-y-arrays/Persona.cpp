#include <iostream>
using namespace std;
#include "Persona.h"

Persona consPersona(string nombre, int edad) {
//Devuelve el nombre de una persona
    PersonaSt* p = new PersonaSt;
    (*p).nombre = nombre;
    (*p).edad = edad;
    return p;
}

string nombre(Persona p) {
//Devuelve el nombre de una persona
    return (*p).nombre;
}

int edad(Persona p) {
//Devuelve la edad de una persona
    return (*p).edad;
}

void crecer(Persona p) {
//Aumenta en uno la edad de la persona.
    (*p).edad++;
}

void cambioDeNombre(string nombre, Persona p) {
//Modifica el nombre una persona.
    (*p).nombre= nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2) {
//Dadas dos personas indica si la primera es mayor que la segunda.
    return ((*p1).edad)>((*p2).edad);
}

Persona laQueEsMayor(Persona p1, Persona p2) {
//Dadas dos personas devuelve a la persona que sea mayor.
    if(((*p1).edad)>((*p2).edad)) {
        return p1;
    }
    return p2;
}
