/*
struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon; //array de pokes del entrenador
    int cantPokemon;
};

typedef EntrenadorSt* Entrenador;

Pokemon consPokemon(TipoDePokemon tipo);
//Dado un tipo devuelve un pokémon con 100 % de energía.

TipoDePokemon tipoDePokemon(Pokemon p);
//Devuelve el tipo de un pokémon.

int energia(Pokemon p);
//Devuelve el porcentaje de energía.

void perderEnergia(int energia, Pokemon p);
//Le resta energía al pokémon.

bool superaA(Pokemon p1, Pokemon p2);
//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
*/

#include <iostream>
using namespace std;
#include "Entrenador.h"

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->pokemon= pokemon;
    e->cantPokemon = cantidad;
    return e;
}

string nombreDeEntrenador(Entrenador e) {
//Devuelve el nombre del entrenador.
    return e->nombre;
}

int cantidadDePokemon(Entrenador e) {
//Devuelve la cantidad de pokémon que posee el entrenador.
    return e->cantPokemon;
}

int cuantosDeEn(TipoDePokemon tipo, Pokemon* p, int cant) {
    int contador = 0;
    for (int i=0; i<cant; i++) {
        if (tipoDePokemon(p[i])==tipo) {
            contador++;
        } 
    }
    return contador;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
    return cuantosDeEn(tipo, e->pokemon, e->cantPokemon);
}

Pokemon pokemonNro(int i, Entrenador e) {
//Devuelve el pokémon número i de los pokémon del entrenador.
    return e->pokemon[i-1];
}

bool leGanaATodos(Entrenador e1, Entrenador e2) {
//Precondición: existen al menos i − 1 pokémon.
//Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
//posee al menos un pokémon que le gane.
    int contador = 0;
    if(cantidadDePokemonDe("Agua", e2)!=0) {
        if(cantidadDePokemonDe("Planta", e1)!=0) {
            contador++;
        }
    } else {
        contador++;
    } //e1 tiene para ganarle a los de agua de e2, o e2 no tiene de agua.
    if(cantidadDePokemonDe("Fuego", e2)!=0) {
        if(cantidadDePokemonDe("Agua", e1)!=0) {
            contador++;
        }
    } else {
        contador++;
    } //e1 tiene para ganarle a los de fuego de e2, o e2 no tiene de fuego.
    if(cantidadDePokemonDe("Planta", e2)!=0) {
        if(cantidadDePokemonDe("Fuego", e1)!=0) {
            contador++;
        }
    } else {
        contador++;
    } //e1 tiene para ganarle a los de planta de e2, o e2 no tiene de planta.
    return contador==3;
} //podría usar subtareas para cada uno de los 3 casos. Hago que devuelvan booleano, y, si es true, suma 1 al cont, y si es false, no suma.
