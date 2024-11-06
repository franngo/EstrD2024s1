/*
struct PokeSt {
    tipoDePokemon tipo;
    int vida;
};

typedef PokeSt* Pokemon;
*/
#include <iostream>
using namespace std;
#include "Pokemon.h"

Pokemon consPokemon (TipoDePokemon tipo) {
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p) {
//Devuelve el tipo de un pokémon.
    return p->tipo;
}

int energia(Pokemon p) {
//Devuelve el porcentaje de energía.
    return p->vida;
}

void perderEnergia(int energia, Pokemon p) {
//Le resta energía al pokémon.
    if (p->vida!=0) {
        (p->vida)-=energia;
    }
}

bool superaA(Pokemon p1, Pokemon p2) {
//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
    return (p1->tipo=="Fuego"&&p2->tipo=="Planta") || (p1->tipo=="Agua" && p2->tipo=="Fuego") || (p1->tipo=="Planta" && p2->tipo=="Agua");
}