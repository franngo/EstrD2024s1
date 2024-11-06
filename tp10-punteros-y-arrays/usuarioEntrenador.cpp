#include <iostream>
using namespace std;
#include "Entrenador.h" //como la de Entrenador incluye de la Pokemon, al incluir la 1ra tmb se incluye la 2da (las operaciones/definiciones)
                        //de todos modos, al compilar, TENGO que poner los 2 cpp igual sí o sí (para tener las implementaciones)

/*
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

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon);
//Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
//un entrenador.

string nombreDeEntrenador(Entrenador e);
//Devuelve el nombre del entrenador.

int cantidadDePokemon(Entrenador e);
//Devuelve la cantidad de pokémon que posee el entrenador.

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);
//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.

Pokemon pokemonNro(int i, Entrenador e);
//Devuelve el pokémon número i de los pokémon del entrenador.

bool leGanaATodos(Entrenador e1, Entrenador e2);
//Precondición: existen al menos i − 1 pokémon.
//Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
//posee al menos un pokémon que le gane.
*/

int main() {
    Pokemon p1 = consPokemon("Fuego");
    Pokemon p2 = consPokemon("Planta");
    perderEnergia(25, p2);
    Pokemon p3 = consPokemon("Agua");
    Pokemon p4 = consPokemon("Agua");
    perderEnergia(20, p4);
    Pokemon* ps = new Pokemon[4];
    ps[0] = p1;
    ps[1] = p2;
    ps[2] = p3;
    ps[3] = p4;
    Entrenador e1 = consEntrenador("Jimmy", 4, ps);
    Pokemon p = consPokemon("Agua");
    perderEnergia(20, p);
    Pokemon* ps2 = new Pokemon[1];
    ps2[0] = p;
    Entrenador e2 = consEntrenador("Enrique", 1, ps2);
    cout << leGanaATodos(e1, e2) << endl; 
    cout << energia(p4) << endl;
}