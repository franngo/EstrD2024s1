Pokemon consPokemon(TipoDePokemon tipo) {
//Dado un tipo devuelve un pokémon con 100 % de energía.
    Pokemon p = new PokemonSt;
    p->tipo = tipo;
    p->vida= 100;
    return p;
}


Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
//Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
//un entrenador.
    Entrenador e = new EntrenadorSt;
    e->nombre = nombre;
    e->pokemon = pokemon;
    e->cantPokemon = cantidad;
    return e;
}
