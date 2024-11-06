#include "Fraccion.h"

/*
struct Fraccion {
    int numerador;
    int denominador;
};
*/

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f) {
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f) {
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f) {
    return float(f.numerador)/f.denominador;
}
//con float(x) se convierte el valor de x en un float. Se puede usar con inmediatos (ej: float(2)). Este método de conversión tmb sirve
//con otros tipos (ej: int(x)). Se llaman operadores de conversión y existen para todos los tipos de c++ (pero supongo que, dependiendo del
//parámetro usado, te puede dar un resultado con sentido o no).
//si al menos uno de los 2 operandos de la división es float en vez de int, el resultado de la división es decimal y no entero.

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2) {
    f1.numerador= f1.numerador*f2.numerador;
    f1.denominador= f1.denominador*f2.denominador;
    return f1;
}

/*OBSERVACIÓN= En el primer caso de llamado recursivo (div decimal no difiere de entera) se puede pasar directamente dc en vez del 2 de 
vuelta, ya que en ningún caso el nuevo divisor común puede ser un número anterior/menor al que acaba de funcionar como divisor común. */
Fraccion simplificadaAux(Fraccion p, int dc) {
    if (p.denominador==1) { //si el denominador es 1, se devuelve p
        return p;
    } 
    else if (float(p.numerador)/dc == p.numerador/dc && float(p.denominador)/dc == p.denominador/dc) { 
        p.numerador=p.numerador/dc;
        p.denominador=p.denominador/dc;
        return simplificadaAux(p,dc); //si div decimal NO difiere de entera, entonces se divide por el dc probado y llamado recurs con new p
    }
    else if (p.denominador==dc) { //si el denominador es el dc probado, se devuelve p
        return p;
    }
    else {
        dc++;
        return simplificadaAux(p,dc);
    }
}
/*Simplificación= Para poder dividir el numerador y el denominador por el mismo número, este tiene que ser divisor común de los dos. Por 
esta razón el proceso de simplificar se detiene cuando los números son primos relativos, o sea, cuando no tienen más divisores comunes 
que 1. Cuando esto pasa, es decir, cuando una fracción no se puede simplificar más, se dice que es irreducible.*/

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p) {
    int dc = 2;
    return simplificadaAux(p,dc);
}

// Propósito: devuelve la primera componente
Fraccion sumF(Fraccion f1, Fraccion f2) {
    if(f1.denominador==f2.denominador) {
        f1.numerador=f1.numerador+f2.numerador;
        return f1;
    }
    f1.numerador=(f1.numerador*f2.denominador)+(f2.numerador*f1.denominador);
    f1.denominador=f1.denominador*f2.denominador;
    return f1;
}
//no sé a que se refiere con "devolver la primera componente". Por el nombre sum asumo que el objetivo es sumar ambas fracciones.
