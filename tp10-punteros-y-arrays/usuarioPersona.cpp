#include <iostream>
using namespace std;
#include "Persona.h"

/* interfaz de Persona:
Persona consPersona(string nombre, int edad)
Devuelve el nombre de una persona

string nombre(Persona p)
Devuelve el nombre de una persona

int edad(Persona p)
Devuelve la edad de una persona

void crecer(Persona p)
Aumenta en uno la edad de la persona.

void cambioDeNombre(string nombre, Persona p)
Modifica el nombre una persona.

bool esMayorQueLaOtra(Persona p1, Persona p2)
Dadas dos personas indica si la primera es mayor que la segunda.

Persona laQueEsMayor(Persona p1, Persona p2)
Dadas dos personas devuelve a la persona que sea mayor.
*/

int main() {
    Persona p = consPersona("Ricardo", 36);
    crecer(p); cambioDeNombre("José Luis", p);
    Persona t = consPersona("Arthur", 99);
    if (esMayorQueLaOtra(t,p)) {
        cout << "Hay alguien más grande que otra persona acá" << endl;
    }
    cout << "El más veterano acá es " << nombre(laQueEsMayor(t,p)) << " con " << edad(laQueEsMayor(t,p)) << " años de edad" << endl;
    cout << "La primera persona en haber llegado acá se llama " << nombre(p) << " y tiene " << edad(p) << " años de edad" << endl;
    /*
    string x;
    cout << "Ahora ingresá tu nombre y apellido" << endl;
    getline (cin, x);
    cout << "Sos un máquina, " << x << endl;
    */
}