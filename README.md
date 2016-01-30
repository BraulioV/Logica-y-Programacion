# Lógica y Programación
Prácticas de la asignatura de Lógica y Programación del curso 2015-2016

En este repositorio hay una serie de ejericicios para la entrega de prácticas de la asignatura de ***Lógica y Programación*** de la Universidad de Granada, programadas en ***Haskell***.

## Ejercicio 1
En el ejercicio 1, tenemos los siguientes ejercicios:
  1. Definir el tipo de dato *Conjunto* y el mayor número de operaciones posibles
  2. Introducir el tipo de dato conjunto en las clases que se pudiese, entre ellas la clase *TiposConNormalizacion*.
  3. Definir el tipo de dato *Congruencia* e incluirla en la clase *TiposConNormalizacion* y añadir distintas operaciones.
  4. Implementar el **Criptosistema Afín**.
  5. Implementar el *Triángulo de Pascal* y usarlo para calcular números combinatorios con él. Compararlo con la eficiencia del cálculo de números combinatorios como el cociente de factoriales.
  6. Implementar el cálculo de números combinatorios con una red de procesos.
  7. Definir una red de procesos para calcular el máximo común divisor y los coeficientes de *Bezout*.
  8. Implementar la resolución de un sistema de congruencias para números enteros.

## Ejercicio 2
El ejercicio 2 incluye por un lado `TorresHanoi.hs`:

- Una función llamada `pasosTorresHanoi`, que a partir de un entero que el usuario introducirá por teclado, nos da la resolución de la torre para dicho número de pisos.

- Una función llamada `pasosTorresHanoi.txt`, que guardará la salida de la función anterior en un fichero.

por otro lado, un módulo llamado `TorresHanoiMenu.hs` que al ejecutar su función main, en primer lugar nos pedirá introducir un entero positivo mayor que cero y después nos pedirá si queremos resolver la torre por pantalla o en un fichero.
