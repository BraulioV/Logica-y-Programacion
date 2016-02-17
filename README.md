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
El ejercicio 2 incluye por un lado __TorresHanoi.hs__:

- Una función llamada __pasosTorresHanoi__, que a partir de un entero que el usuario introducirá, nos da la resolución de la torre para dicho número de pisos.

- Una función llamada __pasosTorresHanoitxt__, que guardará la salida de la función anterior en un fichero.

por otro lado, un módulo llamado __Menu.hs__ que al ejecutar su función main, en primer lugar nos pedirá introducir un entero positivo mayor que cero y después nos pedirá si queremos resolver la torre por pantalla o en un fichero.

## Ejercicio 3
Implementación de los procedimientos elementales de la lógica proporsional: tablas de verdad de fórmulas, normas normales conjuntiva y disyuntiva e implicación semántica. 

Para ello, se definirá el tipo de dato `Formula` que incluirá funciones para obtener su tabla de verdad, para generar la forma normal conjuntiva o disyuntiva y para saber si un conjunto de funciones es tautología.

## Ejercicio 4
El ejercicio 4 consiste en hacer un pequeño proyecto propio, en este caso el __Cifrado de Übchi__ ([Wikipedia](https://en.wikipedia.org/wiki/Transposition_cipher, "Wikipedia")). También incluye la implementación de una __Escítala__ ([Wikipedia](https://es.wikipedia.org/wiki/Esc%C3%ADtala, "Wikipedia"))
