#! /usr/bin/env python
# -*- coding: utf-8 -*-
 
from functools import reduce
 
def cifradoUbchi(mensaje,clave):
    def sortSr(lista):
        t = type(lista)
        resultado, lista = [], list(lista)
        k = len(lista)-1
        if len(lista):
            lista.sort()
            resultado = [lista[i] for i in range(k) if lista[i] != lista[i+1]]
        resultado.append(lista[-1])
        if t == str:
            resultado = ''.join(resultado)
        return resultado
     
    def minusList(lst,mst):
        long_l = len(lst)
        mst = sortSr(mst)
        xst = [i for n in mst for i in range(long_l) if lst[i] == n]
        return [lst[i] for i in range(long_l) if i not in xst]
     
    def partirMasResto(str,n):
        salida = []
        if str != []:
             filas, resto = len(str) // n , len(str) % n
             for i in range(filas):
                 salida.append(str[n*i:n*(i+1)])
             if resto != 0:
                 salida.append(str[n*filas:])
        return salida
     
    def completarResto(msj,n):
        salida = msj
        alfabeto = 'eaosrnidlctumpbgvyqhfzjxwk'
        k = n-len(msj)
        if k:
          salida = msj + "".join(k*[minusList(alfabeto,msj).pop()])
        return salida
     
    def quitaEspacios(m):
        def concatenarStr(s1,s2):
            return s1+s2
        lst = [m[i] for i in range(len(m)) if m[i] != ' ']    
        return reduce(concatenarStr,lst)    
     
    def columnasModif(mensaje,n):
        mensajeSin = quitaEspacios(mensaje)
        msj = partirMasResto(mensajeSin,len(clave))
        salida = []
        if len(mensajeSin) < n:
            salida = 'error: mensaje de longitud menor que la llave'
        elif msj:   
           k = len(msj[-1])
           p = len(msj[0])
           if k < p:     
               colTot = ["".join([m[i] for m in msj]) for i in range(k)]
               colPar = ["".join([m[i] for m in msj[:len(msj)-1]]) for i in range(k,n)]
               col = colTot + colPar
               salida = col
           else:
               colTot = ["".join([m[i] for m in msj]) for i in range(k)]
               salida = colTot
        return salida
         
    def transponerLista(msj,nlst):
        salida = []
        k = len(nlst)
        if k == 0:
            salida = 'error: longitud impropia en segundo argumento'
        elif sortSr(nlst) != list(range(k)):
            salida = 'error: el segundo argumento no es un segmento inicial natural'
        elif len(msj) != k:
            salida = 'error: listas de diferente longitud'
        else:
            salida = [msj[nlst.index(i)] for i in range(k)]
        return salida
 
    def destransponerLista(msj,nlst):
        salida = []
        k = len(nlst)
        if k == 0:
            salida = 'error: longitud impropia en segundo argumento'
        elif sortSr(nlst) != list(range(k)):
            salida = 'error: el segundo argumento no es un segmento inicial natural'
        elif len(msj) != k:
            salida = 'error: listas de diferente longitud'
        else:
            salida = [msj[nlst[i]] for i in range(k)]
        return salida
         
    # uso de las funciones locales: cuerpo del programa
     
    v = columnasModif(mensaje,len(clave))           # Fase 01
    w = transponerLista(v,clave)                    # Fase 02
    x = "".join(w)
    y = partirMasResto(x,len(clave))
    y.append(completarResto(y.pop(),len(clave)))
    z = columnasModif(y,len(clave))                 # Fase 03
    a = destransponerLista(z,clave)                 # Fase 04
    b = "".join(a)
    return partirMasResto(b,5)
 
# Área de pruebas
 
mensaje = 'replegar la brigada a posicion anterior cubrir con fuego de ametralladora'
clave01 = [7, 2, 1, 6, 5, 3, 0, 4, 8]
cifrado01 = cifradoUbchi(mensaje,clave01)
print(cifrado01)
clave02 = [1, 7, 6, 9, 8, 5, 2, 0, 4, 3]
cifrado02 = cifradoUbchi(mensaje,clave02)
print(cifrado02)
clave03 = [3, 5, 2, 7, 8, 0, 1, 6, 4]
cifrado03 = cifradoUbchi(mensaje,clave03)
print(cifrado03)
clave04 = [2, 3, 5, 6, 4, 0, 1, 7]
cifrado04 = cifradoUbchi(mensaje,clave04)
print(cifrado04)
clave05 = [2, 5, 3, 0, 4, 1]
cifrado05 = cifradoUbchi(mensaje,clave05)
print(cifrado05)
