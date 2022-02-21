import numpy as np # instalar numpy con pip
from random import random
import matplotlib.cm as cm
import matplotlib.pyplot as plt
dim = 100
num = dim**2
p = 0.3
res=[random()  for i in range(num)]
valores = []
for ele in res:
    if ele>p:
        valores.append(0)
    else:
        valores.append(ele)
actual = np.reshape(valores, (dim, dim))
print(actual)
def crecimiento(pos):
    fila = pos // dim
    columna = pos % dim
    return actual[fila, columna]+0.1 * (actual[fila, columna]>0)
def nocrecen(pos):
    fila = pos // dim
    columna = pos % dim
    return actual[fila, columna]* (actual[fila, columna]==0)
    
        

##def paso(pos):
##    fila = pos // dim
##    columna = pos % dim
##    vecindad = actual[max(0, fila - 1):min(dim, fila + 2),
##                      max(0, columna - 1):min(dim, columna + 2)]
##    return 0.1* (np.sum(vecindad)== 3)
            
dur = 30
lim = 9
for iteracion in range(dur):
    for pos in range(num):
        fila=pos // dim
        columna = pos % dim
        if actual[fila,columna]>0 and actual[fila,columna]<1:
            actual[fila,columna]=actual[fila, columna]+0.03
        if actual[fila,columna]==0:
            vecindad = actual[max(0, fila - 1):min(dim, fila + 2),
                      max(0, columna - 1):min(dim, columna + 2)]
            suma_vecindad=np.sum(vecindad)
            if suma_vecindad>=2:
                actual[fila,columna]=0.01
    if iteracion in [1,5,10,15,20,25,29]: 
        print('actual',iteracion,actual)
        fig = plt.figure()
        plt.imshow(actual, interpolation='nearest', cmap=cm.Greys)
        fig.suptitle('Paso {:d}'.format(iteracion + 1))
        plt.savefig('p2_reto_1__t{:d}_p.png'.format(iteracion+1))
        plt.close()
