import numpy as np
from math import sqrt, exp, sin, cos, tan, tanh, log
import matplotlib.pyplot as plt 
import numpy as np
import pandas as pd
    



x=5
y=8
l=[x,y,x+x]

print('LA SUMA ES',sum(l))
data = [1, 4, 2, 4, 2, 5, 6, 7, 4, 76, 3, 2, 5, 6, 7]
from scipy.stats import describe
describe(data)

matriz=np.matrix([[1,3,4,5],[3,4,6,9]])
print(matriz)
f=matriz[1,3]

sin_f=sin(f)
if sin_f >0.1:
    print('mayor a 0.1')
    
datos = pd.read_csv('P0.csv', sep=' ')
print(datos)

plt.plot([1, 2, 3],[10,20,30]) 
plt.title('matplotlib.pyplot.plot() example 1') 
plt.draw() 
plt.show()
plt.close()

datos = [1.6, 4.6, 2.6, 3.6, 5.6, 6.6, 3.5, 2.2, 4.4, 5.2, 5.4, 7.6, 5.8, 4.4, 6.4]
plt.hist(datos)
plt.show()
plt.close()

from random import randint
desde = 1
hasta = 1000
menor = 15
mayor = 21
from time import time # traer libreria
for k in range(menor, mayor + 1):
    n = 2 ** k
    lista = [ randint(desde, hasta) for i in range(n) ]
    antes = time() # ver la hora
    lista.sort()
    despues = time() # volver ver la hora
    diferencia = despues - antes # segundos
    print('ordenar', n, 'elementos toma', diferencia, 'segundos')

import sys
from sys import getsizeof
import numpy as np
from random import randint
menor=15
mayor=21
for k in range(menor, mayor + 1):
    n=2**k
    lista = [ randint(1, 1000) for i in range(n) ]
    espacio=getsizeof(lista)
    print('un vector de',n,'elementos ocupa',espacio,'bytes de memoria')
    
