import numpy as np # instalar numpy con pip
from random import random
import matplotlib.cm as cm
import matplotlib.pyplot as plt
from sklearn.manifold import Isomap
import random
dim = 10
num = dim**2
actual=np.zeros([dim,dim])            
dur =100
semillas=[1,2,3,4,5]
for iteracion in range(dur):
    fila=random.randint(0,dim-1)
    columna =random.randint(0,dim-1)
    if actual[fila,columna]==0:
        actual[fila,columna]=random.choice(semillas)
print('actual semillas aleatorias',actual)
fig = plt.figure()
shw1=plt.imshow(actual, cmap=plt.cm.get_cmap('jet', 6))
plt.colorbar(ticks=range(6), label='semilla') 
fig.suptitle('INICIO')
plt.savefig('p2_reto_1_0.png')
plt.close()

for pos in range(num):
    fila = pos// dim
    columna = pos%dim
    if actual[fila,columna]==0:
        vecindad = actual[max(0, fila - 1):min(dim, fila + 2),
                      max(0, columna - 1):min(dim, columna + 2)]
        lista = list(range(0,len(vecindad)))
        lista2 = list(range(0,len(vecindad[0])))
        i=random.choice(lista)
        j=random.choice(lista2)
        semilla=vecindad[i][j]
        actual[fila,columna]=semilla
        while semilla==0:
            i=random.choice(lista)
            j=random.choice(lista2)
            semilla=vecindad[i][j]
        print(semilla)
        actual[fila,columna]=semilla    
print('actual semillas final',actual)
fig = plt.figure()
shw1=plt.imshow(actual, cmap=plt.cm.get_cmap('jet', 5))
plt.colorbar(ticks=range(6), label='semilla')
fig.suptitle('FIN')
plt.savefig('p2_reto_1_2.png')
plt.close()
