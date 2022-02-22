import numpy as np # instalar numpy con pip
from random import random
import matplotlib.cm as cm
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
dim = 10
num = dim**3
p = 0.5
valores = [1 * (random() <p) for i in range(num)]
actual = np.reshape(valores, (dim, dim,dim))
filled = np.ones((dim, dim, dim), dtype=bool)

# repeating values 3 times for grayscale
colors = np.repeat(actual[:, :, :, np.newaxis], 3, axis=3)

fig = plt.figure()
ax = fig.gca(projection='3d')

ax.voxels(filled, facecolors=colors, edgecolors='k')
plt.show()


dur = 2000
lim = 9
seq = 0
import random
if __name__ == "__main__":

    for iteracion in range(dur):
        lista=range(dim-1)
        matriz=random.choice(lista)
        fila=random.choice(lista)
        columna=random.choice(lista)
        vecindad = actual[max(0, matriz - 1):min(dim, matriz+ 2),max(0, fila - 1):min(dim, fila + 2),
                          max(0, columna - 1):min(dim, columna + 2)] 
        vecindad2 = actual[matriz,max(0, fila - 1):min(dim, fila + 2),
                          max(0, columna - 1):min(dim, columna + 2)]
        sum_vecindad=np.sum(vecindad)+np.sum(vecindad2)
        if sum_vecindad>=6:
            actual[matriz,fila,columna]=0
                
        vivos = np.sum(actual)
        print(iteracion, vivos)
        if vivos == 0:
            break; # nadie vivo
##        if iteracion in[5,10,15,20]: 
##            print(iteracion)
##            fig = plt.figure()
##            plt.imshow(actual, interpolation='nearest', cmap=cm.Greys)
##            fig.suptitle('Paso {:d}'.format(iteracion + 1))
##            plt.savefig('p2_t{:d}_p.png'.format(iteracion+1))
##            plt.close()
# para crear un GIF, se puede usar ImageMagick con
# convert -delay 100 -size 300x300 -loop 0 p2_t*.png p2p.gif

filled = np.ones((dim, dim, dim), dtype=bool)

# repeating values 3 times for grayscale
colors = np.repeat(actual[:, :, :, np.newaxis], 3, axis=3)

fig = plt.figure()
ax = fig.gca(projection='3d')

ax.voxels(filled, facecolors=colors, edgecolors='k')
plt.show()

