import numpy as np # instalar numpy con pip
from random import random
import numpy as np
import matplotlib.cm as cm
import matplotlib.pyplot as plt                  
dur = 30
seq = 0
replicas=100
dim = np.array([10,15,20])
p_value = np.array([0.2,0.4,0.6,0.8])
plot_matrix=np.zeros([len(p_value),len(dim)])
if __name__ == "__main__":
    #fig = plt.figure()
    #plt.imshow(actual, interpolation='nearest', cmap=cm.Greys)
    #fig.suptitle('Estado inicial')
    #plt.savefig('p2_t0_p.png')
    #plt.close()
    xd=0
    for d in (dim):
        xp=0
        for p in (p_value):
            num = d**2
            total=0
            for rep in range(replicas):
                valores = [1 * (random() < p) for i in range(num)]
                actual = np.reshape(valores, (d, d))
                def mapeo(pos):  
                    fila = pos // d
                    columna = pos % d
                    return actual[fila, columna]
                assert all([mapeo(x) == valores[x]  for x in range(num)])
                def paso(pos):
                    fila = pos // d
                    columna = pos % d
                    vecindad = actual[max(0, fila - 1):min(d, fila + 2),
                       max(0, columna - 1):min(d, columna + 2)]
                    return 1 * (np.sum(vecindad) - actual[fila, columna] == 3)
                for iteracion in range(dur):
                    valores = [paso(x) for x in range(num)]
                    vivos = sum(valores)
                    #print(rep,iteracion, vivos)
                    actual = np.reshape(valores, (d, d))
                    #print(actual)
                if vivos>0:
                    total=total+1
                    #rint(rep,p,d,'vive')
                    #print(rep,p,d,'no vive')
            #print('Para una matriz de',d,'*',d,'con un valor de p=',p,'despues de 50 replicas quedaron con vida',total,'matrices')
            plot_matrix[xp,xd]=total
            xp=xp+1
            #print(xp)
            #print(xd)
        xd=xd+1
  #ordenamiento de la matriz      
print('Matrices con Vida',plot_matrix)
plot_matrix=(plot_matrix*100)/replicas #probabilidad
print('Probabiliad de Vida',plot_matrix)
dim1=plot_matrix[:,0]                         
dim2=plot_matrix[:,1]
dim3=plot_matrix[:,2]

x=np.array([0.2,0.4,0.6,0.8])

plt.plot(x,dim1, marker="o",label="10")
plt.plot(x,dim2,marker="o",label="15")
plt.plot(x,dim3,marker="o",label="20")
plt.xlim(0.2,0.8)
plt.ylim(0,50)
plt.xlabel("Valor de P")
plt.ylabel("Probabilidad de Vida")
plt.title("Probabilidad de vida variando las dimensiones de la matriz y el valor de P")
plt.legend()
plt.show()

