import numpy as np # instalar numpy con pip
from random import random
import numpy as np
import matplotlib.cm as cm
import matplotlib.pyplot as plt                  
dur = 50
seq = 0
replicas=100
dim = np.array([10,15,20])
p_value = np.array([0.2,0.4,0.6,0.8])
plot_matrix=np.zeros([len(p_value),len(dim)])
if __name__ == "__main__":
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
    print(plot_matrix)
    plot_matrix=plot_matrix*100/replicas#probabilidad
    dim1=plot_matrix[:,0]                         
    dim2=plot_matrix[:,1]
    dim3=plot_matrix[:,2]
    dim1=dim1.tolist()
    dim2=dim2.tolist()
    dim3=dim3.tolist()
    #Gráfica
    labels = ['0.2', '0.4', '0.6', '0.8']
    x = np.arange(len(labels))  # the label locations
    width = 0.30  # the width of the bars

    fig, ax = plt.subplots()
    rects1 = ax.bar(x - width, dim1, width, label='10')
    rects2 = ax.bar(x, dim2, width, label='15')
    rects3 = ax.bar(x + (width), dim3, width, label='20')

# Add some text for labels, title and custom x-axis tick labels, etc.
    ax.set_ylabel('Probabilidad de que exista vida al final de cada replica')
    ax.set_title('Cellular Automata variando el valor de p y las dimensiones de la matriz')
    ax.set_xticks(x, labels)
    ax.legend()

    ax.bar_label(rects1, padding=3)
    ax.bar_label(rects2, padding=3)
    ax.bar_label(rects3, padding=3)

    fig.tight_layout()

    plt.show()  
