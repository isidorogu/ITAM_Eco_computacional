##
## Tarea 2 Eco Computacional - Python. Predictive Models 
# Isidoro Garcia 

# Cargando librerias
import pandas as pd 
import math 
import numpy as np
import datetime as dt
from plotnine import ggplot, aes, geom_point, geom_line, geom_col, geom_histogram, geom_smooth , geom_bar, geom_boxplot, labs, coord_flip, facet_wrap, theme_bw
# import pyreadr as pr


# Leyendo la base de datos
data = pd.read_csv('Bases input/cell2cell.csv')


# Que me muestre todo
pd.set_option('display.max_columns', None)



# ## Contexto
# Cell2Cell es una compañía de teléfonos celulares que intenta mitigar el abandono de sus usuarios. Te contratan para 1) Encontrar un modelo que prediga el abandono con acierto y para usar los insights de este modelo para proponer una estrategia de manejo de abandono.
# Las preguntas que contestaremos son:
# 
# 1. Se puede predecir el abandono con los datos que nos compartieron? 
# 
# 2. Cuáles son las variables que explican en mayor medida el abandono? 
# 
# 3. Qué incentivos da Cell2Cell a sus usarios para prevenir el abandono?
# 
# 4. Cuál es el valor de una estrategia de prevención de abandono focalizada y cómo difiere entre los segmentos de los usuarios? Qué usuarios deberían de recibir incentivos de prevención? Qué montos de incentivos
# 
# Nota: Voy a evaluar las tareas con base en la respuesta a cada pregunta. Como hay algunas preguntas que no tienen una respuesta clara, al final ponderaré de acuerdo al poder predictivo de su modelo vs las respuestas sugeridas. 
# 

# ### 1. Qué variables tienen missing values? Toma alguna decisión con los missing values. Justifica tu respuesta


missings = data.isna().mean()*100
missings = missings[missings>0]
missings

np.shape(data)


# Age, age2, eqpdays: Quito las obs
data = data[~pd.isnull(data.age1)] 
data = data[~pd.isnull(data.age2)]
data = data[~pd.isna(data.eqpdays)]

# El resto las lleno con zero
data = data.fillna(0)
np.shape(data)


# ### 2. Tabula la distribución de la variable `churn`. 
# Muestra la frecuencia absoluta y relativa. Crees que se debe hacer oversampling/undersamping? 
# La variable tiene 70% de ceros y 30% de 1's. Es probable que si tengamos que hacer algo de weightening

100*data.churn.value_counts()/data.shape[0]

ggplot(data)+geom_bar(aes(x = 'churn'))+theme_bw()


# ### 3. (2 pts) Divide tu base en entrenamiento y validación (80/20). 
# Además, considera hacer oversampling (SMOTE) o undersampling. 
# (Tip: Recuerda que el objetivo final es tener muestra ~balanceada en el traning set. En el validation la distribución debe ser la original)
# La distribución esta 70% vs 30%. Si queremos construir un training set = 80%. 
# Dentro del training, hay que hacer el undersampling tal que se balanceen las clases. 

# 3 maneras 
# numpy rand
# pandas sample 
# scikitlearn 

###############
# 1. scikitlearn 
###############
from sklearn.model_selection import train_test_split

# Jalando las X y la Y 
y_col = 'churn'
x_cols = data.loc[:, data.columns != y_col].columns


# Creando las muestras 
X_train, X_test, y_train, y_test = train_test_split(data[x_cols], data[y_col], test_size = 0.2, random_state = 1990)


###############
# 2. pandas 
###############
data_train = data.sample(frac = 0.8, random_state = 1990)
data_test = data.drop(data_train.index)

del X_test, X_train, y_train, y_test
