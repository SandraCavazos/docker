
from fastapi import FastAPI
import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt
import io
import base64


app = FastAPI()

#Leer bd
dir_bd = 'C:/Users/6QW09LA_1909/OneDrive/Escritorio/pruebaDocker/'
df = pd.read_csv(dir_bd + 'CarPrice_Assignment.csv')
y = df['price']
x = df[['carheight' ,'enginesize' , 'boreratio']]
a = df['carheight']


modelo = LinearRegression()
modelo.fit(x, y)
coeficientes = modelo.coef_
intercepto = modelo.intercept_
coef1 = coeficientes[0]
coef2 = coeficientes[1]
coef3 = coeficientes[2]
resumen = pd.DataFrame(x.describe())


print(f"Nuevo_Precio = {intercepto:.2f} + ({coef1:.2f})*x1  + ({coef2:.2f})*x2 + ({coef3:.2f})*x3")
print("Coeficiente(s):", coeficientes)
print("Intercepto:", intercepto)
print("Resumen:", resumen)


plt.scatter(a, y)
plt.xlabel('Altura del coche')
plt.ylabel('Precio')
plt.title('Gráfico de dispersión: Precio vs Altura del coche')
#plt.show()
# Guardar el gráfico en un archivo temporal
buf = io.BytesIO()
plt.savefig(buf, format='png')
buf.seek(0)
img_data = base64.b64encode(buf.read()).decode('utf-8')
plt.close()



@app.get("/")
def read_root():
    ecuacion = f"y4 = {intercepto:.2f} + ({coef1:.2f})*x1  + ({coef2:.2f})*x2 + ({coef3:.2f})*x3"
    espacios = '---------------------------------------------------------------------------------------------'
    #Resumen = Resumen.to_dict()
    #return {"ecuacion": ecuacion, "|": espacios, "Resumen": resumen, "scatter_plot": img_data}
    return {"ecuacion": ecuacion, "|": espacios, "Resumen": resumen.to_dict(), "scatter_plot": f"data:image/png;base64,{img_data}"}

