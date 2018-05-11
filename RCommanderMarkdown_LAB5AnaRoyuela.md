<!-- R Commander Markdown Template -->

LAB5- Ana Royuela-Mayo2018                                                               
=======================

### Ana Royuela

### 2018-05-10




Importing dataset
```{r}
DataReg <- read.table("C:/Users/Ana/Documents/UOC/SOFTWARE ANALISIS DATOS/MODULO 5/DataReg.txt", header=TRUE, sep="", 
  na.strings="NA", dec=".", strip.white=TRUE)
```

Names of variables
```{r}
names(DataReg)
```

Summary of data
```{r}
summary(DataReg)
```

Histograms of data

```{r}
with(DataReg, Hist(weight, scale="frequency", breaks="Sturges", col="tomato", xlab="Weight", main="Histogram of weight"))
```

```{r}
with(DataReg, Hist(age, scale="frequency", breaks="Sturges", col="darkgray", xlab="Age", main="Histogram of age"))
```

```{r}
with(DataReg, Hist(fats, scale="frequency", breaks="Sturges", col="yellow", xlab="Fats", main="Histogram of fats"))
```

Scatterplot matrix de las tres variables:

```{r}
scatterplotMatrix(~age+fats+weight, regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), data=DataReg)
```

```{r}
scatterplotMatrix(~age+fats+weight, regLine=FALSE, smooth=FALSE, diagonal=list(method="none"), data=DataReg)
```

```{r}
scatterplotMatrix(~age+fats+weight, regLine=FALSE, smooth=FALSE, diagonal=list(method="histogram"), data=DataReg)
```

Scatterplot between age and fat
```{r}
scatterplot(age~fats, regLine=TRUE, smooth=FALSE, id=list(method='mahal', n=2), boxplots=FALSE, data=DataReg)
```

Matriz de correlación de las tres variables

```{r}
cor(DataReg[,c("age","fats","weight")], use="complete")
```

Modelo de regresión lineal entre fats y age
```{r}
RegModel.1 <- lm(fats~age, data=DataReg)
summary(RegModel.1)
```

Gráfica del modelo
```{r}
plot(DataReg$age, DataReg$fats, xlab="Age", ylab="Fats")
abline(RegModel.1)
```

Generating predictions from a new dataset
```{r}
newages<-data.frame(age=seq(30,50))
predict(RegModel.1, newages)
```

Intervalos de confianza al 95%
```{r}
Confint(RegModel.1, level=0.95)
```

Intervalos de confianza al 90%
```{r}
Confint(RegModel.1, level=0.90)
```

Diagnosis Model
```{r}
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
```

```{r}
plot(RegModel.1)
```

```{r}
par(oldpar)
```

```{r}
resid<-rstandard(RegModel.1)
adj.values<-fitted(RegModel.1)
plot(adj.values,resid)
```

# Exercise 2 
### 1. With the same data set (previous exercise), create a model that explains the relation between fat and weight.
```{r}
RegModel.2 <- lm(fats~weight, data=DataReg)
summary(RegModel.2)
```
En este modelo, no se observa asociación entre fats and weight, ya que el coeficiente de weight no es estadísticamente diferente de 0.

### 2. Calculate and graph the regression line, together with the corresponding point cloud.
```{r}
plot(DataReg$weight, DataReg$fats, xlab="Weight", ylab="Fats")
abline(RegModel.2)
```

### 3. What is the squared correlation coefficient in this case?
El coeficiente de determinación es igual a 0.07038, mucho menor que en el caso de la edad. La variabilidad de las grasas explicada por el peso es del 7%.

### 4. What are the estimate parameters of the model?
El intercept es  199.298, es decir, que cuando el peso es igual a 0, el valor de las grasas es  199.298.
Por cada unidad de peso, la grasa aumenta en promedio 1.622 unidades.

### 5. Test the hypothesis that the slope of the line is 0 to 0.05 level. (Note: R?Commander has a specific menu.)
La hipótesis de que la pendiente de la línea es 0, es igual a contrastar si el coeficiente de weight es =0. La p asociada a ese contraste es 0.200, 
por tanto no se puede rechazar la hipótesis nula de que la pendiente es igual a 0.

### 6. Calculate a confidence interval for a slope of 90%.
```{r}
Confint(RegModel.2, level=0.90)
```
El IC90% de la pendiente abarca entre (-0.4847468; 3.729432). Cruza la línea del 0, por tanto, no es estadísticamente significativo


### 7. Show the estimated confidence intervals (95%) of the average fat for individuals between 30 and 90 kg.
```{r}
RegModel.3 <- lm(fats~weight, data=DataReg, subset=weight>=30 & weight<=90)
summary(RegModel.3)
predict(RegModel.3,interval = ("confidence"))
```

### 8. Perform a diagnostic model.

```{r}
RegModel.2 <- lm(fats~weight, data=DataReg)
summary(RegModel.2)
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
```

```{r}
plot(RegModel.2)
```

```{r}
par(oldpar)
```

# Exercise 3.

### 1. Use the parent’s heights to predict children’s heights (prediction).

Me he bajado de internet el archivo "Galton.csv" (https://vincentarelbundock.github.io/Rdatasets/datasets.html)

```{r}
library(UsingR) 
```

```{r}
Galton <- read.table("C:/Users/Ana/Documents/UOC/SOFTWARE ANALISIS DATOS/MODULO 5/Galton.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
```

Diagrama de dispersión para visualizar si ambas variables están relacionadas, parece que sí, parece que hay una correlación positiva:

```{r}
scatterplot(child~parent, regLine=TRUE, smooth=FALSE, boxplots=FALSE, data=Galton)
```

```{r}
cor(Galton[,c("child","parent")], use="complete")
```
El coeficiente de correlación de Pearson entre ambas variables es de 0.459.

### 2. Find a relationship between parental and child heights (modelling).
```{r}
Child_height <- lm(child~parent, data=Galton)
summary(Child_height)
Confint(Child_height, level=0.95)
```

En este caso, la interpretación del intercept no tiene sentido, ya que los padres no pueden medir 0 unidades.
Por cada unidad de altura del padre, el hijo medirá 0.646 unidades más (IC 95% 0.565; 0.727). Es decir, hay una asociación
estadísticamente significativa entre la altura de los padres y la de los hijos con una magnitud de 0.646 unidades.


### 3. Investigate the variation in child heights that appears unrelated to parental heights (residual variation), and quantify what impact genotype information has beyond parental height in explaining child height (covariation). An important aspect, especially in questions 2 and 3, is assessing modelling assumptions. 

Esa información la tenemos observando la R^2 del modelo, que nos indica el % de variabilidad de la variable dependiente (altura de los hijos) que es explicada por la variable
independiente (altura de los padres). En nuestro caso, la R^2 es 0.210, es decir, sólo un 21% de la variabilidad de la altura de los hijos es explicada por la genética (altura de los padres). Por tanto, un 79% de la variabilidad de la altura de los hijos no es explicada por la altura de sus padres.


# Exercise 4.

Importar los datos EsterData
```{r}
EsterData <- read.table("C:/Users/Ana/Documents/UOC/SOFTWARE ANALISIS DATOS/MODULO 5/EsterData.csv", header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)
View(EsterData)
```
Gráfica de dispersión:
```{r}
scatterplot(conc~T, regLine=FALSE, smooth=FALSE, boxplots=FALSE, data=EsterData)
```

Añadir una nueva columna con el log(conc):
```{r}
EsterData$log_conc <- with(EsterData, log(conc))
```

Gráfica de dispersión con log(conc):
```{r}
scatterplot(log_conc~T, regLine=FALSE, smooth=FALSE, boxplots=FALSE, data=EsterData)
```

La asociación es ahora lineal, con una correlación inversa y estadísticamente significativa

Modelo lineal
```{r}
RegModel.2 <- lm(log_conc~T, data=EsterData)
summary(RegModel.2)
```

