library(tidyverse)
library(psych)
library(moments)
library(ggcorrplot)
library(corrr)
library(normtest)
library(lmtest)
library(leaps)
library(Metrics)
library(car)
library(cowplot)

## Trabajo práctico: Computación científica actuarial.

## -------------------------------------------------------------------------------------------------------##

## Ejercicio N°1: Análisis exploratorio del dataset y preparación de datos.

## Importación de datos.
data <- read_csv('kc_house_data.csv')

## [1] Chequeo de datos no disponibles. 
sum(is.na(data)) ## Todos los datos están disponibles.

## Chequeo de variables categóricas con {plyr}::count.
data %>%
  count(waterfront)

data %>%
  count(view) ## Se supone que es un factor binario "0-1". Hay número mayores a 1.

data %>%
  count(condition)

data %>%
  count(grade) ## No debería haber puntuaciones iguales a 12 o 13(Corresponde del 1 al 11).

## Chequeo de variables cuantitativas.
data %>%
  count(bathrooms) ## No tiene sentido que haya casa sin baños. (*)

data %>%
  count(floors) ## Número decimal de pisos.

data %>%
  count(bedrooms) ## No tiene sentido que haya una casa sin habitaciones. (*)

clean_data <- data %>%
  mutate(condition = as.factor(condition),
         waterfront = as.factor(waterfront),
         view = as.factor(view),
         grade = as.factor(grade)
         )  %>% 
  mutate(sale_year = lubridate::year(date),
         sale_month = lubridate::month(date),
         sale_day = lubridate::day(date))

## (*) Los baños en EEUU se cuentan por categoría. Por ello es que hay notación decimal.
## Por ejemplo: si un baño tiene sólo inodoro y lavamanos se considera "medio baño" y se representa con "0.5".
## (*) Datos que no corresponden con la descripción de la variable serán detallados en el informe.

## [2] Conversión de unidades del sistema imperial al sistema métrico.
## 1 pie cuadrado = 0.092903 metros cuadrados.
df_metrico <- clean_data %>%
  select(-contains("sqft")) %>%
  bind_cols(
    clean_data %>%
    transmute_at(vars(contains("sqft")), function(x) x / 10.764) %>%
    rename_all(.funs = list(~sub("sqft", "sqm", .))))

## [3] Metros cuadrados totales y Antiguedad
df_metrico <- df_metrico %>%
  mutate(sqm_total = sqm_above + sqm_basement, home_age = ifelse( yr_renovated > yr_built,
    lubridate::year(Sys.Date()) - yr_renovated,
    lubridate::year(Sys.Date()) - yr_built))

## [4] Estadística descriptiva.

## {base}
df_metrico %>% 
  select(-c(date, waterfront, view, yr_built, yr_renovated, condition, grade, sale_year,
            sale_month, sale_day)) %>%
  base::summary()

## {psych}
df_metrico %>% 
  select(-c(date, waterfront, view, yr_built, yr_renovated, condition, grade, sale_year,
            sale_month, sale_day)) %>%
  psych::describe()


## Boxplots e histogramas.
theme_set(theme_bw())

quant_vars <- df_metrico %>% 
  select(-c(waterfront, view,condition, grade, yr_built, yr_renovated, sale_year, sale_month))

cat_vars <- df_metrico %>%
  select(-c(colnames(quant_vars), yr_built, yr_renovated, sale_year, sale_month))

## Variables continuas.

## Histogramas.
for(j in colnames(quant_vars %>%
                  select(-price))){
assign(paste0("hist_", as.character(j)), ggplot(df_metrico, aes_string(j)) +
  geom_histogram() +
  ggtitle(paste0('Histograma de ',j)) +
  scale_y_continuous("Frecuencia"))
}

hist_price <- ggplot(df_metrico, aes(price)) +
  geom_histogram(binwidth = 50000) +
  ggtitle("Histograma de price") +
  scale_x_continuous("Precio en U$S", labels = scales::dollar_format()) +
  scale_y_continuous("Frecuencia")

## Boxplots.
for(j in colnames(quant_vars %>%
                  select(-c(price, date)))){
assign(paste0("boxplot_",j), ggplot(df_metrico, aes_string(1,j)) +
          geom_boxplot() +
          ggtitle(paste0('Boxplot de ',j)) +
          scale_x_continuous("", breaks = c()) +
          scale_y_continuous("")
  )
}

boxplot_price <- ggplot(df_metrico, aes(1, price)) +
  geom_boxplot() +
  ggtitle("Boxplot de 'price'") +
  scale_x_continuous("", breaks= c()) +
  scale_y_continuous("", labels = scales::dollar_format())

## Variables categóricas.
## Histograma.
for(j in colnames(cat_vars)){
  assign(paste0("hist_", j), ggplot(df_metrico %>%
               mutate(grade = as.factor(grade)), aes_string(j)) +
          geom_bar() +
          ggtitle(paste0('Histograma de ',j)) +
          ylab("Frecuencia")
  )
}

hist_sale_year <- ggplot(df_metrico %>%
         mutate(sale_year = as.factor(sale_year)), aes(sale_year)) +
  geom_bar() +
  ggtitle("Histograma de sale_year") +
  scale_y_continuous("Frecuencia")

hist_sale_month <- ggplot(df_metrico %>%
         mutate(sale_month = as.factor(sale_month)), aes(sale_month)) +
  geom_bar() +
  ggtitle("Histograma de sale_month") +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous("Frecuencia")

cowplot::plot_grid(hist_price, hist_bedrooms, hist_bathrooms, hist_floors, hist_condition, hist_view, 
                   hist_waterfront, hist_grade, hist_home_age, hist_date, ncol=2)

cowplot::plot_grid(hist_sale_day, hist_sale_month, hist_sale_year, hist_sqm_total, hist_sqm_above,
                   hist_sqm_basement, hist_sqm_living, hist_sqm_living15,
                   hist_sqm_lot, hist_sqm_lot15, ncol = 2)

cowplot::plot_grid(boxplot_bathrooms, boxplot_bedrooms, boxplot_floors,
          boxplot_home_age, boxplot_price, boxplot_sqm_total)

cowplot::plot_grid(boxplot_sqm_above, boxplot_sqm_basement, boxplot_sqm_living,
          boxplot_sqm_living15, boxplot_sqm_lot, boxplot_sqm_lot15)

rm(list = ls(pattern="^hist_|^boxplot_"))

## [5] Tabla de frecuencias.
freq_table <- lapply(df_metrico, table)

## [6] Momentos centrados.
df_quants <- df_metrico %>% 
  select(-c(date, waterfront, view, yr_built, yr_renovated, condition, grade))

momentos_centrados <- t(all.moments(df_quants, order.max = 4, central = TRUE))
colnames(momentos_centrados) <- c("mc0", "mc1", "mc2", "mc3", "mc4")
rownames(momentos_centrados) <- names(df_quants)

momentos_absolutos <- t(all.moments(df_quants, order.max = 4, absolute = TRUE))
colnames(momentos_absolutos) <- c("ma0", "ma1", "ma2", "ma3", "ma4")
rownames(momentos_absolutos) <- names(df_quants)

## [7] Correlación.
king_corrm <- df_metrico %>%
  select(-c(date, yr_built, yr_renovated)) %>%
  mutate_if(is.factor, as.double) %>%
  cor()

ggcorrplot(king_corrm, method = "square", type = "lower", lab = TRUE) +
  ggtitle("Matriz de correlaciones.")

## [8] Partición de datos.
set.seed(1234)
train_indices <- sample(1:nrow(df_metrico), round(nrow(df_metrico)*.8))

datos_train <- df_metrico[train_indices,]
datos_test <- df_metrico[-train_indices,]

## -------------------------------------------------------------------------------------------------------##

## Ejercicio N°2: Desarrollo de funciones y regresión.

## [1] Función "normalidad":
normalidad <- function(model, method = "jb"){
  residuos <- resid(model)
  if(method == "jb"){
    test <- jb.norm.test(residuos)
  } else if(method == "wb"){
    test <- wb.norm.test(residuos)
  } else{return("Error. Ingresar 'jb' para realizar un test de Jarque-Bera o 'wb' para Weisberg-Bingham")}
  if(test$p.value > 0.1){
    return("No se rechaza H0.")
    } else{return("Se rechaza H0.")}
}

## [2] Funciones "incorrelacion" y "homocedasticidad":
incorrelacion<-function(modelo,metodo="dw"){
  if(metodo=="dw"){
    test<-dwtest(modelo) 
  }else{
    if(metodo=="bg"){
      test<-bgtest(modelo)
    }else{
      return("Error. Ingresar 'dw' para realizar un test de Durbin-Watson o 'bg' para Beusch-Godfrey")
    }
  }
  if(test$p.value>0.1){
    return("No se rechaza H0")
  }else{
    return("Se rechaza H0")
  }
}

homocedasticidad<-function(modelo,metodo="bp"){
  if(metodo=="bp"){
    test<-bptest(modelo)
  }else{
    if(metodo=="white"){
      test<-bptest(modelo, ~fitted(modelo)+I(fitted(modelo)^2))
    }else{
      return("Error. Ingresar 'bp' para realizar un test de Breush-Pagan o 'white' para realizar el test de White.")
    }
  }
  if(test$p.value>0.1){
    return("No se rechaza H0")
  }else{
    return("Se rechaza H0")
  }
}

## [3] Función "supuestos":
supuestos<-function(modelo){
  a<-c(jb.norm.test(resid(modelo))$p.value,dwtest(modelo)$p.value,bptest(modelo)$p.value)
  b<-c(normalidad(modelo),incorrelacion(modelo),homocedasticidad(modelo))
  c<-c("Los errores se encuentran distribuidos normalmente",
       "No hay autocorrelacion de errores",
       "Homocedasticidad")
  A<- data.frame(a,b,c)
  colnames(A)<-c("P-Value","Decision Efectuada","Hipotesis Nula")
  row.names(A)<-c("Normalidad","Incorrelacion","Homocedasticidad")
  return(A)
}

## [4] Función "lm_plus":
lm_plus<-function(modelo){
  return(list(modelo,supuestos(modelo)))
}

## [5] Función "aic":
aic <- function(lista){
  if(is.list(lista) == F){
    return("Error. Ingresar una lista compuesta por modelos lineales.")
  } else{
    lista_resultado <- list()
    for (j in 1:length(lista)){
      temp_model <- lista[[j]]
      lista_resultado[[j]] <- list(temp_model$call,
                                 AIC = AIC(temp_model),
                                 as_tibble(summary(temp_model)$coefficients[,c(1,4)]) %>%
                                   mutate(Decision = ifelse(`Pr(>|t|)` > 0.05, "No significativo", "Significativo")),
                                 supuestos(temp_model) %>%
                                   mutate(Validación = ifelse(`P-Value` > 0.1, "Supuesto validado", "Supuesto no validado"))
                                 )
    }
    return(lista_resultado)
  }
}

## [6] Las 10 variables que mejor explican el precio de venta de una casa del condado de King(2014-2015):

## Primero eliminemos las variables con dependencia lineal (multicolinealidad).
## Las variables que cuantifican los metros cuadrados de la vivienda cumplen esta característica como
## se vio en la matriz de correlaciones.
model_data <- df_metrico[, c(1,3:22,2)] %>%
  select(-c(sqm_above, sqm_living)) %>% ## sqm_above y sqm_living son LD con sqm_total.
  select(-date)

full_model_1 <- lm(price ~ . , model_data)
car::vif(full_model_1) 

## VIF de > 10 indica que la variable explicativa puede ser explicada por las demás.
## Es lógico que el año en que se construyó la casa explique la antigüedad de la casa,
## por eso será eliminada.

model_data <- model_data %>%
  select(-yr_built)
full_model_2 <- lm(price ~ ., model_data)
car::vif(full_model_2)
## Ya no quedan variables con VIF > 10.

model_list <- list()
for(i in colnames(model_data)){
  if (i != "price"){
    temp_formula <- reformulate(i, "price")
    model_list[[i]] <- do.call("lm", c(temp_formula, quote(model_data)))
}
}

models_info <- aic(model_list)

aic_df <- data.frame(feature = colnames(model_data[-ncol(model_data)]), aic = 0)

for(i in 1:(ncol(model_data)-1)){
  aic_df[i,2] <- models_info[[i]]$AIC
}

aic_df %>%
  top_n(10, -aic) %>%
  arrange(aic)

## Las 10 variables más importantes, según el AIC de modelos lineales simples, son:
## grade
## sqm_total
## sqm_living15
## bathrooms
## view
## sqm_basement
## bedrooms
## waterfront
## floors
## yr_renovated

## [7] Modelo lineal múltiple, elección de variables con "forward selection":
## Aún cuando se eliminaron variables por multicolinealidad, mediante una inspección previa, es posible
## que aún haya alguna variable explicativa que sea explicada por el resto de variables independientes.
## El modelo lineal múltiple pone en evidencia esta característica: si agregamos una variable que no aporta
## información al modelo, el AIC la penalizará y el método de "forward selection" elegirá otra variable.
## Este método no elige el mejor modelo sino uno que sea lo suficientemente adecuado sin ser computacionalmente
## prohibitivo.

forward_selection_model <- regsubsets(price~., data = model_data,
                                      nvmax = 20,
                                      method = "forward")
summary(forward_selection_model)

## Las primeras 10 variables elegidas por el método son:
## sqm_total, waterfront, home_age, grade, yr_renovated, bathrooms, bedrooms, view, sqm_lot15, condition.

## Luego, con las variables elegidas, se ajustará un modelo lineal múltiple con training data.
train_data <- datos_train %>%
  select(sqm_total, waterfront, home_age, grade, yr_renovated, bathrooms, bedrooms, view, sqm_lot15, condition, price)


mlr_model <- lm(price ~ sqm_total + grade + home_age + yr_renovated + bathrooms + bedrooms + 
                  view + waterfront + sqm_lot15 + condition, train_data)

par(mfrow=c(2,2))

plot(mlr_model)

log_mlr_model <- lm(log(price) ~ sqm_total + grade + home_age + yr_renovated + bathrooms + bedrooms + 
                      view + waterfront + sqm_lot15 + condition, train_data)

par(mfrow=c(2,2))

plot(log_mlr_model)

supuestos(log_mlr_model)

summary(log_mlr_model)

## Puesta a prueba del modelo con testing data.
test_data <- datos_test %>%
  select(sqm_total, waterfront, home_age, grade, yr_renovated, bathrooms, bedrooms, view, sqm_lot15, condition, price)

X_test <- test_data %>%
  select(-price)

Y_test <- as.numeric(t(test_data[, which(colnames(test_data) == 'price')]))
  
log_model_pred <- predict(log_mlr_model, newdata = X_test)

model_pred <- exp(log_model_pred)

model_fitted <- exp(log_mlr_model$fitted.values)

test_MAPE <- Metrics::mape(Y_test, model_pred)

train_MAPE <- Metrics::mape( as.numeric(t(train_data[, which(colnames(train_data) == 'price')])),
                             exp(log_mlr_model$fitted.values))

## La poca diferencia entre las medidas de evaluación para training y testing data indica que no hay
## overfitting.
## test_MAPE = 26.25%
## train_MAPE = 25.87%

## -------------------------------------------------------------------------------------------------------##