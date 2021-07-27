## Aplicación de GLM en Loss Reserving. ##
library(tidyverse)
library(ChainLadder)

## Preparación de datos.
data_2011 <- readxl::read_excel("ssn_20162017_desarrollo_siniestros_automotores.xlsx", sheet = 8)[15,c(7,11,15,19,23)]
data_2010 <- readxl::read_excel("ssn_20162017_desarrollo_siniestros_automotores.xlsx", sheet = 9)[15,c(7,11,15,19,23)]
data_2009 <- readxl::read_excel("ssn_20162017_desarrollo_siniestros_automotores.xlsx", sheet = 10)[15,c(7,11,15,19,23)]
data_2008 <- readxl::read_excel("ssn_20162017_desarrollo_siniestros_automotores.xlsx", sheet = 11)[15,c(7,11,15,19,23)]
data_2007 <- readxl::read_excel("ssn_20162017_desarrollo_siniestros_automotores.xlsx", sheet = 12)[15,c(7,11,15,19,23)]

matrix_data <- matrix(c(data_2007,data_2008,data_2009,data_2010,data_2011), 5, 5, byrow = TRUE)

rownames(matrix_data) <- c(2007:2011)
colnames(matrix_data) <- c(1:5)

true_data <- as.triangle(matrix_data)

model_data <- true_data

for(i in 5:2){
  for(j in (7-i):5){
    model_data[i,j] <- NA
  }
}

## Método Chain-Ladder.
mack_reserves <- MackChainLadder(model_data)

## Chain-Ladder como caso particular de GLM. Over-dispersed Poisson, i.e, tweedie p = 1.
od_poisson_reserves <- glmReserve(model_data)

## GLM - Distribución Tweedie con "p" estimado, donde 1 < p < 2, es decir, es una distribución compuesta Poisson/Gamma.
poisson_gamma_reserves <- glmReserve(model_data, var.power = NULL)

## GLM - Distribución normal, i.e, tweedie p = 0.
gaussian_reserves <- glmReserve(model_data, var.power = 0)

## Validación por Out Of Sample validation en el contexto de Loss Reserving.
triangle_rmse <- function(true_data, predicted_data){
  triangle_rmse <- list("pointwise_rmse"=c(), "total_rmse" = "")
  triangle_rmse[[1]] <- seq(1,5)
  if(class(predicted_data)[1] == "MackChainLadder"){
    for(l in 1:5){
      triangle_rmse[[1]][l] <- sqrt((true_data[l,5]-predicted_data[[3]][l,5])^2/5)
    }
  } else if (class(predicted_data) == "glmReserve"){
    for(l in 1:5){
      triangle_rmse[[1]][l] <- sqrt((true_data[l,5]-predicted_data[[4]][l,5])^2/5)
    }
  } else{print("error. predicted data is not 'MackChainLadder' nor 'glmReserve'")}
  
  triangle_rmse[[2]] <- sum(triangle_rmse[[1]])
  
  triangle_rmse
}

mack_rmse <- triangle_rmse(true_data, mack_reserves)

odp_rmse <- triangle_rmse(true_data, od_poisson_reserves)

pg_rmse <- triangle_rmse(true_data, poisson_gamma_reserves)

gaussian_rmse <- triangle_rmse(true_data, gaussian_reserves)
