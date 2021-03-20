source("library.R")

# Importação de Dados

dados <-  tibble(read_excel("data/MO0021.xlsx"))
dados1 <- dados %>% mutate(Date = ymd(Date))

# Seleciona o Estado

estado <- dados$SP


# Cria série temporal
serie <- ts(estado, start = c(2018, 3), frequency = 12)

autoplot(decompose(serie))

# --- Exponential smoothing
modelo_ES <- ets(serie)

# --- ARIMA
modelo_ARIMA <- auto.arima(serie)

# --- Neural network autoregression
modelo_NNAR <- nnetar(serie)

valor <-  c(accuracy(modelo_ES)[,"MAPE"],accuracy(modelo_ARIMA)[,"MAPE"],accuracy(modelo_NNAR)[,"MAPE"])
valor

# ---- Validação

checkresiduals(modelo_NNAR)

# ---- Previsão

previsao <- forecast(modelo_NNAR , h = 10)
previsao$mean[1]
autoplot(previsao)




MO0021 <-  tibble(read_excel("data/MO0021.xlsx"))
MO00211 <- MO0021 %>% mutate(Date = ymd(Date))
MO0021AM <- MO0021$AM
MO0021SP <- MO0021$SP
MO0021TO <- MO0021$TO

estado <- MO0021AM

serie <- ts(estado, start = c(2018, 3), frequency = 12)

modelo_ES <- ets(serie)
modelo_ARIMA <- auto.arima(serie)
modelo_NNAR <- nnetar(serie)

if (accuracy(modelo_ES)[,"MAPE"] < accuracy(modelo_ARIMA)[,"MAPE"] & accuracy(modelo_ES)[,"MAPE"] < accuracy(modelo_NNAR)[,"MAPE"]){
  bestModel <- modelo_ES
}
if (accuracy(modelo_ARIMA)[,"MAPE"] < accuracy(modelo_ES)[,"MAPE"] & accuracy(modelo_ARIMA)[,"MAPE"] < accuracy(modelo_NNAR)[,"MAPE"]){
  bestModel <- modelo_ARIMA
}
if (accuracy(modelo_NNAR)[,"MAPE"] < accuracy(modelo_ES)[,"MAPE"] & accuracy(modelo_NNAR)[,"MAPE"] < accuracy(modelo_ARIMA)[,"MAPE"]){
  bestModel <- modelo_NNAR
}
bestModel
previsao <- forecast(bestModel , h = 10)
previsao

