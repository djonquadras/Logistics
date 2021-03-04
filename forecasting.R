# Libraries
#source('library.R')

# Importing datasets

# MO0021 <- tibble(read_excel("D:/GoogleDrive/Academico/UFSC/11-Semestre(2020-2)/Logistica/Trabalho/Logistics/data/MO0021.xlsx"))
# MO0091 <- tibble(read_excel("D:/GoogleDrive/Academico/UFSC/11-Semestre(2020-2)/Logistica/Trabalho/Logistics/data/MO0091.xlsx"))
# MO1401 <- tibble(read_excel("D:/GoogleDrive/Academico/UFSC/11-Semestre(2020-2)/Logistica/Trabalho/Logistics/data/MO1401.xlsx"))

# Adjusts date

#MO0021 <- MO0021 %>% mutate(Date = ymd(Date))
#MO0091 <- MO0091 %>% mutate(Date = ymd(Date))
#MO0091 <- MO0091 %>% mutate(Date = ymd(Date))

# Printing


graficoDemanda <- function(tabela){
  tabela <- tabela %>% mutate(Date = ymd(Date))
  p <- ggplot(tabela)+
    geom_line(aes(x = Date, y = TO), color = "red") +
    geom_line(aes(x = Date, y = SP), color = "blue") +
    geom_line(aes(x = Date, y = AM), color = "green") +
    geom_point(aes(x = Date, y = TO),size = 1, color = "red")+
    geom_point(aes(x = Date, y = SP),size = 1, color = "blue")+
    geom_point(aes(x = Date, y = AM),size = 1, color = "green")+
    xlab("Data")+
    ylab("Demanda")+
    scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y")
  p
}

#graficoDemanda(MO0021)

