library(scales)
paraReal <- function(x){
  paste("R\\", dollar(x, big.mark = ".", decimal.mark = ","), sep="")
}

forecasting <- read_excel("forecasting.xlsx")

#Informações dos Produtos

p21 <- list(demanda = list(sp = forecasting[["previsao0021SP"]],
                           am = forecasting[["previsao0021AM"]],
                           to = forecasting[["previsao0021TO"]]),
            custoCaixa = 115.80, precoCaixa = 172.65, custoPedido = 804, pesoCaixa = 32.4, repre = NULL)
p91 <- list(demanda = list(sp = forecasting[["previsao0091SP"]],
                           am = forecasting[["previsao0091AM"]],
                           to = forecasting[["previsao0091TO"]]),
            custoCaixa = 197.45, precoCaixa = 275.80, custoPedido = 2231, pesoCaixa = 62.3, repre = NULL)
p14 <- list(demanda = list(sp = forecasting[["previsao1401SP"]],
                           am = forecasting[["previsao1401AM"]],
                           to = forecasting[["previsao1401TO"]]),
            custoCaixa = 104.16, precoCaixa = 122.82, custoPedido = 914, pesoCaixa = 35.68, repre = NULL)


# Informações dos Mercados

sp <- list(ta = 2, ns = 0.98, q = NULL, es = 0, pp = 0)
am <- list(ta = 31, ns = 0.95, q = NULL, es = NULL, pp = NULL)
to <- list(ta = 16, ns = 0.99, q = NULL, es = NULL, pp = NULL)

# Custo de Routing

q20pes <- list(p21 = 608, p91 = 320, p14 = 560)

mediaq21sp <- ceiling((mean(p21$demanda$sp))/q20pes$p21)
mediaq91sp <- ceiling((mean(p91$demanda$sp))/q20pes$p91)
mediaq14sp <- ceiling((mean(p14$demanda$sp))/q20pes$p14)
mediaqsp <- mediaq91sp + mediaq21sp + mediaq14sp

mediaq21to <- ceiling((mean(p21$demanda$to) + mean(p21$demanda$am))/q20pes$p21)
mediaq91to <- ceiling((mean(p91$demanda$to) + mean(p91$demanda$am))/q20pes$p91)
mediaq14to <- ceiling((mean(p14$demanda$to) + mean(p14$demanda$am))/q20pes$p14)
mediaqto <- mediaq91to + mediaq21to + mediaq14to



mediaq21am <- ceiling((mean(p21$demanda$am))/q20pes$p21)
mediaq91am <- ceiling((mean(p91$demanda$am))/q20pes$p91)
mediaq14am <- ceiling((mean(p14$demanda$am))/q20pes$p14)
mediaqam <- mediaq91am + mediaq21am + mediaq14am

custoFrete <- list(sp = 1200*mediaqsp,
                   to = 5345*mediaqto,
                   am = 13135*mediaqam,
                   total = 1200*mediaqsp + 5345*mediaqto + 13135*mediaqam)

melhorRota <- list(qsp = mediaqsp, 
                    qto = mediaqto,
                    qam = mediaqam,
                    custoFrete = custoFrete)


# Estoque e Nível de Serviço

CI = 0.2
totalsp <- sum(p21$demanda$sp + p91$demanda$sp + p14$demanda$sp)
totalto <- sum(p21$demanda$to + p91$demanda$to + p14$demanda$to)
totalam <- sum(p21$demanda$am + p91$demanda$am + p14$demanda$am)
demandas <-  c(totalsp, totalto, totalam)
total = totalam + totalto + totalsp
repre = (demandas/total)

qSup <- function(CO, D, CI, U) {
  return(ceiling(sqrt((2*CO*D)/(CI*U))))
}
es <- function(z, sigma, ta){
  return(ceiling(z*sigma*sqrt(ta)))
}
emax <- function(es, q){
  return(ceiling(es + q))
}
emedio <- function(es, q){
  return(ceiling(es + q/2))
}
pp <- function(d,ta,es){
  return(ceiling((d/365)*ta+es))
}


am$q <- list(MO0021 = ceiling(qSup(p21$custoPedido, sum(p21$demanda$am), CI, p21$custoCaixa)),
             MO0091 = ceiling(qSup(p91$custoPedido, sum(p91$demanda$am), CI, p91$custoCaixa)),
             MO1401 = ceiling(qSup(p14$custoPedido, sum(p14$demanda$am), CI, p14$custoCaixa)))

am$es <- list(MO0021 = ceiling(es(qnorm(am$ns), sd((p21$demanda$am)), am$ta)),
              MO0091 = ceiling(es(qnorm(am$ns), sd((p91$demanda$am)), am$ta)),
              MO1401 = ceiling(es(qnorm(am$ns), sd((p14$demanda$am)), am$ta)))

am$pp <- list(MO0021 = ceiling(pp(sum(p21$demanda$am), am$ta, am$es$MO0021)),
              MO0091 = ceiling(pp(sum(p91$demanda$am), am$ta, am$es$MO0091)),
              MO1401 = ceiling(pp(sum(p14$demanda$am), am$ta, am$es$MO1401)))

sp$q <- list(MO0021 = ceiling(qSup(p21$custoPedido, sum(p21$demanda$sp), CI, p21$custoCaixa)),
             MO0091 = ceiling(qSup(p91$custoPedido, sum(p91$demanda$sp), CI, p91$custoCaixa)),
             MO1401 = ceiling(qSup(p14$custoPedido, sum(p14$demanda$sp), CI, p14$custoCaixa)))

sp$es <- 0

sp$pp <- 0

to$q <- list(MO0021 = ceiling(qSup(p21$custoPedido, sum(p21$demanda$to), CI, p21$custoCaixa)),
             MO0091 = ceiling(qSup(p91$custoPedido, sum(p91$demanda$to), CI, p91$custoCaixa)),
             MO1401 = ceiling(qSup(p14$custoPedido, sum(p14$demanda$to), CI, p14$custoCaixa)))

to$es <- list(MO0021 = ceiling(es(qnorm(to$ns), sd((p21$demanda$to)), to$ta)),
              MO0091 = ceiling(es(qnorm(to$ns), sd((p91$demanda$to)), to$ta)),
              MO1401 = ceiling(es(qnorm(to$ns), sd((p14$demanda$to)), to$ta)))

to$pp <- list(MO0021 = ceiling(pp(sum(p21$demanda$to), to$ta, to$es$MO0021)),
              MO0091 = ceiling(pp(sum(p91$demanda$to), to$ta, to$es$MO0091)),
              MO1401 = ceiling(pp(sum(p14$demanda$to), to$ta, to$es$MO1401)))


repreRegiao <- list(sp = totalsp/total,
                    am = totalam/total,
                    to = totalto/total)

p21$repre <- list(sp = sum(p21$demanda$sp)/sum(p21$demanda$sp + p21$demanda$am + p21$demanda$to),
                  am = sum(p21$demanda$am)/sum(p21$demanda$sp + p21$demanda$am + p21$demanda$to),
                  to = sum(p21$demanda$to)/sum(p21$demanda$sp + p21$demanda$am + p21$demanda$to))

p91$repre <- list(sp = sum(p91$demanda$sp)/sum(p91$demanda$sp + p91$demanda$am + p91$demanda$to),
                  am = sum(p91$demanda$am)/sum(p91$demanda$sp + p91$demanda$am + p91$demanda$to),
                  to = sum(p91$demanda$to)/sum(p91$demanda$sp + p91$demanda$am + p91$demanda$to))

p14$repre <- list(sp = sum(p14$demanda$sp)/sum(p14$demanda$sp + p14$demanda$am + p14$demanda$to),
                  am = sum(p14$demanda$am)/sum(p14$demanda$sp + p14$demanda$am + p14$demanda$to),
                  to = sum(p14$demanda$to)/sum(p14$demanda$sp + p14$demanda$am + p14$demanda$to))


CD21 <- list(q = ceiling((to$q$MO0021 + am$q$MO0021)),
             es = ceiling((to$es$MO0021 + am$es$MO0021)),
             PP = ceiling((to$pp$MO0021 + am$pp$MO0021)),
             emed = ceiling((to$es$MO0021 + am$es$MO0021) + (to$q$MO0021 + am$q$MO0021)/2),
             emax = ceiling((to$es$MO0021 + am$es$MO0021) + (to$q$MO0021 + am$q$MO0021)))

CD91 <- list(q = ceiling((to$q$MO0091 + am$q$MO0091)),
             es = ceiling((to$es$MO0091 + am$es$MO0091)),
             PP = ceiling((to$pp$MO0091 + am$pp$MO0091)),
             emed = ceiling((to$es$MO0091 + am$es$MO0091) + (to$q$MO0091 + am$q$MO0091)/2),
             emax = ceiling((to$es$MO0091 + am$es$MO0091) + (to$q$MO0091 + am$q$MO0091)))

CD14 <- list(q = ceiling((to$q$MO1401 + am$q$MO1401)),
             es = ceiling((to$es$MO1401 + am$es$MO1401)),
             PP = ceiling((to$pp$MO1401 + am$pp$MO1401)),
             emed = ceiling((to$es$MO1401 + am$es$MO1401) + (to$q$MO1401 + am$q$MO1401)/2),
             emax = ceiling((to$es$MO1401 + am$es$MO1401) + (to$q$MO1401 + am$q$MO1401)))





# Custo Unitário



nCxam <- list(p21 = mean(p21$demanda$am),
              p91 = mean(p91$demanda$am),
              p14 = mean(p14$demanda$am))

nCxsp <- list(p21 = mean(p21$demanda$sp),
              p91 = mean(p91$demanda$sp),
              p14 = mean(p14$demanda$sp))

nCxto <- list(p21 = mean(p21$demanda$am + p21$demanda$to),
              p91 = mean(p91$demanda$am + p91$demanda$to),
              p14 = mean(p14$demanda$am + p14$demanda$to))

qContam <- list(p21 = ceiling(nCxam$p21/q20pes$p21),
                p91 = ceiling(nCxam$p91/q20pes$p91),
                p14 = ceiling(nCxam$p14/q20pes$p14))

qContsp <- list(p21 = ceiling(nCxsp$p21/q20pes$p21),
                p91 = ceiling(nCxsp$p91/q20pes$p91),
                p14 = ceiling(nCxsp$p14/q20pes$p14))

qContto <- list(p21 = ceiling(nCxto$p21/q20pes$p21),
                p91 = ceiling(nCxto$p91/q20pes$p91),
                p14 = ceiling(nCxto$p14/q20pes$p14))

cFreteReg <- list(sp = 1970, to = 655+1625+3065, am = 13135)

custoFreteam <- list(p21 = qContam$p21*cFreteReg$am,
                     p91 = qContam$p91*cFreteReg$am,
                     p14 = qContam$p14*cFreteReg$am)

custoFretesp <- list(p21 = qContsp$p21*cFreteReg$sp,
                     p91 = qContsp$p91*cFreteReg$sp,
                     p14 = qContsp$p14*cFreteReg$sp)

custoFreteto <- list(p21 = qContto$p21*cFreteReg$to,
                     p91 = qContto$p91*cFreteReg$to,
                     p14 = qContto$p14*cFreteReg$to)

pesoTranspam <- list(p21 = nCxam$p21*p21$pesoCaixa,
                     p91 = nCxam$p91*p91$pesoCaixa,
                     p14 = nCxam$p14*p14$pesoCaixa)

pesoTranspsp <- list(p21 = nCxsp$p21*p21$pesoCaixa,
                     p91 = nCxsp$p91*p91$pesoCaixa,
                     p14 = nCxsp$p14*p14$pesoCaixa)

pesoTranspto <- list(p21 = nCxto$p21*p21$pesoCaixa,
                     p91 = nCxto$p91*p91$pesoCaixa,
                     p14 = nCxto$p14*p14$pesoCaixa)

custoTranspKg <- list(p21 = (custoFreteam$p21/pesoTranspam$p21 + 
                               custoFretesp$p21/pesoTranspsp$p21 + 
                               custoFreteto$p21/pesoTranspto$p21),
                      p91 = (custoFreteam$p91/pesoTranspam$p91 + 
                                 custoFretesp$p91/pesoTranspsp$p91 + 
                                 custoFreteto$p91/pesoTranspto$p91),
                      p14 = (custoFreteam$p14/pesoTranspam$p14 +
                               custoFretesp$p14/pesoTranspsp$p14 +
                               custoFreteto$p14/pesoTranspto$p14))

custoUnitario <<- list(p21 = (custoFreteam$p21/nCxam$p21 + 
                               custoFretesp$p21/nCxsp$p21 + 
                               custoFreteto$p21/nCxto$p21),
                      p91 = (custoFreteam$p91/nCxam$p91 + 
                               custoFretesp$p91/nCxsp$p91 + 
                               custoFreteto$p91/nCxto$p91),
                      p14 = (custoFreteam$p14/nCxam$p14 +
                               custoFretesp$p14/nCxsp$p14 +
                               custoFreteto$p14/nCxto$p14))

returnCustoUnitario <- function(){
  return(custoUnitario)
}

returnCustoTranspKg <- function(){
  return(custoTranspKg)
}

# Custo Estoque

custoTransportePropor <- list(p21 = (p21$repre$sp + p21$repre$am + p21$repre$to)*custoUnitario$p21,
                              p91 = (p91$repre$sp + p91$repre$am + p91$repre$to)*custoUnitario$p91,
                              p14 = (p14$repre$sp + p14$repre$am + p14$repre$to)*custoUnitario$p14)

valorEstoque <- list(p21 = p21$precoCaixa + custoTransportePropor$p21,
                     p91 = p91$precoCaixa + custoTransportePropor$p91,
                     p14 = p14$precoCaixa + custoTransportePropor$p14)

valorEstoquetotal <- list(p21 = CD21$emed*valorEstoque$p21,
                          p91 = CD91$emed*valorEstoque$p91,
                          p14 = CD14$emed*valorEstoque$p14)

tma <- 0.053

valorEstoqueTMA <- list(p21 = valorEstoquetotal$p21*tma,
                        p91 = valorEstoquetotal$p91*tma,
                        p14 = valorEstoquetotal$p14*tma)
custoEstoque <- (valorEstoqueTMA$p21 + valorEstoqueTMA$p91 + valorEstoqueTMA$p14)/12

returnCustoEstoque <- function(){
  return(custoEstoque)
}

#Custo Armazenagem

valorArma <- c(39.37,33.24,18.04,21.91,74.06,13.69,2.35)
custoArma <- valorArma*350
custoArmazenagem <- sum(custoArma)


# Custo Estoque em Transito

tmaDia <- tma/365

valorEstoquePropSP <- list(p21 = valorEstoquetotal$p21*p21$repre$sp,
                           p91 = valorEstoquetotal$p91*p91$repre$sp,
                           p14 = valorEstoquetotal$p14*p14$repre$sp)

valorEstoquePropAM <- list(p21 = valorEstoquetotal$p21*p21$repre$am,
                           p91 = valorEstoquetotal$p91*p91$repre$am,
                           p14 = valorEstoquetotal$p14*p14$repre$am)

valorEstoquePropTO <- list(p21 = valorEstoquetotal$p21*p21$repre$to,
                           p91 = valorEstoquetotal$p91*p91$repre$to,
                           p14 = valorEstoquetotal$p14*p14$repre$to)

custoEstoqueTransitoSP <- list(p21 = valorEstoquePropSP$p21*tmaDia,
                               p91 = valorEstoquePropSP$p91*tmaDia,
                               p14 = valorEstoquePropSP$p14*tmaDia)
custoEstoqueTransitoSPTotal <- 2*(custoEstoqueTransitoSP$p21 + custoEstoqueTransitoSP$p91 + custoEstoqueTransitoSP$p14)

custoEstoqueTransitoAM <- list(p21 = valorEstoquePropAM$p21*tmaDia,
                               p91 = valorEstoquePropAM$p91*tmaDia,
                               p14 = valorEstoquePropAM$p14*tmaDia)
custoEstoqueTransitoAMTotal <- (custoEstoqueTransitoAM$p21 + custoEstoqueTransitoAM$p91 + custoEstoqueTransitoAM$p14)*16

custoEstoqueTransitoTO <- list(p21 = valorEstoquePropTO$p21*tmaDia,
                               p91 = valorEstoquePropTO$p91*tmaDia,
                               p14 = valorEstoquePropTO$p14*tmaDia)
custoEstoqueTransitoTOTotal <- 15*(custoEstoqueTransitoTO$p21 + custoEstoqueTransitoTO$p91 + custoEstoqueTransitoTO$p14)


custoEstoqueTransito <- custoEstoqueTransitoAMTotal + custoEstoqueTransitoSPTotal + custoEstoqueTransitoTOTotal

returnCustoEstoqueTransito <- function(){
  return(custoEstoqueTransito)
}

# Faturamento e Custo por Real Faturado

media21 <- p21$precoCaixa*(p21$demanda$sp + p21$demanda$to + p21$demanda$am)
media91 <- p91$precoCaixa*(p91$demanda$sp + p91$demanda$to + p91$demanda$am)
media14 <- p14$precoCaixa*(p14$demanda$sp + p14$demanda$to + p14$demanda$am)


custoLogistico <- custoArmazenagem + custoEstoque + melhorRota$custoFrete$total + custoEstoqueTransito
faturamento <- mean(media21 + media91 + media14) 
custoPorReal <- custoLogistico/faturamento


faturamentoECusto <- function(){
  
  return(list(custoLogistico = custoLogistico, faturamento = faturamento, custoPorReal = custoPorReal))
  
}


