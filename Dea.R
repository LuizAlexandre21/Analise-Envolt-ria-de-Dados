library(xts)
library(readxl)
library(ggplot2)
library(stats)
library(dplyr)
library(Benchmarking)

###################Importando os Dados
DEA2006<-read_xlsx("variaveis times.xlsx",sheet=2)
DEA2006<-as.data.frame(DEA2006)
DEA2006[is.na(DEA2006)]<-0
str(DEA2006)

###################Estrutura de Dados
#####Output1=Pontos
#####Output2=Gols
#####Insumo1=Cotas
#####Insumo2=Região
#####Insumo3=Part.Série A
#####Insumo4=Capital
##########Matrizes
x=matrix(nrow=39,ncol=4)
x[,1]=DEA2006[,2]
x[,2]=DEA2006[,3]
x[,3]=DEA2006[,5]
x[,4]=DEA2006[,4]

y=matrix(nrow=39,ncol=2)
y[,1]=DEA2006[,6]
y[,2]=DEA2006[,7]

#####################Estimando o DEA
est06<-dea(x,y,RTS="vrs",ORIENTATION="out")
print(est06)
summary(est06)
est06$eff ##########Eficiência

efi_tec06<-1/est06$eff ####Eficiencia técnica
print(efi_tec06)
summary(efi_tec06)
peers(est06,NAMES=TRUE) ####Benchmarks

incre06<-data.frame(y*(est06$eff-1)) ##### Incremento existente
meta06<-data.frame(y+y*(est06$eff-1)) #####S

(1-est06$eff)*x
(1-est06$eff)*x
