# Caregando os pacotes no R
library(tidyverse)
library(nlme)
library(psych)
library(agricolae)
library(ExpDes.pt)
library(pals)
library(car)
library(multcompView)
library(lsmeans)
library(rcompanion)
library(ggpubr)
library(rstatix)
library(lme4)
library(emmeans)
library(lmerTest)
library(xlsx)
library(openxlsx)
library(readxl)


# Craição da funçãod e erro
errFun <- function(x) {
  data.frame(ymin = mean(x) - sd(x)/sqrt(length(x)), 
             ymax = mean(x) + sd(x)/sqrt(length(x)), 
             y = mean(x))}

#abrir excel
dados= readxl::read_xlsx("camila_viana/Atrib.xlsx")
dados <- dados %>% 
  mutate(
    cobertura = sub('\\_.*','',tratamento),
    PDS = sub('.*\\_','',tratamento)
  )


# Separando o tratamento adicional
adicional <- dados %>% 
  filter(tratamento == "sem cobertura_pc")

# Dados sem o tratamento adicional
dados<- dados %>% 
  filter(tratamento != "sem cobertura_pc")

# Gr?fico de m?dias e barras e erro-pdar?o da m?dia
dados %>% 
  group_by(ciclo,tratamento,profundidade,linha_entrelinha) %>%
  ggplot(aes(x=ciclo, y=DS,fill=as.factor(profundidade)))+
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge()) + 
  stat_summary(fun.data = "errFun", geom = "linerange",
               position = position_dodge(.9)) + 
  facet_wrap(~ tratamento) +
  labs(x="Ciclo", y = "DS",fill="Profundidade")+
  theme_minimal() +
  scale_fill_manual(values=c('#999999','#E69F00','#FF6347'))


# Chaves de agrupamento
cob<-levels(factor(dados$cobertura))
prof<-levels(factor(dados$profundidade)) 
posicao<-levels(factor(dados$linha_entrelinha))
ciclos <- levels(factor(dados$ciclo))

sink("camila_viana/Resultado DS.txt")

# Definindo a matriz de coeficientes dos contrastes
contrastes<-cbind(
  c(-3,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0),
  c(0,-3,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0),
  c(0,0,-3,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0),
  c(0,0,0,-3,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0),
  c(0,0,0,0,-3,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),
  c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  
)

# La?os para v?rias an?lises
for(i in 1:length(cob)){
  for(j in 1:length(prof)){
    for(k in 1:length(posicao)){
      print("-----------------------------------------")
      print(paste(cob[i],prof[j],posicao[k]))
      print("-----------------------------------------")
      
      # Criando dados auxiliar
      da<-dados %>% 
        filter(cobertura==cob[i],
               profundidade==prof[j],
               linha_entrelinha==posicao[k])
      
      # Criando os dados com o trat adicional
      add <- adicional %>% 
        filter(profundidade==prof[j],linha_entrelinha==posicao[k])
      
      # Juntandos os dois dados para o desdobramento de graus de liberdade
      dd<-rbind(da,add)
      y<-dd$DS
      trat<-as.factor(paste(dd$PDS,dd$ciclo,sep="_"))
      contrasts(trat) <- contrastes
      modelo.contrastes<-aov(y~trat)
      print("------------------ Fatorial vs Testemunha adicional -------------")
      print(summary(modelo.contrastes, 
                    split= list(trat= 
                                  list("Add vs. Trat (ciclo_1)"=1,
                                       "Add vs. Trat (ciclo_2)"=2,
                                       "Add vs. Trat (ciclo_3)"=3,
                                       "Add vs. Trat (ciclo_4)"=4,
                                       "Add vs. Trat (ciclo_5)"=5,
                                       "Fatorial"= 6:20)))
      )
      cat("\n")
      
    }}
}


# La?os para v?rias an?lises
for(i in 1:length(cob)){
  for(j in 1:length(prof)){
    for(k in 1:length(posicao)){
      print("-----------------------------------------")
      print(paste(cob[i],prof[j],posicao[k]))
      print("-----------------------------------------")
      
      # Criando dados auxiliar
      da<-dados %>% 
        filter(cobertura==cob[i],
               profundidade==prof[j],
               linha_entrelinha==posicao[k])
      
      # Criando os dados com o trat adicional
      add <- adicional %>% 
        filter(profundidade==prof[j],linha_entrelinha==posicao[k])
      
      # Juntandos os dois dados para o desdobramento de graus de liberdade
      
      print("------------------ An?lise em Faixas -------------")
      faixas(da$PDS,da$ciclo,da$bloco,da$DS,
             quali=c(TRUE,TRUE), mcomp = "tukey", 
             fac.names = c("PDS","CICLO"))
      
      cat("\n")
    }}
}

sink(NULL)
