
# Análise para N

``` r
# Caregando os pacotes no R
library(tidyverse)
library(nlme)
library(psych)
library(agricolae)
library(ExpDes.pt)
# library(pals)
library(car)
library(multcompView)
library(lsmeans)
library(rcompanion)
library(ggpubr)
library(rstatix)
library(lme4)
library(emmeans)
library(lmerTest)
```

### Entrada dos dados

``` r
dados<-read.table(
  "dados/horizontes.txt",
  h=TRUE,sep="\t")
glimpse(dados) # Vislumbre
```

    ## Rows: 936
    ## Columns: 31
    ## $ ciclo               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
    ## $ tratamento          <chr> "amendoim_cm", "amendoim_cm", "amendoim_cm", "amen~
    ## $ linha_entrelinha    <chr> "el", "el", "el", "el", "el", "el", "el", "el", "e~
    ## $ profundidade        <chr> "0_20", "0_20", "0_20", "20_30", "20_30", "20_30",~
    ## $ bloco               <chr> "b1", "b2", "b3", "b1", "b2", "b3", "b1", "b2", "b~
    ## $ ds                  <dbl> 1.62, 1.61, 1.65, 1.58, 1.67, 1.68, 1.53, 1.62, 1.~
    ## $ ds_max              <dbl> 1.84, 1.84, 1.84, 1.80, 1.81, 1.81, 1.77, 1.77, 1.~
    ## $ grau_de_compactacao <dbl> 88.36, 87.74, 89.89, 87.75, 92.06, 93.20, 86.20, 9~
    ## $ argila              <dbl> 224.4, 224.4, 224.4, 267.0, 267.0, 267.0, 322.0, 3~
    ## $ silte               <dbl> 102.8, 102.8, 102.8, 102.0, 102.0, 102.0, 107.0, 1~
    ## $ areia               <int> 673, 673, 673, 631, 631, 631, 571, 571, 571, 673, ~
    ## $ macro               <dbl> 0.13, 0.11, 0.15, 0.12, 0.08, 0.15, 0.11, 0.10, 0.~
    ## $ micro               <dbl> 0.25, 0.29, 0.24, 0.19, 0.30, 0.23, 0.33, 0.30, 0.~
    ## $ pt                  <dbl> 0.38, 0.40, 0.39, 0.31, 0.38, 0.38, 0.43, 0.40, 0.~
    ## $ dp                  <dbl> 2.69, 2.69, 2.69, 2.70, 2.70, 2.70, 2.69, 2.69, 2.~
    ## $ rp                  <dbl> 1.17, 1.13, 1.12, 1.03, 1.43, 1.23, 1.98, 1.68, 1.~
    ## $ dmp                 <dbl> 0.54, 0.48, 0.53, 0.31, 0.26, 0.36, 0.88, 0.74, 0.~
    ## $ macroagregado       <dbl> 3.09, 2.58, 3.16, 0.31, 0.30, 0.47, 1.20, 1.50, 1.~
    ## $ ssi                 <dbl> 0.02, 0.02, 0.02, 0.02, 0.01, 0.02, 0.01, 0.01, 0.~
    ## $ p_resina            <dbl> 2.6, 3.2, 2.8, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.1, ~
    ## $ mos                 <dbl> 14.4, 12.4, 16.0, 12.0, 10.0, 13.0, 11.0, 10.0, 13~
    ## $ ph                  <dbl> 4.32, 4.58, 5.00, 4.20, 4.70, 4.70, 4.50, 4.50, 5.~
    ## $ k                   <dbl> 0.56, 0.70, 0.56, 0.30, 0.30, 0.30, 0.35, 0.50, 0.~
    ## $ ca                  <dbl> 9.3, 12.1, 9.6, 5.0, 16.0, 5.0, 6.0, 9.0, 6.0, 11.~
    ## $ mg                  <dbl> 5.10, 6.60, 6.40, 3.00, 3.50, 4.00, 4.00, 7.00, 4.~
    ## $ h_al                <dbl> 25.0, 22.0, 16.4, 25.0, 22.0, 16.0, 22.0, 25.0, 15~
    ## $ c                   <dbl> 4.15, 4.07, 4.11, 4.25, 2.82, 3.53, 1.94, 1.94, 1.~
    ## $ est_c               <dbl> 7.36, 6.71, 7.03, 7.43, 4.94, 6.19, 13.21, 13.21, ~
    ## $ n                   <dbl> 0.64, 0.51, 0.58, 0.60, 0.52, 0.26, 0.60, 0.53, 0.~
    ## $ n_g_kg              <dbl> 6.36, 5.11, 5.80, 5.95, 5.19, 2.59, 6.03, 5.34, 1.~
    ## $ est_n               <dbl> 0.73, 0.61, 0.68, 1.02, 0.89, 0.45, 3.86, 3.42, 1.~

``` r
levels(factor(dados$ciclo))
```

    ## [1] "1" "2" "3" "4"

``` r
levels(factor(dados$tratamento))
```

    ##  [1] "amendoim_cm"      "amendoim_pd"      "amendoim_pp"      "crotalária_cm"   
    ##  [5] "crotalária_pd"    "crotalária_pp"    "milheto_cm"       "milheto_pd"      
    ##  [9] "milheto_pp"       "sem cobertura_pc" "sorgo_cm"         "sorgo_pd"        
    ## [13] "sorgo_pp"

``` r
levels(factor(dados$linha_entrelinha))
```

    ## [1] "el" "l"

``` r
levels(factor(dados$profundidade))
```

    ## [1] "0_20"  "20_30" "30_70"

``` r
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
```

``` r
# Gráfico de médias e barras e erro-pdarão da média
names(dados)
```

    ##  [1] "ciclo"               "tratamento"          "linha_entrelinha"   
    ##  [4] "profundidade"        "bloco"               "ds"                 
    ##  [7] "ds_max"              "grau_de_compactacao" "argila"             
    ## [10] "silte"               "areia"               "macro"              
    ## [13] "micro"               "pt"                  "dp"                 
    ## [16] "rp"                  "dmp"                 "macroagregado"      
    ## [19] "ssi"                 "p_resina"            "mos"                
    ## [22] "ph"                  "k"                   "ca"                 
    ## [25] "mg"                  "h_al"                "c"                  
    ## [28] "est_c"               "n"                   "n_g_kg"             
    ## [31] "est_n"               "cobertura"           "PDS"

``` r
atributos<-names(dados[29:(length(dados)-2)])
```

## Análise de variância

``` r
# Chaves de agrupamento
cob<-levels(factor(dados$cobertura))
prof<-levels(factor(dados$profundidade)) 
# posi<-levels(factor(dados$linha_entrelinha)) 
# posição<-levels(factor(dados$linha_entrelinha)) 
```

### Desdobramento por contrastes

Esse desdobramento é utilizado para comparação entre a testemunha e os
tratamentos fatoriais em cada ciclo de avaliação.

``` r
# Definindo a matriz de coeficientes dos contrastes
contrastes<-cbind(
          c(-3,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0),
          c(0,-3,0,0,0,1,0,0,0,1,0,0,0,1,0,0),
          c(0,0,-3,0,0,0,1,0,0,0,1,0,0,0,1,0),
          c(0,0,0,-3,0,0,0,1,0,0,0,1,0,0,0,1),
          c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
          c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
          c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
          c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
          c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
          c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
          c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
          c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
          c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
          c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
          c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
          )

# Laços para várias análises
for(l in 1:length(atributos)){
  for(i in 1:length(cob)){
    for(j in 1:length(prof)){
        # for(k in 1:length(posição)){
          print("-----------------------------------------")
          print(paste(atributos[l],"-",cob[i],prof[j],sep=" "))
          print("-----------------------------------------")
          
          # Criando dados auxiliar
          da1<-dados %>% 
            filter(cobertura==cob[i],
                  profundidade==prof[j])
          
          # fl<-(da1$linha_entrelinha == posi[k])
          # da1 <- da1[fl,]
          # Criando os dados com o trat adicional
          add <- adicional %>% 
            filter(profundidade==prof[j])
          # fl<-(add$linha_entrelinha == posi[k])
          # add <- add[fl,]
          
          # Juntandos os dois dados para o desdobramento de graus de liberdade
          dd<-rbind(da1,add)
          y<-dd[,5+l]
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
                             "Fatorial"= 5:15)))
          )
          cat("\n")
    # }
    }}
}
```

    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 0.2378 0.015850   2.436 0.00562 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0020 0.002006   0.308 0.58028   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000 0.000035   0.005 0.94194   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0003 0.000272   0.042 0.83843   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0036 0.003613   0.555 0.45834   
    ##   trat: Fatorial               11 0.2318 0.021075   3.240 0.00108 **
    ## Residuals                      80 0.5204 0.006505                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 0.3260 0.021736   2.341 0.00785 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0123 0.012272   1.322 0.25374   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0246 0.024568   2.646 0.10777   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0214 0.021356   2.300 0.13334   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0007 0.000735   0.079 0.77922   
    ##   trat: Fatorial               11 0.2671 0.024282   2.615 0.00671 **
    ## Residuals                      80 0.7429 0.009286                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.2828 0.01885   1.948 0.03007 * 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0018 0.00180   0.186 0.66746   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0003 0.00031   0.032 0.85786   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0961 0.09607   9.925 0.00229 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0018 0.00180   0.186 0.66746   
    ##   trat: Fatorial               11 0.1828 0.01662   1.717 0.08446 . 
    ## Residuals                      80 0.7744 0.00968                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value  Pr(>F)    
    ## trat                           15 0.2611 0.017409   3.108 0.00053 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0005 0.000501   0.089 0.76559    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0110 0.011001   1.964 0.16498    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0268 0.026835   4.790 0.03154 *  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000 0.000006   0.001 0.97496    
    ##   trat: Fatorial               11 0.2228 0.020254   3.615 0.00036 ***
    ## Residuals                      80 0.4482 0.005602                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15 0.1579 0.01052   1.814 0.0467 *
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0153 0.01531   2.639 0.1082  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0035 0.00347   0.599 0.4414  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0397 0.03967   6.838 0.0107 *
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0115 0.01150   1.982 0.1630  
    ##   trat: Fatorial               11 0.0879 0.00799   1.377 0.1998  
    ## Residuals                      80 0.4641 0.00580                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)
    ## trat                           15 0.0708 0.004719   0.601  0.866
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0087 0.008668   1.104  0.296
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0002 0.000200   0.025  0.874
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0098 0.009800   1.249  0.267
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0002 0.000200   0.025  0.874
    ##   trat: Fatorial               11 0.0519 0.004720   0.601  0.822
    ## Residuals                      80 0.6279 0.007848               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value   Pr(>F)    
    ## trat                           15 0.2221 0.014804   3.003 0.000767 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0001 0.000068   0.014 0.906769    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0002 0.000168   0.034 0.853992    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0168 0.016806   3.409 0.068558 .  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0002 0.000200   0.041 0.840892    
    ##   trat: Fatorial               11 0.2048 0.018620   3.777 0.000225 ***
    ## Residuals                      80 0.3944 0.004930                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.1526 0.010171   1.410 0.1631  
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0272 0.027222   3.773 0.0556 .
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0023 0.002335   0.324 0.5710  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0253 0.025313   3.509 0.0647 .
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0003 0.000272   0.038 0.8465  
    ##   trat: Fatorial               11 0.0974 0.008856   1.228 0.2829  
    ## Residuals                      80 0.5771 0.007214                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)
    ## trat                           15 0.0906 0.006041   0.664  0.811
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0001 0.000068   0.007  0.931
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0011 0.001089   0.120  0.730
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0017 0.001701   0.187  0.667
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0174 0.017422   1.916  0.170
    ##   trat: Fatorial               11 0.0703 0.006394   0.703  0.732
    ## Residuals                      80 0.7276 0.009095               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.1544 0.010294   1.992 0.0259 *
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0029 0.002939   0.569 0.4529  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000 0.000022   0.004 0.9479  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0118 0.011756   2.275 0.1354  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0028 0.002813   0.544 0.4628  
    ##   trat: Fatorial               11 0.1369 0.012443   2.409 0.0122 *
    ## Residuals                      80 0.4133 0.005166                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.2111 0.014072   2.064 0.0203 *
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0026 0.002568   0.377 0.5411  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0304 0.030422   4.463 0.0378 *
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0004 0.000401   0.059 0.8089  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0210 0.021013   3.082 0.0830 .
    ##   trat: Fatorial               11 0.1567 0.014243   2.089 0.0304 *
    ## Residuals                      80 0.5453 0.006817                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15 0.1497 0.00998   1.106 0.3648  
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0039 0.00390   0.432 0.5127  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0004 0.00045   0.050 0.8238  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0397 0.03967   4.397 0.0392 *
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0131 0.01307   1.449 0.2323  
    ##   trat: Fatorial               11 0.0926 0.00842   0.933 0.5137  
    ## Residuals                      80 0.7217 0.00902                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.002167 1.444e-04   2.530 0.00405 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000089 8.889e-05   1.557 0.21572   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000006 5.560e-06   0.097 0.75588   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000013 1.250e-05   0.219 0.64109   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000068 6.806e-05   1.192 0.27816   
    ##   trat: Fatorial               11 0.001992 1.811e-04   3.172 0.00131 **
    ## Residuals                      80 0.004567 5.708e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## trat                           15 0.0008625 5.750e-05   1.683 0.0712 .
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0001681 1.681e-04   4.919 0.0294 *
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001681 1.681e-04   4.919 0.0294 *
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000500 5.000e-05   1.463 0.2300  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000500 5.000e-05   1.463 0.2300  
    ##   trat: Fatorial               11 0.0004264 3.876e-05   1.135 0.3464  
    ## Residuals                      80 0.0027333 3.417e-05                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0013958 9.306e-05   3.384 0.000201 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0003125 3.125e-04  11.364 0.001155 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000347 3.472e-05   1.263 0.264515    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000681 6.806e-05   2.475 0.119635    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000014 1.390e-06   0.051 0.822760    
    ##   trat: Fatorial               11 0.0009792 8.902e-05   3.237 0.001086 ** 
    ## Residuals                      80 0.0022000 2.750e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.002283 1.522e-04   2.832 0.00140 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000035 3.472e-05   0.646 0.42393   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000235 2.347e-04   4.367 0.03982 * 
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000013 1.250e-05   0.233 0.63095   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000168 1.681e-04   3.127 0.08084 . 
    ##   trat: Fatorial               11 0.001833 1.667e-04   3.101 0.00162 **
    ## Residuals                      80 0.004300 5.375e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.0009625 6.417e-05   2.655 0.00261 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0001681 1.681e-04   6.954 0.01004 * 
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000500 5.000e-05   2.069 0.15422   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000125 1.250e-05   0.517 0.47412   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000889 8.889e-05   3.678 0.05870 . 
    ##   trat: Fatorial               11 0.0006431 5.846e-05   2.419 0.01185 * 
    ## Residuals                      80 0.0019333 2.417e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.001929 1.286e-04   3.056 0.000635 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000139 1.389e-04   3.300 0.073011 .  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000139 1.389e-04   3.300 0.073011 .  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000035 3.472e-05   0.825 0.366427    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000068 6.806e-05   1.617 0.207173    
    ##   trat: Fatorial               11 0.001549 1.408e-04   3.345 0.000791 ***
    ## Residuals                      80 0.003367 4.208e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.002396 1.597e-04   2.662 0.00255 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000050 5.000e-05   0.833 0.36405   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000168 1.681e-04   2.801 0.09812 . 
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000001 1.390e-06   0.023 0.87946   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000089 8.889e-05   1.481 0.22712   
    ##   trat: Fatorial               11 0.002088 1.898e-04   3.163 0.00135 **
    ## Residuals                      80 0.004800 6.000e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.0011240 7.493e-05   2.704 0.00220 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000014 1.390e-06   0.050 0.82342   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0002722 2.722e-04   9.825 0.00241 **
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0001125 1.125e-04   4.060 0.04727 * 
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000347 3.472e-05   1.253 0.26630   
    ##   trat: Fatorial               11 0.0007031 6.392e-05   2.307 0.01637 * 
    ## Residuals                      80 0.0022167 2.771e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0014333 9.556e-05   4.327 7.85e-06 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000681 6.806e-05   3.082  0.08300 .  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001681 1.681e-04   7.610  0.00719 ** 
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0001125 1.125e-04   5.094  0.02673 *  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000681 6.806e-05   3.082  0.08300 .  
    ##   trat: Fatorial               11 0.0010167 9.242e-05   4.185 6.94e-05 ***
    ## Residuals                      80 0.0017667 2.208e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.002867 1.911e-04   3.373 0.000209 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000068 6.806e-05   1.201 0.276414    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000022 2.222e-05   0.392 0.532951    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000001 1.390e-06   0.025 0.875989    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000006 5.560e-06   0.098 0.755010    
    ##   trat: Fatorial               11 0.002769 2.518e-04   4.443 3.34e-05 ***
    ## Residuals                      80 0.004533 5.667e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0024292 0.0001619   4.683 2.40e-06 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0004014 0.0004014  11.606  0.00103 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001125 0.0001125   3.253  0.07506 .  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000000 0.0000000   0.000  1.00000    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000889 0.0000889   2.570  0.11283    
    ##   trat: Fatorial               11 0.0018264 0.0001660   4.801 1.22e-05 ***
    ## Residuals                      80 0.0027667 0.0000346                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0009906 6.604e-05   2.963 0.000883 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000014 1.390e-06   0.062 0.803529    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001125 1.125e-04   5.047 0.027429 *  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000889 8.889e-05   3.988 0.049239 *  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000347 3.472e-05   1.558 0.215653    
    ##   trat: Fatorial               11 0.0007531 6.847e-05   3.071 0.001765 ** 
    ## Residuals                      80 0.0017833 2.229e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  722.0   48.13   2.638 0.002775 ** 
    ##   trat: Add vs. Trat (ciclo_1)  1    5.7    5.70   0.312 0.577935    
    ##   trat: Add vs. Trat (ciclo_2)  1    0.0    0.04   0.002 0.963804    
    ##   trat: Add vs. Trat (ciclo_3)  1    1.2    1.21   0.066 0.797523    
    ##   trat: Add vs. Trat (ciclo_4)  1   15.6   15.57   0.853 0.358422    
    ##   trat: Fatorial               11  699.5   63.59   3.485 0.000526 ***
    ## Residuals                      80 1459.7   18.25                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 1015.8   67.72   2.391 0.00659 **
    ##   trat: Add vs. Trat (ciclo_1)  1   45.3   45.30   1.599 0.20965   
    ##   trat: Add vs. Trat (ciclo_2)  1   61.0   61.01   2.154 0.14609   
    ##   trat: Add vs. Trat (ciclo_3)  1   63.8   63.81   2.253 0.13730   
    ##   trat: Add vs. Trat (ciclo_4)  1    0.9    0.94   0.033 0.85619   
    ##   trat: Fatorial               11  844.8   76.80   2.712 0.00506 **
    ## Residuals                      80 2265.7   28.32                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  909.1   60.61   1.947 0.03009 * 
    ##   trat: Add vs. Trat (ciclo_1)  1    1.5    1.48   0.047 0.82814   
    ##   trat: Add vs. Trat (ciclo_2)  1    1.3    1.34   0.043 0.83602   
    ##   trat: Add vs. Trat (ciclo_3)  1  314.9  314.88  10.117 0.00209 **
    ##   trat: Add vs. Trat (ciclo_4)  1    4.9    4.85   0.156 0.69402   
    ##   trat: Fatorial               11  586.6   53.32   1.713 0.08522 . 
    ## Residuals                      80 2489.8   31.12                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  765.3   51.02   3.308 0.000262 ***
    ##   trat: Add vs. Trat (ciclo_1)  1    2.8    2.77   0.179 0.673132    
    ##   trat: Add vs. Trat (ciclo_2)  1   24.6   24.63   1.597 0.210033    
    ##   trat: Add vs. Trat (ciclo_3)  1   75.1   75.13   4.871 0.030174 *  
    ##   trat: Add vs. Trat (ciclo_4)  1    0.7    0.74   0.048 0.827629    
    ##   trat: Fatorial               11  662.0   60.18   3.902 0.000157 ***
    ## Residuals                      80 1233.9   15.42                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  527.2   35.15   1.925 0.0325 *
    ##   trat: Add vs. Trat (ciclo_1)  1   53.4   53.37   2.922 0.0912 .
    ##   trat: Add vs. Trat (ciclo_2)  1    9.5    9.53   0.522 0.4721  
    ##   trat: Add vs. Trat (ciclo_3)  1  123.5  123.53   6.764 0.0111 *
    ##   trat: Add vs. Trat (ciclo_4)  1   40.8   40.76   2.232 0.1391  
    ##   trat: Fatorial               11  300.0   27.27   1.493 0.1503  
    ## Residuals                      80 1461.0   18.26                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)
    ## trat                           15  269.4   17.96   0.700  0.777
    ##   trat: Add vs. Trat (ciclo_1)  1   37.6   37.57   1.465  0.230
    ##   trat: Add vs. Trat (ciclo_2)  1    1.5    1.52   0.059  0.808
    ##   trat: Add vs. Trat (ciclo_3)  1   32.1   32.09   1.252  0.267
    ##   trat: Add vs. Trat (ciclo_4)  1    0.0    0.04   0.002  0.968
    ##   trat: Fatorial               11  198.2   18.02   0.703  0.732
    ## Residuals                      80 2051.5   25.64               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  647.9   43.19   3.166 0.000431 ***
    ##   trat: Add vs. Trat (ciclo_1)  1    0.4    0.42   0.031 0.861139    
    ##   trat: Add vs. Trat (ciclo_2)  1    2.9    2.88   0.211 0.647149    
    ##   trat: Add vs. Trat (ciclo_3)  1   46.2   46.24   3.389 0.069323 .  
    ##   trat: Add vs. Trat (ciclo_4)  1    0.1    0.08   0.006 0.938392    
    ##   trat: Fatorial               11  598.3   54.39   3.987 0.000123 ***
    ## Residuals                      80 1091.4   13.64                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  451.1   30.07   1.368 0.1837  
    ##   trat: Add vs. Trat (ciclo_1)  1   85.4   85.43   3.887 0.0521 .
    ##   trat: Add vs. Trat (ciclo_2)  1    1.7    1.69   0.077 0.7825  
    ##   trat: Add vs. Trat (ciclo_3)  1   72.9   72.94   3.318 0.0722 .
    ##   trat: Add vs. Trat (ciclo_4)  1    1.0    1.03   0.047 0.8288  
    ##   trat: Fatorial               11  290.0   26.36   1.199 0.3011  
    ## Residuals                      80 1758.5   21.98                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)
    ## trat                           15  288.3   19.22   0.655  0.819
    ##   trat: Add vs. Trat (ciclo_1)  1    0.0    0.03   0.001  0.974
    ##   trat: Add vs. Trat (ciclo_2)  1    1.8    1.79   0.061  0.806
    ##   trat: Add vs. Trat (ciclo_3)  1    3.9    3.87   0.132  0.717
    ##   trat: Add vs. Trat (ciclo_4)  1   54.1   54.11   1.845  0.178
    ##   trat: Fatorial               11  228.5   20.77   0.708  0.727
    ## Residuals                      80 2346.1   29.33               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  482.6   32.17   2.256 0.01051 * 
    ##   trat: Add vs. Trat (ciclo_1)  1    9.6    9.64   0.676 0.41347   
    ##   trat: Add vs. Trat (ciclo_2)  1    0.0    0.01   0.001 0.97791   
    ##   trat: Add vs. Trat (ciclo_3)  1   33.9   33.91   2.378 0.12700   
    ##   trat: Add vs. Trat (ciclo_4)  1    9.0    8.95   0.628 0.43045   
    ##   trat: Fatorial               11  430.1   39.10   2.742 0.00463 **
    ## Residuals                      80 1140.7   14.26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  628.4   41.89   2.006 0.0247 *
    ##   trat: Add vs. Trat (ciclo_1)  1    4.7    4.70   0.225 0.6366  
    ##   trat: Add vs. Trat (ciclo_2)  1   89.6   89.56   4.288 0.0416 *
    ##   trat: Add vs. Trat (ciclo_3)  1    0.8    0.79   0.038 0.8459  
    ##   trat: Add vs. Trat (ciclo_4)  1   56.9   56.91   2.725 0.1027  
    ##   trat: Fatorial               11  476.4   43.31   2.074 0.0317 *
    ## Residuals                      80 1670.8   20.88                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  476.5   31.77   1.115 0.3570  
    ##   trat: Add vs. Trat (ciclo_1)  1   12.4   12.40   0.435 0.5113  
    ##   trat: Add vs. Trat (ciclo_2)  1    0.9    0.89   0.031 0.8599  
    ##   trat: Add vs. Trat (ciclo_3)  1  106.5  106.46   3.737 0.0568 .
    ##   trat: Add vs. Trat (ciclo_4)  1   45.3   45.30   1.590 0.2110  
    ##   trat: Fatorial               11  311.5   28.32   0.994 0.4592  
    ## Residuals                      80 2279.0   28.49                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Análise em faixas

``` r
for(l in 1:length(atributos)){
  for(i in 1:length(cob)){
    for(j in 1:length(prof)){
        # for(k in 1:length(posição)){
          print("-----------------------------------------")
          print(paste(atributos[l],"-",cob[i],prof[j],sep=" "))
          print("-----------------------------------------")
          
          # Criando dados auxiliar
          da1<-dados %>% 
            filter(cobertura==cob[i],
                  profundidade==prof[j])
          
          # fl<-(da1$linha_entrelinha == posi[k])
          # da1 <- da1[fl,]
          y<-da1[,5+l]

      print("------------------ Análise em Faixas -------------")
      
      if(sd(y)==0 | is.na(sd(y))) {print("dados sem variação")
        }else{
      faixas(da1$PDS,da1$ciclo,da1$bloco,y,
                    quali=c(TRUE,TRUE), mcomp = "tukey",
                    fac.names = c("PDS","CICLO"))}
      
       cat("\n")
    # }
    }}
}
```

    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc  Pr(>Fc)    
    ## Bloco      2 0.02814 0.014068 -3.072 1.000000    
    ## PDS        2 0.00934 0.004672  3.972 0.112168    
    ## Erro a     4 0.00471 0.001176                    
    ## CICLO      3 0.19192 0.063972 36.449 0.000302 ***
    ## Erro b     6 0.01053 0.001755                    
    ## PDS*CICLO  6 0.01172 0.001954  0.260 0.952668    
    ## Erro c    48 0.36054 0.007511                    
    ## Total     71 0.61690                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.063242 %
    ## CV 2 = 2.52014 %
    ## CV 3 = 5.213548 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.670417
    ## 2     pd 1.670417
    ## 3     pp 1.646250
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.707778 
    ## a     4   1.707778 
    ##  b    3   1.651667 
    ##   c   1   1.582222 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.00929 0.004643  0.4398 0.671991   
    ## PDS        2 0.01455 0.007276  0.4088 0.689406   
    ## Erro a     4 0.07121 0.017801                    
    ## CICLO      3 0.23641 0.078802 18.3715 0.001992 **
    ## Erro b     6 0.02574 0.004289                    
    ## PDS*CICLO  6 0.04984 0.008306  0.7202 0.635267   
    ## Erro c    48 0.55357 0.011533                    
    ## Total     71 0.96059                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 8.011987 %
    ## CV 2 = 3.932866 %
    ## CV 3 = 6.448814 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.683750
    ## 2     pd 1.662917
    ## 3     pp 1.649167
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.731667 
    ## ab    3   1.698333 
    ##  bc   4   1.652222 
    ##   c   1   1.578889 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.01338 0.006689 0.9744 0.45213  
    ## PDS        2 0.02297 0.011485 2.1740 0.22959  
    ## Erro a     4 0.02113 0.005283                 
    ## CICLO      3 0.11211 0.037370 4.2368 0.06281 .
    ## Erro b     6 0.05292 0.008820                 
    ## PDS*CICLO  6 0.12023 0.020038 2.7684 0.02157 *
    ## Erro c    48 0.34744 0.007238                 
    ## Total     71 0.69018                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.430314 %
    ## CV 2 = 5.724697 %
    ## CV 3 = 5.185921 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                     GL        SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.0000 0.0164110 0.008206 1.215773 0.306382
    ## PDS : CICLO 2   2.0000 0.0174780 0.008739 1.294794 0.284329
    ## PDS : CICLO 3   2.0000 0.1057000 0.052850   7.8305 0.001249
    ## PDS : CICLO 4   2.0000 0.0036110 0.001806 0.267519 0.766529
    ## Erro combinado 43.3822 0.2927865 0.006749                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.628333
    ## 2     pd 1.608333
    ## 3     pp 1.556667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.671667
    ## 2     pd 1.690000
    ## 3     pp 1.745000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.718333 
    ##  b    pd      1.598333 
    ##  b    pp      1.533333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.631667
    ## 2     pd 1.665000
    ## 3     pp 1.640000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                      GL        SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.0000 0.0319170 0.010639 1.370049 0.265426
    ## CICLO : PDS pd   3.0000 0.0351790 0.011726 1.510095 0.226247
    ## CICLO : PDS pp   3.0000 0.1652460 0.055082 7.093314 0.000603
    ## Erro combinado  40.9125 0.3176856 0.007765                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.628333
    ## 2      2 1.671667
    ## 3      3 1.718333
    ## 4      4 1.631667
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.608333
    ## 2      2 1.690000
    ## 3      3 1.598333
    ## 4      4 1.665000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.745 
    ## ab    4   1.64 
    ##  b    1   1.556667 
    ##  b    3   1.533333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.00970 0.004851  1.2710 0.373853   
    ## PDS        2 0.03937 0.019685  3.6388 0.125803   
    ## Erro a     4 0.02164 0.005410                    
    ## CICLO      3 0.17283 0.057611 13.8961 0.004146 **
    ## Erro b     6 0.02488 0.004146                    
    ## PDS*CICLO  6 0.01424 0.002374  0.4136 0.866360   
    ## Erro c    48 0.27545 0.005739                    
    ## Total     71 0.55811                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.454625 %
    ## CV 2 = 3.899686 %
    ## CV 3 = 4.588011 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.684167
    ## 2     pd 1.635417
    ## 3     pp 1.633750
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   1.706111 
    ## a     3   1.682222 
    ## ab    2   1.638889 
    ##  b    1   1.577222 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.00402 0.002010 0.3141 0.74694  
    ## PDS        2 0.05922 0.029610 4.9544 0.08271 .
    ## Erro a     4 0.02391 0.005976                 
    ## CICLO      3 0.05382 0.017938 2.5947 0.14778  
    ## Erro b     6 0.04148 0.006913                 
    ## PDS*CICLO  6 0.02188 0.003647 0.5617 0.75847  
    ## Erro c    48 0.31161 0.006492                 
    ## Total     71 0.51593                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.570254 %
    ## CV 2 = 4.915498 %
    ## CV 3 = 4.763287 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.732083
    ## 2     pd 1.671667
    ## 3     pp 1.670833
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.661667
    ## 2      2 1.671111
    ## 3      3 1.702222
    ## 4      4 1.731111
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM      Fc Pr(>Fc)
    ## Bloco      2 0.02568 0.012837 2.66446  0.1838
    ## PDS        2 0.00891 0.004454 1.28024  0.3717
    ## Erro a     4 0.01392 0.003479                
    ## CICLO      3 0.01508 0.005027 0.86726  0.5078
    ## Erro b     6 0.03478 0.005797                
    ## PDS*CICLO  6 0.02345 0.003908 0.87662  0.5191
    ## Erro c    48 0.21398 0.004458                
    ## Total     71 0.33579                         
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.638209 %
    ## CV 2 = 4.696157 %
    ## CV 3 = 4.11826 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.636667
    ## 2     pd 1.616250
    ## 3     pp 1.610833
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.609444
    ## 2      2 1.607222
    ## 3      3 1.625000
    ## 4      4 1.643333
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc  Pr(>Fc)    
    ## Bloco      2 0.01770 0.008851 -6.110 1.000000    
    ## PDS        2 0.02285 0.011426  5.006 0.081498 .  
    ## Erro a     4 0.00913 0.002283                    
    ## CICLO      3 0.13648 0.045494 34.132 0.000363 ***
    ## Erro b     6 0.00800 0.001333                    
    ## PDS*CICLO  6 0.04105 0.006841  1.351 0.253651    
    ## Erro c    48 0.24309 0.005064                    
    ## Total     71 0.47830                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.864947 %
    ## CV 2 = 2.189233 %
    ## CV 3 = 4.267342 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.658750
    ## 2     pd 1.651667
    ## 3     pp 1.692500
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   1.713889 
    ## a     2   1.682222 
    ## a     3   1.678333 
    ##  b    1   1.596111 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc Pr(>Fc)
    ## Bloco      2 0.04069 0.020343 4.0680  0.1086
    ## PDS        2 0.04948 0.024739 3.8223  0.1180
    ## Erro a     4 0.02589 0.006472               
    ## CICLO      3 0.05648 0.018826 2.8490  0.1274
    ## Erro b     6 0.03965 0.006608               
    ## PDS*CICLO  6 0.02339 0.003898 0.4825  0.8181
    ## Erro c    48 0.38781 0.008079               
    ## Total     71 0.62338                        
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.818976 %
    ## CV 2 = 4.869213 %
    ## CV 3 = 5.384155 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.640833
    ## 2     pd 1.663333
    ## 3     pp 1.704167
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.657778
    ## 2      2 1.630556
    ## 3      3 1.684444
    ## 4      4 1.705000
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc  Pr(>Fc)   
    ## Bloco      2 0.01402 0.007010  0.855 0.490659   
    ## PDS        2 0.03094 0.015468 34.749 0.002962 **
    ## Erro a     4 0.00178 0.000445                   
    ## CICLO      3 0.01132 0.003774  0.274 0.842466   
    ## Erro b     6 0.08270 0.013784                   
    ## PDS*CICLO  6 0.02179 0.003631  0.602 0.727369   
    ## Erro c    48 0.28956 0.006033                   
    ## Total     71 0.45211                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.309551 %
    ## CV 2 = 7.28717 %
    ## CV 3 = 4.820877 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.640417 
    ##  b    cm      1.597083 
    ##  b    pd      1.595833 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.620556
    ## 2      2 1.625000
    ## 3      3 1.593333
    ## 4      4 1.605556
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.00821 0.004106  3.3672 0.138855   
    ## PDS        2 0.00090 0.000451  0.1192 0.890668   
    ## Erro a     4 0.01515 0.003787                    
    ## CICLO      3 0.09396 0.031320 11.2603 0.007067 **
    ## Erro b     6 0.01669 0.002781                    
    ## PDS*CICLO  6 0.03560 0.005933  1.1092 0.371040   
    ## Erro c    48 0.25675 0.005349                    
    ## Total     71 0.42726                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.699003 %
    ## CV 2 = 3.170197 %
    ## CV 3 = 4.396278 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.658750
    ## 2     pd 1.667083
    ## 3     pp 1.665000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.708333 
    ## a     4   1.682222 
    ## ab    3   1.652778 
    ##  b    1   1.611111 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.02685 0.013426 3.6271 0.12633  
    ## PDS        2 0.05501 0.027506 2.8511 0.16997  
    ## Erro a     4 0.03859 0.009647                 
    ## CICLO      3 0.04765 0.015883 7.6973 0.01764 *
    ## Erro b     6 0.01238 0.002063                 
    ## PDS*CICLO  6 0.08706 0.014509 1.8116 0.11672  
    ## Erro c    48 0.38443 0.008009                 
    ## Total     71 0.65196                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.861467 %
    ## CV 2 = 2.710813 %
    ## CV 3 = 5.340623 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.707917
    ## 2     pd 1.678750
    ## 3     pp 1.640417
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.718889 
    ## ab    3   1.667778 
    ##  b    1   1.665 
    ##  b    4   1.651111 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.02621 0.013106 5.9940 0.06259 .
    ## PDS        2 0.04705 0.023526 5.8612 0.06473 .
    ## Erro a     4 0.01606 0.004014                 
    ## CICLO      3 0.01518 0.005061 1.0836 0.42473  
    ## Erro b     6 0.02802 0.004670                 
    ## PDS*CICLO  6 0.06411 0.010686 1.6445 0.15552  
    ## Erro c    48 0.31189 0.006498                 
    ## Total     71 0.50853                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.890472 %
    ## CV 2 = 4.196578 %
    ## CV 3 = 4.949973 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.592500
    ## 2     pd 1.649583
    ## 3     pp 1.643333
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.613889
    ## 2      2 1.637222
    ## 3      3 1.647778
    ## 4      4 1.615000
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.000053 0.000026 0.3762 0.70840  
    ## PDS        2 0.000086 0.000043 7.7500 0.04208 *
    ## Erro a     4 0.000022 0.000006                 
    ## CICLO      3 0.000694 0.000231 2.8902 0.12440  
    ## Erro b     6 0.000481 0.000080                 
    ## PDS*CICLO  6 0.000181 0.000030 1.9403 0.09331 .
    ## Erro c    48 0.000744 0.000016                 
    ## Total     71 0.002261                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1288969 %
    ## CV 2 = 0.4894122 %
    ## CV 3 = 0.2153645 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.829583 
    ## ab    cm      1.829167 
    ##  b    pp      1.827083 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.833889
    ## 2      2 1.826111
    ## 3      3 1.826667
    ## 4      4 1.827778
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ      QM     Fc Pr(>Fc)
    ## Bloco      2 0.000103 5.1e-05 1.6536  0.2996
    ## PDS        2 0.000103 5.1e-05 2.1143  0.2363
    ## Erro a     4 0.000097 2.4e-05               
    ## CICLO      3 0.000171 5.7e-05 1.4138  0.3278
    ## Erro b     6 0.000242 4.0e-05               
    ## PDS*CICLO  6 0.000342 5.7e-05 1.6995  0.1416
    ## Erro c    48 0.001608 3.4e-05               
    ## Total     71 0.002665                       
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2739983 %
    ## CV 2 = 0.3527182 %
    ## CV 3 = 0.3217085 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.800833
    ## 2     pd 1.799167
    ## 3     pp 1.797917
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.801111
    ## 2      2 1.800556
    ## 3      3 1.797778
    ## 4      4 1.797778
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM      Fc  Pr(>Fc)    
    ## Bloco      2 0.000075 0.000038 10.2857 0.026501 *  
    ## PDS        2 0.000058 0.000029  2.8000 0.173611    
    ## Erro a     4 0.000042 0.000010                     
    ## CICLO      3 0.000161 0.000054  2.1887 0.190310    
    ## Erro b     6 0.000147 0.000025                     
    ## PDS*CICLO  6 0.000964 0.000161  5.1312 0.000384 ***
    ## Erro c    48 0.001503 0.000031                     
    ## Total     71 0.002950                              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1834668 %
    ## CV 2 = 0.2815815 %
    ## CV 3 = 0.3180679 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL          SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.000811000 0.000406 15.74973    5e-06
    ## PDS : CICLO 2   2.00000 0.000100000 0.000050 1.941748 0.153772
    ## PDS : CICLO 3   2.00000 0.000100000 0.000050 1.941748 0.153772
    ## PDS : CICLO 4   2.00000 0.000011000 0.000006  0.21575 0.806659
    ## Erro combinado 51.70393 0.001344302 0.000026                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.766667 
    ## a     pd      1.765 
    ##  b    pp      1.751667 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.761667
    ## 2     pd 1.756667
    ## 3     pp 1.761667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis Medias
    ## 1     cm  1.755
    ## 2     pd  1.760
    ## 3     pp  1.760
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.756667
    ## 2     pd 1.756667
    ## 3     pp 1.758333
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                      GL          SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.0000 0.000500000 0.000167 5.747126 0.001889
    ## CICLO : PDS pd   3.0000 0.000279000 0.000093 3.208812 0.030985
    ## CICLO : PDS pp   3.0000 0.000346000 0.000115 3.975096 0.012983
    ## Erro combinado  48.9671 0.001420046 0.000029                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.766667 
    ## ab    2   1.761667 
    ##  b    4   1.756667 
    ##  b    3   1.755 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.765 
    ## ab    3   1.76 
    ##  b    2   1.756667 
    ##  b    4   1.756667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.761667 
    ## a     3   1.76 
    ## ab    4   1.758333 
    ##  b    1   1.751667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000044 0.000022  0.7485 0.529488   
    ## PDS        2 0.000136 0.000068  1.8491 0.269992   
    ## Erro a     4 0.000147 0.000037                    
    ## CICLO      3 0.000328 0.000109 11.8000 0.006286 **
    ## Erro b     6 0.000056 0.000009                    
    ## PDS*CICLO  6 0.000431 0.000072  4.3816 0.001319 **
    ## Erro c    48 0.000786 0.000016                    
    ## Total     71 0.001928                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.3320713 %
    ## CV 2 = 0.166557 %
    ## CV 3 = 0.2215114 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL           SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.0000330000 0.000017 0.784314 0.471021
    ## PDS : CICLO 2   2.00000 0.0003110000 0.000156 7.320261 0.004546
    ## PDS : CICLO 3   2.00000 0.0000110000 0.000006 0.261438 0.772739
    ## PDS : CICLO 4   2.00000 0.0002110000 0.000106  4.96732  0.01875
    ## Erro combinado 18.51377 0.0003887892 0.000021                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.828333
    ## 2     pd 1.830000
    ## 3     pp 1.831667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.83 
    ## ab    pd      1.823333 
    ##  b    pp      1.82 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.828333
    ## 2     pd 1.828333
    ## 3     pp 1.826667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.83 
    ## ab    cm      1.825 
    ##  b    pp      1.821667 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ       QM        Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0000790000 0.000026  1.930894 0.135812
    ## CICLO : PDS pd   3.00000 0.0001790000 0.000060  4.369919 0.008032
    ## CICLO : PDS pp   3.00000 0.0005000000 0.000167 12.195122    4e-06
    ## Erro combinado  52.82927 0.0007396098 0.000014                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.828333
    ## 2      2 1.830000
    ## 3      3 1.828333
    ## 4      4 1.825000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.83 
    ## a     4   1.83 
    ## ab    3   1.828333 
    ##  b    2   1.823333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.831667 
    ## ab    3   1.826667 
    ##  bc   4   1.821667 
    ##   c   2   1.82 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.000011 0.000006 0.9412 0.46240  
    ## PDS        2 0.000144 0.000072 5.2000 0.07716 .
    ## Erro a     4 0.000056 0.000014                 
    ## CICLO      3 0.000315 0.000105 7.0938 0.02126 *
    ## Erro b     6 0.000089 0.000015                 
    ## PDS*CICLO  6 0.000389 0.000065 2.8426 0.01891 *
    ## Erro c    48 0.001094 0.000023                 
    ## Total     71 0.002099                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2073153 %
    ## CV 2 = 0.2141143 %
    ## CV 3 = 0.265628 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                     GL           SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.0000 0.0002330000 0.000117  5.62249 0.006503
    ## PDS : CICLO 2   2.0000 0.0000110000 0.000006 0.267738  0.76628
    ## PDS : CICLO 3   2.0000 0.0000110000 0.000006 0.267738  0.76628
    ## PDS : CICLO 4   2.0000 0.0002780000 0.000139  6.69344 0.002789
    ## Erro combinado 46.4884 0.0009762564 0.000021                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.803333 
    ## a     pp      1.801667 
    ##  b    cm      1.795 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.800000
    ## 2     pd 1.798333
    ## 3     pp 1.798333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.798333
    ## 2     pd 1.796667
    ## 3     pp 1.796667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.8 
    ##  b    cm      1.791667 
    ##  b    pp      1.791667 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                      GL          SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.0000 0.000246000 0.000082 4.030055 0.011915
    ## CICLO : PDS pd   3.0000 0.000146000 0.000049  2.39071 0.079267
    ## CICLO : PDS pp   3.0000 0.000313000 0.000104 5.122951 0.003537
    ## Erro combinado  51.6214 0.001032428 0.000020                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.8 
    ## ab    3   1.798333 
    ## ab    1   1.795 
    ##  b    4   1.791667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.803333
    ## 2      2 1.798333
    ## 3      3 1.796667
    ## 4      4 1.800000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.801667 
    ## ab    2   1.798333 
    ## ab    3   1.796667 
    ##  b    4   1.791667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000158 0.000079 -6.0000 1.000000   
    ## PDS        2 0.000175 0.000088  5.2500 0.076100 . 
    ## Erro a     4 0.000067 0.000017                    
    ## CICLO      3 0.000550 0.000183  7.7647 0.017287 * 
    ## Erro b     6 0.000142 0.000024                    
    ## PDS*CICLO  6 0.001092 0.000182  3.4026 0.007056 **
    ## Erro c    48 0.002567 0.000053                    
    ## Total     71 0.004750                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2322892 %
    ## CV 2 = 0.2764795 %
    ## CV 3 = 0.4160723 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL          SQ       QM        Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.000233000 0.000117  2.651515 0.080122
    ## PDS : CICLO 2   2.00000 0.000900000 0.000450 10.227273 0.000181
    ## PDS : CICLO 3   2.00000 0.000033000 0.000017  0.378788 0.686575
    ## PDS : CICLO 4   2.00000 0.000100000 0.000050  1.136364 0.328866
    ## Erro combinado 51.71825 0.002275603 0.000044                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.756667
    ## 2     pd 1.763333
    ## 3     pp 1.765000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.76 
    ## a     pd      1.76 
    ##  b    pp      1.745 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.760000
    ## 2     pd 1.758333
    ## 3     pp 1.756667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.751667
    ## 2     pd 1.756667
    ## 3     pp 1.756667
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL        SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0002790 0.000093 2.147436 0.104978
    ## CICLO : PDS pd   3.00000 0.0001460 0.000049 1.121795 0.348444
    ## CICLO : PDS pp   3.00000 0.0012170 0.000406 9.358974  4.5e-05
    ## Erro combinado  53.78139 0.0023126 0.000043                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.756667
    ## 2      2 1.760000
    ## 3      3 1.760000
    ## 4      4 1.751667
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.763333
    ## 2      2 1.760000
    ## 3      3 1.758333
    ## 4      4 1.756667
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.765 
    ## a     3   1.756667 
    ## a     4   1.756667 
    ##  b    2   1.745 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000053 0.000026  5.2414 0.076281 . 
    ## PDS        2 0.000036 0.000018  2.3636 0.210069   
    ## Erro a     4 0.000031 0.000008                    
    ## CICLO      3 0.000744 0.000248 10.1132 0.009215 **
    ## Erro b     6 0.000147 0.000025                    
    ## PDS*CICLO  6 0.000331 0.000055  2.0299 0.079770 . 
    ## Erro c    48 0.001303 0.000027                    
    ## Total     71 0.002644                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1512139 %
    ## CV 2 = 0.2710115 %
    ## CV 3 = 0.2850304 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.827083
    ## 2     pd 1.827500
    ## 3     pp 1.828750
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.832778 
    ## ab    4   1.828333 
    ##  b    3   1.825556 
    ##  b    2   1.824444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.000108 0.000054 4.4571 0.09594 .
    ## PDS        2 0.000408 0.000204 9.8000 0.02873 *
    ## Erro a     4 0.000083 0.000021                 
    ## CICLO      3 0.000406 0.000135 7.8919 0.01665 *
    ## Erro b     6 0.000103 0.000017                 
    ## PDS*CICLO  6 0.000203 0.000034 1.3094 0.27122  
    ## Erro c    48 0.001239 0.000026                 
    ## Total     71 0.002550                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2539279 %
    ## CV 2 = 0.2302529 %
    ## CV 3 = 0.2826355 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.799583 
    ## ab    pd      1.79875 
    ##  b    cm      1.794167 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.801111 
    ## ab    3   1.797222 
    ## ab    4   1.797222 
    ##  b    2   1.794444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM      Fc Pr(>Fc)  
    ## Bloco      2 0.000003 0.000001  0.0784 0.92595  
    ## PDS        2 0.000253 0.000126 13.0000 0.01778 *
    ## Erro a     4 0.000039 0.000010                  
    ## CICLO      3 0.000789 0.000263  8.4776 0.01408 *
    ## Erro b     6 0.000186 0.000031                  
    ## PDS*CICLO  6 0.000269 0.000045  1.9497 0.09179 .
    ## Erro c    48 0.001106 0.000023                  
    ## Total     71 0.002644                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1773858 %
    ## CV 2 = 0.3168448 %
    ## CV 3 = 0.2730271 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.76 
    ## ab    pd      1.757917 
    ##  b    cm      1.755417 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.763333 
    ## ab    3   1.757222 
    ##  b    2   1.755556 
    ##  b    4   1.755 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000211 0.000106  3.2340 0.146011   
    ## PDS        2 0.000253 0.000126  9.1000 0.032465 * 
    ## Erro a     4 0.000056 0.000014                    
    ## CICLO      3 0.001339 0.000446 12.6842 0.005237 **
    ## Erro b     6 0.000211 0.000035                    
    ## PDS*CICLO  6 0.000136 0.000023  1.3803 0.241793   
    ## Erro c    48 0.000789 0.000016                    
    ## Total     71 0.002994                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2036801 %
    ## CV 2 = 0.3241864 %
    ## CV 3 = 0.2215655 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.83125 
    ## ab    cm      1.830833 
    ##  b    pp      1.827083 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.836667 
    ##  b    4   1.829444 
    ##  b    3   1.827778 
    ##  b    2   1.825 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000269 0.000135  3.5113 0.131689   
    ## PDS        2 0.000386 0.000193  6.7805 0.051883 . 
    ## Erro a     4 0.000114 0.000028                    
    ## CICLO      3 0.001304 0.000435 10.7931 0.007852 **
    ## Erro b     6 0.000242 0.000040                    
    ## PDS*CICLO  6 0.000158 0.000026  0.8686 0.524839   
    ## Erro c    48 0.001458 0.000030                    
    ## Total     71 0.003932                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2961895 %
    ## CV 2 = 0.3522831 %
    ## CV 3 = 0.3059615 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.803750
    ## 2     pd 1.802500
    ## 3     pp 1.798333
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.808889 
    ##  b    2   1.799444 
    ##  b    3   1.798889 
    ##  b    4   1.798889 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL       SQ       QM     Fc Pr(>Fc)  
    ## Bloco      2 0.000058 0.000029 0.8615 0.48850  
    ## PDS        2 0.000175 0.000088 8.4000 0.03698 *
    ## Erro a     4 0.000042 0.000010                 
    ## CICLO      3 0.000415 0.000138 3.1474 0.10787  
    ## Erro b     6 0.000264 0.000044                 
    ## PDS*CICLO  6 0.000147 0.000025 1.1944 0.32545  
    ## Erro c    48 0.000986 0.000021                 
    ## Total     71 0.002088                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1834233 %
    ## CV 2 = 0.3768991 %
    ## CV 3 = 0.2575921 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.76125 
    ## ab    pd      1.76 
    ##  b    cm      1.7575 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.763333
    ## 2      2 1.758889
    ## 3      3 1.756667
    ## 4      4 1.759444
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ      QM     Fc Pr(>Fc)    
    ## Bloco      2   71.98  35.992 -2.677 1.00000    
    ## PDS        2   26.36  13.180  3.942 0.11331    
    ## Erro a     4   13.38   3.344                   
    ## CICLO      3  610.75 203.583 40.757 0.00022 ***
    ## Erro b     6   29.97   4.995                   
    ## PDS*CICLO  6   31.67   5.278  0.242 0.96010    
    ## Erro c    48 1045.60  21.783                   
    ## Total     71 1829.71                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.011826 %
    ## CV 2 = 2.458887 %
    ## CV 3 = 5.134905 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 91.37458
    ## 2     pd 91.26458
    ## 3     pp 90.03958
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   93.455 
    ## a     4   93.36222 
    ##  b    3   90.45222 
    ##   c   1   86.30222 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2   27.00  13.501  0.4215 0.682168   
    ## PDS        2   39.77  19.886  0.3669 0.713982   
    ## Erro a     4  216.78  54.195                    
    ## CICLO      3  753.84 251.280 19.5266 0.001692 **
    ## Erro b     6   77.21  12.869                    
    ## PDS*CICLO  6  142.77  23.795  0.6792 0.667053   
    ## Erro c    48 1681.59  35.033                    
    ## Total     71 2938.96                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 7.953496 %
    ## CV 2 = 3.875641 %
    ## CV 3 = 6.394657 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 93.52625
    ## 2     pd 92.43417
    ## 3     pp 91.71875
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   96.22278 
    ## ab    3   94.46167 
    ##  b    4   91.94056 
    ##   c   1   87.61389 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ      QM     Fc Pr(>Fc)  
    ## Bloco      2   40.06  20.029 0.8518 0.49184  
    ## PDS        2   65.16  32.580 1.9384 0.25789  
    ## Erro a     4   67.23  16.808                 
    ## CICLO      3  379.97 126.655 4.2399 0.06273 .
    ## Erro b     6  179.23  29.872                 
    ## PDS*CICLO  6  375.01  62.501 2.6979 0.02445 *
    ## Erro c    48 1112.00  23.167                 
    ## Total     71 2218.66                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.395719 %
    ## CV 2 = 5.860144 %
    ## CV 3 = 5.160655 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL         SQ         QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000  31.223544  15.611772 0.723538 0.490778
    ## PDS : CICLO 2   2.00000  52.493233  26.246617 1.216417  0.30617
    ## PDS : CICLO 3   2.00000 346.641633 173.320817 8.032668 0.001074
    ## PDS : CICLO 4   2.00000   9.807778   4.903889 0.227274 0.797643
    ## Erro combinado 43.49644 938.522381  21.576992                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 92.03167
    ## 2     pd 91.06000
    ## 3     pp 88.88167
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 95.03000
    ## 2     pd 96.22167
    ## 3     pp 99.09833
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      97.75333 
    ##  b    pd      90.915 
    ##  b    pp      87.15167 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 92.99167
    ## 2     pd 94.70833
    ## 3     pp 93.35833
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL        SQ        QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000  115.3408  38.44694 1.513544  0.22589
    ## CICLO : PDS pd   3.00000  127.2216  42.40722 1.669448 0.189084
    ## CICLO : PDS pp   3.00000  512.4103 170.80344 6.724032 0.000895
    ## Erro combinado  39.60134 1005.9508  25.40194                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 92.03167
    ## 2      2 95.03000
    ## 3      3 97.75333
    ## 4      4 92.99167
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 91.06000
    ## 2      2 96.22167
    ## 3      3 90.91500
    ## 4      4 94.70833
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   99.09833 
    ## ab    4   93.35833 
    ##  b    1   88.88167 
    ##  b    3   87.15167 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2   31.93  15.963  1.7437 0.285408   
    ## PDS        2  113.27  56.635  4.1594 0.105436   
    ## Erro a     4   54.47  13.616                    
    ## CICLO      3  544.89 181.630 15.4553 0.003146 **
    ## Erro b     6   70.51  11.752                    
    ## PDS*CICLO  6   34.63   5.772  0.3560 0.902870   
    ## Erro c    48  778.23  16.213                    
    ## Total     71 1627.93                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.080624 %
    ## CV 2 = 3.790986 %
    ## CV 3 = 4.452783 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 92.20125
    ## 2     pd 89.50583
    ## 3     pp 89.57667
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   93.52889 
    ## a     3   92.06222 
    ## ab    2   89.90056 
    ##  b    1   86.22 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2   13.25  6.623 0.3851 0.70315  
    ## PDS        2  195.69 97.843 6.1358 0.06043 .
    ## Erro a     4   63.79 15.946                 
    ## CICLO      3  192.14 64.047 2.9288 0.12171  
    ## Erro b     6  131.21 21.868                 
    ## PDS*CICLO  6   72.84 12.141 0.5889 0.73750  
    ## Erro c    48  989.60 20.617                 
    ## Total     71 1658.51                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.24377 %
    ## CV 2 = 4.969634 %
    ## CV 3 = 4.82539 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 96.42875
    ## 2     pd 92.91583
    ## 3     pp 92.94750
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 92.27056
    ## 2      2 92.92278
    ## 3      3 94.74556
    ## 4      4 96.45056
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ     QM      Fc Pr(>Fc)
    ## Bloco      2   86.58 43.289 2.77837  0.1752
    ## PDS        2   34.30 17.148 1.68435  0.2947
    ## Erro a     4   40.72 10.181                
    ## CICLO      3   57.27 19.089 0.94389  0.4763
    ## Erro b     6  121.34 20.224                
    ## PDS*CICLO  6   98.79 16.465 1.11072  0.3702
    ## Erro c    48  711.56 14.824                
    ## Total     71 1150.56                       
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.458236 %
    ## CV 2 = 4.874169 %
    ## CV 3 = 4.173023 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 93.23625
    ## 2     pd 91.85417
    ## 3     pp 91.70208
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 91.39000
    ## 2      2 91.56111
    ## 3      3 92.47000
    ## 4      4 93.63556
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ      QM     Fc  Pr(>Fc)    
    ## Bloco      2   58.82  29.412 -8.482 1.000000    
    ## PDS        2   63.92  31.960  4.736 0.088155 .  
    ## Erro a     4   26.99   6.748                    
    ## CICLO      3  426.00 142.000 35.428 0.000327 ***
    ## Erro b     6   24.05   4.008                    
    ## PDS*CICLO  6  114.77  19.129  1.345 0.256135    
    ## Erro c    48  682.75  14.224                    
    ## Total     71 1397.30                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.846052 %
    ## CV 2 = 2.193386 %
    ## CV 3 = 4.131963 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 90.87542
    ## 2     pd 90.37417
    ## 3     pp 92.57583
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   93.63278 
    ## a     2   92.26 
    ## a     3   92.00722 
    ##  b    1   87.20056 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ     QM     Fc Pr(>Fc)
    ## Bloco      2  131.62 65.809 3.8723  0.1160
    ## PDS        2  134.15 67.077 3.5304  0.1308
    ## Erro a     4   76.00 19.000               
    ## CICLO      3  168.48 56.161 2.5453  0.1522
    ## Erro b     6  132.39 22.065               
    ## PDS*CICLO  6   78.84 13.140 0.5459  0.7706
    ## Erro c    48 1155.37 24.070               
    ## Total     71 1876.85                      
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.692762 %
    ## CV 2 = 5.057186 %
    ## CV 3 = 5.281966 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 91.45708
    ## 2     pd 92.47292
    ## 3     pp 94.72375
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 91.98056
    ## 2      2 90.93278
    ## 3      3 93.74000
    ## 4      4 94.88500
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ     QM      Fc  Pr(>Fc)   
    ## Bloco      2   50.73 25.366  1.0100 0.441498   
    ## PDS        2   94.72 47.361 29.2805 0.004088 **
    ## Erro a     4    6.47  1.617                    
    ## CICLO      3   37.02 12.340  0.2866 0.833784   
    ## Erro b     6  258.38 43.064                    
    ## PDS*CICLO  6   64.27 10.711  0.5474 0.769455   
    ## Erro c    48  939.18 19.566                    
    ## Total     71 1450.77                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.388369 %
    ## CV 2 = 7.163726 %
    ## CV 3 = 4.828788 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      93.22417 
    ##  b    cm      90.86792 
    ##  b    pd      90.72083 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 91.83778
    ## 2      2 92.58500
    ## 3      3 90.60778
    ## 4      4 91.38667
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 0_20"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2   20.88  10.439  4.0683 0.108624   
    ## PDS        2    3.20   1.601  0.1577 0.859178   
    ## Erro a     4   40.62  10.154                    
    ## CICLO      3  328.69 109.564 14.2172 0.003908 **
    ## Erro b     6   46.24   7.706                    
    ## PDS*CICLO  6   99.04  16.506  1.0792 0.388191   
    ## Erro c    48  734.15  15.295                    
    ## Total     71 1272.82                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.504009 %
    ## CV 2 = 3.052597 %
    ## CV 3 = 4.300453 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 90.64292
    ## 2     pd 91.10417
    ## 3     pp 91.07500
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   93.55722 
    ## a     4   91.99389 
    ## ab    3   90.44222 
    ##  b    1   87.76944 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 20_30"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2   74.25 37.127 4.3757 0.09840 .
    ## PDS        2  155.87 77.936 2.8667 0.16889  
    ## Erro a     4  108.75 27.187                 
    ## CICLO      3  157.57 52.523 8.6755 0.01333 *
    ## Erro b     6   36.33  6.054                 
    ## PDS*CICLO  6  248.33 41.388 1.6718 0.14845  
    ## Erro c    48 1188.31 24.756                 
    ## Total     71 1969.40                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.605154 %
    ## CV 2 = 2.645057 %
    ## CV 3 = 5.348735 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 94.77417
    ## 2     pd 93.12250
    ## 3     pp 91.17417
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   95.52833 
    ##  b    3   92.66278 
    ##  b    1   92.11278 
    ##  b    4   91.79056 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 30_70"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## ------------------------------------------------------------------------
    ## Legenda:
    ## FATOR 1 (parcela):  PDS 
    ## FATOR 2 (subparcela):  CICLO 
    ## ------------------------------------------------------------------------
    ## 
    ## ------------------------------------------------------------------------
    ## Quadro da analise de variancia
    ## ------------------------------------------------------------------------
    ##           GL      SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2   75.77 37.884 5.8451 0.06499 .
    ## PDS        2  133.06 66.528 5.5968 0.06931 .
    ## Erro a     4   47.55 11.887                 
    ## CICLO      3   68.25 22.751 1.5262 0.30124  
    ## Erro b     6   89.44 14.906                 
    ## PDS*CICLO  6  197.07 32.845 1.6170 0.16295  
    ## Erro c    48  974.96 20.312                 
    ## Total     71 1586.09                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.726702 %
    ## CV 2 = 4.173275 %
    ## CV 3 = 4.87154 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 90.60667
    ## 2     pd 93.67625
    ## 3     pp 93.25917
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 91.39722
    ## 2      2 93.06722
    ## 3      3 93.81556
    ## 4      4 91.77611
    ## ------------------------------------------------------------------------
