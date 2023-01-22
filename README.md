
# 1 Índice de Qualidade do Solo (IQS)

``` r
# Caregando os pacotes no R
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.3

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Warning: package 'tibble' was built under R version 4.1.2

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## Warning: package 'readr' was built under R version 4.1.3

``` r
library(nlme)
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.1.3

``` r
library(agricolae)
```

    ## Warning: package 'agricolae' was built under R version 4.1.2

``` r
library(ExpDes.pt)
library(pals)
```

    ## Warning: package 'pals' was built under R version 4.1.3

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 4.1.2

``` r
library(multcompView)
```

    ## Warning: package 'multcompView' was built under R version 4.1.3

``` r
library(lsmeans)
```

    ## Warning: package 'lsmeans' was built under R version 4.1.3

    ## Warning: package 'emmeans' was built under R version 4.1.3

``` r
library(rcompanion)
```

    ## Warning: package 'rcompanion' was built under R version 4.1.3

``` r
library(ggpubr)
library(rstatix)
library(lme4)
```

    ## Warning: package 'lme4' was built under R version 4.1.3

``` r
library(emmeans)
library(lmerTest)
```

    ## Warning: package 'lmerTest' was built under R version 4.1.3

# 3 Atributos do solo

## 3.1 Validação de dados

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
atributos<-names(dados[27:(length(dados)-2)])
```

## 3.3 Análise de variância

``` r
# Chaves de agrupamento
cob<-levels(factor(dados$cobertura))
prof<-levels(factor(dados$profundidade)) 
posi<-levels(factor(dados$linha_entrelinha)) 
posição<-levels(factor(dados$linha_entrelinha)) 
```

### 3.3.2 Desdobramento por contrastes

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
        for(k in 1:length(posição)){
          print("-----------------------------------------")
          print(paste(atributos[l],"-",cob[i],prof[j],posição[k],sep=" "))
          print("-----------------------------------------")
          
          # Criando dados auxiliar
          da1<-dados %>% 
            filter(cobertura==cob[i],
                  profundidade==prof[j])
          
          fl<-(da1$linha_entrelinha == posi[k])
          da1 <- da1[fl,]
          # Criando os dados com o trat adicional
          add <- adicional %>% 
            filter(profundidade==prof[j])
          fl<-(add$linha_entrelinha == posi[k])
          add <- add[fl,]
          
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
  }}}
}
```

    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value   Pr(>F)    
    ## trat                           15 0.14208 0.009472   4.746 0.000107 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00188 0.001878   0.941 0.339334    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00588 0.005878   2.945 0.095810 .  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.01480 0.014803   7.417 0.010377 *  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00001 0.000011   0.006 0.940987    
    ##   trat: Fatorial               11 0.11951 0.010865   5.444 7.99e-05 ***
    ## Residuals                      32 0.06387 0.001996                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value   Pr(>F)    
    ## trat                           15 0.16493 0.010995   3.898 0.000602 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00040 0.000400   0.142 0.708981    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00722 0.007225   2.561 0.119336    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00967 0.009669   3.428 0.073352 .  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00780 0.007803   2.766 0.106044    
    ##   trat: Fatorial               11 0.13983 0.012712   4.507 0.000404 ***
    ## Residuals                      32 0.09027 0.002821                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.11371 0.00758   1.724 0.09609 . 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00667 0.00667   1.517 0.22712   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00080 0.00080   0.183 0.67206   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.03803 0.03803   8.646 0.00604 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00134 0.00134   0.306 0.58417   
    ##   trat: Fatorial               11 0.06687 0.00608   1.382 0.22851   
    ## Residuals                      32 0.14073 0.00440                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.3330 0.02220   2.918 0.00537 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0056 0.00562   0.739 0.39627   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0625 0.06250   8.215 0.00729 **
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0001 0.00014   0.018 0.89444   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000 0.00000   0.000 0.98487   
    ##   trat: Fatorial               11 0.2648 0.02407   3.163 0.00535 **
    ## Residuals                      32 0.2435 0.00761                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15 0.2637 0.01758   1.479 0.1721  
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0011 0.00111   0.093 0.7618  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0022 0.00218   0.183 0.6715  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0617 0.06167   5.187 0.0296 *
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000 0.00001   0.001 0.9758  
    ##   trat: Fatorial               11 0.1987 0.01806   1.519 0.1729  
    ## Residuals                      32 0.3805 0.01189                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.23348 0.01557   2.830 0.00660 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00871 0.00871   1.584 0.21731   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00047 0.00047   0.085 0.77206   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.03610 0.03610   6.564 0.01532 * 
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00321 0.00321   0.584 0.45041   
    ##   trat: Fatorial               11 0.18499 0.01682   3.058 0.00664 **
    ## Residuals                      32 0.17600 0.00550                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value   Pr(>F)    
    ## trat                           15 0.17490 0.011660   5.922 1.25e-05 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00250 0.002500   1.270    0.268    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00063 0.000625   0.317    0.577    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00360 0.003600   1.829    0.186    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00028 0.000278   0.141    0.710    
    ##   trat: Fatorial               11 0.16790 0.015263   7.753 2.52e-06 ***
    ## Residuals                      32 0.06300 0.001969                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 0.18523 0.012349   3.195 0.00283 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00667 0.006669   1.726 0.19829   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.03004 0.030044   7.774 0.00885 **
    ##   trat: Add vs. Trat (ciclo_3)  1 0.02947 0.029469   7.626 0.00945 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00018 0.000178   0.046 0.83153   
    ##   trat: Fatorial               11 0.11887 0.010807   2.796 0.01143 * 
    ## Residuals                      32 0.12367 0.003865                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.03559 0.002373   0.956 0.5183  
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00321 0.003211   1.294 0.2637  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00111 0.001111   0.448 0.5082  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00000 0.000000   0.000 1.0000  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.01000 0.010000   4.030 0.0532 .
    ##   trat: Fatorial               11 0.02127 0.001934   0.779 0.6578  
    ## Residuals                      32 0.07940 0.002481                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.24618 0.01641   2.513 0.01405 * 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.01400 0.01400   2.144 0.15289   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.01361 0.01361   2.084 0.15857   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.07934 0.07934  12.147 0.00145 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00267 0.00267   0.409 0.52717   
    ##   trat: Fatorial               11 0.13656 0.01241   1.901 0.07724 . 
    ## Residuals                      32 0.20900 0.00653                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)
    ## trat                           15 0.1260 0.008398   0.677  0.787
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000 0.000025   0.002  0.964
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0005 0.000469   0.038  0.847
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0025 0.002500   0.201  0.657
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000 0.000011   0.001  0.976
    ##   trat: Fatorial               11 0.1230 0.011178   0.901  0.550
    ## Residuals                      32 0.3972 0.012413               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.08300 0.005533   1.998 0.0492 *
    ##   trat: Add vs. Trat (ciclo_1)  1 0.01604 0.016044   5.795 0.0220 *
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00174 0.001736   0.627 0.4343  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00810 0.008100   2.926 0.0969 .
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00028 0.000278   0.100 0.7535  
    ##   trat: Fatorial               11 0.05684 0.005167   1.866 0.0832 .
    ## Residuals                      32 0.08860 0.002769                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value   Pr(>F)    
    ## trat                           15 0.16033 0.010688   3.949 0.000539 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00934 0.009344   3.453 0.072361 .  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00174 0.001736   0.642 0.429068    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00100 0.001003   0.371 0.547007    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00538 0.005378   1.987 0.168283    
    ##   trat: Fatorial               11 0.14286 0.012988   4.799 0.000240 ***
    ## Residuals                      32 0.08660 0.002706                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 0.15670 0.010447   3.561 0.00125 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00723 0.007225   2.463 0.12639   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00360 0.003600   1.227 0.27620   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.02300 0.023003   7.842 0.00859 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00284 0.002844   0.970 0.33214   
    ##   trat: Fatorial               11 0.12003 0.010912   3.720 0.00177 **
    ## Residuals                      32 0.09387 0.002933                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.16936 0.01129   2.369 0.01992 * 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.03738 0.03738   7.841 0.00859 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00722 0.00722   1.516 0.22724   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00040 0.00040   0.084 0.77393   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00538 0.00538   1.128 0.29611   
    ##   trat: Fatorial               11 0.11898 0.01082   2.269 0.03500 * 
    ## Residuals                      32 0.15253 0.00477                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15 0.19413 0.01294   2.248 0.0268 *
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00160 0.00160   0.278 0.6017  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00028 0.00028   0.048 0.8276  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.04202 0.04202   7.298 0.0109 *
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00934 0.00934   1.623 0.2119  
    ##   trat: Fatorial               11 0.14088 0.01281   2.224 0.0386 *
    ## Residuals                      32 0.18427 0.00576                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)
    ## trat                           15 0.1911 0.012738   0.967  0.508
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0015 0.001469   0.112  0.741
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0013 0.001344   0.102  0.751
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0005 0.000469   0.036  0.851
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0015 0.001469   0.112  0.741
    ##   trat: Fatorial               11 0.1863 0.016937   1.286  0.277
    ## Residuals                      32 0.4215 0.013173               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.05275 0.003517   0.963 0.5117  
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00250 0.002500   0.685 0.4140  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00010 0.000100   0.027 0.8696  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00134 0.001344   0.368 0.5482  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.02200 0.022003   6.028 0.0197 *
    ##   trat: Fatorial               11 0.02680 0.002436   0.668 0.7576  
    ## Residuals                      32 0.11680 0.003650                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value   Pr(>F)    
    ## trat                           15 0.13106 0.008737   4.639 0.000132 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00034 0.000336   0.178 0.675520    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00007 0.000069   0.037 0.848937    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.01174 0.011736   6.232 0.017892 *  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00147 0.001469   0.780 0.383657    
    ##   trat: Fatorial               11 0.11745 0.010677   5.669 5.52e-05 ***
    ## Residuals                      32 0.06027 0.001883                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.07858 0.005239   1.727 0.0953 .
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00340 0.003403   1.122 0.2975  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00023 0.000225   0.074 0.7871  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00202 0.002025   0.668 0.4199  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.00134 0.001344   0.443 0.5103  
    ##   trat: Fatorial               11 0.07158 0.006508   2.145 0.0457 *
    ## Residuals                      32 0.09707 0.003033                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## trat                           15 0.11867 0.007911   1.978 0.0518 .
    ##   trat: Add vs. Trat (ciclo_1)  1 0.01068 0.010678   2.669 0.1121  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00022 0.000225   0.056 0.8140  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00054 0.000544   0.136 0.7146  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.01102 0.011025   2.756 0.1066  
    ##   trat: Fatorial               11 0.09619 0.008745   2.186 0.0418 *
    ## Residuals                      32 0.12800 0.004000                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.26401 0.01760   3.495 0.00144 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.00100 0.00100   0.199 0.65842   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.05367 0.05367  10.658 0.00261 **
    ##   trat: Add vs. Trat (ciclo_3)  1 0.00003 0.00003   0.005 0.94426   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.01000 0.01000   1.986 0.16841   
    ##   trat: Fatorial               11 0.19932 0.01812   3.598 0.00224 **
    ## Residuals                      32 0.16113 0.00504                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq  Mean Sq F value Pr(>F)
    ## trat                           15 0.2034 0.013558   1.156  0.352
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0004 0.000400   0.034  0.855
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0027 0.002669   0.228  0.636
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0016 0.001600   0.136  0.714
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0034 0.003403   0.290  0.594
    ##   trat: Fatorial               11 0.1953 0.017754   1.514  0.175
    ## Residuals                      32 0.3752 0.011725               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 0.14118 0.00941   2.000 0.04904 * 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.01174 0.01174   2.494 0.12414   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.00667 0.00667   1.417 0.24263   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.05840 0.05840  12.410 0.00131 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.01068 0.01068   2.269 0.14181   
    ##   trat: Fatorial               11 0.05370 0.00488   1.037 0.43871   
    ## Residuals                      32 0.15060 0.00471                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.003967 0.0002644   14.10 4.58e-10 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000225 0.0002250   12.00  0.00153 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000000 0.0000000    0.00  1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000000 0.0000000    0.00  1.00000    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000225 0.0002250   12.00  0.00153 ** 
    ##   trat: Fatorial               11 0.003517 0.0003197   17.05 2.16e-10 ***
    ## Residuals                      32 0.000600 0.0000188                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 1.30e-03 8.667e-05   4.622 0.000137 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 2.80e-06 2.780e-06   0.148 0.702858    
    ##   trat: Add vs. Trat (ciclo_2)  1 1.11e-05 1.111e-05   0.593 0.447064    
    ##   trat: Add vs. Trat (ciclo_3)  1 2.50e-05 2.500e-05   1.333 0.256763    
    ##   trat: Add vs. Trat (ciclo_4)  1 1.11e-05 1.111e-05   0.593 0.447064    
    ##   trat: Fatorial               11 1.25e-03 1.136e-04   6.061 2.96e-05 ***
    ## Residuals                      32 6.00e-04 1.875e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## trat                           15 0.0007000 4.667e-05   1.723 0.0962 .
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000694 6.944e-05   2.564 0.1191  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000694 6.944e-05   2.564 0.1191  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000444 4.444e-05   1.641 0.2094  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000111 1.111e-05   0.410 0.5264  
    ##   trat: Fatorial               11 0.0005056 4.596e-05   1.697 0.1192  
    ## Residuals                      32 0.0008667 2.708e-05                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)
    ## trat                           15 0.0008583 5.722e-05   1.616  0.125
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0001000 1.000e-04   2.824  0.103
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001000 1.000e-04   2.824  0.103
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000111 1.111e-05   0.314  0.579
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000444 4.444e-05   1.255  0.271
    ##   trat: Fatorial               11 0.0006028 5.480e-05   1.547  0.163
    ## Residuals                      32 0.0011333 3.542e-05               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0007146 4.76e-05   2.079 0.040428 *  
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0004000 4.00e-04  17.455 0.000212 ***
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000028 2.80e-06   0.121 0.730004    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000111 1.11e-05   0.485 0.491261    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000444 4.44e-05   1.939 0.173336    
    ##   trat: Fatorial               11 0.0002563 2.33e-05   1.017 0.454675    
    ## Residuals                      32 0.0007333 2.29e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## trat                           15 0.0011313 7.542e-05   2.413 0.0179 *
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000250 2.500e-05   0.800 0.3778  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001000 1.000e-04   3.200 0.0831 .
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000694 6.944e-05   2.222 0.1458  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000250 2.500e-05   0.800 0.3778  
    ##   trat: Fatorial               11 0.0009118 8.289e-05   2.653 0.0155 *
    ## Residuals                      32 0.0010000 3.125e-05                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.003831 0.0002554  17.514 2.54e-11 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000000 0.0000000   0.000   1.0000    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000069 0.0000694   4.762   0.0366 *  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000025 0.0000250   1.714   0.1998    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000336 0.0003361  23.048 3.54e-05 ***
    ##   trat: Fatorial               11 0.003401 0.0003092  21.199 1.19e-11 ***
    ## Residuals                      32 0.000467 0.0000146                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)    
    ## trat                           15 0.0011812 7.875e-05   4.200 0.00032 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000694 6.944e-05   3.704 0.06322 .  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001778 1.778e-04   9.481 0.00424 ** 
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0001000 1.000e-04   5.333 0.02753 *  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000000 0.000e+00   0.000 1.00000    
    ##   trat: Fatorial               11 0.0008340 7.582e-05   4.044 0.00095 ***
    ## Residuals                      32 0.0006000 1.875e-05                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## trat                           15 0.0004646 3.097e-05   1.487 0.1689  
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000444 4.444e-05   2.133 0.1539  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000111 1.111e-05   0.533 0.4705  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000028 2.780e-06   0.133 0.7174  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0001000 1.000e-04   4.800 0.0359 *
    ##   trat: Fatorial               11 0.0003063 2.784e-05   1.336 0.2504  
    ## Residuals                      32 0.0006667 2.083e-05                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## trat                           15 0.0007646 5.097e-05   1.748 0.0907 .
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0001361 1.361e-04   4.667 0.0383 *
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000444 4.444e-05   1.524 0.2260  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000111 1.111e-05   0.381 0.5415  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000111 1.111e-05   0.381 0.5415  
    ##   trat: Fatorial               11 0.0005618 5.107e-05   1.751 0.1063  
    ## Residuals                      32 0.0009333 2.917e-05                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.0007479 4.986e-05   2.992 0.00452 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000000 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000000 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000028 2.780e-06   0.167 0.68581   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000111 1.111e-05   0.667 0.42025   
    ##   trat: Fatorial               11 0.0007340 6.673e-05   4.004 0.00102 **
    ## Residuals                      32 0.0005333 1.667e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0026646 1.776e-04   6.090 9.40e-06 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0002778 2.778e-04   9.524  0.00416 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0002778 2.778e-04   9.524  0.00416 ** 
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000444 4.444e-05   1.524  0.22603    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000694 6.944e-05   2.381  0.13266    
    ##   trat: Fatorial               11 0.0019951 1.814e-04   6.219 2.32e-05 ***
    ## Residuals                      32 0.0009333 2.917e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.003700 0.0002467  16.914 4.07e-11 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000025 0.0000250   1.714      0.2    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000025 0.0000250   1.714      0.2    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000000 0.0000000   0.000      1.0    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000400 0.0004000  27.429 9.97e-06 ***
    ##   trat: Fatorial               11 0.003250 0.0002955  20.260 2.19e-11 ***
    ## Residuals                      32 0.000467 0.0000146                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0017917 1.194e-04   5.212 4.44e-05 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000250 2.500e-05   1.091  0.30410    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001778 1.778e-04   7.758  0.00891 ** 
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000028 2.780e-06   0.121  0.73000    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000444 4.444e-05   1.939  0.17334    
    ##   trat: Fatorial               11 0.0015417 1.401e-04   6.116 2.72e-05 ***
    ## Residuals                      32 0.0007333 2.292e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0012146 0.0000810   3.533  0.00132 ** 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000028 0.0000028   0.121  0.73000    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0007111 0.0007111  31.030 3.78e-06 ***
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000028 0.0000028   0.121  0.73000    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000694 0.0000694   3.030  0.09133 .  
    ##   trat: Fatorial               11 0.0004285 0.0000390   1.700  0.11854    
    ## Residuals                      32 0.0007333 0.0000229                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.0006583 4.389e-05   1.915 0.06034 . 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000000 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000111 1.111e-05   0.485 0.49126   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0001778 1.778e-04   7.758 0.00891 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000000 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 0.0004694 4.268e-05   1.862 0.08389 . 
    ## Residuals                      32 0.0007333 2.292e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0006479 4.319e-05   3.456 0.001573 ** 
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000000 0.000e+00   0.000 1.000000    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001778 1.778e-04  14.222 0.000663 ***
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000028 2.780e-06   0.222 0.640550    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0001000 1.000e-04   8.000 0.008007 ** 
    ##   trat: Fatorial               11 0.0003674 3.340e-05   2.672 0.014849 *  
    ## Residuals                      32 0.0004000 1.250e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.0013479 8.986e-05   3.594 0.00116 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0001361 1.361e-04   5.444 0.02608 * 
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000250 2.500e-05   1.000 0.32481   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0002778 2.778e-04  11.111 0.00218 **
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000028 2.780e-06   0.111 0.74106   
    ##   trat: Fatorial               11 0.0009063 8.239e-05   3.295 0.00409 **
    ## Residuals                      32 0.0008000 2.500e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df   Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.004058 0.0002706  16.233 7.09e-11 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.000100 0.0001000   6.000 0.019962 *  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000069 0.0000694   4.167 0.049546 *  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000003 0.0000028   0.167 0.685810    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000278 0.0002778  16.667 0.000278 ***
    ##   trat: Fatorial               11 0.003608 0.0003280  19.682 3.24e-11 ***
    ## Residuals                      32 0.000533 0.0000167                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0016583 1.106e-04   4.824 9.24e-05 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000028 2.780e-06   0.121  0.73000    
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000028 2.780e-06   0.121  0.73000    
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000111 1.111e-05   0.485  0.49126    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0001778 1.778e-04   7.758  0.00891 ** 
    ##   trat: Fatorial               11 0.0014639 1.331e-04   5.807 4.42e-05 ***
    ## Residuals                      32 0.0007333 2.292e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## trat                           15 0.0019146 0.0001276   5.570 2.32e-05 ***
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0006250 0.0006250  27.273 1.04e-05 ***
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000694 0.0000694   3.030 0.091332 .  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000028 0.0000028   0.121 0.730004    
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000111 0.0000111   0.485 0.491261    
    ##   trat: Fatorial               11 0.0012063 0.0001097   4.785 0.000246 ***
    ## Residuals                      32 0.0007333 0.0000229                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## trat                           15 0.0011479 7.653e-05   1.837 0.0731 .
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000111 1.111e-05   0.267 0.6091  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000444 4.444e-05   1.067 0.3094  
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000028 2.780e-06   0.067 0.7979  
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0001000 1.000e-04   2.400 0.1312  
    ##   trat: Fatorial               11 0.0009896 8.996e-05   2.159 0.0443 *
    ## Residuals                      32 0.0013333 4.167e-05                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 0.0007646 5.097e-05   3.058 0.00387 **
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000028 2.780e-06   0.167 0.68581   
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0000250 2.500e-05   1.500 0.22961   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0000111 1.111e-05   0.667 0.42025   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0000028 2.780e-06   0.167 0.68581   
    ##   trat: Fatorial               11 0.0007229 6.572e-05   3.943 0.00115 **
    ## Residuals                      32 0.0005333 1.667e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## trat                           15 0.0006583 4.389e-05   1.915 0.0603 .
    ##   trat: Add vs. Trat (ciclo_1)  1 0.0000000 0.000e+00   0.000 1.0000  
    ##   trat: Add vs. Trat (ciclo_2)  1 0.0001000 1.000e-04   4.364 0.0448 *
    ##   trat: Add vs. Trat (ciclo_3)  1 0.0001000 1.000e-04   4.364 0.0448 *
    ##   trat: Add vs. Trat (ciclo_4)  1 0.0001000 1.000e-04   4.364 0.0448 *
    ##   trat: Fatorial               11 0.0003583 3.258e-05   1.421 0.2112  
    ## Residuals                      32 0.0007333 2.292e-05                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  419.1   27.94   4.769 0.000103 ***
    ##   trat: Add vs. Trat (ciclo_1)  1    5.9    5.86   1.001 0.324596    
    ##   trat: Add vs. Trat (ciclo_2)  1   15.2   15.18   2.592 0.117260    
    ##   trat: Add vs. Trat (ciclo_3)  1   42.4   42.38   7.233 0.011273 *  
    ##   trat: Add vs. Trat (ciclo_4)  1    0.6    0.56   0.096 0.759207    
    ##   trat: Fatorial               11  355.1   32.28   5.510 7.16e-05 ***
    ## Residuals                      32  187.5    5.86                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  503.9   33.59   4.232 0.000300 ***
    ##   trat: Add vs. Trat (ciclo_1)  1    0.9    0.91   0.115 0.737287    
    ##   trat: Add vs. Trat (ciclo_2)  1   17.4   17.40   2.192 0.148464    
    ##   trat: Add vs. Trat (ciclo_3)  1   24.6   24.55   3.093 0.088181 .  
    ##   trat: Add vs. Trat (ciclo_4)  1   23.3   23.35   2.941 0.096021 .  
    ##   trat: Fatorial               11  437.7   39.79   5.013 0.000165 ***
    ## Residuals                      32  254.0    7.94                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  358.4   23.89   1.812 0.07762 . 
    ##   trat: Add vs. Trat (ciclo_1)  1   23.2   23.22   1.761 0.19395   
    ##   trat: Add vs. Trat (ciclo_2)  1    4.2    4.22   0.320 0.57572   
    ##   trat: Add vs. Trat (ciclo_3)  1  109.6  109.59   8.310 0.00699 **
    ##   trat: Add vs. Trat (ciclo_4)  1    3.2    3.17   0.241 0.62704   
    ##   trat: Fatorial               11  218.2   19.84   1.504 0.17835   
    ## Residuals                      32  422.0   13.19                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15 1017.8   67.85   2.882 0.00583 **
    ##   trat: Add vs. Trat (ciclo_1)  1   22.1   22.09   0.938 0.33994   
    ##   trat: Add vs. Trat (ciclo_2)  1  171.6  171.61   7.290 0.01098 * 
    ##   trat: Add vs. Trat (ciclo_3)  1    0.7    0.69   0.029 0.86551   
    ##   trat: Add vs. Trat (ciclo_4)  1    0.2    0.17   0.007 0.93264   
    ##   trat: Fatorial               11  823.2   74.84   3.179 0.00518 **
    ## Residuals                      32  753.2   23.54                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  832.2   55.48   1.477 0.1728  
    ##   trat: Add vs. Trat (ciclo_1)  1   10.6   10.62   0.283 0.5987  
    ##   trat: Add vs. Trat (ciclo_2)  1    5.4    5.40   0.144 0.7071  
    ##   trat: Add vs. Trat (ciclo_3)  1  194.9  194.88   5.187 0.0296 *
    ##   trat: Add vs. Trat (ciclo_4)  1    0.0    0.00   0.000 0.9981  
    ##   trat: Fatorial               11  621.3   56.48   1.503 0.1786  
    ## Residuals                      32 1202.2   37.57                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  778.0   51.87   2.868 0.00604 **
    ##   trat: Add vs. Trat (ciclo_1)  1   24.8   24.77   1.369 0.25056   
    ##   trat: Add vs. Trat (ciclo_2)  1    0.5    0.47   0.026 0.87305   
    ##   trat: Add vs. Trat (ciclo_3)  1  124.0  123.99   6.855 0.01339 * 
    ##   trat: Add vs. Trat (ciclo_4)  1    9.8    9.80   0.542 0.46710   
    ##   trat: Fatorial               11  619.0   56.27   3.111 0.00595 **
    ## Residuals                      32  578.8   18.09                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  490.3   32.69   5.405 3.12e-05 ***
    ##   trat: Add vs. Trat (ciclo_1)  1    5.9    5.92   0.979    0.330    
    ##   trat: Add vs. Trat (ciclo_2)  1    2.6    2.57   0.424    0.520    
    ##   trat: Add vs. Trat (ciclo_3)  1   11.6   11.56   1.911    0.176    
    ##   trat: Add vs. Trat (ciclo_4)  1    0.1    0.05   0.008    0.927    
    ##   trat: Fatorial               11  470.2   42.75   7.068 6.55e-06 ***
    ## Residuals                      32  193.5    6.05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  545.0   36.33   3.312 0.00217 **
    ##   trat: Add vs. Trat (ciclo_1)  1   22.9   22.90   2.087 0.15826   
    ##   trat: Add vs. Trat (ciclo_2)  1   74.3   74.30   6.774 0.01390 * 
    ##   trat: Add vs. Trat (ciclo_3)  1   78.5   78.47   7.153 0.01169 * 
    ##   trat: Add vs. Trat (ciclo_4)  1    1.0    0.97   0.089 0.76770   
    ##   trat: Fatorial               11  368.4   33.49   3.053 0.00671 **
    ## Residuals                      32  351.0   10.97                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15 110.71    7.38   0.965 0.5102  
    ##   trat: Add vs. Trat (ciclo_1)  1  10.24   10.24   1.339 0.2558  
    ##   trat: Add vs. Trat (ciclo_2)  1   4.01    4.01   0.525 0.4741  
    ##   trat: Add vs. Trat (ciclo_3)  1   0.01    0.01   0.001 0.9785  
    ##   trat: Add vs. Trat (ciclo_4)  1  33.18   33.18   4.339 0.0453 *
    ##   trat: Fatorial               11  63.27    5.75   0.752 0.6823  
    ## Residuals                      32 244.71    7.65                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  816.2   54.41   2.634 0.01051 * 
    ##   trat: Add vs. Trat (ciclo_1)  1   50.9   50.86   2.462 0.12646   
    ##   trat: Add vs. Trat (ciclo_2)  1   40.6   40.58   1.964 0.17068   
    ##   trat: Add vs. Trat (ciclo_3)  1  249.4  249.43  12.074 0.00149 **
    ##   trat: Add vs. Trat (ciclo_4)  1   10.7   10.68   0.517 0.47730   
    ##   trat: Fatorial               11  464.6   42.24   2.045 0.05672 . 
    ## Residuals                      32  661.0   20.66                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)
    ## trat                           15  427.7   28.51   0.720  0.747
    ##   trat: Add vs. Trat (ciclo_1)  1    0.1    0.10   0.003  0.960
    ##   trat: Add vs. Trat (ciclo_2)  1    2.5    2.46   0.062  0.805
    ##   trat: Add vs. Trat (ciclo_3)  1    9.7    9.69   0.245  0.624
    ##   trat: Add vs. Trat (ciclo_4)  1    0.2    0.17   0.004  0.949
    ##   trat: Fatorial               11  415.3   37.75   0.954  0.505
    ## Residuals                      32 1266.6   39.58               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15 300.41   20.03   2.102 0.0382 *
    ##   trat: Add vs. Trat (ciclo_1)  1  69.67   69.67   7.313 0.0109 *
    ##   trat: Add vs. Trat (ciclo_2)  1  10.98   10.98   1.152 0.2911  
    ##   trat: Add vs. Trat (ciclo_3)  1  23.99   23.99   2.519 0.1223  
    ##   trat: Add vs. Trat (ciclo_4)  1   0.49    0.49   0.051 0.8229  
    ##   trat: Fatorial               11 195.28   17.75   1.864 0.0836 .
    ## Residuals                      32 304.83    9.53                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  436.8   29.12   3.568 0.001227 ** 
    ##   trat: Add vs. Trat (ciclo_1)  1   30.2   30.21   3.702 0.063282 .  
    ##   trat: Add vs. Trat (ciclo_2)  1    3.4    3.37   0.413 0.524868    
    ##   trat: Add vs. Trat (ciclo_3)  1    2.5    2.49   0.305 0.584461    
    ##   trat: Add vs. Trat (ciclo_4)  1   10.0   10.01   1.226 0.276425    
    ##   trat: Fatorial               11  390.7   35.52   4.352 0.000535 ***
    ## Residuals                      32  261.2    8.16                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  477.9   31.86   3.676 0.000969 ***
    ##   trat: Add vs. Trat (ciclo_1)  1   21.0   20.98   2.420 0.129618    
    ##   trat: Add vs. Trat (ciclo_2)  1   17.9   17.95   2.071 0.159841    
    ##   trat: Add vs. Trat (ciclo_3)  1   64.6   64.61   7.455 0.010201 *  
    ##   trat: Add vs. Trat (ciclo_4)  1    7.6    7.61   0.878 0.355816    
    ##   trat: Fatorial               11  366.8   33.34   3.847 0.001382 ** 
    ## Residuals                      32  277.4    8.67                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  498.8   33.26   2.240 0.02726 * 
    ##   trat: Add vs. Trat (ciclo_1)  1  116.1  116.06   7.817 0.00868 **
    ##   trat: Add vs. Trat (ciclo_2)  1    9.8    9.84   0.663 0.42163   
    ##   trat: Add vs. Trat (ciclo_3)  1    1.5    1.50   0.101 0.75261   
    ##   trat: Add vs. Trat (ciclo_4)  1   14.6   14.59   0.983 0.32893   
    ##   trat: Fatorial               11  356.8   32.44   2.185 0.04195 * 
    ## Residuals                      32  475.1   14.85                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  576.7   38.45   2.181 0.0315 *
    ##   trat: Add vs. Trat (ciclo_1)  1    5.3    5.28   0.300 0.5879  
    ##   trat: Add vs. Trat (ciclo_2)  1    1.7    1.69   0.096 0.7589  
    ##   trat: Add vs. Trat (ciclo_3)  1  117.8  117.79   6.681 0.0145 *
    ##   trat: Add vs. Trat (ciclo_4)  1   27.7   27.65   1.568 0.2195  
    ##   trat: Fatorial               11  424.3   38.57   2.188 0.0417 *
    ## Residuals                      32  564.2   17.63                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)
    ## trat                           15  631.6   42.11   1.011  0.469
    ##   trat: Add vs. Trat (ciclo_1)  1    6.4    6.38   0.153  0.698
    ##   trat: Add vs. Trat (ciclo_2)  1    3.5    3.47   0.083  0.775
    ##   trat: Add vs. Trat (ciclo_3)  1    1.9    1.87   0.045  0.834
    ##   trat: Add vs. Trat (ciclo_4)  1    4.2    4.17   0.100  0.754
    ##   trat: Fatorial               11  615.7   55.98   1.344  0.247
    ## Residuals                      32 1332.5   41.64               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  161.1   10.74   0.902 0.5700  
    ##   trat: Add vs. Trat (ciclo_1)  1    5.2    5.16   0.433 0.5152  
    ##   trat: Add vs. Trat (ciclo_2)  1    0.0    0.00   0.000 0.9935  
    ##   trat: Add vs. Trat (ciclo_3)  1    2.0    2.00   0.168 0.6846  
    ##   trat: Add vs. Trat (ciclo_4)  1   69.9   69.92   5.868 0.0213 *
    ##   trat: Fatorial               11   84.0    7.64   0.641 0.7803  
    ## Residuals                      32  381.3   11.91                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## trat                           15  381.9   25.46   4.716 0.000114 ***
    ##   trat: Add vs. Trat (ciclo_1)  1    1.1    1.05   0.195 0.661583    
    ##   trat: Add vs. Trat (ciclo_2)  1    1.0    0.96   0.178 0.676029    
    ##   trat: Add vs. Trat (ciclo_3)  1   34.4   34.40   6.371 0.016761 *  
    ##   trat: Add vs. Trat (ciclo_4)  1    9.3    9.27   1.717 0.199378    
    ##   trat: Fatorial               11  336.3   30.57   5.662 5.59e-05 ***
    ## Residuals                      32  172.8    5.40                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15 262.11  17.474   2.035 0.0450 *
    ##   trat: Add vs. Trat (ciclo_1)  1  11.31  11.312   1.318 0.2595  
    ##   trat: Add vs. Trat (ciclo_2)  1   1.27   1.273   0.148 0.7027  
    ##   trat: Add vs. Trat (ciclo_3)  1   5.62   5.617   0.654 0.4246  
    ##   trat: Add vs. Trat (ciclo_4)  1   1.41   1.408   0.164 0.6882  
    ##   trat: Fatorial               11 242.50  22.046   2.568 0.0185 *
    ## Residuals                      32 274.72   8.585                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)  
    ## trat                           15  353.9  23.591   1.966 0.0532 .
    ##   trat: Add vs. Trat (ciclo_1)  1   24.0  23.961   1.997 0.1673  
    ##   trat: Add vs. Trat (ciclo_2)  1    0.4   0.407   0.034 0.8550  
    ##   trat: Add vs. Trat (ciclo_3)  1    1.5   1.452   0.121 0.7302  
    ##   trat: Add vs. Trat (ciclo_4)  1   29.1  29.106   2.426 0.1292  
    ##   trat: Fatorial               11  298.9  27.176   2.265 0.0353 *
    ## Residuals                      32  383.9  11.998                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value  Pr(>F)   
    ## trat                           15  812.0   54.13   3.495 0.00144 **
    ##   trat: Add vs. Trat (ciclo_1)  1    3.3    3.35   0.216 0.64510   
    ##   trat: Add vs. Trat (ciclo_2)  1  162.4  162.44  10.486 0.00280 **
    ##   trat: Add vs. Trat (ciclo_3)  1    0.0    0.00   0.000 0.98894   
    ##   trat: Add vs. Trat (ciclo_4)  1   27.8   27.81   1.795 0.18973   
    ##   trat: Fatorial               11  618.4   56.22   3.629 0.00211 **
    ## Residuals                      32  495.7   15.49                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)
    ## trat                           15  652.8   43.52   1.185  0.331
    ##   trat: Add vs. Trat (ciclo_1)  1    1.7    1.67   0.046  0.832
    ##   trat: Add vs. Trat (ciclo_2)  1    9.2    9.23   0.251  0.620
    ##   trat: Add vs. Trat (ciclo_3)  1    2.8    2.75   0.075  0.786
    ##   trat: Add vs. Trat (ciclo_4)  1   10.9   10.91   0.297  0.589
    ##   trat: Fatorial               11  628.2   57.11   1.555  0.160
    ## Residuals                      32 1175.0   36.72               
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq F value Pr(>F)   
    ## trat                           15  437.9   29.19   1.915 0.0603 . 
    ##   trat: Add vs. Trat (ciclo_1)  1   39.4   39.35   2.582 0.1179   
    ##   trat: Add vs. Trat (ciclo_2)  1   19.1   19.14   1.256 0.2708   
    ##   trat: Add vs. Trat (ciclo_3)  1  167.3  167.27  10.974 0.0023 **
    ##   trat: Add vs. Trat (ciclo_4)  1   38.6   38.63   2.534 0.1212   
    ##   trat: Fatorial               11  173.5   15.77   1.035 0.4407   
    ## Residuals                      32  487.8   15.24                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15   6142   409.4 9.552e+27 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1      0     0.0 1.215e+01 0.00145 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1      0     0.0 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1      0     0.0 0.000e+00 0.98811    
    ##   trat: Add vs. Trat (ciclo_4)  1    546   545.9 1.274e+28 < 2e-16 ***
    ##   trat: Fatorial               11   5596   508.7 1.187e+28 < 2e-16 ***
    ## Residuals                      32      0     0.0                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 5.937e-25 3.96e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 4.750e-25 4.75e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 1.187e-25 1.08e-26   0.273 0.98689   
    ## Residuals                      32 1.267e-24 3.96e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15   6142   409.4 9.552e+27 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1      0     0.0 1.215e+01 0.00145 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1      0     0.0 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1      0     0.0 0.000e+00 0.98811    
    ##   trat: Add vs. Trat (ciclo_4)  1    546   545.9 1.274e+28 < 2e-16 ***
    ##   trat: Fatorial               11   5596   508.7 1.187e+28 < 2e-16 ***
    ## Residuals                      32      0     0.0                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 5.937e-25 3.96e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 4.750e-25 4.75e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 1.187e-25 1.08e-26   0.273 0.98689   
    ## Residuals                      32 1.267e-24 3.96e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15   6142   409.4 9.552e+27 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1      0     0.0 1.215e+01 0.00145 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1      0     0.0 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1      0     0.0 0.000e+00 0.98811    
    ##   trat: Add vs. Trat (ciclo_4)  1    546   545.9 1.274e+28 < 2e-16 ***
    ##   trat: Fatorial               11   5596   508.7 1.187e+28 < 2e-16 ***
    ## Residuals                      32      0     0.0                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 5.937e-25 3.96e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 4.750e-25 4.75e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 1.187e-25 1.08e-26   0.273 0.98689   
    ## Residuals                      32 1.267e-24 3.96e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15   6142   409.4 9.552e+27 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1      0     0.0 1.215e+01 0.00145 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1      0     0.0 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1      0     0.0 0.000e+00 0.98811    
    ##   trat: Add vs. Trat (ciclo_4)  1    546   545.9 1.274e+28 < 2e-16 ***
    ##   trat: Fatorial               11   5596   508.7 1.187e+28 < 2e-16 ***
    ## Residuals                      32      0     0.0                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 5.937e-25 3.96e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 4.750e-25 4.75e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 1.187e-25 1.08e-26   0.273 0.98689   
    ## Residuals                      32 1.267e-24 3.96e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.745e-24 1.163e-25   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.396e-24 1.396e-24  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 3.490e-25 3.170e-26   0.273 0.98689   
    ## Residuals                      32 3.722e-24 1.163e-25                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq  Mean Sq F value  Pr(>F)   
    ## trat                           15 4.362e-25 2.91e-26   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 3.490e-25 3.49e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.00e+00   0.000 1.00000   
    ##   trat: Fatorial               11 8.720e-26 7.90e-27   0.273 0.98689   
    ## Residuals                      32 9.306e-25 2.91e-26                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15  3.591  0.2394 5.159e+25 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1  0.000  0.0000 1.200e+01 0.00154 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1  0.000  0.0000 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1  0.000  0.0000 0.000e+00 0.99903    
    ##   trat: Add vs. Trat (ciclo_4)  1  0.319  0.3192 6.879e+25 < 2e-16 ***
    ##   trat: Fatorial               11  3.272  0.2975 6.410e+25 < 2e-16 ***
    ## Residuals                      32  0.000  0.0000                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15  3.591  0.2394 5.159e+25 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1  0.000  0.0000 1.200e+01 0.00154 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1  0.000  0.0000 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1  0.000  0.0000 0.000e+00 0.99903    
    ##   trat: Add vs. Trat (ciclo_4)  1  0.319  0.3192 6.879e+25 < 2e-16 ***
    ##   trat: Fatorial               11  3.272  0.2975 6.410e+25 < 2e-16 ***
    ## Residuals                      32  0.000  0.0000                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15  3.591  0.2394 5.159e+25 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1  0.000  0.0000 1.200e+01 0.00154 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1  0.000  0.0000 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1  0.000  0.0000 0.000e+00 0.99903    
    ##   trat: Add vs. Trat (ciclo_4)  1  0.319  0.3192 6.879e+25 < 2e-16 ***
    ##   trat: Fatorial               11  3.272  0.2975 6.410e+25 < 2e-16 ***
    ## Residuals                      32  0.000  0.0000                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## trat                           15  3.591  0.2394 5.159e+25 < 2e-16 ***
    ##   trat: Add vs. Trat (ciclo_1)  1  0.000  0.0000 1.200e+01 0.00154 ** 
    ##   trat: Add vs. Trat (ciclo_2)  1  0.000  0.0000 0.000e+00 1.00000    
    ##   trat: Add vs. Trat (ciclo_3)  1  0.000  0.0000 0.000e+00 0.99903    
    ##   trat: Add vs. Trat (ciclo_4)  1  0.319  0.3192 6.879e+25 < 2e-16 ***
    ##   trat: Fatorial               11  3.272  0.2975 6.410e+25 < 2e-16 ***
    ## Residuals                      32  0.000  0.0000                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.091e-25 7.270e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 8.724e-26 8.724e-26  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.181e-26 1.980e-27   0.273 0.98689   
    ## Residuals                      32 2.326e-25 7.270e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Fatorial vs Testemunha adicional -------------"
    ##                                Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## trat                           15 1.484e-25 9.900e-27   1.000 0.47859   
    ##   trat: Add vs. Trat (ciclo_1)  1 1.187e-25 1.188e-25  12.000 0.00153 **
    ##   trat: Add vs. Trat (ciclo_2)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_3)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Add vs. Trat (ciclo_4)  1 0.000e+00 0.000e+00   0.000 1.00000   
    ##   trat: Fatorial               11 2.970e-26 2.700e-27   0.273 0.98689   
    ## Residuals                      32 3.167e-25 9.900e-27                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### 3.3.3 Análise em faixas

``` r
for(l in 1:length(atributos)){
  for(i in 1:length(cob)){
    for(j in 1:length(prof)){
        for(k in 1:length(posição)){
          print("-----------------------------------------")
          print(paste(atributos[l],"-",cob[i],prof[j],posição[k],sep=" "))
          print("-----------------------------------------")
          
          # Criando dados auxiliar
          da1<-dados %>% 
            filter(cobertura==cob[i],
                  profundidade==prof[j])
          
          fl<-(da1$linha_entrelinha == posi[k])
          da1 <- da1[fl,]
          y<-da1[,5+l]

      print("------------------ Análise em Faixas -------------")
      
      if(sd(y)==0 | is.na(sd(y))) {print("dados sem variação")
        }else{
      faixas(da1$PDS,da1$ciclo,da1$bloco,y,
                    quali=c(TRUE,TRUE), mcomp = "tukey",
                    fac.names = c("PDS","CICLO"))}
      
       cat("\n")
  }}}
}
```

    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 0_20 el"
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
    ##           GL       SQ       QM     Fc  Pr(>Fc)    
    ## Bloco      2 0.005422 0.002711 -5.880 1.000000    
    ## PDS        2 0.000239 0.000119  0.177 0.843754    
    ## Erro a     4 0.002694 0.000674                    
    ## CICLO      3 0.100289 0.033430 31.782 0.000444 ***
    ## Erro b     6 0.006311 0.001052                    
    ## PDS*CICLO  6 0.022561 0.003760  1.720 0.199646    
    ## Erro c    12 0.026239 0.002187                    
    ## Total     35 0.163756                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.50993 %
    ## CV 2 = 1.886816 %
    ## CV 3 = 2.72041 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.722500
    ## 2     pd 1.716667
    ## 3     pp 1.717500
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.763333 
    ## a     4   1.745556 
    ## a     3   1.737778 
    ##  b    1   1.628889 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 0_20 l"
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
    ## Bloco      2 0.027517 0.013758  4.4105 0.09734 . 
    ## PDS        2 0.017317 0.008658  2.9309 0.16452   
    ## Erro a     4 0.011817 0.002954                   
    ## CICLO      3 0.115475 0.038492 19.3264 0.00174 **
    ## Erro b     6 0.011950 0.001992                   
    ## PDS*CICLO  6 0.017083 0.002847  1.5589 0.24119   
    ## Erro c    12 0.021917 0.001826                   
    ## Total     35 0.223075                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.384675 %
    ## CV 2 = 2.779124 %
    ## CV 3 = 2.661314 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.618333
    ## 2     pd 1.624167
    ## 3     pp 1.575000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   1.67 
    ## a     2   1.652222 
    ##  b    3   1.565556 
    ##  b    1   1.535556 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 20_30 el"
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
    ## Bloco      2 0.000439 0.000219 0.0164 0.98381  
    ## PDS        2 0.001356 0.000678 0.0464 0.95513  
    ## Erro a     4 0.058378 0.014594                 
    ## CICLO      3 0.056678 0.018893 6.1365 0.02933 *
    ## Erro b     6 0.018472 0.003079                 
    ## PDS*CICLO  6 0.053222 0.008870 2.0718 0.13326  
    ## Erro c    12 0.051378 0.004281                 
    ## Total     35 0.239922                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 7.035052 %
    ## CV 2 = 3.231152 %
    ## CV 3 = 3.8104 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.725000
    ## 2     pd 1.716667
    ## 3     pp 1.710000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   1.764444 
    ## ab    2   1.744444 
    ## ab    4   1.696667 
    ##  b    1   1.663333 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 20_30 l"
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
    ## Bloco      2 0.01832 0.009158 0.7807 0.51730  
    ## PDS        2 0.01792 0.008958 0.7237 0.53921  
    ## Erro a     4 0.04952 0.012379                 
    ## CICLO      3 0.23098 0.076993 8.9344 0.01243 *
    ## Erro b     6 0.05171 0.008618                 
    ## PDS*CICLO  6 0.04677 0.007795 0.8413 0.56172  
    ## Erro c    12 0.11119 0.009266                 
    ## Total     35 0.52640                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 6.896387 %
    ## CV 2 = 5.753987 %
    ## CV 3 = 5.966599 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.642500
    ## 2     pd 1.609167
    ## 3     pp 1.588333
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.718889 
    ## ab    3   1.632222 
    ## ab    4   1.607778 
    ##  b    1   1.494444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 30_70 el"
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
    ## Bloco      2 0.009089 0.004544 2.1028 0.23763  
    ## PDS        2 0.011822 0.005911 1.2704 0.37398  
    ## Erro a     4 0.018611 0.004653                 
    ## CICLO      3 0.083089 0.027696 5.5931 0.03579 *
    ## Erro b     6 0.029711 0.004952                 
    ## PDS*CICLO  6 0.064378 0.010730 1.4415 0.27726  
    ## Erro c    12 0.089322 0.007444                 
    ## Total     35 0.306022                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.114621 %
    ## CV 2 = 4.244803 %
    ## CV 3 = 5.204306 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.663333
    ## 2     pd 1.676667
    ## 3     pp 1.633333
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.737778 
    ## ab    1   1.652222 
    ## ab    4   1.624444 
    ##  b    3   1.616667 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - amendoim 30_70 l"
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
    ## Bloco      2 0.00487 0.002433 0.2472 0.79211  
    ## PDS        2 0.02645 0.013225 1.4421 0.33761  
    ## Erro a     4 0.03668 0.009171                 
    ## CICLO      3 0.09180 0.030600 4.6208 0.05299 .
    ## Erro b     6 0.03973 0.006622                 
    ## PDS*CICLO  6 0.09188 0.015314 2.5744 0.07704 .
    ## Erro c    12 0.07138 0.005949                 
    ## Total     35 0.36280                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.899249 %
    ## CV 2 = 5.012959 %
    ## CV 3 = 4.751165 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.661667
    ## 2     pd 1.604167
    ## 3     pp 1.604167
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.543333
    ## 2      2 1.666667
    ## 3      3 1.616667
    ## 4      4 1.666667
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 0_20 el"
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
    ## Bloco      2 0.002467 0.001233  0.3181 0.744413   
    ## PDS        2 0.004517 0.002258  0.9393 0.462978   
    ## Erro a     4 0.009617 0.002404                    
    ## CICLO      3 0.121567 0.040522 16.0661 0.002842 **
    ## Erro b     6 0.015133 0.002522                    
    ## PDS*CICLO  6 0.013017 0.002169  2.0689 0.133704   
    ## Erro c    12 0.012583 0.001049                    
    ## Total     35 0.178900                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.904184 %
    ## CV 2 = 2.974634 %
    ## CV 3 = 1.918002 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.695833
    ## 2     pd 1.672500
    ## 3     pp 1.696667
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   1.738889 
    ## a     4   1.732222 
    ## a     2   1.688889 
    ##  b    1   1.593333 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 0_20 l"
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
    ## Bloco      2 0.008906 0.004453 0.7270 0.53789  
    ## PDS        2 0.066372 0.033186 6.8859 0.05066 .
    ## Erro a     4 0.019278 0.004819                 
    ## CICLO      3 0.071256 0.023752 4.5442 0.05478 .
    ## Erro b     6 0.031361 0.005227                 
    ## PDS*CICLO  6 0.035228 0.005871 1.4973 0.25946  
    ## Erro c    12 0.047056 0.003921                 
    ## Total     35 0.279456                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.301549 %
    ## CV 2 = 4.479675 %
    ## CV 3 = 3.880085 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.672500
    ## 2     pd 1.598333
    ## 3     pp 1.570833
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.561111
    ## 2      2 1.588889
    ## 3      3 1.625556
    ## 4      4 1.680000
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 20_30 el"
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
    ## Bloco      2 0.002072 0.001036 0.12202  0.8883
    ## PDS        2 0.006872 0.003436 0.76594  0.5228
    ## Erro a     4 0.017944 0.004486                
    ## CICLO      3 0.006764 0.002255 0.42548  0.7420
    ## Erro b     6 0.031794 0.005299                
    ## PDS*CICLO  6 0.017594 0.002932 2.26700  0.1073
    ## Erro c    12 0.015522 0.001294                
    ## Total     35 0.098564                         
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.921327 %
    ## CV 2 = 4.261848 %
    ## CV 3 = 2.105641 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.727500
    ## 2     pd 1.700000
    ## 3     pp 1.696667
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.691111
    ## 2      2 1.698889
    ## 3      3 1.716667
    ## 4      4 1.725556
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 20_30 l"
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
    ## Bloco      2 0.00702 0.003508 1.3894 0.34818  
    ## PDS        2 0.06847 0.034233 3.8938 0.11515  
    ## Erro a     4 0.03517 0.008792                 
    ## CICLO      3 0.06119 0.020396 4.6541 0.05224 .
    ## Erro b     6 0.02629 0.004382                 
    ## PDS*CICLO  6 0.07178 0.011963 1.1234 0.40496  
    ## Erro c    12 0.12779 0.010649                 
    ## Total     35 0.39770                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.597844 %
    ## CV 2 = 3.952224 %
    ## CV 3 = 6.160857 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.736667
    ## 2     pd 1.643333
    ## 3     pp 1.645000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.632222
    ## 2      2 1.643333
    ## 3      3 1.687778
    ## 4      4 1.736667
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 30_70 el"
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
    ## Bloco      2 0.035356 0.017678 16.4870  0.0117 *
    ## PDS        2 0.001672 0.000836  0.1692  0.8500  
    ## Erro a     4 0.019761 0.004940                  
    ## CICLO      3 0.023031 0.007677  2.2312  0.1852  
    ## Erro b     6 0.020644 0.003441                  
    ## PDS*CICLO  6 0.024194 0.004032  0.5517  0.7602  
    ## Erro c    12 0.087706 0.007309                  
    ## Total     35 0.212364                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.333509 %
    ## CV 2 = 3.616513 %
    ## CV 3 = 5.270926 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.625000
    ## 2     pd 1.628333
    ## 3     pp 1.612500
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.591111
    ## 2      2 1.603333
    ## 3      3 1.651111
    ## 4      4 1.642222
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - crotalária 30_70 l"
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
    ## Bloco      2 0.004239 0.002119 0.6629  0.5641
    ## PDS        2 0.014039 0.007019 2.4714  0.2001
    ## Erro a     4 0.011361 0.002840               
    ## CICLO      3 0.010633 0.003544 1.1826  0.3923
    ## Erro b     6 0.017983 0.002997               
    ## PDS*CICLO  6 0.033450 0.005575 2.1115  0.1275
    ## Erro c    12 0.031683 0.002640               
    ## Total     35 0.123389                        
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.288641 %
    ## CV 2 = 3.378279 %
    ## CV 3 = 3.170742 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.648333
    ## 2     pd 1.604167
    ## 3     pp 1.609167
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.627778
    ## 2      2 1.611111
    ## 3      3 1.598889
    ## 4      4 1.644444
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 0_20 el"
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
    ##           GL       SQ       QM     Fc  Pr(>Fc)    
    ## Bloco      2 0.027639 0.013819 81.557 0.000573 ***
    ## PDS        2 0.022039 0.011019 28.235 0.004376 ** 
    ## Erro a     4 0.001561 0.000390                    
    ## CICLO      3 0.064342 0.021447 12.236 0.005736 ** 
    ## Erro b     6 0.010517 0.001753                    
    ## PDS*CICLO  6 0.044583 0.007431  3.765 0.024190 *  
    ## Erro c    12 0.023683 0.001974                    
    ## Total     35 0.194364                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.164178 %
    ## CV 2 = 2.467151 %
    ## CV 3 = 2.617961 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL         SQ       QM        Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.00628900 0.003144  1.992677 0.174576
    ## PDS : CICLO 2   2.00000 0.00180000 0.000900  0.570342  0.57842
    ## PDS : CICLO 3   2.00000 0.03326700 0.016633 10.540769  0.00176
    ## PDS : CICLO 4   2.00000 0.02526700 0.012633  8.005915 0.005123
    ## Erro combinado 13.45749 0.02123593 0.001578                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.596667
    ## 2     pd 1.660000
    ## 3     pp 1.616667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.703333
    ## 2     pd 1.733333
    ## 3     pp 1.703333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.803333 
    ##  b    cm      1.696667 
    ##  b    pd      1.66 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.803333 
    ##  b    cm      1.706667 
    ##  b    pd      1.68 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL         SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.02522500 0.008408 4.424662 0.017757
    ## CICLO : PDS pd   3.00000 0.01080000 0.003600 1.894404 0.168533
    ## CICLO : PDS pp   3.00000 0.07290000 0.024300 12.78723 0.000124
    ## Erro combinado  17.15028 0.03258552 0.001900                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   1.706667 
    ## a     2   1.703333 
    ## ab    3   1.696667 
    ##  b    1   1.596667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.660000
    ## 2      2 1.733333
    ## 3      3 1.660000
    ## 4      4 1.680000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   1.803333 
    ## a     4   1.803333 
    ## ab    2   1.703333 
    ##  b    1   1.616667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 0_20 l"
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
    ## Bloco      2 0.010850 0.005425   1.797 0.27749    
    ## PDS        2 0.006867 0.003433   0.561 0.60991    
    ## Erro a     4 0.024483 0.006121                    
    ## CICLO      3 0.078100 0.026033 110.259 1.2e-05 ***
    ## Erro b     6 0.001417 0.000236                    
    ## PDS*CICLO  6 0.060333 0.010056   3.013 0.04917 *  
    ## Erro c    12 0.040050 0.003338                    
    ## Total     35 0.222100                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.775326 %
    ## CV 2 = 0.9378987 %
    ## CV 3 = 3.526212 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                    GL         SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.000 0.01135600 0.005678 1.407568 0.275885
    ## PDS : CICLO 2   2.000 0.02962200 0.014811 3.671797 0.050913
    ## PDS : CICLO 3   2.000 0.02420000 0.012100  2.99969 0.080839
    ## PDS : CICLO 4   2.000 0.00202200 0.001011 0.250663 0.781547
    ## Erro combinado 14.689 0.05925543 0.004034                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.616667
    ## 2     pd 1.553333
    ## 3     pp 1.533333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.670000
    ## 2     pd 1.573333
    ## 3     pp 1.710000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.563333
    ## 2     pd 1.673333
    ## 3     pp 1.673333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.716667
    ## 2     pd 1.680000
    ## 3     pp 1.696667
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL         SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.03956700 0.013189 5.724344 0.010645
    ## CICLO : PDS pd   3.00000 0.03920000 0.013067 5.671296 0.010992
    ## CICLO : PDS pp   3.00000 0.05966700 0.019889  8.63233  0.00225
    ## Erro combinado  12.55832 0.02893438 0.002304                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   1.716667 
    ## ab    2   1.67 
    ## ab    1   1.616667 
    ##  b    3   1.563333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   1.68 
    ## a     3   1.673333 
    ## ab    2   1.573333 
    ##  b    1   1.553333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.71 
    ## a     4   1.696667 
    ## a     3   1.673333 
    ##  b    1   1.533333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 20_30 el"
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
    ## Bloco      2 0.041450 0.020725 1.8273 0.27307   
    ## PDS        2 0.022200 0.011100 1.3175 0.36344   
    ## Erro a     4 0.033700 0.008425                  
    ## CICLO      3 0.032811 0.010937 1.9625 0.22109   
    ## Erro b     6 0.033439 0.005573                  
    ## PDS*CICLO  6 0.093422 0.015570 5.8613 0.00465 **
    ## Erro c    12 0.031878 0.002656                  
    ## Total     35 0.288900                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.468985 %
    ## CV 2 = 4.448075 %
    ## CV 3 = 3.070967 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL         SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.02815600 0.014078 3.435071 0.067144
    ## PDS : CICLO 2   2.00000 0.04882200 0.024411 5.956472 0.016498
    ## PDS : CICLO 3   2.00000 0.03235600 0.016178 3.947484 0.049055
    ## PDS : CICLO 4   2.00000 0.00628900 0.003144 0.767265 0.486348
    ## Erro combinado 11.66572 0.04780612 0.004098                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.596667
    ## 2     pd 1.720000
    ## 3     pp 1.710000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.73 
    ## ab    cm      1.61 
    ##  b    pp      1.553333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.77 
    ## ab    cm      1.69 
    ##  b    pd      1.623333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.676667
    ## 2     pd 1.720000
    ## 3     pp 1.740000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL         SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.01973300 0.006578 1.812892 0.181913
    ## CICLO : PDS pd   3.00000 0.02270000 0.007567 2.085439 0.138946
    ## CICLO : PDS pp   3.00000 0.08380000 0.027933 7.698668 0.001723
    ## Erro combinado  17.51755 0.06355368 0.003628                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.596667
    ## 2      2 1.610000
    ## 3      3 1.690000
    ## 4      4 1.676667
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.720000
    ## 2      2 1.730000
    ## 3      3 1.623333
    ## 4      4 1.720000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   1.77 
    ## a     4   1.74 
    ## a     1   1.71 
    ##  b    2   1.553333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 20_30 l"
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
    ## Bloco      2 0.02061 0.010303 0.88626  0.4802
    ## PDS        2 0.05396 0.026978 1.95570  0.2556
    ## Erro a     4 0.05518 0.013794                
    ## CICLO      3 0.02641 0.008804 2.27301  0.1803
    ## Erro b     6 0.02324 0.003873                
    ## PDS*CICLO  6 0.07689 0.012815 2.12075  0.1261
    ## Erro c    12 0.07251 0.006043                
    ## Total     35 0.32879                         
    ## ------------------------------------------------------------------------
    ## CV 1 = 7.072919 %
    ## CV 2 = 3.747819 %
    ## CV 3 = 4.681211 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.638333
    ## 2     pd 1.628333
    ## 3     pp 1.715000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.640000
    ## 2      2 1.630000
    ## 3      3 1.674444
    ## 4      4 1.697778
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 30_70 el"
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
    ##           GL       SQ       QM     Fc  Pr(>Fc)   
    ## Bloco      2 0.004822 0.002411  0.206 0.822263   
    ## PDS        2 0.046339 0.023169 52.131 0.001365 **
    ## Erro a     4 0.001778 0.000444                   
    ## CICLO      3 0.029400 0.009800  0.557 0.662328   
    ## Erro b     6 0.105533 0.017589                   
    ## PDS*CICLO  6 0.043550 0.007258  1.151 0.391862   
    ## Erro c    12 0.075667 0.006306                   
    ## Total     35 0.307089                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.322207 %
    ## CV 2 = 8.317826 %
    ## CV 3 = 4.980263 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.644167 
    ##  b    cm      1.578333 
    ##  b    pd      1.560833 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.597778
    ## 2      2 1.624444
    ## 3      3 1.547778
    ## 4      4 1.607778
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - milheto 30_70 l"
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
    ## Bloco      2 0.011006 0.005503 -2.0657  1.0000  
    ## PDS        2 0.002772 0.001386  5.5754  0.0697 .
    ## Erro a     4 0.000994 0.000249                  
    ## CICLO      3 0.008711 0.002904  1.1236  0.4112  
    ## Erro b     6 0.015506 0.002584                  
    ## PDS*CICLO  6 0.020072 0.003345  0.6086  0.7196  
    ## Erro c    12 0.065961 0.005497                  
    ## Total     35 0.125022                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.9686461 %
    ## CV 2 = 3.123007 %
    ## CV 3 = 4.554684 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.615833
    ## 2     pd 1.630833
    ## 3     pp 1.636667
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.643333
    ## 2      2 1.625556
    ## 3      3 1.638889
    ## 4      4 1.603333
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 0_20 el"
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
    ## Bloco      2 0.002272 0.001136 -1.697 1.00000    
    ## PDS        2 0.000606 0.000303  0.232 0.80262    
    ## Erro a     4 0.005211 0.001303                   
    ## CICLO      3 0.057919 0.019306 58.735 7.7e-05 ***
    ## Erro b     6 0.001972 0.000329                   
    ## PDS*CICLO  6 0.048972 0.008162  3.547 0.02948 *  
    ## Erro c    12 0.027611 0.002301                   
    ## Total     35 0.144564                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.114539 %
    ## CV 2 = 1.062143 %
    ## CV 3 = 2.810166 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                     GL         SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.0000 0.01520000 0.007600 3.704606 0.048713
    ## PDS : CICLO 2   2.0000 0.00006700 0.000033 0.016248   0.9839
    ## PDS : CICLO 3   2.0000 0.02548900 0.012744 6.212257 0.010576
    ## PDS : CICLO 4   2.0000 0.00882200 0.004411 2.150188 0.150294
    ## Erro combinado 15.3202 0.03143704 0.002052                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.69 
    ## ab    cm      1.65 
    ##  b    pp      1.59 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.750000
    ## 2     pd 1.753333
    ## 3     pp 1.756667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.79 
    ## ab    pd      1.716667 
    ##  b    cm      1.66 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.746667
    ## 2     pd 1.670000
    ## 3     pp 1.710000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL         SQ       QM        Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.02630000 0.008767  5.333604 0.012745
    ## CICLO : PDS pd   3.00000 0.01169200 0.003897  2.371054 0.117342
    ## CICLO : PDS pp   3.00000 0.06890000 0.022967 13.972825 0.000224
    ## Erro combinado  13.11154 0.02155538 0.001644                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.75 
    ## ab    4   1.746667 
    ## ab    3   1.66 
    ##  b    1   1.65 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.690000
    ## 2      2 1.753333
    ## 3      3 1.716667
    ## 4      4 1.670000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   1.79 
    ## a     2   1.756667 
    ## a     4   1.71 
    ##  b    1   1.59 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 0_20 l"
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
    ## Bloco      2 0.006572 0.003286 0.3847 0.70337  
    ## PDS        2 0.000772 0.000386 0.0712 0.93239  
    ## Erro a     4 0.021678 0.005419                 
    ## CICLO      3 0.055586 0.018529 3.7383 0.07952 .
    ## Erro b     6 0.029739 0.004956                 
    ## PDS*CICLO  6 0.011139 0.001856 1.0121 0.46164  
    ## Erro c    12 0.022011 0.001834                 
    ## Total     35 0.147497                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.543473 %
    ## CV 2 = 4.345075 %
    ## CV 3 = 2.643266 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.615833
    ## 2     pd 1.626667
    ## 3     pp 1.618333
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.578889
    ## 2      2 1.663333
    ## 3      3 1.583333
    ## 4      4 1.655556
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 20_30 el"
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
    ## Bloco      2 0.028117 0.014058 2.0836 0.23987  
    ## PDS        2 0.040550 0.020275 2.4440 0.20254  
    ## Erro a     4 0.033183 0.008296                 
    ## CICLO      3 0.025275 0.008425 4.2067 0.06369 .
    ## Erro b     6 0.012017 0.002003                 
    ## PDS*CICLO  6 0.045917 0.007653 2.1549 0.12143  
    ## Erro c    12 0.042617 0.003551                 
    ## Total     35 0.227675                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.355108 %
    ## CV 2 = 2.631205 %
    ## CV 3 = 3.503784 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.731667
    ## 2     pd 1.716667
    ## 3     pp 1.654167
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.686667
    ## 2      2 1.724444
    ## 3      3 1.727778
    ## 4      4 1.664444
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 20_30 l"
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
    ## Bloco      2 0.00949 0.004744 1.6360 0.30256  
    ## PDS        2 0.02154 0.010769 1.3374 0.35913  
    ## Erro a     4 0.03221 0.008053                 
    ## CICLO      3 0.05388 0.017959 7.2051 0.02052 *
    ## Erro b     6 0.01496 0.002493                 
    ## PDS*CICLO  6 0.15497 0.025829 3.3783 0.03452 *
    ## Erro c    12 0.09174 0.007645                 
    ## Total     35 0.37879                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.436792 %
    ## CV 2 = 3.024792 %
    ## CV 3 = 5.297477 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL        SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.0082670 0.004133  0.53354 0.596609
    ## PDS : CICLO 2   2.00000 0.1112670 0.055633 7.181274 0.005951
    ## PDS : CICLO 3   2.00000 0.0449560 0.022478 2.901482 0.084123
    ## PDS : CICLO 4   2.00000 0.0120220 0.006011 0.775928 0.476851
    ## Erro combinado 15.99168 0.1238876 0.007747                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.636667
    ## 2     pd 1.683333
    ## 3     pp 1.610000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.803333 
    ## a     pd      1.78 
    ##  b    pp      1.556667 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.626667
    ## 2     pd 1.513333
    ## 3     pp 1.683333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.670000
    ## 2     pd 1.586667
    ## 3     pp 1.656667
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL         SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.05989200 0.019964 3.367917 0.048065
    ## CICLO : PDS pd   3.00000 0.12109200 0.040364 6.809406 0.004388
    ## CICLO : PDS pp   3.00000 0.02786700 0.009289  1.56704 0.240406
    ## Erro combinado  14.41003 0.08542265 0.005928                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.803333 
    ## a     4   1.67 
    ## a     1   1.636667 
    ## a     3   1.626667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.78 
    ## ab    1   1.683333 
    ##  b    4   1.586667 
    ##  b    3   1.513333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.610000
    ## 2      2 1.556667
    ## 3      3 1.683333
    ## 4      4 1.656667
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 30_70 el"
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
    ## Bloco      2 0.014289 0.007144 -3.8735 1.00000  
    ## PDS        2 0.005422 0.002711  0.9969 0.44535  
    ## Erro a     4 0.010878 0.002719                  
    ## CICLO      3 0.046031 0.015344  4.4884 0.05613 .
    ## Erro b     6 0.020511 0.003419                  
    ## PDS*CICLO  6 0.062444 0.010407  1.3038 0.32671  
    ## Erro c    12 0.095789 0.007982                  
    ## Total     35 0.255364                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.176008 %
    ## CV 2 = 3.560906 %
    ## CV 3 = 5.441373 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.627500
    ## 2     pd 1.657500
    ## 3     pp 1.640833
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.630000
    ## 2      2 1.641111
    ## 3      3 1.697778
    ## 4      4 1.598889
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "c - sorgo 30_70 l"
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
    ## Bloco      2 0.025400 0.012700 22.1942 0.006833 **
    ## PDS        2 0.059617 0.029808 12.7750 0.018323 * 
    ## Erro a     4 0.009333 0.002333                    
    ## CICLO      3 0.010700 0.003567  0.8992 0.494427   
    ## Erro b     6 0.023800 0.003967                    
    ## PDS*CICLO  6 0.042517 0.007086  1.2371 0.353733   
    ## Erro c    12 0.068733 0.005728                    
    ## Total     35 0.240100                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.990996 %
    ## CV 2 = 3.899782 %
    ## CV 3 = 4.686197 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.645833 
    ## a     pd      1.641667 
    ##  b    cm      1.5575 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.597778
    ## 2      2 1.633333
    ## 3      3 1.597778
    ## 4      4 1.631111
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 0_20 el"
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
    ## Bloco      2 0.000022 0.000011 0.2222 0.81000  
    ## PDS        2 0.000089 0.000044 4.0000 0.11111  
    ## Erro a     4 0.000044 0.000011                 
    ## CICLO      3 0.000319 0.000106 2.2115 0.18752  
    ## Erro b     6 0.000289 0.000048                 
    ## PDS*CICLO  6 0.000156 0.000026 2.8000 0.06094 .
    ## Erro c    12 0.000111 0.000009                 
    ## Total     35 0.001031                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1822877 %
    ## CV 2 = 0.3794621 %
    ## CV 3 = 0.1664052 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.830833
    ## 2     pd 1.827500
    ## 3     pp 1.827500
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.833333
    ## 2      2 1.826667
    ## 3      3 1.825556
    ## 4      4 1.828889
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 0_20 l"
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
    ## Bloco      2 0.000039 0.000019 0.5385 0.62075  
    ## PDS        2 0.000172 0.000086 7.7500 0.04208 *
    ## Erro a     4 0.000044 0.000011                 
    ## CICLO      3 0.000431 0.000144 3.7805 0.07789 .
    ## Erro b     6 0.000228 0.000038                 
    ## PDS*CICLO  6 0.000161 0.000027 2.0714 0.13332  
    ## Erro c    12 0.000156 0.000013                 
    ## Total     35 0.001231                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1822877 %
    ## CV 2 = 0.3369448 %
    ## CV 3 = 0.1968932 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.831667 
    ## ab    cm      1.8275 
    ##  b    pp      1.826667 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.834444
    ## 2      2 1.825556
    ## 3      3 1.827778
    ## 4      4 1.826667
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 20_30 el"
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
    ## Bloco      2 0.000039 1.9e-05 1.0000  0.4444
    ## PDS        2 0.000106 5.3e-05 2.7143  0.1800
    ## Erro a     4 0.000078 1.9e-05               
    ## CICLO      3 0.000208 6.9e-05 2.2727  0.1803
    ## Erro b     6 0.000183 3.1e-05               
    ## PDS*CICLO  6 0.000317 5.3e-05 1.7273  0.1979
    ## Erro c    12 0.000367 3.1e-05               
    ## Total     35 0.001297                       
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2450148 %
    ## CV 2 = 0.3071423 %
    ## CV 3 = 0.3071423 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.801667
    ## 2     pd 1.797500
    ## 3     pp 1.800000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.803333
    ## 2      2 1.800000
    ## 3      3 1.796667
    ## 4      4 1.798889
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 20_30 l"
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
    ## Bloco      2 0.000139 6.9e-05 5.0000 0.08163 .
    ## PDS        2 0.000172 8.6e-05 7.7500 0.04208 *
    ## Erro a     4 0.000044 1.1e-05                 
    ## CICLO      3 0.000089 3.0e-05 0.9143 0.48821  
    ## Erro b     6 0.000194 3.2e-05                 
    ## PDS*CICLO  6 0.000361 6.0e-05 2.0312 0.13950  
    ## Erro c    12 0.000356 3.0e-05                 
    ## Total     35 0.001356                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1852996 %
    ## CV 2 = 0.3164593 %
    ## CV 3 = 0.3025929 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.800833 
    ## ab    cm      1.8 
    ##  b    pp      1.795833 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.798889
    ## 2      2 1.801111
    ## 3      3 1.798889
    ## 4      4 1.796667
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 30_70 el"
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
    ##           GL       SQ      QM      Fc Pr(>Fc)  
    ## Bloco      2 0.000039 1.9e-05  2.3333 0.21302  
    ## PDS        2 0.000072 3.6e-05 13.0000 0.01778 *
    ## Erro a     4 0.000011 3.0e-06                  
    ## CICLO      3 0.000075 2.5e-05  0.8182 0.52933  
    ## Erro b     6 0.000183 3.1e-05                  
    ## PDS*CICLO  6 0.000417 6.9e-05  2.7778 0.06234 .
    ## Erro c    12 0.000300 2.5e-05                  
    ## Total     35 0.001097                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.09471192 %
    ## CV 2 = 0.3141239 %
    ## CV 3 = 0.2841358 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.761667 
    ##  b    pd      1.759167 
    ##  b    pp      1.758333 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.761111
    ## 2      2 1.761111
    ## 3      3 1.758889
    ## 4      4 1.757778
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - amendoim 30_70 l"
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
    ## Bloco      2 0.000139 0.000069 -5.0000 1.00000  
    ## PDS        2 0.000039 0.000019  1.0000 0.44444  
    ## Erro a     4 0.000078 0.000019                  
    ## CICLO      3 0.000097 0.000032  3.1818 0.10588  
    ## Erro b     6 0.000061 0.000010                  
    ## PDS*CICLO  6 0.000894 0.000149  3.4255 0.03302 *
    ## Erro c    12 0.000522 0.000044                  
    ## Total     35 0.001831                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2507425 %
    ## CV 2 = 0.1814741 %
    ## CV 3 = 0.3751174 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL           SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.0006220000 0.000311 8.241354  0.00394
    ## PDS : CICLO 2   2.00000 0.0001560000 0.000078 2.060338 0.162443
    ## PDS : CICLO 3   2.00000 0.0000890000 0.000044 1.177336 0.335384
    ## PDS : CICLO 4   2.00000 0.0000670000 0.000033 0.883002 0.434256
    ## Erro combinado 14.78424 0.0005618013 0.000038                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.77 
    ## a     cm      1.763333 
    ##  b    pp      1.75 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.763333
    ## 2     pd 1.753333
    ## 3     pp 1.760000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.753333
    ## 2     pd 1.760000
    ## 3     pp 1.760000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.753333
    ## 2     pd 1.756667
    ## 3     pp 1.760000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0003000000 0.000100 3.061224 0.063786
    ## CICLO : PDS pd   3.00000 0.0004670000 0.000156 4.761905 0.017597
    ## CICLO : PDS pp   3.00000 0.0002250000 0.000075 2.295918 0.123211
    ## Erro combinado  13.72946 0.0004530722 0.000033                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.763333
    ## 2      2 1.763333
    ## 3      3 1.753333
    ## 4      4 1.753333
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.77 
    ## ab    3   1.76 
    ## ab    4   1.756667 
    ##  b    2   1.753333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis Medias
    ## 1      1   1.75
    ## 2      2   1.76
    ## 3      3   1.76
    ## 4      4   1.76
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 0_20 el"
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
    ##           GL       SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000022 1.1e-05 -1.0000 1.000000   
    ## PDS        2 0.000022 1.1e-05  1.6000 0.308642   
    ## Erro a     4 0.000028 7.0e-06                    
    ## CICLO      3 0.000111 3.7e-05 10.0000 0.009472 **
    ## Erro b     6 0.000022 4.0e-06                    
    ## PDS*CICLO  6 0.000156 2.6e-05  1.1915 0.373498   
    ## Erro c    12 0.000261 2.2e-05                    
    ## Total     35 0.000622                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1441768 %
    ## CV 2 = 0.1052918 %
    ## CV 3 = 0.2552106 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.828333
    ## 2     pd 1.828333
    ## 3     pp 1.826667
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.83 
    ## ab    3   1.828889 
    ##  bc   4   1.826667 
    ##   c   2   1.825556 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 0_20 l"
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
    ## Bloco      2 0.000022 1.1e-05 0.4000 0.69444  
    ## PDS        2 0.000139 6.9e-05 1.9231 0.25990  
    ## Erro a     4 0.000144 3.6e-05                 
    ## CICLO      3 0.000233 7.8e-05 7.0000 0.02191 *
    ## Erro b     6 0.000067 1.1e-05                 
    ## PDS*CICLO  6 0.000417 6.9e-05 3.5714 0.02883 *
    ## Erro c    12 0.000233 1.9e-05                 
    ## Total     35 0.001256                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.3290737 %
    ## CV 2 = 0.1825373 %
    ## CV 3 = 0.2414741 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL           SQ       QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.0000670000 0.000033 1.433692 0.270211
    ## PDS : CICLO 2   2.00000 0.0002670000 0.000133 5.734767  0.01458
    ## PDS : CICLO 3   2.00000 0.0000670000 0.000033 1.433692 0.270211
    ## PDS : CICLO 4   2.00000 0.0001560000 0.000078 3.345281 0.063797
    ## Erro combinado 14.54224 0.0003344716 0.000023                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.826667
    ## 2     pd 1.830000
    ## 3     pp 1.833333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.83 
    ## ab    pd      1.823333 
    ##  b    pp      1.816667 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.830000
    ## 2     pd 1.826667
    ## 3     pp 1.823333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.823333
    ## 2     pd 1.830000
    ## 3     pp 1.820000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0000920000 0.000031 1.870748 0.175499
    ## CICLO : PDS pd   3.00000 0.0000920000 0.000031 1.870748 0.175499
    ## CICLO : PDS pp   3.00000 0.0004670000 0.000156  9.52381 0.000773
    ## Erro combinado  15.89459 0.0002543134 0.000016                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.826667
    ## 2      2 1.830000
    ## 3      3 1.830000
    ## 4      4 1.823333
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.830000
    ## 2      2 1.823333
    ## 3      3 1.826667
    ## 4      4 1.830000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.833333 
    ##  b    3   1.823333 
    ##  b    4   1.82 
    ##  b    2   1.816667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 20_30 el"
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
    ##           GL       SQ      QM          Fc Pr(>Fc)  
    ## Bloco      2 0.000067 3.3e-05 -1.0931e+15 1.00000  
    ## PDS        2 0.000067 3.3e-05  8.0000e+00 0.04000 *
    ## Erro a     4 0.000017 4.0e-06                      
    ## CICLO      3 0.000189 6.3e-05  3.0000e+00 0.09434 .
    ## Erro b     6 0.000111 1.9e-05                      
    ## PDS*CICLO  6 0.000178 3.0e-05  1.0000e+00 0.32580  
    ## Erro c    12 0.000272 2.3e-05                      
    ## Total     35 0.000900                              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1135074 %
    ## CV 2 = 0.2392946 %
    ## CV 3 = 0.2648506 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.8 
    ## ab    pp      1.798333 
    ##  b    cm      1.796667 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.800000
    ## 2      2 1.798889
    ## 3      3 1.800000
    ## 4      4 1.794444
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 20_30 l"
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
    ##           GL       SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000089 4.4e-05 -8.0000 1.000000   
    ## PDS        2 0.000089 4.4e-05  2.9091 0.165981   
    ## Erro a     4 0.000061 1.5e-05                    
    ## CICLO      3 0.000231 7.7e-05 10.3750 0.008655 **
    ## Erro b     6 0.000044 7.0e-06                    
    ## PDS*CICLO  6 0.000311 5.2e-05  1.8361 0.174388   
    ## Erro c    12 0.000339 2.8e-05                    
    ## Total     35 0.001164                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2175181 %
    ## CV 2 = 0.1514602 %
    ## CV 3 = 0.2957355 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.795833
    ## 2     pd 1.799167
    ## 3     pp 1.795833
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.8 
    ## a     2   1.798889 
    ##  b    3   1.794444 
    ##  b    4   1.794444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 30_70 el"
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
    ## Bloco      2 0.000022 0.000011 0.6667 0.56250  
    ## PDS        2 0.000039 0.000019 7.0000 0.04938 *
    ## Erro a     4 0.000011 0.000003                 
    ## CICLO      3 0.000453 0.000151 5.8214 0.03287 *
    ## Erro b     6 0.000156 0.000026                 
    ## PDS*CICLO  6 0.000072 0.000012 1.0000 0.46822  
    ## Erro c    12 0.000144 0.000012                 
    ## Total     35 0.000897                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.09468203 %
    ## CV 2 = 0.2892584 %
    ## CV 3 = 0.1970964 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.761667 
    ## ab    pd      1.76 
    ##  b    cm      1.759167 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.765556 
    ## ab    2   1.76 
    ## ab    3   1.76 
    ##  b    4   1.755556 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - crotalária 30_70 l"
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
    ## Bloco      2 0.000172 0.000086 31.0000 0.003673 ** 
    ## PDS        2 0.000506 0.000253 10.7059 0.024777 *  
    ## Erro a     4 0.000094 0.000024                     
    ## CICLO      3 0.000319 0.000106  8.8462 0.012729 *  
    ## Erro b     6 0.000072 0.000012                     
    ## PDS*CICLO  6 0.001739 0.000290  8.8169 0.000793 ***
    ## Erro c    12 0.000394 0.000033                     
    ## Total     35 0.003297                              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2769171 %
    ## CV 2 = 0.1977204 %
    ## CV 3 = 0.3267337 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL           SQ       QM        Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.0002890000 0.000144   4.69738 0.025126
    ## PDS : CICLO 2   2.00000 0.0018000000 0.000900 29.268293    5e-06
    ## PDS : CICLO 3   2.00000 0.0000670000 0.000033  1.084011 0.362185
    ## PDS : CICLO 4   2.00000 0.0000890000 0.000044  1.445348  0.26528
    ## Erro combinado 15.74707 0.0004881593 0.000031                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.763333 
    ## ab    pp      1.76 
    ##  b    cm      1.75 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.76 
    ## a     pd      1.76 
    ##  b    pp      1.73 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.760000
    ## 2     pd 1.756667
    ## 3     pp 1.753333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.750000
    ## 2     pd 1.756667
    ## 3     pp 1.756667
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ       QM        Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0003000000 0.000100  3.846154 0.032379
    ## CICLO : PDS pd   3.00000 0.0000920000 0.000031  1.175214 0.352945
    ## CICLO : PDS pp   3.00000 0.0016670000 0.000556 21.367521  1.3e-05
    ## Erro combinado  14.65477 0.0003810241 0.000026                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   1.76 
    ## a     3   1.76 
    ## a     1   1.75 
    ## a     4   1.75 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.763333
    ## 2      2 1.760000
    ## 3      3 1.756667
    ## 4      4 1.756667
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.76 
    ## a     4   1.756667 
    ## a     3   1.753333 
    ##  b    2   1.73 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 0_20 el"
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
    ## Bloco      2 0.000022 1.1e-05 1.0000  0.4444
    ## PDS        2 0.000006 3.0e-06 0.4000  0.6944
    ## Erro a     4 0.000028 7.0e-06               
    ## CICLO      3 0.000164 5.5e-05 2.9500  0.1203
    ## Erro b     6 0.000111 1.9e-05               
    ## PDS*CICLO  6 0.000128 2.1e-05 1.4839  0.2636
    ## Erro c    12 0.000172 1.4e-05               
    ## Total     35 0.000631                       
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1441111 %
    ## CV 2 = 0.2353324 %
    ## CV 3 = 0.2071727 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.828333
    ## 2     pd 1.829167
    ## 3     pp 1.828333
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.831111
    ## 2      2 1.825556
    ## 3      3 1.830000
    ## 4      4 1.827778
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 0_20 l"
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
    ## Bloco      2 0.000039 0.000019 0.6364 0.57551  
    ## PDS        2 0.000089 0.000044 2.9091 0.16598  
    ## Erro a     4 0.000061 0.000015                 
    ## CICLO      3 0.000964 0.000321 8.4634 0.01414 *
    ## Erro b     6 0.000228 0.000038                 
    ## PDS*CICLO  6 0.000311 0.000052 2.2857 0.10508  
    ## Erro c    12 0.000272 0.000023                 
    ## Total     35 0.001964                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2139463 %
    ## CV 2 = 0.3372521 %
    ## CV 3 = 0.2607029 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.825833
    ## 2     pd 1.825833
    ## 3     pp 1.829167
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.834444 
    ## ab    4   1.828889 
    ##  b    2   1.823333 
    ##  b    3   1.821111 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 20_30 el"
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
    ## Bloco      2 0.000039 0.000019 -7.0000 1.000000   
    ## PDS        2 0.000406 0.000203 10.4286 0.025895 * 
    ## Erro a     4 0.000078 0.000019                    
    ## CICLO      3 0.000367 0.000122 14.6667 0.003606 **
    ## Erro b     6 0.000050 0.000008                    
    ## PDS*CICLO  6 0.000417 0.000069  2.2727 0.106584   
    ## Erro c    12 0.000367 0.000031                    
    ## Total     35 0.001722                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2453556 %
    ## CV 2 = 0.1606229 %
    ## CV 3 = 0.3075695 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.8 
    ## a     pp      1.799167 
    ##  b    cm      1.7925 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.801111 
    ## a     3   1.797778 
    ## a     4   1.797778 
    ##  b    2   1.792222 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 20_30 l"
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
    ##           GL       SQ      QM   Fc Pr(>Fc)  
    ## Bloco      2 0.000072 3.6e-05 2.60 0.18904  
    ## PDS        2 0.000106 5.3e-05 4.75 0.08779 .
    ## Erro a     4 0.000044 1.1e-05               
    ## CICLO      3 0.000133 4.4e-05 3.20 0.10485  
    ## Erro b     6 0.000083 1.4e-05               
    ## PDS*CICLO  6 0.000250 4.2e-05 3.75 0.02452 *
    ## Erro c    12 0.000133 1.1e-05               
    ## Total     35 0.000822                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1854141 %
    ## CV 2 = 0.2072993 %
    ## CV 3 = 0.1854141 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                GL       SQ      QM       Fc  valor.p
    ## PDS : CICLO 1   2 0.000022 1.1e-05 1.010101 0.386263
    ## PDS : CICLO 2   2 0.000067 3.3e-05 3.030303 0.076563
    ## PDS : CICLO 3   2 0.000200 1.0e-04 9.090909 0.002305
    ## PDS : CICLO 4   2 0.000067 3.3e-05 3.030303 0.076563
    ## Erro combinado 16 0.000176 1.1e-05                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.800000
    ## 2     pd 1.803333
    ## 3     pp 1.800000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.796667
    ## 2     pd 1.793333
    ## 3     pp 1.800000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.8 
    ## a     pp      1.8 
    ##  b    cm      1.79 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.796667
    ## 2     pd 1.793333
    ## 3     pp 1.800000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ      QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0001580000 5.3e-05 4.398148 0.017397
    ## CICLO : PDS pd   3.00000 0.0002250000 7.5e-05     6.25 0.004311
    ## CICLO : PDS pp   3.00000 0.0000000000 0.0e+00        0        1
    ## Erro combinado  17.89872 0.0002147846 1.2e-05                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.8 
    ## ab    2   1.796667 
    ## ab    4   1.796667 
    ##  b    3   1.79 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.803333 
    ## ab    3   1.8 
    ##  b    2   1.793333 
    ##  b    4   1.793333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis Medias
    ## 1      1    1.8
    ## 2      2    1.8
    ## 3      3    1.8
    ## 4      4    1.8
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 30_70 el"
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
    ##           GL       SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2 0.000006 3.0e-06  1.0000 0.444444   
    ## PDS        2 0.000172 8.6e-05 31.0000 0.003673 **
    ## Erro a     4 0.000011 3.0e-06                    
    ## CICLO      3 0.000231 7.7e-05  7.5455 0.018467 * 
    ## Erro b     6 0.000061 1.0e-05                    
    ## PDS*CICLO  6 0.000161 2.7e-05  2.6364 0.072178 . 
    ## Erro c    12 0.000122 1.0e-05                    
    ## Total     35 0.000764                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.09480171 %
    ## CV 2 = 0.1815314 %
    ## CV 3 = 0.1815314 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.76 
    ## a     pd      1.759167 
    ##  b    cm      1.755 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.761111 
    ## ab    3   1.76 
    ##  b    2   1.755556 
    ##  b    4   1.755556 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - milheto 30_70 l"
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
    ## Bloco      2 0.000000 0.000000 0.0000 1.00000  
    ## PDS        2 0.000117 0.000058 2.8000 0.17361  
    ## Erro a     4 0.000083 0.000021                 
    ## CICLO      3 0.000786 0.000262 7.0750 0.02139 *
    ## Erro b     6 0.000222 0.000037                 
    ## PDS*CICLO  6 0.000372 0.000062 2.5283 0.08088 .
    ## Erro c    12 0.000294 0.000025                 
    ## Total     35 0.001875                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2597072 %
    ## CV 2 = 0.3462763 %
    ## CV 3 = 0.2818485 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.755833
    ## 2     pd 1.756667
    ## 3     pp 1.760000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.765556 
    ##  b    2   1.755556 
    ##  b    3   1.754444 
    ##  b    4   1.754444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 0_20 el"
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
    ## Bloco      2 0.000139 0.000069  2.2727 0.219104    
    ## PDS        2 0.000156 0.000078  2.4348 0.203383    
    ## Erro a     4 0.000128 0.000032                     
    ## CICLO      3 0.000586 0.000195 30.1429 0.000515 ***
    ## Erro b     6 0.000039 0.000006                     
    ## PDS*CICLO  6 0.000156 0.000026  3.2941 0.037392 *  
    ## Erro c    12 0.000094 0.000008                     
    ## Total     35 0.001297                              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.3088024 %
    ## CV 2 = 0.1390978 %
    ## CV 3 = 0.1532783 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL          SQ      QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.000067000 3.3e-05 2.380952 0.141192
    ## PDS : CICLO 2   2.00000 0.000156000 7.8e-05 5.555556 0.023046
    ## PDS : CICLO 3   2.00000 0.000022000 1.1e-05 0.793651 0.477968
    ## PDS : CICLO 4   2.00000 0.000067000 3.3e-05 2.380952 0.141192
    ## Erro combinado 10.31579 0.000144421 1.4e-05                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.840000
    ## 2     pd 1.836667
    ## 3     pp 1.833333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.83 
    ## ab    pd      1.826667 
    ##  b    pp      1.82 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.830000
    ## 2     pd 1.826667
    ## 3     pp 1.830000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.830000
    ## 2     pd 1.833333
    ## 3     pp 1.826667
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ      QM        Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0002250000 7.5e-05 10.227273 0.000469
    ## CICLO : PDS pd   3.00000 0.0002250000 7.5e-05 10.227273 0.000469
    ## CICLO : PDS pp   3.00000 0.0002920000 9.7e-05 13.257576 0.000112
    ## Erro combinado  16.66667 0.0001166667 7.0e-06                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.84 
    ##  b    2   1.83 
    ##  b    3   1.83 
    ##  b    4   1.83 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.836667 
    ## a     4   1.833333 
    ##  b    2   1.826667 
    ##  b    3   1.826667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.833333 
    ## ab    3   1.83 
    ##  b    4   1.826667 
    ##   c   2   1.82 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 0_20 l"
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
    ## Bloco      2 0.000117 0.000058 1.6154 0.30602  
    ## PDS        2 0.000150 0.000075 9.0000 0.03306 *
    ## Erro a     4 0.000033 0.000008                 
    ## CICLO      3 0.000764 0.000255 5.8511 0.03251 *
    ## Erro b     6 0.000261 0.000044                 
    ## PDS*CICLO  6 0.000161 0.000027 1.7059 0.20289  
    ## Erro c    12 0.000189 0.000016                 
    ## Total     35 0.001675                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1578178 %
    ## CV 2 = 0.3606482 %
    ## CV 3 = 0.2168999 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.831667 
    ## ab    cm      1.829167 
    ##  b    pp      1.826667 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.836667 
    ## ab    4   1.828889 
    ## ab    3   1.826667 
    ##  b    2   1.824444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 20_30 el"
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
    ## Bloco      2 0.000072 0.000036  0.6190 0.583140   
    ## PDS        2 0.000572 0.000286 14.7143 0.014318 * 
    ## Erro a     4 0.000078 0.000019                    
    ## CICLO      3 0.000733 0.000244  5.1765 0.042086 * 
    ## Erro b     6 0.000283 0.000047                    
    ## PDS*CICLO  6 0.000383 0.000064  7.6667 0.001488 **
    ## Erro c    12 0.000100 0.000008                    
    ## Total     35 0.002222                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2446749 %
    ## CV 2 = 0.3812983 %
    ## CV 3 = 0.1601773 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL           SQ      QM        Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.0006000000 3.0e-04 27.906977  1.7e-05
    ## PDS : CICLO 2   2.00000 0.0002000000 1.0e-04  9.302326  0.00294
    ## PDS : CICLO 3   2.00000 0.0000000000 0.0e+00         0        1
    ## PDS : CICLO 4   2.00000 0.0001560000 7.8e-05  7.235142 0.007419
    ## Erro combinado 13.37432 0.0001471175 1.1e-05                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.82 
    ##  b    pd      1.81 
    ##   c   pp      1.8 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      1.803333 
    ## a     pd      1.803333 
    ##  b    pp      1.793333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis Medias
    ## 1     cm    1.8
    ## 2     pd    1.8
    ## 3     pp    1.8
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.803333 
    ## ab    cm      1.8 
    ##  b    pp      1.793333 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ       QM        Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0008250000 0.000275 13.095238 0.000415
    ## CICLO : PDS pd   3.00000 0.0001580000 0.000053  2.513228 0.107492
    ## CICLO : PDS pp   3.00000 0.0001330000 0.000044  2.116402 0.151012
    ## Erro combinado  12.11294 0.0002543717 0.000021                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.82 
    ##  b    2   1.803333 
    ##  b    3   1.8 
    ##  b    4   1.8 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.810000
    ## 2      2 1.803333
    ## 3      3 1.800000
    ## 4      4 1.803333
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.800000
    ## 2      2 1.793333
    ## 3      3 1.800000
    ## 4      4 1.793333
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 20_30 l"
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
    ## Bloco      2 0.000217 0.000108 7.8000 0.04165 *
    ## PDS        2 0.000017 0.000008 0.2857 0.76562  
    ## Erro a     4 0.000117 0.000029                 
    ## CICLO      3 0.000586 0.000195 8.4400 0.01423 *
    ## Erro b     6 0.000139 0.000023                 
    ## PDS*CICLO  6 0.000139 0.000023 0.6024 0.72401  
    ## Erro c    12 0.000461 0.000038                 
    ## Total     35 0.001675                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2998955 %
    ## CV 2 = 0.2671681 %
    ## CV 3 = 0.344222 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.801667
    ## 2     pd 1.800833
    ## 3     pp 1.800000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.807778 
    ##  b    2   1.798889 
    ##  b    4   1.798889 
    ##  b    3   1.797778 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 30_70 el"
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
    ##           GL       SQ       QM  Fc  Pr(>Fc)   
    ## Bloco      2 0.000050 0.000025 3.0 0.160000   
    ## PDS        2 0.000117 0.000058 7.0 0.049383 * 
    ## Erro a     4 0.000033 0.000008                
    ## CICLO      3 0.000408 0.000136 9.8 0.009949 **
    ## Erro b     6 0.000083 0.000014                
    ## PDS*CICLO  6 0.000017 0.000003 0.2 0.970335   
    ## Erro c    12 0.000167 0.000014                
    ## Total     35 0.000875                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.1639423 %
    ## CV 2 = 0.2116486 %
    ## CV 3 = 0.2116486 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.7625 
    ## a     pd      1.761667 
    ## a     cm      1.758333 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     1   1.766667 
    ##  b    2   1.758889 
    ##  b    3   1.758889 
    ##  b    4   1.758889 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_c - sorgo 30_70 l"
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
    ## Bloco      2 0.000067 3.3e-05 0.7500 0.52893  
    ## PDS        2 0.000067 3.3e-05 2.0000 0.25000  
    ## Erro a     4 0.000067 1.7e-05                 
    ## CICLO      3 0.000189 6.3e-05 1.5455 0.29698  
    ## Erro b     6 0.000244 4.1e-05                 
    ## PDS*CICLO  6 0.000311 5.2e-05 4.0000 0.01966 *
    ## Erro c    12 0.000156 1.3e-05                 
    ## Total     35 0.001100                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.2321791 %
    ## CV 2 = 0.3630055 %
    ## CV 3 = 0.2047627 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL           SQ      QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 0.0000000000 0.0e+00        0        1
    ## PDS : CICLO 2   2.00000 0.0001560000 7.8e-05 5.555556 0.014928
    ## PDS : CICLO 3   2.00000 0.0001560000 7.8e-05 5.555556 0.014928
    ## PDS : CICLO 4   2.00000 0.0000670000 3.3e-05 2.380952 0.124875
    ## Erro combinado 15.75879 0.0002206231 1.4e-05                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis Medias
    ## 1     cm   1.76
    ## 2     pd   1.76
    ## 3     pp   1.76
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      1.763333 
    ## ab    pp      1.76 
    ##  b    cm      1.753333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      1.76 
    ## ab    pd      1.753333 
    ##  b    cm      1.75 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 1.763333
    ## 2     pd 1.756667
    ## 3     pp 1.760000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL           SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 0.0003330000 0.000111 4.975124 0.012846
    ## CICLO : PDS pd   3.00000 0.0001670000 0.000056 2.487562 0.098313
    ## CICLO : PDS pp   3.00000 0.0000000000 0.000000        0        1
    ## Erro combinado  15.72804 0.0003460168 0.000022                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   1.763333 
    ## ab    1   1.76 
    ## ab    2   1.753333 
    ##  b    3   1.75 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 1.760000
    ## 2      2 1.763333
    ## 3      3 1.753333
    ## 4      4 1.756667
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis Medias
    ## 1      1   1.76
    ## 2      2   1.76
    ## 3      3   1.76
    ## 4      4   1.76
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 0_20 el"
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
    ##           GL     SQ      QM     Fc  Pr(>Fc)    
    ## Bloco      2  13.42   6.710 -5.274 1.000000    
    ## PDS        2   0.69   0.345  0.180 0.841968    
    ## Erro a     4   7.69   1.923                    
    ## CICLO      3 328.68 109.560 35.176 0.000334 ***
    ## Erro b     6  18.69   3.115                    
    ## PDS*CICLO  6  58.85   9.808  1.554 0.242494    
    ## Erro c    12  75.72   6.310                    
    ## Total     35 503.74                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.475922 %
    ## CV 2 = 1.878217 %
    ## CV 3 = 2.673409 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 94.15750
    ## 2     pd 93.84667
    ## 3     pp 93.88417
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   96.53 
    ## a     4   95.38889 
    ## a     3   95.12222 
    ##  b    1   88.81 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 0_20 l"
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
    ##           GL     SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2  71.71  35.857  4.5396 0.093530 . 
    ## PDS        2  47.76  23.880  3.0793 0.155045   
    ## Erro a     4  31.02   7.755                    
    ## CICLO      3 353.43 117.811 21.1010 0.001373 **
    ## Erro b     6  33.50   5.583                    
    ## PDS*CICLO  6  44.73   7.455  1.3705 0.301716   
    ## Erro c    12  65.28   5.440                    
    ## Total     35 647.43                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.170903 %
    ## CV 2 = 2.690499 %
    ## CV 3 = 2.655695 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 88.59167
    ## 2     pd 88.68250
    ## 3     pp 86.19500
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   91.33556 
    ## a     2   90.38 
    ##  b    3   85.78222 
    ##  b    1   83.79444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 20_30 el"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2   1.05  0.527 0.0129 0.98718  
    ## PDS        2   3.22  1.610 0.0374 0.96363  
    ## Erro a     4 172.29 43.071                 
    ## CICLO      3 195.42 65.140 6.4172 0.02659 *
    ## Erro b     6  60.91 10.151                 
    ## PDS*CICLO  6 153.05 25.508 2.0454 0.13729  
    ## Erro c    12 149.65 12.471                 
    ## Total     35 735.58                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 6.875867 %
    ## CV 2 = 3.337981 %
    ## CV 3 = 3.699817 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 95.79500
    ## 2     pd 95.48417
    ## 3     pp 95.06500
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   98.24667 
    ## ab    2   96.95778 
    ## ab    4   94.37111 
    ##  b    1   92.21667 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 20_30 l"
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
    ## Bloco      2   51.71  25.857 0.6932 0.55146  
    ## PDS        2   51.42  25.712 0.6562 0.56696  
    ## Erro a     4  156.75  39.186                 
    ## CICLO      3  713.04 237.679 8.9835 0.01227 *
    ## Erro b     6  158.74  26.457                 
    ## PDS*CICLO  6  130.93  21.822 0.7699 0.60796  
    ## Erro c    12  340.13  28.344                 
    ## Total     35 1602.72                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 6.980939 %
    ## CV 2 = 5.736112 %
    ## CV 3 = 5.937159 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 91.25750
    ## 2     pd 89.38417
    ## 3     pp 88.37250
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   95.48778 
    ## ab    3   90.67667 
    ## ab    4   89.51 
    ##  b    1   83.01111 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 30_70 el"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2  22.97 11.486 1.5376 0.31962  
    ## PDS        2  32.86 16.431 1.1411 0.40542  
    ## Erro a     4  57.60 14.399                 
    ## CICLO      3 269.00 89.665 5.3518 0.03927 *
    ## Erro b     6 100.52 16.754                 
    ## PDS*CICLO  6 195.06 32.510 1.3727 0.30093  
    ## Erro c    12 284.20 23.683                 
    ## Total     35 962.21                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.028563 %
    ## CV 2 = 4.345524 %
    ## CV 3 = 5.166548 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 94.40667
    ## 2     pd 95.24167
    ## 3     pp 92.93083
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   98.79222 
    ## ab    1   93.67111 
    ## ab    4   92.43 
    ##  b    3   91.87889 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - amendoim 30_70 l"
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
    ## Bloco      2   17.31   8.653 0.2657 0.77924  
    ## PDS        2   83.74  41.871 1.4045 0.34510  
    ## Erro a     4  119.25  29.812                 
    ## CICLO      3  313.75 104.584 4.7888 0.04933 *
    ## Erro b     6  131.03  21.839                 
    ## PDS*CICLO  6  300.67  50.111 2.6268 0.07290 .
    ## Erro c    12  228.92  19.077                 
    ## Total     35 1194.68                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.912937 %
    ## CV 2 = 5.060874 %
    ## CV 3 = 4.730015 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 94.49667
    ## 2     pd 91.21083
    ## 3     pp 91.31417
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   94.94222 
    ## a     2   94.77444 
    ## a     3   92.00111 
    ## a     1   87.64444 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 0_20 el"
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
    ##           GL     SQ      QM      Fc  Pr(>Fc)   
    ## Bloco      2   8.29   4.146  0.3590 0.718816   
    ## PDS        2  13.84   6.922  0.9734 0.452444   
    ## Erro a     4  28.44   7.111                    
    ## CICLO      3 373.94 124.648 16.2480 0.002759 **
    ## Erro b     6  46.03   7.672                    
    ## PDS*CICLO  6  40.76   6.794  2.1012 0.128934   
    ## Erro c    12  38.80   3.233                    
    ## Total     35 550.11                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.88619 %
    ## CV 2 = 2.997773 %
    ## CV 3 = 1.946123 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis  Medias
    ## 1     cm 92.8125
    ## 2     pd 91.5175
    ## 3     pp 92.8525
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   95.02222 
    ## a     4   94.88778 
    ## a     2   92.6 
    ##  b    1   87.06667 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 0_20 l"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2  28.91 14.453 1.0461 0.43109  
    ## PDS        2 184.70 92.350 8.0051 0.03996 *
    ## Erro a     4  46.15 11.536                 
    ## CICLO      3 227.60 75.867 5.4441 0.03789 *
    ## Erro b     6  83.61 13.935                 
    ## PDS*CICLO  6  88.61 14.769 1.2670 0.34136  
    ## Erro c    12 139.88 11.656                 
    ## Total     35 799.45                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.839549 %
    ## CV 2 = 4.219934 %
    ## CV 3 = 3.859459 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      91.59 
    ## ab    pd      87.49417 
    ##  b    pp      86.30083 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   92.17 
    ## ab    3   89.10222 
    ## ab    2   87.20111 
    ##  b    1   85.37333 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 20_30 el"
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
    ##           GL      SQ      QM      Fc Pr(>Fc)  
    ## Bloco      2   6.782  3.3909 0.12818 0.88317  
    ## PDS        2  21.590 10.7951 0.78446 0.51591  
    ## Erro a     4  55.045 13.7612                  
    ## CICLO      3  22.961  7.6537 0.46369 0.71803  
    ## Erro b     6  99.036 16.5060                  
    ## PDS*CICLO  6  53.872  8.9787 2.35464 0.09746 .
    ## Erro c    12  45.759  3.8132                  
    ## Total     35 305.045                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.904607 %
    ## CV 2 = 4.276327 %
    ## CV 3 = 2.055395 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 96.09667
    ## 2     pd 94.54500
    ## 3     pp 94.37583
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 93.99556
    ## 2      2 94.52000
    ## 3      3 95.45778
    ## 4      4 96.05000
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 20_30 l"
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
    ## Bloco      2   27.00  13.501 2.1302 0.23449  
    ## PDS        2  229.98 114.991 4.5635 0.09285 .
    ## Erro a     4  100.79  25.198                 
    ## CICLO      3  221.26  73.753 5.0860 0.04364 *
    ## Erro b     6   87.01  14.501                 
    ## PDS*CICLO  6  227.67  37.945 1.1374 0.39828  
    ## Erro c    12  400.33  33.361                 
    ## Total     35 1294.04                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.386652 %
    ## CV 2 = 4.086352 %
    ## CV 3 = 6.198042 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 96.76083
    ## 2     pd 91.28667
    ## 3     pp 91.51917
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   96.85111 
    ## ab    3   94.03333 
    ## ab    2   91.32556 
    ##  b    1   90.54556 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 30_70 el"
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
    ##           GL     SQ     QM      Fc Pr(>Fc)  
    ## Bloco      2 111.33 55.667 13.0422 0.01768 *
    ## PDS        2   7.47  3.737  0.2400 0.79722  
    ## Erro a     4  62.29 15.573                  
    ## CICLO      3  88.77 29.590  2.4160 0.16478  
    ## Erro b     6  73.48 12.247                  
    ## PDS*CICLO  6  80.78 13.463  0.5716 0.74599  
    ## Erro c    12 282.62 23.552                  
    ## Total     35 706.75                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.28099 %
    ## CV 2 = 3.796418 %
    ## CV 3 = 5.264668 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 92.52500
    ## 2     pd 92.48167
    ## 3     pp 91.53750
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 90.16444
    ## 2      2 91.14889
    ## 3      3 93.90556
    ## 4      4 93.50667
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - crotalária 30_70 l"
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
    ##           GL     SQ      QM      Fc Pr(>Fc)  
    ## Bloco      2  17.23  8.6155 0.96466 0.45510  
    ## PDS        2  48.57 24.2848 3.03909 0.15753  
    ## Erro a     4  31.96  7.9908                  
    ## CICLO      3  35.49 11.8311 1.15325 0.40155  
    ## Erro b     6  61.55 10.2589                  
    ## PDS*CICLO  6 136.68 22.7793 2.44450 0.08845 .
    ## Erro c    12 111.82  9.3186                  
    ## Total     35 443.31                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.061069 %
    ## CV 2 = 3.468386 %
    ## CV 3 = 3.305619 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 93.94750
    ## 2     pd 91.22667
    ## 3     pp 91.86667
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 92.61556
    ## 2      2 91.97333
    ## 3      3 91.03444
    ## 4      4 93.76444
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 0_20 el"
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
    ##           GL     SQ     QM     Fc  Pr(>Fc)   
    ## Bloco      2  83.04 41.519 37.859 0.002518 **
    ## PDS        2  63.83 31.914 22.196 0.006832 **
    ## Erro a     4   5.75  1.438                   
    ## CICLO      3 197.73 65.909 12.317 0.005641 **
    ## Erro b     6  32.11  5.351                   
    ## PDS*CICLO  6 124.55 20.758  3.647 0.026913 * 
    ## Erro c    12  68.30  5.692                   
    ## Total     35 575.30                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.29273 %
    ## CV 2 = 2.493851 %
    ## CV 3 = 2.572118 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL        SQ        QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 21.080822 10.540411 2.277286 0.139712
    ## PDS : CICLO 2   2.00000  3.081867  1.540933 0.332923 0.722421
    ## PDS : CICLO 3   2.00000 94.010422 47.005211 10.15561 0.001938
    ## PDS : CICLO 4   2.00000 70.202600 35.101300 7.583736  0.00599
    ## Erro combinado 13.81211 63.929300  4.628497                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 87.05667
    ## 2     pd 90.75000
    ## 3     pp 88.34667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 93.45667
    ## 2     pd 94.66333
    ## 3     pp 93.39000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      98.41333 
    ##  b    cm      92.79667 
    ##  b    pd      90.77333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      98.35333 
    ##  b    cm      93.19333 
    ##  b    pd      91.88333 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL        SQ        QM        Fc  valor.p
    ## CICLO : PDS cm   3.00000  84.17142 28.057142  5.029652 0.010997
    ## CICLO : PDS pd   3.00000  30.51882 10.172942  1.823648 0.180434
    ## CICLO : PDS pp   3.00000 207.58389 69.194631 12.404147 0.000143
    ## Erro combinado  17.30118  96.51194  5.578346                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   93.45667 
    ## a     4   93.19333 
    ## a     3   92.79667 
    ##  b    1   87.05667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 90.75000
    ## 2      2 94.66333
    ## 3      3 90.77333
    ## 4      4 91.88333
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   98.41333 
    ## a     4   98.35333 
    ## ab    2   93.39 
    ##  b    1   88.34667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 0_20 l"
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
    ##           GL     SQ     QM      Fc Pr(>Fc)    
    ## Bloco      2  34.31 17.153   1.714 0.28991    
    ## PDS        2  21.31 10.653   0.571 0.60519    
    ## Erro a     4  74.64 18.660                    
    ## CICLO      3 240.38 80.127 119.551   1e-05 ***
    ## Erro b     6   4.02  0.670                    
    ## PDS*CICLO  6 177.48 29.580   3.172 0.04206 *  
    ## Erro c    12 111.90  9.325                    
    ## Total     35 664.03                           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.810656 %
    ## CV 2 = 0.91173 %
    ## CV 3 = 3.400767 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL         SQ        QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000  32.343267 16.171633 1.387096 0.281498
    ## PDS : CICLO 2   2.00000  91.792867 45.896433 3.936694 0.043477
    ## PDS : CICLO 3   2.00000  68.338467 34.169233 2.930812   0.0858
    ## PDS : CICLO 4   2.00000   6.308956  3.154478  0.27057 0.766768
    ## Erro combinado 14.28318 166.522190 11.658624                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 88.33333
    ## 2     pd 84.71000
    ## 3     pp 84.00667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      93.62 
    ## ab    cm      92.18667 
    ##  b    pd      86.24333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 86.13000
    ## 2     pd 92.16667
    ## 3     pp 91.76333
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 93.85000
    ## 2     pd 91.80333
    ## 3     pp 92.71333
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL        SQ        QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 111.88857 37.296189 5.791284 0.010212
    ## CICLO : PDS pd   3.00000 130.79989 43.599964 6.770122 0.005816
    ## CICLO : PDS pp   3.00000 175.16989 58.389964 9.066686 0.001834
    ## Erro combinado  12.56746  80.93515  6.440056                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     4   93.85 
    ## ab    2   92.18667 
    ## ab    1   88.33333 
    ##  b    3   86.13 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   92.16667 
    ## a     4   91.80333 
    ## ab    2   86.24333 
    ##  b    1   84.71 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   93.62 
    ## a     4   92.71333 
    ## a     3   91.76333 
    ##  b    1   84.00667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 20_30 el"
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
    ##           GL     SQ     QM     Fc  Pr(>Fc)   
    ## Bloco      2 132.23 66.117 1.8033 0.276525   
    ## PDS        2  54.89 27.444 1.0581 0.427711   
    ## Erro a     4 103.75 25.936                   
    ## CICLO      3  92.81 30.936 1.6886 0.267597   
    ## Erro b     6 109.92 18.320                   
    ## PDS*CICLO  6 289.93 48.322 6.3643 0.003312 **
    ## Erro c    12  91.11  7.593                   
    ## Total     35 874.64                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.451428 %
    ## CV 2 = 4.581673 %
    ## CV 3 = 2.949531 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL        SQ        QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000  87.27016 43.635078 3.582942 0.062602
    ## PDS : CICLO 2   2.00000 140.94562 70.472811 5.786629 0.018748
    ## PDS : CICLO 3   2.00000  99.06016 49.530078 4.066989  0.04694
    ## PDS : CICLO 4   2.00000  17.54309  8.771544 0.720245 0.507814
    ## Erro combinado 11.22505 136.70492 12.178561                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 88.63667
    ## 2     pd 95.47667
    ## 3     pp 94.98000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pd      96.08333 
    ## ab    cm      90.59 
    ##  b    pp      86.42 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      98.47333 
    ## ab    cm      93.97 
    ##  b    pd      90.36333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 93.51667
    ## 2     pd 95.64000
    ## 3     pp 96.90000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL        SQ       QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000  57.20223 19.06741 1.707238 0.203256
    ## CICLO : PDS pd   3.00000  65.47429 21.82476 1.954123 0.159167
    ## CICLO : PDS pp   3.00000 260.06400 86.68800 7.761778 0.001761
    ## Erro combinado  17.03111 190.21327 11.16858                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 88.63667
    ## 2      2 90.59000
    ## 3      3 93.97000
    ## 4      4 93.51667
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 95.47667
    ## 2      2 96.08333
    ## 3      3 90.36333
    ## 4      4 95.64000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   98.47333 
    ## a     4   96.9 
    ## a     1   94.98 
    ##  b    2   86.42 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 20_30 l"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)
    ## Bloco      2  66.36 33.181 0.9784  0.4509
    ## PDS        2 154.77 77.385 1.9136  0.2612
    ## Erro a     4 161.76 40.441               
    ## CICLO      3  83.97 27.992 2.3788  0.1686
    ## Erro b     6  70.60 11.767               
    ## PDS*CICLO  6 224.51 37.418 2.0453  0.1373
    ## Erro c    12 219.53 18.294               
    ## Total     35 981.51                      
    ## ------------------------------------------------------------------------
    ## CV 1 = 6.886197 %
    ## CV 2 = 3.714509 %
    ## CV 3 = 4.631566 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 91.23583
    ## 2     pd 90.55500
    ## 3     pp 95.25417
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 90.93000
    ## 2      2 90.83444
    ## 3      3 93.21111
    ## 4      4 94.41778
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 30_70 el"
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
    ##           GL     SQ     QM     Fc  Pr(>Fc)   
    ## Bloco      2  18.26  9.130  0.247 0.791972   
    ## PDS        2 145.92 72.962 50.550 0.001448 **
    ## Erro a     4   5.77  1.443                   
    ## CICLO      3 106.91 35.636  0.643 0.614820   
    ## Erro b     6 332.39 55.399                   
    ## PDS*CICLO  6 143.55 23.925  1.200 0.369680   
    ## Erro c    12 239.23 19.936                   
    ## Total     35 992.04                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.32586 %
    ## CV 2 = 8.214164 %
    ## CV 3 = 4.927513 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      93.405 
    ##  b    cm      89.6975 
    ##  b    pd      88.735 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 90.55222
    ## 2      2 92.56889
    ## 3      3 87.89667
    ## 4      4 91.43222
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - milheto 30_70 l"
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
    ##           GL     SQ      QM      Fc Pr(>Fc)
    ## Bloco      2  37.46 18.7318 -2.1592  1.0000
    ## PDS        2   6.28  3.1401  4.0040  0.1110
    ## Erro a     4   3.14  0.7842                
    ## CICLO      3  21.38  7.1263  0.8853  0.5002
    ## Erro b     6  48.30  8.0499                
    ## PDS*CICLO  6  61.24 10.2062  0.5829  0.7379
    ## Erro c    12 210.11 17.5093                
    ## Total     35 387.91                        
    ## ------------------------------------------------------------------------
    ## CV 1 = 0.9563779 %
    ## CV 2 = 3.064089 %
    ## CV 3 = 4.518986 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 92.03833
    ## 2     pd 92.70667
    ## 3     pp 93.04333
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 93.12333
    ## 2      2 92.60111
    ## 3      3 93.31889
    ## 4      4 91.34111
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 0_20 el"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)    
    ## Bloco      2   6.02  3.008 -1.358 1.00000    
    ## PDS        2   3.09  1.547  0.486 0.64732    
    ## Erro a     4  12.74  3.185                   
    ## CICLO      3 199.34 66.446 69.312 4.8e-05 ***
    ## Erro b     6   5.75  0.959                   
    ## PDS*CICLO  6 140.23 23.372  3.676 0.02622 *  
    ## Erro c    12  76.30  6.359                   
    ## Total     35 443.47                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 1.913564 %
    ## CV 2 = 1.049882 %
    ## CV 3 = 2.703929 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                      GL        SQ        QM       Fc  valor.p
    ## PDS : CICLO 1   2.00000 40.304289 20.152144 3.621127 0.051959
    ## PDS : CICLO 2   2.00000  1.401867  0.700933  0.12595 0.882576
    ## PDS : CICLO 3   2.00000 73.217756 36.608878 6.578228 0.008831
    ## PDS : CICLO 4   2.00000 28.401089 14.200544 2.551688 0.111043
    ## Erro combinado 15.08024 83.923941  5.565158                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 89.89667
    ## 2     pd 91.91000
    ## 3     pp 86.76667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 95.49000
    ## 2     pd 95.98333
    ## 3     pp 96.45667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      97.75 
    ## ab    pd      93.94 
    ##  b    cm      90.77333 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 95.47333
    ## 2     pd 91.13000
    ## 3     pp 93.53000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL        SQ        QM        Fc  valor.p
    ## CICLO : PDS cm   3.00000  80.61777 26.872589   5.89485 0.008907
    ## CICLO : PDS pd   3.00000  42.71063 14.236875  3.123043 0.062232
    ## CICLO : PDS pp   3.00000 216.23916 72.079719 15.811619 0.000119
    ## Erro combinado  13.16988  60.03693  4.558655                   
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   95.49 
    ## a     4   95.47333 
    ## ab    3   90.77333 
    ##  b    1   89.89667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 91.91000
    ## 2      2 95.98333
    ## 3      3 93.94000
    ## 4      4 91.13000
    ## ------------------------------------------------------------------------
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   97.75 
    ## a     2   96.45667 
    ## a     4   93.53 
    ##  b    1   86.76667 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 0_20 l"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2  16.15  8.076 0.3475 0.72583  
    ## PDS        2   2.26  1.132 0.0768 0.92742  
    ## Erro a     4  58.98 14.746                 
    ## CICLO      3 185.94 61.981 4.4808 0.05632 .
    ## Erro b     6  83.00 13.833                 
    ## PDS*CICLO  6  32.16  5.360 1.0034 0.46635  
    ## Erro c    12  64.10  5.342                 
    ## Total     35 442.60                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 4.333013 %
    ## CV 2 = 4.196661 %
    ## CV 3 = 2.60794 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 88.37750
    ## 2     pd 88.96750
    ## 3     pp 88.52417
    ## ------------------------------------------------------------------------
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 86.01444
    ## 2      2 91.13778
    ## 3      3 86.73000
    ## 4      4 90.61000
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 20_30 el"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)  
    ## Bloco      2  80.40 40.200 2.0113  0.2486  
    ## PDS        2 106.87 53.433 2.1615  0.2310  
    ## Erro a     4  98.88 24.720                 
    ## CICLO      3  87.20 29.066 4.7664  0.0498 *
    ## Erro b     6  36.59  6.098                 
    ## PDS*CICLO  6 134.22 22.370 2.0653  0.1342  
    ## Erro c    12 129.97 10.831                 
    ## Total     35 674.12                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.266793 %
    ## CV 2 = 2.615847 %
    ## CV 3 = 3.486212 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 95.99917
    ## 2     pd 95.19750
    ## 3     pp 92.01000
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   96.02444 
    ## a     2   95.86111 
    ## a     1   93.16333 
    ## a     4   92.56 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 20_30 l"
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
    ## Bloco      2   26.51 13.256 1.8743 0.26649  
    ## PDS        2   68.28 34.141 1.4438 0.33728  
    ## Erro a     4   94.59 23.647                 
    ## CICLO      3  169.46 56.487 7.8354 0.01693 *
    ## Erro b     6   43.26  7.209                 
    ## PDS*CICLO  6  470.93 78.488 3.3000 0.03718 *
    ## Erro c    12  285.41 23.784                 
    ## Total     35 1158.44                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 5.306198 %
    ## CV 2 = 2.929788 %
    ## CV 3 = 5.321502 %
    ## 
    ## 
    ## 
    ## Interacao significativa: desdobrando a interacao
    ## ------------------------------------------------------------------------
    ## 
    ## Desdobrando  PDS  dentro de cada nivel de  CICLO 
    ## ------------------------------------------------------------------------
    ##                     GL        SQ        QM       Fc  valor.p
    ## PDS : CICLO 1   2.0000  25.06136  12.53068 0.527609 0.599931
    ## PDS : CICLO 2   2.0000 339.16696 169.58348 7.140384 0.006076
    ## PDS : CICLO 3   2.0000 137.96642  68.98321 2.904567 0.083918
    ## PDS : CICLO 4   2.0000  37.01562  18.50781 0.779279 0.475389
    ## Erro combinado 15.9999 379.99622  23.74991                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  PDS dentro de CICLO 1
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 90.74333
    ## 2     pd 93.24667
    ## 3     pp 89.19667
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 2
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     cm      100.26 
    ## a     pd      98.77 
    ##  b    pp      86.55667 
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 3
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 90.53333
    ## 2     pd 84.01000
    ## 3     pp 93.36000
    ## ------------------------------------------------------------------------
    ## 
    ##  PDS dentro de CICLO 4
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 92.66000
    ## 2     pd 88.16333
    ## 3     pp 92.24000
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## Desdobrando  CICLO  dentro de cada nivel de  PDS 
    ## ------------------------------------------------------------------------
    ##                       GL        SQ        QM       Fc  valor.p
    ## CICLO : PDS cm   3.00000 188.38156  62.79385  3.43904 0.045716
    ## CICLO : PDS pd   3.00000 366.95449 122.31816 6.699016 0.004782
    ## CICLO : PDS pp   3.00000  85.05363  28.35121 1.552715  0.24422
    ## Erro combinado  14.25633 260.30806  18.25912                  
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS cm
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   100.26 
    ## a     4   92.66 
    ## a     1   90.74333 
    ## a     3   90.53333 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pd
    ## ------------------------------------------------------------------------
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     2   98.77 
    ## ab    1   93.24667 
    ##  b    4   88.16333 
    ##  b    3   84.01 
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ##  CICLO dentro de PDS pp
    ## ------------------------------------------------------------------------
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 89.19667
    ## 2      2 86.55667
    ## 3      3 93.36000
    ## 4      4 92.24000
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 30_70 el"
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
    ##           GL     SQ     QM      Fc Pr(>Fc)  
    ## Bloco      2  45.42 22.711 -3.3972 1.00000  
    ## PDS        2  14.12  7.061  0.8758 0.48365  
    ## Erro a     4  32.25  8.062                  
    ## CICLO      3 158.59 52.862  5.1848 0.04195 *
    ## Erro b     6  61.17 10.196                  
    ## PDS*CICLO  6 193.26 32.211  1.2914 0.33158  
    ## Erro c    12 299.31 24.943                  
    ## Total     35 804.13                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 3.044652 %
    ## CV 2 = 3.42392 %
    ## CV 3 = 5.3554 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1     cm 92.57667
    ## 2     pd 94.08833
    ## 3     pp 93.10583
    ## ------------------------------------------------------------------------
    ## CICLO
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     3   96.58444 
    ## ab    2   93.31556 
    ## ab    1   92.20111 
    ##  b    4   90.92667 
    ## ------------------------------------------------------------------------
    ## 
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n - sorgo 30_70 l"
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
    ##           GL     SQ     QM     Fc Pr(>Fc)   
    ## Bloco      2  75.61 37.807 31.807 0.00350 **
    ## PDS        2 176.98 88.489 12.680 0.01856 * 
    ## Erro a     4  27.91  6.978                  
    ## CICLO      3  33.66 11.220  0.869 0.50726   
    ## Erro b     6  77.50 12.917                  
    ## PDS*CICLO  6 126.08 21.014  1.123 0.40497   
    ## Erro c    12 224.48 18.706                  
    ## Total     35 742.23                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## ------------------------------------------------------------------------
    ## CV 1 = 2.878547 %
    ## CV 2 = 3.916237 %
    ## CV 3 = 4.712919 %
    ## 
    ## Interacao nao significativa: analisando os efeitos simples
    ## ------------------------------------------------------------------------
    ## PDS
    ## Teste de Tukey
    ## ------------------------------------------------------------------------
    ## Grupos Tratamentos Medias
    ## a     pp      93.4125 
    ## a     pd      93.26417 
    ##  b    cm      88.63667 
    ## ------------------------------------------------------------------------
    ## 
    ## CICLO
    ## De acordo com o teste F, as medias desse fator sao estatisticamente iguais.
    ## ------------------------------------------------------------------------
    ##   Niveis   Medias
    ## 1      1 90.59333
    ## 2      2 92.81889
    ## 3      3 91.04667
    ## 4      4 92.62556
    ## ------------------------------------------------------------------------
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - amendoim 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - crotalária 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - milheto 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "n_g_kg - sorgo 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - amendoim 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - crotalária 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - milheto 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 0_20 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 0_20 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 20_30 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 20_30 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 30_70 el"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
    ## 
    ## [1] "-----------------------------------------"
    ## [1] "est_n - sorgo 30_70 l"
    ## [1] "-----------------------------------------"
    ## [1] "------------------ Análise em Faixas -------------"
    ## [1] "dados sem variação"
