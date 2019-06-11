#####################################################################################
#####################################################################################
#####################################################################################
#############                                                           #############
#############           UNIVERSIDADE FEDERAL DE PERNAMBUCO              ############# 
############# PROGRAMA DE POS-GRADUCAO EM CIENCIA POLITICA - MESTRADO   ############# 
#############                 RECIFE, 11 DE JUNHO DE 2017               ############# 
#############          DISCENTE: DAVID VICTOR DE MELO CHAVES            ############# 
#############                 DOCENTE: DAVI MOREIRA                     ############# 
#############                                                           ############# 
#####################################################################################
#####################################################################################
#####################################################################################


#### Questão 1 ####

#a) You are confident that the correlation between Zi and Xi is equal to zero.

#b) You think that the correlation between Zi and Xi is positive.

#c) You think that the correlation between Zi and Xi is negative.

#### Questão 2 ####
  
# A porcentagem de residentes do estado com formacao superior, da coluna A, e variavel indenpendente. tendo estimativa do parametreo de  704.02, estatitiscamente significativo e erro padrao de 140.22. 
# A interceptcao de y para a regressao e de 28768.01, estatitiscamente significante.
# Existe 34% de variação na DP
# Ja na coluna b a DI e renda per capita, a estimativa de parametreo e de 0.68, estatisticamente significante com erro padrão de 0.11; 
# A interceptacao y estimada e de 21168.11, estatisticamente significante. B apresenta  47%  de variacao na VD

#### Questão 3 ####

# Na coluna c a VI é renda per capita, controlada pelos efeitos da variavel porcentagem de residentes do estado com formacao superior os resultados foram semlhantes ao obtido no modelo bitivariavel. O valor estimado passa de 0.68 para 0.66, ainda significativo, porém o erro padrão aumente de 0.11 para 0.16. entretanto tendo em vista a variável residentes do estado com formação superior demostra uma mudança significativa. Contralado pela variável renda per capta a estimativa do parâmetro caiu para 24.56, com erro padrão de 231.72, não significativo. Já o modelo c detem 47% de variação da VD.  O Poder de explicação da variavel renda per capta é demostrada por essa mudança do modelo bivariado b

### 4.1.a ###
  
  ```{r}
wordrecall <- read.table("C:/Users/DELL/Documents/2019.1/Lista 8/wordrecall.txt",
                         header = TRUE)


reg.1 <- lm(prop ~ time, data = wordrecall)

confint(reg.1)

summary(reg.1)

mse1 <- mean(residuals(reg.1)^2)  #0.01
rmse1 <- sqrt(mse1) #0.14

plot(reg.1)

```

#O R2 explica 57% da variação da VD, o erro rmse é baixa igual 0,14,  entretanto os graficos sugerem uma relação não linear entre as variaveis


library(ggplot2)

ggplot(wordrecall, aes(x = time, y = prop)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")


# Os graficos surgerem que não existe linearidade entre as variveis. sendo necessário avaliar este problema antes do teste da hipótese de que a variação é igual. Usa-se o log natural para transformar os valores de x. como x é o preditor basta apagar o log  de cada valor no conjuto de dados. Ao fazer isto, cria-se um preditor chamado Intine


x <- wordrecall$time
log(x, base  = exp(1))

Intime <- log(x, base  = exp(1))

wordrecall$Intime <- Intime

head(wordrecall)


library(ggplot2)

ggplot(wordrecall, aes(x = Intime, y = prop)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

reg.4log <- lm(prop ~ Intime, data = wordrecall)

summary(reg.4log)


# O uso do log natural  e valido pois o r2 aumenta de 57% para 99%

  
plot(reg.4log)


#o gráfico residuals vs fitted sugere um aumento significativo em relação ao baseado nos dados não transformados.  Ja o gráfico normal  q-q  sugere que a tranformacao dos valores de x não tiveram efeito sobre a normalidade dos termos de erro.


### Questão 4.1.b ###

shortleaf <- read.table("C:/Users/DELL/Documents/2019.1/Lista 8/shortleaf.txt",
                        header = TRUE)

reg.shortleaf <- lm(Vol ~ Diam, data = shortleaf)

summary(reg.shortleaf)

confint(reg.shortleaf)

mse2 <- mean(residuals(reg.shortleaf)^2) # 94.78 
rmse2 <- sqrt(mse2) # 9.73
```

O r2 é um tanto alto 89.3%, ou seja, a capacidade da variável independente explicar a variável independente é um tanto alta. Alta também é a taxa de erro rmse, 9.73%. No entanto, os gráficos indicam que o relacionamente das variáveis não é linear

```{r}
library(ggplot2)

ggplot(shortleaf, aes(x = Diam, y = Vol)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#Os gráficos residuals vs fitted e normal Q-Q também sugestem a falta de linearidade: 

plot(reg.shortleaf)


#A falta de linearidade domina o gráfico residuals vs fitted, então, não podemos usá-lo para avaliar se as variâncias de erro são iguais. Temos que corrigir o problema da não linearidade antes de podermos avaliar a hipótese de variações iguais. O Q-Q normal sugere que os termos de erro não são normais, ou seja, linearmente distribuídos. Em resumo, parece que a relação entre o diâmetro e o volume da árvore não é linear. Além disso, parece que os termos de erro não são normalmente distribuídos.

#*Tentando transformar apenas os valores de X

#Então, vamos usar o logaritmo natural dos diâmetros das árvores para obter o novo preditor x = lnDiam


y <- shortleaf$Diam
log(y, base  = exp(1))

InDiam <- log(y, base  = exp(1))

shortleaf$InDiam <- InDiam

head(shortleaf)

reg.4log.b <- lm(Vol ~ InDiam, data = shortleaf)

summary(reg.4log.b)

library(ggplot2)

ggplot(shortleaf, aes(x = InDiam, y = Vol)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")


#O gráfico de linhas ajustadas com y = volume como a resposta e x = lnDiam como o preditor sugere que a relação ainda não é linear. 

```{r}
plot(reg.4log.b)
```


#Transformar apenas os valores de x não alterou a não linearidade. O gráfico de residuals versus fitted também ainda sugere uma relação não linear e há pouca melhoria na normalidade dos termos de erro no gráfico normal Q-Q.

#*Tentando transformar os valores de y também

Então, transformar apenas x não ajudou muito. Vamos também tentar transformar os valores de resposta (y). Em particular, vamos pegar o logaritmo natural dos volumes da árvore para obter a nova resposta y = lnVol:
  
  ```{r}
z <- shortleaf$Vol
log(z, base  = exp(1))

InVol <- log(z, base  = exp(1))

shortleaf$InVol <- InVol

head(shortleaf)

reg.4log.z <- lm(InVol ~ InDiam, data = shortleaf)

summary(reg.4log.z)

```
```

A relação entre o log natural do diâmetro e o log natural do volume parece linear e forte, com o r2 de 97.3%, ou seja, a capacidade da variável independente sobre a dependente.O gráfico reforça a visão da linearidade: 
  
  ```{r}
library(ggplot2)

ggplot(shortleaf, aes(x = InDiam, y = InVol)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
```

O gráfico residuals vs fitted fornece ainda mais evidências de uma relação linear entre lnVol e lnDiam; o normal q-q também melhorou substancialmente

```{r}
plot(reg.4log.z)
```

Em resumo, parece que o modelo com o log natural do volume da árvore como a resposta e o log natural do diâmetro da árvore como o preditor funciona bem. O relacionamento parece ser linear e os termos de erro aparecem como independentes e normalmente distribuídos com variações iguais.

**Questão 4.1.c**
  
  ```{r}
mammgest <- read.table("C:/Users/DELL/Documents/2019.1/Lista 8/mammgest.txt",
                       header = TRUE)

reg.mammgest <- lm(Gestation ~ Birthwgt, data = mammgest)

summary(reg.mammgest)

confint(reg.mammgest)

mse3 <- mean(residuals(reg.mammgest)^2) #3574.18
rmse3 <- sqrt(mse3) #59.78
```

O r2 explica cerca de 83% da variável dependente, ou seja, do quanto a variável dependente consegue ser explicada pela indipendente. A taxa de erro rmse é um tanto alta 59.78%. O gráfico de linha ajustado sugere que a relação entre comprimento de gestação (y) e peso ao nascer (x) é linear, mas que a variância dos termos de erro pode não ser igual:
  
  ```{r}
library(ggplot2)

ggplot(mammgest, aes(x = Gestation, y = Birthwgt)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
```

O gráfico residuals vs fitted fornece ainda mais evidências de que a variância dos termos de erro pode não ser igual; O normal q-q reforça a suposição de termos de erro normalmente distribuídos:
  
  
  ```{r}
plot(reg.mammgest)
```

* Transformando os valores de y usando o logorítimo natural do tamanho da gestação

Ao fazer isso, obtemos a nova resposta y = lnGest:
  
  
  
  ```{r}
y <- mammgest$Gestation
log(y, base  = exp(1))

InGest <- log(y, base  = exp(1))

mammgest$Gestation <- InGest

head(mammgest)

reg.4log.mammgest <- lm(Birthwgt ~ InGest, data = mammgest)

summary(reg.4log.mammgest)

library(ggplot2)

ggplot(mammgest, aes(x = Birthwgt, y = InGest)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
```



O r2 dimuniu apenas um pouco para 80.33%. O gráfico de linha ajustado com y = lnGest como a resposta e x = Birthwgt como o preditor sugere que a transformação de log da resposta ajudou. Como esperado, a transformação logarítmica tendeu a "espalhar-se" pelas gestações menores e tendeu a "trazer" as maiores. O novo residuals vs fitted mostra uma melhoria acentuada na disseminação dos resíduos; quanto ao gráfico normal q-q, a  transformação de log da resposta não afetou negativamente a normalidade dos termos de erro 

```{r}
plot(reg.4log.mammgest)
```

Em suma, o valor r2 é menor para o modelo transformado do que para o modelo não transformado (80,3% versus 83,9%). Isso não significa que o modelo não transformado seja preferível. Uma vez que o modelo não transformado não satisfez a condição de variância igual, portanto, não devemos usar esse modelo de qualquer maneira.


**Questão 4.2**
  
  a) Diferente dos modelos polinomiais, nos modelos lineares não forja-se ou acrescenta-se outras variáveis (polinomiais), por isso não é possível usar o arcabouço conhecido dos modelos lineares para desenvolver um modelo polinomial.

b) 

```{r}
bluegills <- read.table("C:/Users/DELL/Documents/2019.1/Lista 8/bluegills.txt",
                        header = TRUE)



reg.bluegills <- lm(length ~ age, data = bluegills)

confint(reg.bluegills)

summary(reg.bluegills)

mse4 <- mean(residuals(reg.bluegills)^2)  #152.47
rmse4 <- sqrt(mse4) #12.34

```


No modelo linar, o r2, ou seja, a capacidade de explicação da variável dependente a partir da variável independente, ficou no valor de 73%, o que é um valor razoável. A taxa de erro rmse é um pouco alta, 12.34%. Os gráficos indicam um padrão de comportamento, no entanto, uma falta de linearidade dos dados: 
  
  ```{r}
library(ggplot2)

ggplot(bluegills, aes(x = age, y = length)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

plot(reg.bluegills)
```


* Adicionando um termo quadrático 

```{r}

h <- bluegills$age
quadratico <- h^2

bluegills$quadratico <- quadratico

head(bluegills)


reg.bluegills.pol <- lm(bluegills$length ~ bluegills$age + I(bluegills$quadratico))

summary(reg.bluegills.pol)


mse6 <- mean(residuals(reg.bluegills.pol)^2) #114.36
rmse6 <- sqrt(mse6) #10.69
```

O modelo polinominal é essencialmente um modelo linear em duas variáveis, uma das quais é o quadrado da outra. Nós vemos que os resultados se mantiveram semelhantes ao do modelo linear, mas um modelo quadrático funciona ainda melhor, explicando um adicional de quase 10% da variância. No modelo linear o r2 é 73%, no polinominal 80%. O erro padrão rmse, por sua vez, diminuiu de 12.34% para 10.69%

```{r}

plot(reg.bluegills.pol)
```

Os gráficos demonstram que acrescentar o termo quadrático deixaram os dados muito mais lineares. 

