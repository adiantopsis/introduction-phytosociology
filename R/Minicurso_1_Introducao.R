# Comandos básicos --------------------------------------------------------
getwd() # Mostrar o diretório de trabalho atual

setwd(dir = "~/Documentos") # Mudar o diretório de trabalho com um caminho 
## Ou apenas pressione Ctrl+Shift+H e selecione a pasta de trabalho

dir(all.files = T) # Listar os arquivos do diretório

#Considere os seguintes valores de abundancia de duas especies ao longo de 10 UA 
parc <- c('UA 1',"UA 2", "UA 3", "UA 4", "UA 5", "UA 6", 
          "UA 7", "UA 8", "UA 9", "UA 10")
parc
?c


class(parc)

str(parc)

parc[6]
parc

parc_n <- 1:100
parc_n

str(parc_n)

?str # ou selecione a função e aperte F1 
str(parc_n, give.length=F, give.head=F)


sp1 <- c(1,3,3,5,5,5,7,8,9,10)
sp2 <- c(0,0,0,0,2,5,4,8,10,9)

class(sp1)
str(sp1)
str(sp2)

#Elaborando matrix
dados_mat <- cbind (sp1, sp2)
dados_mat
class(dados_mat)
row.names(dados_mat)

row.names(dados_mat) <- parc

dados_mat

colnames(dados_mat) <- c("Myrsine sp.1", "Eugenia uniflora")

dados_mat

colnames(dados_mat) <- c("sp1", "sp2")
dados_mat


class(dados_mat)
str(dados_mat)
dim(dados_mat) # numero de linhas e colunas


#Elaborando data frame
dados_df <- cbind.data.frame(parc, sp1, sp2)

#row.names(dados_df) <- parc
#dados_df

class(dados_df)
str(dados_df)
dim(dados_df) 
dados_df

## Elaborando lista
data_list <- list(sp1, dados_df)
data_list
class(data_list)
str(data_list)
dim(data_list) 

data_list[[1]] # vetor de abundancia da sp1
data_list[[2]] # df 


# Elaborando vetor do tipo fator - dados com categoria
encosta <- rep("encosta", 5)
ribeirinha <- rep("ribeirinha", 5)

env <- c(encosta, ribeirinha)
class(env)

env_fact <- factor(env)
env_fact
class(env_fact)

# dados_df$env <- env_fact
dados_df$env


dados_df$env <- as.factor(env) 
dados_df
str(dados_df)


#Visualizar dados
View(dados_df)

## Salvar dados
write.table (x = dados_df, file = "outputs/my_data.txt", sep = ";", 
            row.names = FALSE, dec = ",")
?write.table

write.csv (x = dados_df, file = "outputs/my_data.csv", row.names = TRUE)

# Manipulando objetos  ----------------
rm(list=ls()) # CUIDADO!!!  Remove todos os dados já criados 

# Carregando os dados
dados_df <- read.table(file = "outputs/my_data.txt", sep=";", header = T)

dados_df

dim(dados_df)
View(dados_df)

dados_csv <- read.csv(file = "outputs/my_data.csv")

dim(dados_csv)
class(dados_csv)

dados_csv [1:10, 1:5]
dados_csv [,5] # [linha , coluna]
dados_csv [,"env"] # usando o nome da coluna

dados_csv$env #para DATA FRAME também podemos usar $ para acessar as colunas
dados_csv [5,5] # [linha , coluna]


dados_csv [,-1] # remover coluna 1
dados_csv
dados_csv [-1,] # remover linha 1
dados_csv [,-c(1,5)] # remover colunas 1 e 5
dados_csv [,-1:-3] # remover da coluna 1 a 4 

dados_csv <- dados_csv [,-1] # remover coluna 1

dados_csv

# Estatísticas Descritivas ------------------------------------------------
ls() # Lista de objetos criados até o momento
rm(list=ls()) # CUIDADO!!!  Remove todos os dados já criados 

dados_df <- read.table(file = "outputs/my_data.txt", sep=";", header = T)
dim(dados_df)
dados_df

# Cálculos simples
1+1
3-2
100/20
pi*100
10^2
log10(10)
exp(1) 
exp(log(10))
factorial(3)


#Medidas de posição central 
mean_sp1 <- sum(dados_df$sp1)/length(dados_df$sp1)
mean_sp1

mean_sp2 <- sum(dados_df$sp2)/length(dados_df$sp2)
mean_sp2


mean(dados_df$sp1)
mean(dados_df$sp2)

##> A mediana é o valor que separa um conjunto de dados ordenados em
##> duas partes iguais, de modo que 50% dos valores estão abaixo dela e
##> 50% estão acima dela.

sort(dados_df$sp1, decreasing = F)
(5+5)/2

median(dados_df$sp1)

sort(dados_df$sp2)
(2+4)/2
median(dados_df$sp2)

##> Se o numero de obs é par a mediana é o valor médio entre os dois valores centrais
##> aqui 2 e 4

##> Se o numero de obs é impar a mediana é o valores central quando organizado
##> de forma crescente
seq_nump<-seq(from=1,to=31,by=5)
seq_nump
median(seq_nump)


## Visualize os dados em um histograma
hist(dados_df$sp1)


#Melhorando o histograma
hist(dados_df$sp1, #dados brutos
     xlab="Abundância da sp 1", #nome do eixo x
     main="Histograma da abundância da sp 1 em 10 UA", # titulo 
     col="green", #cor de preenchimento 
     ylab="Frequência",  #nome do eixo y
     family="serif") # tipo de fonte


# adicionando linha representando mediana
abline(v=median(dados_df$sp1), lty=1, col="red")
abline(v=mean(dados_df$sp1), lty=2, col="blue")

text(x=5, y=1.5, "Mediana aqui", cex = 1, family="serif", font = 2)


#Medidas de dispersão/variabilidade
var(dados_df$sp1) #variancia
sd(dados_df$sp1)^2 # idem

sd(dados_df$sp1) #devio padrao, usando n - 1 
sqrt(var(dados_df$sp1)) # idem
# 
# sp1<-dados_df$sp1
# range(sp1) #amplitude
# min(sp1) #minimo
# max(sp1) #maximo
# 
# # O que significa 1 desvio padrão da média?
# # Quanto é um desvio padrão a cima e abaixo da média?
# sd_p1<- mean(sp1) + sd(sp1) 
# sd_m1<- mean(sp1) - sd(sp1)
# 
# sp1
# length(sp1[which(sp1 >= mean(sp1) & sp1 <= sd_p1)])
# length(sp1[which(sp1 <= mean(sp1) & sp1 >= sd_m1)])
# 
# sum(3+3)/length(sp1) # ~68% dos valores ficam a um desvio padrão da média 
# 
# # Visualizando a distribuição dos dados
# hist(sp1) # Eixo X = Frequencia ou contagem de valores, y = intervalo de valores
# 
# abline(v=mean(sp1), col="red", lty=2) #Linha mostrando a posição da média
# abline(v=mean(sp1)+sd(sp1), col="blue", lty=2) #Linha mostrando a posição do DP
# abline(v=mean(sp1)-sd(sp1), col="blue", lty=2) #Linha mostrando a posição do DP
# text(x=sd_p1, .05, "+1 SD = 8.52") 
# text(x=sd_m1, .05, "-1 SD = 2.47") 

##> Outras medidas de posição central são:
##> Moda;
##> Ponto médio ((max+min)/2);
##> Média quadrática;
##> Média harmonica;
##> Média ponderada;


# Um pouco mais sobre manipulacao e visualizacao de dados --------------------------------

## Vamos avaliar os padrões dos nossos dados de abundância
dados_df
## Comparando médias
mean(dados_df$sp1)
mean(dados_df$sp2)
## Transforme em matrix para facilitar
mat_num<- as.matrix(dados_df[,c(2,3)])
row.names(mat_num)<-dados_df$parc
mat_num

apply(mat_num, ## aplicar uma funcao as linhas (1) ou as colunas (2) da matriz
      2,
      sum) # abundancia total por UA

## Calcular média da abundacia por ambiente
tapply(dados_df$sp1, # Aplica uma funcao a uma vetor (sp1) agrupando ele conforme outro (env)
       dados_df$env,
       mean)

tapply(dados_df$sp2, dados_df$env, mean)

## Visualizar dados
par(mfrow=c(1,2)) # divide a janela plot em duas colunas 
boxplot (dados_df$sp1 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 1", col="lightgreen")
boxplot (dados_df$sp2 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 2", col="lightblue")

## Ajustar letra
dados_df$env %in% "ribeirinha" # Retorna um vetor com valores logicos mostrando quais
# dos valores sao iguais a "ribeirinha" (TRUE) e quais nao sao (FALSE)
#> Estamos selecionando na coluna env apenas os valores iguais a "ribeirinha"
dados_df$env [dados_df$env %in% "ribeirinha"] <- "Ribeirinha" 
dados_df$env[dados_df$env %in% "encosta"] <- "Encosta"

## Plotar grafico novamente
boxplot (dados_df$sp1 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 1", col="lightgreen")
boxplot (dados_df$sp2 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 2", col="lightblue")
 
## Salvar grafico com boa resolucao
jpeg(filename = "figs/b1.jpg", 
     width = 170, #largura
     height = 100, #altura
     units = "mm", # unidade, pode ser pixel, mm ou cm
     res = 300) # resolução

par(mfrow=c(1,2)) # divide a aba plot em duas colunas 

boxplot (dados_df$sp1 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 1", col="lightgreen")

boxplot (dados_df$sp2 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 2", col="lightblue")

dev.off()



##> A espécie 1 está mais associada a ambiente ribeirinho, mas ocorre em encosta,
##>  enquanto a espécie 2 está mais associada a ambiente ribeirinho e ausente em encostas




# # Testes inferenciais básicos ---------------------------------------------
# dados_df <- read.table(file = "outputs/my_data.txt", sep=";", header = T)
# par(mfrow=c(1,2))
# boxplot(dados_df$sp1 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 1", col="lightgreen")
# boxplot(dados_df$sp2 ~ dados_df$env, xlab="Ambientes", ylab="Espécie 2", col="lightblue")
# 
# ### Teste t e alternativa não paramétrica
# 
# shapiro.test(dados_df$sp1)
# shapiro.test(dados_df$sp2)
# # ambos possuem distribuição próxima a gaussiana
# 
# bartlett.test(dados_df$sp1,dados_df$env)
# bartlett.test(dados_df$sp2,dados_df$env)
# ### Não ha homogenidade de variancias
# 
# alt_sp1 <- dados_df$sp1
# t_res<-stats::t.test(dados_df$sp1 ~ dados_df$env)
# 
# ##> Concluimos que a abundância média da sp 1 é estatisticamente superior em
# ##> ambiente ribeirinho em comparação ao ambiente de encosta (test t = -3.32, df = 6.81; p < 0.05),
# ##> em uma taxa de rejeição da hipótese nula de 5%.
# 
# ## Teste wilcox para dados que violam os pressupostos do teste t, como é o caso da sp2
# wilcox_res<-stats::wilcox.test(sp2 ~ env, data=dados_df)



