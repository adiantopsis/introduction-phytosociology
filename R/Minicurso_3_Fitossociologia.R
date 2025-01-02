#### --- Packages --- ####
library(vegan)
library(adiv)
library(readxl)
library(reshape2)
library(openxlsx)
library(tidyverse)
library(patchwork)
library(reshape)
library(reshape2)
library(iNEXT)
library(flora)
library(DHARMa)
source("func/FitoR.R")
# source("func/FitoR_camp.R", encoding = "utf-8")
source("func/parc_resume.R")

options(scipen = 9999999, ## Remover notacao cientifica
        OutDec= ",", ## Utilizar a virgula como separador decimal (padrao BR)
        digits = 4) ## Numero de digitos para aparecer no console

###### --- Carregar dados --- ####
w_matrix <- readxl::read_xlsx("data/field_survey.xlsx", sheet ='Fitossociologia')
## Carregar dados de fitossociologia

glimpse(w_matrix)

e_matrix <- readxl::read_xlsx("data/field_survey.xlsx", sheet = "Ambiente") 
## Carregar dados ambientais

###### --- Curva do coletor --- ####
!w_matrix$spp %in% "Morta"

## Preparacao de dados
## Remover mortas dos dados com tidyverse
w <- filter(w_matrix, !spp %in% "Morta") 

w$spp %in% "Morta" %>% summary()

## Rearranjar os dados para obter dados uma tabelas simples de presenca e ausencia
col <- reshape2::dcast(parc ~ spp, # formula que arranja as parcelas nas linhas e as spp nas colunas
                       data = w, # indicacao dos dados
                       value.var = "dap", # indicacao de uma coluna para preencher a matriz de dados
                       length) # usar uma funcao para preencher os dados, nesse caso a length retorna quantas 
# vezes aparecem valores de dap para as especie i ao longo das parcelas, ou seja, a abundancia da 
# especie i ao longo das parcelas


## Transformar dados de "abundancia" em presenca e ausencia
w_pa <- decostand(col[,-1], # remover primeira coluna - parcelas
                  method = "pa", # metodo de transformacao, nesse caso presencia e ausencia
                  MARGIN = 1) # onde é para aplicar a transformacao nas colunas (2) ou nas linhas (1)

#Alternativamente use ifelse
# w_pa <- ifelse(col[,-1] < 1, # se os dados da matris forem menores que 1
# 0, # preencha com zero
# 1) # de outro modo, preencha com 1

## Curva do coletor simples (vegan)
r <- specaccum(w_pa, method = "random", permutations = 999) # criar curva com rarefaçao 
c <- specaccum(w_pa, method = "collector") #criar a curva classica
class(r)


## Primeiro vamos criar o grafico com a curva de acumulacao
## Crie a base com a linha e outros elementos do grafico
plot(c)
plot(c, #use diretamente os dados
     lwd=5, # espessura da linha
     col="grey", # cor da linha
     xlab="Unidades amostrais", #nome do eixo x 
     ylab="Riqueza") #nome do eixo y

## Adicione os pontos na curva, e preciso fazer meio que na mao
points(y=c$richness, # inicando que dentro do objeto estao os valores de y (riqueza)
       x=c$sites, #x unidades amostrais
       cex=3, #tamanho dos pontos
       pch=16, #tipo dos pontos
       col="black") #cor

## Agora vamos criar o grafico com a curva de rarefacao
## Crie a base com a linha e outros elementos do grafico
plot(r, # use o objeto
     random = T,  # aqui nos inicamos se as curvas criadas por cada randomizacao deve aparecer, o default é que nao apareca
     col="grey", 
     xlab="Unidades amostrais", 
     ylab="Riqueza")

## Adicionar a linha media considerando todas as 999 aleatorizacoes e o intervalo de confianca
plot(r, 
     add=T, # esse argumento adiciona esse grafico ao grafico anterior
     ci.type = "line", # definir tipo de intervalo de confianca 
     ci.col="lightgreen", # cor do intervalo de confianca 
     col="black",  
     lwd=3)

## adicionar pontos 
points(y=r$richness,x=r$sites, cex=3, pch=16, col="black")

### Curva de rarefacao com boxplot
# plot(r, random=T, ci = 2, lty = 6, col="grey", xlab="Unidades amostrais", ylab="Riqueza", cex.lab=1.5, 
#      cex.axis=1.2, family="serif")
# boxplot(r, add=T, col="lightgreen", lwd=1)


### Criar curva com ggplot2
### Passe os dados para um dataframe contendo os dados
data1 <- data.frame(Sites=seq(from=100, to=length(c$sites)*100, by=100),# UA
                  Richness_collector = c$richness, #Riqueza acumulada real
                  Richness_rarefaction = r$richness,  #Riqueza estimada por raferafacao
                  upper=r$richness + r$sd, # 1 Desvio padrao acima da media 
                  lower=r$richness - r$sd) # 1 Desvio padrao abaixo da media
data1
## Curva de acumulacao
ggplot(data=data1, aes(x=Sites, y=Richness_collector)) +
  geom_line(col="red", lwd=1, lty=6) +
  geom_point(col="red", size=6) +
  theme_bw(base_family = "serif", base_size = 20)+
  labs(x="Área (m²)", y="Riqueza")+
  scale_y_continuous(limits = c(3,21))

ggsave("figs/curva_acum.png", dpi = 300, width =25 , height = 10, units = "cm")

## Curva de rarefacao
ggplot(data=data1, aes(x=Sites, y=Richness_rarefaction)) +
  geom_line(col= "lightgreen", lwd=1, lty=6) +
  geom_point(col= "lightgreen",size=6) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha=0.3, fill="lightgreen") +
  theme_bw(base_family = "sans", base_size = 20)+
  # theme(legend.title = element_text(face = "bold"),
  #       axis.title = element_text(family = "Utopia", face="bold", colour = "black"), 
  #       axis.text = element_text(colour = "black"))+
  labs(x="Área (m²)", y="Riqueza")+
  scale_y_continuous(limits = c(3,21))

ggsave("figs/curva_rare.png", dpi = 300, width =25 , height = 10, units = "cm")

### Para mais infos sobre o ggplot acesse é possivel acessar os seguintes tutoriais:
##> https://ggplot2.tidyverse.org/articles/ggplot2.html
##> https://opencodecom.net/post/2021-08-22-introducao-ao-ggplot2/
##> https://rpubs.com/jarrais/GET00183_Topico3


## Curva dos slides
# ggplot(data=data1) +
#   geom_line(aes(x=Sites, y=Richness_rarefaction, col="Rarefação"), lwd=1, lty=6) +
#   geom_point(aes(x=Sites, y=Richness_rarefaction, col="Rarefação"), fill= "lightgreen",size=6, lwd=5, shape=21) +
#   geom_ribbon(data=data1, aes(x=Sites, y=Richness_rarefaction, ymin = lower, ymax = upper), 
#               linetype=2, alpha=0.3, fill="lightgreen") +
#   geom_line(aes(x=Sites, y=Richness_collector, col="Acumulação"), lwd=1, lty=6) +
#   geom_point(aes(x=Sites, y=Richness_collector, col="Acumulação"),fill="red",size=6, lwd=5, shape=21) +
#   theme_bw(base_family = "Utopia", base_size = 20)+
#   theme(legend.position = "right", legend.title = element_text(face = "bold"),
#         axis.title = element_text(family = "Utopia", face="bold", colour = "black"), axis.text = element_text(colour = "black"))+
#   labs(x="Área (m²)", y="Riqueza", col="Tipo de curva")+
#   scale_y_continuous(limits = c(3,21))+
#   scale_colour_manual(values =c("red", "lightgreen"))
# 
# ggsave("figs/curva_ggplot.png", dpi = 300, width =25 , height = 10, units = "cm")


######> INEXT - Rarefaction and Extrapolation
##> Organizando os dados
col1 <- apply(w_pa, 2, sum) %>% # Soma as incidencias das spp ao longo das UA
  append(values = c(Total=20)) %>% # Adiciona o total de UA presente dos dados
  sort(decreasing = T)  # Arranja os dados em ordem decrescente


## Realizando a extrapolacao com base nos numeros de hill
out1 <- iNEXT(x = col1, # indicando os dados
              q = c(0,1,2),  #indicando a ordem dos numeros de Hill, 0=riqueza, 1=exp(shannon), 2=simpson
              datatype = "incidence_freq", #tipo de dados, nesse caso incidência
              endpoint = 40) #ponto final da extrapolacao
              
## Avaliando os resultados
out1$DataInfo # informacoes basicas
out1$AsyEst # estimadores de riqueza 

#H' obtido
log(13.02)
#H' esperado
log(14.39)

## Visualizando as curvas
ggiNEXT(out1, type=2, color.var = "Order.q") # Cobertura ao longo da amostragem
out1$DataInfo 

ggiNEXT(out1, type=3, color.var = "Order.q") # Cobertura em funcao da diversidade

ggiNEXT(out1, type=1, color.var = "Order.q") # Curva de rarefacao com diferentes medias de diversidade


## Curva baseada na riqueza apenas
out_0 <- iNEXT(x = col1, # indicando os dados
              q = 0,  #indicando a ordem dos numeros de Hill, 0=riqueza, 1=exp(shannon), 2=simpson
              datatype="incidence_freq", #tipo de dados, nesse caso incidência
              endpoint = 40, #ponto final da extrapolacao
              conf=.95) #nivel de confiancia

ggiNEXT(out_0, type=1, color.var = "Order.q")+
  theme_minimal()+
  theme(legend.position = "right", legend.title = element_text(face = "bold"),
        axis.title = element_text(family = "Utopia", face="bold", colour = "black"), 
        axis.text = element_text(colour = "black"))+
  labs(x="Unidades amostrais", y="Riqueza")+
  scale_colour_manual(values="red")+
  scale_fill_manual(values="red")+
  scale_linetype_manual(values=c(1,3),
                        labels = c("Rarefação", "Extrapolação"))+
  guides(shape="none", fill="none", col="none")

ggsave("figs/Coletor_INEXT.png", dpi = 300, width =170 , height = 100, units = "mm", bg="white")

# estimateD(col1,nboot=999, datatype="incidence_freq")

### Veja mais informacoes sobre o pacote em: https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.pdf


###### --- Analise fitossociologica --- #######
fit <- fitoR(data.frame(w_matrix), area=100, VI= "percent", filename = "outputs/fit")
str(fit)
fit

fit$Resumo
# Densidade total (ind/ha)
# Area basal (m²/ha)
# Riqueza (spp.)   
# Diversidade Shannon-Wiener (nats)   
# Equitabilidade 

fit$Fitossociologia

fit$Fitossociologia %>% arrange(desc(N)) %>% head(n=5L)
fit$Fitossociologia %>% arrange(desc(FA)) %>% head(n=5L)
fit$Fitossociologia %>% arrange(desc(DoA)) %>% head(n=5L)
fit$Fitossociologia %>% arrange(desc(VI)) %>% head(n=5L)

## Avaliar se nomes estao corretos
get.taxa(row.names(fit$Fitossociologia),
         replace.synonyms = T,
         life.form = T,
         establishment = T,
         endemism = T,
         domain = T,
         habitat = T,
         states = T,
         drop = c(
           "accepted.name",
           "taxon.status",
           "taxon.rank",
           "authorship",
           "genus",
           "specific.epiteth",
           "infra.epiteth",
           "name.status")
) %>% view()

## Adicionar ao data.frame uma coluna com nome de spp com o nome das espécies (nome das linhas) 
fito<-fit$Fitossociologia %>% 
  mutate(spp= row.names(.)) 
fito

### Criar grafico do valor de importancia
df_imp <- data.frame(arrange(fito, desc(VI)) [1:10,]) %>% # Selecionar as 10 principais especies em VI
  dplyr::select(spp, DR, DoR, FR) %>% # Selecionando vars que contribuem para o VI
  melt(id.vars="spp", meansure.vars=c("DR", "FR", "DoR")) # Rearranjado dados para obter colunas e facilitar no ggplot

df_imp

ggplot(data=df_imp, aes(y=reorder(spp, value), x=value, fill=variable))+ 
  # Criar objeto, repare que os eixos estao modificados e estou usando o preenchimento como os nomes das vars
  geom_bar(stat='identity')+
  theme_bw(base_size = 13)+
theme(axis.text.y = element_text(angle = 0, face = "italic", colour = "black"),
        axis.text.x = element_text(colour = "black"))+
  labs(y="Espécies", x="Valor de importância", fill="Descritores")+
  scale_fill_brewer(palette = "Dark2")

hcl.pals()


ggsave("figs/ini_VI.jpg", dpi = 300, width =170 , height = 100, units = "mm", bg="white")

###### --- Resumo das parcelas --- #####
resume <- parc.resume(w_matrix)
resume$Input

resume$Resume$Est<- e_matrix$Estrato
resume$Resume$Alt<- e_matrix$Altitude


res_parc<-resume$Resume

res_geral<-resume$Input %>% merge(e_matrix, by="parc")

###### --- Salvar dados ---- #####
my_list <- list(Tabela_Fitossociologia=fit$Fitossociologia,
                Resumo_Fitossociologia=fit$Resumo,
                Resultados_Parcelas=resume$Resume,
                Dados_Brutos = resume$Input)

openxlsx::write.xlsx(my_list, file = "outputs/Fitossociologia_Final.xlsx", 
                     rowNames=T)


###### --- Estatísticas inferenciais no R aplicadas a fitossociologia --- #######
# Por favor revise os pressupostos de cada analise e sempre tenham uma hipotese
# para cada uma delas

##> Há correlacao entre riqueza e diversidade nessa comunidade? <-----------------
plot(log(res_parc$S), res_parc$div, xlab = "Riqueza em log", 
     ylab="Diversidade de Shannon-Wiener",
     col="red",
     pch=16,
     cex=1.5)

## Corelacao - quando nao ha causa e efeito 
cor.test(log(res_parc$S), res_parc$div)


##> Existe uma correlação entre riqueza e area basal? <-----------------
##> Por um lado seria esperado que quanto maior a riqueza mais plantas com diferentes
##> estariam presentes para ocupar o habitat. 
#
##> Por outro, a área pode representar um habitat 
##> com poucas espécies dominantes, que estão bem adaptadas a exploração dos recursos, portanto,
##> ocupam a maior parte do habitat.

plot(y=log(res_parc$S), x=res_parc$AB, xlab = "Area Basal", 
     ylab="Riqueza",
     col="red",
     pch=16,
     cex=1.5)

## Regressao linear - quando ha causa (x) e efeito (y)
model <-lm(log(res_parc$S)~ res_parc$AB)
summary(model)
# Beta: cada aumento de um ponto na riqueza em log (i.e., 2,7 sp) ha um incremento de 0,79 m² na AB
# Alfa (intercepto): Ponto de partida da reta, se a area basal fosse 0 seria esperado 1,3 riqueza em log (3 spp).
abline(model)


simulateResiduals(model, plot=T)


##> Há diferenca entre a diversidade média dos estratos? <-----------------
boxplot(formula = div ~ Est, data = res_parc) # Parece que nao

# execute uma anova
anova_div <-aov(data=res_parc, div ~ Est)
# Avalie os pressupostos da anova: homogenidade das variancias dentro dos grupos
# e normalidade dos residuos
bartlett.test(div ~ Est, data=res_parc) # nao signif, ou seja, var homogeneas
shapiro.test(residuals(anova_div)) #não signif, residuos com distr. gaussiana

#veja o resultado
summary(anova_div)


##> Há diferenca entre a riqueza dos estratos? <-----------------
boxplot(formula = S ~ Est, data = res_parc) # Tambem parece que nao

# execute uma anova
anova_riq <-aov(data=res_parc, S ~ Est)
# Avalie os pressupostos da anova: homogenidade das variancias dentro dos grupos
# e normalidade dos residuos
bartlett.test(S ~ Est, data=res_parc) # nao signif, ou seja, var homogeneas
shapiro.test(residuals(anova_riq)) #signif, residuos com distr. gaussiana.
## Nesse caso use o log da riqueza
anova_riq <-aov(data=res_parc, log(S) ~ Est)
shapiro.test(residuals(anova_riq)) #nao signif, i.e. residuos com distr. gaussiana.

#veja o resultado
summary(anova_div)


##> Há diferenca entre o AB das plantas dos estratos? <-----------------
boxplot(formula = AB ~ Est, data = res_parc) # Tambem parece que SIM

# execute uma anova
anova_AB <-aov(data=res_parc, AB ~ Est)
# Avalie os pressupostos da anova: homogenidade das variancias dentro dos grupos
# e normalidade dos residuos
bartlett.test(AB ~ Est, data=res_parc) # nao signif, ou seja, var homogeneas
shapiro.test(residuals(anova_AB)) #signif, residuos com distr. gaussiana.
## Nesse caso use o log da riqueza
anova_AB <-aov(data=res_parc, log(AB) ~ Est)
shapiro.test(residuals(anova_AB)) #nao signif, i.e. residuos com distr. gaussiana.

#veja o resultado
summary(anova_AB)


##> Há diferenca entre a H das plantas dos estratos? <-----------------
boxplot(formula = mean_h ~ Est, data = res_parc) # Tambem parece que SIM

# execute uma anova
anova_H <-aov(data=res_parc, mean_h ~ Est)
# Avalie os pressupostos da anova: homogenidade das variancias dentro dos grupos
# e normalidade dos residuos
bartlett.test(mean_h ~ Est, data=res_parc) # nao signif, ou seja, var homogeneas
shapiro.test(residuals(anova_H)) #nao signif, residuos com distr. gaussiana.

#veja o resultado
summary(anova_H)












### DAP profile
resume <- lapply(w_split, parc_resume)
## Aplicando a regra de sturge para quebra de classes do hist
breaks <- pretty(range(resume$Inicial$Input$dap_eq),
                 n = nclass.Sturges(resume$Inicial$Input$dap_eq),
                 min.n = 0)

ggplot(data=resume$Inicial$Input)+
  geom_histogram(aes(x = dap_eq), breaks = breaks, color = 1, fill = "#97bf80")+
  geom_vline(aes(xintercept = mean(dap_eq), colour = "Média"), linetype=2)+
  theme_bw(base_size = 13)+
  # theme(axis.text.y = element_text(angle = 0, face = "italic", colour = "black"),
  #       axis.text.x = element_text(colour = "black"))+
  labs(y="Frequência", x="Classes de DAP", col=" ")+
  scale_x_continuous(breaks = breaks)
filter(resume$Inicial$Input, dap_eq> 30)

ggsave("figs/Ini_Perfil_DAP.jpg", dpi = 300, width =170 , height = 100, units = "mm", bg="white")

### Height profile
mean((resume$Inicial$Input$H))
breaks <- pretty(range(resume$Inicial$Input$H),
                 n = nclass.Sturges(resume$Inicial$Input$H),
                 min.n = 0)
ggplot(data=resume$Inicial$Input)+
  geom_histogram(aes(x = H), breaks = breaks, color = 1, fill = "#97bf80")+
  geom_vline(aes(xintercept = mean(H), colour = "Média"), linetype=2)+
  theme_bw(base_size = 13)+
  # theme(axis.text.y = element_text(angle = 0, face = "italic", colour = "black"),
  #       axis.text.x = element_text(colour = "black"))+
  labs(y="Frequência", x="Classes de Altura", col=" ")+
  scale_x_continuous(breaks = breaks)
filter(resume$Inicial$Input, H> 10)

ggsave("figs/Ini_Perfil_H.jpg", dpi = 300, width =170 , height = 100, units = "mm", bg="white")

## Estagio medio
fit$Médio[[5]]$N %>% sum


fit$Médio
fit$Médio[[5]] %>% arrange(desc(N)) %>% head(n=5L)
fit$Médio[[5]] %>% arrange(desc(DoA)) %>% head(n=5L)
fit$Médio[[5]] %>% arrange(desc(VI)) %>% head(n=5L)




fito<-fit$Médio[[5]] %>% mutate(spp= row.names(.))

spp_m<-get.taxa(row.names(fit$Médio[[5]]),
         replace.synonyms = T,
         life.form = T,
         establishment = T,
         endemism = T,
         domain = T,
         habitat = T,
         states = T,
         drop = c(
           "accepted.name",
           "taxon.status",
           "taxon.rank",
           "authorship",
           "genus",
           "specific.epiteth",
           "infra.epiteth",
           "name.status")
)
view(spp_m)
spp_m$search.str[is.na(spp_m$search.str)]<-"Morta"

fito<-fit$Médio[[5]] %>% mutate(spp= spp_m$search.str)
### Importance graph 

ggplot(data=data.frame(arrange(fito, desc(VI)) [1:10,]))+
  geom_bar(aes(y=reorder(spp, VI), x=VI), stat='identity', fill="#318d32")+
  theme_bw(base_size = 13)+
  theme(axis.text.y = element_text(angle = 0, face = "italic", colour = "black"),
        axis.text.x = element_text(colour = "black"))+
  labs(y="Espécies", x="Valor de importância")

ggsave("figs/med_VI.jpg", dpi = 300, width =170 , height = 100, units = "mm", bg="white")

####### ---- Perfil diametrico e de altura da comunidade -----------------------------####
## Perfil diametrico
mean(res_geral$dap_eq)
range(res_geral$dap_eq)

## Histograma do perfil diamétrico
hist(res_geral$dap_eq, 
     col = "#318d32",
     main = " ", # Remover titulo
     xlab= "Classes de diâmetro", 
     ylab ="Frequência")

# Adicionar linha vertical representando a média
abline(v=mean(res_geral$dap_eq), lty=2) 

### Histograma com ggplot
## Aplicando a regra de sturge para quebra de classes do hist
breaks <- pretty(range(res_geral$dap_eq),
                 n = nclass.Sturges(res_geral$dap_eq),
                 min.n = 0)

ggplot(data=res_geral, aes(dap_eq))+
  geom_histogram(breaks = breaks, color = 1, fill = "#318d32")+
  geom_vline(aes(xintercept = mean(dap_eq), colour = "Média"), linetype=2)+
  theme_bw(base_size = 13)+
  labs(y="Frequência", x="Classes de DAP", col=" ")+
  scale_x_continuous(breaks = breaks)

ggsave("figs/Med_Perfil_DAP.jpg", dpi = 300, width =170 , height = 100, units = "mm", bg="white")
filter(resume$Médio$Input, dap_eq> 70)


### Perfil de altura 
mean(res_geral$H)
range(res_geral$H)
## Aplicando a regra de sturge para quebra de classes do hist
breaks <- pretty(range(res_geral$H),
                 n = nclass.Sturges(res_geral$H),
                 min.n = 0)

ggplot(data=res_geral)+
  geom_histogram(aes(x = H), breaks = breaks, color = 1, fill = "#318d32")+
  geom_vline(aes(xintercept = mean(H), colour = "Média"), linetype=2)+
  theme_bw(base_size = 13)+
  # theme(axis.text.y = element_text(angle = 0, face = "italic", colour = "black"),
  #       axis.text.x = element_text(colour = "black"))+
  labs(y="Frequência", x="Classes de Altura", col=" ")+
  scale_x_continuous(breaks = breaks)
filter(resume$Médio$Input, H> 14)

ggsave("figs/Med_Perfil_H.jpg", dpi = 300, width =170 , height = 100, units = "mm", bg="white")


####### ---- Amostragem Estratificada --- ######
w_matrix
## Carregar dados ambientais
e_matrix

## Unir as duas matrizes
my_matrix <- left_join(e_matrix, w_matrix, by="parc")

glimpse(my_matrix)

## Dividir dados por estrato
w_split<-split.data.frame(my_matrix, ~ my_matrix$Estrato)

glimpse(w_split)

## Elaborar planilha fitossociologica
fit<- lapply(w_split, function (x) fitoR(data.frame(x), 100, filename = NA))


## Elaborar curva do coletor
w <- lapply (w_split, function(x) filter(x, spp != "Morta"))


w_pa <- lapply(w, function (x) { 
  a<-dcast(parc~spp, data = x, value.var = "dap", length)
  b<-ifelse(a[,-1] < 1, 0, 1) 
  return(b)})

### Curva de acumulacao e rarefacao
r<- lapply(w_pa, function (x) specaccum(x, "random", permutations = 999))
c<- lapply(w_pa, function (x) specaccum(x, "collector"))

## Unir dados da curva de acumulacao por estrato
lc<-lapply(c, function(x) cbind.data.frame(Sites=x$sites, 
                                           Richness=x$richness)) %>% 
  list_rbind(names_to = "id")

### Curva estagio inicial
filter(lc, id=="Inicial")%>% ggplot() +
  geom_line(aes(x=Sites, y=Richness), col="lightgreen", linewidth=1, lty=6) +
  geom_point( aes(x=Sites, y=Richness), fill="lightgreen",size=4, 
              col="gray30", shape=21) +
  theme_bw(base_family = "Utopia", base_size = 12)+
  theme(legend.position = "right", legend.title = element_text(face = "bold"),
        axis.title = element_text(family = "Utopia", face="bold", colour = "black"),
        axis.text = element_text(colour = "black"))+
  scale_x_continuous(labels= seq(0, 1000, 200 ), limits = c(0,10), breaks = seq(0,10, 2))+
  scale_y_continuous(limits = c(3,16))+
  labs(x="Área (m²)", y="Riqueza", fill="Estágio sucessional", col= " ")+ 
  guides(col="none")

ggsave("figs/ini_curv.png", dpi = 300, width =25 , height = 10, units = "cm")

### Curva estagio médio
filter(lc, id=="Médio")%>% ggplot() +
  geom_line(aes(x=Sites, y=Richness), col="green4", linewidth=1, lty=6) +
  geom_point( aes(x=Sites, y=Richness), fill="green4",size=4, 
              col="gray30", shape=21) +
  theme_bw(base_family = "Utopia", base_size = 12)+
  theme(legend.position = "right", legend.title = element_text(face = "bold"),
        axis.title = element_text(family = "Utopia", face="bold", colour = "black"),
        axis.text = element_text(colour = "black"))+
  scale_x_continuous(labels= seq(0, 1000, 200 ), limits = c(0,10), breaks = seq(0,10, 2))+
  scale_y_continuous(limits = c(3,16))+
  labs(x="Área (m²)", y="Riqueza", fill="Estágio sucessional", col= " ")+ 
  guides(col="none")

ggsave("figs/méd_curv.png", dpi = 300, width =25 , height = 10, units = "cm")


### Inext 
### Preparar dados 
col1 <- lapply(w_pa, FUN = function(x) { a<-apply(x, 2, sum) 
names(a)<- NULL
a<-sort(a, decreasing = T)
return(a)})

col1[[1]]<-c(10, col1[[1]])
col1[[2]]<-c(10, col1[[2]])


### Realizar extrapolacao
out1 <- iNEXT(col1, q=c(0,1,2), datatype="incidence_freq", knot=50, conf=.95)

### Comparar os estimadores
ggiNEXT(out1, type=1, facet.var = "Order.q", color.var = "Assemblage")
ggiNEXT(out1, type=2, color.var = "Assemblage")
ggiNEXT(out1, type=3, color.var = "Assemblage")

### Curva de riqueza
out1 <- iNEXT(col1, q=0, datatype="incidence_freq", knot=50, conf=.95)
ggiNEXT(out1, type=1, color.var = "Assemblage")+
  theme_minimal()+
  theme(legend.position = "right", legend.title = element_text(face = "bold"),
        axis.title = element_text(family = "Utopia", face="bold", colour = "black"), 
        axis.text = element_text(colour = "black"))+
  labs(x="Unidades amostrais", y="Riqueza")+
  scale_colour_manual(values=c("red", "blue")
  )+
  scale_fill_manual(values=c("red", "blue"))+
  scale_linetype_manual(values=c(1,3),
                        labels = c("Rarefação", "Extrapolação"))+
  guides(shape="none", fill="none")

ggsave("figs/Coletor_INEXT.png", dpi = 300, width =170 , height = 100, units = "mm", bg="white")


my_list <- list(Médio=fit$Médio$Fitossociologia, Inicial=fit$Inicial$Fitossociologia)

openxlsx::write.xlsx(my_list,
                     file = "outputs/Plan_fit.xlsx", 
                     rowNames=T)



####### ---- Executar analises para inventario florestal --- ######
### Fique atento aos resultados 
### Confira a documentacao no github: https://github.com/igorcobelo/florestal

### Instalar pacote
# remotes::install_github("igorcobelo/florestal")
# library(florestal)
# 
# ### Transformar dados com multiplas colunas de DAP para uma unica de diametro equivalente 
# IF<-dap.eq(my_matrix, ff=0.55)
# glimpse(IF)
# 
# ### Construir tabela conforme documentacao
# D<-dplyr::select(IF,dap_eq)
# D$Stratum<-as.numeric(as.factor(IF$Estrato))
# D$Plot<-IF$parc
# D$Individual<-seq(1, length(IF$parc), 1)
# D$Specie<-IF$spp
# D$"h (m)"<-IF$H
# D$"d (cm)"<-IF$dap_eq
# D$"Volume (m3)"<-IF$vm3
# D<-dplyr::select(D,-dap_eq)%>% arrange(Stratum)
# 
# ### Aplicar analise de dados para amostragem casual estatificada 
# ace1<-florestal::ace(D, a=0.01, aj=c(1.385,1.862))
# ace1$`parametros fito`
# 
# ### Procure onde estao os resultados
# tempdir()











