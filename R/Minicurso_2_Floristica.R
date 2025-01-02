### --- Packages --- #####
library(flora)
library(readxl)
library(reshape2)
library(openxlsx)
library(tidyverse)
library(patchwork)

#### -- Carregando os dados --- ####
flor_data <- read_xlsx(path = "data/field_survey.xlsx", sheet= "Qualitativo")
flor_data

 ### Remover nome de autores ###### 
# x<-matrix(NA, nrow=length(flor_data$Espécie), ncol=1)
# 
# for (i in 1:length(flor_data$Espécie)) {
#   x[i]<-remove.authors(flor_data$Espécie[i])
#   }

### --- Atualizar binomios --- ####
## Crie um vetor com os nomes das spp
spp <- flor_data$Espécie

## Confira o tamanho do vetor
length(spp)

#Utilize a funca get.taxa
up_spp <- get.taxa(
  spp, # vetor de spp.
  replace.synonyms = T, # fazer a substituicao de sinonimos
  life.form = T, # obter dados das formas de vida (nesse caso habito) 
  establishment = T, # obter origem
  endemism = T, # obter endemismo
  domain = T, #bioma (dominio) de ocorrencia
  habitat = T, # tipo de habitat (substrato)
  states = T, #estados de ocorrencia
  drop = c( #esse argumento remove varias coisas não desejaveis que a funcao retornaria
    "accepted.name",
    "taxon.status",
    "taxon.rank",
    "authorship",
    "genus",
    "specific.epiteth",
    "infra.epiteth",
    "name.status")
  )

#saveRDS(up_spp, file = "data/up_spp.RDS") #Salva um objeto do environment 

#Para quem esta sem acesso a internet execute os comandos abaixo
# up_spp <-readRDS(file='data/up_spp.RDS') #Carrega um objeto .RDS (exportado direto do environment)

# comparar dimensoes dos resultados
length(spp)
dim(up_spp)

view(up_spp)
up_spp$notes 
#> Notas com informacoes se houve a substituicao de sinonimo,
#> se sp nao foi encontrada na base de dados e outras coisas


#### --------- Manipulando dados florísticos ----------- ####
# up_spp$family # Nas linhas 7 e 16 tem dados ausentes (NA)
# is.na(up_spp$family) #Essa funcao retorna de maneira logica (TRUE ou FALSE)
# # onde estao os dados ausentes
# summary(is.na(up_spp$family))
# 
# up_spp[is.na(up_spp$family),] #filtrar dentro dos dados apenas os nomes ausentes (NAs)
# 
# ##> Inserir familias que nao foram encontradas
# up_spp[is.na(up_spp$family),]$original.search 
# 
# up_spp[is.na(up_spp$family),]$family <- c("Asparagaceae","Cyperaceae", "Lauraceae",
#                                            "Ruscaceae", "Zingiberaceae")
# summary(is.na(up_spp$family))
# view(up_spp)
# 
# 
# ##> Inserir origem nao foram encontradas
# up_spp$original.search[is.na(up_spp$establishment)]
# up_spp$establishment[is.na(up_spp$establishment)]<- c("naturalized", "naturalized",NA,
#                                                      NA, "cultivated",
#                                                      NA,NA,"naturalized",NA, NA, NA,
#                                                      "cultivated","naturalized")
# 
# ## Traduzir os nomes obtidos com get.taxa
# up_spp$establishment[up_spp$establishment %in% "native"]<-"Nativa" 
# up_spp$establishment[up_spp$establishment %in% "cultivated"]<-"Cultivada"
# up_spp$establishment[up_spp$establishment %in% "naturalized"]<-"Naturalizada"
# up_spp$establishment[is.na(up_spp$establishment)]<-"Indeterminada"
# 
# ### Substituir os nomes ausentes na coluna de nomes atualizados pelos nomes buscados
# ## originalmente
# up_spp$original.search[is.na(up_spp$search.str)]
# 
# up_spp$search.str[is.na(up_spp$search.str)] <- 
#   up_spp$original.search[is.na(up_spp$search.str)]
# 
# view(up_spp)
# 
# ###------- Elaborando gráficos com ggplot e Manipuland dados com tidyverse ----####
# ## Uso do pipe ( %>% ), esse simbolo insere o resultado da esquerda na função da direito 
# up_spp$family %>% length() #Obtemos o comprimento da coluna family
# up_spp$family %>% length() %>% class() #Obtemos a classe do resultado do comprimento da coluna family
# up_spp$family %>% length() %>% class() %>% class() #Obtemos a classe da classe do resultado do comprimento da coluna family
# 
# cbind(N1=c(1:3), N2 = c(4:6)) #Atribuir nomes as colunas
# c(N1=1,N2=2,N3=3) #Atribuir nomes aos valores do vetor
# 
# # resumir dados com o tidyverse
# df_family <- group_by(up_spp, family) %>%  #funçao para agrupar dados por familia
#   summarise(N = n()) %>%  #retorna o tamanho de cada grupo (familias)
#   mutate(perc = proportions(N)*100) %>% #cria uma coluna o percentual de riqueza por familia
#   arrange(desc(N)) #Reordena os dados para as familias mais ricas aparacerem primeiro
# 
# 
# 
# #se houver espécies duplicadas use:
# # df_family <- group_by(up_spp, family) %>%  #funçao para agrupar dados por familia
# #   summarise(N=length(unique(search.str))) %>% # Species richness
# #   mutate(perc = proportions(N)*100) %>%  #Creating % column
# #   arrange(desc(N)) #Reordering species by number
# 
# 
# #Se quiser remover as indeterminadas do grafico use:
# # df_family <-df_family %>% na.omit()
# # df_family <-df_family %>% filter(family != "Indeterminada")
# 
# #Contagem de familias removendo as indeterminadas
# # if(!is.na(summary(df_family$family == "Indeterminada")[3])) { length(df_family$family)-1} else {
# #   length(df_family$family)}
# 
# #Species number
# sum(df_family$N)
# 
# #Final graph barplot (funcao nativa)
# barplot(df_family$N, #valores de riqueza
#         names.arg = df_family$family, # nomes das familias
#         col="lightgreen",
#         las=2, #estilo dos nomes do eixo 2 = nomes na vertical 
#         cex.names=0.6, # diminuir letra dos nomes no eixo x
#         xlab=" ", #remover o titulo do eixo x
#         ylab="Riqueza", # Adicionar titulo no eixo Y
#         space=.3 #espaco entre barras
#         )
# 
# #salvar grafico
# jpeg(filename = "figs/fam_graph_base", res = 300, width = 170, height = 150, units = "mm")
# barplot(df_family$N, names.arg = df_family$family,
#         col="lightgreen",
#         las=2, #estilo dos nomes do eixo 2 = perpendicular ao eixo
#         cex.names=0.6, # diminuir letra dos nomes no eixo x
#         xlab=" ", #remover o titulo do eixo x
#         ylab="Riqueza", # Adicionar titulo no eixo Y
#         space=.3 #espaco entre barras
# )
# dev.off()
# 
# #Final graph ggplot
#  p1<- 
#   ggplot(data=df_family, mapping = aes(x=reorder(family,-N), y=N))+  # argumento para iniciar um objeto do ggplot
#   geom_bar(stat = "identity", col="gray70", fill="lightgreen", alpha=1)+ # criar um grafico de barra
#   theme_classic(base_family = "serif", base_size = 10)+ # utilizar um tema predefinido no ggplot
#   theme(axis.text = element_text(colour = "black"), # definir a cor do texto nos eixos do grafico
#         axis.title = element_text(colour ="black", face = "bold"), #definir a cor e o tipo de fonte do titulo dos eixos
#         axis.text.x = element_text(colour = "black", size = 10, angle = 90, face = "italic"),  # modificar angulo dos nomes no eixo x
#         title = element_text(size = 9))+  # reduzir ou aumentar titulo
#   labs(x="Famílias", y="Riqueza", title = "Riqueza por família botânica")+ #adicionar titulo dos eixos
#   scale_y_continuous(limits = c(0,8)) # definir limites do eixo y
# 
# p1
# 
# #Salvar grafico    
# ggsave(plot = p1, "figs/fam_graph_ggplot.png", dpi = 300, width =170 , height = 100, units = "mm") 
# 
# 
# ## Origem ----------------------------------------------------------
# # # Transformacao de dados
# # df_est<- group_by(up_spp, establishment) %>%  #Grouping data
# #   summarise(N=length(unique(search.str))) %>% # Species richness
# #   mutate(perc = proportions(N)*100) %>%  #Creating % column
# #   arrange(desc(N)) #Reordering species by number
# # 
# # up_spp[up_spp$establishment %in% "Naturalizada",]
# # #n life forms
# # length(df_est$N)
# # #n especies
# # sum(df_est$N)
# # 
# # 
# # #Plotar grafico de familias
# # p2 <- ggplot(df_est, aes(y=reorder(establishment,N),x=N))+
# #   geom_bar(stat="identity", col="black", fill="grey", alpha=1)+
# #   theme_bw(base_family = "Cambria", base_size = 12 )+
# #   theme(axis.text = element_text(colour = "black"), axis.title = element_text(colour ="gray10", face = "bold"),
# #         axis.text.y = element_text(colour = "black", size = 12),
# #         title = element_text(size = 9))+
# #   scale_x_continuous(breaks = seq(0, 80, 10), limits = c(0,50))+
# #   labs(y="Origem", x="Número de espécies", title = "Estabelecimento das espécies")
# # p2
# # #Save graph    
# # ggsave(plot = p2,"figs/est_graph_ggplot2.jpg", dpi = 300, width =170 , height = 100, units = "mm")  
# # 
# # 
# # p1+p2+plot_annotation(tag_level = "A", tag_suffix = ")")
# # 
# # ggsave("figs/result_ggplot.jpg", dpi = 300, width =170 , height = 100, units = "mm")  
# 
# ### Para mais infos sobre o ggplot acesse é possivel acessar os seguintes tutoriais:
# ##> https://ggplot2.tidyverse.org/articles/ggplot2.html
# ##> https://opencodecom.net/post/2021-08-22-introducao-ao-ggplot2/
# ##> https://rpubs.com/jarrais/GET00183_Topico3
# 



# Adjusting final  list ----------------------------------------------
## Add family and other data
flor_data$Família <- up_spp$family #adicionar o nome das familias
flor_data$Espécie <- up_spp$search.str #nome corrigido das sp
flor_data$Espécie_Original <-up_spp$original.search #adicionar uma coluna com os nomes originais
flor_data$Estabelecimento <- up_spp$establishment #adicionar dados de estabelecimento

## Add endemism
flor_data$Endemicas <-up_spp$endemism

## Add protected sp
flor_data$Protegidas_BR <- up_spp$threat.status
flor_data$Protegidas_RS <- NA
flor_data$Protegidas_RS [grep("Ficus", flor_data$Espécie)] <- "Imune"
flor_data$Protegidas_RS [grep("Erythrina", flor_data$Espécie)] <- "Imune"
view(flor_data)




flor_data <- arrange(flor_data, Família) #Organizar alfabeticamente a planilha

view(flor_data)

final_data<- list("Florística"=flor_data
                 # , "Riqueza por Família"= df_family
                 # , "Riqueza por Estabelecimento" = df_est
                 )

write.xlsx(final_data, file = "outputs/floristic_up.xlsx")


### Crossing with local red list
# am_rs <- read_xlsx(path="data/ameacadas_rs.xlsx", sheet = "Ameacadas") %>% arrange(`NOME CIENTÍFICO`)
# quase_am <- read_xlsx(path="data/ameacadas_rs.xlsx", sheet = "Quase ameacadas") %>% arrange(`NOME CIENTÍFICO`)
# 
# summary(flor_data$Espécie %in% am_rs$`NOME CIENTÍFICO`)
# summary(flor_data$Espécie %in% quase_am$`NOME CIENTÍFICO`)
# 
# flor_data$Protegidas_RS[flor_data$Espécie %in% quase_am$`NOME CIENTÍFICO`] <- "NT"
# am_rs[am_rs$`NOME CIENTÍFICO`%in% flor_data$Espécie, ]
# 
# 
# flor_data$Protegidas_RS[flor_data$Espécie %in% am_rs$`NOME CIENTÍFICO`] <- 
#   am_rs$CATEGORIA[am_rs$`NOME CIENTÍFICO` %in% flor_data$Espécie ]
# 
# am_rs[am_rs$`NOME CIENTÍFICO` %in% flor_data$Espécie,]
# flor_data$Espécie[flor_data$Espécie %in% am_rs$`NOME CIENTÍFICO` ]
# 
# 
# view(flor_data)
# protec_list <- dplyr::select(flor_data, Família, Espécie, Endemicas, Protegidas_RS, Protegidas_BR) %>% 
#   filter(!is.na(Endemicas) | !is.na(Protegidas_BR)| !is.na(Protegidas_RS))
# 
# final_data <- list(flor_data, protec_list) %>% `names<-`(c("Floristic", "Protected species"))
# write_xlsx(final_data, path = "outputs/floristic_up.xlsx")


