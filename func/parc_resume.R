####> Funçao para resumir os dados de inventário florestal por unidade amostral
####> Para rodar essa funçao depende dos pacotes "reshape", "tidyverse" e "vegan"

####> Como utilizar:
#>  O arquivo de entrada precisa das seguintes colunas com os nomes exatos em minusculo:
#>	- parc (identificação das parcelas, numerico ou letra, tanto faz)
#>	- spp (nome vulgar ou científico do indivíduo)
#>	- dap (diâmetro a altura do peito em cm)
#>	ou
#>	- cap (circunferência a altura do peito em cm)
#>  - H (altura em metros)
#>
#>	É aconselhavel que se utilize colunas diferentes para indivíduos com fuste
#>      multifurcado, com nome: cap ou dap1, dap2, dap3, dap4, etc. Ou então, se utilize
#>      o diâmetro equivalente. Logo, NÃO separe os fustes dos indivíduos em linhas.
#>
#>	OBS: o script buscará pelas palavras-chave 'cap' e 'dap' nas colunas para localiza-los.
#>	Portanto, os dados não podem possuir nenhuma coluna extra com 'cap' e 'dap' contidos em
#>	seu nome, ou causará diferença no cálculo, por ex.: uma coluna chamada 'dap_medio'.
#>
####> Argumentos
#> A função pode ser usada da seguinte forma:
#> parc.resume(variavel_input), ou seja,
#> parc.resume(data)
#>
#> data: um data frame contendo nas colunas as parcelas (parc), espécies (spp), altura (H), diametros (dap) ou circunferências (cap);
#>
#####> Valores
#> Ao final do processo a função retorna uma lista composta por um resumo dos dados
#> por unidade amostral: abundancia, riqueza (aconselhavel remover espécimes mortos para não contar aqui),
#> diversidade de Shannon-Wiever (nats), média e desvio padrão da altura (m), média e desvio padrão de DAP ou CAP (cm),
#> equitabilidade de Pielou e área basal (m²).
#>
#####> Exemplo
#>
#> data <- read.csv(file="example_tab.csv", sep=";", dec=",")
#> parc.resume(data=data)


parc.resume <- function(data){
  require(tidyverse)
  require(reshape)
  require(vegan)
  cols = grep('cap', colnames(data))
  ncols = length(cols)
  if (ncols>0) param="cap"
  
  cols2 = grep('dap', colnames(data))
  ncols2 = length(cols2)
  if (ncols2>0) param="dap"
  
  if (param=="dap") cols=cols2
  if (param=="dap") ncols=ncols2
  
  
  if (param=="cap") {data[,cols] <- data[,cols]/3.14 
  colnames(data)[cols] <- paste0(rep("dap", ncols), seq(1, ncols, 1))}  else {
  data <- data }

  
  
  #Riqueza e abundancia
  data_clean<-data[-c(which(data$spp=="Morta")),]
  data_raw<-data
  ab<-cast(data=data_clean, parc~spp, value = "H", length)
  freq<-ab
  freq[freq>0]<-1
  
  S<-apply(freq, 1, sum)#Riqueza por UA
  N<-apply(ab, 1, sum)#Numero de individuos sem as mortas
  
  #Diversidade e equitatividade
  div<-diversity(ab, index = "shannon", MARGIN = 1)#diversidade de shannon
  J<-div/log(S) #equitatividade de pielou
  
  #Diametro
  N_raw<-cast(data=data_raw, parc~spp, value = "H", length)%>%apply(., 1, sum) #Numero de individuos incluindo as mortas
  df<-data.frame(data_raw)
  df[is.na(df)]<-0
  DAP<-grep("dap", colnames(df))
  len<-ifelse(df[,DAP]>0,1,0)
  l<-apply(len, 1, sum)
  q<-which(l>1)
  df[q,DAP]<-df[q,DAP]^2
  df$dap_eq <- df [,DAP[1]]
  df$dap_eq[q]<-sqrt(apply(df[q,DAP], 1, sum))
  
  df<-cbind.data.frame(parc=df$parc,spp=df$spp, H=df$H, dap_eq=df$dap_eq)
  par_sum<-tapply(df$dap_eq, df$parc, sum)
  mean_dap<-tapply(df$dap_eq, df$parc, mean)
  sd_dap<-tapply(df$dap_eq, df$parc, sd)
  
  #Area basal
  df$g<-(pi*df$dap_eq^2)/40000
  AB<-tapply(df$g, df$parc, sum)
  
  #Altura

  mean_h<-tapply(data_raw$H, data_raw$parc, mean)
  
  sd_h<-tapply(data_raw$H, data_raw$parc, sd)
  
  #Data Frame final
  Parc_res=cbind.data.frame(N, S, div, mean_h, sd_h, mean_dap, sd_dap,J, AB)
  print(Parc_res)
  return(list(Resume=Parc_res, Input=df))
}
