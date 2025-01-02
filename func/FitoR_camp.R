###> Função fitoR_camp calcula os parâmetros fitossociologicos para 
###> vegetação campestre.
#> 
####> Como utilizar:
#>  O arquivo de entrada precisa das seguintes colunas com os nomes exatos em minusculo:
#>	- parc (identificação das parcelas, numerico ou letra, tanto faz)
#>	- spp (nome vulgar ou científico do indivíduo)
#>	- cob (cobertura absoluta em % mensurada segundo alguma escala)
#>
####> Argumentos
#> A função pode ser usada da seguinte forma: 
#> fitoR(input_data, area_de_cada_parcela_em_m2, 'nome_do_arquivo'), ou seja,
#> fitoR(x, area, filename)
#>
#> x: um data frame contendo nas colunas as parcelas (parc), espécies (spp) e cobertura absoluta (cob);  
#> area: area das unidades amostrais em metros quadrados; 
#> filename: nome do arquivo de output dos resultados entre aspas simples ou duplas.
#>
#####> Valores 
#> Ao final do processo a função retorna uma lista composta por um resumo dos dados,
#> incluindo: número de unidades amostrais avaliadas, área total amostrada,
#> riqueza, diversidade de Shannon-Wiever (nats - calculada com base na cobertura)
#> e equitabilidade de Pielou.
#> 
#####> Exemplo
#>
#> data_camp <- read.csv(file="example_tab_camp.csv", sep=";", dec=",")
#> fitoR(x=data_camp, area=1, filename="my_result_camp")
#> 


fitoR_camp<-function(x, area, filename)
{
  matriz<-table(x$spp,x$parc)
  
  x[is.na(x)] <- 0
  
  #numero de parcelas
  nparc<-length(levels(as.factor(x$parc)))
  
  #area total amostrada
  area.parc=(area*nparc)
  
  
  #calcula frequencias 
  freq<-(if (length(dim(matriz)) > 1)
  {apply(matriz > 0,1,sum)} else sum(matriz > 0))
  FA<-(freq/nparc)*100
  FR<-(FA/sum(FA))*100
  
  #Cobertura
  
  CA<-tapply(x$cob, x$spp, sum)
  CR<-(CA/sum(CA))*100
  
  
  dim(CR)
  
  #calcula o indice de valor de importancia
  VI<-(CR+FR)/2
  
  #monta a tabela
  fito=data.frame(FA=FA,FR=FR, CA, CR,VI=VI)
  fito$FR<-round(fito$FR,digits=4)
  fito$FA<-round(fito$FA,digits=4)
  fito$CA<-round(fito$CA,digits=4)
  fito$CR<-round(fito$CR,digits=4)
  fito$VI<-round(fito$VI,digits=4)
  fito <- fito[order(VI, decreasing = TRUE),]
  print(fito)
  
  #calcula os indices de diversidade
  Pi<-CA/sum(CA)
  Pi<-Pi*log(Pi)
  SW=-sum(Pi)
  S=nrow(fito)
  J=SW/log(S)
  
  cat("Numero de parcelas = ",round(nparc,digits=2),"parcelas", fill=TRUE)
  cat("Area total amostrada = ", round(area.parc,digits=2),"m�", fill=TRUE)
  cat("Riqueza = ",S,"esp.", fill=TRUE)
  cat("Indice de Shannon-Wiener (H') = ",SW, fill=TRUE)
  cat("Equabilidade de Pielou (J) = ",J, fill=TRUE)
  
  
  if(!missing(filename)) filename = paste(filename, ".csv", sep="") else filename = 'fito.csv'
  write.table(fito, file = filename, row.names = TRUE, dec=",", sep=";", quote=FALSE, col.names=NA)
  write.table(' ', file = filename, sep=";", quote=TRUE, append=TRUE, row.names=FALSE, col.names=FALSE)
  cat("Numero de parcelas = ",round(nparc,digits=2),"parcelas", fill=TRUE, file=filename, append=TRUE)
  cat("Area total amostrada = ", round(area.parc,digits=2),"m²", fill=TRUE, file=filename, append=TRUE)
  cat("Riqueza = ",S,"spp.", fill=TRUE, file=filename, append=TRUE)
  cat("Indice de Shannon-Wiener (H') = ",SW, fill=TRUE, file=filename, append=TRUE)
  cat("Equabilidade de Pielou (J) = ",J, fill=TRUE, file=filename, append=TRUE)
  
  
}

