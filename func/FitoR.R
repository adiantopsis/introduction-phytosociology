#-----------------------------------------------------------------------------------------------------------------------#
###> Função para cálculo dos descritores fitossociológicos e ajustes das 
###> planilhas de dados fitossociológicos.
###> Grande parte dessa função é uma adaptação da função fitoR oriunda de
###> https://github.com/ricds/fitoR, produzida por: Alexandre Gabriel Christo, 
###> Pedro Higuchi, Ricardo Dal'Agnol, Arthur Vinicius Rodrigues
#>
####> Como utilizar:
#>  O arquivo de entrada precisa das seguintes colunas com os nomes exatos em minusculo:
#>	- parc (identificação das parcelas, numerico ou letra, tanto faz)
#>	- spp (nome vulgar ou científico do indivíduo)
#>	- dap (diâmetro a altura do peito em cm)
#>	ou
#>	- cap (circunferência a altura do peito em cm)
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
#> fitoR(variavel_input, area_de_cada_parcela_em_m2,tipo_de_VI, 'nome_do_arquivo'), ou seja,
#> fitoR(x, area, VI, filename)
#>
#> x: um data frame contendo nas colunas as parcelas (parc), espécies (spp), altura (H), diametros (dap) ou circunferências (cap);  
#> area: area das unidades amostrais em metros quadrados; 
#> VI: tipo de valor de importancia, escolha entre "cottam" para a soma dos parâmetros relativizados ou 
#> "percent" (default) para retornar a média dos parâmetros relativizados.
#> filename: nome do arquivo de output dos resultados entre aspas simples ou duplas.
#>
#####> Valores 
#> Ao final do processo a função retorna uma lista composta por um resumo dos dados,
#> incluindo: area basal (m²/ha), densidade (ind./ha), riqueza 
#> (-1, i.e., sem espécimes mortos inclusos nos dados brutos),
#> diversidade de Shannon-Wiever (nats) e equitabilidade de Pielou.
#> 
#####> Exemplo
#>
#> data <- read.csv(file="example_tab.csv", sep=";", dec=",")
#> fitoR(x=data, area=100, VI="cottam", filename="my_result")
#>

#-----------------------------------------------------------------------------------------------------------------------#

#####> Função fitoR <######
fitoR<-function(x,area,filename, VI)
{
 if(missing(VI)){VI<-"cottam"} else {VI<-VI}
matriz<-table(x$spp,x$parc)

#numero de parcelas
nparc<-length(levels(as.factor(x$parc)))

#area total amostrada
area.parc=(area*nparc)

#densidade
dta=length(x$spp)/(area.parc/10000)

#desvio da densidade entre parcelas
dtadesv = 0
dtai = 1
vetor=1
while(dtai <= nparc)
{
length(vetor) <- nparc
vetor[dtai] = sum(matriz[,dtai])
dtadesv = sd(vetor)/(area/10000)
dtai = dtai + 1
}

#calcula o numero de ind amostrados
N<-apply(matriz,1,sum)

#calcula densidades
DA<-apply(matriz,1,sum)/(area.parc/10000)
DR<-DA/sum(DA)*100

#calcula frequencias 
freq<-(if (length(dim(matriz)) > 1)
 {apply(matriz > 0,1,sum)} else sum(matriz > 0))
FA<-(freq/nparc)*100
FR<-(FA/sum(FA))*100

#checa por NAs nos dados e transforma em zeros
x[is.na(x)] <- 0

#determina se existe "caps" ou "daps" e quais colunas estão
cols = grep('cap', colnames(x))
ncols = length(cols)
if (ncols>0) param="cap"

cols2 = grep('dap', colnames(x))
ncols2 = length(cols2)
if (ncols2>0) param="dap"

if (param=="dap") cols=cols2
if (param=="dap") ncols=ncols2

#calcula a area da seção transversal para cada cap/dap e faz a soma por individuo
i=1
x$areasec=0
while (i<=ncols)
{
if (param=="cap") x$areasec<-x$areasec+((pi*(x[,cols[i]]/pi)^2)/40000)
if (param=="dap") x$areasec<-x$areasec+((pi*x[,cols[i]]^2)/40000)
i=i+1
}

#calcula as dominancias
DoA<-tapply(x$areasec, x$spp, sum)/(area.parc/10000)
DoR<-DoA/sum(DoA) *100
 
# area basal por espécie
AB<-tapply(x$areasec, x$spp, sum)

#area basal
abta=sum(DoA)
 
#desvio da area basal entre parcelas
somag<-tapply(x$areasec, x$parc, sum)/(area/10000)
abdesv = sd(somag)

#calcula o indice de valor de importancia
if (VI=="cottam") {VI<-(DR+DoR+FR)} 
else {if(VI=="percent"){VI<-(DR+DoR+FR)/3}}

#monta a tabela
fito=data.frame(N=N,AB=AB,DA=DA,DR=DR,DoA=DoA,DoR=DoR,FA=FA,FR=FR,VI=VI)
#fito$AB<-round(fito$AB,digits=5)
#fito$DR<-round(fito$DR,digits=5)
#fito$DA<-round(fito$DA,digits=5)
#fito$FR<-round(fito$FR,digits=5)
#fito$FA<-round(fito$FA,digits=5)
#fito$DoR<-round(fito$DoR,digits=5)
#fito$DoA<-round(fito$DoA,digits=5)
#fito$VI<-round(fito$VI,digits=5)
fito <- fito[order(VI, decreasing = TRUE),]

#calcula os indices de diversidade
Pi<-N/sum(N)
Pi<-Pi*log(Pi)
SW=-sum(Pi)
S=nrow(fito)-1
J=SW/log(S)

p<- rbind("Densidade total" = dta,
"Area basal" = round(abta,digits=2),
Riqueza = S,
Diversidade = SW,
Equitabilidade = J
)



l <- list(Resumo=p,
Fitossociologia = fito)

return(l)

cat("Densidade total por hectare = ",dta,"\u00B1",round(dtadesv,digits=2),"ind/ha", fill=TRUE)
cat("Área basal total por hectare = ",round(abta,digits=2),"\u00B1",round(abdesv,digits=2),"m2/ha", fill=TRUE)
cat("Riqueza = ",S,"sp.", fill=TRUE)
cat("Índice de Shannon-Wiener (H') = ",SW, fill=TRUE)
cat("Equabilidade de Pielou (J) = ",J, fill=TRUE)

if(!missing(filename)) filename = paste(filename, ".csv", sep="") else filename = 'fito.csv'
write.table(fito, file = filename, row.names = TRUE, dec=",", sep=";", quote=FALSE, col.names=NA)
write.table(' ', file = filename, sep=";", quote=TRUE, append=TRUE, row.names=FALSE, col.names=FALSE)
cat("Densidade total por hectare = ",round(dta,digits=2),"\u00B1",round(dtadesv,digits=2),"ind/ha", fill=TRUE, file=filename, append=TRUE)
cat("Área basal total por hectare = ",round(abta,digits=2),"\u00B1",round(abdesv,digits=2),"m2/ha", fill=TRUE, file=filename, append=TRUE)
cat("Riqueza = ",S,"esp.", fill=TRUE, file=filename, append=TRUE)
cat("Índice de Shannon-Wiener (H') = ",SW, fill=TRUE, file=filename, append=TRUE)
cat("Equabilidade de Pielou (J) = ",J, fill=TRUE, file=filename, append=TRUE)

}

#------------------------------------------------------------------------------#
####> Função DAPeq <######
####> Descrição: 
#> Mensura o diâmetro equivalente para plantas com fuste 
#> multifurcados.
#> 
####> Argumentos:
#>
#> dap_eq (x)
#> 
#> x: um data frame contendo nas colunas as parcelas (parc), espécies (spp), 
#> altura (H), diametros (dap) ou circunferências (cap); 
#> ff: fator forma, se ausente será utilizado 0.5;
#> 
####> Valores:
#> Ao fim do processo a função retorna um novo data.frame com o diâmetro equivalente
#> e os volumes das plantas, calculados com base no fator forma informado.
#>
#####> Exemplo: 
#> data <- read.csv(file="example_tab.csv", sep=";", dec=",")
#> dap.eq (x=data, ff=0.5)
#> 

dap.eq<- function (x, ff){ 
  df<-x
  if(missing(ff)) { ff <- 0.5 } else {ff <- ff}
  DAP<-grep("dap", colnames(df))
  if(length(DAP)>1){
    
    df[is.na(df)]<-0
    col_dap<-grep("dap", colnames(df))
    df$dap_eq<-sqrt(rowSums(df[,col_dap]^2))
    g<-((pi*(df$dap_eq^2))/40000)
    g1<-g*df$H*ff
    df$vm3<-g1
    return(df[,-col_dap])
  } else {df<-df
  g<-((pi*(df$dap_eq^2))/40000)
  g1<-g*df[,grep("H", colnames(df))]*ff
  df$vm3<-g1
  return(df[,-col_dap])}
}


