################################################################################
################# INTRODUÇÃO À ANÁLISE TÉCNICA DE AÇÕES COM R ##################
################################################################################

# Faremos uma análise de alguns indicadores técnicos para uma ação
# A empresa da B3 escolhida foi a ENJOEI
# É uma empresa brasileira de comércio eletrônico 

# Carregando bibliotecas
library("quantmod") # extração de dados das ações / geração de índices
library("TTR") # funções de análise ténica para ações

# importando os dados de uma ação
# passamos o ticker (abreviatura do nome da ação), fonte dos dados, e o período
# pegamos os dados de 2021 até o dia da realização da análise
getSymbols("ENJU3.SA", src='yahoo', from = '2021-01-01', to = '2022-12-08')

length(ENJU3.SA) # São 2886 valores
head(ENJU3.SA) # visualizando as 5 primeiras linhas

# alterand os nomes das colunas
# "open": abetura
# "high": valor mais alto
# "low": valor mais baixo
# "close": fechamento
# "volume": volume de negociação
# "ajusted": valor ajustado
names(ENJU3.SA)<-c("open","high","low","close", "volume","ajusted")
head(ENJU3.SA)

# verificando se há algum valor na
sum(is.na(ENJU3.SA)==TRUE) # não há

# verificando a periodicidade dos dados
periodicity(ENJU3.SA) # diário
class(ENJU3.SA) # objeto de série temporal

# Gráfico da variação percentual entre os fechamentos
plot(ClCl(ENJU3.SA))

# plotando gráficos básicos para cada coluna
plot(Cl(ENJU3.SA), col="blue",lwd =2)

# valores mínimo e máximo da série
seriesLo(ENJU3.SA) 
seriesHi(ENJU3.SA) 

# gráfico básico de candlesticks para o período selecionado com o volume
candleChart(ENJU3.SA, theme="white")

# gerando o gráfico mensal sem volume
# o parâmetro TA=NULL retira o gráfico de volume
chartSeries(to.monthly(ENJU3.SA), TA=NULL, theme = "white")

# gerando o gráfico semanal sem volume
chartSeries(to.weekly(ENJU3.SA),TA=NULL, theme = "white")

# gerando o gráfico por trimestre sem volume
chartSeries(to.quarterly(ENJU3.SA), TA=NULL, theme = "white")

# gráfico apenas para o ano de 2022
candleChart(ENJU3.SA['2022-01::'], theme="white")

# calculando os retornos para esta ação
dailyReturn(ENJU3.SA) # retorno diário da ação
weeklyReturn(ENJU3.SA) # retorno semanal
monthlyReturn(ENJU3.SA) # retorno mensal
yearlyReturn(ENJU3.SA) # retorno anual
allReturns(ENJU3.SA) # retorno de todo o período

# gráfico do retorno mensal da ação
plot(monthlyReturn(ENJU3.SA))

# Indicadores

# Os indicadores são classificados como rastreadores de tendência ou osciladores
# Os indicadores rastradores de tendência ajudam a identificá-la por meio da 
# construção de média móveis.
# Em mercados sem tendência os indicadores osciladores ajudam identificar 
# suporte e resistência

# Os indicadores são gerados de forma a empilhar no gráfico original
# caso queira remover os índices que já foram inseridos basta gerar o gráfico 
# original novamente

# MÉDIAS MÓVEIS (MM)

# As médias móveis podem ser: Simples, Ponderada e Exponencial
# A média móvel facilita visualizar a tendência suavizando os ruídos do gráfico
# A desvantagem da MM é o atraso 
# Esse atraso pode ser atenuado mudando o período da média

# plotando uma média móvel simples de 10 dias (em vermelho)
# e 50 dias (em azul) para o ano de 2022
# usamos o valor de fechamento
plot(ENJU3.SA['2022-01::']$close)
lines(SMA(ENJU3.SA['2022-01::']$close,n=10), col='red')
lines(SMA(ENJU3.SA['2022-01::']$close,n=50), col='blue')

# A MM Exponencial atribui um peso que cresce exponencialmente ao longo do tempo
# Para um ativo de baixa volatilidade pode-se utilizar a MM Simples
# Para ativos de alta volatilidade recomenda-se MM ponderada ou Exponencial

# plotando uma média móvel exponencial para os fechamentos do ano de 2022
plot(ENJU3.SA['2022-01::']$close)
lines(SMA(ENJU3.SA['2022-01::']$close, n = 10), col='red')

# Um primeiro indicador de compra ou venda é no cruzamento do preço com a MM
# se o preço cruzar a média de baixo para cima é um indicativo de compra
# se o preço cruzar a MM de cima para baixo é indicativo de venda

# Entretando, isto gera muitos alarmes falsos
# uma forma de contornar esse problema é analisando o cruzamento entre MM

# Se a média de menor período cruzar de baixo para cima a média de maior período 
# é um indicativo de compra

# se a média de menor período cruzar de cima para baixo a média de maior período
# é indicativo de venda

# Plotando várias Médias Móveis para o ano de 2022

# a melhor forma de comparar as médias móveis é com o gráfico de linhas
chartSeries(ENJU3.SA['2022::'],type="line", TA=NULL,theme="white")

# Acrescentando várias médias móveis em um gráfico
# utilizamos apenas o ano de 2022 pois melhora a visualização
# e sempre usando o valor de fechamento (Close)

# Média Móvel Simples de 26 peŕiodos
addSMA(n = 26,  with.col = Cl, overlay = TRUE, col = "brown")

# Média Móvel Ponderada de 26 dias
# o parâmetro on = 1 para imprimir no mesmo gráfico anterior
addWMA(n = 26 , on = 1, with.col = Cl, overlay = TRUE, col = "yellow")

# Média Móvel Exponencial Ponderada de 26 dias
addDEMA(n = 26, on = 1, with.col = Cl, overlay = TRUE, col = "pink")

# Média Módel Exponencial Ponderada considerando o volume para 26 dias
addEVWMA(n = 26, on = 1, with.col = Cl, overlay = TRUE, col = "purple")

# Média Móvel Exponencial de 26 dias
addZLEMA(n = 26, ratio=NULL, on = 1, with.col = Cl, overlay = TRUE, col = "red")

# colocando as lengendas: passamos a posição abaixo e à esquerda: "bottomleft"
# lty=1: tipo da linha
# gerar o vetor de cores com as mesmas cores que passei no gráfico
# bg="black": cor do fundo da legenda
# cor do texto da legenda
# cex=0.7: diminuir espaço da legenda
legend("bottomleft",lty=1,col=c("green","brown","yellow","pink","purple","red"), 
       legend=c("Dados","SMA","WMA","DEMA","EVWMA","ZLEMA"),bg="white",text.col="black", cex=0.6,text.width = 12)

# INDICADORES

# Indicador MACD (Moving Average Convergence Divergence)

# Este indicador é o resultado da diferença entre duas mm exponenciais de 12 e 26
# além disso há uma linha chamada de Sinal que é uma MM exponencial de 9 períodos
# então um possível indicativo de compra é quando a linha MACD cruzar de baixo 
# para cima a linha de Sinal
# e o indicativo de venda quando a linha MACD cruzar de cima para baixo a linha 
# de Sinal

# Temos que passar os períodos da MM rápida(fast), lenta (slow) e sinal (signal)
# passamos o tipo da média móvel, que nesse caso será exponencial
# podemos ou não gerar o histograma
candleChart(ENJU3.SA, theme="white", TA = NULL)
addMACD(fast = 12, slow = 26, signal = 9, type = "EMA", histogram = TRUE)

# Neste mesmo gráfico também aparece o Histogramde MACD
# esse histograma é a diferença entre a linha MACD e a linha de Sinal
# Interpretação do histograma:
# Histograma positivo com tendência descendente - opotunidade de venda
# Histograma negativo com tendência ascendente - oportunidade de compra

# Bandas de Bollinger

# As Bandas são formadas por três linhas:
# a linha central é uma MM Simples
# a linha (banda) superior é a linha central mais duas vezes o seu desvio-padrão
# a linha (banda) inferior é a linha central menos duas vezes o seu desvio-padrão

# o desvio-padrão representa o nível de volatilidade
# preços com mais volatilidade tem bandas maiores
# quando os preços saem dos limites, se espera que eles voltem  a uma média

# passamos o intervalo que vai adicionar as bandas; o desvio-padrão das bandas
# o tipo de média móvel, usaremos média móvel simples ("SMA"); 
# draw é o que ele vai plotar
# o parâmetro on = -1 é para plotar no mesmo gráfico principal de candlesticks
candleChart(ENJU3.SA['2022-01::'], theme="white", TA = NULL)
addBBands(n = 20, sd = 2, maType = "SMA", draw = 'bands', on = -1)

# quando o preço se afasta da banda superior:
# é indicativo de venda (compradores perdendo força)

# quando a barra de preço se afasta da banda inferior:
# é indicativo de compra (vendedores perdendo força)

# quando ocorre o estreitamento da banda:
# é forte sinalização de rompimento para cima ou para baixo

# Indicador de Momentum

# indica a velocidade com que os preços estão mudando
# mede se uma tendência está acelerando ou desacelerando
# ajuda a visualizar a persistência de uma tendência
candleChart(ENJU3.SA, theme="white", TA = NULL)
addMomentum(n=1)
