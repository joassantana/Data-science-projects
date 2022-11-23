# Bibliotecas:
library(tidyverse) # pacote para manipulacao de dados
library(cluster) # algoritmo de cluster
library(dendextend) # comparação de dendogramas
library(factoextra) # algoritmo de cluster e visualização
library(fpc) # algoritmo de cluster e visualização
library(gridExtra) # para a função grid arrange

# definindo o diretório de trabalho
setwd('/home/joas/Data-science-projects/Análise de Cluster')

################################################################################
####################        CLUSTER HIERARQUICO          #######################        
################################################################################

### LEITURA DOS DADOS ###

# separado por vígula (sep = ","); com cabeçalho (header = T)
municipios <- read.table("dados/municipios-pe.csv", sep = ",", header = T)
head(municipios) # visualizando as primeiras linhas

# transformando a primeira coluna em índice de linhas
rownames(municipios) <- municipios[,1]
municipios <- municipios[,-1] # excluindo a primeira coluna

# separando os municípios da RMR
RMR <- municipios[c('Recife','Abreu e Lima','Araçoiaba',
                    'Cabo de Santo Agostinho','Camaragibe','Goiana', 'Igarassu',
                    'Ilha de Itamaracá', 'Ipojuca', 'Itapissuma',
                    'Jaboatão dos Guararapes', 'Moreno', 'Olinda','Paulista',
                    'São Lourenço da Mata'),]
RMR <- data.frame(RMR) # salvando em um dataframe

#Padronizar variaveis (é necessário pois tem variável em escalas diferentes)
RMR.padronizado <- scale(RMR)

#CALCULANDO MATRIZ DE DISTANCIAS
matriz_distancia_RMR <- dist(RMR.padronizado, method = "euclidean")

# DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
ch1 <- hclust(matriz_distancia_RMR, method = "single" )
ch2 <- hclust(matriz_distancia_RMR, method = "complete" )
ch3 <- hclust(matriz_distancia_RMR, method = "average" )
ch4 <- hclust(matriz_distancia_RMR, method = "ward.D" )

#DESENHANDO O DENDOGRAMA
plot(ch1, cex = 0.6, hang = -1)
plot(ch2, cex = 0.6, hang = -1)
plot(ch3, cex = 0.6, hang = -1)
plot(ch4, cex = 0.6, hang = -1)

# separando em 4 clusters
rect.hclust(ch4, k = 4)

# COMPARANDO DENDOGRAMAS
# comparando o método single com complete
dend_ch1 <- as.dendrogram(ch1)
dend_ch2 <- as.dendrogram(ch2)
dendogramas <- dendlist(dend_ch1, dend_ch2) # guardando em uma lista
#EMARANHADO
tanglegram(dend_ch1, dend_ch2, main = paste("Emaranhado =", 
                                            round(entanglement(dendogramas),2)))

# DEFININDO O NÚMERO DE CLUSTERS COM MÉTODO ELBOW
# passamos os dados padronizados;
# o tipo de agrupamento (hcut - método hierárquico); 
# e o método de estimação (wss = Weighted Sum of Squares)
fviz_nbclust(RMR, FUN = hcut, method = "wss")

# vamos salvar os grupos cortados
grupo_municipios <- cutree(ch4, k = 4) # cortando ch4 em 4 grupos
table(grupo_municipios)

#transformando em data frame a saida do cluster
grupo_municipios <- data.frame(grupo_municipios)

#juntando com a base original
base_municipios_fim <- cbind(RMR, grupo_municipios)

# montando uma tabela com a média de cada variável para cada grupo formado
media_grupo_RMR <- base_municipios_fim %>% 
  group_by(grupo_municipios) %>% 
  summarise(qtde_municipios = n(), # número de indivíduos de cada grupo
            populacao_media = mean(população), 
            escolarizacao_media = mean(escolarizacao),
            idhm_medio = mean(idhm),
            receita_media = mean(receitas),
            pib_percapita_medio = mean(pib_percapita),
            salario_medio_mensal = mean(salario_medio_mensal))
media_grupo_RMR

### Análise para todos os municípios de PE ###

# padronizando as variáveis
municipios.padronizado <- scale(municipios)

# calculando a matriz de distância utilizando o método euclidiano
matriz_distancia_municipios <- dist(municipios.padronizado,method = "euclidean")

# analisando o número de grupos pelo método Elbow
fviz_nbclust(municipios.padronizado, FUN = hcut, method = "wss") # 7 grupos

# definindo o cluster apenas para os métodos single e ward.
hc1 <- hclust(matriz_distancia_municipios, method = "single" )
hc4 <- hclust(matriz_distancia_municipios, method = "ward.D")

# dendogramas
plot(hc1, cex = 0.6, hang = -1)
plot(hc4, cex = 0.6, hang = -1)
rect.hclust(hc4, k = 7) # cortando o último dendograma em 7 grupos

grupo_municipios_total <- cutree(hc4, k = 7) # cortando hc14 em 4 grupos
table(grupo_municipios_total)

################################################################################
##################         MÉTODO NÃO HIERARQUICO         ######################        
################################################################################

### k-means ###

# Calcular o Cluster - utilizamos os dados padronizados
RMR.cluster <- kmeans(RMR.padronizado, centers = 4)

# Visualizando os clusters
fviz_cluster(RMR.cluster,geom="point",data = RMR.padronizado) + ggtitle("k = 4")

# o gráfico retorna duas dimensões, Dim1 e Dim2
# Dim1 está pegando 41.8% da variabilidade dos dados
# Dim2 está pegando 32.5% da variabilidade dos dados
# se a área de algum cluster está grande e com muitas observações 
# é porque há grande variabilidade de dados podendo, assim, criar outros grupos
# podemos testar com mais centróides caso seja necessário

# criando mais clusters - testando com mais grupos
RMR.cluster2 <- kmeans(RMR.padronizado, centers = 2)
RMR.cluster3<- kmeans(RMR.padronizado, centers = 3)
RMR.cluster5 <- kmeans(RMR.padronizado, centers = 5)

# criando e salvando os gráficos
G1<-fviz_cluster(RMR.cluster2,geom = "point",
                 data=RMR.padronizado)+ggtitle("k = 2")
G2<-fviz_cluster(RMR.cluster3,geom = "point",  
                 data = RMR.padronizado) + ggtitle("k = 3")
G3<-fviz_cluster(RMR.cluster,geom = "point",  
                 data = RMR.padronizado) + ggtitle("k = 4")
G4<-fviz_cluster(RMR.cluster5,geom = "point",  
                 data = RMR.padronizado) + ggtitle("k = 5")

# Imprimindo os gráficos em uma mesma figura
grid.arrange(G1, G2, G3, G4, nrow = 2)

# nesta perspectiva podemos ter noção de como estão sendo criados os grupos

# verificando a quantidade de grupos pelo método Elbow
fviz_nbclust(RMR.padronizado, kmeans, method = "wss")

### Método k-means para todos os municípios de PE ###

# verificando a quantidade de grupos pelo método Elbow
fviz_nbclust(municipios.padronizado, FUN = kmeans, method = "wss") # 6 grupos

# Calcular o Cluster - utilizamos os dados padronizados
municipios.cluster <- kmeans(municipios.padronizado, centers = 6)

# Visualizando os clusters
fviz_cluster(municipios.cluster, geom = "point", 
             data = municipios.padronizado) + ggtitle("k = 6")
# observamos a sobreposição de vários grupos

# Dim1 está pegando 27.3% da variabilidade dos dados
# Dim2 está pegando 16% da variabilidade dos dados

# criando mais clusters - testando com mais grupos
municipios.cluster4<- kmeans(municipios.padronizado, centers = 4)
municipios.cluster5 <- kmeans(municipios.padronizado, centers = 5)
municipios.cluster7 <- kmeans(municipios.padronizado, centers = 7)

# criando e salvando os gráficos
G1 <- fviz_cluster(municipios.cluster4, geom = "point", 
                   data = municipios.padronizado) + ggtitle("k = ")
G2 <- fviz_cluster(municipios.cluster5, geom = "point",
                   data = municipios.padronizado) + ggtitle("k = 5")
G3 <- fviz_cluster(municipios.cluster, geom = "point",
                   data = municipios.padronizado) + ggtitle("k = 6")
G4 <- fviz_cluster(municipios.cluster7, geom = "point", 
                   data = municipios.padronizado) + ggtitle("k = 7")

# Imprimindo os gráficos em uma mesma figura
grid.arrange(G1, G2, G3, G4, nrow = 2)

### DBSCAN ###

# Calcular o Cluster - o parâmetro eps é a largura da borda
# nesse caso colocamos eps=1 de maneira subjetiva, o ideal é ir ajustando 
# MinPts é o mínimo de pontos dentro do círculo, 
# colocamos MinPts=3 para ser o mínimo para formar um grupo
# esse método é muito sensível a esses parâmetros
dbscan <- fpc::dbscan(RMR.padronizado,eps = 1, MinPts = 3)

# para sabermos onde cada observação está em cada grupo
base_municipios_fim$dbscan <- dbscan$cluster

#visualizando em cores os clusters
base_municipios_fim %>% ggplot() +
  geom_point(aes(x = idhm,
                 y = pib_percapita,
                 color = as.factor(dbscan)),
             size = 3)
