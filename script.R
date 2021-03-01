# ----------------------------------------------------------------------------------------------

# Rascunho do Projeto Grafos de Portabilidade dos alunos do Data Camp

# ----------------------------------------------------------------------------------------------

# Carrega Pacotes
library(igraph)
library(tidyverse)
library(tidyverse)

# ----------------------------------------------------------------------------------------------

# Carrega dados
load("StudentEdgelist.RData")
load("StudentCustomers.RData")
load("StudentNetwork.RData")

# Declara dados
arestas <- edgeList
clientes <- customers

# Imprime 5 amostras do dataframe de arestas
head(arestas)

# Imprime 5 amostras do dataframe de vertices
head(clientes)

# ----------------------------------------------------------------------------------------------

# Inspeciona numero de arestas
nrow(arestas)

# Inspeciona numero de vertices
nrow(clientes)

# Tabela de numero de churns
table(clientes$churn)

# ----------------------------------------------------------------------------------------------

# Constroi rede
rede <- graph_from_data_frame(arestas, directed = FALSE)

# Conta numero de arestas
ecount(rede)

# Conta numero de vertices
vcount(rede)

# Verifica se o grafo é simples
is.simple(rede)

# Verifica se a rede e conectada (cada vertice esta ao alcance de qualquer outro vertices)
is.connected(rede)

# Calcula grau da rede ou numero de arestas incidentes
graus <- degree(rede)

# Calcula numero de componentes (subgrafos conectados maximamente)
n_cluster <- clusters(rede)$no

# Calcula maior distancia no grafo (distancia geodesica ou menor caminho)
diametro <- diameter(rede)

# ----------------------------------------------------------------------------------------------

# Adiciona atributo de churn a rede
V(rede)$churn <- clientes$churn

# Adiciona cor ao vertice de churn
V(rede)$color <- V(rede)$churn

# Muda cor de churners
V(rede)$color <- gsub("1", "green", V(rede)$color) 
V(rede)$color <- gsub("0", "blue", V(rede)$color)

# Desenha grafo
plot(rede,
     vertex.label = NA,
     edge.label = NA,
     edge.color = "black",
     vertex.size = 2)

# ----------------------------------------------------------------------------------------------

# Gera subgrafo de clientes que optaram pela portabilidade
rede_portabilidade <- induced_subgraph(rede, v = V(rede)[which(V(rede)$churn == 1)])

# Imprime grafico 
plot(rede_portados,
     vertex.label = NA,
     vertex.size = 3,
     edge.size = 4,
     edge.color = "black",
     vertex.color = "green")

# ----------------------------------------------------------------------------------------------

# Declara vetor de ids de clientes que optaram pela portabilidade
portabilidade_ids <- as.vector(clientes[clientes$churn == 1,]$id)

# Declara vetor de ids dos clientes que optaram pela portabilidade
nao_portabilidade_ids <- as.vector(clientes[clientes$churn == 0,]$id)

# Define vizinhanca
vizinhanca <- arestas %>% 
                  mutate(from = as.numeric(from)) %>%
                  arrange(from) %>%
                  mutate(saiu = ifelse(to %in% portabilidade_ids, 1, 0),
                         ficou = ifelse(!to %in% portabilidade_ids, 1, 0)) %>%
                  group_by(from) %>%
                  summarise(n_saiu = sum(saiu),
                            n_ficou = sum(ficou),
                            perc_saiu = (n_saiu / (n_saiu + n_ficou)) * 100,
                            perc_ficou = (n_ficou / (n_saiu + n_ficou)) * 100,
                            perc_saiu_maior_50 = ifelse(perc_saiu >= 100, 1, 0))

# Extrai ids de clientes com maior probabilidade de portabilidade
ids_risco <- vizinhanca %>%
                filter(perc_saiu_maior_50 == 1) %>% 
                inner_join(clientes %>%  mutate(id = as.double(id)),
                           by = c("from" = "id"))

# ----------------------------------------------------------------------------------------------

# Extrai graus
V(rede)$degree <- degree(rede, normalized=TRUE)

# Extrai graus de ordem 2
graus2 <- neighborhood.size(rede, 2)

# Normaliza
V(rede)$graus2 <- graus2 / (length(V(rede)) - 1)

# Extrai numero de triangulos
V(rede)$triangles <- count_triangles(rede)

# Extrai mediacao
V(rede)$mediacao <- betweenness(rede, normalized=TRUE)

# Extrai proximidade
V(rede)$proximidade <- closeness(rede, normalized=TRUE)

# Extrai autovalor de centralidade
V(rede)$autoCentralidade <- eigen_centrality(rede, scale = TRUE)$vector

# Extrai transitividade local
V(rede)$trnasitividade <- transitivity(rede, type="local", isolates='zero')

# Calcula transitividade geral da rede
transitivity(rede)

# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------------