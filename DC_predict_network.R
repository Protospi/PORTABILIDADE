# ------------------------------------------------------------------------

# Curso Data Camp Predictive Nanalytics with R data

# ------------------------------------------------------------------------

# Carrega Pacotes
library(igraph)
library(tidyverse)

# ------------------------------------------------------------------------

# Carrega dados
load("DATA_CAMP/StudentEdgelist.RData")
load("DATA_CAMP/StudentCustomers.RData")
load("DATA_CAMP/StudentNetwork.RData")

# ------------------------------------------------------------------------

# Inspect edgeList
head(edgeList)

# Construct the igraph object
network <- graph_from_data_frame(edgeList, directed = FALSE)

# View your igraph object
network

# ------------------------------------------------------------------------

# Inspect the customers dataframe
head(customers)

# Count the number of churners and non-churners
table(customers$churn)

# Add a node attribute called churn
V(network)$churn <- customers$churn

# Visualize the network
plot(network, vertex.label = NA, edge.label = NA,
     edge.color = 'black', vertex.size = 2)

# ------------------------------------------------------------------------

# Add a node attribute called color
V(network)$color <- V(network)$churn

# Change the color of churners to red and non-churners to white
V(network)$color <- gsub("1", "red", V(network)$color) 
V(network)$color <- gsub("0", "white", V(network)$color)

# Plot the network
plot(network, vertex.label = NA, edge.label = NA,
     edge.color = "black", vertex.size = 2)

# ------------------------------------------------------------------------

# Create a subgraph with only churners
churnerNetwork <- induced_subgraph(network, 
                                   v = V(network)[which(V(network)$churn == 1)])

# Plot the churner network 
plot(churnerNetwork, vertex.label = NA, vertex.size = 2)

# ------------------------------------------------------------------------

# Declara ids de churns
churn_ids <- as.vector(customers[customers$churn == 1,]$id)

# Declara numchurn ids
numchur_ids <- as.vector(customers[customers$churn == 0,]$id)

# Define churn neighbors
ChurnNeighbors <- edgeList %>% 
                    mutate(from = as.numeric(from)) %>%
                    arrange(from) %>%
                    mutate(churned = ifelse(to %in% churn_ids, 1, 0),
                           nonchurned = ifelse(!to %in% churn_ids, 1, 0)) %>%
                    group_by(from) %>%
                    summarise(n_churn = sum(churned),
                              n_nonchurn = sum(nonchurned))

# Compute the churn probabilities
churnProb <- ChurnNeighbors$n_churn / (ChurnNeighbors$n_churn +
                                       ChurnNeighbors$n_nonchurn)

# Find who is most likely to churn
mostLikelyChurners <- which(churnProb == max(churnProb))

# Extract the IDs of the most likely churners
customers$id[mostLikelyChurners]

# ------------------------------------------------------------------------

# Add the column edgeList$FromLabel
edgeList$FromLabel <- customers[match(edgeList$from, customers$id), 2]

# Add the column edgeList$ToLabel
edgeList$ToLabel <- customers[match(edgeList$to, customers$id), 2]

# Add the column edgeList$edgeType
edgeList$edgeType <- edgeList$FromLabel + edgeList$ToLabel

# Count the number of each type of edge
table(edgeList$edgeType)

# ------------------------------------------------------------------------

# Count churn edges
ChurnEdges <- sum(edgeList$edgeType == 2)

# Count non-churn edges
NonChurnEdges <- sum(edgeList$edgeType == 0)

# Count mixed edges
MixedEdges <- sum(edgeList$edgeType == 1)

# Count all edges
edges <- ChurnEdges + NonChurnEdges + MixedEdges

#Print the number of edges
edges

# ------------------------------------------------------------------------

# Count the number of churn nodes
ChurnNodes <- sum(customers$churn == 1)

# Count the number of non-churn nodes
NonChurnNodes <- sum(customers$churn == 0)

# Count the total number of nodes
nodes <- ChurnNodes + NonChurnNodes

# Compute the network connectance
connectance <- 2 * edges / nodes / (nodes - 1)

# Print the value
connectance

# ------------------------------------------------------------------------

# Compute the expected churn dyadicity
ExpectedDyadChurn <- ChurnNodes * (ChurnNodes -1) * connectance / 2

# Compute the churn dyadicity
DyadChurn <- ChurnEdges / ExpectedDyadChurn

# Inspect the value
DyadChurn

# ------------------------------------------------------------------------

# Non churn diadicity
NonChurnEdges / ((NonChurnNodes * (NonChurnNodes-1) * connectance) / 2)

# ------------------------------------------------------------------------

# Compute the expected heterophilicity
ExpectedHet <- NonChurnNodes * ChurnNodes * connectance

# Compute the heterophilicity
Het <- MixedEdges / ExpectedHet

# Inspect the heterophilicity
Het

# ------------------------------------------------------------------------

# Extract network degree
V(network)$degree <- degree(network, normalized=TRUE)

# Extract 2.order network degree
degree2 <- neighborhood.size(network, 2)

# Normalize 2.order network degree
V(network)$degree2 <- degree2 / (length(V(network)) - 1)

# Extract number of triangles
V(network)$triangles <- count_triangles(network)

# ------------------------------------------------------------------------

# Extract the betweenness
V(network)$betweenness <- betweenness(network, normalized=TRUE)

# Extract the closeness
V(network)$closeness <- closeness(network, normalized=TRUE)

# Extract the eigenvector centrality
V(network)$eigenCentrality <- eigen_centrality(network, scale = TRUE)$vector

# ------------------------------------------------------------------------

# Extract the local transitivity
V(network)$transitivity <- transitivity(network, type="local", isolates='zero')

# Compute the network's transitivity
transitivity(network)

# ------------------------------------------------------------------------

# Extract the adjacency matrix
AdjacencyMatrix <- as_adjacency_matrix(network)

# Compute the second order matrix
SecondOrderMatrix_adj <- AdjacencyMatrix %*% AdjacencyMatrix

# Adjust the second order matrix
SecondOrderMatrix <- ((SecondOrderMatrix_adj) > 0) + 0
diag(SecondOrderMatrix) <- 0

# Inspect the second order matrix
SecondOrderMatrix[1:10, 1:10]

# ------------------------------------------------------------------------

# Compute V(network)$Churn
V(network)$Churn <- customers$churn

# Compute the number of churn neighbors
V(network)$ChurnNeighbors <- as.vector(AdjacencyMatrix %*% V(network)$Churn)

# Compute the number of non-churn neighbors
V(network)$NonChurnNeighbors <- as.vector(AdjacencyMatrix %*% (1 -
                                                               V(network)$Churn))

# Compute the relational neighbor probability
V(network)$RelationalNeighbor <- as.vector(V(network)$ChurnNeighbors / 
                                             (V(network)$ChurnNeighbors +
                                                V(network)$NonChurnNeighbors))

# ------------------------------------------------------------------------

# Compute the number of churners in the second order neighborhood
V(network)$ChurnNeighbors2 <- as.vector(SecondOrderMatrix %*% V(network)$Churn)

# Compute the number of non-churners in the second order neighborhood
V(network)$NonChurnNeighbors2 <- as.vector(SecondOrderMatrix %*% (1 - 
                                                                  V(network)$Churn))

# Compute the relational neighbor probability in the second order neighborhood
V(network)$RelationalNeighbor2 <- as.vector(V(network)$ChurnNeighbors2 / 
                                              (V(network)$ChurnNeighbors2 +
                                                 V(network)$NonChurnNeighbors2))

# ------------------------------------------------------------------------

# Extract the average degree of neighboring nodes
V(network)$averageDegree <- 
  as.vector(AdjacencyMatrix %*% V(network)$degree) / degree(network)

# Extract the average number of triangles of neighboring nodes
V(network)$averageTriangles <- 
  as.vector(AdjacencyMatrix %*% V(network)$triangles) / degree(network)

# Extract the average transitivity of neighboring nodes    
V(network)$averageTransitivity<-
  as.vector(AdjacencyMatrix %*% V(network)$transitivity) / degree(network)

# Extract the average betweenness of neighboring nodes    
V(network)$averageBetweenness <- 
  as.vector(AdjacencyMatrix %*% V(network)$betweenness) / degree(network)

# ------------------------------------------------------------------------

# Extract the dataset
studentnetworkdata_full <- igraph::as_data_frame(network, what = "vertices")

# Inspect the dataset
head(studentnetworkdata_full)

# Remove customers who already churned
studentnetworkdata_filtered <- studentnetworkdata_full[-which(studentnetworkdata_full$Churn == 1), ]

# Remove useless columns
studentnetworkdata <- studentnetworkdata_filtered[, -c(1, 2, 9)]

# ------------------------------------------------------------------------

# Inspect the feature
summary(studentnetworkdata$RelationalNeighbor2)

# ------------------------------------------------------------------------

# Remove the Future column from studentnetworkdata 
no_future <- studentnetworkdata[, -1]

# Load the corrplot package
library(corrplot)

# Generate the correlation matrix
M <- cor(no_future)

# Plot the correlations
corrplot(M, method = "circle")

# Print the column names
colnames(studentnetworkdata)

# Create toRemove
toRemove <- c(10, 13, 19, 22)

# Remove the columns
studentnetworkdata_no_corrs <- studentnetworkdata[, -toRemove]

# ------------------------------------------------------------------------

# Adiciona vetor de Futuro vetor
studentnetworkdata <- studentnetworkdata_full

# Set the seed
set.seed(7)

# Create the index vector
index_train <- sample(1:nrow(studentnetworkdata), 2 / 3 * nrow(studentnetworkdata))

# Make the training set
training_set <- studentnetworkdata[index_train, ]

# Make the test set
test_set <- studentnetworkdata[-index_train, ]

# ------------------------------------------------------------------------

# Make firstModel
firstModel <- glm(Churn ~ degree + degree2 + triangles + betweenness +
                  closeness + transitivity,
                  family = "binomial",
                  data = training_set[, -c(1,2,3)])

# ------------------------------------------------------------------------

# Build the model
secondModel <- glm(Churn ~ ChurnNeighbors +
                     RelationalNeighbor +
                     ChurnNeighbors2 +
                     RelationalNeighbor2 + 
                     averageDegree +
                     averageTriangles +
                     averageTransitivity +
                     averageBetweenness, 
                   family = "binomial",
                   data = training_set[, -c(1,2,3)])

# ------------------------------------------------------------------------

# Build the model
thirdModel <- glm(Churn ~ ., 
                   family = "binomial",
                   data = training_set[, -c(1,2,3)])


# ------------------------------------------------------------------------

# Load package
library(randomForest)

# Set seed
set.seed(863)

# Build model
rfModel <- randomForest(as.factor(Churn)~. ,data=training_set[, -c(1,2,3)])

# Plot variable importance
varImpPlot(rfModel)

# ------------------------------------------------------------------------

# Load the package
library(pROC)

# Predict with the first model
firstPredictions <- predict(firstModel,
                            newdata = test_set[, -c(1,2,3)],
                            type = "response")

# Predict with the second model
secondPredictions <- predict(secondModel,
                             newdata = test_set[, -c(1,2,3)],
                             type = "response")

# Predict with the third model
thirdPredictions <- predict(thirdModel,
                            newdata = test_set[, -c(1,2,3)],
                            type = "response")

# Predict with the rfModel
rfPredictions<- predict(rfModel,
                        newdata = test_set[, -c(1,2,3)],
                        type= "prob")

# ------------------------------------------------------------------------

# Calculate auc
auc(test_set$churn, firstPredictions)

# Calculate auc
auc(test_set$churn, secondPredictions)

# Calculate auc
auc(test_set$churn, thirdPredictions)

# Calculate auc
auc(test_set$churn, rfPredictions[,2])

# ------------------------------------------------------------------------

# Carrega pacote
library(lift)

# Calculate Top Decile
TopDecileLift(test_set$churn, firstPredictions)

# Calculate Top Decile
TopDecileLift(test_set$churn, secondPredictions)

# Calculate Top Decile
TopDecileLift(test_set$churn, thirdPredictions)

# Calculate Top Decile
TopDecileLift(test_set$churn, rfPredictions[,2])

# ------------------------------------------------------------------------

