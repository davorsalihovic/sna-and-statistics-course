# 4. MATRICES AND NETWORK MEASURES ------------
#    Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be

# Clean the R environment
rm(list = ls())

library(netseg); library(statnet); library(ITNr); library(xUCINET); library(igraph)
library(ggplot2); library(dplyr); library(tidyr)

# Load your data first
load("data2.RData")


#### 4.1 GRAPH DATA AS A MATRIX ----
# Let's create a matrix representing friendships from a simulated network
par(mfrow = c(1, 1))
set.seed(5)
friendships <- sample_smallworld(1, 10, 3, 0.3)
V(friendships)$name <- LETTERS[1:10] # we give the first ten letters of the alphabet as names

igraph_options(vertex.label = V(friendships)$name, vertex.label.cex = 2, vertex.size = 0, 
               edge.color = "grey", edge.width = 1, vertex.color = "black",
               edge.arrow.size = .5)
plot(friendships)

# Creating and multiplying matrices of non-directed graphs
  # Meaning that the matrices will be symmetrical
friend.matrix <- as.matrix(as_adjacency_matrix(friendships))
friend.matrix

friend.matrix %*% friend.matrix # Multiplying a matrix, meaning that we square it, returns a matrix
  # where (i, j)th entry contains the number of walks between i and j that are of length 2

# Another way of interpreting this is that (i, j)th entry contains the number of i's neighbors that also have j as neighbor

library(expm) # We need the "expm" library to exponentiate beyond the second degree
friend.matrix %^% 3
  # Similarly, a matrix to the power of 3 returns the number of walks of length 3, etc.

# Creating and multiplying matrices of directed graphs
# Here, matrices will NOT be symmetrical!
set.seed(5)
friends.directed <- as_directed(friendships, mode = "random")  # Create a simulated directed network, where edges are randomly directed
plot(friends.directed)

friends.dir.mat <- as.matrix(get.adjacency(friends.directed))
friends.dir.mat %*% friends.dir.mat # Similarly, returns the number of paths of length two, but along the direction of the edge.
  # Here the direction of the edge matters!
friends.dir.mat %*% t(friends.dir.mat) # Number of common targets - outgoing ties (the diagonal is just the out-degree) 
t(friends.dir.mat) %*% friends.dir.mat # Number of common sources - incoming ties (the diagonal is just the in-degree)

# Adjacency matrix of our "relations" data
g.mat <- as.matrix(as_adjacency_matrix(g))
class(g.mat)

View(g.mat) # given that our graph is a directed graph, the matrix is not symmetrical

View(g.mat %*% t(g.mat))
View(t(g.mat) %*% g.mat) # Here it becomes rather impractical to track the values

# But we can turn them into a more readable format, say an edgelist
# We first create a graph where the weight (a value attached to edges) represents the number of common targets
g.weight <- graph_from_adjacency_matrix(g.mat %*% t(g.mat), 
                                        mode = "undirected", diag = F, weighted = T) # we add the weight to capture the number of shared targets

g.weight.el <- cbind(as.data.frame(as_edgelist(g.weight)), weight = E(g.weight)$weight)
  # Note, however, that here we do not have weights of value zero!

# We can, however, arrive to this via the simple "get.data.frame" function
g.weight.el <- get.data.frame(g.weight)
# Again, note that we have no zeros here, as we are converting an adjacency matrix into an edgelist data frame;
sum(g.weigth.el$weight == 0)

# Let's plot our new data in a meaningful way, so that we can observe potential groupings
g.weight <- delete_vertices(g.weight, V(g.weight)[degree(g.weight) == 0])

layout.g.weight <- layout_with_mds(g.weight, distances(g.weight, weights = E(g.weight)$weight))

plot(g.weight, vertex.label = V(g.weight)$name, vertex.label.cex = .8, 
     vertex.label.dist = 1, vertex.label.color = "black", 
     vertex.color = "lightblue", vertex.size = 2,
     edge.width = E(g.weight)$weight, edge.color = "grey", 
     layout = layout.g.weight)

# Here, then, we have a new insight into our data, where we plot nodes so that their proximity 
  # and the width of the edge represents the number of shared targets
  # The closer the nodes are, the more similar in their choosing the targets


#### 4.2 TRANSFORMING DATA ----

## DICHOTOMIZING THE MATRIX: turning whatever values our edges may have into either 1 or 0 ##

# Let's try creating a binary matrix (0 or 1), based on whether nodes share more than 10 common targets or not
  # in our original data
binary.matrix <- g.mat %*% t(g.mat)
binary.matrix <- 1 * (binary.matrix > 10) # There are many ways of telling the computer to put 1 where the original number is bigger than 10
  # Here we choose to multiply 1 by TRUE or FALSE values, i.e. by 1 or 0, respectively
  # We state 1 * whatever value comes out once we evaluate whether each entry in the matrix is (1) or is not (0) larger than 10

# Let's create a new graph from this matrix
binary.graph <- graph_from_adjacency_matrix(binary.matrix, mode = "undirected", diag = F) 
  # we do not want the diagonal of the matrix to be included
    # i.e. we are leaving self-loops out

binary.graph <- igraph::delete_vertices(binary.graph, V(binary.graph)[igraph::degree(binary.graph) == 0])
# We want to declutter our graph, so we delete all vertices that will be left without any edges
# i.e. those that share no more than 10 targets with anyone.

plot(binary.graph, vertex.label = V(binary.graph)$name, vertex.label.cex = 1.2, vertex.size = 1,
     vertex.color = "lightblue")
  # Now we have a graph of people who share more than 10 common targets in their correspondence
    # Note that this is now a conceptually different relation than what we originally started with
      # and one that in fact may not make a lot of sense.


## TRANSPOSING A MATRIX ##
# In effect rotating a matrix 90 degrees anti-clockwise, so that columns become rows.
friends.dir.mat
t(friends.dir.mat) # we simply apply the t() function for "transpose"


## SYMMETRIZING A MATRIX ##
# Directed matrices are usually not symmetrical (they may be, but then this beats the purpose of ever having a directed matrix), 
    # i.e. the ties tend not to be reciprocated
# In symmetric matrices, ties are reciprocated, so the upper and lower halves of the matrix are literally mirror images - they are symmetric

friend.matrix # this is a symmetric matrix, meaning that all ties are reciprocated
# (i, j) is repeated in (j, i)

# One half of the matrix, in other words, is redundant:
friend.matrix.lower <- friend.matrix # let's make a copy to play with
friend.matrix.lower[upper.tri(friend.matrix.lower)] <- 0 # we set one triangle of the matrix to be all zeros, 
                                                          # just to demonstrate that the two sides are informationally the same
friend.matrix; friend.matrix.lower # and indeed they are

# If we now plot the two matrices, we get the same results, 
    # as only one half of the matrix contains enough information when we have undirected, symmetric data.
par(mfrow = c(1, 2)) # set our plot to have place for two images in one row
set.seed(5) # use the same seed to get the same layout
plot(graph_from_adjacency_matrix(friend.matrix, mode = "undirected"))
set.seed(5)
plot(graph_from_adjacency_matrix(friend.matrix.lower, mode = "undirected"))


friends.dir.mat # This, however, is a directed matrix, and here both triangles matter 
  # as ties are not (necessarily) reciprocated
par(mfrow = c(1, 1))

# We can symmetrize our matrices, so that all edges are reciprocated
friends.sym.mat <- sna::symmetrize(friends.dir.mat)
friends.sym.mat # and what we get when we symmetrize is our original "friend.matrix":
friend.matrix == friends.sym.mat


#### 4.3 NODE-LEVEL MEASURES AND CENTRALITY ----

### DEGREE ###
# Degree is simply the number of adjacent nodes
# In directed networks, given outgoing and incoming ties, one can investigate 
  # out- and in-degrees, respectively

igraph::degree(g, loops = F, mode = "in") # In-degree
igraph::degree(g, loops = F, mode = "out") # Out-degree
degree_distribution(g, mode = "in") # The relative frequency of each degree in the data

# We can plot the frequencies of each degree:
par(mfrow = c(1, 1))
hist(igraph::degree(g, loops = F, mode = "in"), # Here we decide to plot the in-degree 
     xlim = c(0, 60), ylim = c(0, 250),
     xlab = "Node Degree", ylab = "Frequency",
     main = "Degree Distribution")
  
  # Or, slightly more complicated, but with more options:
dd <- as.data.frame(degree_distribution(g, mode = "in")) %>%
  mutate(in_degree = 1:nrow(dd))
names(dd) <- c("freq", "in_degree")

  # Simple
plot(dd$freq ~ dd$in_degree, type = "l")
lines(mfit)

  # With ggplot
plot_dd <- ggplot(data = dd, aes(x = in_degree, y = freq)) +
  geom_line(color = "red", lwd = 1, alpha = .4) +
  geom_point() +
  labs(x = "In-degree", y = "Frequency") +
  theme_classic()
plot_dd


### STRENGTH ###
# Strength is the sum of weights of edges incident to a given node
g.weight # remember, edges represent the number of shared targets and this is a non-directed graph
strength(g.weight, loops = F) # this returns the strengths of ties for each node
hist(strength(g.weight, loops = F), # And we can, for instance, similarly plot the frequencies of strengths in a histogram
     xlim = c(0, 600), ylim = c(0, 25),
     xlab = "Strength of Ties",
     main = "Distribution of the Strength of Ties")


### HOMOPHILIY ###
# Homophily is the tendency, observed in many social structures, for people to associate (or otherwise establish links) with similar others
# Similarity can be of any sort and measured on any variable
library(netseg)

# There are various measures of homophily or assortativity that approach the issue 
  # slightly differently in the conceptual sense, as well as offer different values

# Consult the documentation on each of these and the literature to learn more 
  # about what they mean and represent before applying them in research

## Krackhard and Stern's E-I index: returns -1 when ties are homophilous, 1 when ties are heterophilous
g.simple <- simplify(g)
ei(g.simple, "sex", directed = T) # -0.23, indicating a slight homophily with respect to sex

# One can also calculate the individual E-I indices with functions from the "ITNr" package
library(ITNr)
eid <- ei_ind(g.simple, "sex")

ggplot(data = eid, aes(x = group, y = EI)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  theme_classic() +
  labs(x = "Sex", y = "E-I Index")


## Odds ratio of within-group ties
orwg(g.simple, "sex")
  # The odds ratio of 1.47 shows that there is a slight preference for in-group ties 
    # (if there were none, the ratio would be 1)


## Coleman's index, particularly useful in investigating the behavior of particular groups in the data
coleman(g.simple, "sex")
  # It returns a number between -1 and 1 (just like the correlation coefficient), 
      # indicating whether all ties from a node are sent to the opposite group or the same group, respectively.
  # Both groups in our network seem to be leaning towards corresponding with men more than women.


## Assortativity Coefficient
# Nominal - by sex (or any sort of categorical variable)
assortativity_nominal(g.simple, as.factor(V(g.simple)$sex))
  # This measures whether all ties are within-group ties, in which case the result is 1.
  # The coefficient is interpreted in a way similar to correlation coefficients: 
  # Our coefficient is close to zero, indicating that the network is neither really assortative, nor dissasortative.

# Degree assortativity is one type of assortativity measured on a numerical variable
# Degree assortavity is useful in detecting networks of preferential attachment
assortativity_degree(g.simple, directed = T)
  # The coefficient of -0.16 indicates a slightly dissasortative network
  # Meaning that overall nodes of different degree send ties to each other.


# Like most measures of homophily, assortativity too can be applied to any sort of attribute
# Here we try measuring assortativity on component membership
  # We first give our nodes the attribute of component membership:
V(g.simple)$component <- igraph::components(g.simple)$membership

  # # And then calculate the coefficient. This must be 1. Try to answer why.
assortativity(g.simple, V(g.simple)$component)
  

### NODE SIMILARITY WITH RESPECT TO NEIGHBORHOOD ###

## Jaccard and similar types of coefficients tell us how similar any two nodes are with respect to their immediate neighbors
  # I.e. what proportion of the nodes they are connected to are the same
  # See the documentation for the igraph "similarity" function to learn about further types of measures
    # and what exactly - in mathematical terms - each of them mean
similarity(g.simple, mode = "out", method = "jaccard") 
# This returns the matrix of Jaccard similarities with respect to outgoing ties
# Jaccard similarity, the basis of measures we examine further on, is mathematically the similarity between sets:
  # It is the size of the intersection of the two sets divided by the size of their union:
  # In other words, how many elements they share divided by the total number of elements in both sets.

## A more sophisticated measure, one that gives more importance to shared nodes that are less popular in the overall network
  # is the inverse log-weighted similarity
node.similarity <- similarity(g.simple, mode = "out", method = "invlogweighted", loops = F)
# This returns a matrix of similarities
colnames(node.similarity) <- V(g.simple)$name # Let's change the row and column headers to names of our nodes
rownames(node.similarity) <- V(g.simple)$name

node.similarity["P0053", "P0035"]
# This is then the number of common neighbors weighted by the neighbors' degree, meaning that 
  # common neighbors of smaller degree (less central, less chosen neighbors) are deemed more important
  # in defining the relationship/similarity of two nodes at interest

# We can demonstrate how the inverse log weighted similarity works - follow the equation to understand what is going on
ego.graphs.53.35 <- make_ego_graph(g.simple, order = 1, mode = "out", nodes = c("P0053", "P0035"))
  # Let's measure the similarity between our two nodes "P0053" and "P0035"
  # And to do this we need to find nodes both of them share. 
    # We do this by intersecting their ego-graphs:
intersection(ego.graphs.53.35[[1]], ego.graphs.53.35[[2]])
  # We find that they share (both send ties to - note the "out" mode) nodes "P0040", "P0032", and "P0055".
  # We now need to find the in-degrees of these ties, to find out how "popular" they are
igraph::degree(g.simple, v = c("P0040", "P0032", "P0055"), mode = "in")
# And we sum the reciprocals of the natural logarithms of their degrees
(1/log(22)) + (1/log(7)) + (1/log(20)) # We get the same result of 1.17


### GEODESIC DISTANCE ###

# Geodesic distance between any pair of nodes is the length of the shortest path between them
plot(friendships)
distances(friendships) # The "distances" function from igraph returns the matrix of geodesic distances
geodist(friend.matrix) # The output of this function from the sna package shows both the distances between vertices, and the number of occurrence of such distances
all_shortest_paths(friendships, from = "A", to = "B") # Returns the list of ALL shortest paths between two specified nodes
mean_distance(friendships) # Returns the mean geodesic distance of the entire network - this is one way to investigate, say, the cohesiveness of the network
  # If the mean geodesic distance is small, this is an indication of a closely knitted group


### CENTRALITY ###

## DEGREE CENTRALITY ##
# This is simply one's degree, and depends solely on one's immediate contacts
  # Therefore, degree centrality is a rudimentary measure of node centrality, as it disregards the structure
    # of the network beyond one's immediate neighborhood
igraph::degree(g.simple, mode = "in")

# Degree, in case of directed networks, can be either in- or out-degree - the number of ties received or sent - or both
igraph::degree(g.simple, mode = "out")


## EIGENVECTOR CENTRALITY ##
# A more sophisticated take on the nodes' centralities is eigenvector centrality.
# Eigenvector centrality measures the centrality of a conditional on the centrality of its neighbors, 
  # which depends on the centrality of their neighbors, which depends..., and so on.

# Measures of eigenvector centrality cannot handle multiple disconnected components well
# Therefore, it is usually best to first filter our data so that we consider only the largest component as one connected graph
large.component <- induced_subgraph(g.simple, V(g.simple)[V(g.simple)$component == "3"])
plot(large.component, vertex.label = NA, vertex.size = 2, 
     edge.arrow.size = .1)

# And we calculate the eigenvector centrality:
eigen_centrality(large.component, directed = T)
# This function returns the eigenvector centrality measure for each node as well as some other information
# The most important among this other data is the eigenvalue (8.13 in this case), which is the largest - but not the only - eigenvalue
# The largest eigenvalue is traditionally used in calculating the eigenvector centrality


## BETA (or POWER) CENTRALITY ## 
# Beta centrality in a sense conceptually generalizes the degree and the eigenvector centrality
# It works on the principle of exponentiating adjacency matrices X^k (which results in the number of k walks between (i, j)),
  # and on weighing the lengths of walks by some constant factor Beta chosen by the researcher.
  # What this means is that the longer the walk, the smaller its importance in the overall centrality of the node

# When Beta comes close to 1/eigenvalue, this measure comes close to eigenvector centrality

# Showing the principle of calculating the Beta Centrality by hand should demonstrate the logic behind it
# The equation governing beta centrality runs as follows: Beta.centrality = X + beta*X^2 + beta^2*X^3 + ... + beta^k-1*X^k
beta.hand <- (friend.matrix %*% friend.matrix) * 0.16 # this is the adjacency matrix squared times the Beta we chose (i.e the beta*X^2)
beta.hand1 <- (friend.matrix %^% 3) * 0.16^2 # this is the adjacency matrix cubed times the Beta squared (i.e. the beta^2*X^3)
beta.hand2 <- (friend.matrix %^% 4) * 0.16^3 # and so on...

beta.matrix <- (beta.hand + beta.hand1 + beta.hand2 + friend.matrix) # So, beta centrality is the sum of all of these values plus the original matrix
diag(beta.matrix) = 0

(rowSums(beta.matrix)/sum(rowSums(beta.matrix)))*10 # we need some adjustments to get to visually appealing values

# If we then use the igraph function "power_centrality" which calculates these measures, 
  # we get the same results (the slight difference is due to the fact that we have only gone three steps,
  # while the function goes into infinity)
power_centrality(friendships, exponent = .16)

# Another advantage of Beta/Power centrality is that the interpretation of the "power" of each node
  # depends on whether the k exponent is positive or negative
  # If positive, then the node is more powerful as its neighbors get more powerful...
  # If negative, the node is more powerful the weaker its neighbors are...

# The conceptual background here is that either the fewer neighbors your neighbors have, 
  # you have greater power over controlling your neighbors
# Or, in the positive sense, the more neighbors your neighbors have, 
  # the more powerful you are in, say, recruiting your considerable social resources, etc.


## CLOSENESS CENTRALITY ## 
# Defined as the sum of geodesic distances from a node to all other nodes in the network
  # Better understood as "farness" in this sense, as large numbers mean smaller "closeness" of a particular node

par(mfrow = c(1, 1))
plot(friendships)

# The sum of geodesic distances, which is something we can get, for each node, by summing the rows of the matrix
# of geodesic distances, since the rows indicate the distance from the row node (i) to the column node (j)

sum(geodist(friend.matrix)$gdist[1, ]) # for node A (i.e. row 1) in our friendship network
sum(geodist(friend.matrix)$gdist[2, ]) # for node B (i.e. row 2), etc.

# An easier way to do this by hand is:
geodesics <- geodist(friend.matrix)$gdist # Get the matrix of geodesic distances
rownames(geodesics) <- V(friendships)$name # Attach the proper names to rows
rowSums(geodesics) # And sum the rows

# And an easier way still would be to use the "distances" function from igraph, rather than "geodist" from sna:
rowSums(distances(friendships))

# There is also a designated function that does this for us:
igraph::closeness(friendships, cutoff = -1) # Note that this function has many further arguments
# We can denote whether we wish "out" distance, "in" distance or some other measure, depending on whether the network is directed
# This function, furthermore, returns the inverse of geodesic distance
# By using the inverse we evade the issue of infinite distance in unconnected networks
# If we had unconnected nodes, the distance between them would be infinite, which can be conceptually and mathematically quite tricky
# However, the inverse of "infinity" approaches ZERO (for instance, 1 divided by 1 billion is 1e-9, or 0,00000001), and this is how we deal with infinite distances


## K-STEP REACH CENTRALITY ##
# K-step reach centrality counts how many nodes can any one node reach in k number of steps
# Steps mean edges.
ego_size(friendships, order = 1)-1 # Raw number of alters, i.e. those nodes can reach in 1 step (note that we subtract 1, as ego_size function includes ego)
ego_size(friendships, order = 2)-1 # The number of nodes they can reach in 2 steps.
  # You will notice that they can all reach 9 alters.
  # Since there are 10 nodes altogether, this network is, of course, quite compact

# We can express this in proportions, if we divide the number of alters by all possible alters:
(ego_size(friendships, order = 1)-1)/(vcount(friendships)-1)
(ego_size(friendships, order = 2)-1)/(vcount(friendships)-1) # Here then, all nodes can reach 100% of other nodes.


# NOTE that with both closeness and reach centrality, directed graphs can pose problems
# Some nodes may not be connected at all!


## BETWEENNESS CENTRALITY ##

# This is a measure of how often a node falls on the shortest path between other two nodes
# The measure is a sum of proportions of the time that the node of interest falls on shortest paths, across all pairs in the graph
plot(friendships)
igraph::betweenness(friendships, cutoff = -1)


#### 4.4 GROUP AND DYAD-LEVEL MEASURES ----

### DENSITY ###
# Density refers to the proportion of existing ties in the total number of possible ties in the network
# That is, how many of the possible ties have actually been established

# The number of possible ties naturally differs in directed and undirected networks.
# For directed networks, it is  n(n-1), and (n(n-1))/2 for undirected, n being the number of nodes in the network

# Undirected network:
ecount(friendships)/((vcount(friendships)*(vcount(friendships)-1)/2)) # 0.66, so quite a dense network
# Or simply use:
edge_density(friendships)

# Directed network
ecount(g.simple)/(vcount(g.simple)*(vcount(g.simple)-1)) # 0.011, not so dense
edge_density(g.simple, loops = F)


### RECIPROCITY ###
# A measure that gives the proportion of reciprocated edges in a directed graph. 
  # (A binary, undirected graph contains only reciprocated edges by definition).
# The most common way of interpreting this is as: 
  # the sum of the element-wise product of the matrix and its transpose divided by the total number of edges,
  # which thus gives the proportion of reciprocated edges

# So, in mathematical terms, we can calculate the reciprocity as follows:
simple.matrix <- as.matrix(get.adjacency(g.simple)) # create the adjacency matrix of the network we are interested in
diag(simple.matrix) <- 0 # delete all loops

(sum(simple.matrix * t(simple.matrix))/ecount(g.simple))
# We first calculate the element-wise product of the matrix and its transpose
# What this means should be immediately apparent: If i sends tie to j, then entry (i, j) = 1; of entry (j, i) is also 1, the product matrix will contain 1, otherwise 0
  # In other words, the product matrix will have 1s only where there are reciprocated ties, and 0s elsewhere

# We lastly divide the sum of the product matrix by the number of all edges in the network.
  # In other words, we divide the number of reciprocated by the number of existing ties to get the proportion of reciprocated ties.
  # In this case, it's 0.2, or 20%.

# The function "reciprocity" from igraph does the same job:
reciprocity(g.simple) # 0.2, or 20%.

# A conceptually different measure is the so-called "ratio":
reciprocity(g.simple, mode = "ratio") # 0.11
# Here we sum the size of the groups of nodes that are not connected and the size of the group of those connected with non-reciprocated ties
  # and then divide the size of the group with reciprocated ties by this sum.
# This is, then, the proportion of the number of nodes with reciprocated ties in the total number of nodes connected in any other way and not connected at all

# In this sense, reciprocity also has a probabilistic aspect in that it will be considerably higher or considerably lower than density 
  # if there is or is no tendency towards reciprocating edges, respectively.
# This is related to the fact that we may interpret reciprocity as a probability that a tie in the other direction exists, 
  # given that one tie is already there.


### TRANSITIVITY ###
# Transitivity describes the tendency of the network to have "transitive triples" (in directed networks) or "triadic closure" in undirected
  # This is the case when i -> j, j -> k, i -> k
# In terms of probabilities, this we can interpret as a probability that there is an (i, k) edge, given that the two other edges already exist
  # Similar to reciprocity, this will be close to density if there is no force pushing towards transitivity in the network
transitivity(g.simple) # "global", or the entire graph
transitivity(friendships, type = "local") # "local", or each vertex


### COMPONENTS AND CONNECTEDNESS ###
# A component is a MAXIMAL set of nodes in which every node can reach every other node by some path
  # "Maximal" here refers to the fact that it cannot be extended by including further nodes:
  # A (maximal) component must include all nodes that satisfy the condition that all are connected by some path.

# Components, in addition to, say, density or average geodesic distance, can be used to reflect the cohesiveness in the network

# One way of finding components in networks is with using igraph functions:
igraph::components(g.simple) # five (weak) components, the largest comprising 254 nodes
igraph::components(friendships)

# In case of directed networks, we can distinguish between WEAK and STRONG components.
# Weak components disregard the direction of the tie, and strong do not.
# So, members of the same strong component will only be those nodes that can reach each other by some directed path.


## COMPONENT RATIO ##
# This ratio is the number of components in the network divided by the number of possible ties
  # What this means is that if the number of components is the same as the number of nodes, i.e. each node is its own component, disconnected from any other node,
    # then this number will be 1. If, on the other hand, there is only 1 large component, the equation will return 0.
# The result can go from 0 (everybody in one component) to 1 (everybody their own component)
  # This is, therefore, also a good measure of network cohesiveness.
n.of.components <- igraph::components(g.simple)$no
n.of.nodes <- vcount(g.simple)

# Following the equation: (c-1)/(n-1), we get:
(n.of.components-1)/(n.of.nodes-1) # The result, the component ratio, is 0.014

# One can also use the following function from xUCINET, which returns the same result
xComponents(as.matrix(get.adjacency(g.simple)), Type = "weak")

# Or, if you feel brave enough, you can write your own functions.
  # Let's write a function that returns the number of components, the number of nodes, and the component ratio
comp.ratio <- function(x) {
  library(igraph)
  c <- igraph::components(x)$no - 1
  n <- vcount(x) - 1
  return(c(c + 1, n + 1, c/n))
}

# Let's use our "comp.ratio" function to calculate the component ratios:
comp.ratio(g.simple) # We get the correct answer: 0.014


## CONNECTEDNESS ##
# Connectedness takes into consideration both the size of the components and the number of components to arrive at a measure of cohesiveness
# This is simply the proportion of pairs of nodes (dyads) that can reach each other by any path,
  # which is to say: the proportion of the pairs of nodes that are in the same component

# Another way of interpreting this is that connectedness is the same as the density of the reachability matrix
# And reachability matrices are matrices that indicate which two nodes can reach each other by some path.
# Note the distinction between this and the adjacency matrix, which indicates only DIRECT connections.
sna::reachability(friend.matrix) # This is the reachability matrix, here indicating that everybody can reach everybody, 
  # so we have 1 big component

# Following the equation and satisfying its constraints, we calculate connectedness in our large data set as follows:
reach <- sna::reachability(as.matrix(get.adjacency(g.simple)))
diag(reach) = 0 # With this, we satisfy the "i != j" part of the equation
sum(reach)/(vcount(g.simple)*(vcount(g.simple)-1)) # we get 0.189, or 18.9% of nodes can reach each other in the directed matrix
# Note that here too we respect the direction of the edge

# We can also write a little function to calculate this more quickly
connect <- function(x) {
  library(sna)
  library(igraph)
  r <- sna::reachability(as.matrix(get.adjacency(x)))
  diag(r) <- 0
  result <- sum(r)/(vcount(x)*(vcount(x)-1))
  return(result)
}

connect(g.simple) # we get the same 0.189
connect(as_undirected(g.simple)) # we can also give it undirected graphs
connect(friendships) # For our friendships network, we get 1, as everybody, remember, is in the same component

# Or one can use the function from xUCINET, which requires a matrix rather than graphs:
xConnectedness(as.matrix(get.adjacency(g.simple)))


### GEODESIC DISTANCE AND RECIPROCAL GEODESIC DISTANCE ###
# As already mentioned, another intuitive way of measuring cohesiveness in a network is to use the average geodesic distance
# The logic behind this is that if geodesic distances are small (everybody is relatively close together), then the network is quite closely knit

# However, as we know, geodesic distances can be infinite, if a node cannot be reached 
mean(distances(g.simple, mode = "in")) # Here, for instance, we get "Inf" which is a nonsensical result
  # To tackle this issue, we again return to inverses of geodesic distances, i.e. 1/geodesic distance

# A measure that is built around this idea of inverse distances is COMPACTNESS
# This is the quotient of the sum of reciprocated distances and the number of possible ties in the network
# So, it will be 0 of every node is an isolate, and 1 if every node is directly connected to every other node

# We can again write a little function:
compact <- function(x) {
  library(igraph)
  if (is_directed(x)) {
    i <- 1/distances(x, mode = "out")
    diag(i) <- 0
    n <- vcount(x)*(vcount(x)-1)
    return(sum(i)/n)
  } else {
    i <- 1/distances(x)
    diag(i) <- 0
    n <- (vcount(x)*(vcount(x)-1))
    return(sum(i)/n)
  }
}

compact(friendships) # for our friendship network, the compactness is 0.83
compact(g.simple) # and for our large, directed network, it's only 0.0749.
# Note here that we only take outgoing directed edges into consideration,
  # as per the formula in our function
# We can, though, use the versatility of our function to transform our directed data into undirected:
compact(as_undirected(g.simple)) # Now the compactness is 0.295

# Or, again, one can use xUCINET:
xCompactness(as.matrix(get.adjacency(g.simple)))


#### 4.5 COHESIVE SUBGROUPS ----

### CLIQUES ###

# A clique is a MAXIMAL COMPLETE SUBGRAPH, i.e. a subgraph in which every node is adjacent to every other node
  # be it a directed or non-directed graph
# Note the difference between a clique and a component. In the latter, nodes need not be directly connected.

# A clique must have at least three nodes (by convention): dyads and single nodes are not considered cliques
set.seed(1234)
plot(friendships)

cliques(friendships, min = 3) # This igraph command returns a list of NON-MAXIMAL cliques in the network
# Even a network as small as "friendships" has dozens of NON-MAXIMAL cliques
  # Returns all complete subgraphs (not maximal complete subgraphs!)
    # Therefore all cliques that may not necessarily be maximal.

max_cliques(friendships, min = 3) # This igraph function, on the other hand, does find all MAXIMAL complete subgraphs
  # We, then, have far fewer maximal cliques (since non-maximal subgraphs are not really cliques by definition)

# Note that these methods disregard the direction of the edge!

largest_cliques(g.simple) # This igraph function returns the largest maximal clique

# The xUCINET way:
xCliquesMembership(friend.matrix) # An actor x clique matrix for the maximal subgraphs


## CLIQUE OVERLAP ##
# Cliques can (and most certainly will) overlap - share their members
# xUCINET has functions to deal with such problems:
xCliquesCoMembership(friend.matrix, Min = 3) # returns a proximity-like actor-by-actor matrix that counts how many times i & j share a clique
xCliquesOverlap(friend.matrix, Min = 3) # A similar matrix, but a clique-by-clique, counting the number of nodes they share


### COMMUNITY DETECTION ###
# Given that we can define cliques in a formal way - as maximal complete subgraphs - we can rely on
  # algorithms to help us distinguish cliques in a graph

# There are several algorithms designed for this purpose (and one can design their own, if none of the 
  # existing ones match our theoretical approach)


## GIRVAN-NEWMAN ALGORITHM ##
# Girvan-Newman algorithm relies on iteratively removing those edges that have the highest betweenness.
# The algorithm proceeds either until it reaches the highest modularity or until it finds as many communities
  # as we define in advance.

# Modularity is simply the proportion of edges that exist within communities (i.e. edges within communities divided by the number of all edges),
  # minus the number of such edges that would exist in a purely random scenario within the communities.
# We will meet with random graphs again in sections on the inferential statistics of networks.

# igraph G-N algorithm
g.new <- cluster_edge_betweenness(g.simple, directed = F) # igraph way of running the algorithm
communities(g.new) # igraph community-detection functions return a "communities" object that needs to be treated with further functions
membership(g.new)

# Let's try plotting the communities from the igraph algorithm
V(g.simple)$community <- membership(g.new)
plot(g.simple, vertex.label = NA, edge.arrow.size = .1, 
     vertex.size = 4, vertex.color = V(g.simple)$community)


## LOUVAIN METHOD ##
# Another, and usually very successful method of community detection, is the Louvain method.
# It works on a different principle than the GN algorithm
# It optimizes modularity by first identifying each node as its own community, calculating modularity,
  # then calculates modularities if nodes are added to their neighbouring communities. 
  # If each of this step returns a better modularity, the algorithm goes on until it can no longer improve modularity.
# This first step is something that usually goes by the name of "greedy algorithm".

# In the next step, it does the same, but now considers the whole communities it identified in the first step as nodes in a network.

# igraph louvain method:
g.undirected <- as_undirected(g.simple)
c.louv <- cluster_louvain(g.undirected) # twelve communities
communities(c.louv)

# Let's plot the communities and members:
V(g.undirected)$louvain_community <- membership(c.louv)
plot(g.undirected, vertex.label = NA, edge.arrow.size = .1, 
     vertex.size = 4, vertex.color = V(g.undirected)$louvain_community)

#### SAVE YOUR WORK (if necessary) -----
rm(list = setdiff(ls(), c("g", "g.net", "g.simple", "g.undirected")))
save.image("data3.RData")
