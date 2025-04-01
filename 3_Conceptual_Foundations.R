
# 3. CONCEPTUAL FOUNDATIONS ------------
#    Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be

# Clean the R environment
rm(list = ls())

library(igraph); library(netseg); library(statnet); library(ITNr); library(xUCINET)
library(readxl); library(dplyr); library(tidyr)

# Load your data first
load("data2.RData")

### 3.1 GRAPHS ----
# Directed graphs (networks) 
g

# Un-directed graphs (networks)
is_simple(g) # this is FALSE, meaning that our graph has either loops or multiple edges or both - so this is a "multi-graph"
g.simple <- simplify(g) # We can turn our graph into a "simple" graph, meaning that we leave out all loops and multiple edges.
# This is often recommended, as it is often difficult to conceptually justify multiple edges or self-loops
g.undirected <- as_undirected(g) # We turn a directed graph into an undirected graph.
# Now all direction is disregarded and edges are reciprocated.

set.seed(1234)
par(mfrow = c(1, 1))
plot(g.undirected, 
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
     edge.arrow.size = .2,
     layout = layout_with_fr)


### 3.2 NODES AND DYADS ----
V(g)$name # V() function can be used with igraph objects to get to vertices
V(g)$sex
V(g)[[]] # display vertices of a graph
V(g)[[52]] # display the 52nd vertex in the graph

gorder(g) # get the number of vertices in a graph
vcount(g) # the same as above

as_edgelist(g) # display the edgelist of a graph
E(g)[[1:10]] # E() function returns the edges in igraph
E(g)[[]]

ecount(g) # number of edges in a network

neighbors(g, "P0105", mode = "in") # who are nodes who send ties to P0105?
adjacent_vertices(g, "P0002", mode = "out") # Whom does P0002 send correspondence
ego(g, nodes = "P0002", mode = "out") # this function differs only in that it includes the ego


# Extracting and plotting an ego network
p0051.graph <- make_ego_graph(g, order = 1, nodes = "P0051", mode = "out")
plot(p0051.graph[[1]], 
     vertex.label.cex = .7,
     vertex.size = 10,
     vertex.color = ifelse(V(p0051.graph[[1]])$sex == "male", "steelblue", "tomato"),
     edge.arrow.size = .2,
     layout = layout_with_fr)


# Making and comparing multiple ego graphs visually
ego.graphs <- make_ego_graph(g.undirected, order = 1, nodes = c("P0051", "P0002"))

par(mfrow = c(1, 2))

# A neat trick to setup your plot preferences only once by using the igraph_options() function that 
# sets the graphics for all graphs that follow:
igraph_options(vertex.size = 9, 
               vertex.color = ifelse(V(p0051.graph[[1]])$sex == "male", "steelblue", "tomato"),
               vertex.label.cex = .7,
               edge.arrow.size = .17,
               layout = layout_with_fr)

plot(ego.graphs[[1]])

plot(ego.graphs[[2]])

intersection(ego.graphs[[1]], ego.graphs[[2]]) # use intersection from set theory to find overlaps


### 3.3 PATHS, TRAILS, WALKS, COMPONENTS ###
# Paths
all_simple_paths(g, "P0051", to = "P0105", cutoff = 2) # paths of length two

distances(g, "P0051", "P0204", mode = "out") # The shortest distance from P0051 to P0204 is two.
distances(g, "P0051", "P0204", mode = "in") # Infinite, meaning there is no path

# Components
igraph::components(g, mode = c("weak")) # weak components of the directed graph
# Remember, components are collections of nodes in which each node can reach every other node by some path.

vcount(largest_component(g)) # the number of vertices in the largest component
