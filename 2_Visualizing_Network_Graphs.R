
# 2. VISUALIZING NETWORK GRAPHS ------------
#    Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be

# Clean the R environment
rm(list = ls())

packages <- c("netseg", "statnet", "ITNr", "expm", "blockmodeling", "readxl")
install.packages(packages)
# Download and install the xUCINET package:
# https://sites.google.com/view/asnr-2022/xucinet

library(igraph); library(netseg); library(statnet); library(ITNr); library(xUCINET)
library(readxl); library(dplyr); library(tidyr)

# Load your data first
load("data.RData")

##### 2.1 CREATING AND VISUALIZING THE GRAPH ----

# Load and clean additional attribute data for the nodes in the network:
attribute <- read_xlsx("data/attributes.xlsx")
attribute <- as.data.frame(attribute) %>% 
  filter(sex_label %in% c("male", "female")) # Reformat as data frame and keep only relevant data.

# Create an igraph graph:
g <- graph_from_data_frame(relations[, c("source", "target")], directed = T)

nodes <- attribute[match(V(g)$name, attribute$id),] # Identify those nodes that are in the network
V(g)$sex <- nodes$sex_label # Add attributes to the graph nodes and edges (sex, labels, and dates of correspondence)
V(g)$label <- nodes$id
V(g)$date <- relations$date[match(V(g)$name, relations$target)]
E(g)$date <- relations$date

set.seed(1234) # Remember that if you want to always get the same coordinates of the nodes in the plot,
# you need to repeatedly use the same random seed generator
plot(g) # the simplest and ugliest plot of the igraph object
# Here we use the default R "plot" function which recognizes the class of the argument automatically
# and plots the appropriate plot.

# Let's try making it a bit more meaningful
set.seed(1234)
plot(g, 
     vertex.label = NA, # we leave out the labels, as they just clutter the plot
     vertex.size = 4,
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"), # we use the ifelse function to set the color to blue if the node is male or red if female
     vertex.frame.color = "black", # this is the argument that sets the color of the outline of the nodes
     edge.arrow.size = .2) # this sets the size of the arrow

names(igraph:::.igraph.shapes) # With this command, one can look at all the shapes available in igraph

# Try looking for help files in R studio about the plot() function, as well as igraph.
# Look for additional help in igraph documentation, both in R studio and at: https://igraph.org/
# Try other sources of information, such as the R graph gallery: https://r-graph-gallery.com/
# Or, for more complex and detailed work, look at the literature on plotting with R, especially on packages like ggplot2.


# Try creating and plotting the "network" object rather than "igraph"
# Here, instead of creating an igraph object, i.e. an igraph graph, we create a network object
# Note that there are various packages that can deal with network data in R, such as igraph, sna, and network,
# and usually they deal with different classes of objects - igraph graphs, network objects, matrices, etc.
g.net <- network(relations[, c("source", "target", "date")],
                 vertex.attr = list(nodes$id, nodes$sex),
                 vertex.attrnames = list("label", "sex"),
                 loops = T, # we must explicitly declare what happens in the data. If it has loops, we must state so, otherwise we will get errors.
                 directed = T, # The same here
                 multiple = T) # and here

set.seed(1234) # Note that we use the same seed generator as above
# In the code below, you will notice that some arguments differ from the ones used in the function that plots the igraph object
# This, unfortunately, is just the way it is, as authors of different packages decide to use different terms
# This is something one gets used to with experience, but you can always consult the help files on any particular package.
plot(g.net,
     vertex.col = ifelse(get.vertex.attribute(g.net, "sex") == "male", "steelblue", "tomato"),
     vertex.border = "black",
     label = get.vertex.attribute(g.net, "label"),
     label.cex = .5,
     label.col = "black",
     vertex.cex = 1,
     edge.col = "grey")


# Another fun and optionally interactive way of plotting the graph is with the "networkD3" package
# The networkD3 package creates Java versions of the graphs that seem visually appealing but have no
# analytical advantage over any of the above.
g.d3 <- igraph_to_networkD3(g, group = V(g)$sex)
plot.g.d3 <- forceNetwork(Links = g.d3$links, Nodes = g.d3$nodes,
                          Source = "source", Target = "target",
                          NodeID = "name", Group = "group",
                          zoom = T)
saveNetwork(plot.g.d3, "networkd3.html") # This will save your d3 document in the working directory.
# You can then open and view it in your web browser.


# Plotting with gplot
# gplot is a function from the "sna" package, which takes either the "network" object or a matrix, but not the igraph object
gplot(g.net, 
      gmode = "digraph", # digraph is another term for directed graphs
      vertex.col = ifelse(get.vertex.attribute(g.net, "sex") == "male", "steelblue", "tomato"),
      vertex.border = "black",
      edge.col = "grey")


# Plotting with ggraph (ggplot2)
# ggraph comes from ggplot2, which is a very useful and (usually) the most versatile graphics library in R
# ggplot2 has its own website: https://ggplot2.tidyverse.org/, where you can find additional information.
# There is also a lot of literature on ggplot2 and its various applications in plotting all sorts of graphs, including network graphs
# Introductory tutorials as well as further literature are available on the website.
set.seed(1234)
ggraph(g, layout = "fr") +
  geom_edge_link(color = "darkgrey", 
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_node_point(aes(fill = as.factor(sex), size = centrality_degree(mode = "in")), shape = 21, color = "black") +
  labs(fill = "Sex", size = "In-degree") +
  theme_void() +
  theme(legend.title = element_text(face = "bold.italic", family = "serif"))

# Using ggraph to plot the components of the graph
ggraph(g, "fr") +
  geom_edge_link(color = "darkgrey") +
  geom_node_point(aes(fill = as.factor(sex)), shape = 21, color = "black") +
  labs(fill = "Sex") +
  facet_nodes(~ group_components(type = "weak"))

# Setting the size of the node to their outdegree with plot() and an igraph object
set.seed(1234)
plot(g,
     vertex.label = NA,
     vertex.size = igraph::degree(g, mode = "out")/mean(igraph::degree(g, mode = "out")), # note the differences that may influence interpretation - in this case, all values are scaled the same, so ratios are preserved
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
     edge.arrow.size = .2)

# Splitting our network into periods and plotting the network at four different stages
unique(relations$date)
dates <- c("1434-12-29", "1435-01-07", "1435-01-15", "1435-02-27") # Choosing relevant dates

g.snapshots <- list() # Creating a list containing the graphs corresponding to correspondence accumulated by the four dates
# We make use of a "for loop", in that we create our four networks by deleting those edges in our complete network
# that were established before a given date
# In order to avoid having isolated nodes lying around our plot, we also have to delete them
# For this, we use the fact that they will have a degree 0, so we delete all vertices that have no connections
# at each particular date

for (i in seq_along(dates)) {
  g.snapshots[[i]] <- igraph::delete_edges(g, which(E(g)$date > dates[[i]]))
  g.snapshots[[i]] <- igraph::delete_vertices(g.snapshots[[i]], which(igraph::degree(g.snapshots[[i]]) == 0))
}


names(g.snapshots) <- dates # Name the list elements with the corresponding date


# Plotting the network at four different stages
par(mfrow = c(2, 2)) # This sets our plotting area so that we have 2 rows and 2 columns, i.e. enough place for 4 images
for (i in seq_along(dates)) { # we again need to use the "for loop" to iterate over the four dates and apply the same code to four different data sets
  plot(g.snapshots[[i]], 
       vertex.label = NA,
       vertex.size = 6,
       vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
       edge.arrow.size = .15,
       main = dates[[i]])
}


# Looking at how degree centrality develops
for (i in seq_along(dates)) {
  plot(g.snapshots[[i]],
       vertex.label = NA, 
       vertex.size = igraph::degree(g.snapshots[[i]], mode = "in"), # We set the size of the node to represent its centrality
       vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
       edge.arrow.size = .1,
       edge.color = "darkgrey",
       main = dates[[i]])
}

# Doing the same, but with ggraph
ggraph.list <- list() # First we create a list of ggraph objects

for (i in seq_along(dates)) {
  ggraph.list[[i]] <- ggraph(g.snapshots[[i]], layout = "fr") +
    geom_edge_link(color = "grey", arrow = arrow(length = unit(.1, "cm"))) +
    geom_node_point(aes(fill = as.factor(sex), size = centrality_degree(mode = "in")),
                    shape = 21,
                    color = "black",
                    stroke = .7) +
    theme_void() +
    ggtitle(dates[[i]]) +
    labs(fill = "Sex", size = "Centrality")
}

gridExtra::grid.arrange(grobs = ggraph.list,
                        nrow = 2, 
                        top = "Network at Critical Dates") #print the four graphs


# Observing the evolution of the network via edge color
par(mfrow = c(1, 1)) # Get the 1 row, 1 column layout back
pal <- colorRampPalette(c("grey", "red")) # set a palette, a spectrum from grey to red
time <- get.edge.attribute(g.net, "date") # we define the variable time as the list of dates from our graph data
col.time <- pal(length(unique(time)))[as.numeric(cut(time, breaks = 30))]
# Lastly, we define different colors from grey to red depending on time

plot(g.net, edge.col = col.time, # Here we use the col.time object, which contains the colors that we attach to edges
     vertex.cex = 0.9,
     vertex.col = ifelse(get.vertex.attribute(g.net, "sex") == "male", "steelblue", "tomato"))

# Let's do this via vertex color too
pal # use the same palette
days <- V(g)$date[!is.na(V(g)$date)] # we get all days apart from "NA".
col.days <- pal(length(unique(days)))

plot(g, 
     vertex.label = NA,
     vertex.size = 4,
     vertex.color = col.days,
     edge.arrow.size = .2)


### 2.1.2 GRAPH LAYOUTS ----
par(mfrow = c(1, 1))
set.seed(1234)

help(layout) # This will lead you to the documentation related to layouts in igraph
# There you will find information as well as detailed literature on all layouts that exist in igraph
# This literature is not overly concerned with graphical representations, but rather focuses on mathematical treatments of each layout
# The documentation on their application in R is, however, written in a much more approachable way

# 1) Fruchteman-Reingold
# Fruchteman-Reingold plots nodes as magnets with identical polarization and edges as springs of the same strength
plot(g, 
     vertex.label = NA,
     vertex.label.cex = .5,
     vertex.size = 4,
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
     edge.arrow.size = .2,
     layout = layout_with_fr)

# 2) Kamada-Kawai
# Kamada-Kawai assumes nothing about the nodes, but plots edges as springs whose strength is not identical,
# but depends on geodesic distances between the nodes
plot(g, 
     vertex.label = NA,
     vertex.label.cex = .5,
     vertex.size = 4,
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
     edge.arrow.size = .2,
     layout = layout_with_kk)

# 3) Circle - which is not a fitting plot in this case
plot(g, 
     vertex.label = NA,
     vertex.label.cex = .5,
     vertex.size = 4,
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
     edge.arrow.size = .2,
     layout = layout_in_circle)

# 4) Grid - also not particularly helpful
plot(g, 
     vertex.label = NA,
     vertex.label.cex = .5,
     vertex.size = 4,
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"),
     edge.arrow.size = .2,
     layout = layout_on_grid)


# 5) A layout that allows us to keep each node in the same place
g.snapshots1 <- list() # let's first create a new list of graphs for specific dates

for (i in seq_along(dates)) {
  g.snapshots1[[i]] <- delete_edges(g, which(E(g)$date > dates[[i]]))
} # and populate it so that now we do not delete the vertices 
# Unlike before, we fix the position of each node (regardless of whether it is present at a specific date)
# through a layout command:
set.seed(1234)
layout.fixed <- layout_with_fr(g) # We set the layout to be the Fruchteman-Reingold layout, but
# with coordinates taken from the full graph
par(mfrow = c(2, 2)) 
for (i in seq_along(dates)) {
  plot(g.snapshots1[[i]],
       vertex.label = NA,
       vertex.size = 6,
       vertex.color = ifelse(igraph::degree(g.snapshots1[[i]], mode = "total") == 0, grey(0, 0),
                             ifelse(V(g)$sex == "male", "steelblue", "tomato")),
       vertex.frame.color = ifelse(igraph::degree(g.snapshots1[[i]], mode = "total") == 0, "grey", "black"),
       edge.arrow.size = .15,
       main = dates[[i]],
       layout = layout.fixed)
}

# Save your data
rm(list = setdiff(ls(), c("g", "g.net")))
# And save the data:
save.image("data2.RData")
