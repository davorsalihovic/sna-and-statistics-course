# 6. INTRODUCTION TO INFERENTIAL STATISTICS FOR NETWORKS ------------
#    Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be

# Clean the R environment
rm(list = ls())

remotes::install_github("jason-morgan/ina")
install.packages("migraph")

library(sna); library(ina); library(igraph); library(network); library(statnet)
library(migraph)

load("data3.RData")

# The run-of-the-mill frequentist statistics we encountered in previous sessions cannot usually be
  # directly applied to modeling ties in networks, as network data violates some of the central assumptions of standard statistics.
# One of these assumptions is the assumption of INDEPENDENCE.

  # If we are to capture the effect some explanatory phenomenon may have on another (outcome) phenomenon we are investigating,
    # then we have to do the best we can in isolating the impact of this explanatory variable.
    # It is very unlikely that we will be able to filter out all unwanted unsystematic variance, 
      # but we must make an effort to do our best to get rid of it.
    # A lot of this, especially in network data, comes down to INTERDEPENDENCIES between each observation.

# These kinds of scenarios can, in fact, be handled by the frequentist statistics through a class of models
  # called "random-effects models" or "multilevel models". 
  # However, these models usually cannot handle the kind of interdependence that exists in networks.

# Networks, by definition, contain interdependencies caused not (only) by personal or group characteristics, 
  # but by the structures in the network.
# An example is the transitive triple or the triangle.
  # If i and j are friends, and j and k are friends too, then it is highly likely that i and k will establish such a connection as well.
  # In mathematical terms, this means that this triangle cannot possibly exist if the edges between i and j, and j and k, weren't there before. 
  # In other words, the existence of the triangle is dependent upon the existence of other structural characteristics in the network.

# This is the reason why we usually cannot model the existence/absence of edges between nodes in the network
  # via the standard set of statistical tests and models.


# The second issue - one that you would encounter less often and only in special circumstances -
  # is that with networks you may in fact work with the entire population, rather than a sample.

# These two reasons, and especially the first one, is why we often need a specific set of mathematics and statistical models
  # that take into consideration these peculiarities of network data.
# The main goal of inferential network analysis is to model the existence or absence of ties, 
  # and the endogenous and exogenous structural or other kind of effects that influence this.


#### 6.1 CONDITIONAL UNIFORM GRAPH TEST ----
# Conditional uniform graph tests are used to discern the statistical significance of some endogenous
  # effect in shaping the network of interest.

# As always with inferential statistics, this test tells us whether the effect we are testing for is
  # exceptional in the network, and so whether such tendencies had an impact on shaping the network.

# The conditional uniform graph tests work on the principle of permutation tests.
# The goal of permutation tests is to find a distribution of observations that would exist
  # if the processes that governed the creation of the data we observe were entirely random, 
    # that is if the graphs against which our hypotheses are tested were random.
# In other words, a distribution of scores that may have come about by chance alone.

# If, when comparing this distribution to the values we actually observe in our data, these values
  # have the probability of occurring among the randomly generated scores below the chosen alpha-value
    # of, say, the traditional 5%, then there is obviously something very exceptional about the score we observe.
# Put differently, it is very unlikely that it might have occurred by chance alone.

# The principle, therefore, is the same as with any other inferential statistics, just that instead of 
  # standardizing our coefficient with the goal of getting the appropriate test statistic (the z-score, t-value, etc.),
  # and then comparing this to the known distributions, here we observe the empirical distribution,
  # and compare our values to it.

# The CUG test achieves this permutation by permuting thousands of networks under the pre-determined constraints.
# One of those constraints may be the number of nodes in the network.
# If you have a network with 10 nodes, you permute, say, 10 000 networks with 10 nodes, whose 
  # edges are determined by random processes, without any bias one way or the other.
# We then calculate the statistic of interest for these 10 000 networks, and get the frequency distribution
  # of the statistic as it would look like if it had come about through random processes.
# If, then, the statistic we observe in our network is highly probable in this distribution,
  # it is then NOT SIGNIFICANT, but if it's very improbable, then IT IS SIGNIFICANT.

# There are various types of constraints you can set, depending on your theoretical assumptions about the 
  # processes that may have governed the emergence of the network you investigate.
# Another thing to bear in mind are theoretical considerations from network science on random networks, on which there is abundant literature.

  # The constraints that are usually used in these kinds of tests are the number of nodes, the dyad census, and the number of edges.

# Let's get some data and demonstrate the CUG test.
g.undirected
g

# Plot the network
par(mfrow = c(1, 1))
set.seed(123)
plot(g, vertex.label = NA, 
     vertex.color = ifelse(V(g)$sex == "male", "steelblue", "tomato"), vertex.size = 3,
     edge.color = "grey", edge.arrow.size = .2)

# And let's create the adjacency matrices, which we need to conduct the CUG tests.
undir.matrix <- as.matrix(as_adjacency_matrix(g.undirected))
dir.matrix <- as.matrix(as_adjacency_matrix(g))

# Now that we have the data, let's test for the tendency of the egdes in our network
  # to be 1) transitive, 2) reciprocated.

transitivity(g.undirected) # The descriptive statistic for transitivity in our graph is at 0.224
# As always with descriptive statistics, this is rather meaningless, as we simply do not know whether this is 
  # high or low, expected or unexpected, etc.

reciprocity(g) # And for reciprocity, we get 0.2.

set.seed(123)
  # And run the cug.test for transitivity
    # We constrain ("cmode") the permutations by the number of edges
    # It may take some time for the test to run
cug.trans <- cug.test(undir.matrix, FUN = gtrans, mode = "graph", cmode = "edges")
  # Look at the results
cug.trans
  # The output reports the observed statistic for transitivity (so, the same we have above),
    # and then the results of the test:
  # The Pr(X >= Obs) part gives us the probability that what we observe is lower than the distribution
  # The Pr(X <= Obs) works the other way round
  # We see that one is 0 and the other 1, in other words all of the permuted measures for transitivity
    # from networks that came about through random processes are lower than what we actually observe
# Since ALL of them are lower, there is essentially 0% chance that what we observe may actually belong
  # to this distribution, and we conclude that there is indeed something very exceptional about transitivity in our network
# There is a statistically significant tendency for edges to be transitive.

plot(cug.trans) # You can plot the distribution and the position of our observation relative to it (the red line)
  # to check these results visually.

# Running the analysis in the "migraph" package:
  # Migraph takes igraph objects and functions, so you don't need to extract the adjacency matrix
set.seed(123)
recip <- test_random(g, FUN = reciprocity)
recip
plot(recip) # The results and the plots are interpreted the same way as before.
  # Reciprocity too turns out to be significant in our network.

# We can test for all kinds of effects. Let's see what's going on with sex homophily in our network.
set.seed(123)
hom <- test_random(g, network_heterophily, attribute = "sex")
hom
plot(hom) # This too turns out to be significant.

# Or mean distance. Remember that distance means the number of steps between nodes.
# In this case we are taking the average of all the distances in the network (which is a measure of cohesion)
  # and looking at whether our network exhibits peculiarities here too.
set.seed(123)
dist <- test_random(g, mean_distance)
dist
plot(dist) # And indeed it does. The mean distance is significantly lower than expected from RANDOM GRAPHS.


## In order to demonstrate the mathematics behind the CUG tests in more detail, let's run them by hand.

  # Running a CUG test on the number of communities in the network
  # We restrict the simulated networks with the fixed numbers of edges and vertices
  # Let's use 1000 trials

n.vertex <- vcount(g.undirected) # Number of nodes in our network
n.edge <- ecount(g.undirected) # Number of edges
degrees <- igraph::degree(g.undirected) # Degree
n.trials <- 1000

set.seed(123)
num.com <- numeric(n.trials)
for (i in (1:n.trials)) {
  gr <- sample_gnm(n.vertex, n.edge) # Here we generate the Erdős-Rényi RANDOM GRAPHS against which we measure our observations.
  c <- cluster_louvain(gr)
  num.com[i] <- length(c)
}

hist(num.com) # Plot the distribution of the permuted number of communities.
set.seed(123)
obs <- length(cluster_louvain(g.undirected))
abline(v = obs, lwd = 2.2, col = "red") # add what we observe as a red line

# Calculate probabilities
prob.greater <- mean(num.com >= obs)
prob.lower <- mean(num.com <= obs)
prob.greater
prob.lower
  # This result is not statistically significant.


# Let's try calculating the significance of the statistics we have already done above
  # Reciprocity
set.seed(123)
num.recip <- numeric(n.trials)
for (i in 1:n.trials) {
  gr <- sample_gnm(n.vertex, n.edge, directed = T)
  rec <- reciprocity(gr)
  num.recip[i] <- rec
}

hist(num.recip, xlim = c(0, 0.3))
obs <- reciprocity(g.simple)
abline(v = obs, lwd = 2, col = "red")

mean(num.recip >= obs)
mean(num.recip <= obs)
  # The same results as earlier - a statistically significant result.

# Transitivity
set.seed(123)
num.trans <- numeric(n.trials)
for (i in 1:n.trials) {
  gr <- sample_gnm(n.vertex, n.edge, directed = T)
  num.trans[i] <- transitivity(gr)
}

hist(num.trans, xlim = c(0, 0.3))
obs <- transitivity(g.simple)
abline(v = obs, lwd = 2, col = "red")

mean(num.trans >= obs)
mean(num.trans <= obs)
  # Same results as earlier.

## BE WARY OF WHAT CONSTRAINTS YOU CHOOSE when conducting the CUG test.
# It can happen that your results are biased, and you get a significant result when using one type of constraint 
    # and non-significant results with another type.
# Consider these test done on the Florentine families data from Padgett (we return to this data later on).
data(florentine) # call the data

par(mfrow = c(1, 1))
set.seed(123)
cug.cent.size <- cug.test(flobusiness,
                          sna::centralization,
                          FUN.arg = list(FUN = sna::betweenness), 
                          mode = "graph", 
                          cmode = "size")
cug.cent.size
plot(cug.cent.size)

set.seed(123)
cug.cent.dyad <- cug.test(flobusiness,
                          sna::centralization,
                          FUN.arg = list(FUN = sna::betweenness), 
                          mode = "graph", 
                          cmode = "dyad.census")

cug.cent.dyad
plot(cug.cent.dyad)

set.seed(123)
cug.cent.edge <- cug.test(flobusiness,
                          sna::centralization,
                          FUN.arg = list(FUN = sna::betweenness), 
                          mode = "graph", 
                          cmode = "edges")
cug.cent.edge
plot(cug.cent.edge)

par(mfrow = c(1, 3))
plot(cug.cent.size); plot(cug.cent.dyad); plot(cug.cent.edge)
# From the plots, you can best observe that we get a significant result when we use the number of nodes,
  # but a non-significant result when using the dyad census or the number of edges
# One therefore needs to beware of choosing wrong constraints.
  # There are no golden rules about this.
  # This decision is based solely on theoretical observations about your network.



#### 6.2 QUADRATIC ASSIGNMENT PROCEDURE ----

# QAP, or Quadratic Assignment Procedure, is a method of statistical modeling that allows us to investigate 
  # the effects of variables exogenous to the network of interest, be it other networks or characteristics of
  # the nodes in the network, i.e. those variables that do not belong to the structural characteristics of the network.

# QAP is also based on permutations, but of different kind. 
# Whereas in CUG we use permutations of random networks to get to a distribution of some statistic,
  # in QAP we "scramble" or permute the matrix of the dependent variable (the dependent network).
# By permuting this matrix, the correlations (or whatever other statistic) between it and the matrices of independent variables are also permuted, 
  # and we can again get a distribution of these statistics against which we compare the observed statistic.

# This "scrambling" of matrices is the method through which we get rid of all dependencies other than those that actually matter,
  # and we wish to measure.

# Note that it follows from what we've said so far that in order to run QAPs, we need to work with matrices.
# This means that both the explanatory (independent) and dependent variables need to be expressed as matrices.

# We demonstrate all of this in the following lines.

## Let's first create, on purpose, two positively correlated graphs, and run a correlation test  
par(mfrow = c(1, 1))
set.seed(123)
graphs <- sample_correlated_gnp_pair(50, 0.6, 0.03, directed = T)
graphs
graph1 <- graphs$graph1
graph2 <- graphs$graph2

par(mfrow = c(1, 2))
set.seed(123)
plot(graph1, vertex.label = NA, edge.arrow.size = .3, vertex.size = 7)
set.seed(123)
plot(graph2, vertex.label = NA, edge.arrow.size = .3, vertex.size = 7)

# We then need to get to their adjacency matrices.
mat.graph1 <- as.matrix(get.adjacency(graph1))
mat.graph2 <- as.matrix(get.adjacency(graph2))

# And let's see what is the correlation coefficient between the two
gcor(mat.graph1, mat.graph2) # It is 0.68, which is both a positive and fairly high correlation. 
  # Remember, correlations are measured between 0 and 1.

# As always, the descriptive statistic - in this case the correlation coefficient - tells us nothing
  # about the statistical significance of this.
# And, so, we turn to the QAP test for correlation.

# Using the QAP to test.
set.seed(123)
qap.cor <- qaptest(list(mat.graph1, mat.graph2), gcor, g1 = 1, g2 = 2)
summary(qap.cor)
# The output of the result is very similar to what we saw in the CUG outputs.
# It gives us the actual correlation observed, and it's significance in terms of probabilities under the assumption of randomness.

par(mfrow = c(1, 1))
plot(qap.cor, xlim = c(-0.05, 0.7)) # And, again, we can plot our results and explore them visually.
# In this case we have a statistically significant correlation.

## We continue with some concrete data from historical sources
# This data comes from the famous paper by Padgett and Ansell, "Robust Action and the Rise of the Medici", 
  # that heavily relies on networks and their analysis

# We will demonstrate how one would analyze this data with QAP and later with ERGM.
data(florentine)
# The data set contains the business network and the marriage network.
# The former is a representation of what 15th-c. Florentine families did business with each other,
  # and the latter is the marriage network between the same families.

# Marriage network
set.seed(5)
plot(flomarriage,
     label = network::get.vertex.attribute(flomarriage, "vertex.names"),
     label.cex = 1,
     label.col = "black",
     label.pos = 1,
     vertex.cex = 1,
     edge.col = "grey")

# Business network
set.seed(12)
plot(flobusiness, label = network::get.vertex.attribute(flomarriage, "vertex.names"),
     label.cex = 1,
     label.col = "black",
     label.pos = 1,
     vertex.cex = 1,
     edge.col = "grey")

# Let's plot the two networks side by side using the same coordinates for both, 
  # so that the position of each node is the same in both plots.
set.seed(123)
coords <- gplot(flomarriage)
par(mfrow = c(1, 2))
gplot(flomarriage, 
      usearrows = FALSE, 
      displaylabels = TRUE,
      coord = coords,      
      label.cex = 1,
      label.pos = 12,
      vertex.col = "grey",
      vertex.cex = .7,
      edge.col = "darkgrey",
      main = "Marriages")

gplot(flobusiness, 
      usearrows = FALSE, 
      displaylabels = TRUE,
      coord = coords,      
      label.cex = 1,
      label.pos = 12,
      vertex.col = "grey",
      vertex.cex = .7,
      edge.col = "darkgrey",
      main = "Business ties")

# Zoom onto the plot once you plot it to get a better image.

# Let's see how the two networks correlate by using the QAP test for correlation from earlier
padgm <- network::as.matrix.network(flomarriage)
padgb <- network::as.matrix.network(flobusiness)

set.seed(123)
pad.cor <- qaptest(list(padgm, padgb), gcor, g1 = 1, g2 = 2, reps = 1000)
summary(pad.cor) # we have a correlation of 0.37, which turns out to be significant
par(mfrow = c(1, 1))
plot(pad.cor, xlim = c(-0.4, 0.4))
# From this we conclude that it seems that the networks behave similarly, i.e. that business ties are mirrored by the marriage ties.
  # But let's run some further analyses and regressions to verify this.

# As noted earlier, in order to run statistical models with QAP, we need to turn whatever variables
  # we deem important into matrices.
# Say that we wish to examine the impact of wealth on the marriage network, in addition to the business network.
  # To operationalize this in the matrix form, we have to come up with some way of representing the wealth of each family as a matrix.
  # One of the ways is to create a "distance matrix", a matrix in which each cell represents the difference in wealth between family i and family j.

# Let's create such a matrix
# The "florentine" data has the wealth of each family stored as a vertex attribute
wealth <- data.frame(id = network::get.vertex.attribute(flomarriage, "vertex.names"),
                     wealth = network::get.vertex.attribute(flomarriage, "wealth"))
wealth # And now we have a dataframe with the wealth of each family.

  # We will use it to create the distance matrix,
w.dist <- as.matrix(dist(wealth[, 2])) # the difference in wealth in matrix format

# We can additionally create matrices of wealth for a directed scenario.
# Since the senders of ties are always those nodes listed in the rows of the matrix, and receivers those that are in the columns,
  # we can simply enter the wealth of senders into rows, and wealth of receivers into columns:
w.send <- matrix(wealth[, 2], nrow(wealth), nrow(wealth), byrow = F) # the wealth of the sender in matrix format
w.rec <- matrix(wealth[, 2], nrow(wealth), nrow(wealth), byrow = T) # the wealth of the recipient in matrix format

# Let's examine what we have
w.dist
w.send
w.rec

## Before we proceed to the models, we will examine measures of autocorrelation, such as Moran's I and Geary's C.
# These statistics are often used primarily in spatial statistics, to account for the autocorrelation that exists
  # in geographically close variables (remember Tobler's first rule of geography: "everything is related to everything else, but near things are more related than distant things.")
# This sort of autocorrelation is also present in networks, where "near things", or rather adjacent nodes, may be more similar to each other than those far away.
# And we can test this.

# Let's examine the Moran's I for the nodes' wealth
flo.auto <- nacf(padgm, network::get.vertex.attribute(flomarriage, "wealth"), type = "moran")
flo.auto # We get 15 measures, corresponding to 15 steps, the maximum number of steps from the focal node
# The Moran's I for the immediate neighborhood (i.e. order 1) is -0.31
  # Here we have a negative autocorrelation, meaning that, when it comes to wealth, it seems that families do not marry into "similar" families.

flo.cor <- nacf(padgm, network::get.vertex.attribute(flomarriage, "wealth"), type = "correlation")
flo.cor[2] # And we can run the regular correlation too, with the result of -0.29, suggesting the same thing.

cor.test(padgm, w.dist) # If we run the statistical test for the significance of the correlation between
  # the matrix of marriage ties and the matrix of wealth distance, we get a 
  # significant POSITIVE CORRELATION. 
  # Although it may seem wrong that above we had a negative correlation,
    # and here we have a positive one, this is not really so, if we think about what our data looks like.

  # Here we correlate the matrix of distances in wealth with the matrix of ties.
  # This positive correlation is due to the fact that the GREATER THE DISTANCE the GREATER THE CHANCE OF THERE BEING A TIE. 
  # Which is the same thing as saying that those families that marry into each other differ in wealth.
  # Therefore, this too confirms our findings about wealth.


# Let's now turn to modeling the marriage network through the functions of the "sna" package and QAP.
# These models are specified similarly to the linear models we specified in previous sessions.
# We have a matrix we wish to examine, and a set of explanatory variables.

# We can choose to use the "netlm()" function, which allows us to fit a linear model using ordinary least squares
  # to our network data, relying either on the traditional (here called "classical") null hypothesis, i.e. the same known distributions we would use with regular data,
  # or the QAP null distributions which, as we now know, we get from the data themselves.

set.seed(123)
lin.flo <- netlm(padgm, padgb, nullhyp = "classical") # classical null hypothesis
lin.flo

set.seed(123)
qap.flo <- netlm(padgm, padgb, nullhyp = "qap", reps = 1000) # QAP null hypothesis
qap.flo
# In both cases you will of course get the same coefficients, but the p-values that determine the significance will be different.
# It is better to use the QAP hypothesis, for all the reasons we said at the beginning about the interdependencies.

# Here we are examining the effect of business ties on the marriage ties, 
  # and the output tells us that there is a significant positive effect.
# That is, the existence of business ties raises the probability of there being a marriage tie too.

# Note that here we are running a linear model, which is not an optimal strategy for dichotomous (0, 1) data (remember the Poisson model from the previous lesson.)
# We return to this below.

# Let's now add to this model another explanatory variable, the difference in wealth.
set.seed(123)
qap.flo1 <- netlm(padgm, list(padgb, w.dist), nullhyp = "qap", reps = 1000)
qap.flo1 # We again get the positive and significant results both for business ties, and for difference in wealth.
# Therefore, the greater the difference in wealth, the greater the probability of there being a marriage tie
# This confirms what we've found earlier.

# However, as noted earlier, using the linear model with dichotomous data is not an optimal strategy.
# Linear models are best for continuous variables and, in this case, weighted matrices - those that have something other than 1 or 0 as entries.
# For instance, a matrix of strength of friendships (measured, for instance, by the number of years two people have been friends) is an example.
# Or, in this particular instance, the distance in wealth matrix may be the appropriate dependent variable.

set.seed(123)
netlm(w.dist, padgm, nullhyp = "qap") # This, for instance. 
                                      # Here we see that a one unit increase in ties, in other words there being a tie, increases the difference in wealth by 23.07.


# For dichotomous or any sort of categorical data, for that matter, it is best to use BINOMIAL MODELS.
  # They are most appropriate when each trial must result in one of two events:
  # These models, both here with networks and in the standard statistics, therefore, deal with data that take 0 or 1.
set.seed(123)
qap.log.flo <- netlogit(padgm, list(padgb, w.dist), nullhyp = "qap", reps = 1000)
qap.log.flo # Here we have a slightly different set of coefficients.
  # Coefficients here are reported not as absolute values, but as logarithms of odds.
  # If we exponentiate this, we get ODD RATIOS, which mean exactly what you think they mean.
# For instance, for business network, we have an odds ratio of 12.2, meaning that 
  # if there is a tie between two families in the business network, there is a 12.14 times greater chance
  # of there also being a marriage tie compared to the scenario where isn't a business tie present.

  # We can moreover express these results in terms of probabilities by running them through
    # the logistic (sigmoid) function:
exp(2.5) / (1+exp(2.5))
  # or simply:
plogis(2.5)
    # Once there is a business tie, wealth held constant, there is a 92% probability of a marriage tie.

# We also get a positive, but small effect for the difference in wealth, with the odds ratio of 1.02.
# Note that when we are talking about odds, then 1 effectively means 0. 
  # If chances are 1 time greater, they are of course not greater at all.
plogis(0.0199)

# Although enormously different in magnitude, both of these effects are statistically significant.

# So, in our QAP analysis of the early 15th century Florentine marriage networks, we have found that
  # business ties significantly influence the establishment of marriage ties, as well as that
  # the effect of the difference in wealth acts in a similar manner.
  # In summary, business partners married off their sons and daughters to each other, and
    # poorer families on average married into wealthier families, and the other way round.



#### 6.3 EXPONENTIAL RANDOM GRAPH MODELS ----

# So far we have learned that the CUG test can tell us something about the significance of the structural
  # characteristics of the network measured at the network level, and that we can use statistical models
  # with QAP permutations and matrices to look for correlations between two networks as well as measure the effects
  # of exogenous variables, i.e. characteristics of the nodes involved.

# Since we are working with network data and all of their peculiarities when we are investigating social networks,
  # inferential network statistics would not be very useful without the ability to also account for the 
  # dependencies in the network structure and their impact on the existence or non-existence of ties in the network.

# Thankfully, there are various models, and a a large field of statistics and mathematics, designed to tackle this issue.
  # These models, such Stochastic Actor-Oriented Modes, Relational Event Models, Dynamic Actor-Network Models, or Exponential Random-Graph Models,
  # are all aimed at understanding the impact of both those variables that are exogenous to the network (e.g. characterstics of each node - gender, wealth...),
  # and those that are endogenous to it - i.e. the structure of the network - how the ways in which edges are distributed impacts the existence of ties.

# These models generally use the the approach of the traditional statistics in measuring the importance of each effect against a set of known distributions,
# but, especially in the case of ERGMs, also rely on modeling these effects by generating random graphs that contain the structural features of the networks we try to model.

# ERGMs are, therefore, a powerful tool in investigating the generative features of processes that produced the network we observe, 
  # or, put differently, in investigating how ties in the network come about due both to the endogenous structural characteristics of the network,
  # and the exogenous characteristics of its nodes.

# Let's first demonstrate how to work with ERGMs on the Krackhardt data on high-tech managers.
# This data represents a network of advice seeking. The directed ties show who sought advice from whom.
# The tie sender is the seeker, rather than the giver of advice.

data("Krackhardt")
krack <- Krackhardt
krack

par(mfrow = c(1, 3))

# Plot with a color ramp showing tenure in company, age, and the department of each manager in the network
pal1 <- colorRampPalette(c("white", "blue"))
pal2 <- colorRampPalette(c("yellow", "red"))

tenure <- network::get.vertex.attribute(krack, "Tenure") 
col.tenure <- pal1(length(unique(tenure)))[as.numeric(cut(tenure, breaks = length(unique(tenure))))]

set.seed(1)
plot(krack, edge.col = "gray", label.col = "black",
     vertex.cex = 1.5, vertex.col = col.tenure,
     main = "Tenure")

# Plot with age
age <- network::get.vertex.attribute(krack, "Age")
col.age <- pal1(length(unique(age)))[as.numeric(cut(age, breaks = length(unique(age))))]

set.seed(1)
plot(krack, edge.col = "gray", label.col = "black",
     vertex.cex = 1.5, vertex.col = col.age,
     main = "Age")

# Plot with department
dept <- network::get.vertex.attribute(krack, "Department")
col.dept <- pal2(length(unique(dept)))[as.numeric(cut(dept, breaks = length(unique(dept))))]

set.seed(1)
plot(krack, edge.col = "gray", label.col = "black",
     vertex.cex = 1.5, vertex.col = col.dept,
     main = "Department")


# Try to investigate the three network plots and see what you can gather about the advice-seeking 
# tendencies in the network. Are older age or longer tenure the characteristics of those who are asked most for advice,
# are there any within-department tendencies...?


### Building the ERGM ###
# Let's consider what sort of endogenous and exogenous effects would make sense in our network:
# First, take a look at what our network contains
krack
# We see that vertices (managers) have the following attributes: age, deparment, level, tenure (and names).
# The edges have no attributes, but the network itself has "reports to" attribute - this is a matrix, essentially another network of bosses and their employees.

# Endogenous covariates - reciprocity in seeking advice (mutually giving advice) - we assume that people may ask each other for advice.
#                       - outstars: individuals who seek many advice (having a large outdegree) - outstar is a shape with the focal node at the centre and its outgoing ties emanating from it. 
#                           We shall test for two- and three-stars (i.e. two and three edges).
#                       - clustering measure: transitivity - there may be groups of people (e.g. within departments) that seek advice more often within than without the group.

# Exogenous covariates - edge-level "reports to" to control for the relationship with bosses - underlings may ask their bosses for advice.
#                      - tenure and age, both in three ways: 
#                         - as the effect on incoming ties, on outgoing ties, and as difference.
# We also include the number of edges, which represent the intercept in ERGMs.
library(ergm)

# Let's first examine what we have in terms of the descriptives for the structural features
krack # we have 21 managers, and 190 edges between them

summary(krack ~ edges + ostar(2:3) + transitiveties) # If we run the summary this way, it gives us the ergm summary of the structural descriptive statistics
# This tells us that we indeed have 190 edges, 1062 two-outstars and 4322 three-outstars, as well as 188 transitive ties in our advice-seeking network.

# Let's then run our ERGM - this may take a while.
set.seed(1234)
ergm.mod1 <- ergm(krack ~ edges + mutual + ostar(2:3) + transitiveties +
                    edgecov("reportsto") +
                    nodeicov("Tenure") + nodeocov("Tenure") + 
                    absdiff("Tenure") + 
                    nodeicov("Age") + nodeocov("Age") + 
                    absdiff("Age"),
                  control = control.ergm(MCMC.samplesize = 500, MCMC.burnin = 1000, MCMLE.maxit = 10),
                  verbose = T)


## Checking for model fit ##
# It is imperative to check how well our ERGM fits BEFORE WE INTERPRET THE COEFFICIENTS.
# If our coefficients model a network which is very different from ours, then they are obviously useless.

gof.mod1 <- gof(ergm.mod1) # The gof() function means "Goodness of fit".

par(mfrow = c(2, 2))
plot(gof.mod1) # Run this command four times, as there are four plots to be plotted
# What we are looking for in these plots is that the black line (which is the data that comes from our network),
  # behaves similarly to blue dots, which are the mean values from the networks simulated from the estimated coefficients.
# This means that our network, if the model successfully represents the generative forces that built it, 
  # has to be just one of many similar networks. And this is how we check whether that is true.
# Specifically, we get four plots of four structural characteristics - the outdegree, indegree, edge-wise shared partners (similar to triangles),
  # and the minimum geodesic distance. Values from our network therefore need to be characteristic of all other simulated networks, i.e. there MUST NOT BE anything peculiar about it.

# Here we see that, although there are some sketchy points, the black line generally behaves well.
gof.mod1 # We can also print the output of the GOF test
  # It essentially represents the same thing as the plots, but what we are looking for are p-values that indicate statistical significance.
  # These p-values report the statistical significance of the difference between the observed values and the mean values in the simulated networks.
  # If the difference is statistically significant, then there is obviously something wrong there, as our model cannot fit well this particular data point.
  # In this case, we are generally fine.
  # We therefore conclude that our model fits well, and can continue with interpreting it.


## Interpreting the results ## 
summary(ergm.mod1) # Look at the p-values. We have a few significant things here.
# ostar2, the "reports to", the in-tie "Tenure", the difference in "Tenure", and the in-tie Age are significant.
# This means that, while controlling for other variables, the "sociability" of nodes increases the probability of there being a tie;
  # as does the existence of a "reports to" tie. People with longer tenure tend to receive more requests for advice, 
    # but the difference in Tenure reduced the probability of there being a tie. In other words, the greater the difference in time two people spent in the company, 
    # the smaller the probability of them asking each other for advice. Lastly, the older the people are the fewer request they get.

# We can calculate the exact probabilities of the existence of ties
# The coefficient estimates in ERGMs are reported as log-odds, similar to logit models from above
# In order to turn these log odds into percentages, we can use the same function as earlier:
# I.e. either 1/(1 + exp(-x)), or exp(x)/(1 + exp(x))

# Just the edges
1/(1 + exp(3.65)) # or exp(-3.62)/(1 + exp(-3.62))
# So, the basic probability of there being a tie between any two nodes is 2.5%
plogis(-3.65)

# Edges + Reports to
-3.65 + 3.73 # Sum the two coefficients
1/(1 + exp(-0.08)) # A 51.9% chance
  # Here we have a 50% greater probability of edges existing in the reports to network than outside it.
plogis(0.08)

# Edges + Reports to + In ties Age
-3.65 + 3.73 - 0.048*35 # Let's pick the age of 35
1/(1 + exp(1.6))
# A chance of only 16.8%, percent. In other words, age significantly reduces the chances.
plogis(-1.6)

# But, the younger the manager, the more likely it is that people will seek advice from them,
# as the model tells us.
-3.65 + 3.73 - 0.048*25
1/(1 + exp(1.12)) # A nearly 25% chance, or around 8% larger, for a 10 years younger manager.
plogis(-1.12)

## For the exercise, let's purposefully fit a badly specified model:
g.net.ergm <- ergm(g.net ~ edges + mutual + transitiveties + nodematch("sex"),
                   control = control.ergm(MCMC.samplesize = 2000, MCMC.burnin = 15000, MCMLE.maxit = 5),
                   verbose = T)

gof.g.net <- gof(g.net.ergm)
par(mfrow = c(2, 2))
plot(gof.g.net)
gof.g.net # Examine the plots and the GOF output as before, you'll see a drastic difference and a lot of statistically significant differences.
summary(g.net.ergm)


### ERGM withe the Florentine networks ###
# Let's yet again turn to our 15th-century Florentine families, and examine our hypotheses with the help of an ERGM.

# As we will again examine the impact of business ties on the marriage network,
  # we must include the business ties as an attribute of the marriage network
flo2 <- network(flomarriage)
bus.net <- network::as.matrix.network(flobusiness)
flo2 <- set.network.attribute(flo.new, attrname = "business", value = bus.net)

# Basic model with just the effect of edges - we have not yet included any structural dependencies or exogenous covariates
set.seed(123)
flo.ergm0 <- ergm(flomarriage ~ edges)
gof0 <- gof(flo.ergm0)
gof0 # The fit looks fine.
summary(flo.ergm0)

# Let's add the effect of business ties. We still do not add any endogenous effects.
set.seed(123)
flo.ergm1 <- ergm(flo2 ~ edges + edgecov("business"))
gof1 <- gof(flo.ergm1)
gof1 # The fit, again, looks fine
summary(flo.ergm1)
plogis(-2.04 + 2.18)
# The output now tells us that there is a basic 11% chance of there being a marriage tie in the network
# But, if two families also have business ties, this increases drastically.
# This confirms our earlier findings.

# Let's also add the difference in wealth
set.seed(123)
flo.ergm2 <- ergm(flo2 ~ edges + edgecov("business") + absdiff("wealth"))
gof2 <- gof(flo.ergm2)
gof2 # Again, the fit looks fine
summary(flo.ergm2) 
# And, as before, we get a small, but positive effect. Notice how the other effects change when we introduce new variables.
# The reasons are the same as those we encountered in linear models.
# What we see here is, again, that as the difference in wealth grew larger, there was a greater chance for the tie to exist in the marriage network.

# Let's see what sort of differences we have there in wealth, and calculate the probabilities for a big and small difference
max(w.dist); min(w.dist[w.dist != 0])
# The largest difference we have is 143, and the smallest one that isn't zero is 1.

# We then calculate the probability in the following way:
plogis(-3.04 + 2.49 + 0.0199*1) # For two families WITH BUSINESS TIES and difference in wealth of 1, the log odds are -0.53
# Or, in percentages, the chances of them marrying each other is 37%

plogis(-3.04 + 2.49 + 0.0199*143) # For two families WITH BUSINESS TIES and difference in wealth of 143, the log odds are 2.2957
# Or the chance of 90.85%, so nearly 2.5 times larger than for those of near-equal wealth.


# You may have noticed that we still haven't included any structural effects, only exogenous covariates.
# Given that we are investigating a marriage network, we should think of structural effects that 1) we are interested in, and 2) would make sense.
# We can include the effect for a large degree of, say, three, to investigate whether some families had ties to many other families.
# Since this is an undirected network, we have no way of differentiating between in- and out-degrees (i.e. sociability and popularity), 
  # but we can still investigate some sort of activity in the network.

set.seed(123)
flo.ergm3 <- ergm(flo2 ~ edges + edgecov("business") + absdiff("wealth") + degree(3) + gwdsp(1, fixed = T),
                  control = control.ergm(MCMC.samplesize = 3000, MCMC.burnin = 1000, MCMLE.maxit = 10,
                                         parallel = 3),
                  verbose = T)

# Check model fit
gof3 <- gof(flo.ergm3)
gof3
par(mfrow = c(2, 2))
plot(gof3) # The fit looks good. The distribution for the edge-wise shared partners tells us that we have accounted for triangles well.

#  Check degeneracy
deg3 <- gof(flo.ergm3, GOF = ~ model)
deg3 # These tests look good too (which you will also find in the output above)

# Check MCMC convergence
mcmc.diagnostics(flo.ergm3) # The MCMC diagnostics also look fine.
  # The three chains we specified converge well around the central values for the structural effects.
# Note that we have used more MCMC simulations than necessary (3000) to improve the convergence.
  # or rather prevent any issues with convergence.
# The very last part of the output here tells us (Joint P-value) that we have no big issues with convergence.

# Check the results
summary(flo.ergm3)
# Still, when we check the results, we see that our effects for degree and triangles are not significant
# We have not improved upon our previous model that included only the node attributes
# Neither the existence of triangles nor some form of "popularity" raise the chance of there being ties in the network.


# By using further statistical test, we can check which of our two models performs better - the one without or with the structural coefficients
logLik(flo.ergm2) # The closer the log-likelihood to zero, the better.
logLik(flo.ergm3) # Our model 2 has a LL of -43.48, and model 3 of -41.98, so it seems slightly better.
# However, as always, we need to know whether this statistic, this difference, is statistically significant.
AIC(flo.ergm2, flo.ergm3) # Here too - with the Akaike Information Criterion - the smaller, the better.
# AIC is a better measure than log-likelihood because it penalizes the model that has more predictors, and is therefore expected to fit better due to this reason.
# You see that, although we have a better LL for model 3, here we have a better AIC for model 2. Model 2 fits better according to AIC.

# But let's check the difference in log-likelihood statistically
anova(flo.ergm2, flo.ergm3) # We have NO SIGNIFICANT DIFFERENCE in the deviance of model three (in the amount of variation in our data that it explains).
# In other words, model three does not perform any better than model two, and this we may have expected as soon as we saw the non-significant coefficients for our structural effects.


# In conclusion, what we find, time and again, is that the existence of business ties between two families 
  # drastically increases the probability of there being a marriage tie as well. 
  # The difference in wealth is also a consistently positive and significant effect, telling us that poorer families married into those that were better off,
    # and/or the other way round.

# Given that this effect for the difference in wealth is so small, and given that our data set is small to begin with,
  # it would be best to examine this effect in detail, if we were conducting a true study.
# It may be, in fact, that given that there are so few very wealthy families, such as the Strozzi or the Medici,
  # and everybody else is significantly "poorer", that this effect comes not from some conscious choice the families made,
  # but is simply there due to the distribution of wealth. In other words, there was no-one else to choose but those families whose
  # wealth was smaller, almost by default, than that of the focal family, especially given that the wealthiest families
  # have the highest degrees. It may be, however, that they have the highest degrees precisely because everybody wanted to marry their sons and daughters to
  # the members of these families. One would need to study this in more detail, then.


