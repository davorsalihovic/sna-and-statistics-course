
# 1. R ENVIRONMENT AND LOADING NETWORK DATA ------------
#    Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be                                         


# Clear your R environment
rm(list = ls())

## First have your working directories set up properly, with the data there. ##
  # To check what your current working directory is, use the following command:
getwd()

  # To setup your working directory (if you hadn't done that when you opened a new project in R Studio), use:
setwd() # in the following way: setwd("C:/some path to your chosen directory")


# Installing and loading packages in R

install.packages("igraph")
install.packages('readxl') # you can use either "quotation marks" or the 'apostrophe', but not both at the same time
                           # It can either be "readxl" or 'readxl', but not "readxl'.
install.packages("splitstackshape")
install.packages("networkD3")
install.packages("sna")
install.packages("ggraph")
install.packages("tidygraph")

  # Another alternative is to install packages from a vector:
packages <- c("igraph", "readxl", "splitstackshape", "networkD3", "sna", "ggraph", "tidygraph")
install.packages(packages)

# loading packages in R
library(igraph)
library(readxl)
library(splitstackshape)
library(network)
library(sna)
library(networkD3)
library(ggraph)
library(tidygraph)

# looking for further information about packages or particular functions
??igraph # Helps you find detailed documentation
vignette("igraph") # Returns the vignette of the library, with detailed instructions on how and what to use it for 
??"graph_from_adjacency_matrix" # Getting help with particular functions
                                    # Another way of doing this is by placing your caret (the blinking line when typing text)
                                    # on the name of the function, and then pressing "F1".



#### LOADING AND CLEANING DATA ----
relations <- read.csv("data/correspondence.csv", header = T, stringsAsFactors = F) 
  # Load the CSV file into R and create an object called "relations"
relations # Examine the file in the Console

class(relations) # Check the class of your object
# the class is "data.frame"

# Let's examine our data frame further
names(relations) # This returns the column names in your data frame
View(relations) # And this opens a new tab with your full data frame 
                  

nrow(relations) # nrow() function returns the number of rows in the data
nrow(relations[!is.na(relations$source), ])
          # Here we embed another function in the nrow() function, such that we get the number of non-empty rows
          # Remember that "!" is a LOGICAL OPERATOR meaning "NOT", and is.na() is a function that returns TRUE or FALSE,
            # depending on whether a specific entry in your data is or is not empty. 
            # What we demand here, then, is information on whether a particular entry is "NOT IS EMPTY", or isn't empty in plain English.
          # We ask the computer to show us the number of rows of the relations data frame that all have non-empty entries in the "source" column.

          # Note, also, that when going through a data frame, like any other sheet, entries are identified by their corresponding ROWS and COLUMNS.
            # So, in the our data frame:
          relations[5, 2] # means the entry at the 5th row and 2nd column
          relations[5, ] # If we leave the column index empty, in R this means that we want all columns
                           # So here we get the entire fifth row
          relations[, 2] # And, similarly, with this we get the entire second column
                          # This is the same as:
                          relations$class # because the "class" column is the second column
            
              # So, this:
              relations[relations$source == "P0002", ] # (note the double == sign, which is the sign of comparison used in R, because = is used for other things)
                  # returns ALL COLUMNS, and all the rows where relations in the source column equals P0002
                # Therefore, this:
              relations[!is.na(relations$source), ]
                  # returns ALL COLUMNS, and all the rows where relations in the source column is not empty
                    # Specifically, "return relations data frame at ROWS that contain non-empty entries in the 'source' column, 
                      # and at ALL COLUMNS".
              
              # If we, say, specified this:
              relations[!is.na(relations$source), 5]
                  # then we would not get the entire data frame (all columns), 
                  # but just column 5 AND only those entries in column five that correspond to entries in column "source" that are not empty
              
                  # This, in fact, is the same as stating:
              relations$target[!is.na(relations$source)] 
              
                  # or the same as stating:
              relations[, 5][!is.na(relations$source)]
                        # see if you can discern why these three things return the same data.
        

# What we need, then, is to filter out everything that does not belong to classes we are interested in
  # We remove everything that is not a "correspondence",
              
      # NOTE here that "class" in this instance is not an R designation for the class of an object,
        # but rather a way of discerning different types of events that we used when collecting the data
        # The R class of these entries is just "character":
              class(relations$class)
              
relations <- relations[relations$class == "correspondence",] # It should now be clear how we are using the ROW by COLUMN structure of the data here

unique(relations$class) # check what are now the unique values we have in the "class" column
                          # Again, this is the same as saying:
                          unique(relations[, 2]) # because 2nd column is the "class" column

nrow(relations) # Now re-check the number of rows of the data. It should be far smaller than what we started with.


# And now, let's unravel the "target" column
  # Given that we have multiple recipients of correspondence ("target") separated by "#",
    # we need to separate them, reformat this data into a long format,
    # and give each instance of correspondence its separate row

    # We can do this several ways. For instance, using the "dplyr" and "tidyr" packages:
relations <- relations %>% 
  mutate(target = strsplit(target, "#")) %>%
  unnest(target)

relations <- as.data.frame(relations)

    # or, even more quickly, with the splitstackshape package:
relations <- cSplit(relations, "target", sep = "#", direction = "long")
      # Be sure NOT TO RUN this line above, if you'd chosen to reformat the data
      # with tidyr and had already reformatted it.

# Getting rid of entities other than persons and other imperfections in the data
relations$target <- gsub("[", "", relations$target, fixed = T) # Remove "[" that may have been left in the "target" column, so that we get just "P000" from "[P000]"
relations$target <- gsub("]", "", relations$target, fixed = T) # Remove "]" in the same place

relations <- relations[startsWith(relations$target, "P"),] # Keep only recipients and senders that are actually persons.
relations <- relations[startsWith(relations$source, "P"),]
relations <- relations[relations$target != "NS", ] # And remove any entries that contain "NS" - NOTE that this is already performed by the command above
relations <- relations[!is.na(relations$event_id),] # Make sure to keep only the relevant observations.
relations <- relations[relations$source != relations$target,]

  # examine the data again
View(relations)
unique(relations$target)
unique(relations$source)

length(unique(relations$target)) # Get the number of recipients persons (formally, what we get is a length of the "vector", i.e. the number of unique entities in this particular column)
length(unique(relations$source)) # Get the number of senders

intersect(unique(relations$target), unique(relations$source)) # The overlap between the deponents and the accused

class(relations$date) # this is still a "character" class, but what we want is for our dates to be recognized as proper dates by the computer.
relations$date <- as.Date(relations$date) # turning our dates into the "Date" class

# Lastly, having learned how to filter out the data we want, let's turn all "NA" dates into some date
# In this case we arbitrarily decided to use 27 January 1435, as it seemed appropriate:
unique(relations$date) # Note that the only missing date we have is between 26 January and 28 January, so we pick 27 January.
relations$date[is.na(relations$date)] <- as.Date("1435-01-27")

# And ALWAYS save your data. This will save all the objects you've created,
  # as R WILL NOT AUTOMATICALLY SAVE your objects, even if you save this R script file:

  # Remove all unnecessary objects (all apart from relations): 
rm(list = setdiff(ls(), "relations"))
  # And save the data:
save.image("data.RData")
  