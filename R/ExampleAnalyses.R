
##
## Author Astrid Alvarez
##


## Library ape to read trees
library(jrich)

## set working directory to R source
## You must change it to your own directory




## change directory to data folder

setwd("../data/")



# read localities

loc <- read.delim("localidades.txt")

#head(loc)

#tail(loc)

# lit of trees and distributions

phylo <- list.files(,pattern="_tree")

comm <- list.files(,pattern="_samp")



## create a multitaxon object

allTaxaAndDistributions <- list()


for(i in length(phylo)){
## Read tree

tree <- read.tree (phylo[i])

## Read distribution

distrib <- read.delim(comm[i])

samp <- t(distrib[,2:dim(distrib)[2]])

samp <- data.frame(samp)

colnames(samp) <- distrib[,1]

spp <- colnames(distrib)[2:dim(distrib)[2]]

samp$species <- spp

row.names(samp) <- c()

distrib <- samp

match(colnames(distrib),loc$Localidad)


##list file

taxon1 <- list()
taxon1[[1]] <- tree
taxon1[[2]] <- distrib

nm <- paste0("taxon",i)
assign(nm,taxon1)

allTaxaAndDistributions[[length(allTaxaAndDistributions)+1]] <- assign(nm,taxon1)

}


## check the object

str(allTaxaAndDistributions)

###### Checking names

q <- match(tree$tip.label,distrib$species)
qq <- na.omit(q)
distrib$species[attr(qq,"na.action")]

p <- match(distrib$species,tree$tip.label)
pp <- na.omit(p)
distrib$species[attr(pp,"na.action")]


######




########### Index calculation

## initial ranking for Multitaxon

InitialValues <-  Multi.Index.Calc(allTaxaAndDistributions)

InitialValues 

setwd("../results/")
write.csv(InitialValues,"Results.csv")

## correlation between values

cor(InitialValues[,2:10],InitialValues[,2:10])

#Plot the initial Values, I index

library(ggplot2)

vector = InitialValues$I

qplot(InitialValues$area,vector, xlab = "Areas", ylab = "I index",
main = "I index value")


