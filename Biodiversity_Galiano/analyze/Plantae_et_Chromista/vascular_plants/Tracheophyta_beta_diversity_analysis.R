# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(betapart)
library(CommEcol)
library(RColorBrewer)
library(dplyr)
library(ecodist)
library(picante)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(terra)
library(tidyr)
library(vegan)


# Implementing gridded beta diversity analysis using the function and test data from this site: 
# https://rfunctions.blogspot.com/2015/08/calculating-beta-diversity-on-grid.html

#### FUNCTION: copy and paste into R ####

betagrid<-function(gridshp, comp, xfeature, yfeature, radius, phylotree, phylobeta=F, index="sorensen"){
  data<-data.frame(gridshp[xfeature],gridshp[yfeature],comp)
  mean_turnover<-numeric(length(comp[,1]))
  mean_nestedness<-numeric(length(comp[,1]))
  mean_beta<-numeric(length(comp[,1]))
  for(i in 1:length(gridshp[[2]])){
    adj<-select.window(xf=data[i,1], yf=data[i,2], radius, xydata=data)[,-c(1,2)]
    if(phylobeta==F){
      ifelse(sum(nrow(adj))==0 || ncol(adj)==0, res<-0 , res<-beta.pair(adj, index.family=index))
    }else if(phylobeta==T){
      ifelse(sum(nrow(adj))==0 || ncol(adj)==0, res<-0 , res<-phylo.beta.pair(adj, phylotree, index.family=index))
    }
    ifelse(sum(nrow(adj))==0 || ncol(adj)==0, mean_turnover[i]<-0 , mean_turnover[i]<-mean(as.matrix(res[[1]])[2:length(as.matrix(res[[1]])[,1]),1],na.rm=TRUE) )
    ifelse(sum(nrow(adj))==0 || ncol(adj)==0, mean_nestedness[i]<-0 , mean_nestedness[i]<-mean(as.matrix(res[[2]])[2:length(as.matrix(res[[2]])[,1]),1],na.rm=TRUE) )
    ifelse(sum(nrow(adj))==0 || ncol(adj)==0, mean_beta[i]<-0 , mean_beta[i]<-mean(as.matrix(res[[3]])[2:length(as.matrix(res[[3]])[,1]),1],na.rm=TRUE) )
  }
  return(data.frame(cell=row.names(comp), mean_turnover, mean_nestedness, mean_beta))
}

#############
## MY DATA ##
#############

# Adapting the above code to implement gridded beta diversity analysis of my data:

# Load the grid
shape <- readOGR("betagrid/inputs/1km_grid_WGS84_overlap_vascular_plants_2022-11-04.shp")

# Converting to UTM breaks the function 'betagrid' below
# shape <- vect(shape, crs="+proj=utm +zone=10 +datum=WGS84  +units=m")

# Read species occurrences

data <- read.csv("betagrid/inputs/Galiano_vascular_plants_intersect_1km_grid_WGS84.csv")

# Subset relevant fields

data <- data %>% dplyr::select('Taxon'|'id')

# Add count field to generate matrix

data$Count <- 1

# Generate matrix 

matrix <- ecodist::crosstab(data$id, data$Taxon, data$Count)

# Convert to presence / absence

matrix[matrix > 0] <- 1 

# Which fields correspond with LONG (6) & LAT (7)? 

names(shape)
str(shape)

# Call the function and get results! Let us calculate beta diversity for each focal cell. Note that the function will return results containing four columns: number of grid cell, the mean turnover partition of beta diversity, the mean nestedness partition of beta diversity, and the mean total beta diversity. Also, note that radius equals 0.25 degree, which is the same size as the resolution of our grid. This will make the function use only the 8 (or fewer) adjacent cells in relation to the focal cells. If you want more neighbor cells to be included in the analysis, you can use the double (0.5 in this example) or greater values.

results <- betagrid(gridshp=shape, comp=matrix, xfeature=6, yfeature=7, radius=0.25, index="sorensen")

# Standardize results fields to merge with grid in QGIS

names(results) <- c('id','mean_turnover','mean_nestedness','mean_beta')

# Add species richness to results

matrix <- matrix %>% mutate(richness = rowSums(.))

matrix$richness

matrix$id <- row.names(matrix) 

results$richness  <- matrix$richness[match(unlist(results$id), matrix$id)]

# Output results

# write.csv(results,"betagrid/outputs/betagrid_vascular_plants.csv")


#### SCRIPT PARTLY RECOVERED; SHOULDN'T TAKE TOO MUCH WORK TO GET ORDINATION WORKING AGAIN...
# Load ENV metadata?


## Ordination of gridded vascular plant data

env$geo <-  labeled.grid$geo[match(unlist(env$id), labeled.grid$id)]

#### COMMUNITY ORDINATION PLOTS ####

# First remove grid cells with <30 species

matrix <- matrix[rowSums(matrix[])>=30,]

## Consider ordinations against environmental variables
## http://kembellab.ca/r-workshop/biodivR/SK_Biodiversity_R.html

head(Galiano.spp.fit)
ordiplot(matrix.MDS, type = "n", main = "hulls")
orditorp(matrix.MDS, display = "sites", labels = F, pch = c(16, 8, 17, 18, 10, 1, 19, 14) [as.numeric(env$geo)], col = c("green", "blue", "orange", "black", "red", "brown", "pink", "purple") [as.numeric(env$geo)], cex = 1)
ordihull(matrix.MDS, groups = env$geo, draw = "polygon", lty = 1, col = "grey90")
legend(x="bottomleft", legend=levels(env$geo), col=env$geo, pch=env$geo)
legend(x="bottomleft", legend=levels(env$geo), col=env$geo, pch=env$geo)

# Save NMDS results into dataframe

site.scrs <- as.data.frame(scores(matrix.MDS, display = "sites")) 

site.scrs <- cbind(site.scrs, id = rownames(site.scrs)) #add site names as variable if you want to display on plot

site.scrs$id <- as.integer(site.scrs$id)

# Bind NMDS scores with environmental metadata

env$id <- as.integer(env$id)

matrix.MDS.results <- left_join(env, site.scrs, by = 'id')

# Concatenate grid cell labels

matrix.MDS.results$label_new <- paste(matrix.MDS.results$id, matrix.MDS.results$label, sep = "", collapse = NULL)

# Plot using GGPlot

MDS.plot <- ggplot(matrix.MDS.results, aes(x=NMDS1, y=NMDS2, label=label_new))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(matrix.MDS.results$geo)), size = 2)+ #adds site points to plot
  geom_text(hjust=0, vjust=0)+
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "GEO", shape = "GEO")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

MDS.plot + labs(title = "Galiano Island Vascular Plant Communities") #displays plot