# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(betapart)
library(CommEcol)
library(dplyr)
library(ecodist)
library(picante)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(tidyr)
library(vegan)

# Read spatially explicit biodiversity data

data <- read.csv("Galiano_vascular_plants_x_1km_grid_TPI_extent_2022-10-10.csv")

# Subset relevant fields

data <- data %>% dplyr::select('Taxon'|'id_2')

# Add count field to generate matrix

data$Count <- 1

# Generate matrix 

matrix <- ecodist::crosstab(data$id_2, data$Taxon, data$Count)

# Convert to presence / absence

matrix[matrix > 0] <- 1 

# Turns out it is not straightforward to visualize beta diversity in a spatially explicit framework... SOOO...

# Let's play with the function from this site: https://rfunctions.blogspot.com/2015/08/calculating-beta-diversity-on-grid.html

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

###################

# Then, load the grid (shapefile). This is a grid of 0.25 degree lat/long of Cerrado.
shape <- readOGR("grided_analysis_test/final_shape.shp")

# Load the species occurrences. These are the occurrences (not real, just as example) of some mammals of Cerrado.
occurrences <- read.table("grided_analysis_test/occur.cerr.txt", row.names=1, head=T)

# And load the phylogeny of these species (if you want to calculate phylogenetic beta diversity):
phylo <- read.tree("grided_analysis_test/phylo.txt")

# We have to know which features corresponds to the longitude (x) and latitude (y). Type the following code and observe that the second (2) feature corresponds to longitude while the third (3) corresponds to latitude. We need these numbers.
names(shape)

# Call the function and get results! Let us calculate beta diversity for each focal cell. Note that the function will return results containing four columns: number of grid cell, the mean turnover partition of beta diversity, the mean nestedness partition of beta diversity, and the mean total beta diversity. Also, note that radius equals 0.25 degree, which is the same size as the resolution of our grid. This will make the function use only the 8 (or fewer) adjacent cells in relation to the focal cells. If you want more neighbor cells to be included in the analysis, you can use the double (0.5 in this example) or greater values.

results <- betagrid(gridshp=shape, comp=occurrences, xfeature=2, yfeature=3, radius=0.25, index="sorensen")

#### GRAPH ####

# Create a new layer in our grid file for the mean total beta diversity.
shape$betadiv <- results[,4]

# Now create a raster with the same extent and resolution as our previous grid (in our example, 0.25 degree lat/long):
emptyraster <- raster(extent(shape))
res(emptyraster)=0.25

# Assign values to the new raster according to the beta diversity layer in our shapefile.
rbeta <- rasterize(shape, field="betadiv", emptyraster)

# Make a cool color palette:
my.colors = colorRampPalette(c("white","lightblue", "yellow","orangered", "red"))

# Plot the map
plot(rbeta, col=my.colors(255), frame.plot=F, axes=F, box=F, add=F, legend.width=0.8, legend.shrink=1)


# Adapting the above code to implement the same gridded analysis of my data:

# Then, load the grid (shapefile). This is a grid of 0.25 degree lat/long of Cerrado.
shape <- readOGR("grided_analysis_test/final_shape.shp")


