

# Exploratory analysis of Howe Sound vascular plant diversity

# Implementing gridded beta diversity analysis of Howe Sound vegetation data based on:
# https://rfunctions.blogspot.com/2015/08/calculating-beta-diversity-on-grid.html

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

shape <- readOGR("spatial_data/vectors/1km_grid_WGS84_coordinates_x_vascular_plant_grid_NA_omit.shp")

# Read species occurrences

data <- read.csv("tabular_data/1km_gridded_vascular_plant_records_2022-12-24_WGS84.csv")

# Subset relevant fields

head(data)

data <- data %>% dplyr::select('scientific'|'id')

nrow(data)

# Add count field to generate matrix

data$count <- 1

# Generate matrix 

matrix <- ecodist::crosstab(data$id, data$scientific, data$count)

# Convert to presence / absence

matrix[matrix > 0] <- 1 

# Compare dimensions of matrix and grid cells

nrow(matrix) # matrix with 771 rows matching 771 grid cells found in shape below
nrow(shape2)  # shape with 771 grid cells 

# Which fields correspond with LONG (3) & LAT (2)? 

names(shape)
str(shape)

# Call the function and get results! Let us calculate beta diversity for each focal cell. Note that the function will return results containing four columns: number of grid cell, the mean turnover partition of beta diversity, the mean nestedness partition of beta diversity, and the mean total beta diversity. Also, note that radius equals 0.25 degree, which is the same size as the resolution of our grid. This will make the function use only the 8 (or fewer) adjacent cells in relation to the focal cells. If you want more neighbor cells to be included in the analysis, you can use the double (0.5 in this example) or greater values.

results <- betagrid(gridshp=shape, comp=matrix, xfeature=3, yfeature=2, radius=0.25, index="sorensen")

# Standardize results fields to merge with grid in QGIS

names(results) <- c('id','mean_turnover','mean_nestedness','mean_beta')

# Add species richness to results

matrix <- matrix %>% mutate(richness = rowSums(.))

matrix$richness

matrix$id <- row.names(matrix) 

results$richness  <- matrix$richness[match(unlist(results$id), matrix$id)]

# Output results

write.csv(results,"outputs/betagrid_vascular_plants.csv", row.names = FALSE)

#### GRAPH ####
# Note: code not working due to projection

# Create a new layer in our grid file for the mean total beta diversity.
shape$betadiv <- results[,4]

# writeOGR(shape, dsn = "/Users/andrewsimon/GitHub/bioinformatics/adfsimon-bioinfo/Biodiversity_Galiano/2022/3_analyze/Plantae_et_Chromista/vascular_plants/gridded_analysis_mydata/", 
# layer = "betagrid_plants.shp", driver = "ESRI Shapefile")

# Now create a raster with the same extent and resolution as our previous grid (in our example, 0.25 degree lat/long):
emptyraster <- raster(extent(shape))
res(emptyraster)=1000

# Assign values to the new raster according to the beta diversity layer in our shapefile.
rbeta <- rasterize(shape, field="betadiv", emptyraster)

# Make a cool color palette:
my.colors = colorRampPalette(c("white","lightblue", "yellow","orangered", "red"))

# Plot the map
plot(rbeta, col=my.colors(255), frame.plot=F, axes=F, box=F, add=F, legend.width=0.8, legend.shrink=1)


## Ordination of gridded vascular plant data

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

# Read labeled grid dataset

labeled.grid <- read.csv("betagrid/outputs/betagrid_vascular_plants_labeled_grid_cells_geo_attributes_2022-11-09.csv")

# Remove rows with <2 taxa

matrix <- matrix[rowSums(matrix[])>2,]

nrow(matrix)

# Create environment dataframe that matches cell IDs with labels

env <- matrix

env$id <- row.names(matrix)

env$label <-  labeled.grid$label[match(unlist(env$id), labeled.grid$id)]

env <- env[,730:731]

env$geo <-  labeled.grid$geo[match(unlist(env$id), labeled.grid$id)]

#### COMMUNITY ORDINATION PLOTS ####

## Consider ordinations against environmental variables
## http://kembellab.ca/r-workshop/biodivR/SK_Biodiversity_R.html

## Create MDS Jaccard similarity Index for Site Commmunity Data

matrix.MDS <- metaMDS(matrix, distance = "jaccard", k = 2, trymax = 100, zerodist = "add")

stressplot(matrix.MDS)

str(matrix.MDS)

#### CLASSIC nMDS Plot

ordiplot(matrix.MDS,type="p")
ordiplot (matrix.MDS, display = 'sites', type = 'p')
ordihull (matrix.MDS, groups = env$geo, lty = 'dotted')

### Species contributing to clustering

Galiano.spp.fit <- envfit(matrix.MDS, matrix, permutations = 999) 
head(Galiano.spp.fit)

# Plot with hulls based on geographic position

ordiplot(matrix.MDS, type = "n", main = "hulls")
orditorp(matrix.MDS, display = "sites", labels = F, pch = c(16, 8, 17, 18, 10, 1, 19, 14) [as.numeric(env$geo)], col = c("green", "blue", "orange", "black", "red", "brown", "pink", "purple") [as.numeric(env$geo)], cex = 1)
ordihull(matrix.MDS, groups = env$geo, draw = "polygon", lty = 1, col = "grey90")
legend(x="bottomleft", legend=levels(env$geo), col=env$geo, pch=env$geo)