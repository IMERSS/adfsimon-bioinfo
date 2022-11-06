###############
## TEST DATA ##
###############

# Test data works fine... suppress to work with your data

# Then, load the grid (shapefile). This is a grid of 0.25 degree lat/long of Cerrado.
# shape <- readOGR("betagrid/inputs/test_data/final_shape.shp")

# Load the species occurrences. These are the occurrences (not real, just as example) of some mammals of Cerrado.
# occurrences <- read.table("betagrid/inputs/test_data/occur.cerr.txt", row.names=1, head=T)

# And load the phylogeny of these species (if you want to calculate phylogenetic beta diversity):
# phylo <- read.tree("betagrid/inputs/test_data/phylo.txt")

# We have to know which features corresponds to the longitude (x) and latitude (y). Type the following code and observe that the second (2) feature corresponds to longitude while the third (3) corresponds to latitude. We need these numbers.
# names(shape)

# Call the function and get results! Let us calculate beta diversity for each focal cell. Note that the function will return results containing four columns: number of grid cell, the mean turnover partition of beta diversity, the mean nestedness partition of beta diversity, and the mean total beta diversity. Also, note that radius equals 0.25 degree, which is the same size as the resolution of our grid. This will make the function use only the 8 (or fewer) adjacent cells in relation to the focal cells. If you want more neighbor cells to be included in the analysis, you can use the double (0.5 in this example) or greater values.

# results <- betagrid(gridshp=shape, comp=occurrences, xfeature=2, yfeature=3, radius=0.25, index="sorensen")

#### GRAPH ####

# Create a new layer in our grid file for the mean total beta diversity.
# shape$betadiv <- results[,4]

# Now create a raster with the same extent and resolution as our previous grid (in our example, 0.25 degree lat/long):
# emptyraster <- raster(extent(shape))
# res(emptyraster)=0.25

# Assign values to the new raster according to the beta diversity layer in our shapefile.
# rbeta <- rasterize(shape, field="betadiv", emptyraster)

# Make a cool color palette:
# my.colors = colorRampPalette(c("white","lightblue", "yellow","orangered", "red"))

# Plot the map
# plot(rbeta, col=my.colors(255), frame.plot=F, axes=F, box=F, add=F, legend.width=0.8, legend.shrink=1)
