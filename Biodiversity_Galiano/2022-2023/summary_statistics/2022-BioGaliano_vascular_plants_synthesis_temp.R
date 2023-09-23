# Vascular plants summary stats

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## Load R packages

library(dplyr)
library(gridExtra)
library(readr)
library(tidyverse)
library(viridis)

# Custom R function:

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Load vascular plants summary

PLANTS <- read.csv("../2_review/Plantae_et_Chromista/vascular_plants/summaries/Tracheophyta_review_summary_reviewed_2022-10-31.csv")

# First set up basic vectors for dataframes

Reported <- c(NA)
Confirmed <- c(NA)
New <- c(NA)
Total <- c(NA)
Prop.confirmed <- c(NA)
Prop.new <- c(NA)

Taxa.VAS <- c('VAS')

VAS.count <- data.frame(Taxa.VAS,Reported,Confirmed,New,Total,Prop.confirmed,Prop.new)
names(VAS.count)[names(VAS.count) == 'Taxa.VAS'] <- 'Taxon'

VAS <- PLANTS %>% dplyr::filter(Stats.Code == 'VAS')
VAS.reported <- VAS %>% dplyr::filter(Reporting.Status == 'reported')
VAS.confirmed <- VAS %>% dplyr::filter(Reporting.Status == 'confirmed')
VAS.new <- VAS %>% dplyr::filter(grepl("new",Reporting.Status))

VAS.count$Reported <- nrow(VAS.reported)
VAS.count$Confirmed <- nrow(VAS.confirmed)
VAS.count$New <- nrow(VAS.new)
VAS.count$Total <- nrow(VAS)
VAS.total.reported = nrow(VAS.reported) + nrow(VAS.confirmed)
VAS.count$Prop.confirmed = VAS.count$Confirmed/VAS.total.reported
VAS.count$Prop.new <- VAS.count$New/VAS.count$Total

VAS.count$new.2015 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2015"))
VAS.count$new.2016 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2016"))
VAS.count$new.2017 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2017"))
VAS.count$new.2018 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2018"))
VAS.count$new.2019 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2019"))
VAS.count$new.2020 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2020"))
VAS.count$new.2021 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2021"))
VAS.count$new.2022 <- sum(str_count(VAS.new$Reporting.Status, pattern = "2022"))
VAS.count$new.prior.2015 <- VAS.count$New - (VAS.count$new.2015+VAS.count$new.2016+VAS.count$new.2017+VAS.count$new.2018+VAS.count$new.2019+VAS.count$new.2020+VAS.count$new.2021+VAS.count$new.2022)

VAS.count

VAS.count.chronology <- VAS.count %>% pivot_longer(cols = new.2015:new.2022)
VAS.count.chronology <- VAS.count.chronology %>% dplyr::select(Taxon,name,value)

colnames(VAS.count.chronology)<- c("Taxon","Year","New.spp")

VAS.count.chronology$Year <- as.numeric(substrRight(VAS.count.chronology$Year, 4))

VAS.count.chronology

# Plot species richness by year

historical.summary <- read.csv("2022_Vascular_plants_historical_summary.csv")

str(historical.summary)

nrow(historical.summary)

historical.summary <- distinct(historical.summary, PERIOD, .keep_all = TRUE)

head(historical.summary)


ggplot(data=historical.summary, aes(x=PERIOD, y=Richness)) +
  geom_line()+
  geom_point()
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_path()+
  geom_point()

p <- ggplot(XN.O.Taxon.Plot,aes(Sample, Value, fill=Taxa))
p <- p + geom_bar(stat="identity") + facet_grid(. ~ Type, drop=TRUE, scale = "free", space = "free_x")
p <- p + theme_minimal() + ylab("Proportions")
p <- p + scale_y_continuous(expand = c(0,0)) + theme(strip.background = element_rect(fill="white"))+theme(panel.spacing = unit(0.3, "lines"))
p <- p + scale_fill_manual(values = order.colors)
p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p <- p + theme(axis.title.x = element_text(size = 14, face = "bold"))
p <- p + theme(axis.text.x = element_text(size = 12))
p <- p + theme(axis.title.y = element_text(size = 14, face = "bold"))
p <- p + theme(axis.text.y = element_text(size = 12))
p <- p + theme(legend.title = element_text(size = 14))
p <- p + theme(legend.text = element_text(size = 12))
p

