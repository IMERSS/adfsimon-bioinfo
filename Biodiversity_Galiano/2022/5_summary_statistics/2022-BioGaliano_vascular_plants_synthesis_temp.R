# Vascular plants

Taxa.VAS <- c('VAS')

VAS.count <- data.frame(Taxa.VAS,Reported,Confirmed,New,Total,Prop.confirmed,Prop.new)
names(VAS.count)[names(VAS.count) == 'Taxa.VAS'] <- 'Taxon'

VAS <- ALGAE_PLANTS %>% dplyr::filter(Stats.Code == 'VAS')
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
VAS.count$new.prior.2015 <- VAS.count$New - (VAS.count$new.2015+VAS.count$new.2016+VAS.count$new.2017+VAS.count$new.2018+VAS.count$new.2019+VAS.count$new.2020+VAS.count$new.2021)

VAS.count

VAS.count.chronology <- VAS.count %>% pivot_longer(cols = new.2015:new.2021)
VAS.count.chronology <- VAS.count.chronology %>% dplyr::select(Taxon,name,value)

colnames(VAS.count.chronology)<- c("Taxon","Year","New.spp")

VAS.count.chronology$Year <- as.numeric(substrRight(VAS.count.chronology$Year, 4))

VAS.count.chronology
