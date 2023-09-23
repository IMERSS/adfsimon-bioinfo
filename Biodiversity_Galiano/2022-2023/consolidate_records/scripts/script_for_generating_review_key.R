key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(iNaturalist.observations.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- iNaturalist.observations.names.unmatched.unmatched$scientificName

unmatched.taxa$Genus <- word(iNaturalist.observations.names.unmatched.unmatched$scientificName, 1)

unmatched.taxa$Species <- word(iNaturalist.observations.names.unmatched.unmatched$scientificName, 2)

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(iNaturalist.observations.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review.key.csv")