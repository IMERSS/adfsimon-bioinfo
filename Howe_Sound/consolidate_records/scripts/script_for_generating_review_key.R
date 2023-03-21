key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(LGL.2020.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- LGL.2020.names.unmatched.unmatched$scientificName

unmatched.taxa$Genus <- word(LGL.2020.names.unmatched.unmatched$scientificName, 1)

unmatched.taxa$Species <- word(LGL.2020.names.unmatched.unmatched$scientificName, 2)

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(LGL.2020.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review.key.csv")