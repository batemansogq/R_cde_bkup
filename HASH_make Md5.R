######
# create MD5 hash for use in cloudera
######

# hashing library
library(digest)

#load in data
da_csv <- read.csv2("E://R/Football/ANZ/FB_Ref_data_teams.csv", sep=","
                    , header = TRUE, stringsAsFactors = FALSE, 
                    na.strings = "NA") 

da_df <- as.data.frame(da_csv)


da_df$MD5Hash <- sapply(da_df$Ladder, digest, algo="md5")

head(da_df)

write.table(da_df, file=paste0("E://R/Football/ANZ/FB_Ref_data_teams_HASH.txt"), sep=","
            , row.names = FALSE)