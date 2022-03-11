### create wq input table ###

#read in all data from WQP
wq_path <- "O:/PRIV/NERL_ORD_CYAN/Sentinel2/Matchups/WQP/20180710"
out_path <- wq_path


sta <- read.csv(file.path(wq_path, "station.csv"), stringsAsFactors=FALSE)
bio <- read.csv(file.path(wq_path, "biologicalresult.csv"), stringsAsFactors=FALSE)

#create unique ID for sta and for bio
sta$sta_uniqueID <- 1:nrow(sta)
bio$bio_uniqueID <- 1:nrow(bio)


### remove empty fields - for each field, if one unique value, and if that value is NA, add to vector of field names. then create new data frame with all fields but those ones.
#also create table showing how many rows are populated in each field.
#also manually remove some fields

#create copies in case needed
sta_orig <- sta
bio_orig <- bio

# sta
nrow(sta) #
sta_fields <- data.frame()
sta_NA <- c()
for (col in 1:ncol(sta)) {
  sta_fields <- rbind(sta_fields, data.frame(
    field=colnames(sta)[col], 
    nonNARows=length(sta[,col]) - sum(is.na(sta[,col])),
    nonEmptyRows=length(sta[,col]) - length(sta[sta[,col]=="",col]),
    nonSpaceRows=length(sta[,col]) - length(sta[sta[,col]==" ",col]),
    uniqueValues=length(unique(sta[,col]))))
  if (length(unique(sta[,col]))==1) {
    if (is.na(unique(sta[,col]))) {
      sta_NA <- c(sta_NA, colnames(sta[col]))
    }
  }
  for (row in 1:length(sta[,col])) {
    #replace no text cells with NA
    if (!is.na(sta[row,col])) {
      if (sta[row,col]=="") {
        sta[row,col] <- NA
      }
    }
    #replace new line characters with *n*
    sta[row,col] <- gsub("\n", " *n* ", sta[row,col])
    sta[row,col] <- gsub("\\|", ";", sta[row,col])
    
  }
}
sta <- sta[, !(names(sta) %in% sta_NA)]

#manually remove columns
sta_rem <- c("DrainageAreaMeasure.MeasureValue",
             "DrainageAreaMeasure.MeasureUnitCode",
             "ContributingDrainageAreaMeasure.MeasureValue",
             "ContributingDrainageAreaMeasure.MeasureUnitCode",
             "CountryCode",
             "ConstructionDateText",
             "AquiferTypeName") #could also do "Vertical..." fields
sta <- sta[, !(names(sta) %in% sta_rem)]

sta_fields$status <- NA
for (row in 1:nrow(sta_fields)) {
  if (sta_fields$field[row] %in% colnames(sta)) {
    sta_fields$status[row] = "kept"
  } else {
    sta_fields$status[row] = "removed"
  }
}

write.csv(sta_fields, file.path(out_path, "sta_fields.csv"))


# bio
nrow(bio) #29635
bio_fields <- data.frame()
bio_NA <- c()
for (col in 1:ncol(bio)) {
  bio_fields <- rbind(bio_fields, data.frame(
    field=colnames(bio)[col], 
    nonNARows=length(bio[,col]) - sum(is.na(bio[,col])),
    nonEmptyRows=length(bio[,col]) - length(bio[bio[,col]=="",col]),
    nonSpaceRows=length(bio[,col]) - length(bio[bio[,col]==" ",col]),
    uniqueValues=length(unique(bio[,col]))))
  if (length(unique(bio[,col]))==1) {
    if (is.na(unique(bio[,col]))) {
      bio_NA <- c(bio_NA, colnames(bio[col]))
    }
  }
  for (row in 1:length(bio[,col])) {
    #replace no text cells with NA
    if (!is.na(bio[row,col])) {
      if (bio[row,col]=="") {
        bio[row,col] <- NA
      }
    }
    #replace new line characters with *n*
    bio[row,col] <- gsub("\n", " *n* ", bio[row,col])
    bio[row,col] <- gsub("\\|", ";", bio[row,col])
  }
  # THIS TAKES FOREVER (> 8 hours on 46,000 records!!!)
}
bio <- bio[, !(names(bio) %in% bio_NA)]

'
#manually remove columns
bio_rem <- c("ActivityMediaName",
             "ActivityEndDate",
             "ActivityEndTime.Time",
             "ActivityEndTime.TimeZoneCode",
             "ActivityLocation.LatitudeMeasure",
             "ActivityLocation.LongitudeMeasure",
             "ActivityLocation.SourceMapScaleNumeric",
             "ActivityLocation.HorizontalAccuracyMeasure.MeasureValue",
             "ActivityLocation.HorizontalAccuracyMeasure.MeasureUnitCode",
             "MeasureQualifierCode",
             "StatisticalBaseCode",
             "AnalysisStartTime.Time",
             "AnalysisStartTime.TimeZoneCode",
             "AnalysisEndDate",
             "AnalysisEndTime.Time",
             "AnalysisEndTime.TimeZoneCode")
bio <- bio[, !(names(bio) %in% bio_rem)]
'

bio_fields$status <- NA
for (row in 1:nrow(bio_fields)) {
  if (bio_fields$field[row] %in% colnames(bio)) {
    bio_fields$status[row] = "kept"
  } else {
    bio_fields$status[row] = "removed"
  }
}

write.csv(bio_fields, file.path(out_path, "bio_fields.csv"))

### merge bio and sta tables: for each row in the combined bio table, add a row to the "input" dataframe with lat, long, state, and all columns from bio
#also add unique ID
input <- data.frame() #create empty dataframe to be filled
for (row in 1:nrow(bio)) {
  input <- rbind(input, data.frame(sta[which(sta$MonitoringLocationIdentifier==bio$MonitoringLocationIdentifier[row]),], bio[row,]))
}

### write the input, and sta and bio in case unique IDs needed, to csvs
write.csv(input, file.path(out_path, "input.csv"))
write.csv(sta, file.path(out_path, "sta.csv"))
write.csv(bio, file.path(out_path, "bio.csv"))



#################



#test to make sure it was written properly
incsv <- read.csv(file.path(wq_path, "input.csv"))

### this was all for using write.table; write.csv appears to work just fine
#write.table(input, file.path(wq_path, "matchup_automation_input_ALL.txt"), quote = FALSE, row.names = FALSE, sep="|")


#to check
#import <- read.table(file.path(wq_path, "matchup_automation_input_ALL.txt"), sep="|", header=TRUE, nrows=29635, row.names=NULL, fill=TRUE) #only reads 16289 rows of 29635

nrow(import)
import$uniqueID
tail(levels(import$uniqueID))
###


#####

#investigate ActivityIdentifier
id_df <- data.frame()
for (id in unique(bio$ActivityIdentifier)) {
  id_df <- rbind(id_df, data.frame(id=id, count=length(which(bio$ActivityIdentifier==id))))
}

id_df_sub <- subset(id_df, id_df$count>1)
AI3 <- id_df_sub[id_df_sub$count>2,] #look at those with 3 entries
bio[bio$ActivityIdentifier==AI3$id[3],] #look at one such Act.Id.

