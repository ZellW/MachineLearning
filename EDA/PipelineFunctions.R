if(!require(easypackages)){
     install.packages("easypackages")
     library(easypackages)}
     packages("readr", "reshape2", "dplyr", "ggplot2", "corrplot", "psych", "plyr", "infotheo", prompt = FALSE)

#####################################

Fix_Date_Features <- function(data_set){
     #Looks for the feature names that are character or factor
     text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
     #Loop thru each of the feature variables found above
     for (feature_name in text_features){
          feature_vector <- as.character(data_set[,feature_name])
          # assuming date pattern: '01/11/2012'
          date_pattern <- '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]'
          #10 characters in a properly formatted date like 12/02/1961
          if(max(nchar(feature_vector)) == 10){
               #grepl returns true/false list
               if(sum(grepl(date_pattern, feature_vector)) > 0){
                    #print(paste('Casting feature to date:', feature_name))#nive to have, not needed
                    #If the feature variable makes it this far, format like a date
                    data_set[, feature_name] <-  as.Date(feature_vector, format="% d/%m/%Y")}
          }
     }
     return (data_set)
}

#####################################

wordcount <- function(str) {sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1}

#####################################Modified to capture the 1st and 2nd words

Get_Free_Text_Measures <- function(data_set, minimum_unique_threshold=0.9, features_to_ignore=c()){
     text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
     for (f_name in setdiff(text_features, features_to_ignore)){
          f_vector <- as.character(data_set[,f_name])
          if (length(unique(as.character(f_vector))) > (nrow(data_set) * minimum_unique_threshold)){
               data_set[,paste0(f_name, '_word_count')] <- sapply(strsplit(f_vector, " "), length)
               data_set[,paste0(f_name, '_character_count')] <- nchar(as.character(f_vector))
               data_set[,paste0(f_name, '_1st_word')] <- sapply(strsplit(as.character(f_vector), " "), `[`, 1)
               data_set[,paste0(f_name, '_2nd_word')] <- sapply(strsplit(as.character(f_vector), " "), `[`, 2)
               data_set[,f_name] <- NULL
          }
     }
     return(data_set)
}

#####################################

Binarize_Features <- function(data_set, features_to_ignore=c(), leave_out_one_level=FALSE, max_level_count=20){
     require(dplyr)
     text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
     for (feature_name in setdiff(text_features, features_to_ignore)){
          feature_vector <- as.character(data_set[,feature_name])
          # check that data has more than one level
          if (length(unique(feature_vector)) == 1)
               next
          # We set any non-data to text
          feature_vector[is.na(feature_vector)] <- 'NA'
          feature_vector[is.infinite(feature_vector)] <- 'INF'
          feature_vector[is.nan(feature_vector)] <- 'NAN'
          
          # only give us the top x most popular categories
          temp_vect <- data.frame(table(feature_vector)) %>% arrange(desc(Freq)) %>% head(max_level_count)
          feature_vector <- ifelse(feature_vector %in% temp_vect$feature_vector, feature_vector, 'Other')
          
          # loop through each level of a feature and create a new column
          first_level=TRUE
          for (newcol in unique(feature_vector)){
               if (leave_out_one_level & first_level){
                    # avoid dummy trap and skip first level
                    first_level=FALSE
                    next
               }
               data_set[,paste0(feature_name,"_",newcol)] <- ifelse(feature_vector==newcol,1,0)
          }
          # remove original feature
          data_set <- data_set[,setdiff(names(data_set),feature_name)]
     }
     return (data_set)
}

#####################################

Impute_Features <- function(data_set, features_to_ignore=c(), 
                            use_mean_instead_of_0=TRUE, 
                            mark_NAs=FALSE,
                            remove_zero_variance=FALSE){
     
     for(feature_name in setdiff(names(data_set), features_to_ignore)){
          #print(feature_name)#Nice but not needed
          # remove any fields with zero variance
          if(remove_zero_variance){
               if(length(unique(data_set[, feature_name]))==1){
                    data_set[, feature_name] <- NULL
                    next
               }
          }
          if(mark_NAs){
               # note each field that contains missing or bad data.  is.na catches NA and NaN
               if(any(is.na(data_set[,feature_name]))){
                    # create binary column before imputing
                    newName <- paste0(feature_name, '_NA')
                    data_set[,newName] <- as.integer(ifelse(is.na(data_set[,feature_name]),1,0)) }
               
               if(any(is.infinite(data_set[,feature_name]))){
                    newName <- paste0(feature_name, '_inf')
                    data_set[,newName] <- as.integer(ifelse(is.infinite(data_set[, feature_name]),1,0)) }
          }
          
          if (use_mean_instead_of_0){
               #Need to replace Inf with NA so that na.rm=TRUE works below 0 there is no Inf.rm; NaNs are ok as is
               data_set[is.infinite(data_set[,feature_name]),feature_name] <- NA
               data_set[is.na(data_set[,feature_name]),feature_name] <- mean(data_set[,feature_name], na.rm=TRUE)
          } else {
               #You deciced to keep 0's rather than mean
               data_set[is.na(data_set[,feature_name]),feature_name] <- 0
               data_set[is.infinite(data_set[,feature_name]),feature_name] <- 0
          }
     }
     return(data_set)
}

#####################################

Feature_Engineer_Dates <- function(data_set, remove_original_date=TRUE) {
     data_set <- data.frame(data_set)
     date_features <- names(data_set[sapply(data_set, is.Date)])#is.Date is from lubridate
     for (feature_name in date_features) {
          data_set[,paste0(feature_name,'_DateInt')] <- as.numeric(data_set[,feature_name])
          data_set[,paste0(feature_name,'_Month')] <- as.integer(format(data_set[, feature_name], "%m"))
          data_set[,paste0(feature_name,'_ShortYear')] <- as.integer(format(data_set[,feature_name], "%y"))
          data_set[,paste0(feature_name,'_LongYear')] <- as.integer(format(data_set[,feature_name], "%Y"))
          data_set[,paste0(feature_name,'_Day')] <- as.integer(format(data_set[,feature_name], "%d"))
          # week day number requires first pulling the weekday label, 
          # creating the 7 week day levels, and casting to integer
          data_set[,paste0(feature_name,'_WeekDayNumber')] <- as.factor(weekdays(data_set[, feature_name]))
          
          levels(data_set[, paste0(feature_name, '_WeekDayNumber')]) <- 
               list(Monday=1, Tuesday=2, Wednesday=3, Thursday=4, Friday=5, Saturday=6, Sunday=7)
          
          data_set[, paste0(feature_name,'_WeekDayNumber')] <- 
               as.integer(data_set[,paste0(feature_name,'_WeekDayNumber')])
          
          data_set[,paste0(feature_name,'_IsWeekend')] <-  
               as.numeric(grepl("Saturday|Sunday", weekdays(data_set[, feature_name])))
          
          data_set[,paste0(feature_name,'_YearDayCount')] <- yday(data_set[,feature_name])
          data_set[,paste0(feature_name,'_Quarter')] <- lubridate::quarter(data_set[, feature_name], with_year = FALSE)
          data_set[,paste0(feature_name,'_Quarter')] <- lubridate::quarter(data_set[, feature_name], with_year = TRUE)
          if (remove_original_date)#delete the original date column only if remove_original_date=TRUE in function call
               data_set[, feature_name] <- NULL
     }
     return(data_set)
}

#####################################

Feature_Engineer_Integers <- function(data_set, features_to_ignore=c()) {
     data_set <- data.frame(data_set)
     for (feature_name in setdiff(names(data_set), features_to_ignore)) {
          if (class(data_set[,feature_name])=='numeric' | class(data_set[,feature_name])=='integer') {
               feature_vector <- data_set[,feature_name]
               if (all((feature_vector - round(feature_vector)) == 0)) {
                    # make sure we have more than 2 values excluding NAs
                    if (nrow(data_set %>% filter_(!is.na(feature_name)) %>% distinct_(feature_name)) > 2) {
                         #print(feature_name)#not really needed/helpful
                         data_set[,paste0(feature_name,'_IsZero')] <- ifelse(data_set[,feature_name]==0,1,0)
                         data_set[,paste0(feature_name,'_IsPositive')] <- ifelse(data_set[,feature_name]>=0,1,0)
                         # separate data into two bins using infotheo
                         data_discretized <- discretize(data_set[,feature_name], disc='equalfreq', nbins=2)
                         data_set[,paste0(feature_name,'_2Bins')] <- data_discretized$X
                         if (nrow(data_set %>% filter_(!is.na(feature_name)) %>% distinct_(feature_name)) > 4) {
                              # try 4 bins
                              data_discretized <- discretize(data_set[,feature_name], disc='equalfreq', nbins=4)
                              data_set[,paste0(feature_name,'_4Bins')] <- data_discretized$X
                         }
                    }
               }
          }
     }
     return (data_set)
}

#####################################

Feature_Engineer_Numbers <- function(data_set, features_to_ignore=c()) {
     data_set <- data.frame(data_set)
     date_features <- setdiff(names(data_set[sapply(data_set, is.numeric)]), features_to_ignore)
     for (feature_name in date_features) {
          feature_vector <- data_set[,feature_name]
          if (is.integer(feature_vector) | is.numeric(feature_vector)) {
               if (any((feature_vector - round(feature_vector)) != 0)) {
                    # make sure we have more than 2 values excluding NAs
                    if (nrow(data_set %>% filter_(!is.na(feature_name)) %>% distinct_(feature_name)) > 2) {
                         # print(feature_name)#Not really helpful
                         # polynomial transformation
                         poly_vector <- poly(x=feature_vector, degree = 2)
                         data_set[,paste0(feature_name, "_poly1")] <- poly_vector[,1]
                         data_set[,paste0(feature_name, "_poly2")] <- poly_vector[,2]
                         # log transform
                         data_set[,paste0(feature_name, "_log")] <- log(x = feature_vector)
                         # exponential transform
                         data_set[,paste0(feature_name, "_exp")] <- exp(x = feature_vector)
                         # rounding
                         data_set[,paste0(feature_name, "_rnd")] <- round(x = feature_vector, digits = 0)
                         # binning into 2 bins
                         data_discretized <- discretize(data_set[,feature_name], disc='equalfreq', nbins=2)
                         data_set[,paste0(feature_name,'_2Bins')] <- data_discretized$X
                    }
               }
          }
     }
     return(data_set)
}

#####################################

Get_Top_Relationships <- function(data_set, correlation_abs_threshold=0.8, pvalue_threshold=0.01) {
     feature_names <- names(data_set)
     # strip var names to index for pair-wise identification     
     names(data_set) <- seq(1:ncol(data_set))     
     # calculate correlation and significance numbers     
     cor_data_df <-  psych::corr.test(data_set)
     # apply var names to correlation matrix over index     
     rownames(cor_data_df$r) <- feature_names
     colnames(cor_data_df$r) <- feature_names
     # top cor and sig
     relationships_set <- cor_data_df$ci[,c('r','p')]
     # apply var names to data over index pairs
     relationships_set$feature_1 <- feature_names[as.numeric(sapply(strsplit(rownames(relationships_set), "-"), `[`, 1))]
     relationships_set$feature_2 <- feature_names[as.numeric(sapply(strsplit(rownames(relationships_set), "-"), `[`, 2))]
     relationships_set <- select(relationships_set, feature_1, feature_2, r, p) %>% dplyr::rename(correlation=r, pvalue=p)
     # return only the most insteresting relationships
     return(filter(relationships_set, abs(correlation) > correlation_abs_threshold | pvalue < pvalue_threshold) %>%
                 arrange(pvalue))
}

#####################################

Identify_Outliers <- function(data_set, features_to_ignore=c(), 
                              outlier_sd_threshold = 2, remove_outlying_features = FALSE) {
     # get standard deviation for each feature
     outliers <- c()
     for (feature_name in setdiff(names(data_set),features_to_ignore)) {
          feature_mean <- mean(data_set[,feature_name], na.rm = TRUE)
          feature_sd <- sd(data_set[,feature_name], na.rm = TRUE)
          outlier_count <- sum(data_set[,feature_name] > (feature_mean + (feature_sd * outlier_sd_threshold)) | 
                                    data_set[,feature_name] < (feature_mean - (feature_sd * outlier_sd_threshold))) 
          if (outlier_count > 0) {
               outliers <- rbind(outliers, c(feature_name, outlier_count))
               if (remove_outlying_features)
                    data_set[, feature_name] <- NULL
          }
     } 
     outliers <- data.frame(outliers) %>% rename(feature_name=X1, outlier_count=X2) %>% 
          mutate(outlier_count=as.numeric(as.character(outlier_count))) %>% arrange(desc(outlier_count))
     if (remove_outlying_features) {
          return(data_set)
     } else {
          return(outliers)
     }
} 