
descriptive_summary_function <- function(df){
  
  summ_df <- data.frame(Variable = NA, Type = NA, Distinct_count= NA, Missing_percent = NA,
                        Min= NA, Max=NA,Mean = NA, Trimmed_mean= NA , Q1 =NA, Median = NA,
                        Q3 = NA, SD = NA)
  
  for(k in names(df)){
    
    temp_df <- data.frame(Variable = NA, Type = NA, Distinct_count= NA, Missing_percent = NA,
                          Min= NA, Max=NA,Mean = NA, Trimmed_mean= NA , Q1 =NA, Median = NA,
                          Q3 = NA, SD = NA)
    if(class(df[,get(k)]) %in% c('character', 'logical', 'factor')){
      
      temp_df$Variable = k
      temp_df$Type = 'Categorical'
      temp_df$Distinct_count = length(unique(df[,get(k)]))
      temp_df$Missing_percent = round(sum(is.na(df[,get(k)]))/nrow(df)*100,2)
      summ_df <- rbind(summ_df,temp_df)
      
    }else if(class(df[,get(k)]) %in% c('numeric','double','integer')){
      
      temp_df$Variable = k
      temp_df$Type = 'Numeric'
      temp_df$Distinct_count = length(unique(df[,get(k)]))
      temp_df$Missing_percent = round(sum(is.na(df[,get(k)]))/nrow(df)*100,2)
      temp_df$Min = min(df[,get(k)],na.rm = T)
      temp_df$Max = max(df[,get(k)],na.rm = T)
      temp_df$Mean = round(mean(df[,get(k)],na.rm = T),3)
      temp_df$Trimmed_mean = round(mean(df[,get(k)],trim = 0.1,na.rm = T),3)
      temp_df$Q1 = round(quantile(x =  df[,get(k)], probs = 0.25,names = F,na.rm = T),3)
      temp_df$Median = round(quantile(x =  df[,get(k)], probs = 0.5,names = F,na.rm = T),3)
      temp_df$Q3  = round(quantile(x =  df[,get(k)], probs = 0.75,names = F,na.rm = T),3)
      temp_df$SD = round(sd(df[,get(k)],na.rm = T),3)
      summ_df <- rbind(summ_df,temp_df)
      
    } else{
      temp_df$Variable = k
      temp_df$Type = class(df[,get(k)])
      summ_df <- rbind(summ_df,temp_df)
    }
  }
  return(summ_df[!is.na(summ_df$Variable),])
}
