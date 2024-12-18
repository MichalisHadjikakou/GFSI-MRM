# Trimming outliers
trim_residuals <- function(stat_df,ind,i){

outliers <- boxplot(stat_df[,ind[i]],plot=FALSE)$out # Create boxplot of all data

if(length(outliers)!=0){

  #stat_df[which(stat_df[,ind[i]] %in% outliers),]

  stat_df <- stat_df %>% filter(!!sym(ind[[i]]) %notin% outliers) # Filtering out outliers
  
  } else  {
  
  stat_df <- stat_df
  }

}