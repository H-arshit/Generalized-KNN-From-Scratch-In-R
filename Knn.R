convert_to_num_matrix <- function(data_df)
{
  result_matric <- c()
  
  for ( i in c( 1:ncol(data_df) ) ) 
    {
        if( is.factor( data_df[ , i]) == TRUE )
          {
              result_matric <- cbind( result_matric , as.numeric(as.character(data_df[,i]) ) ) 
          }
        else
          {
              result_matric <- cbind( result_matric , as.numeric(data_df[,i]) )
          }
    
    }
  
  return( result_matric )
}





euclid_cal <- function(train_data , test_data_row)
{
  euclid_dist <- c()
  
  test_data_row  <- test_data_row[rep(seq(nrow(test_data_row)), each = nrow(train_data)), ]
     
  euclid_dist <- sqrt(rowSums(((train_data) - (test_data_row))^2))
  
  return(euclid_dist)

}




KNN <- function(train_data , train_class , test_data , k)
{
  
  train_data = convert_to_num_matrix(train_data)
  test_data = convert_to_num_matrix(test_data)

  result_data <- c()
  
  for ( i in c( 1:nrow(test_data) ) ) 
  {
    print("___________________________")    
    print(i)    
    euclid_dist <- euclid_cal(train_data , test_data[ i, , drop = FALSE ])
    
    
    
    
    euclid_df <- data.frame(euclid_dist , train_class)
    
    euclid_sorted <- euclid_df[order(euclid_df$euclid_dist) , ]
    
    euclid_top_k <- euclid_sorted[1:k , ]
    
    count_individual_class <- table(euclid_top_k$train_class)
    
    
    result_class <- names(count_individual_class[which.max(count_individual_class)])
    
    result_data <- c(result_data , result_class)
  }
  
  result_data <- as.factor(result_data)
  return(result_data)
  
}


normalize <- function(x) 
{

  return ((x - min(x)) / (max(x) - min(x))) 
}




# This is a Test Example

 data("iris")

 library(caTools)
 set.seed(3273)


  iris[ , -5] <- as.data.frame(lapply(iris[,-5] , normalize))


 split_data <- sample.split(iris$Species , SplitRatio = 0.8)
 train_data <- subset(iris , split_data == TRUE)
 test_data <- subset(iris , split_data == FALSE)
 
 predict_res <- KNN(train_data =  train_data[,-5] , test_data = test_data[ , -5] , train_class = train_data[,5] , k = 12)
 
 library(caret)
 confusionMatrix(table(predict_res , test_data[,5]))
 




