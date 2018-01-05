#' A Predict_Cluster Function
#'
#' This function allows you to predict the cluster group in which point lies
#' @param input input data in csv or dataframe format
#' @keywords input file
#' @export
#' @examples
#' predict_cl()


# predicting the cluster
predict_cl <- function(input){

  #input can either be csv file or data
  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input)
  } else {
    as.data.frame(input)
  }
  stopifnot("Step.Duration..sec." %in% names(newdata))
  stopifnot("Attempts" %in% names(newdata))
  stopifnot("Hints" %in% names(newdata))
  stopifnot("count_subskill" %in% names(newdata))
  stopifnot("Opportunity.SubSkills." %in% names(newdata))
  stopifnot("Correct.First.Attempt" %in% names(newdata))

  course1 = read.csv(file = "/datadrive/dataset/bclust_sample_v2.csv",header = T)
  course1_pca = read.csv(file = "/datadrive/dataset/course1pca_Chaptercat.csv",header = T)

  summary = sapply(course1[,c(4:13)], function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE)))
  summary = as.data.frame(summary)

  # Fitting Hierarchical Clustering to the dataset
  hc = hclust(d = dist(course1_pca[,c(4,5)], method = 'euclidean'), method = 'ward.D')
  y_hc = cutree(hc, 3)

  # Saving the output with cluster name in particular course data used
  course1_cluster = cbind(course1,y_hc)
  course1_cluster$y_hc[course1_cluster$y_hc == 1] = "HARD"
  course1_cluster$y_hc[course1_cluster$y_hc == 2] = "EASY"
  course1_cluster$y_hc[course1_cluster$y_hc == 3] = "MEDIUM"
  write.csv(course1_cluster, file = "/datadrive/dataset/ChapterCat_OUTPUT/bc_clust_output.csv",row.names = F)

  # function to find centroid in cluster i
  clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
  }

  centroid = sapply(unique(y_hc), clust.centroid, course1[,c(4:13)], y_hc)
  centroid = as.data.frame(t(centroid))

  points = rbind(centroid,newdata[,c(4:13)])
  dist_matrix = dist(points)
  dist_matrix = as.matrix(dist_matrix)
  dist_matrix = as.data.frame(dist_matrix)
  dist_matrix = dist_matrix[1:3,]
  c_index =  which.min(dist_matrix[,4])
  print(c_index)
  #returning the prediction
  if(c_index == 1){
    output <- "Problem is of HARD level"

  } else if(c_index  ==2){
    output <- "Problem is of EASY level"
  }
   else{
   output <- "Problem is of MEDIUM level "
       }
  return(output)

}

