#' Measures a degree of mutual dissimilarity between all objects in a cluster
#'
#' @param my_k - a cluster number (from the k column)
#' @param df - a tibble with the k column and the signature column
#' @param sample_size - size of the sample (~maxhist)
#'
#' @export
k_homogeneity = function(my_k, df, sample_size){
  df_one_k = df[df$k == my_k, ]
  sum_dist = 0
  n_elem = 0
  max_nums = sample(1:nrow(df_one_k), sample_size)
  for (i in max_nums){
    for (j in max_nums){
      tmp_dist = philentropy::jensen_shannon(df_one_k$signature[[i]],
                                df_one_k$signature[[j]],
                                testNA = FALSE,
                                unit = "log2")
      sum_dist = sum_dist + tmp_dist
      n_elem = n_elem + 1
    }
  }
  n_elem = n_elem - length(max_nums)
  avg_dist = sum_dist / n_elem
  return(avg_dist)
}

#' It is an average distance between the focus cluster and all of the rest of the clusters
#'
#' @param my_k - a cluster number (from the k column)
#' @param df - a tibble with the k column and the signature column
#' @param sample_size - size of the sample (~maxhist)
#'
#' @export
k_interdistance = function(my_k, df, sample_size){
  df_one_k = df[df$k == my_k, ]
  sum_dist = 0
  n_elem = 0
  for (kk in setdiff(unique(df$k), my_k)){
    df_two_k = df[df$k == kk, ]
    max_nums1 = sample(1:nrow(df_one_k), sample_size)
    max_nums2 = sample(1:nrow(df_two_k), sample_size)
    for (i in max_nums1){
      for (j in max_nums2){
        tmp_dist = philentropy::jensen_shannon(df_one_k$signature[[i]],
                                  df_two_k$signature[[j]],
                                  testNA = FALSE,
                                  unit = "log2")
        sum_dist = sum_dist + tmp_dist
        n_elem = n_elem + 1
      }
    }
  }
  avg_dist = sum_dist / n_elem
  return(avg_dist)
}
