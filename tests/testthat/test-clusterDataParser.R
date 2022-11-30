test_that("Parsing of cluster members", {
  expected_members <- "England/MILK-344B0FD/2022 England/LSPA-3C3C65F/2022 England/ALDP-3C3C103/2022 England/MILK-354B241/2022 England/QEUH-3C5EAE2/2022 England/ALDP-3C4336D/2022"
  expected_members <- sort(strsplit(expected_members,split=" ")[[1]])

  cl1_file <-  test_path("testdata", "clustering1.uc")
  test_object <- clusterAnalysisUtils::clusterDataParser$new(cl1_file)
  got_members <- sort(test_object$df$Member)
  expect_equal(Reduce("&",got_members==expected_members),T)
})

test_that("Parsing of cluster representatives", {
  expected_cluster <- "England/MILK-344B0FD/2022"
  expected_clusters <- sort(strsplit(expected_cluster,split=" ")[[1]])
  cl1_file <-  test_path("testdata", "clustering1.uc")
  test_object <- clusterAnalysisUtils::clusterDataParser$new(cl1_file)
  got_clusters <- unique(sort(test_object$df$Cluster))
  expect_equal(Reduce("&",got_clusters==expected_clusters),T)
})

test_that("Parsing Mothur and generation of correct 'Member' column", {
  cl3 <- system.file("extdata", "set1.derep.swarm.out", package="clusterAnalysisUtils")
  cl3o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl3,format = "MT",remove_sizes = T)
  
  cl4 <- system.file("extdata", "set1.derep.swarm.uc", package="clusterAnalysisUtils")
  cl4o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl4,format = "UC",remove_sizes = T)
  
  expect_equal(sort(cl3o$df$Member),sort(cl4o$df$Member))
})

test_that("Parsing Mothur and generation of correct 'Cluster' column", {
  cl3 <- system.file("extdata", "set1.derep.swarm.out", package="clusterAnalysisUtils")
  cl3o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl3,format = "MT",remove_sizes = T)
  
  cl4 <- system.file("extdata", "set1.derep.swarm.uc", package="clusterAnalysisUtils")
  cl4o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl4,format = "UC",remove_sizes = T)
  
  expect_equal(sort(cl3o$df$Cluster),sort(cl4o$df$Cluster))
})

test_that("Parsing Mothur and generation of correct dataframe", {
  library(dplyr)
  cl3 <- system.file("extdata", "set1.derep.swarm.out", package="clusterAnalysisUtils")
  cl3o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl3,format = "MT",remove_sizes = T)
  
  cl4 <- system.file("extdata", "set1.derep.swarm.uc", package="clusterAnalysisUtils")
  cl4o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl4,format = "UC",remove_sizes = T)
  df1 <- cl3o$df %>%
    arrange(-Member) %>%
    as.data.frame()
  df2 <- cl4o$df %>%
    arrange(-Member) %>%
    as.data.frame()
    
  
  expect_equal(df1,df2)
})

test_that("Merging with upper level clustering",{
  cl1 <- system.file("extdata", "set1.derep.uc", package="clusterAnalysisUtils")
  cl2 <- system.file("extdata", "set1.derep.swarm.out", package="clusterAnalysisUtils")
  cl1o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl1,format = "UC",remove_sizes = T)
  cl2o <- clusterAnalysisUtils::clusterDataParser$new(fileName = cl2,format = "MT",remove_sizes = T)
  new_clusters <- cl2o$seq_rep[cl1o$df$Cluster]
  cl1o$add_upper_lever_clusters(cl2o)
  expect_equal(length(unique(cl1o$df$Cluster)),length(unique(new_clusters)))
})