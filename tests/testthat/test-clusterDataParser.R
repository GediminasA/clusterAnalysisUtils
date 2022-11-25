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


