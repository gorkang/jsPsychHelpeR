testthat::test_that('standard model test for dataframe df_SBS', {

# Dimensions DF
  testthat::expect_equal(nrow(df_SBS), 4)
  testthat::expect_equal(dim(df_SBS), c(4, 26))

# Specific variables DF
  testthat::expect_equal(df_SBS %>% count(`id`) %>% pull(n), c(1, 1, 1, 1))
  testthat::expect_equal(df_SBS %>% count(`id`) %>% pull(n), c(1, 1, 1, 1))
})
