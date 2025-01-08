test_that("redirections", {
  df_redirections = load_redirections()
  expect_equal(ncol(df_redirections), 3)
  expect_gt(nrow(df_redirections), 1000)

  active_codes = c(622925, 1988, 254925)
  obsolete_codes = c(280569,83648,295107)
  deprecated_codes = c(2853, 83618, 70474)
  all_codes = c(active_codes, obsolete_codes, deprecated_codes)

  redir_codes1 = redirect_code(all_codes)
  redir_codes2 = redirect_code(all_codes, deprecated_only = FALSE)

  expect_equal(sum(is_active(all_codes)), 3)
  expect_equal(sum(is_active(redir_codes1)), 6)
  expect_gt(sum(is_active(redir_codes2)), 6)
})
