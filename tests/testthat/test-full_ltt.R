test_that("test full_ltt can correctly switch stt_table to nltt_table",{
  set.seed(1)
  sim <- get_DAISIE_sim(parameters = c(0.5,0.1,0.05,0.2),
                                  K = Inf,
                                  replicates = 1)
  ltt <- full_ltt(sim[[1]])
  stt_all <- sim[[1]][[1]][[1]]$stt_all
  total_spec <- stt_all[nrow(stt_all),"nI"] +
    stt_all[nrow(stt_all),"nA"] +
    stt_all[nrow(stt_all),"nC"]
  expect_equal(as.numeric(ltt[nrow(ltt),2]), as.numeric(total_spec))
})