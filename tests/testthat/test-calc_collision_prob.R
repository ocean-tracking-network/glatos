# R/sim-calc_collision_prob.r

test_that("calc_collision_prob works", {
  set.seed(24)

  expect_equal(
    calc_collision_prob(
      delayRng = c(45, 90),
      burstDur = 5.12,
      maxTags = 3,
      nTrans = 10
    ),
    ccp_shouldbe
  )
})
