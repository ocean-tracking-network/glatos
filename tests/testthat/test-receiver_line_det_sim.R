# R/sim-receiver_line_det_sim.r

test_that("receiver_line_det_sim works", {
  pdrf <- function(dm, b = c(5.5, -1 / 120)) {
    p <- 1 / (1 + exp(-(b[1] + b[2] * dm)))
    return(p)
  }

  # Simulate detection using pdrf; default values otherwise
  set.seed(24)
  expect_equal(round(receiver_line_det_sim(rngFun = pdrf), 3), 0.997)

  # 10 virtual fish and optional plot
  vdiffr::expect_doppelganger("ten-fish-sim",
    fig = function() {
      receiver_line_det_sim(rngFun = pdrf, nsim = 10, showPlot = TRUE)
    }
  )

  # 4 virtual fish and optional plot with irregular spacing
  vdiffr::expect_doppelganger("four-fish-sim",
    fig = function() {
      receiver_line_det_sim(
        rngFun = pdrf, recSpc = c(2000, 4000, 2000),
        outerLim = c(1000, 1000), nsim = 4, showPlot = TRUE
      )
    }
  )
})
