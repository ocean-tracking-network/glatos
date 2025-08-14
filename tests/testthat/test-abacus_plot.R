# R/vis-abacus_plot.r

test_that("abacus_plot works", {
  # subset one transmitter
  det2 <- walleye_detections[walleye_detections$animal_id == 153, ]

  # plot without control table and main tile and change color to red
  vdiffr::expect_doppelganger(
    "basic-functionality",
    fig = function() abacus_plot(det2)
  )

  # example with locations specified
  vdiffr::expect_doppelganger("custom-locations-xaxis", fig = function() {
    abacus_plot(
      det2,
      locations = c(
        "DRF",
        "DRL",
        "FMP",
        "MAU",
        "PRS",
        "RAR",
        "DRM",
        "FDT"
      ),
      x_res = "months",
      x_format = "%b-%y",
      xlim = as.POSIXct(c("2012-01-01", "2014-01-01"), tz = "UTC"),
      main = "TagID: 32054",
      col = "red"
    )
  })

  # example with reclocations specified
  vdiffr::expect_doppelganger("reciver-history", fig = function() {
    abacus_plot(
      det2,
      locations = c(
        "DRF",
        "DRL",
        "FMP",
        "MAU",
        "PRS",
        "RAR",
        "DRM",
        "FDT"
      ),
      receiver_history = sample_receivers,
      main = "TagID: 32054",
      col = "red"
    )
  })
})
