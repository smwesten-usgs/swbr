test_that("calculate APWL", {

  tol <- 0.035

  # test calculations against table values from Thornthwaite and Mather (1957), Table 10.
  apwl <- calc_APWL(max_soil_moisture_in=4, soil_moisture_in=2.39)
  expect_equal(apwl, 2.0, tolerance=tol)

  apwl <- calc_APWL(max_soil_moisture_in=1, soil_moisture_in=0.32)
  expect_equal(apwl, 1.0, tolerance=tol)

  apwl <- calc_APWL(max_soil_moisture_in=12, soil_moisture_in=10.15)
  expect_equal(apwl, 2.0, tolerance=tol)

  apwl <- calc_APWL(max_soil_moisture_in=12, soil_moisture_in=4.99)
  expect_equal(apwl, 10.5, tolerance=tol)

  apwl <- calc_APWL(max_soil_moisture_in=16, soil_moisture_in=2.23)
  expect_equal(apwl, 31.5, tolerance=tol)

  apwl <- calc_APWL(max_soil_moisture_in=16, soil_moisture_in=9.11)
  expect_equal(apwl, 9.0, tolerance=tol)

  apwl <- calc_APWL(max_soil_moisture_in=16, soil_moisture_in=14.57)
  expect_equal(apwl, 1.5, tolerance=tol)

})
