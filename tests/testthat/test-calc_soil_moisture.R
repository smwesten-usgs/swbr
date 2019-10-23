test_that("calculate soil moisture", {

  tol <- 0.03

  # test calculations against table values from Thornthwaite and Mather (1957), Table 10.
  sm <- calc_soil_moisture( max_soil_moisture_in=2, apwl_in=3)
  expect_equal(sm, 0.4, tolerance=tol)

  sm <- calc_soil_moisture( max_soil_moisture_in=2, apwl_in=6.5)
  expect_equal(sm, 0.06, tolerance=tol/10)

  sm <- calc_soil_moisture( max_soil_moisture_in=8, apwl_in=2.5)
  expect_equal(sm, 5.85, tolerance=tol)

  sm <- calc_soil_moisture( max_soil_moisture_in=8, apwl_in=21.1)
  expect_equal(sm, 0.57, tolerance=tol)

})
