test_that("math_betrag",{
  expect_equal(math_betrag(c(1,1)), sqrt(2))
  expect_equal(math_betrag(c(1,1,1)), sqrt(3))
})

test_that("signal_center",{
  x <- rnorm(100,-2.5)|>signal_center()
  c_range = range(x)|>abs()
  expect_equal(c_range[1],c_range[2])
})
