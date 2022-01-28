test_that("math_betrag",{
  expect_equal(math_betrag(c(1,1)), sqrt(2))
  expect_equal(math_betrag(c(1,1,1)), sqrt(3))
})
