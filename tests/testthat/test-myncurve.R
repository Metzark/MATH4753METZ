test_that("Test 1 for myncurve is correct", {
  n1 <- myncurve(1, 1, 0.5)
  expect_length(object=n1, 3)
  expect_equal(n1$mu, 1)
  expect_equal(n1$sigma, 1)
  expect_equal(round(n1$area,1), 0.3)
})

test_that(" Test 2 for myncurve is correct", {
  n2 <- myncurve(10, 4, 11)
  expect_length(object=n2, 3)
  expect_equal(n2$mu, 10)
  expect_equal(n2$sigma, 4)
  expect_equal(round(n2$area,1), 0.6)
})

test_that(" Test 3 for myncurve is correct", {
  n3 <- myncurve(5, 2, 4)
  expect_length(object=n3, 3)
  expect_equal(n3$mu, 5)
  expect_equal(n3$sigma, 2)
  expect_equal(round(n3$area,1), 0.3)
})
