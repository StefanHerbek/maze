context("Maze generation")

test_that("maze_graph works as expected", {
  set.seed(123)
  g <- maze_graph(2, 2, "runif")
  expect_true(are_adjacent(g, 1, 2))
  expect_true(are_adjacent(g, 1, 3))
  expect_true(are_adjacent(g, 2, 4))
  expect_false(are_adjacent(g, 1, 4))
  expect_false(are_adjacent(g, 2, 3))
  expect_false(are_adjacent(g, 3, 4))
})
