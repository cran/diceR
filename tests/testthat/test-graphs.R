context("Graphical displays")

set.seed(911)
x <- matrix(rnorm(100), nrow = 10)
CC1 <- consensus_cluster(x, nk = 2:4, reps = 5,
                         algorithms = c("hc", "ap", "km"), progress = FALSE)
p1 <- graph_cdf(CC1)

test_that("graph_cdf object can have added/modified ggplot layers", {
  p2 <- p1 +
    labs(y = "Probability") +
    stat_ecdf(aes(colour = Method)) +
    scale_color_brewer(palette = "Set2") +
    theme(legend.position = "none")
  expect_is(p1, "ggplot")
  expect_is(p2, "ggplot")
  expect_false(isTRUE(all.equal(p1, p2)))
})

test_that("graph_delta_area works", {
  expect_error(graph_delta_area(CC1), NA)
})

test_that("graph_heatmap can have same plot but different titles", {
  phm1 <- graph_heatmap(CC1[, , 1, 1, drop = FALSE])
  phm2 <- graph_heatmap(CC1[, , 1, 1, drop = FALSE], main = "A2")
  expect_identical(phm1[1], phm2[1])  # 2nd element = titles, 3rd element = cols
  file.remove(list.files(pattern = "Rplots"))
})

test_that("error in graph_heatmap if too few titles", {
  expect_error(graph_heatmap(CC1, main = "A"))
})

test_that("graph_tracking works", {
  expect_error(graph_tracking(CC1), NA)
})

test_that("graph_all runs all of the above", {
  expect_error(graph_all(CC1), NA)
})