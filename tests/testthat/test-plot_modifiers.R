require(flexplot)
require(magrittr)
set.seed(1212)

test_that("modify_points works", {
  flexplot(y~x, data=small) %>%
    modify_points(shape=12, colour="black", size=1.5) %>%
    vdiffr::expect_doppelganger("modify_points with bivariate plot", .)
})

test_that("modify_labels works", {
  flexplot(y~x + a | b + z, data=small, method="lm") %>%
    modify_labels("Y", "X", "A", "B", "Z") %>%
    vdiffr::expect_doppelganger("modify_labels", .)
})

test_that("modify_smooth works", {
  flexplot(y~x, data=small) %>% modify_smooth(color="black", method="lm") %>%
    vdiffr::expect_doppelganger(title = "modify_smooth", fig = .)
  flexplot(y~x, data=small) %>% modify_smooth(method="lm") %>%
    vdiffr::expect_doppelganger(title = "modify_smooth no color", fig = .)
  flexplot(y~x + a, data=small) %>% modify_smooth(color="black") %>% expect_error()
  flexplot(y~x + a, data=small) %>% modify_smooth(color=c("red", "black")) %>%
    vdiffr::expect_doppelganger(title = "modify_smooth with group aes", fig = .)

})



