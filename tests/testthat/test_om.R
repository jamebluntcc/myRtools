#test om theme
om_test <- ggplot2::ggplot(mtcars,aes(wt,mpg))
om_test + geom_point() + theme_om()
#test scale
p1 <- om_test + geom_point(aes(color = factor(cyl))) + theme_om() + scale_color_om()
#test facet
om_test + geom_point(aes(color = factor(cyl))) +
  facet_grid(.~cyl) +
  theme_om() +
  scale_color_om()
