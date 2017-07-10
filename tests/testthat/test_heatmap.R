data("mtcars")
cluster_group <- list(c('mpg','cyl','disp'),
                     c('hp','drat'),
                     c('wt','qsec','vs','am','gear','carb')
)
heatmap_plot(data = mtcars,clusterGroup = cluster_group,saveType = "png",fileName = "haha")
