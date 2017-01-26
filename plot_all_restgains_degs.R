graphics.off()
current_object = current_gains_degs_object$avoided_degs
mx = max(unlist(current_object))
mn = min(unlist(current_object))

plot(current_object[[1]][[1]], ylim = c(mn, mx), type = 'l')
for (plot_ind in seq_along(current_object)){
  lines(current_object[[plot_ind]][[1]], ylim = c(mn, mx))
}

which(apply(simplify2array(simplify2array(current_object)), 2, sum) <0)