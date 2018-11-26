
#Example of the Logistic function
xseq = seq(-7, 7, length.out = 500)
plot.logistic = function(v){
  options(repr.plot.width=5, repr.plot.height=4)
  logistic = exp(xseq - v)/(1 + exp(xseq - v))
  df = data.frame(x = xseq, y = logistic)
  ggplot(df, aes(x,y)) +
    geom_line(size = 1, color = 'red') +
    geom_vline(xintercept = v, size = 1, color = 'black') +
    geom_hline(yintercept = 0.5, size = 1, color = 'black') +
    ylab('log likelihood') + xlab('Value of x') +
    ggtitle('Logistic function for \n two-class classification') +
    theme_grey(base_size = 18)
}
plot.logistic(0)