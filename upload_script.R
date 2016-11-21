library(RWordPress)
options(WordpressLogin = c(vitaly = '*5BaNUjck8c9'),
        WordpressURL = 'http://www.westendstats.com/xmlrpc.php')
library(knitr)
opts_knit$set(upload.fun = function(file){library(RWordPress);uploadFile(file)$url;})

# knit2wp("C:/Users/vdruk/Desktop/fish_bound.Rmd",
#   title = 'Conditional MLE Variance 2',
#   mt_keywords = c('CASI', 'R', 'mle'),
#   publish=FALSE)

knit2wp("C:/Users/vdruk/Desktop/fish_bound.Rmd",
        action = "editPost",
        title = 'Conditional MLE Variance',
        postid  = 90,
        mt_keywords = c('CASI', 'R', 'mle'),
        publish=FALSE)
