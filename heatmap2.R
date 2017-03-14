# Setup R error handling to go to stderr
options(show.error.messages=F, error=function(){cat(geterrmessage(),file=stderr());q("no",1,F)})

# We need to not crash galaxy with an UTF8 error on German LC settings.
loc <- Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")


# Import library
library("getopt")
library("RColorBrewer")
library("gplots")
options(stringAsfactors = FALSE, useFancyQuotes = FALSE)
# Take in trailing command line arguments
args <- commandArgs(trailingOnly = TRUE)


# get options, using the spec as defined by the enclosed list.
# we read the options from the default: commandArgs(TRUE).
option_specification = matrix(c(
  'input', 'i', 2, 'character',
  'title', 't', 2, 'character',
  'transform', 'c', 2, 'character',
  'key', 'k', 2, 'character',
  'colorscheme', 'z', 2, 'character',
  'output', 'o', 2, 'character'
  ), byrow=TRUE, ncol=4);

# Parse options
options = getopt(option_specification);



# Print options to see what is going on
cat("\n input: ",options$input)
cat("\n title: ",options$title)
cat("\n output: ",options$output)

input <- read.delim(options$input,sep='\t',header=TRUE)

mat_input <- data.matrix(input[,2:ncol(input)])  


if(options$transform == "none"){
    linput <- mat_input
}else if(options$transform == "log2"){
    linput <- log2(mat_input)
}else if(options$transform == "log2plus1"){
    linput <- log2(mat_input+1)
}else if(options$transform == "log10"){
    linput <- log10(mat_input)
}else if(options$transform == "log10plus1"){
    linput <- log10(mat_input+1)
    }else{
}

if(options$colorscheme == "Default"){
  colorscale = colfunc <- colorRampPalette(c("white", "red"))
} else {
  colorscale = colfunc <- colorRampPalette(c("blue","white", "red"))
}


hclust_fun = function(x) hclust(x, method="complete")
dist_fun = function(x) dist(x, method="maximum")

pdf(file="Rplot.pdf")
colorscale
heatmap.2(linput,
          distfun=dist_fun, hclustfun=hclust_fun, scale = "none",
          col=colfunc(50), trace="none", density.info = "none",labRow=FALSE, margins=c(8,2),
          main = options$title, key.xlab= options$key, keysize=1)
dev.off()
