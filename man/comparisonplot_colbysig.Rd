% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/main.R
\name{comparisonplot_colbysig}
\alias{comparisonplot_colbysig}
\title{Qualitative plot of comparison for two word clouds}
\usage{
comparisonplot_colbysig(freq, qvals, cutoff = 0.05, colors = c("red",
  "black"), size.limits = 50:1/20, xlim = c(-0.5, 1), ylim = c(-0.5, 1),
  axes = FALSE, xlab = "Group 1", ylab = "Group 2")
}
\arguments{
\item{freq}{A numeric matrix with two columns. The first column indicates the
frequency of a term in the first group, and the second column indicates
frequency of a term in the second group. The rows describe terms. Rownames
must be set to the terms.}

\item{qvals}{A vector of q-values for each of the terms. The names for the vector
must be the terms. Q-values of 0 are rounded to 0.00001. Note that there
must be at least two distinct q-values (i.e., they cannot all be 1).}

\item{colors}{A character vector of length two. The first element corresponds to the color
that should be used for significant (q < \code{cutoff}) terms.
Defaults to \code{c('red','black')}}

\item{size.limits}{Bounds for the sizes of the words. Must be provided in the format max:min.
The sizes are interpreted by \code{cex}, so they are scaled. Defaults to \code{50:1/20}.}

\item{xlim}{A numeric vector of length two with the lower and upper bounds for the x-axis of the graph.
Defaults to \code{c(-0.5,1)}.}

\item{ylim}{A numeric vector of length two with the lower and upper bounds for the y-axis of the graph.
Defaults to \code{c(-0.5,1)}.}

\item{axes}{A boolean value specifying whether axes (frequencies) should be shown. Note that for large
data sets, the words may displace each other so much that the axes do not provide a meaningful
reference. Defaults to \code{FALSE}.}

\item{xlab}{A label for the x-axis. If \code{axes=FALSE}, the label is placed in the general area where
it would be if there were axes. Defaults to 'Group 1'.}

\item{ylab}{A label for the y-axis. If \code{axes=FALSE}, the label is placed in the general area where
            it would be if there were axes. Defaults to 'Group 2'.}
}
\description{
Main user interface for qualitative analysis of two word clouds.
Creates a plot with frequencies of each group on each axis. The size of each
term is inversely related to the q-values provided. The color of each term
indicates whether the corresponding q-value was less than the specified cutoff.
Finally, the diagonal (representing words of equal frequencies for each group) is
superimposed on the word cloud. For size based on frequency, see \link{comparisonplot_colbycount}.

Note that all parameters must describe the same number of terms.
}
\examples{
group1 <- c('head','toe','hand',rep('knee',4))
group2 <- c(rep('toe',3),'hand',rep('head',2))

stats <- wordcloudstats(group1, group2, names=c('Group 1','Group 2'))
qval <- runif(nrow(stats$frequency),0.0,1.0)
names(qval) <- rownames(stats$outputs)
comparisonplot_colbysig(stats$frequency, qval, cutoff=0.4, colors=c('blue','black'))
}
\seealso{
\code{\link[wordcloud]{wordlayout}}
}

