library(lattice)
library(RColorBrewer)
library(extrafont)
library(ggplot2)
library(reshape2)
library(fontcm)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#############################################################################

pdf("bufferthresh.pdf", family="CM Roman")

runs <- read.table("170823-102435-BaseValues.txt", sep="\t", header=TRUE)
runs <- subset(runs, EVENTS > 37)

buffer_threshold.f <- factor(runs$BUFFER_THRESHOLD, levels=c(3,4,5,6,7,8))
sample_interval.f <- factor(runs$SAMPLE_INTERVAL, levels=c(5,10,20,30))

ggplot(runs, aes(x=sample_interval.f, y = (runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES) / runs$EVENTS, fill = buffer_threshold.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Sampling interval", y = "Accuracy") +
    scale_fill_discrete("Buffer threshold") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(runs, aes(x=buffer_threshold.f, y = (runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES) / runs$EVENTS, fill = sample_interval.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Buffer threshold", y = "Accuracy") +
    scale_fill_discrete("Sampling interval") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(runs, aes(x=sample_interval.f, y = runs$TRUE_POSITIVES / runs$POSITIVES, fill = buffer_threshold.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Sampling interval", y = "True positive rate") +
    scale_fill_discrete("Buffer threshold") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=12), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), axis.text.y = element_text(size=12), legend.title = element_text(size=18), legend.text = element_text(size=18))

ggplot(runs, aes(x=buffer_threshold.f, y = runs$TRUE_POSITIVES / runs$POSITIVES, fill = sample_interval.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Buffer threshold", y = "True positive rate") +
    scale_fill_discrete("Sampling interval") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=12), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), axis.text.y = element_text(size=12), legend.title = element_text(size=18), legend.text = element_text(size=18))

ggplot(runs, aes(x=sample_interval.f, y = runs$FALSE_POSITIVES / runs$NEGATIVES, fill = buffer_threshold.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Sampling interval", y = "False positive rate") +
    scale_fill_discrete("Buffer threshold") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=12), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), axis.text.y = element_text(size=12), legend.title = element_text(size=18), legend.text = element_text(size=18))

ggplot(runs, aes(x=buffer_threshold.f, y = runs$FALSE_POSITIVES / runs$NEGATIVES, fill = sample_interval.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Buffer threshold", y = "False positive rate") +
    scale_fill_discrete("Sampling interval") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=12), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), axis.text.y = element_text(size=12), legend.title = element_text(size=18), legend.text = element_text(size=18))

ggplot(runs, aes(x=sample_interval.f, y = runs$TRUE_POSITIVES / (runs$TRUE_POSITIVES + runs$FALSE_POSITIVES), fill = buffer_threshold.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Sampling interval", y = "Precision") +
    scale_fill_discrete("Buffer threshold") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(runs, aes(x=buffer_threshold.f, y = runs$TRUE_POSITIVES / (runs$TRUE_POSITIVES + runs$FALSE_POSITIVES), fill = sample_interval.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Buffer threshold", y = "Precision") +
    scale_fill_discrete("Sampling interval") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(runs, aes(x=sample_interval.f, y = runs$TRUE_NEGATIVES / runs$NEGATIVES, fill = buffer_threshold.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Sampling interval", y = "Specificity") +
    scale_fill_discrete("Buffer threshold") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(runs, aes(x=buffer_threshold.f, y = runs$TRUE_NEGATIVES / runs$NEGATIVES, fill = sample_interval.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Buffer threshold", y = "Specificity") +
    scale_fill_discrete("Sampling interval") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

runs.sent <- read.table("170823-102435-Packets.txt", sep="\t", header=TRUE)

buffer_threshold.sent.f <- factor(runs.sent$BUFFER_THRESHOLD, levels=c(3,4,5,6,7,8))
sample_interval.sent.f <- factor(runs.sent$SAMPLE_INTERVAL, levels=c(5,10,20,30))

sent = ggplot(runs.sent, aes(x=sample_interval.sent.f, y = runs.sent$PACKETS_SENT, fill = buffer_threshold.sent.f)) +
        #ggtitle("(a) Total number of packets sent") +
        geom_boxplot() +
        labs(x = "Sampling interval", y = "Total packets sent") +
        scale_fill_discrete("Buffer threshold") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
sent

sent = ggplot(runs.sent, aes(x=buffer_threshold.sent.f, y = runs.sent$PACKETS_SENT, fill = sample_interval.sent.f)) +
        #ggtitle("(a) Total number of packets sent") +
        geom_boxplot() +
        labs(x = "Buffer threshold", y = "Total packets sent") +
        scale_fill_discrete("Sampling interval") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
sent

dev.off()
embed_fonts("bufferthresh.pdf", outfile="bufferthresh.pdf")
