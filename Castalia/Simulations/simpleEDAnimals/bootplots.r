library(lattice)
#library(agsemisc)
library(RColorBrewer)
library(extrafont)
library(ggplot2)
library(reshape2)
library(fontcm)
library(boot)

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

mean.w <- function(x,w) sum(x*w)

#############################################################################

pdf("determinants.pdf", family="CM Roman")

runs <- read.table("180312-160342-BaseValues-filterOn.txt", sep="\t", header=TRUE)
runs <- subset(runs, EVENTS > 37)

numsources.f <- factor(runs$NUMSOURCES, levels=c(2,3,4,5))
sensdist.f <- factor(runs$SENSINGDISTANCE, levels=c(30,40,50))

#accuracy.boot <- boot(data=(runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES) / runs$EVENTS,
                            #statistic = mean.w,
                            #R=999, stype="w")

#plot(accuracy.boot)

B = 1000
n = nrow(runs)
boot.samples = matrix(sample((runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES)
                             / runs$EVENTS, size = B*n, replace = TRUE),
                      B, n)
boot.statistics = apply(boot.samples, 1, mean)

ggplot(data.frame(acc=boot.statistics), aes(x=acc)) +
  geom_histogram(binwidth=0.1, aes(y=..density..)) +
  geom_density(color="red")

ggplot(runs, aes(x=numsources.f, y = (runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES) / runs$EVENTS, fill = sensdist.f)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of event sources", y = "Accuracy") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(runs, aes(x=numsources.f, y = runs$TRUE_POSITIVES / runs$POSITIVES, fill = sensdist.f)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of event sources", y = "True positive rate") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=12), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), axis.text.y = element_text(size=12), legend.title = element_text(size=18), legend.text = element_text(size=18))

ggplot(runs, aes(x=numsources.f, y = runs$FALSE_POSITIVES / runs$NEGATIVES, fill = sensdist.f)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of event sources", y = "False positive rate") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=12), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), axis.text.y = element_text(size=12), legend.title = element_text(size=18), legend.text = element_text(size=18))

ggplot(runs, aes(x=numsources.f, y = runs$TRUE_POSITIVES / (runs$TRUE_POSITIVES + runs$FALSE_POSITIVES), fill = sensdist.f)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of event sources", y = "Precision") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(runs, aes(x=numsources.f, y = runs$TRUE_NEGATIVES / runs$NEGATIVES, fill = sensdist.f)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of event sources", y = "Specificity") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))

runs.sent <- read.table("180312-160342-Packets.txt", sep="\t", header=TRUE)

numsources.sent.f <- factor(runs.sent$NUMSOURCES, levels=c(2,3,4,5))
sensdist.sent.f <- factor(runs.sent$SENSINGDISTANCE, levels=c(30,40,50))
filter.sent.f <- factor(runs.sent$FILTER, levels=c("Off", "On", "Ideal"))

sent = ggplot(runs.sent, aes(x=sensdist.sent.f, y = runs.sent$PACKETS_SENT, fill = filter.sent.f)) +
        #ggtitle("(a) Total number of packets sent") +
        geom_boxplot(outlier.shape = NA) +
        ylim(0,29000) +
        labs(x = "Sensing range", y = "Total packets sent") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
sent

runs.edr <- read.table("180312-160342-Determinants.txt", sep="\t", header=TRUE)
mlt <- melt(runs.edr, measure.vars=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))

numsources.edr.f <- factor(mlt$NUMSOURCES, levels=c(2,3,4,5))
sensdist.edr.f <- factor(mlt$variable, levels=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))
filter.edr.f <- factor(mlt$FILTER, levels=c("Off", "On", "Ideal"))

edr = ggplot(mlt, aes(x=sensdist.edr.f, y = mlt$value, fill = filter.edr.f)) +
        geom_boxplot(outlier.shape = NA) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Sensing range", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        scale_x_discrete(labels = c("30", "40", "50")) +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
edr

runs.fail <- read.table("180312-160342-RX.txt", sep="\t", header=TRUE)

numsources.fail.f <- factor(runs.fail$NUMSOURCES, levels=c(2,3,4,5))
sensdist.fail.f <- factor(runs.fail$SENSINGDISTANCE, levels=c(30,40,50))
filter.fail.f <- factor(runs.fail$FILTER, levels=c("Off", "On", "Ideal"))

failure = ggplot(runs.fail, aes(x=sensdist.fail.f, y = (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX) / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX + runs.fail$RECEIVED_DESPITE_INT + runs.fail$RECEIVED_NO_INT), fill = filter.fail.f)) +
        #ggtitle("(c) Packet failure rate on radio level") +
        geom_boxplot(outlier.shape = NA) +
        #coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Sensing range", y = "Packet failure rate on radio level") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
failure

ggplot(runs.fail, aes(x=sensdist.fail.f, y = (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX) / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX + runs.fail$RECEIVED_DESPITE_INT + runs.fail$RECEIVED_NO_INT), fill = filter.fail.f)) +
        geom_boxplot(outlier.shape = NA) +
        labs(x = "Sensing range", y = "Packet failure rate on radio level") +
        scale_fill_discrete("Filter") +
        theme(legend.position = "top")

p2 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_NO_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(outlier.shape = NA) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed without interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p3 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_WITH_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(outlier.shape = NA) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed with interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p4 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_BELOW_SENS / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(outlier.shape = NA) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed below sensitivity") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p5 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_NON_RX / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(outlier.shape = NA) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed due to non-RX state") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

multiplot(p2, p4, p3, p5, cols=2)

dev.off()
embed_fonts("determinants.pdf", outfile="determinants.pdf")
