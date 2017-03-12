library(lattice)
library(agsemisc)
library(RColorBrewer)
library(extrafont)
library(ggplot2)
library(reshape2)

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

pdf("determinants_fancy.pdf", family="CM Roman")

runs <- read.table("170228-091819-BaseValues-new.txt", sep="\t", header=TRUE)
runs <- subset(runs, EVENTS > 37)
runs2 <- subset(runs, NUMSOURCES==2)
runs3 <- subset(runs, NUMSOURCES==3)
runs4 <- subset(runs, NUMSOURCES==4)
runs5 <- subset(runs, NUMSOURCES==5)

numsources.f <- factor(runs$NUMSOURCES, levels=c(2,3,4,5))
sensdist.f <- factor(runs$SENSINGDISTANCE, levels=c(30,40,50))
sensdist2.f <- factor(runs2$SENSINGDISTANCE, levels=c(30,40,50))
sensdist3.f <- factor(runs3$SENSINGDISTANCE, levels=c(30,40,50))
sensdist4.f <- factor(runs4$SENSINGDISTANCE, levels=c(30,40,50))
sensdist5.f <- factor(runs5$SENSINGDISTANCE, levels=c(30,40,50))

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = (runs2$TRUE_POSITIVES + runs2$TRUE_NEGATIVES) / runs2$EVENTS, fill = sensdist2.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = (runs3$TRUE_POSITIVES + runs3$TRUE_NEGATIVES) / runs3$EVENTS, fill = sensdist3.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = (runs4$TRUE_POSITIVES + runs4$TRUE_NEGATIVES) / runs4$EVENTS, fill = sensdist4.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = (runs5$TRUE_POSITIVES + runs5$TRUE_NEGATIVES) / runs5$EVENTS, fill = sensdist5.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

ggplot(runs, aes(x=numsources.f, y = (runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES) / runs$EVENTS, fill = sensdist.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    #ggtitle("Accuracy") +
    labs(x = "Number of event sources", y = "Accuracy") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$TRUE_POSITIVES / runs2$POSITIVES, fill = sensdist2.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$TRUE_POSITIVES / runs3$POSITIVES, fill = sensdist3.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$TRUE_POSITIVES / runs4$POSITIVES, fill = sensdist4.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$TRUE_POSITIVES / runs5$POSITIVES, fill = sensdist5.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

ggplot(runs, aes(x=numsources.f, y = runs$TRUE_POSITIVES / runs$POSITIVES, fill = sensdist.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    #ggtitle("True positive rate") +
    labs(x = "Number of event sources", y = "True positive rate") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$FALSE_POSITIVES / runs2$NEGATIVES, fill = sensdist2.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$FALSE_POSITIVES / runs3$NEGATIVES, fill = sensdist3.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$FALSE_POSITIVES / runs4$NEGATIVES, fill = sensdist4.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$FALSE_POSITIVES / runs5$NEGATIVES, fill = sensdist5.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

ggplot(runs, aes(x=numsources.f, y = runs$FALSE_POSITIVES / runs$NEGATIVES, fill = sensdist.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    #ggtitle("False positive rate") +
    labs(x = "Number of event sources", y = "False positive rate") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$TRUE_POSITIVES / (runs2$TRUE_POSITIVES + runs2$FALSE_POSITIVES), fill = sensdist2.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$TRUE_POSITIVES / (runs3$TRUE_POSITIVES + runs3$FALSE_POSITIVES), fill = sensdist3.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$TRUE_POSITIVES / (runs4$TRUE_POSITIVES + runs4$FALSE_POSITIVES), fill = sensdist4.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$TRUE_POSITIVES / (runs5$TRUE_POSITIVES + runs5$FALSE_POSITIVES), fill = sensdist5.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

ggplot(runs, aes(x=numsources.f, y = runs$TRUE_POSITIVES / (runs$TRUE_POSITIVES + runs$FALSE_POSITIVES), fill = sensdist.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    #ggtitle("Precision") +
    labs(x = "Number of event sources", y = "Precision") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$TRUE_NEGATIVES / runs2$NEGATIVES, fill = sensdist2.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$TRUE_NEGATIVES / runs3$NEGATIVES, fill = sensdist3.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$TRUE_NEGATIVES / runs4$NEGATIVES, fill = sensdist4.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$TRUE_NEGATIVES / runs5$NEGATIVES, fill = sensdist5.f)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

ggplot(runs, aes(x=numsources.f, y = runs$TRUE_NEGATIVES / runs$NEGATIVES, fill = sensdist.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    #ggtitle("Specificity") +
    labs(x = "Number of event sources", y = "Specificity") +
    scale_fill_discrete("Sensing range") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

#runs <- read.table("170220-094941-Determinants-sum-new.txt", sep="\t", header=TRUE)
runs.sent <- read.table("170228-091819-Packets.txt", sep="\t", header=TRUE)

numsources.sent.f <- factor(runs.sent$NUMSOURCES, levels=c(2,3,4,5))
sensdist.sent.f <- factor(runs.sent$SENSINGDISTANCE, levels=c(30,40,50))

sent = ggplot(runs.sent, aes(x=sensdist.sent.f, y = runs.sent$PACKETS_SENT, fill = runs.sent$FILTER)) +
        ggtitle("(a) Total number of packets sent") +
        geom_boxplot(notch=TRUE) +
        labs(x = "Sensing range", y = "Total packets sent") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
sent

runs.edr <- read.table("170228-091819-Determinants-new.txt", sep="\t", header=TRUE)

numsources.edr.f <- factor(runs.edr$NUMSOURCES, levels=c(2,3,4,5))
sensdist.edr.f <- factor(runs.edr$SENSINGDISTANCE, levels=c(30,40,50))

edr = ggplot(runs.edr, aes(x=sensdist.edr.f, y = runs.edr$EDR, fill = runs.edr$FILTER)) +
        ggtitle("(b) Event delivery rate") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Sensing range", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
edr

runs.fail <- read.table("170228-091819-RX-new.txt", sep="\t", header=TRUE)

numsources.fail.f <- factor(runs.fail$NUMSOURCES, levels=c(2,3,4,5))
sensdist.fail.f <- factor(runs.fail$SENSINGDISTANCE, levels=c(30,40,50))

failure = ggplot(runs.fail, aes(x=sensdist.fail.f, y = (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX) / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX + runs.fail$RECEIVED_DESPITE_INT + runs.fail$RECEIVED_NO_INT), fill = runs.fail$FILTER)) +
        ggtitle("(c) Packet failure rate on radio level") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Sensing range", y = "Packet failure rate on radio level") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
failure

ggplot(runs.fail, aes(x=sensdist.fail.f, y = (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX) / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX + runs.fail$RECEIVED_DESPITE_INT + runs.fail$RECEIVED_NO_INT), fill = runs.fail$FILTER)) +
        geom_boxplot(notch=TRUE) +
        labs(x = "Sensing range", y = "Packet failure rate on radio level") +
        scale_fill_discrete("Filter") +
        theme(legend.position = "top")

p2 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_NO_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = runs.fail$FILTER)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed without interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p3 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_WITH_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = runs.fail$FILTER)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed with interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p4 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_BELOW_SENS / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = runs.fail$FILTER)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed below sensitivity") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p5 <- ggplot(runs.fail, aes(x=sensdist.fail.f, y = runs.fail$FAILED_NON_RX / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = runs.fail$FILTER)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Sensing range", y = "Packets failed due to non-RX state") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

multiplot(p2, p4, p3, p5, cols=2)

runs.overflows <- read.table("170228-091819-BO-new.txt", sep="\t", header=TRUE)
#runs <- read.table("170220-094941-BO-new.txt", sep="\t", header=TRUE)

mlt <- melt(runs.overflows, measure.vars=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))

numsources.overflows.f <- factor(mlt$NUMSOURCES, levels=c(2,3,4,5))
sensdist.overflows.f <- factor(mlt$variable, levels=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))

#ggplot(mlt, aes(x=sensdist.f, y = mlt$value, fill = mlt$FILTER)) +
        #geom_boxplot() +
        #stat_summary(fun.y = mean, geom = "point") +
        #labs(x = "Sensing range", y = "Buffer overflows") +
        #scale_fill_discrete("Filter") +
        #scale_x_discrete(labels = c("30", "40", "50")) +
        #theme(legend.position = "top")

#ggplot(mlt, aes(x=sensdist.f, y = mlt$value, fill = mlt$FILTER)) +
        #geom_boxplot() +
        #stat_summary(fun.y = mean, geom = "point") +
        #coord_cartesian(ylim = c(0, 100)) +
        #labs(x = "Sensing range", y = "Buffer overflows") +
        #scale_fill_discrete("Filter") +
        #scale_x_discrete(labels = c("30", "40", "50")) +
        #theme(legend.position = "top")

overflows = ggplot(mlt, aes(x=sensdist.overflows.f, y = mlt$value, fill = mlt$FILTER)) +
        ggtitle("(d) Number of buffer overflows (log)") +
        geom_boxplot(notch=TRUE) +
        labs(x = "Sensing range", y = "Buffer overflows") +
        scale_fill_discrete("Filter") +
        scale_x_discrete(labels = c("30", "40", "50")) +
        scale_y_log10() +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
overflows

multiplot(sent, failure, edr, overflows, cols=2)

dev.off()
embed_fonts("determinants_fancy.pdf", outfile="determinants_fancy.pdf")
