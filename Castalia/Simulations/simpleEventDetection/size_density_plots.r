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

pdf("determinants_size_density_ideal.pdf", family="CM Roman")

runs <- read.table("170324-085812-BaseValues-filterOn.txt", sep="\t", header=TRUE)
runs <- subset(runs, EVENTS > 37)

numsources.f <- factor(runs$NUMSOURCES, levels=c(2,3,4,5))
sensdist.f <- factor(runs$SENSINGDISTANCE, levels=c(30,40,50))
field.f <- factor(runs$FIELD, levels=c(100,200,300))
nodes.f <- factor(runs$NODES, levels=c(50,100,200,300))

ggplot(runs, aes(x=field.f, y = (runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES) / runs$EVENTS, fill = nodes.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Field size (m)", y = "Accuracy") +
    scale_fill_discrete("Number of nodes") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=nodes.f, y = (runs$TRUE_POSITIVES + runs$TRUE_NEGATIVES) / runs$EVENTS, fill = field.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of nodes", y = "Accuracy") +
    scale_fill_discrete("Field size (m)") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=field.f, y = runs$TRUE_POSITIVES / runs$POSITIVES, fill = nodes.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Field size (m)", y = "True positive rate") +
    scale_fill_discrete("Number of nodes") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=nodes.f, y = runs$TRUE_POSITIVES / runs$POSITIVES, fill = field.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of nodes", y = "True positive rate") +
    scale_fill_discrete("Field size (m)") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=field.f, y = runs$FALSE_POSITIVES / runs$NEGATIVES, fill = nodes.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Field size (m)", y = "False positive rate") +
    scale_fill_discrete("Number of nodes") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=nodes.f, y = runs$FALSE_POSITIVES / runs$NEGATIVES, fill = field.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of nodes", y = "False positive rate") +
    scale_fill_discrete("Field size (m)") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=field.f, y = runs$TRUE_POSITIVES / (runs$TRUE_POSITIVES + runs$FALSE_POSITIVES), fill = nodes.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Field size (m)", y = "Precision") +
    scale_fill_discrete("Number of nodes") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=nodes.f, y = runs$TRUE_POSITIVES / (runs$TRUE_POSITIVES + runs$FALSE_POSITIVES), fill = field.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of nodes", y = "Precision") +
    scale_fill_discrete("Field size (m)") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=field.f, y = runs$TRUE_NEGATIVES / runs$NEGATIVES, fill = nodes.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Field size (m)", y = "Specificity") +
    scale_fill_discrete("Number of nodes") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

ggplot(runs, aes(x=nodes.f, y = runs$TRUE_NEGATIVES / runs$NEGATIVES, fill = field.f)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    labs(x = "Number of nodes", y = "Specificity") +
    scale_fill_discrete("Field size (m)") +
    theme(plot.title = element_text(hjust=0.5), legend.position = "top")

runs.sent <- read.table("170324-085812-Packets.txt", sep="\t", header=TRUE)

numsources.sent.f <- factor(runs.sent$NUMSOURCES, levels=c(2,3,4,5))
sensdist.sent.f <- factor(runs.sent$SENSINGDISTANCE, levels=c(30,40,50))
field.sent.f <- factor(runs.sent$FIELD, levels=c(100,200,300))
nodes.sent.f <- factor(runs.sent$NODES, levels=c(50,100,200,300))
filter.sent.f <- factor(runs.sent$FILTER, levels=c("Off", "On", "Ideal"))

sent.field = ggplot(runs.sent, aes(x=field.sent.f, y = runs.sent$PACKETS_SENT, fill = filter.sent.f)) +
        ggtitle("(a) Total number of packets sent") +
        geom_boxplot(notch=TRUE) +
        labs(x = "Field size (m)", y = "Total packets sent") +
        scale_fill_discrete("Filter") +
        scale_y_log10() +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
sent.field

sent.nodes = ggplot(runs.sent, aes(x=nodes.sent.f, y = runs.sent$PACKETS_SENT, fill = filter.sent.f)) +
        ggtitle("(a) Total number of packets sent") +
        geom_boxplot(notch=TRUE) +
        labs(x = "Number of nodes", y = "Total packets sent") +
        scale_fill_discrete("Filter") +
        scale_y_log10() +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
sent.nodes

runs.edr <- read.table("170324-085812-Determinants.txt", sep="\t", header=TRUE)

numsources.edr.f <- factor(runs.edr$NUMSOURCES, levels=c(2,3,4,5))
sensdist.edr.f <- factor(runs.edr$SENSINGDISTANCE, levels=c(30,40,50))
field.edr.f <- factor(runs.edr$FIELD, levels=c(100,200,300))
nodes.edr.f <- factor(runs.edr$NODES, levels=c(50,100,200,300))
filter.edr.f <- factor(runs.edr$FILTER, levels=c("Off", "On", "Ideal"))

edr.field = ggplot(runs.edr, aes(x=field.edr.f, y = runs.edr$EDR, fill = filter.edr.f)) +
        ggtitle("(b) Event delivery rate") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Field size (m)", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
edr.field

edr.nodes = ggplot(runs.edr, aes(x=nodes.edr.f, y = runs.edr$EDR, fill = filter.edr.f)) +
        ggtitle("(b) Event delivery rate") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Number of nodes", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
edr.nodes

edr.sensdist = ggplot(runs.edr, aes(x=sensdist.edr.f, y = runs.edr$EDR, fill = filter.edr.f)) +
        ggtitle("(b) Event delivery rate") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Sensing range", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
edr.sensdist

edr.numsources = ggplot(runs.edr, aes(x=numsources.edr.f, y = runs.edr$EDR, fill = filter.edr.f)) +
        ggtitle("(b) Event delivery rate") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Number of event sources", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
edr.numsources

runs.fail <- read.table("170324-085812-RX.txt", sep="\t", header=TRUE)

numsources.fail.f <- factor(runs.fail$NUMSOURCES, levels=c(2,3,4,5))
sensdist.fail.f <- factor(runs.fail$SENSINGDISTANCE, levels=c(30,40,50))
field.fail.f <- factor(runs.fail$FIELD, levels=c(100,200,300))
nodes.fail.f <- factor(runs.fail$NODES, levels=c(50,100,200,300))
filter.fail.f <- factor(runs.fail$FILTER, levels=c("Off", "On", "Ideal"))

failure.field = ggplot(runs.fail, aes(x=field.fail.f, y = (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX) / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX + runs.fail$RECEIVED_DESPITE_INT + runs.fail$RECEIVED_NO_INT), fill = filter.fail.f)) +
        ggtitle("(c) Packet failure rate on radio level") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Field size (m)", y = "Packet failure rate on radio level") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
failure.field

failure.nodes = ggplot(runs.fail, aes(x=nodes.fail.f, y = (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX) / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX + runs.fail$RECEIVED_DESPITE_INT + runs.fail$RECEIVED_NO_INT), fill = filter.fail.f)) +
        ggtitle("(c) Packet failure rate on radio level") +
        geom_boxplot(notch=TRUE) +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Number of nodes", y = "Packet failure rate on radio level") +
        scale_fill_discrete("Filter") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
failure.nodes

p2.field <- ggplot(runs.fail, aes(x=field.fail.f, y = runs.fail$FAILED_NO_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Field size (m)", y = "Packets failed without interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p3.field <- ggplot(runs.fail, aes(x=field.fail.f, y = runs.fail$FAILED_WITH_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Field size (m)", y = "Packets failed with interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p4.field <- ggplot(runs.fail, aes(x=field.fail.f, y = runs.fail$FAILED_BELOW_SENS / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Field size (m)", y = "Packets failed below sensitivity") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p5.field <- ggplot(runs.fail, aes(x=field.fail.f, y = runs.fail$FAILED_NON_RX / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Field size (m)", y = "Packets failed due to non-RX state") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

multiplot(p2.field, p4.field, p3.field, p5.field, cols=2)

p2.nodes <- ggplot(runs.fail, aes(x=nodes.fail.f, y = runs.fail$FAILED_NO_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Number of nodes", y = "Packets failed without interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p3.nodes <- ggplot(runs.fail, aes(x=nodes.fail.f, y = runs.fail$FAILED_WITH_INT / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Number of nodes", y = "Packets failed with interference") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p4.nodes <- ggplot(runs.fail, aes(x=nodes.fail.f, y = runs.fail$FAILED_BELOW_SENS / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Number of nodes", y = "Packets failed below sensitivity") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

p5.nodes <- ggplot(runs.fail, aes(x=nodes.fail.f, y = runs.fail$FAILED_NON_RX / (runs.fail$FAILED_NO_INT + runs.fail$FAILED_WITH_INT + runs.fail$FAILED_BELOW_SENS + runs.fail$FAILED_NON_RX), fill = filter.fail.f)) +
            geom_boxplot(notch=TRUE) +
            coord_cartesian(ylim = c(0.0, 1.0)) +
            labs(x = "Number of nodes", y = "Packets failed due to non-RX state") +
            scale_fill_discrete("Filter") +
            theme(legend.position = "top")

multiplot(p2.nodes, p4.nodes, p3.nodes, p5.nodes, cols=2)

runs.overflows <- read.table("170324-085812-BO.txt", sep="\t", header=TRUE)

mlt <- melt(runs.overflows, measure.vars=c("NODES50", "NODES100", "NODES200", "NODES300"))

numsources.overflows.f <- factor(mlt$NUMSOURCES, levels=c(2,3,4,5))
sensdist.overflows.f <- factor(mlt$SENSINGDISTANCE, levels=c(30,40,50))
field.overflows.f <- factor(mlt$FIELD, levels=c(100,200,300))
nodes.overflows.f <- factor(mlt$variable, levels=c("NODES50", "NODES100", "NODES200", "NODES300"))
filter.overflows.f <- factor(mlt$FILTER, levels=c("Off", "On", "Ideal"))

overflows.field = ggplot(mlt, aes(x=field.overflows.f, y = mlt$value, fill = filter.overflows.f)) +
        ggtitle("(d) Number of buffer overflows (log)") +
        geom_boxplot(notch=TRUE) +
        labs(x = "Field size (m)", y = "Buffer overflows") +
        scale_fill_discrete("Filter") +
        scale_x_discrete(labels = c("100", "200", "300")) +
        scale_y_log10() +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
overflows.field

overflows.nodes = ggplot(mlt, aes(x=nodes.overflows.f, y = mlt$value, fill = filter.overflows.f)) +
        ggtitle("(d) Number of buffer overflows (log)") +
        geom_boxplot(notch=TRUE) +
        labs(x = "Number of nodes", y = "Buffer overflows") +
        scale_fill_discrete("Filter") +
        scale_x_discrete(labels = c("50", "100", "200", "300")) +
        scale_y_log10() +
        theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
overflows.nodes

multiplot(sent.field, failure.field, edr.field, overflows.field, cols=2)

multiplot(sent.nodes, failure.nodes, edr.nodes, overflows.nodes, cols=2)

dev.off()
embed_fonts("determinants_size_density_ideal.pdf", outfile="determinants_size_density_ideal.pdf")
