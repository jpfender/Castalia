library(lattice)
library(agsemisc)
library(RColorBrewer)
library(extrafont)
library(ggplot2)
library(reshape2)

pdf("delay_energy.pdf", family="CM Roman")

runs.delay <- read.table("180312-160342-DELAY.txt", sep="\t", header=TRUE)

mlt <- melt(runs.delay, measure.vars=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))

numsources.delay.f <- factor(mlt$NUMSOURCES, levels=c(2,3,4,5))
sensdist.delay.f <- factor(mlt$variable, levels=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))
filter.delay.f <- factor(mlt$FILTER, levels=c("Off", "On", "Ideal"))

delay = ggplot(mlt, aes(x=sensdist.delay.f, y = mlt$value, fill = filter.delay.f)) +
        geom_boxplot(outlier.shape=NA) +
        #expand_limits(y=0) +
        ylim(3,25) +
        labs(x = "Sensing range", y = "Latency (s)") +
        scale_fill_discrete("Filter") +
        scale_x_discrete(labels = c("30", "40", "50")) +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
delay

runs.energy <- read.table("180312-160342-ENERGY.txt", sep="\t", header=TRUE)

mlt <- melt(runs.energy, measure.vars=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))

numsources.energy.f <- factor(mlt$NUMSOURCES, levels=c(2,3,4,5))
sensdist.energy.f <- factor(mlt$variable, levels=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))
filter.energy.f <- factor(mlt$FILTER, levels=c("Off", "On", "Ideal"))

print("Calculating energy...")
energy = ggplot(mlt, aes(x=sensdist.energy.f, y = mlt$value, fill = filter.energy.f)) +
        #ggtitle("Consumed energy (Joules)") +
        geom_boxplot(outlier.shape=NA) +
        #expand_limits(y=0) +
        ylim(6,10) +
        labs(x = "Sensing range", y = "Energy (J)") +
        scale_fill_discrete("Filter") +
        scale_x_discrete(labels = c("30", "40", "50")) +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
energy
print("Done.")

runs.lifetime <- read.table("180312-160342-LIFETIME.txt", sep="\t", header=TRUE)

mlt <- melt(runs.lifetime, measure.vars=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))

numsources.lifetime.f <- factor(mlt$NUMSOURCES, levels=c(2,3,4,5))
sensdist.lifetime.f <- factor(mlt$variable, levels=c("SENSDISTANCE30", "SENSDISTANCE40", "SENSDISTANCE50"))
filter.lifetime.f <- factor(mlt$FILTER, levels=c("Off", "On", "Ideal"))

print("Calculating lifetime...")
lifetime = ggplot(mlt, aes(x=sensdist.lifetime.f, y = mlt$value, fill = filter.lifetime.f)) +
        #ggtitle("Estimated network lifetime (days)") +
        geom_boxplot(outlier.shape=NA) +
        #expand_limits(y=0) +
        ylim(10,21) +
        labs(x = "Sensing range", y = "Lifetime (days)") +
        scale_fill_discrete("Filter") +
        scale_x_discrete(labels = c("30", "40", "50")) +
        theme(plot.title = element_text(hjust=0.5), legend.position = "top", axis.text.x = element_text(size=16), axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), axis.text.y = element_text(size=16), legend.title = element_text(size=24), legend.text = element_text(size=24))
lifetime
print("Done.")

dev.off()
embed_fonts("delay_energy.pdf", outfile="delay_energy.pdf")
