library(relaimpo);

pdf("evdel-dominance.pdf")

message("\n");
message("=== Running Dominance Analysis ===\n");

runs <- read.csv("170202-142451-Determinants.txt", sep="\t", header=T);
row.names(runs) <- runs$RUN;
runs <- subset(runs, select=c(-RUN));

message("\n");
message("=== Calculating dominance for EDR ===\n");

crf <- calc.relimp(EDR~FILTER+NUMSOURCES+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(EDR~FILTER+NUMSOURCES+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);
