# Adapted from https://github.com/Lakens/EquivalenceTestingForPsychologicalScience/blob/master/H0H1_plot.R

####  Setup ----------------------------------------
library(ggplot2)
library(gridExtra)

plotheight <- 0.9
lowerbound <- -0.2
upperbound <- 0.2
df <- data.frame()

####  Base plot ------------------------------------
baseplot <-   ggplot(df) +
  scale_y_continuous(limits = c(0,plotheight+0.02), breaks=NULL) + # no y-axis will be displayed
  theme_classic() +
  theme(plot.title = element_text(size = rel(1), face = "bold"), #font size & appearance for plot titles
        axis.title.y = element_blank(), #remove title of y-axis
        axis.title.x = element_text(size=rel(0.7), lineheight = 0.5), #font size for x-axis label
        plot.margin=unit(c(0.5,0.8,0.3,0.8),"cm")) #add padding around each plot to make them look nicer when combined; margin order: top, right, bottom, left

#NHST plot
NHSTplot <- baseplot +  
  scale_x_continuous(limits = c(-0.85,0.85), breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8),
                     labels=c("", "", "", "", "0", "", "", "", ""), name = "effect size") +
  ggtitle("(A) Classic NHST (two-sided)") +
  annotate("segment", x = 0, xend = 0, y = plotheight-plotheight/2, yend = -Inf) + #vertical line at x=0 (H0)
  annotate("text", size = rel(3.5), x=0, y = plotheight-plotheight/20, parse=TRUE, label="H[0]", hjust = 0.3) + #label for point null (H0)
  annotate("segment", x = 0, xend = 0, y = plotheight-plotheight/6, yend=plotheight-plotheight/2.3, 
           arrow = arrow(type = "closed", length=unit(2, "mm"))) + #arrow pointing from H0 label to H0 line
  annotate("text", size = rel(3.5), x=-0.45, y=plotheight/3, parse=TRUE, label="H[1]") + #label for lower area (H1)
  annotate("text", size = rel(3.5), x=0.45, y=plotheight/3, parse=TRUE, label="H[1]") #label for upper area (H1)

#equivalence test plot
eqplot <- baseplot +  
  ggtitle("(B) Equivalence test") +
  scale_x_continuous(limits = c(-0.85,0.85), breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8),
                     labels=c("", "", "", bquote(Delta[L]), "0", bquote(Delta[U]), "", "", ""), name = "effect size") +
  annotate("segment", x = lowerbound, xend = lowerbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for lower bound
  annotate("segment", x = upperbound, xend = upperbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for upper bound
  annotate("rect", xmin = -Inf, xmax = lowerbound, ymin = -Inf, ymax = plotheight, fill = "darkgrey", alpha = .4, color = NA) + #shading for lower area
  annotate("rect", xmin = upperbound, xmax = Inf, ymin = -Inf, ymax = plotheight, fill = "darkgrey", alpha = .4, color = NA) + #shading for upper area
  annotate("text", size = rel(3.5), x=-0.6, y=plotheight/2.5, parse=TRUE, label="H[0]") + #label for lower area (H0)
  annotate("text", size = rel(3.5), x=0.6, y=plotheight/2.5, parse=TRUE, label="H[0]") + #label for upper area (H0)
  annotate("text", size = rel(3.5), x=-0, y=plotheight/2.5, parse=TRUE, label="H[1]", hjust = 0.3) #label for equivalence area

#inferiority plot
noninfplot <-   baseplot +  
  scale_x_continuous(limits = c(-0.85,0.85), breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8),
                     labels=c("", "", "", "", "0", bquote(Delta), "", "", ""), name = "effect size") +
  ggtitle("(C) Non-Inferiority test") +
  annotate("segment", x = upperbound, xend = upperbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for inferiority bound
  annotate("rect", xmin = -Inf, xmax = upperbound, ymin = -Inf, ymax = plotheight, fill = "darkgrey", alpha = .4, color = NA) + #shading for lower area
  annotate("text", size = rel(3.5), x=0.6, y=plotheight/2.5, parse=TRUE, label="H[1]") + #label for upper area (H0)
  annotate("text", size = rel(3.5), x=-0.35, y=plotheight/2.5, parse=TRUE, label="H[0]") #label for lower area (H1)

#equivalence test plot
bestplot <- baseplot +  
  ggtitle("(C) Bayesian Estimation (BEST) / ROPE") +
  scale_x_continuous(limits = c(-0.85,0.85), breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8),
                     labels=c("", "", "", bquote(Delta[L]), "0", bquote(Delta[U]), "", "", ""), name = "effect size") +
  annotate("segment", x = lowerbound, xend = lowerbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for lower bound
  annotate("segment", x = upperbound, xend = upperbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for upper bound
#  annotate("text", size = rel(3.5), x=-0.6, y=plotheight/2.5, parse=TRUE, label="H[0]") + #label for lower area (H0)
#  annotate("text", size = rel(3.5), x=0.6, y=plotheight/2.5, parse=TRUE, label="H[0]") + #label for upper area (H0)
  annotate("text", size = rel(3.5), x=-0.02, y=plotheight/2.5, parse=TRUE, label="ROPE", hjust = 0.3) #label for equivalence area

#bayes factor plot
plotheight.bf <- plotheight + 0.8
bfplot <- ggplot(data.frame(x = c(-0.85, 0.85)), aes(x))+
  scale_y_continuous(limits = c(0,plotheight.bf+0.02), breaks=NULL) + # no y-axis will be displayed
  theme_classic() +
  theme(plot.title = element_text(size = rel(1), face = "bold"), #font size & appearance for plot titles
        axis.title.y = element_blank(), #remove title of y-axis
        axis.title.x = element_text(size=rel(0.7), lineheight = 0.5), #font size for x-axis label
        plot.margin=unit(c(0.5,0.8,0.3,0.8),"cm")) +
  ggtitle("(D) Bayes factor") +
  scale_x_continuous(limits = c(-0.85,0.85), breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8),
                     labels=c("", "", "", "", "0", "", "", "", ""), name = "effect size") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 0.4), color = "black", alpha = .6) +
  annotate("text", size = rel(3.5), x = 0.4, y = plotheight.bf-plotheight.bf/2, parse=TRUE,
           label="M[1]", hjust = 0.3, color = "black", alpha = .6) +
  annotate("segment", x = 0, xend = 0, y = plotheight.bf-plotheight.bf/2.8, yend = -Inf, linetype = "solid") +
  annotate("text", size = rel(3.5), x=0, y = plotheight.bf-plotheight.bf/20, parse=TRUE, label="M[0]", hjust = 0.3) +
  annotate("segment", x = 0, xend = 0, y = plotheight.bf-plotheight.bf/8, yend=plotheight.bf-plotheight.bf/3, 
           arrow = arrow(type = "closed", length=unit(1, "mm"))) #arrow pointing from H0 label to H0 line
  
  

#tiff(file="NHSTplot.tiff",width=1400,height=2000, units = "px", res = 300)
grid.arrange(NHSTplot, eqplot, bestplot, bfplot, ncol = 1) #combine plots in one column (all stacked)
#dev.off()