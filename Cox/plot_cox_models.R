library(reshape2)
library(ggplot2)

# This file plots the f-score and accuracy plots

outs = read.csv("results_hol_lin.csv")

num_orgs = outs[seq(1,nrow(outs), by=3), 1]
acc_idx = seq(2,nrow(outs),by=3)
# Plot accuracies
accs.toplot = data.frame(accs=outs[acc_idx, ], num_orgs=num_orgs)
colnames(accs.toplot) = c("Interval with Cluster", "Interval without Cluster", 
                          "Right with Cluster", "Right without Cluster", "num_orgs")
melted = melt(accs.toplot, id.vars="num_orgs", variable.name="Model")
ggplot(melted, aes(x=num_orgs,y=value)) +
  geom_line(aes(color=Model), size=1) +
  ggtitle("Holling FVT and Linear Hazard") + 
  xlab("Number of Organisms")+ 
  ylab("Accuracy") + 
  theme_bw() +
  theme(plot.title = element_text(size=18, hjust=0.5),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))
ggsave("hol_linear_accs.png")

fb_idx = seq(3,nrow(outs),by=3)
fb.toplot = data.frame(fb=outs[fb_idx, ], num_orgs=num_orgs)
colnames(fb.toplot) = c("Interval with Cluster", "Interval without Cluster", 
                        "Right with Cluster", "Right without Cluster", "num_orgs")
melted = melt(fb.toplot, id.vars="num_orgs", variable.name="Model")
ggplot(melted, aes(x=num_orgs,y=value)) +
  geom_line(aes(color=Model), size=1) +
  ggtitle("Holling FVT and Linear Hazard") + 
  xlab("Number of Organisms")+ 
  ylab("F-Score") + 
  theme_bw() +
  theme(plot.title = element_text(size=14, hjust=0.5),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))
ggsave("hol_linear_fscores.png")


outs = read.csv("results_sin_lin.csv")

num_orgs = outs[seq(1,nrow(outs), by=3), 1]
acc_idx = seq(2,nrow(outs),by=3)
# Plot accuracies
accs.toplot = data.frame(accs=outs[acc_idx, ], num_orgs=num_orgs)
colnames(accs.toplot) = c("Interval with Cluster", "Interval without Cluster", 
                          "Right with Cluster", "Right without Cluster", "num_orgs")
melted = melt(accs.toplot, id.vars="num_orgs", variable.name="Model")
ggplot(melted, aes(x=num_orgs,y=value)) +
  geom_line(aes(color=Model), size=1) +
  ggtitle("Sin FVT and Linear Hazard") + 
  xlab("Number of Organisms")+ 
  ylab("Accuracy") + 
  theme_bw() +
  theme(plot.title = element_text(size=18, hjust=0.5),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))
ggsave("sin_linear_accs.png")

fb_idx = seq(3,nrow(outs),by=3)
fb.toplot = data.frame(fb=outs[fb_idx, ], num_orgs=num_orgs)
colnames(fb.toplot) = c("Interval with Cluster", "Interval without Cluster", 
                        "Right with Cluster", "Right without Cluster", "num_orgs")
melted = melt(fb.toplot, id.vars="num_orgs", variable.name="Model")
ggplot(melted, aes(x=num_orgs,y=value)) +
  geom_line(aes(color=Model), size=1) +
  ggtitle("Sin FVT and Linear Hazard") + 
  xlab("Number of Organisms")+ 
  ylab("F-Score") + 
  theme_bw() +
  theme(plot.title = element_text(size=14, hjust=0.5),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))
ggsave("sin_linear_fscores.png")

outs = read.csv("results_sin_exp.csv")

num_orgs = outs[seq(1,nrow(outs), by=3), 1]
acc_idx = seq(2,nrow(outs),by=3)
# Plot accuracies
accs.toplot = data.frame(accs=outs[acc_idx, ], num_orgs=num_orgs)
colnames(accs.toplot) = c("Interval with Cluster", "Interval without Cluster", 
                          "Right with Cluster", "Right without Cluster", "num_orgs")
melted = melt(accs.toplot, id.vars="num_orgs", variable.name="Model")
ggplot(melted, aes(x=num_orgs,y=value)) +
  geom_line(aes(color=Model), size=1) +
  ggtitle("Sin FVT and Exponential Hazard") + 
  xlab("Number of Organisms")+ 
  ylab("Accuracy") + 
  theme_bw() +
  theme(plot.title = element_text(size=18, hjust=0.5),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))
ggsave("sin_exponential_accs.png")

fb_idx = seq(3,nrow(outs),by=3)
fb.toplot = data.frame(fb=outs[fb_idx, ], num_orgs=num_orgs)
colnames(fb.toplot) = c("Interval with Cluster", "Interval without Cluster", 
                        "Right with Cluster", "Right without Cluster", "num_orgs")
melted = melt(fb.toplot, id.vars="num_orgs", variable.name="Model")
ggplot(melted, aes(x=num_orgs,y=value)) +
  geom_line(aes(color=Model), size=1) +
  ggtitle("Sin FVT and Exponential Hazard") + 
  xlab("Number of Organisms")+ 
  ylab("F-Score") + 
  theme_bw() +
  theme(plot.title = element_text(size=14, hjust=0.5),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))
ggsave("sin_exponential_fscores.png")


