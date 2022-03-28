library(tidyverse)
library(dplyr)
library(ggplot2)
#Run "Invasion analysis.R"

#FIGURE 3
Partitioning_GRWR_LACO_const <- as.data.frame(rbind(GRWR_LACO_const_summary, LACO_const_epsilon_0_summary, LACO_const_epsilon_alpha_summary, LACO_const_epsilon_lambda_summary, LACO_const_epsilon_interaction_summary)) %>%
  mutate(mechanism = c("r_overall", "epsilon_0", "epsilon_alpha", "epsilon_lambda", "epsilon_int"))
Partitioning_GRWR_LACO_const$mechanism <- ordered(Partitioning_GRWR_LACO_const$mechanism, levels = c("r_overall", "epsilon_0", "epsilon_alpha", "epsilon_lambda", "epsilon_int"))
xlabels <- c("r_overall" = expression(bar("r")[i]^" "), 
             "epsilon_0" = expression(epsilon[i]^0), 
             "epsilon_alpha" = expression(epsilon[i]^alpha),
             "epsilon_lambda" = expression(epsilon[i]^lambda),
             "epsilon_int" = expression(epsilon[i]^{alpha*lambda}))
Part_const <- ggplot(Partitioning_GRWR_LACO_const, aes(x = mechanism, y = Mean, fill = mechanism))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lowCI, ymax = upCI), width = 0.4, alpha = 0.9, size = 0.8) +
  theme_classic(base_size = 20)+
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("grey27", "grey60", "grey60", "grey60", "grey60"))+
  ylim(-1.2, 1.2)+
  annotate("text", x = 2, y = 1.1, label = "Constructed", size = 6)+
  scale_x_discrete(labels = xlabels)

Partitioning_GRWR_LACO_ref <- as.data.frame(rbind(GRWR_LACO_ref_summary, LACO_ref_epsilon_0_summary, LACO_ref_epsilon_alpha_summary, LACO_ref_epsilon_lambda_summary, LACO_ref_epsilon_interaction_summary)) %>%
  mutate(mechanism = c("r_overall", "epsilon_0", "epsilon_alpha", "epsilon_lambda", "epsilon_int"))
Partitioning_GRWR_LACO_ref$mechanism <- ordered(Partitioning_GRWR_LACO_ref$mechanism, levels = c("r_overall", "epsilon_0", "epsilon_alpha", "epsilon_lambda", "epsilon_int"))
Part_ref <- ggplot(Partitioning_GRWR_LACO_ref, aes(x = mechanism, y = Mean, fill = mechanism))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lowCI, ymax = upCI), width = 0.4, alpha = 0.9, size = 0.8) +
  theme_classic(base_size = 20)+
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("grey27", "grey60", "grey60", "grey60", "grey60"))+
  ylim(-1.2, 1.2)+
  annotate("text", x = 2, y = 1.1, label = "Reference", size = 6)+
  scale_x_discrete(labels = xlabels)
figure_partitioning <- ggarrange(Part_ref, Part_const, ncol = 2, nrow = 1, legend = "none", 
                                 labels = c("(a)", "(b)"), font.label = list(size = 20))
annotate_figure(figure_partitioning, bottom = text_grob("Mechanisms", size = 20),
                left = text_grob("Partitioning of Low Density Growth Rate", size = 18, rot = 90))
