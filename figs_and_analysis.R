# Script Title: Data Analysis for RUSHPRO
# Author: Will Patterson
# Email: patterw@ccf.org
# Date: 10/24/2023
# Purpose: This script conducts exploratory data analysis (EDA) on RUSHPRO data.
# Requirements: Requires the 'RUSHPRO_data.xlsx' dataset in same wd.
# Additional Notes: Please ensure that the working directory is set to the folder containing the data file.

# load in libraries
library(readxl)
library(magrittr)
library(table1)
library(flextable)
library(ggpubr)
library(ggthemes)
library(rstatix)
library(tidyverse)


# read in data and define new columns
data <- read_excel("RUSHPRO_data.xlsx")[, 1:16] %>%
  mutate(stage = as.factor(cut(semester, 
                     breaks = c(-Inf, 4, 10, Inf), 
                     labels = c("Pre-clinical", "Clinical", "Practical"))),
         semester   = factor(semester),
         grade_diff = writtenexamfinal - writtenexamentry) # post-intervention
         
# create table one ----
label(data$age)               <- "Age"
label(data$semester)          <- "Semester of UME"
label(data$stage)             <- "Stage of UMEᵃ"
label(data$prevexp)           <- "Previous exposure to ultrasound"
label(data$writtenexamentry)  <- "Entry quiz performance"
label(data$writtenexamfinal)  <- "Final quiz performance"
label(data$grade_diff)        <- "Difference in quiz score after intervention"
label(data$scantimeexam)      <- "Final RUSH scan time, simulation"
label(data$scanskillsexam)    <- "Final RUSH scan performance, simulation"
label(data$diagskillsexam)    <- "Final RUSH diagnostic performance, simulation"

units(data$age)               <- "years"
units(data$writtenexamentry)  <- "% correct"
units(data$writtenexamfinal)  <- "% correct"
units(data$grade_diff)        <- "points"
units(data$scantimeexam)      <- "minutes"
units(data$scanskillsexam)    <- "%"
units(data$diagskillsexam)    <- "%"


footnote                      <- "ᵃ Pre-clinical: semesters 1-4; Clinical: semesters 5-10; Practical: semesters 11-12"

# define tbl1 using table1 package
(tbl1 <- table1(~ age + semester + stage + prevexp + writtenexamentry + 
                  writtenexamfinal + grade_diff + scantimeexam + scanskillsexam +
                  diagskillsexam, 
       data = data, 
       footnote = footnote, # define clinical stages
       overall = "Total")) 


  
tbl1 %>% t1flex() %>% save_as_docx(path="tbls/table1.docx") # save as .docx

# Pre- and post-intervention written quiz scores ----
# wilcox test - performance on written exams before / after course participation
wilcox.test(data$writtenexamentry, data$writtenexamfinal, 
            paired = TRUE, 
            exact = FALSE) # ties present, and don't need exact 

# reshape data for pre- post- comparison
df_long <- data %>% 
  select(writtenexamentry, writtenexamfinal) %>%
  gather(key = "Exam", value = "Score")

# create boxplot
stat.test <- df_long %>% rstatix::wilcox_test(Score ~ Exam, paired = TRUE, 
                                              exact = FALSE, detailed = TRUE)

(gradediff_pairedboxplot <- ggpaired(df_long, x = "Exam", y = "Score",
        color = "Exam", line.color = "gray", line.size = 0.4,
         palette = "npg", xlab = FALSE) +
  ylab("Performance\n(% correct MC questions)") +
  scale_x_discrete(labels = c("Entry quiz", "Exit quiz")) +
  stat_pvalue_manual(stat.test %>% 
                       add_xy_position() %>%
                       p_format(p, add.p = TRUE, accuracy = 0.001),
                     tip.length = 0.01,
                     bracket.nudge.y = 2) +
  theme(legend.position = "None"))
ggsave("figs/gradediff_pairedboxplot.png", width = 7, height = 7.5, units = 'in')

# dotchart of score differences 
(gradediff_dotchart <- ggdotchart(data, x = "id", y = "grade_diff",
           color = "stage",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "none",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 2,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           ylab = "Difference in exam performance\n(Post-intervention - pre-intervention)"
) +
  theme_cleveland())                                      # Add dashed grids
ggsave("figs/gradediff_bystudent_dotchart.png", width = 7, height = 7.5, units = 'in')

# Scan timing ----
# Convert the data from wide to long format using tidyr's gather function
df_long <- data %>% 
  select(scanskillsp1, scanskillsp2, scanskillsp3, scanskillsexam, 
         diagskillsexam, id) %>%
  gather(key = "Metric", value = "Value", -c("id"))

# Reorder the levels of the Metric variable
df_long$Metric <- factor(df_long$Metric, 
                                levels = c("scanskillsp1", 
                                           "scanskillsp2", 
                                           "scanskillsp3", 
                                           "scanskillsexam", 
                                           "diagskillsexam"))

# Figure 2 ----
(data_wilcox <- df_long %>%
    pairwise_wilcox_test(Value ~ Metric,
                         p.adjust.method = 'holm',
                         paired = TRUE))

data_wilcox %>%
  write_csv(file="tbls/table2_fig2PairwiseComparisons.csv")

df_long <- df_long %>%
mutate(highlight = if_else(Metric %in% c("scanskillsexam", 
                    "diagskillsexam"), T, F))

# Create the boxplot using ggplot2
(fig2 <- ggplot(df_long, aes(x = Metric, y = Value, fill = highlight)) +
  geom_boxplot() +
  xlab("") +
  ylab("Performance\n(% RUSH exam views acquired)") +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_discrete(labels = c("Patient A", "Patient B", 
                              "Patient C", "Performance\nscore", 
                              "Diagnostic\nscore")) +
  # stat_pvalue_manual(data_wilcox %>% add_xy_position(),
  #                    hide.ns = TRUE,
  #                    step.increase = 0.07,
  #                    bracket.nudge.y = 1.5,
  #                    tip.length = 0.01) +
  #scale_fill_manual(values = c(rep("white", 3), rep("orange",2))) +
  scale_fill_manual(values = c("white", "orange"),
                    labels = c("Patient", "Simulator")) +
  theme_linedraw(base_size = 20) +
  theme(
    legend.title = element_blank(),
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(-4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  ))
ggsave("figs/gradediff_bypatient_boxplot.png", 
       width = 8, height = 8, units = "in")


# Figure 3 ----
# Convert the data from wide to long format using tidyr's gather function
df_long <- data %>% 
  select(scantimep1, scantimep2, scantimep3, scantimeexam, id) %>%
  gather(key = "Metric", value = "Value", -c('id'))

# Change the levels of the Metric variable
df_long$Metric <- factor(df_long$Metric, 
                         levels = c("scantimep1", 
                                    "scantimep2", 
                                    "scantimep3", 
                                    "scantimeexam"))

# is there any relationship here with a significant relationship?
df_long %>%
  filter(!Metric %in% c('scantimeexam')) %>%
  friedman_test(Value ~ Metric | id)

# pairwise wilcox tests with adjustment for multiple comparison
(data_wilcox <- df_long %>%
  pairwise_wilcox_test(Value ~ Metric,
                  p.adjust.method = 'holm',
                  paired = TRUE,
                  detailed = TRUE))

# summary statistics for examinations
data %>% 
  select(writtenexamentry, writtenexamfinal) %>% 
  rstatix::get_summary_stats()

# plot distributions as boxplot
## highlight exam performance as different
df_long <- df_long %>%
  mutate(highlight = if_else(Metric %in% c("scantimeexam"), T, F))

# plot the figure
(fig3 <- ggplot(df_long, aes(x = Metric, y = Value, fill = highlight)) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "tomato1") +  
  annotate("text", x = 0.935, y = 4.85, 
           label = "Clinical utility threshold" , color="tomato1", 
           size=4) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(3,10,1)) +
  xlab("") +
  ylab("Scan Time\n(minutes)") +
  scale_x_discrete(labels = c("Patient A", "Patient B", 
                              "Patient C", "Final exam")) +
  # stat_pvalue_manual(data_wilcox %>% add_y_position(step.increase = 0.02),
  #                    step.increase = 0.07,
  #                    tip.length = 0.01,
  #                    hide.ns = TRUE) +
  scale_fill_manual(values = c("white", "orange"),
                     labels = c("Patient", "Simulator")) +
  theme_linedraw(base_size = 20) +
  theme(
    legend.title = element_blank(),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  ))
ggsave("figs/scanning_times_boxplot_withp.png", width = 7, height = 8, units = "in")

# arrange pre- and post-intervention plots together
ggarrange(gradediff_pairedboxplot, gradediff_dotchart, 
          nrow = 2, labels = c("A", "B"))
ggsave("figs/mc_performance.png", width = 8, height = 10, units = "in")

# arrange repeated measure plots together
ggarrange(fig2, fig3, ncol = 2, labels = c("A", "B"))
ggsave("figs/rush_performance.png", width = 16, height = 10, units = "in",
       bg = 'white')
