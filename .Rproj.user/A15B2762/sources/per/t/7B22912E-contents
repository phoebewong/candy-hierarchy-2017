# Candy Hierarchy
## Question ##
# Age & number of mehs/joy/despair
# Who likes candy corn?
# Which candy has the most extreme response?
# Who tends to finish the survey the most? Gender/age/chocolate lover?
# likerplot of candy preference?

library(tidyverse)
library(stringr)
library(jpeg)
library(grid)
# devtools::install_github('jbryer/likert')
library(likert)
library(plotly)

img <- readJPEG("1468620.jpg")
data_orig <- read_csv("candyhierarchy2017.csv")
data      <- data_orig
colnames(data) <- colnames(data) %>% 
  str_replace_all(c("\\\\xd5" = "\\'",
                    "Q6 \\| |Q\\d{1,2}\\: " = ""))
colnames(data) <- gsub("�", "\\'", colnames(data)) #stringr doesn't handle empty character

# Re-order levels of Candy Corn for future plotting purposes
data$`Candy Corn` <- factor(data$`Candy Corn`, levels = c("DESPAIR", "MEH","JOY", NA))
# First, more people feel despair about candy corn than joy.
# NA in this context means they left the question blank which can mean "they don't know the candy" as instructured in the survey
# or they skipped this option/question

# Plot
g <- ggplot(data, aes(`Candy Corn`))
g + annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_bar(na.rm = TRUE, fill = "#ffdb99") +
  labs(title = "Feelings when Receive Candy Corn", x = "Feelings", y = "Frequency") +
  theme_minimal()
  # geom_text(aes(label = `Candy Corn`), y = `Candy Corn`)

# Now, let's look at these in percentage
cc_pert <- table(data$`Candy Corn`, useNA = "ifany") %>%
  prop.table() %>% #ignoring NAs
  data.frame() #%>%
  # filter(Freq > 0) # remove NA row
stderror <- function(p) {qnorm(.975) * sqrt(p * (1-p) / length(data$`Candy Corn`))} #95% CI
cc_pert$se <- sapply(cc_pert$Freq, stderror)

g <- ggplot(cc_pert, aes(x = Var1, y = Freq))
g + annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
  geom_col(fill = "#ffdb99") +
  geom_errorbar(data=cc_pert, aes(ymin = Freq-se, ymax = Freq+se), width = 0.15, color = "dark grey") +
  geom_text(label = round(cc_pert$Freq, 3) * 100) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Feelings when Receive Candy Corn", x = "Feelings",
       subtitle = "With 95% Confidence Interval") +
  theme_minimal()

# Likert Plot
candy <- data[, substr(colnames(data_orig), 1, 2) == "Q6"] #use index from orig data
candy <- candy %>%
  data.frame() %>%
  unclass() %>%
  as.data.frame()
lvls <- c("DESPAIR", "MEH","JOY", NA)
candy2[] <-  lapply(candy2, factor, levels=lvls)
colnames(candy2) <- colnames(data)[grep("Q6", colnames(data_orig))]

likert_plot <- likert(candy2)

plot(likert_plot) + ggtitle(paste0("Candy Preference of (N = ", nrow(candy2), ")"))
plot(likert_plot, colors = c("black", "grey", "orange")) + ggtitle(paste0("Candy Preference of N = ", nrow(candy2))) # change to Halloween color
plot(likert_plot, include.histogram = TRUE) #plot the missing data

p <- plot(likert_plot) + ggtitle(paste0("Candy Preference (N = ", nrow(candy2), ")"))
# Interactive likert plot
ggplotly(p)


# Who likes Candy corn (binned age)?
# First, clean up age
data$age2 <- as.numeric(data$AGE)
table(data$age2) #age 1, 312, 1000 and 39.4
data$age2[which(data$age2 >= 100 | data$age2 <= 1)] <- NA
# Second, bin age
agebreaks <- c(4, 9, 13, 18, 25, 35, 45, Inf)
agelabels <- c("4-8","9-12", "13-17", "18-24", "25-34", "35-44", "45+")
data$agebin <-cut(data$age2, breaks = agebreaks, right = FALSE, labels = agelabels)

# Likert plot by age
plot_age <- function(agebin){
  candy_age4 <- data %>%
    filter(agebin == agebin) %>%
    select(c(7:109)) %>%
    data.frame() %>%
    unclass() %>%
    as.data.frame()
  
  lvls <- c("DESPAIR", "MEH","JOY", NA)
  candy_age4[] <-  lapply(candy_age4, factor, levels=lvls)
  colnames(candy_age4) <- colnames(data)[grep("Q6", colnames(data_orig))]
  
  likert_plot_age4 <- likert(candy_age4)
  print(plot(likert_plot_age4) + ggtitle(paste0("Candy Preference of Age ", agebin)))  
}

sapply(levels(data$agebin), plot_age)


# Plot: Candy Corn and Age
ggplot(data, aes(x = factor(agebin), fill = factor(`Candy Corn`))) +
  geom_bar(na.rm = TRUE, position = position_dodge()) + 
  labs(title = "Feelings when Receive Candy Corn", x = "Age") +
  guides(fill=guide_legend(title="Feelings")) +
  theme_minimal()

# Plot: Candy Corn and Going Out
# So Should I give candy corn out on Oct 31st? Yes, or it does not really matter to them.
ggplot(data, aes(x = factor(`Q1GOINGOUT?`), fill = factor(`Candy Corn`))) +
  geom_bar(na.rm = TRUE, position = position_dodge()) + 
  labs(title = "Feelings when Receive Candy Corn", x = "Going Trick or Treating?") +
  guides(fill=guide_legend(title="Feelings")) +
  theme_minimal()