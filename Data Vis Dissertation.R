library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
#font_import() only do this one time - it takes a while
#loadfonts(device = "win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

## IJ Outcomes by Location boxplot

ICdata <- read.csv("Immigration Judges & Outcomes - Sheet1.csv")

ICboxplot <- ggplot(ICdata, aes(x = Immigration.Court, y = X..Granted.Asylum/Total.Decisions*100)) +
  geom_boxplot( fill="gray", color="black") +
  ggtitle(label = "Figure 5: Percentage of Cases Granted Asylum", subtitle = "TRAC data FY 2018 - 2023") +
  ylab("Percentage Granted by Immigration Judge") +
  xlab("Immigration Court") +
  ylim(0,100) +
  theme_minimal() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  theme(text = element_text('Times New Roman'),plot.title = element_text(size=16))

ICboxplot

## Sample data
data <- read.csv("Cases - Sheet8.csv")

## COOs in sample

COOdata <- data %>%
  group_by(COO) %>%
  summarise(count = n())

G1 <- gvisGeoChart(COOdata, locationvar='COO', 'count',
                   options=list(height=500))
T <- gvisTable(COOdata %>% select(COO, count) %>% arrange(desc(count)),
               options=list(width=220, height=500, titleTextStyle="{fontName:'Times New Roman', fontSize:16}"))

#consider a side by side table/plot
GT <- gvisMerge(G1,T, horizontal=TRUE)

#ultimately went with just a gvis chart
plot(T)


## Histogram of Asylum %s by IJ

IJdata <- read.csv("Immigration Judges & Outcomes - Sheet1.csv")

IJHistogram <- ggplot(IJdata, aes(x=X..Granted.Asylum)) +
  geom_histogram(fill="gray", color="black", binwidth = 5, center = 2.5) +
  scale_x_continuous(breaks = seq(0, 100,25))+
  theme_minimal() +
  theme_bw() +
  xlab("Approval Rate") +
  ylab("Count") +
  ylim(0,100) +
  ggtitle("Figure 4: Asylum Granted by Immigration Judge", subtitle = "Each IJ's individual approval rate. TRAC data FY 2018 - 2023") +
  theme(text = element_text('Times New Roman'))


IJHistogram

## Bar plot of nexus partial/full

Nexus <- data %>%
  mutate(nexus_attribution = ifelse(Was.the.nexus.the.reason.the.case.was.denied. == "Yes", "Standalone Nexus", "Nexus + Other")) %>%
  group_by(nexus_attribution) %>%
  summarise(count = n())

NexusAttribution <- ggplot(Nexus, aes(x=nexus_attribution, y = count)) +
  geom_bar(stat = "identity", fill="gray", color="black") +
  theme_minimal() +
  theme_bw() +
  xlab("Attribution") +
  ylab("Count") +
  ylim(0, 50) +
  ggtitle("Figure 3: Role of Nexus in the Sample's Cases", subtitle = "Did lower courts deny the case for multiple reasons or only because of the nexus?")+
  theme(text = element_text('Times New Roman'))

NexusAttribution


## Counsel bar chart

cols <- c("Asylum Granted", "Asylum Denied", "Other Relief")
Status <- c("Represented", "Unrepresented")
Asylum_Granted <- c(29716, 2043)
Asylum_Denied <- c(26346, 9524)
Other_Relief <- c(556, 247)
Counsel <- data.frame(Status, Asylum_Granted, Asylum_Denied, Other_Relief)


Counsel2 <- Counsel %>% pivot_longer(!Status, names_to = "Outcome", values_to = "count") %>%
  group_by(Status) %>%
  mutate(Percentage = count/sum(count)*100)


ggplot(Counsel2, aes(y=Status, x=Percentage, fill = factor(Outcome,labels=c('Asylum Denied', 'Asylum Granted', 'Other Relief'))))+
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme_bw() +
  #scale_fill_discrete(labels=c('Asylum Denied', 'Asylum Granted', 'Other Relied')) +
  scale_fill_manual(values = c("firebrick3", "darkgreen", "gray")) +
  theme(legend.title=element_blank()) +
  xlab("Percentage of Cases") +
  ylab("") +
  ggtitle("Figure 6: Immigration Court Outcome by Counsel", subtitle = "TRAC data FY24 through August 2024")+
  theme(text = element_text('Times New Roman'))


