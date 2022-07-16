library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)
library(RColorBrewer)

# set up indices and directory names
out <- readRDS('landis-output.rds')

# get economically-relevant harvested totals (Mg)
econ_status <-  read.csv('input/WI_economic_trees.csv')
colnames(econ_status) <- c('Species', 'Commercial')
econ_status[, 'Commercial'] <- ifelse(econ_status[, 'Commercial'] == 'X', TRUE, FALSE)
harvest <- lapply(out, function(l) l$Harvest)
harvest_tot <- ldply(harvest, .fun=colSums)[,-1]
colnames(harvest_tot) <- sapply(strsplit(colnames(harvest_tot), '_'), '[', 3)
econ_harvest <- harvest_tot[, colnames(harvest_tot) %in% econ_status[which(econ_status[, 'Commercial']), 'Species']]

# get stored carbon in soil and underground (MT C)
Cstore <- ldply(lapply(out, function(l) l$Soil_OrgMat), .fun='sum')

# get biomass of culturally relevant species
culturallyImportantSpp<-c("WhitePine", "SugarMaple","ServiceBerries", "BUtternut")
biomass <- lapply(out, function(l) l$AvgBiomass[l$AvgBiomass$Time == 100 & l$AvgBiomass$Species %in% culturallyImportantSpp, 3])
biomass <- ldply(biomass, .fun='sum')

# get diversity metric
diversity <- ldply(lapply(out, function(l) l$Diversity[l$Diversity$each_year == 100, 3]), .fun='identity')

harvest <- seq(0, 4)
wind <- seq(0, 3)
climate <- seq(0, 6)
gdd <- seq(0, 2)
scenarios <- expand.grid(harvest, wind, climate, gdd)
dat <- data.frame(scenarios, econ_harvest=rowSums(econ_harvest), Cstore=as.numeric(Cstore$sum), biomass=as.numeric(biomass$sum), diversity=diversity$identity)
colnames(dat) <- c('harvest', 'wind', 'climate', 'gdd', 'econ_harvest', 'Cstore', 'biomass', 'diversity')
dat$climate <- mapvalues(dat$climate, from=c("0", "1", "2", "3", "4", "5", "6"), to=c('Baseline', 'HAD', 'bcc', 'CCSM4', 'CSIRO', 'MRI', 'MIROC'))
dat$harvest <- mapvalues(dat$harvest, from=c("0", "1", "2", "3", "4"), to=c("BAU", "BAU Plus 20%", "BAU Minus 20%", "Even-Aged", "Uneven-Aged"))
dat <- dat %>% group_by(wind, gdd) %>% mutate(deltaC = Cstore - Cstore[climate == 'Baseline' & harvest == 'BAU'])
dat <-  dat %>% group_by(wind, gdd) %>% mutate(deltaH = econ_harvest - econ_harvest[climate == 'Baseline' & harvest == 'BAU'])
dat$deltaH <- dat$deltaH / 1e3
dat <- dat %>% group_by(wind, gdd) %>% mutate(deltaB = biomass - biomass[climate == 'Baseline' & harvest == 'BAU'])
dat$diversity <- log(dat$diversity)
dat <- dat %>% group_by(wind, gdd) %>% mutate(deltaD = diversity[climate == 'Baseline' & harvest == 'BAU'] - diversity)
dat$deltaD <- -exp(dat$deltaD)
dat_diff <- dat %>% filter(climate != 'Baseline')
dat_diff <- as.data.frame(dat_diff)

dat_agg_list <- apply(dat_diff[,9:12], 2, function(col) {aggregate(col, by=list(dat_diff$harvest, dat_diff$climate), FUN=mean)})
dat_agg <- as.data.frame(ldply(dat_agg_list))
colnames(dat_agg) <- c('Metric', 'Harvest', 'Climate', 'Value')
dat_agg <- as.data.frame(pivot_wider(dat_agg, names_from=Metric, values_from=Value))

clim_labeller <- c(
  "Baseline" = "Baseline",
  "HAD" = "High\nWarming +\nDrier",
  "bcc" = "Moderate\nWarming +\nWetter",
  "CCSM4" = "Moderate\nWarming +\nDrier",
  "CSIRO" = "High\nWarming +\nWetter",
  "MRI" = "Less Warming",
  "MIROC" = "Moderate Warming")

colorder <- c('MRI', 'CCSM4', 'MIROC', 'bcc', 'HAD', 'CSIRO')
dat_agg$Climate <- factor(dat_agg$Climate, ordered=TRUE, levels=colorder)
dat_agg$Harvest <- factor(dat_agg$Harvest, ordered=TRUE, levels=colorder)
dat_diff$climate <- factor(dat_diff$climate, ordered=TRUE, levels=colorder)
dat_diff$harvest <- factor(dat_diff$harvest, ordered=TRUE)

cols <- brewer.pal(7, 'Dark2')

### plot parallel axis plot grouped and colored by climate
## combine everything into one big dataframe for plotting
dat_all <- dat_diff  %>%
    group_by(climate) %>%
    summarise(across(starts_with("delta"), list(mean = mean))) %>%
    pivot_longer(cols = starts_with("delta"),
                 names_sep = "_",
                 names_to = c(".value", "variable")) %>%
    mutate(alpha = 1) %>%
    bind_rows(mutate(dat_diff[, c(3, 9:12)], alpha = 0.05, variable = "raw"))

dat_all <- data.frame(dat_all)
colnames(dat_all) <- c('climate', 'summary', 'carbon', 'harvest', 'biomass', 'diversity', 'alpha')
dat_all$summary <- factor(dat_all$summary)

p <- ggparcoord(dat_all, columns = 3:6, groupColumn = 1, order = "anyClass", showPoints = FALSE, alpha = "alpha", shadeBox = NULL, scale = "uniminmax")
p$data$summary <- factor(p$data$summary)
p <- p + geom_line(aes(linetype=summary, alpha=alpha))
p <- p + theme_minimal(base_size=10)
p <- p +  scale_color_manual('Climate Scenario', labels=c("Less Warming", "Moderate Warming + Drier", "Moderate Warming", "Moderate Warming + Wetter", "High Warming + Drier", "High Warming + Wetter"), 
                             values = cols[c(1:5, 7)])
p <- p + scale_linetype_manual('', values=c('solid', '1F'))
p <- p + guides(alpha = "none", linetype="none")
p <- p + scale_x_discrete(labels=c('Change from Baseline\nTotal Harvest\nfrom 2000-2100\n(Tt)', 'Change from Baseline\nShannon\nDiversity', 
                                   'Change from Baseline\nBiomass of\nCulturally Significant\nSpecies\n(g/m^2)', 'Change from Baseline\nStored C\nin 2100\n(Mt C)'),
                          expand=c(0.04, 0.04))
p <- p + scale_y_continuous(expand=c(0.02, 0.02))
# Remove axis ticks and labels
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text.y = element_blank())
# Clear axis lines
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major.y = element_blank())
# Darken vertical lines
p <- p + theme(panel.grid.major.x = element_line(color = "#bbbbbb"))
# Move label to bottom
p <- p + theme(legend.position = "bottom")
# Change axis label font size
p <- p + theme(axis.text.x = element_text(size=9, color="black"))
# Figure out y-axis range after GGally scales the data
min_y <- min(p$data$value)
max_y <- max(p$data$value)
pad_y <- (max_y - min_y) * 0.1
# Calculate label positions for each veritcal bar
lab_x <- rep(1:4, times = 2) # 2 times, 1 for min 1 for max
lab_y <- rep(c(min_y - pad_y, max_y + pad_y), each = 4)
# Get min and max values from original dataset
lab_z <- c(sapply(dat_diff[, c(10, 12, 11, 9)], min), sapply(dat_diff[, c(10, 12, 11, 9)], max))
lab_z[c(1, 3, 4, 5, 7, 8)] <- round(lab_z[c(1, 3, 4, 5, 7, 8)], 0)
lab_z[c(2, 6)] <- round(lab_z[c(2, 6)], 2)
# Convert to character for use as labels
lab_z <- as.character(lab_z)
# Add labels to plot
p <- p + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)
p <- p + annotation_custom(grob=grid::linesGrob(arrow=grid::arrow(type='open', ends='last', length=unit(3, 'mm')), gp=gpar(col='blue')),
                           xmin=0.9, xmax=0.9, ymin=-0, ymax=0.9)
p <- p + annotation_custom(grob=grid::textGrob(label=expression('Preferred Direction'), gp=gpar(col='blue', fontsize=8), rot=90),
                           xmin=0.8, xmax=0.8, ymin=0.25, ymax=0.5)
p <- p + coord_cartesian(xlim=c(0.75, 4.25), clip = "off")
# add zero points to plot
# calculate the y-axis positions after ggparcoord's scaling
zero_y <- rbind(sapply(dat_diff[, c(10, 12, 11, 9)], min), sapply(dat_diff[, c(10, 12, 11, 9)], max))
zero_y <- -zero_y[1,]/(zero_y[2,] - zero_y[1,])
p1 <- p + annotate("point", x=lab_x[c(1, 3, 4)], y=zero_y[c(1, 3, 4)], color="black") # biodiversity doesn't include zero
p1 <- p1 + labs(tag=("a)"))

### plot parallel axis plot grouped and colored by harvest strategy
## combine everything into one big dataframe for plotting
dat_all <- dat_diff  %>%
    group_by(harvest) %>%
    summarise(across(starts_with("delta"), list(mean = mean))) %>%
    pivot_longer(cols = starts_with("delta"),
                 names_sep = "_",
                 names_to = c(".value", "variable")) %>%
    mutate(alpha = 1) %>%
    bind_rows(mutate(dat_diff[, c(1, 9:12)], alpha = 0.05, variable = "raw"))

dat_all <- data.frame(dat_all)
colnames(dat_all) <- c('climate', 'summary', 'carbon', 'harvest', 'biomass', 'diversity', 'alpha')
dat_all$summary <- factor(dat_all$summary)

p <- ggparcoord(dat_all, columns = 3:6, groupColumn = 1, order = "anyClass", showPoints = FALSE, alpha = "alpha", shadeBox = NULL, scale = "uniminmax")
p$data$summary <- factor(p$data$summary)
p <- p + geom_line(aes(linetype=summary, alpha=alpha))
p <- p + theme_minimal(base_size=10)
p <- p +  scale_color_manual('Harvest Strategy', values = cols[c(1:5, 7)])
p <- p + scale_linetype_manual('', values=c('solid', '1F'))
p <- p + guides(alpha = "none", linetype="none")
p <- p + scale_x_discrete(labels=c('Change from Baseline\nTotal Harvest\nfrom 2000-2100\n(Tt)', 'Change from Baseline\nShannon\nDiversity', 
                                   'Change from Baseline\nBiomass of\nCulturally Significant\nSpecies\n(g/m^2)', 'Change from Baseline\nStored C\nin 2100\n(Mt C)'),
                          expand=c(0.04, 0.04))
p <- p + scale_y_continuous(expand=c(0.02, 0.02))
# Remove axis ticks and labels
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text.y = element_blank())
# Clear axis lines
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major.y = element_blank())
# Darken vertical lines
p <- p + theme(panel.grid.major.x = element_line(color = "#bbbbbb"))
# Move label to bottom
p <- p + theme(legend.position = "bottom")
# Change axis label font size
p <- p + theme(axis.text.x = element_text(size=9, color="black"))
# Figure out y-axis range after GGally scales the data
min_y <- min(p$data$value)
max_y <- max(p$data$value)
pad_y <- (max_y - min_y) * 0.1
# Calculate label positions for each veritcal bar
lab_x <- rep(1:4, times = 2) # 2 times, 1 for min 1 for max
lab_y <- rep(c(min_y - pad_y, max_y + pad_y), each = 4)
# Get min and max values from original dataset
lab_z <- c(sapply(dat_diff[, c(10, 12, 11, 9)], min), sapply(dat_diff[, c(10, 12, 11, 9)], max))
lab_z[c(1, 3, 4, 5, 7, 8)] <- round(lab_z[c(1, 3, 4, 5, 7, 8)], 0)
lab_z[c(2, 6)] <- round(lab_z[c(2, 6)], 2)
# Convert to character for use as labels
lab_z <- as.character(lab_z)
# Add labels to plot
p <- p + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)
p <- p + annotation_custom(grob=grid::linesGrob(arrow=grid::arrow(type='open', ends='last', length=unit(3, 'mm')), gp=gpar(col='blue')),
                           xmin=0.9, xmax=0.9, ymin=-0, ymax=0.9)
p <- p + annotation_custom(grob=grid::textGrob(label=expression('Preferred Direction'), gp=gpar(col='blue', fontsize=8), rot=90),
                           xmin=0.8, xmax=0.8, ymin=0.25, ymax=0.5)
p <- p + coord_cartesian(xlim=c(0.75, 4.25), clip = "off")
# add zero points to plot
# calculate the y-axis positions after ggparcoord's scaling
zero_y <- rbind(sapply(dat_diff[, c(10, 12, 11, 9)], min), sapply(dat_diff[, c(10, 12, 11, 9)], max))
zero_y <- -zero_y[1,]/(zero_y[2,] - zero_y[1,])
p2 <- p + annotate("point", x=lab_x[c(1, 3, 4)], y=zero_y[c(1, 3, 4)], color="black") # biodiversity doesn't include zero
p2 <- p2 + labs(tag=("b)"))

pdf('figures/par-coords.pdf', height=8, width=7)
grid.arrange(p1, p2, ncol=1)
dev.off()

png('figures/par-coords.png', height=8, width=7, units="in", res=300)
grid.arrange(p1, p2, ncol=1)
dev.off()
