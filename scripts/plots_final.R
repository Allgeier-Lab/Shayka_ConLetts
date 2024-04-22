###########
#This code takes data from the Seagrass and the Water Column Experiments
#It creates figures for the manuscript
#Created by Bridget Shayka
##########


##Load libraries -------------
library(tidyverse)
library(ggpubr) #for ggarrange
library(akima) #for grids in Fig 3
library(png) #for importing pngs
library(cowplot) #for adding pngs to ggplots
library(magick) #need this with cowplot for drawing pics on ggplots
library(prismatic) #for saturating colors
library(ggrepel) #for adjusting labels on plots
library(patchwork) #for figure 2


##Load data -----------------

growthdata <- read_csv('processed_data/combined_growth.csv',
                       col_types = cols(.default = "d", n = "f", n.s = "f", expt = "f", nfac = "f"))
#ntrt, p, np are dbl; nstand, p.s, np.s are dbl standardized values; n is factor of ntrt; n.s is factor of nstand; nfac is factor of n values as chrs (a,b,c)


seagrassgrowth <- growthdata %>%
  filter(expt == "seagrass") 

phytogrowth <- growthdata %>%
  filter(expt == "phyto") 

dredgeweights <- read_csv('data/Dredgeweights.csv') %>%
  select(-c(numofmodels, totalpts)) %>%
  pivot_longer(cols = !Model,
               names_to = "modelfactor",
               values_to = "weight")

seagrassphytoimg <- readPNG('data/keyimages1.png')
seagrassphytoimg2 <- readPNG('data/keyimages2.png')
seagrassimg <- readPNG('data/seagrass.png')
phytoimg <- readPNG('data/phyto.png')

npseagrassplot <- read_csv('data/seagrass_for_plots.csv')
npphytoplot <- read_csv('data/phyto_for_plots.csv')

seagrassalldata <- read_csv('processed_data/ratio_data_wctrl.csv',
                            col_types = cols(.default = "d", n = "f", n.s = "f", Site = "f", nfac = "f"))

wclongalldata <- read_csv('processed_data/wc_data_long.csv',
                            col_types = cols(.default = "d", n = "f", n.s = "f", bottle = "f", nfac = "f", day = "f"))




##Load models ---------------
growthmodel2b <- lm(growth.s ~ (nstand * p.s * expt) + (log(np.s+10) * expt), data=growthdata) #assumptions are good

seagrass_extrap <- lm(growth ~ ntrt + p + np, data = seagrassgrowth) #good assumptions
phyto_extrap <- lm(sqrt(growth) ~ ntrt + p + np, data = phytogrowth) #good assumptions



##Figure 2 (Fig 1 is the conceptual figure) --------

emp <- emmeans::emmip(growthmodel2b, expt ~ p.s, 
                      at = list(p.s = c(0.162,0.386,-0.360,-0.509,0.908,-0.524,-0.062,0.810,0.480,0.217,-0.047,-0.377,-0.575)), #-0.509 in both
                      CIs = T, plotit = F)
growthp1 <- ggplot() +
  geom_point(aes(x=p.s, y=growth.s, color=expt), size = 5, data=growthdata, show.legend = FALSE) +
  theme_classic() +
  draw_image(seagrassphytoimg,  x = -0.9, y = 0.71, scale = .5) +
  geom_line(aes(y=yvar, x=xvar, color=expt), data=emp) +
  geom_ribbon(aes(y=yvar, x=xvar, ymin=LCL, ymax=UCL, fill=expt), alpha=0.1, data=emp) +
  labs(x= 'P treatment (standardized)',
       y= 'Growth rate (standardized)',
       color= "Experiment",
       fill = "Experiment",
       tag = "(a)") +
  scale_color_manual(values = c("#b2df8a", "#1f78b4"),
                     labels=c('Seagrass', 'Phytoplankton')) +
  scale_fill_manual(values = c("#b2df8a", "#1f78b4"),
                    labels=c('Seagrass', 'Phytoplankton')) +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=24, color = "black"),
        plot.tag = element_text(face = "bold", size=24),
        legend.position = "none")  


emn <- emmeans::emmip(growthmodel2b, expt ~ nstand, 
                      at = list(nstand = c(-0.598, -0.554, -0.079, 0, 0.598, 0.633)), 
                      CIs = T, plotit = F)
growthn1 <- ggplot() +
  geom_point(aes(x=nstand, y=growth.s, color=expt), size = 5, data=growthdata, show.legend = FALSE) +
  theme_classic() +
  draw_image(seagrassphytoimg,  x = -0.95, y = 0.69, scale = .5) +
  geom_line(aes(y=yvar, x=xvar, color=expt), data=emn) +
  geom_ribbon(aes(y=yvar, x=xvar, ymin=LCL, ymax=UCL, fill=expt), alpha=0.1, data=emn) +
  labs(x= 'N treatment (standardized)',
       y= 'Growth rate (standardized)',
       color= "Experiment",
       fill = "Experiment",
       tag = "(b)") +
  scale_color_manual(values = c("#b2df8a", "#1f78b4"),
                     labels=c('Seagrass', 'Phytoplankton')) +
  scale_fill_manual(values = c("#b2df8a", "#1f78b4"),
                    labels=c('Seagrass', 'Phytoplankton')) +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=24, color = "black"),
        plot.tag = element_text(face = "bold", size=24),
        legend.position = "none")




dredgeweights$Model = factor(dredgeweights$Model, levels = c("Full", "Seagrass", "Phytoplankton"), ordered = T)
dredgeweights$modelfactor = factor(dredgeweights$modelfactor, levels = c("NP","N","P","NxP","Expt","ExptxNP","ExptxN","ExptxP","ExptxNxP"), ordered = T)

dredgeplot <- ggplot() +
  geom_col(aes(x=modelfactor, y=weight, fill=Model), data=dredgeweights,
           position = position_dodge(), show.legend = FALSE) +
  theme_classic() +
  scale_x_discrete(labels=c("NxP"="N x P","ExptxNP"="Expt x NP","ExptxN"="Expt x N","ExptxP"="Expt x N","ExptxNxP"="Expt x N x P")) +
  scale_fill_manual(values = c("Full" = "#BEBEBE", "Seagrass" = "#b2df8a", "Phytoplankton" = "#1f78b4"),
                    labels=c('Full model', 'Seagrass only', 'Phytoplankton only')) +
  labs(x= "Model Factor",
       y= "Weight",
       color= "Experiment",
       fill = "Experiment",
       tag = "(c)") +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "blank") +
  theme(axis.text = element_text(size=24),
        axis.title.y = element_text(size=24, color = "black"),
        axis.title.x = element_blank(),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.tag = element_text(face = "bold", size=24)) +
  draw_image(seagrassphytoimg2,  x = 7.9, y = 0.43, scale = 2)



seagrassnpgam <- ggplot(seagrassgrowth, aes(np, growth)) +
  geom_point(size=5, color="#b2df8a") +
  theme_classic() +
  draw_image(seagrassimg, x=145, y=0.00141, halign = 0, valign=0, scale = 12) +
  geom_smooth(method = "gam", formula = y ~ s(x), fill="#b2df8a", alpha=0.1, color="#b2df8a") + #this fits a gam line instead of the default loess
  labs(x= "N:P",
       y= expression('Seagrass growth ('*g~shoot^-1~d^-1*')'),
       color= "Experiment",
       tag = "(d)") +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=24, color = "black"),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24),
        plot.tag = element_text(face = "bold", size=24),
        legend.position = "none")


phytonplinear <- ggplot(phytogrowth, aes(np, growth)) +
  geom_point(size=5, color="#1f78b4") +
  theme_classic() +
  draw_image(phytoimg, x=90, y=0.0031, halign = 0, valign=0, scale = 18) +
  geom_smooth(method = "lm", fill="#1f78b4", alpha=0.1, color="#1f78b4") + #this fits a linear line instead of the default loess
  labs(x= "N:P",
       y= expression('Phytoplankton growth ('*mu*g~L^-1~hr^-1*')'),
       color= "Experiment",
       tag = "(e)") +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=24, color = "black"),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24),
        plot.tag = element_text(face = "bold", size=24),
        legend.position = "none")

layout <- "
AB
CC
DE"
fig2patch <- growthp1 + growthn1 + dredgeplot + seagrassnpgam + phytonplinear + plot_layout(design = layout)

ggsave(filename = "Fig2.tiff", path="outputs", plot=fig2patch, device = "tiff", width = 15, height = 20, units="in", dpi=300)


##Figure 3 ------------

#seagrass expt ranges: N = 0.11-0.61, P = 0.008-0.2 (g m-2 d-1 for both)
seagrass_pred_df <- data.frame(
  ntrt = rep(seq(from = 0.02, to = 3, length.out = 30), each = 30), #each repeats each number x times
  p = rep(seq(from = 0.007, to = 0.3, length.out = 30), times = 30) #times repeats the whole series x times
) %>%
  mutate(np = (ntrt/14)/(p/30.97))

seagrass_pred_result <- add_column(seagrass_pred_df, data.frame(predict(seagrass_extrap, newdata = seagrass_pred_df, interval = "confidence"))) %>%
  mutate(growth = fit)

interpdatasg <- akima::interp(y=seagrass_pred_result$ntrt, x=seagrass_pred_result$p, z=seagrass_pred_result$growth, 
                              duplicate = "mean") #duplicate tells it what to do with duplicate z values #, duplicate="mean"
interpdfsg <- interp2xyz(interpdatasg, data.frame = T) #turns list output from interp() into df

seagrassinterpplot <- ggplot() + 
  geom_tile(aes(x = x, y = y, fill = z), data=interpdfsg) +
  theme_classic() +
  labs(x= expression('P ('*g~m^-2~d^-1*')'),
       y= expression('N ('*g~m^-2~d^-1*')'),
       fill = expression('Seagrass growth rate ('*g~shoot^-1~d^-1*')'),
       tag = "(a)") +
  scale_fill_gradient(low="white", high=prismatic::clr_darken("#b2df8a", shift = 0.1),
                         breaks=c(0.0003,0.0011,0.0019),
                         labels=c("0.0003","0.0011","0.0019"), limits=c(0.0001,0.00195)) +
  geom_segment(aes(x = 0.008, y = 0.11, xend = 0.2, yend = 0.11), linetype="dashed") +
  geom_segment(aes(x = 0.008, y = 0.61, xend = 0.2, yend = 0.61), linetype="dashed") +
  geom_segment(aes(x = 0.2, y = 0.11, xend = 0.2, yend = 0.61), linetype="dashed") +
  geom_segment(aes(x = 0.008, y = 0.11, xend = 0.008, yend = 0.61), linetype="dashed") +
  geom_point(aes(x=p, y=n), data=npseagrassplot) +
  geom_text_repel(aes(x=p, y=n, label=place), data=npseagrassplot, nudge_x = 0.03) +
  theme(legend.position = "bottom", legend.title.align = 0.5) +
  guides(fill = guide_colourbar(title.position="top", barwidth = 23)) +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18, color = "black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        plot.margin = margin(10, 11, 10, 10), #top,right,bottom,left
        axis.text.x=element_text(vjust=0.5),
        plot.tag = element_text(face = "bold", size=18)) +
  scale_x_continuous(limits=c(0,0.301), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0,2.5), expand = c(0, 0))


#phyto expt ranges: N = 10-50, P = 1-22 (ug L-1 for both)
phyto_pred_df <- data.frame(
  ntrt = rep(seq(from = 4, to = 125, length.out = 20), each = 10), #each repeats each number x times
  p = rep(seq(from = 1, to = 47, length.out = 10), times = 20) #times repeats the whole series x times
) %>%
  mutate(np = (ntrt/14.0067)/(p/30.97376))

phyto_pred_result <- add_column(phyto_pred_df, data.frame(predict(phyto_extrap, newdata = phyto_pred_df, interval = "confidence"))) %>%
  mutate(growth = fit^2)

interpdataphyto <- akima::interp(y=phyto_pred_result$ntrt, x=phyto_pred_result$p, z=phyto_pred_result$growth, 
                                 duplicate = "mean") #duplicate tells it what to do with duplicate z values #, duplicate="mean"
interpdfphyto <- interp2xyz(interpdataphyto, data.frame = T) #turns list output from interp() into df

phytointerpplot <- ggplot() +
  geom_tile(aes(x = x, y = y, fill = z), data=interpdfphyto) +
  theme_classic() +
  labs(x= expression('P (TP '*mu*g~L^-1*')'),
       y= expression('N (NH'[4]^'+ '*mu*g~L^-1*')'),
       fill = expression('Phytoplankton growth rate ('*mu*g~L^-1~hr^-1*')'),
       tag = "(b)") +
  scale_fill_gradient(low="white", high="#1f78b4",
                                               breaks=c(0.001,0.0035,0.006),
                                               labels=c(0.0010,0.0035,0.0060), limits=c(0.0009,0.00605)) +
  geom_segment(aes(x = 1, y = 10, xend = 22, yend = 10), linetype="dashed") +
  geom_segment(aes(x = 1, y = 50, xend = 22, yend = 50), linetype="dashed") +
  geom_segment(aes(x = 1, y = 10, xend = 1, yend = 50), linetype="dashed") +
  geom_segment(aes(x = 22, y = 10, xend = 22, yend = 50), linetype="dashed") +
  geom_point(aes(x=p, y=n), data=npphytoplot) +
  geom_text_repel(aes(x=p, y=n, label=place), data=npphytoplot) + #, direction = "y"
  theme(legend.position = "bottom", legend.title.align = 0.5) +
  guides(fill = guide_colourbar(title.position="top", barwidth = 23)) +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18, color = "black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        plot.margin = margin(10, 16, 10, 10),
        axis.text.x=element_text(vjust=0.5),
        plot.tag = element_text(face = "bold", size=18)) +
  scale_x_continuous(limits=c(0,46), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0,120), expand = c(0, 0))


fig3 <- ggpubr::ggarrange(seagrassinterpplot, phytointerpplot,
                          ncol = 2, nrow = 1,
                          font.label = list(size = 20))

ggsave(filename = "Fig3.tiff", path="outputs", plot=fig3, device = "tiff", width = 12.5, height = 7, units="in", dpi=300)




##Figures S1 & S2 ------------
#Plots of seagrass responses with nutrient treatments
bladearean <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=avgbladearea)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Blade Area ('*mm^2~shoot^-1*')')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
bladeareap <- seagrassalldata %>%
  ggplot(aes(x=p, y=avgbladearea)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Blade Area ('*mm^2~shoot^-1*')')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

bladeheightn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=avgheight)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Blade Height (mm)')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
bladeheightp <- seagrassalldata %>%
  ggplot(aes(x=p, y=avgheight)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Blade Height (mm)')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

epsn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=avgepweightperarea)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Epiphytes ('*g~mm^-2*')')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
epsp <- seagrassalldata %>%
  ggplot(aes(x=p, y=avgepweightperarea)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Epiphytes ('*g~mm^-2*')')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

bitesn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=avgbitespersa)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Bites '*mm^-2)) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
bitesp <- seagrassalldata %>%
  ggplot(aes(x=p, y=avgbitespersa)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Bites '*mm^-2)) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

richn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=species)) +
  geom_jitter(width = 0.008, height=0) +
  theme_classic() +
  ylab(label=expression('Richness')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
richp <- seagrassalldata %>%
  ggplot(aes(x=p, y=species)) +
  geom_jitter(width = 0.002, height=0) +
  theme_classic() +
  ylab(label=expression('Richness')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

divn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=D)) +
  geom_jitter(width = 0.008, height=0) +
  theme_classic() +
  ylab(label=expression('Diversity')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
divp <- seagrassalldata %>%
  ggplot(aes(x=p, y=D)) +
  geom_jitter(width = 0.002, height=0) +
  theme_classic() +
  ylab(label=expression('Diversity')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

shootsn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=tshoots)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Shoots '*m^-2)) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
shootsp <- seagrassalldata %>%
  ggplot(aes(x=p, y=tshoots)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('Shoots '*m^-2)) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

ccontn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=meanC)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('%C')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
ccontp <- seagrassalldata %>%
  ggplot(aes(x=p, y=meanC)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('%C')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

ncontn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=meanN)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('%N')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
ncontp <- seagrassalldata %>%
  ggplot(aes(x=p, y=meanN)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('%N')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

pcontn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=meanP)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('%P')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
pcontp <- seagrassalldata %>%
  ggplot(aes(x=p, y=meanP)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('%P')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

ctonn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=CN)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('C:N')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
ctonp <- seagrassalldata %>%
  ggplot(aes(x=p, y=CN)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('C:N')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

ctopn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=CP)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('C:P')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
ctopp <- seagrassalldata %>%
  ggplot(aes(x=p, y=CP)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('C:P')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))

ntopn <- seagrassalldata %>%
  ggplot(aes(x=ntrt, y=NP)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('N:P')) +
  xlab(label=expression('N ('*g~m^-2~d^-1*')'))
ntopp <- seagrassalldata %>%
  ggplot(aes(x=p, y=NP)) +
  geom_point() + 
  theme_classic() +
  ylab(label=expression('N:P')) +
  xlab(label=expression('P ('*g~m^-2~d^-1*')'))


quartz()
figS1 <- ggpubr::ggarrange(ggarrange(epsn, epsp, labels = c("A1", "A2"), ncol = 2),
                           ggarrange(bitesn, bitesp, labels = c("B1", "B2"), ncol = 2),
                           ggarrange(richn, richp, labels = c("C1", "C2"), ncol = 2),
                           ggarrange(divn, divp, labels = c("D1", "D2"), ncol = 2),
                           ggarrange(shootsn, shootsp, labels = c("E1", "E2"), ncol = 2),
                           ggarrange(ccontn, ccontp, labels = c("F1", "F2"), ncol = 2),
                           ggarrange(ncontn, ncontp, labels = c("G1", "G2"), ncol = 2),
                           ggarrange(pcontn, pcontp, labels = c("H1", "H2"), ncol = 2),
                           nrow = 8)

figS2 <- ggpubr::ggarrange(ggarrange(ctonn, ctonp, labels = c("A1", "A2"), ncol = 2),
                           ggarrange(ctopn, ctopp, labels = c("B1", "B2"), ncol = 2),
                           ggarrange(ntopn, ntopp, labels = c("C1", "C2"), ncol = 2),
                           ggarrange(bladearean, bladeareap, labels = c("D1", "D2"), ncol = 2),
                           ggarrange(bladeheightn, bladeheightp, labels = c("E1", "E2"), ncol = 2),
                           nrow = 5) 

ggsave(filename = "FigS1.pdf", path="outputs", plot=figS1, device = "pdf", width = 8, height = 22, units="in", dpi=300)
ggsave(filename = "FigS2.pdf", path="outputs", plot=figS2b, device = "pdf", width = 8, height = 16, units="in", dpi=300)


##Figure S3 ------------
#Plots of ammonium and phosphate (by day) in phytoplankton experiment

nh4n <- wclongalldata %>%
  filter(day == 1 | day == 3) %>%
  ggplot(aes(x=ntrt, y=nh4, color=day)) +
  geom_point(size=4) + 
  theme_classic() +
  theme(axis.text = element_text(size = 18, colour = "black"), 
        axis.title = element_text(size = 18, colour = "black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  labs(x= expression('N ('*mu*g~L^-1*')'),
       y= expression('Ammonium ('*mu*g~L^-1*')'),
       color= "Day")

srpp <- wclongalldata %>%
  filter(day == 1 | day == 3) %>%
  ggplot(aes(x=p, y=srp, color=day)) +
  geom_point(size=4) + 
  theme_classic() +
  theme(axis.text = element_text(size = 18, colour = "black"), 
        axis.title = element_text(size = 18, colour = "black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18)) +
  labs(x= expression('P ('*mu*g~L^-1*')'),
       y= expression('Soluble Reactive Phosphorus ('*mu*g~L^-1*')'),
       color= "Day")

figS3 <- ggpubr::ggarrange(nh4n, srpp,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           font.label = list(size = 20),
                           common.legend = T, legend = "bottom")

ggsave(filename = "FigS3.pdf", path="outputs", plot=figS3, device = "pdf", width = 13, height = 6, units="in", dpi=300)




