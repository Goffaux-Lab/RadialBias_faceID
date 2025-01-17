###################################################################################
###################################################################################
# Roux-Sibilon, Peyrin, Greenwood, and Goffaux
# Radial bias in face identification
###################################################################################
###################################################################################
packages = c("tidyverse",
             "magrittr",
             "readxl",
             "Rmisc" ,
             "ggplot2",
             "reshape2",
             "lme4",
             "lmerTest",
             "modelbased",
             "ggeffects")

## Load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


load('.RData')





#color palette
#https://davidmathlogic.com/colorblind/#%23332288-%23F9A63D-%2334A584
colorblind.UL <- c("black", "#4a3466", "#B983FF")
color.fov.UL <- c("black", "grey40")
color.hor.UL <- c("#4a3466", "grey40")
color.vert.UL <- c("#B983FF", "grey40")

colorblind.LR <- c("black", "#00814f", "#00D885")
color.fov.LR <- c("black", "grey40")
color.hor.LR <- c("#00814f", "grey40")
color.vert.LR <- c("#00D885", "grey40")


colorblind <- c("black", "#995e4e", "#ff9e83")
color.fov <- c("black", "grey40")
color.hor <- c("#995e4e", "grey40")
color.vert <- c("#ff9e83", "grey40")

colorsslopes <- c("#00D885", "#B983FF")

# ggplot theme 
alxtheme <- theme_bw() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.background = element_rect(fill = 'white'),
    panel.border = element_blank(), #element_rect(size = 2, color = "grey"),
    axis.line = element_blank(), 
    strip.background = element_rect(fill = 'white', color = "white"),
    strip.placement = 'outside',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines"),
    axis.text = element_text(colour = "black"),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 17)) 




# ages group up-left
ages <- c(24,25,20,20,19,20,19,22,19,20,24,23,23,22,21,21,23,23,22,20)
meanage <- mean(ages)
sdage <- sd(ages)

# 1 -- Prepare data ----------------------------------------------------------------------------------------------------

# Import data (from excel files created by E-merge / E-Prime)

dfUL <- read_excel("data_left_up.xlsx")
dfUL %<>%
  filter(ExperimentName != "ExperimentName") %>% 
  select(
    Subject = SubjectID,
    Session = Session,
    triplet = triplet,
    inversion = plannarOr,
    firstName = firstName,
    Running = Running,
    correctResp = correct,
    phase = PhaCoh,
    imName = imName,
    Resp = mask.RESP,
    RT = mask.RT,
    accuracy = mask.ACC,
    x = x,
    y = y,
    VF = VF
  ) %>% 
  mutate(subjGroup = "up-left")


dfUL %<>%
  mutate (
    Subject =
      case_when(
        Subject == "roso0909"  ~ "ROSO0909",
        TRUE            ~ Subject
      )) 


dfLR <- read_excel("data_right_low.xlsx")
dfLR %<>%
  select (
    Subject = Subject,
    Session = Session,
    triplet = triplet,
    inversion = plannarOr,
    firstName = firstName,
    Running = Running,
    correctResp = correct,
    phase = PhaCoh,
    imName = imName,
    Resp = mask.RESP,
    RT = mask.RT,
    accuracy = mask.ACC,
    x = x,
    y = y,
    VF = VF
  )%>% 
  mutate(subjGroup = "low-right")

dfLR %<>%
  mutate (
    Subject =
      case_when(
        Subject == 18  ~ 101,
        Subject == 018 ~ 101,
        Subject == 16033 ~ 1603,
        TRUE            ~ Subject
      )) 





df <- rbind(dfUL,dfLR)







# Recode phase coherence levels
# Phase coherence starts from 0% to 90%, with 7 steps
interval <- 90/6 #interval size = 15%
print(seq(0,90,interval))

df %<>%
  mutate (
    phase =
      case_when(
        phase == "SCR1" ~ 1,
        phase == "SCR2" ~ 2,
        phase == "SCR3" ~ 3,
        phase == "SCR4" ~ 4,
        phase == "SCR5" ~ 5,
        phase == "SCR6" ~ 6,
        phase == "SCR7" ~ 7,
        TRUE            ~ 555
      )) %>%
  mutate (
    VF =
      case_when(
        VF == "fovea"    ~ "Fovea",
        VF == "leftVF8"  ~ "Left-HM",
        VF == "upVF8"    ~ "Upper-VM",
        VF == "rightVF8" ~ "Right-HM",
        VF == "lowVF8"   ~ "Lower-VM",
        TRUE             ~ "555"
      )
  )


df$phase <- as.numeric(df$phase) #must be numeric

df$RT <- as.numeric(df$RT) #must be numeric

df$accuracy <- as.numeric(df$accuracy) #must be numeric



df %<>%
  filter (Resp %in% c("1", "2", "3")) #remove trials for which no response was given



# To remove RTs outliers?

rt.averages <- df %>% 
  dplyr::summarise(
    meanRT = mean(RT),
    sd = sd(RT)
  ) 


lowerCut = rt.averages$meanRT - 3*(rt.averages$sd)
higherCut = rt.averages$meanRT + 3*(rt.averages$sd)

df %<>%
  mutate(
    RToutlier = case_when(
      RT < lowerCut  ~ 1,
      RT > higherCut ~ 1,
      TRUE           ~ 0
    )
  ) %>% 
  filter(RToutlier != 1)



# ADD MERIDIAN VARIABLE

df %<>% 
  mutate (meridian =
            case_when(
              VF == "Fovea" ~ "Fovea",
              VF == "Left-HM" ~ "HorizontalMeridian",
              VF == "Right-HM" ~ "HorizontalMeridian",
              VF == "Lower-VM" ~ "VerticalMeridian",
              VF == "Upper-VM" ~ "VerticalMeridian",
              TRUE            ~ "truc"
            ))



df$inversion <-
  factor(df$inversion, levels = c("upright", "inverted"))


df$triplet <- as.factor(df$triplet)



# List groups and subjects 
df$Subject <- as.factor(df$Subject)
subj <- levels(df$Subject)

groups <- df %>% 
  select(Subject, subjGroup) %>% 
  group_by(Subject) %>% 
  unique()

subjLR <- groups %>% filter(subjGroup == "low-right")
subjLR <- as.vector(subjLR$Subject)
subjUL <- groups %>% filter(subjGroup == "up-left")
subjUL <- as.vector(subjUL$Subject)

tripletz <- df %>% 
  select(Subject, subjGroup, triplet) %>% 
  group_by(Subject) %>% 
  unique()

# 2 -- Plot raw data -------------------------------------------------------------------------

# both groups averaged
averages <- df %>%
  group_by(Subject, meridian, inversion, phase) %>%
  dplyr::summarise(
    accuracy = mean(accuracy) # first summarize across trial (1 value per subj*condition)
  ) %>% 
  Rmisc::summarySE( # then compute CI etc.
    measurevar = "accuracy",
    groupvars = c("meridian", "inversion", "phase"))
averages$meridian <- as.factor(averages$meridian)

ggplot(
  averages,
  aes(
    x = phase,
    y = accuracy,
    color = inversion,
    fill = inversion,
    group = inversion
  )
) +
  geom_point (shape = 21, size = 4) +
  geom_line (size = 2) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = .2,
                size = 1.2) +
  facet_grid (. ~ meridian, switch = 'y') +
  labs (y = "Accuracy", x = "Phase coherence") +
  scale_color_manual (values = colors) +
  scale_fill_manual (values = colors) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0.25, 1), xlim = c(0, 7)) +
  geom_segment(x = 1, xend = 7, y = 0.25, yend = 0.25) +
  geom_segment(x = 0, xend = 0, y = 0.33, yend = 1) 






# Group Up-left + Group Low-right
averages2 <- df %>%
  group_by(subjGroup, Subject, meridian, inversion, phase) %>%
  dplyr::summarise(
    accuracy = mean(accuracy) # first summarize across trial (1 value per subj*condition)
  ) %>% 
  Rmisc::summarySE( # then compute CI etc.
    measurevar = "accuracy",
    groupvars = c("subjGroup", "meridian", "inversion", "phase"))
averages2$meridian <- as.factor(averages2$meridian)


ggplot(
  averagesIE,
  aes(
    x = phase,
    y = IE,
    color = meridian
  )
) +
  geom_point (size = 4, position = position_dodge(width = 0.5), alpha = 0.5) +
  geom_line (size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = IE - ci,
                    ymax = IE + ci),
                width = 0,
                size = 1.2, 
                position = position_dodge(width = 0.5), 
                alpha = 0.5) +
  labs (y = "Accuracy", x = "Phase coherence") +
  # scale_color_manual (values = colors) +
  # scale_fill_manual (values = colors) +
  scale_x_continuous (expand = c(0, 0)) +
  alxtheme +
  theme (
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 20),
    axis.text = element_text(size = 15)
  ) + theme_tufte()





# 3 -- Fit hierarchical GLMM models ----------------------------------------------------------------------------------------

# Full model, LOWRIGHT
fullmodel_LR = glmer(
  accuracy ~ 1 + phase*meridian*inversion + (1 + phase + meridian + inversion || triplet/Subject),
  family = binomial(link = "probit"),
  data = df %>% filter(subjGroup == "low-right"),
  control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=100000))
)
saveRDS(fullmodel_LR, file = "GLMM_logistic/models/fullmodel_LOWRIGHT.rds")

# Full model, UPLEFT
fullmodel_UL = glmer(
  accuracy ~ 1 + phase*meridian*inversion + (1 + phase + meridian + inversion || triplet/Subject),
  family = binomial(link = "probit"),
  data = df %>% filter(subjGroup == "up-left"),
  control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=100000))
)
saveRDS(fullmodel_UL, file = "GLMM_logistic/models/fullmodel_UPLEFT.rds")



# Main effect of Inversion at each position
df$subjGroup <- as.factor(df$subjGroup)
twogroups <- levels(df$subjGroup)

df$meridian <- as.factor(df$meridian)
meridians <- levels(df$meridian)


for (thisGroup in twogroups) {
  for (thisMeridian in meridians) {

    # fit main effect model
    fit = glmer(
      accuracy ~ 1 + phase*inversion + (1 + phase*inversion || triplet/Subject),
      family = binomial(link = "probit"),
      data = df %>% filter(subjGroup == thisGroup, meridian == thisMeridian),
      control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=100000))
    )
    zegroup = ifelse(thisGroup == "low-right", "LOWRIGHT", "UPLEFT")
    filename = paste("GLMM_logistic/models/", zegroup, "_inversion_", thisMeridian, ".rds", sep = "")
    saveRDS(fit, file = filename)
    
  }
}



# Critical interaction

# .... for each group
for (thisGroup in twogroups) {
  
  # fit radial interaction model
  fit = glmer(
    accuracy ~ 1 + phase*inversion*meridian + (1 + phase*inversion*meridian || triplet/Subject),
    family = binomial(link = "probit"),
    data = df %>% filter(subjGroup == thisGroup, meridian != "Fovea"),
    control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000))
  )
  zegroup = ifelse(thisGroup == "low-right", "LOWRIGHT", "UPLEFT")
  filename = paste("GLMM_logistic/models/", zegroup, "_radialInter", ".rds", sep = "")
  saveRDS(fit, file = filename)
  
}



# 4 -- Test effects ---------------------------------------------------------------------------------------------------


# up-left ----------------------------------------------------------------------------------------------------------


# Is there an inversion effect at each VF location?

#fovea
UPLEFT_inversion_Fovea <- readRDS("GLMM_logistic/models/UPLEFT_inversion_Fovea.rds")
summary(UPLEFT_inversion_Fovea)

#leftHM
UPLEFT_inversion_HorizontalMeridian <- readRDS("GLMM_logistic/models/UPLEFT_inversion_HorizontalMeridian.rds")
summary(UPLEFT_inversion_HorizontalMeridian)

#upperVM
UPLEFT_inversion_VerticalMeridian <- readRDS("GLMM_logistic/models/UPLEFT_inversion_VerticalMeridian.rds")
summary(UPLEFT_inversion_VerticalMeridian)


# Is the IE larger on the horizontal than vertical meridian?
UPLEFT_radialInter <- readRDS("GLMM_logistic/models/UPLEFT_radialInter.rds")
summary(UPLEFT_radialInter)



# low-right -------------------------------------------------------------------------------------------------------

# Is there an inversion effect at each VF location?

#fovea
LOWRIGHT_inversion_Fovea <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_Fovea.rds")
summary(LOWRIGHT_inversion_Fovea)

#rightHM
LOWRIGHT_inversion_HorizontalMeridian <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_HorizontalMeridian.rds")
summary(LOWRIGHT_inversion_HorizontalMeridian)

#lowerVM
LOWRIGHT_inversion_VerticalMeridian <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_VerticalMeridian.rds")
summary(LOWRIGHT_inversion_VerticalMeridian)


# Is the IE larger on the horizontal than vertical meridian?
LOWRIGHT_radialInter <- readRDS("GLMM_logistic/models/LOWRIGHT_radialInter.rds")
summary(LOWRIGHT_radialInter)



# 5 -- Plot full models ----------------------------------------------------------------------------------------------------

# Plot UPLEFT
predictionsUL <-
  ggeffects::ggpredict(fullmodel_UL, terms = c("phase", "inversion", "meridian")) %>%
  dplyr::rename(phase = x,
                meridian = facet,
                inversion = group)

levels(predictionsUL$meridian) <- c("Fovea", "Left Horizontal Meridian", "Upper Vertical Meridian")

averagesUL <- averages2 %>% filter(subjGroup == "up-left")
averagesUL$meridian <- as.factor(averagesUL$meridian)
levels(averagesUL$meridian) <- c("Fovea", "Left Horizontal Meridian", "Upper Vertical Meridian")

averagesUL %>%
  ggplot(aes(
    x = phase,
    y = accuracy,
    color = inversion,
    fill = inversion,
    group = inversion
  )) +
  geom_point (shape = 21, size = 4) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1.5) +
  geom_line(
    data = predictionsUL,
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 1.5
  ) +
  geom_ribbon(
    data = predictionsUL,
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.1
  ) +
  facet_grid (. ~ meridian, switch = 'y') +
  labs (y = "Accuracy", x = "Phase coherence") +
  scale_color_manual (values = colorsLeftUP, name = "") +
  scale_fill_manual (values = colorsLeftUP, name = "") +
  # geom_hline (yintercept = .50,
  #             color = 'black',
  #             size = 0.2) +
  # geom_hline (yintercept = .67,
  #             color = 'blue',
  #             size = 0.2) +
  alxtheme +
  theme (
    text = element_text(size = 25),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(1,7,1)) +#seq(0, 90, 15)) +
  labs(title = "up-left group") 




# Plot LOWRIGHT
predictionsLR <-
  ggeffects::ggpredict(fullmodel_LR, terms = c("phase", "inversion", "meridian")) %>%
  dplyr::rename(phase = x,
                meridian = facet,
                inversion = group)

levels(predictionsLR$meridian) <- c("Fovea", "Left Horizontal Meridian", "Upper Vertical Meridian")

averagesLR <- averages2 %>% filter(subjGroup == "low-right")
averagesLR$meridian <- as.factor(averagesLR$meridian)
levels(averagesLR$meridian) <- c("Fovea", "Left Horizontal Meridian", "Upper Vertical Meridian")

averagesLR %>%
  ggplot(aes(
    x = phase,
    y = accuracy,
    color = inversion,
    fill = inversion,
    group = inversion
  )) +
  geom_point (shape = 21, size = 4) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1.5) +
  geom_line(
    data = predictionsLR,
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 1.5
  ) +
  geom_ribbon(
    data = predictionsLR,
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.1
  ) +
  facet_grid (. ~ meridian, switch = 'y') +
  labs (y = "Accuracy", x = "Phase coherence") +
  scale_color_manual (values = colorsRightLOW, name = "") +
  scale_fill_manual (values = colorsRightLOW, name = "") +
  # geom_hline (yintercept = .50,
  #             color = 'black',
  #             size = 0.2) +
  # geom_hline (yintercept = .67,
  #             color = 'blue',
  #             size = 0.2) +
  alxtheme +
  theme (
    text = element_text(size = 25),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(1,7,1)) +#seq(0, 90, 15)) +
  labs(title = "low-right group") 







# 6 -- Plot observer-level fit (GLM + GLMM) ---------------------------------------------------------------------------


# up-left -------------------------------------------------------------------------------------------------------------
GLMMpreds.UL <- crossing(
  phase = seq(
    from = 1,
    to = 7,
    length.out = 100
  ),
  Subject = subjUL,
  inversion = c("upright", "inverted"),
  meridian = c("HorizontalMeridian", "VerticalMeridian")
)


tripletC <- tripletz %>% filter(subjGroup == "up-left", triplet == "C")
tripletC <- as.vector(tripletC$Subject)

GLMMpreds.UL %<>%
  mutate (triplet = ifelse(Subject %in% tripletC, "C", "F"))

#add predictions
GLMMpreds.UL$predict = predict(UPLEFT_radialInter_maxfun, type = "response", newdata = GLMMpreds.UL, allow.new.levels = TRUE)



for (thisSubj in subjUL) {
  
  minidf <- df %>%
    filter(Subject == thisSubj, meridian != "Fovea")
  
  
  # Predictions of the GLMM for this subject
  GLMMpreds.thisSubj <- GLMMpreds.UL %>% 
    filter(Subject == thisSubj)

  # create plot
  plot <- minidf %>%
    ggplot(aes(
      x = phase,
      y = accuracy,
      color = inversion,
      group = inversion
    )) +
    stat_summary(fun.data = "mean_cl_normal", size = 0.75) +
    geom_line(
      data = GLMMpreds.thisSubj,
      aes(
        x = phase,
        y = predict,
        color = inversion,
        group = inversion
      ),
      inherit.aes = FALSE,
      size = 1.5,
      linetype = "solid",
      show.legend = FALSE
    ) +
    facet_grid( ~ meridian) +
    scale_color_manual (values = colorsLeftUP) +
    scale_fill_manual (values = colorsLeftUP) +
    labs(x = "phase coherence", y = "accuracy") +
    alxtheme +
    theme (text = element_text(size = 20),
           axis.text = element_text(size = 15))+
    scale_y_continuous(breaks = c(0, 0.33, 0.67, 1), limits = c(0,1.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15)) 
  plot
  
  # open jpeg
  jpgname <- paste("GLMM_logistic/wihtin_probit_plots/fit", thisSubj, ".jpg", sep = "")
  
  jpeg(jpgname, width = 700, height = 400)
  
  # print plot
  print(plot)
  
  # close jpeg file
  dev.off()
  
  
}





# low-right -----------------------------------------------------------------------------------------------------------
GLMMpreds.LR <- crossing(
  phase = seq(
    from = 1,
    to = 7,
    length.out = 100
  ),
  Subject = subjLR,
  inversion = c("upright", "inverted"),
  meridian = c("HorizontalMeridian", "VerticalMeridian")
)


tripletC <- tripletz %>% filter(subjGroup == "low-right", triplet == "C")
tripletC <- as.vector(tripletC$Subject)

GLMMpreds.LR %<>%
  mutate (triplet = ifelse(Subject %in% tripletC, "C", "F"))

#add predictions
GLMMpreds.LR$predict = predict(LOWRIGHT_radialInter_maxfun, type = "response", newdata = GLMMpreds.LR, allow.new.levels = TRUE)



for (thisSubj in subjLR) {
  
  minidf <- df %>%
    filter(Subject == thisSubj, meridian != "Fovea")
  
  
  # Predictions of the GLMM for this subject
  GLMMpreds.thisSubj <- GLMMpreds.LR %>% 
    filter(Subject == thisSubj)

  
  # create plot
  plot <- minidf %>%
    ggplot(aes(
      x = phase,
      y = accuracy,
      color = inversion,
      group = inversion
    )) +
    stat_summary(fun.data = "mean_cl_normal", size = 0.75) +
    geom_line(
      data = GLMMpreds.thisSubj,
      aes(
        x = phase,
        y = predict,
        color = inversion,
        group = inversion
      ),
      inherit.aes = FALSE,
      size = 1.5,
      linetype = "solid",
      show.legend = FALSE
    ) +
    # geom_hline (yintercept = .50,
    #             color = 'black',
    #             size = 1) +
    facet_grid(~ meridian) +
    scale_color_manual (values = colorsRightLOW) +
    scale_fill_manual (values = colorsRightLOW) +
    labs(x = "phase coherence", y = "accuracy") +
    alxtheme +
    theme (text = element_text(size = 20),
           axis.text = element_text(size = 15)) +
    scale_y_continuous(breaks = c(0, 0.33, 0.67, 1), limits = c(0, 1.1)) +
    scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15)) 
  
  # open jpeg
  jpgname <- paste("GLMM_logistic/wihtin_probit_plots/fit", thisSubj, ".jpg", sep = "")
  
  jpeg(jpgname, width = 700, height = 400)
  
  # print plot
  print(plot)
  
  # close jpeg file
  dev.off()
  
  
}




# 7 -- Observers slope estimates --------------------------------------------------------------------------------------

# up-left -------------------------------------------------------------------------------------------------------------

#upleft (when ok, duplicate for lowright)
UPLEFT_pars_ranef <- coef(UPLEFT_radialInter)$Subject %>%
  rownames_to_column(var = "Subject") %>%
  mutate(group = "UPLEFT",
         horiz_upright_intercept  = `(Intercept)` + `inversionupright` + `meridianHorizontalMeridian` + `inversionupright:meridianHorizontalMeridian`,
         horiz_inverted_intercept = `(Intercept)` + `inversioninverted` + `meridianHorizontalMeridian` + `inversioninverted:meridianHorizontalMeridian`,
         vert_upright_intercept  = `(Intercept)` + `inversionupright` + `meridianVerticalMeridian` + `inversionupright:meridianVerticalMeridian`,
         vert_inverted_intercept = `(Intercept)` + `inversioninverted` + `meridianVerticalMeridian` + `inversioninverted:meridianVerticalMeridian`,
         
         horiz_upright_slope      = `phase` +  `phase:inversionupright` + `phase:meridianHorizontalMeridian` + `phase:inversionupright:meridianHorizontalMeridian`,
         horiz_inverted_slope     = `phase` +  `phase:inversioninverted`+ `phase:meridianHorizontalMeridian` + `phase:inversioninverted:meridianHorizontalMeridian`,
         vert_upright_slope      = `phase` +  `phase:inversionupright` + `phase:meridianVerticalMeridian` + `phase:inversionupright:meridianVerticalMeridian`,
         vert_inverted_slope     = `phase` +  `phase:inversioninverted`+ `phase:meridianVerticalMeridian` + `phase:inversioninverted:meridianVerticalMeridian`
         
  ) %>% 
  select(-seq(2,19,1)) %>%
  mutate(
    wslope.horiz = horiz_upright_slope - horiz_inverted_slope,
    wslope.vert  = vert_upright_slope - vert_inverted_slope,
    is.radial = ifelse(abs(wslope.horiz) > abs(wslope.vert), 1, 0),
    radialeffectsize = abs(wslope.horiz) - abs(wslope.vert))




# low-right -----------------------------------------------------------------------------------------------------------

LOWRIGHT_pars_ranef <- coef(LOWRIGHT_radialInter)$Subject %>%
  rownames_to_column(var = "Subject") %>%
  mutate(group = "LOWRIGHT",
         horiz_upright_intercept  = `(Intercept)` + `inversionupright` + `meridianHorizontalMeridian` + `inversionupright:meridianHorizontalMeridian`,
         horiz_inverted_intercept = `(Intercept)` + `inversioninverted` + `meridianHorizontalMeridian` + `inversioninverted:meridianHorizontalMeridian`,
         vert_upright_intercept  = `(Intercept)` + `inversionupright` + `meridianVerticalMeridian` + `inversionupright:meridianVerticalMeridian`,
         vert_inverted_intercept = `(Intercept)` + `inversioninverted` + `meridianVerticalMeridian` + `inversioninverted:meridianVerticalMeridian`,
         
         horiz_upright_slope      = `phase` +  `phase:inversionupright` + `phase:meridianHorizontalMeridian` + `phase:inversionupright:meridianHorizontalMeridian`,
         horiz_inverted_slope     = `phase` +  `phase:inversioninverted`+ `phase:meridianHorizontalMeridian` + `phase:inversioninverted:meridianHorizontalMeridian`,
         vert_upright_slope      = `phase` +  `phase:inversionupright` + `phase:meridianVerticalMeridian` + `phase:inversionupright:meridianVerticalMeridian`,
         vert_inverted_slope     = `phase` +  `phase:inversioninverted`+ `phase:meridianVerticalMeridian` + `phase:inversioninverted:meridianVerticalMeridian`
         
  ) %>% 
  select(-seq(2,19,1)) %>%
  mutate(
    wslope.horiz = horiz_upright_slope - horiz_inverted_slope,
    wslope.vert  = vert_upright_slope - vert_inverted_slope,
    is.radial = ifelse(abs(wslope.horiz) > abs(wslope.vert), 1, 0),
    radialeffectsize = abs(wslope.horiz) - abs(wslope.vert))


