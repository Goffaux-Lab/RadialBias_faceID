###################################################################################
###################################################################################
# Roux-Sibilon, Peyrin, Greenwood, and Goffaux
# Radial bias in face identification
# Complementary analysis (model comparison using log likelihood ratio)
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

## Now load or install&load all
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

# 2 -- Fit hierarchical GLMM models ----------------------------------------------------------------------------------------

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
    
    # fit reduced model
    fitreduced = glmer(
      accuracy ~ 1 + phase + (1 + phase*inversion || triplet/Subject),
      family = binomial(link = "probit"),
      data = df %>% filter(subjGroup == thisGroup, meridian == thisMeridian),
      control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=100000))
    )
    zegroup = ifelse(thisGroup == "low-right", "LOWRIGHT", "UPLEFT")
    filename = paste("GLMM_logistic/models/", zegroup, "_inversion_", thisMeridian, "_r.rds", sep = "")
    saveRDS(fitreduced, file = filename)
    
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

# .... for each groups
for (thisGroup in twogroups) {
  
  # fit reduced model
  fitreduced = glmer(
    accuracy ~ phase*inversion + (1 +phase*inversion*meridian || triplet/Subject),
    family = binomial(link = "probit"),
    data = df %>% filter(subjGroup == thisGroup, meridian != "Fovea"),
    control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000))
  )
  zegroup = ifelse(thisGroup == "low-right", "LOWRIGHT", "UPLEFT")
  filename = paste("GLMM_logistic/models/", zegroup, "_radialInter", "_r.rds", sep = "")
  saveRDS(fitreduced, file = filename)
  
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



# 3 -- Test effects ---------------------------------------------------------------------------------------------------


# up-left ----------------------------------------------------------------------------------------------------------


# Is there an inversion effect at each VF location?

#fovea
UPLEFT_inversion_Fovea_r <- readRDS("GLMM_logistic/models/UPLEFT_inversion_Fovea_r.rds")
UPLEFT_inversion_Fovea <- readRDS("GLMM_logistic/models/UPLEFT_inversion_Fovea.rds")
summary(UPLEFT_inversion_Fovea_r)
summary(UPLEFT_inversion_Fovea)

anova(UPLEFT_inversion_Fovea_r, UPLEFT_inversion_Fovea)

#leftHM
UPLEFT_inversion_HorizontalMeridian_r <- readRDS("GLMM_logistic/models/UPLEFT_inversion_HorizontalMeridian_r.rds")
UPLEFT_inversion_HorizontalMeridian <- readRDS("GLMM_logistic/models/UPLEFT_inversion_HorizontalMeridian.rds")
summary(UPLEFT_inversion_HorizontalMeridian_r)
summary(UPLEFT_inversion_HorizontalMeridian)

anova(UPLEFT_inversion_HorizontalMeridian_r, UPLEFT_inversion_HorizontalMeridian)

#upperVM
UPLEFT_inversion_VerticalMeridian_r <- readRDS("GLMM_logistic/models/UPLEFT_inversion_VerticalMeridian_r.rds")
UPLEFT_inversion_VerticalMeridian <- readRDS("GLMM_logistic/models/UPLEFT_inversion_VerticalMeridian.rds")
summary(UPLEFT_inversion_VerticalMeridian_r)
summary(UPLEFT_inversion_VerticalMeridian)

anova(UPLEFT_inversion_VerticalMeridian_r, UPLEFT_inversion_VerticalMeridian)



# Is the IE larger on the horizontal than vertical meridian?
UPLEFT_radialInter_r <- readRDS("GLMM_logistic/models/UPLEFT_radialInter_r.rds")
UPLEFT_radialInter <- readRDS("GLMM_logistic/models/UPLEFT_radialInter.rds")
summary(UPLEFT_radialInter_r)
summary(UPLEFT_radialInter)

anova(UPLEFT_radialInter_r, UPLEFT_radialInter)



# low-right -------------------------------------------------------------------------------------------------------

# Is there an inversion effect at each VF location?

#fovea
LOWRIGHT_inversion_Fovea_r <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_Fovea_r.rds")
LOWRIGHT_inversion_Fovea <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_Fovea.rds")
summary(LOWRIGHT_inversion_Fovea_r)
summary(LOWRIGHT_inversion_Fovea)

anova(LOWRIGHT_inversion_Fovea_r, LOWRIGHT_inversion_Fovea)

#rightHM
LOWRIGHT_inversion_HorizontalMeridian_r <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_HorizontalMeridian_r.rds")
LOWRIGHT_inversion_HorizontalMeridian <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_HorizontalMeridian.rds")
summary(LOWRIGHT_inversion_HorizontalMeridian_r)
summary(LOWRIGHT_inversion_HorizontalMeridian)

anova(LOWRIGHT_inversion_HorizontalMeridian_r, LOWRIGHT_inversion_HorizontalMeridian)

#lowerVM
LOWRIGHT_inversion_VerticalMeridian_r <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_VerticalMeridian_r.rds")
LOWRIGHT_inversion_VerticalMeridian <- readRDS("GLMM_logistic/models/LOWRIGHT_inversion_VerticalMeridian.rds")
summary(LOWRIGHT_inversion_VerticalMeridian_r)
summary(LOWRIGHT_inversion_VerticalMeridian)

anova(LOWRIGHT_inversion_VerticalMeridian_r, LOWRIGHT_inversion_VerticalMeridian)



# Is the IE larger on the horizontal than vertical meridian?
LOWRIGHT_radialInter_r <- readRDS("GLMM_logistic/models/LOWRIGHT_radialInter_r.rds")
LOWRIGHT_radialInter <- readRDS("GLMM_logistic/models/LOWRIGHT_radialInter.rds")
summary(LOWRIGHT_radialInter_r)
summary(LOWRIGHT_radialInter)

anova(LOWRIGHT_radialInter_r, LOWRIGHT_radialInter)



















