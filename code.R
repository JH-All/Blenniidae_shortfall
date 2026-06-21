# Dates ---------------------
# rFishTaxa search date = March 11th 2026, 413 valid species

# Downloads ---------------------------------
if (!requireNamespace(c("devtools","tibble"), quietly = TRUE))
  install.packages("devtools","tibble")

devtools::install_github("Otoliths/rFishTaxa", build_vignettes = FALSE)

options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("xml2")  
packageVersion("xml2")  # should be >= "1.4.0"

# Packages ----------------------------------
options(warn = -1)

load_or_install <- function(pkgs){
  for(p in pkgs){
    if(!requireNamespace(p, quietly = TRUE)){
      install.packages(p, dependencies = TRUE)
    }
    library(p, character.only = TRUE)
  }
}

pkgs <- c(
  "readxl",
  "rFishTaxa",
  "magrittr",
  "dplyr",
  "stringr",
  "writexl",
  "rfishbase",
  "tidyverse",
  "rgbif",
  "httr",
  "CoordinateCleaner",
  "sp",
  "sf",
  "dggridR",
  "rnaturalearth",
  "rnaturalearthdata",
  "ggspatial",
  "cowplot",
  "performance",
  "MASS",
  "DHARMa",
  "ggeffects",
  "mgcv",
  "car",
  "classInt",
  "nlme",
  "AICcmodavg",
  "patchwork",
  "performance",
  "glmmTMB",
  "openxlsx",
  "RColorBrewer",
  "bbmle"
)

load_or_install(pkgs)

# Getting data ---------------------------
species <- search_cas(query = "blenniidae",type = "species_family")

# Data organization rFishTaxa  ------------------------
species_new = species[species$status == "Validation",]

species_new <- species_new %>%
  mutate(year = str_extract(author, "\\d{4}")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(genus = word(species, 1))%>%
  mutate(num_authors = author %>%
           str_remove("\\d{4}") %>%               
           str_replace_all("[\\(\\)]", "") %>%   
           str_replace_all("\\s*&\\s*", ",") %>% 
           str_replace_all("\\s+et\\s+al\\.?\\s*", "") %>% 
           str_squish() %>%                   
           str_split(",| e ") %>%              
           sapply(function(x) sum(nzchar(trimws(x)))) 
  )


sum(is.na(species_new$year))
which(is.na(species_new$year))
species_new[is.na(species_new$year), ]

species_new <- species_new %>%
  dplyr::select(3, 4, 7, 8, 9)

species_new <- species_new %>%
  mutate(year = ifelse(species == "Hypsoblennius invemar", 1980, year))

# Organizing valid species and synonyms  --------------
species_all <- species %>%
  mutate(year = str_extract(author, "\\d{4}")) %>% 
  mutate(genus = word(species, 1))%>%
  mutate(num_authors = author %>%
           str_remove("\\d{4}") %>%               
           str_replace_all("[\\(\\)]", "") %>%   
           str_replace_all("\\s*&\\s*", ",") %>% 
           str_replace_all("\\s+et\\s+al\\.?\\s*", "") %>% 
           str_squish() %>%                   
           str_split(",| e ") %>%              
           sapply(function(x) sum(nzchar(trimws(x)))) 
  )


species_all$status = as.factor(species_all$status)
levels(species_all$status)
str(species_all)

species_plot <- species_all %>%
  filter(status %in% c("Validation", "Synonym")) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year, status) %>%
  summarise(n_species = n(), .groups = "drop") %>%
  mutate(status = dplyr::recode(status,
                                "Validation" = "Valid species",
                                "Synonym" = "Synonyms"))

# Figure 1 -----------------------------
## Figure 1A --------------
species_cum <- species_all %>%
  filter(status %in% c("Validation", "Synonym")) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year, status) %>%
  summarise(n_species = n(), .groups = "drop") %>%
  arrange(status, year) %>%                 
  group_by(status) %>%
  mutate(cumulative = cumsum(n_species))     

species_cum <- species_cum %>%
  mutate(status = dplyr::recode(status,
                         "Validation" = "Valid species",
                         "Synonym"   = "Synonyms"))


fig1_A <- ggplot(
  species_cum,
  aes(x = year, y = cumulative, color = status)
) +
  geom_line(linewidth = 1.9) +
  scale_color_manual(
    values = c(
      "Valid species" = "#2E86AB",
      "Synonyms" = "#E07A5F"
    )
  ) +
  labs(
    x = NULL,
    y = "Cumulative number \nof species",
    color = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    limits = c(1754, 2024),
    breaks = seq(1754, 2024, by = 30)
  )
fig1_A

## Figure 1B ----------------------------
authors_mean <- species_new %>%
  mutate(year = as.numeric(year)) %>%        
  group_by(year) %>%
  summarise(mean_authors = mean(num_authors, na.rm = TRUE),
            .groups = "drop")              

head(authors_mean)

fig1_B = ggplot(authors_mean, aes(x = year, y = mean_authors)) +
  geom_point(size = 5 , alpha = 0.6, shape = 21, fill = "gray", color = "black") +
  geom_line(linewidth = 0.5, color = "gray", linetype = "dashed") +
  scale_x_continuous(limits = c(1754, 2024), breaks = seq(1754, 2024, by = 30))+
  labs(
    x = NULL,
    y = "Average number of \nauthors per species"
  ) +
  theme_classic(base_size = 18) 

fig1_B

## Complete figure ----------------------------
fig1 <- (fig1_A | fig1_B) +
  plot_annotation(tag_levels = "A")

fig1

ggsave("Figure_1.jpg", fig1, width = 14, height = 6)

# Total species N estimates -----------------------------------
## Preparing ----------------------------------------
species <- read.xlsx ("valid_species.xlsx")

data <- species %>%
  filter(!is.na(species)) %>%
  mutate (Family = "Blenniidae") %>%
  mutate(
    author = author %>%
      str_remove_all("\\(|\\)") %>%     
      str_remove("\\s*\\d{4}$") %>%    
      str_replace_all(",", " &") %>%     
      str_squish()                       
  ) %>%
  rename(Species = species, Genus = genus, taxonomist = author, Year = year)  %>%
  select (Family, Genus, Species, taxonomist, Year)


data <- data %>%
  mutate(
    taxonomist = str_replace_all(
      taxonomist,
      c(
        "Asso y del Rio" = "Asso_y_del_Rio",
        "De Vis" = "De_Vis",
        "Miranda Ribeiro" = "Miranda_Ribeiro",
        "Smith-Vaniz & Acero P" = "Smith-Vaniz & Acero_P",
        "von Bonde" = "von_Bonde"
      )
    )
  )


n_distinct(data$Species)

initial.data <- data 

family.position <-1
genus.position <-2
species.position <-3
taxonomist.position <-4
year.position <-5

####AUTHOR SPLITTING FUNCTION
taxonomic.splitting.function<-function(dataset,taxonomist.column){
  start.data<-as.matrix(dataset)
  split.in<-strsplit(start.data[,taxonomist.column],split=c(" & "))
  mx.tx<-max(unlist(lapply(split.in,function(x){x<-length(x)}))) ###the maximum number of authors describing a single species###
  
  #########THIS SPLITS THE AUTHORS INTO INDIVIDUAL COLUMNS
  na.matrix<-matrix(data="NA",ncol=mx.tx,nrow=nrow(start.data))
  start.data1<-cbind(start.data,na.matrix) ###there are never more than mx.tx authors per species...
  colnames(start.data1)<-c(colnames(start.data),paste("Taxonomist_",seq(1,mx.tx,1),sep=""))
  
  for(j in 1:mx.tx){
    start.data1[,(j+ncol(start.data))]<-unlist(lapply(split.in,function(x){x<-noquote(x[j])}))
  }
  
  start.data1<-as.data.frame(start.data1)
  return(start.data1)
}

cleaned.tax.data <- taxonomic.splitting.function(dataset=initial.data,taxonomist.column = taxonomist.position)
write.table(cleaned.tax.data,file=paste("","blenniidae_taxonimist.txt",sep=""),col.names=TRUE,row.names=FALSE,sep="\t")

input.data <- read.table("blenniidae_taxonimist.txt", h = T) 
colnames(input.data)

input.data <- input.data %>%
  mutate(Year = as.numeric(Year))

## Model specifications -----------------------------------
start.year <- 1758
end.year <- 2026
year.interval <- 2

# 2 years (2026), 4 years (2026), 6 years (2028), 8 years (2030), 10 years (2030)

########YEARLY SUMMARY FUNCTION
yearly.summary.function<-function(dataset,year.column,genus.column,species.column,start.year,end.year,year.interval){
  yrs<-seq(start.year,end.year,year.interval)	
  mat<-matrix(data=0,ncol=5,nrow=length(yrs))
  colnames(mat)<-c("Start_Year","Species","Taxonomists","SpeciesPerTaxonomist","CumulativeSpecies")
  mat[,1]<-yrs
  for (q in 1:length(mat[,1])){
    spec.data<-dataset
    sam<-spec.data[which(spec.data[,year.column] >= mat[q,1] & spec.data[,year.column] < mat[q,1] + year.interval),]
    tx.pos<-grep("Taxonomist_",colnames(spec.data))
    n.tx<-length(grep("Taxonomist_",colnames(spec.data)))
    
    for (j in 1:n.tx){
      assign(paste("sam.tax",j,sep=""),as.matrix(sam[,tx.pos[j]]))
    }
    
    tax.dat<-ls()[grep("sam.tax",ls())]
    out.list<-c()
    for(k in 1:length(tax.dat)){
      out.list<-c(out.list,get(tax.dat[k]))
    }
    sam.alltax<-unique(out.list)
    sam.un.tax<-sam.alltax[!is.na(sam.alltax)]
    
    mat[q,2]<-length(unique(paste(sam[,genus.column],sam[,species.column],sep=" "))) 
    mat[q,3]<-length(sam.un.tax)
  }
  mat[,4]<-mat[,2]/mat[,3]
  mat[1,5]<-0	
  for (k in 2:length(mat[,1])){
    mat[k,5]<-mat[k-1,5]+mat[k-1,2]
  }
  return(mat)
}


year.summary.mat <- yearly.summary.function(dataset = input.data, genus.column = genus.position, species.column = species.position, year.column = year.position, start.year = start.year, end.year = end.year, year.interval = year.interval)

sp.dis <- as.data.frame(year.summary.mat)

sp.dis <- sp.dis %>%
  rename(time = Start_Year, sp.per = Species, tax.per = Taxonomists, sp.cum = CumulativeSpecies
  ) %>%
  select(time, sp.per, sp.cum, tax.per) %>% 
  mutate(sp.cum = lead(sp.cum)) %>%                     
  filter(!is.na(sp.cum)) %>%
  filter(sp.per != 0)   


write.table(sp.dis, "blenniidae_2_years.txt")

#  Script based on Lu & He 2017,  Appendix S3 codes for fitting the species discovery model

source("run_models_modificado.R")
source("functions.R")

sp.dis <- read.table("blenniidae_2_years.txt") 
resu <- run_models(sp.dis)

## Model selection --------------------------------------
model_names <- c("Lu & He", "Joppa et al.", "Logistic", "Negative exponential")
names(resu) <- model_names
sel <- model_selection(resu[1:4])
description_percentage <- sp.dis[nrow(sp.dis), "sp.cum"] / sel$Average_S_tot

#Model adequacy plots
predictions_final <- prediction(resu, sel$AIC)
predictions_final<-predictions_final[,- 5]

# est - 2 anos: 875 espécies (546 a 1203) Lu & He
# est - 4 anos: 902 espécies (422 a 1384) Lu & He
# est - 6 anos: sem convergência
# est - 8 anos: sem convergência
# est - 10 anos: sem convergência

## Table 1 -------------------------------------------------

model_names <- c("Lu & He (2017)",
                 "Joppa et al. (2011)",
                 "Logistic",
                 "Negative exponential")

names(resu) <- model_names

get_AICc <- function(model, n) {
  k <- attr(logLik(model), "df")
  AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
}

get_Stot <- function(model) {
  coef(model)["S.tot"]
}

get_CI <- function(model) {
  ci <- tryCatch({
    intervals(model)$coef["S.tot", c("lower", "upper")]
  }, error = function(e) c(lower = NA, upper = NA))
  
  ci
}

n_eff <- nrow(sp.dis)

table1 <- purrr::imap_dfr(resu, function(model, name) {
  
  ci <- get_CI(model)
  
  tibble::tibble(
    Model = name,
    AICc = get_AICc(model, n_eff),
    Stotal = get_Stot(model),
    CI_low = ci["lower"],
    CI_high = ci["upper"]
  )
}) %>%
  arrange(AICc) %>%
  mutate(
    Delta_AICc = AICc - min(AICc),
    wAICc = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc)),
    AICc = round(AICc, 2),
    Stotal = round(Stotal, 0),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    Delta_AICc = round(Delta_AICc, 3),
    wAICc = round(wAICc, 3),
    `Stotal (CI 95%: Lower - Upper)` =
      paste0(Stotal, " (", CI_low, " - ", CI_high, ")")
  ) %>%
  select(
    Model,
    AICc,
    `Stotal (CI 95%: Lower - Upper)`,
    Delta_AICc,
    wAICc
  )

table1

## Figure 2 -------------------------------
pred_df <- as.data.frame(predictions_final)

colnames(pred_df) <- c(
  "Lu & He",
  "Joppa et al.",
  "Logistic",
  "Negative exponential"
)

plot_df <- bind_cols(
  sp.dis %>% select(time, sp.cum),
  pred_df %>% mutate(across(everything(), cumsum))
) %>%
  pivot_longer(
    cols = -time,
    names_to = "Series",
    values_to = "Accumulated"
  ) %>%
  mutate(
    Series = recode(Series, sp.cum = "Observed values"),
    Series = factor(
      Series,
      levels = c(
        "Observed values",
        "Lu & He",
        "Joppa et al.",
        "Logistic",
        "Negative exponential"
      )
    )
  )

cols <- c(
  "Observed values" = "darkgrey",
  "Lu & He" = brewer.pal(8, "Set2")[1],
  "Joppa et al." = brewer.pal(8, "Set2")[2],
  "Logistic" = brewer.pal(8, "Set2")[3],
  "Negative exponential" = "purple"
)

fig2 <- ggplot(
  plot_df,
  aes(time, Accumulated,
      color = Series,
      linetype = Series)
) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = rep("solid", length(cols))) +
  scale_x_continuous(
    limits = c(1758, 2026),
    breaks = seq(1760, 2020, 20)
  ) +
  scale_y_continuous(limits = c(0, 600)) +
  theme_classic(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Description dates",
    y = "Cumulative number of\nspecies descriptions"
  )

fig2

ggsave(
  "Figure_2.png",
  fig2,
  width = 17,
  height = 12,
  units = "cm",
  dpi = 300
)

## Table S1 - 4-year interval -----------------------------------------
year.interval <- 4
end_i <- 2026

year.summary.mat <- yearly.summary.function(
    dataset = input.data,
    genus.column = genus.position,
    species.column = species.position,
    year.column = year.position,
    start.year = start.year,
    end.year = end_i,
    year.interval = year.interval
)

sp.dis.4 <- as.data.frame(year.summary.mat) %>%
  rename(
    time = Start_Year,
    sp.per = Species,
    tax.per = Taxonomists,
    sp.cum = CumulativeSpecies
  ) %>%
  select(time, sp.per, sp.cum, tax.per) %>%
  mutate(sp.cum = lead(sp.cum)) %>%
  filter(!is.na(sp.cum)) %>%
  filter(sp.per != 0)


resu.4 <- run_models(sp.dis.4)

model_names <- c(
  "Lu & He (2017)",
  "Joppa et al. (2011)",
  "Logistic",
  "Negative exponential"
)

resu.4 <- resu.4[1:4]
names(resu.4) <- model_names

## Helper functions
get_AICc <- function(model, n) {
  k <- attr(logLik(model), "df")
  AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
}

get_Stot <- function(model) {
  coef(model)["S.tot"]
}

get_CI <- function(model) {
  tryCatch({
    intervals(model)$coef["S.tot", c("lower", "upper")]
  }, error = function(e) c(lower = NA, upper = NA))
}

n_eff.4 <- nrow(sp.dis.4)

## Build Table S1
table_s1 <- purrr::imap_dfr(resu.4, function(model, name) {
  
  ci <- get_CI(model)
  
  tibble(
    Model = name,
    AICc = get_AICc(model, n_eff.4),
    Stotal = as.numeric(get_Stot(model)),
    CI_low = as.numeric(ci["lower"]),
    CI_high = as.numeric(ci["upper"])
  )
}) %>%
  arrange(AICc) %>%
  mutate(
    Delta_AICc = AICc - min(AICc),
    wAICc = exp(-0.5 * Delta_AICc) /
      sum(exp(-0.5 * Delta_AICc)),
    
    AICc = round(AICc, 2),
    Stotal = round(Stotal, 0),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    Delta_AICc = round(Delta_AICc, 3),
    wAICc = round(wAICc, 3),
    
    `Stotal (CI 95%: Lower - Upper)` =
      paste0(Stotal, " (", CI_low, " - ", CI_high, ")")
  ) %>%
  select(
    Model,
    AICc,
    `Stotal (CI 95%: Lower - Upper)`,
    Delta_AICc,
    wAICc
  )

table_s1

# Year ~ Body size --------------------------------
data = read_excel("valid_species.xlsx")
str(data)
data$year = as.numeric(data$year)
summary(data$year)
data$SL_male_cm = as.numeric(data$SL_male_cm)

sum(is.na(data$year))
sum(is.na(data$SL_male_cm))
data$log_SL <- log10(data$SL_male_cm)
data$genus <- as.factor(data$genus)

## GLMM -----------------------
mod_glmm <- glmmTMB(
  year ~ log_SL + (1 | genus),
  data = data,
  family = nbinom2(link = "log")
)

summary(mod_glmm)
r2(mod_glmm)

## Figure 3A ------------------
fig3_A = data %>% 
  ggplot(aes(x =log_SL, y = year ))+
  geom_point(fill = "#2E86AB", shape = 21, size = 4,
             alpha = 0.7)+
  geom_smooth(method = "glm", se = T,
              linetype = "dashed",
              color = "black", linewidth = 1.4)+
  scale_y_continuous(limits = c(1754, 2024), breaks = seq(1754, 2024, by = 30))+
  theme_classic(base_size = 18)+
  labs(x = expression(log[10]*" male standard length"),
       y = "Year of description")

fig3_A

# Year ~ Área de ocorrência ----------------------------
## Getting species occurrence area ready -------------------------
blen_all <- readRDS("blen_all.rds")

class(blen_all)
str(blen_all)

sp_data <- unique(data$species)

blen_union <- blen_all %>%
  filter(sci_name %in% sp_data) %>%
  st_drop_geometry() %>%
  group_by(sci_name) %>%
  summarise(.groups = "drop")


n_match <- nrow(blen_union)
n_match

length(unique(data$species))

sf::sf_use_s2(FALSE)

blen_sf_union <- blen_all %>%
  filter(sci_name %in% sp_data) %>%
  st_make_valid() %>%
  group_by(sci_name) %>%
  summarise(.groups = "drop")

sf::sf_use_s2(TRUE)

blen_proj <- st_transform(blen_sf_union, 6933)

blen_simple <- st_simplify(blen_proj, dTolerance = 50000)

blen_simple <- blen_simple %>%
  st_make_valid() %>%
  st_transform(4326)

nrow(blen_simple)            
head(blen_simple$sci_name) 
unique(blen_simple$sci_name)

blen_area_df <- blen_simple %>%
  st_transform(6933) %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  dplyr::select(sci_name, area_km2)

data <- data %>%
  left_join(blen_area_df, by = c("species" = "sci_name"))

## GLMM -----------------------
data$log_area <- log10(data$area_km2)

data_model <- data[is.finite(data$log_area), ]

mod_area_glmm <- glmmTMB(
  year ~ log_area + (1 | genus),
  data = data_model,
  family = nbinom2(link = "log")
)

summary(mod_area_glmm)
r2(mod_area_glmm)

## Figure 3B ----------------------------
fig3_B = data_model %>% 
  ggplot(aes(x = log_area, y = year)) +
  geom_point(fill = "#2E86AB", shape = 21, size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
              color = "black", linewidth = 1.4) +
  scale_y_continuous(limits = c(1754, 2024),
                     breaks = seq(1754, 2024, by = 30)) +
  theme_classic(base_size = 18) +
  labs(x = expression(log[10]*" range size (km²)"),
       y = "Year of description")

fig3_B

## Figure 3 Complete ----------------------
fig3 = (fig3_A +  fig3_B) +
  plot_annotation(tag_levels = "A")

fig3

ggsave("Figure_3.jpg", fig3, width = 11, height = 5)

# Year ~ IUCN -------------------------
## Kruskal-Wallis ---------------------------
data$iucn = as.factor(data$iucn)
levels(data$iucn)

data <- data %>%
  mutate(
    iucn = dplyr::recode(iucn,
                         "Lc" = "LC",
                         "EM" = "EN")
  )

data$iucn <- factor(
  data$iucn,
  levels = c("NE", "DD", "LC", "NT", "VU", "EN")
)

data2 <- data %>% 
  filter(!iucn %in% c("NE","DD", "NT"))

kruskal.test(year ~ iucn, data = data2)

pairwise.wilcox.test(
  data2$year,
  data2$iucn,
  p.adjust.method = "BH"
)
## Figure 4 ------------------------------------
data_iucn <- data[!is.na(data$iucn), ]

fig4 = data_iucn %>% 
  ggplot(aes(x = iucn, y = year, fill = iucn)) +
  
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  
  geom_jitter(
    width = 0.12, size = 2, shape = 21,
    alpha = 0.5, show.legend = FALSE) +
  
  scale_fill_manual(values = c(
    "NE" = "darkgray",
    "DD" = "lightgray",
    "LC" = "#a1d99b",   # light green
    "NT" = "#006d2c",   # dark green
    "VU" = "#FFD92F",   # yellow
    "EN" = "#E6550D"    # orange
  )) +
  
  scale_y_continuous(
    limits = c(1754, 2024),
    breaks = seq(1754, 2024, by = 30)
  ) +
  
  theme_classic(base_size = 18) +
  
  labs(
    x = "IUCN categories",
    y = "Year of description"
  )

fig4

ggsave("Figure_4.jpg", fig4)

# Species richness  ---------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

sf_use_s2(FALSE) 
target_crs <- 4326
work_crs   <- 6933  

lam_wgs_valid <- st_make_valid(st_transform(blen_simple, target_crs))

bbox_blen <- st_bbox(blen_simple)

grid_highres <- st_make_grid(
  x = st_as_sfc(bbox_blen),
  cellsize = c(0.5, 0.5),   
  what = "polygons",
  square = TRUE
) |>
  st_sf() |>
  st_set_crs(target_crs)

grid_highres$cell_id <- seq_len(nrow(grid_highres))

hits <- st_intersects(grid_highres, blen_simple)
keep <- lengths(hits) > 0

grid_keep <- grid_highres[keep, ]
grid_keep$richness <- lengths(hits[keep])

class(grid_keep$richness)
summary(grid_keep$richness) 
grid_keep$log_S = log10(grid_keep$richness)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(target_crs)

# Figure 5 ---------------------------------
blen_year <- blen_simple %>%
  left_join(
    data %>% st_drop_geometry() %>% dplyr::select(species, year),
    by = c("sci_name" = "species")
  )

hits_year <- st_intersects(grid_highres, blen_year)

keep_year <- lengths(hits_year) > 0
grid_year <- grid_highres[keep_year, ]

grid_year$mean_year <- sapply(hits_year[keep_year], function(i) {
  mean(blen_year$year[i], na.rm = TRUE)
})

fig5 = ggplot() +
  geom_sf(data = world,
          fill = "gray90",
          color = "white",
          linewidth = 0.2) +
  geom_sf(data = grid_year,
          aes(fill = mean_year),
          color = NA,
          alpha = 0.85) +
  scale_fill_viridis_c(
    option = "C",
    name   = "Description \nyear",
    breaks = scales::pretty_breaks(5),
    limits = range(grid_year$mean_year, na.rm = TRUE)
  ) +
  coord_sf(
    xlim = c(bbox_blen["xmin"], bbox_blen["xmax"]),
    ylim = c(bbox_blen["ymin"], bbox_blen["ymax"]),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

fig5

ggsave("Figure_5.jpg",fig5)

# Figure 6 --------------------------------

grid_bivar <- grid_year %>%
  left_join(
    grid_keep %>% st_drop_geometry() %>% dplyr::select(cell_id, richness),
    by = "cell_id"
  ) %>%
  mutate(
    year_cat = cut(mean_year,
                   breaks = classIntervals(mean_year, n = 4, style = "quantile")$brks,
                   include.lowest = TRUE, labels = 1:4),
    rich_cat = cut(richness,
                   breaks = classIntervals(richness, n = 4, style = "quantile")$brks,
                   include.lowest = TRUE, labels = 1:4),
    bi_class = paste(year_cat, rich_cat, sep = "-")
  )

bi_pal <- c(
  "1-1"="#e8e8e8","2-1"="#b5e3e3","3-1"="#7bccc4","4-1"="#43a2ca",
  "1-2"="#dfb0d6","2-2"="#b8b8d8","3-2"="#8c96c6","4-2"="#6a51a3",
  "1-3"="#c994c7","2-3"="#9ebcda","3-3"="#8c6bb1","4-3"="#88419d",
  "1-4"="#ae017e","2-4"="#7a0177","3-4"="#54278f","4-4"="#2c115f"
)


p_map <- ggplot() +
  geom_sf(data = world,
          fill = "gray90",
          color = "white",
          linewidth = 0.2) +
  geom_sf(data = grid_bivar,
          aes(fill = bi_class),
          color = NA,
          alpha = 0.9) +
  scale_fill_manual(values = bi_pal, drop = FALSE) +
  coord_sf(
    xlim = c(bbox_blen["xmin"], bbox_blen["xmax"]),
    ylim = c(bbox_blen["ymin"], bbox_blen["ymax"]),
    expand = FALSE
  ) +
  theme_minimal(base_size = 22) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )


legend_df <- expand.grid(
  year_cat = factor(1:4, levels = 1:4),
  rich_cat = factor(1:4, levels = 1:4)
) %>%
  mutate(bi_class = paste(year_cat, rich_cat, sep = "-"))

p_leg <- ggplot(legend_df, aes(x = year_cat, y = rich_cat, fill = bi_class)) +
  geom_tile() +
  scale_fill_manual(values = bi_pal, drop = FALSE) +
  labs(
    x = "Year\nOlder  →  Recent",
    y = "Richness\nLow  →  High"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_text(size = 9),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

fig_6 = ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +
  draw_plot(p_leg, 0.78, 0.05, 0.16, 0.16)

fig_6

ggsave("Figure_6.jpg", fig_6, width = 15, height = 10)

sf_use_s2(TRUE)