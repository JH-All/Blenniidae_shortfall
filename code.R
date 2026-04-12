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
  "glmmTMB"
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
## Figure 1A ----------------------
fig1_A = ggplot(species_plot, aes(x = year, y = n_species, fill = status)) +
  geom_point(shape = 21, size = 5 , alpha = 0.6, show.legend = F)+
  scale_fill_manual(values = c("Valid species" = "#2E86AB", 
                               "Synonyms" = "#E07A5F"))+
  facet_wrap(~status, nrow = 2)+
  labs(
    x = NULL,
    y = "Number of species described"
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(limits = c(1754, 2024), breaks = seq(1754, 2024, by = 30))+
  scale_y_continuous(limits = c(0,100))

fig1_A


## Figure 1B --------------
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


fig1_B = ggplot(species_cum, aes(x = year, y = cumulative, fill = status)) +
  geom_point(shape = 21, size = 5 , alpha = 0.6, color = "black",
             show.legend = F) +
  scale_fill_manual(values = c("Valid species" = "#2E86AB",
                               "Synonyms" = "#E07A5F")) +
  labs(
    x = NULL,
    y = "Cumulative number \nof species"
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(limits = c(1754, 2024), breaks = seq(1754, 2024, by = 30))

fig1_B 


## Figure 1C ----------------------------
authors_mean <- species_new %>%
  mutate(year = as.numeric(year)) %>%        
  group_by(year) %>%
  summarise(mean_authors = mean(num_authors, na.rm = TRUE),
            .groups = "drop")              

head(authors_mean)

fig1_C = ggplot(authors_mean, aes(x = year, y = mean_authors)) +
  geom_point(size = 5 , alpha = 0.6, shape = 21, fill = "#2E86AB", color = "black") +
  geom_line(linewidth = 0.3, color = "#2E86AB", linetype = "dashed") +
  scale_x_continuous(limits = c(1754, 2024), breaks = seq(1754, 2024, by = 30))+
  labs(
    x = NULL,
    y = "Average number of \nauthors per species"
  ) +
  theme_classic(base_size = 18) 

fig1_C

## Complete figure ----------------------------
fig1 <- (fig1_A | (fig1_B / fig1_C)) +
  plot_annotation(tag_levels = "A")

fig1

ggsave("Figure_1.jpg", fig1, width = 14, height = 9)

# Total species N estimates -----------------------------------
data = read_excel("valid_species.xlsx")
str(data)
data$year = as.numeric(data$year)
summary(data$year)

intervalos = read_excel("authors_by_interval.xlsx")

parse_decade_start <- function(lbl_vec) {
  x <- gsub("\\[|\\)|\\]", "", lbl_vec)
  a <- sub(",.*$", "", x)
  as.integer(round(as.numeric(a)))
}
parse_decade_end <- function(lbl_vec) {
  x <- gsub("\\[|\\)|\\]", "", lbl_vec)
  b <- sub("^.*,", "", x)
  as.integer(round(as.numeric(b)))
}
make_label <- function(a, b, scientific_flag) {
  aa <- if (scientific_flag) format(a, scientific = TRUE) else as.character(a)
  bb <- if (scientific_flag) format(b, scientific = TRUE) else as.character(b)
  paste0("[", aa, ",", bb, ")")
}

dados <- data %>%
  rename(ano_descricao = year,
         num_autores   = num_authors) %>%
  filter(!is.na(ano_descricao)) %>%
  mutate(decade_start = floor(ano_descricao/10)*10)

use_sci <- grepl("e\\+03", intervalos$intervalo[1])

lab_1750 <- make_label(1750, 1760, scientific_flag = use_sci)

intervalos_ext <- intervalos %>%
  mutate(intervalo = as.character(intervalo)) %>%
  bind_rows(tibble(intervalo = lab_1750, n_authors_unique = 1)) %>%
  distinct(intervalo, .keep_all = TRUE) %>%
  mutate(
    decade_start  = parse_decade_start(intervalo),
    decade_end    = parse_decade_end(intervalo)
  ) %>%
  arrange(decade_start)

master_decades <- intervalos_ext$decade_start

intervalos_key <- intervalos_ext %>%
  mutate(intervalo_fac = factor(intervalo, levels = intervalos_ext$intervalo)) %>%
  dplyr::select(decade_start, decade_end, intervalo, intervalo_fac, n_authors_unique)

desc_intervalo <- dados %>%
  group_by(decade_start) %>%
  summarise(
    Ano_médio = mean(ano_descricao, na.rm = TRUE),
    Delta_St  = n(),
    .groups   = "drop"
  ) %>%
  complete(decade_start = master_decades,
           fill = list(Delta_St = 0, Ano_médio = NA_real_)) %>%
  right_join(intervalos_key, by = "decade_start") %>% 
  arrange(intervalo_fac) %>%
  mutate(
    Tt = ifelse(is.na(n_authors_unique), 0L, n_authors_unique)
  ) %>%
  dplyr::select(intervalo, decade_start, decade_end, Ano_médio, Delta_St, Tt)

desc_intervalo <- desc_intervalo %>%
  mutate(
    Ano_médio_fill = ifelse(is.na(Ano_médio), decade_start + 5, Ano_médio),
    St              = lag(cumsum(Delta_St), default = 0),
    Acumulado_Total = cumsum(Delta_St),
    Tempo           = Ano_médio_fill - min(Ano_médio_fill, na.rm = TRUE)
  )

nrow(desc_intervalo)  
desc_intervalo %>% filter(decade_start == 1750) %>% dplyr::select(intervalo, Tt, Delta_St)
desc_intervalo %>% filter(decade_start == 2020) %>% dplyr::select(intervalo, Tt, Delta_St)

desc_intervalo

Stot_init <- max(desc_intervalo$Acumulado_Total) * 2.5
a_init <- 0.01 
b_init <- 0.001  
r_init <- 0.1  

## Model 1: Joppa et al. (2011) ---------------------------
modelo_joppa <- tryCatch({
  gnls(
    Delta_St ~ Tt * (a + b * Tempo) * (Stot - St),
    data = desc_intervalo,
    start = list(Stot = Stot_init, a = a_init, b = b_init),
    control = gnlsControl(msVerbose = TRUE, maxIter = 500, nlsMaxIter = 100, tolerance = 1e-4)
  )
}, error = function(e) {
  message("Erro no modelo Joppa: ", e$message)
  return(NULL)
})

## Model 2: Lu & He (2017) --------------------------------------
modelo_luhe <- tryCatch({
  gnls(
    Delta_St ~ (Tt * a * (Stot - St)) / (1 - Tt * b * (Stot - St)),
    data = desc_intervalo,
    start = list(Stot = Stot_init, a = a_init, b = b_init/100),
    control = gnlsControl(msVerbose = TRUE, maxIter = 500, nlsMaxIter = 100, tolerance = 1e-4)
  )
}, error = function(e) {
  message("Erro no modelo Lu & He: ", e$message)
  return(NULL)
})

## Model 3: Logistic -------------------------
modelo_logistico <- tryCatch({
  nls(
    Acumulado_Total ~ Stot / (1 + exp(-r * (Tempo - t0))),
    data = desc_intervalo,
    start = list(Stot = Stot_init, r = r_init, t0 = mean(desc_intervalo$Tempo)),
    control = nls.control(maxiter = 1000, warnOnly = TRUE)
  )
}, error = function(e) {
  message("Erro no modelo Logístico: ", e$message)
  return(NULL)
})

## Model 4: Negative Exponential -----------------------
modelo_exp_neg <- tryCatch({
  nls(
    Acumulado_Total ~ Stot * (1 - exp(-r * Tempo)),
    data = desc_intervalo,
    start = list(Stot = Stot_init, r = r_init),
    control = nls.control(maxiter = 1000, warnOnly = TRUE)
  )
}, error = function(e) {
  message("Erro no modelo Exponencial Negativo: ", e$message)
  return(NULL)
})

## Model comparison ---------------------------------
get_k <- function(m) length(coef(m))

AICc_local <- function(m, n) {
  k  <- get_k(m)
  Ai <- AIC(m)
  corr <- if (n > (k + 1)) 2 * k * (k + 1) / (n - k - 1) else Inf
  Ai + corr
}

modelos_lista <- list(
  "Joppa"        = modelo_joppa,
  "LuHe"         = modelo_luhe,
  "Logístico"    = modelo_logistico,
  "ExpNegativo"  = modelo_exp_neg
) |> discard(is.null)

n_eff <- nrow(desc_intervalo)

## Table 1 -------------------------------------
tabela_comparacao <- if (length(modelos_lista) > 0) {
  tibble(
    Modelo = names(modelos_lista),
    AICc   = map_dbl(modelos_lista, ~ AICc_local(.x, n = n_eff)),
    k      = map_int(modelos_lista, get_k),
    Stot_Estimado = map_dbl(modelos_lista, ~ { cf <- coef(.x); if ("Stot" %in% names(cf)) cf[["Stot"]] else NA_real_ })
  ) |>
    arrange(AICc) |>
    mutate(
      Delta_AICc = AICc - min(AICc),
      wAICc      = round(exp(-0.5*Delta_AICc) / sum(exp(-0.5*Delta_AICc)), 4),
      Stot_Estimado = round(Stot_Estimado, 1)
    )
} else tibble()

print(tabela_comparacao)

## Confidence interval ------------------------------
set.seed(1234)
boot_stot <- function(model, data, nboot = 1000) {
  
  stot_vals <- numeric(nboot)
  
  for (i in 1:nboot) {
    
    data_boot <- data[sample(1:nrow(data), replace = TRUE), ]
    
    fit <- tryCatch({
      update(model, data = data_boot)
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      cf <- coef(fit)
      if ("Stot" %in% names(cf)) {
        stot_vals[i] <- cf["Stot"]
      } else {
        stot_vals[i] <- NA
      }
    } else {
      stot_vals[i] <- NA
    }
  }
  
  stot_vals <- na.omit(stot_vals)
  
  quantile(stot_vals, c(0.025, 0.975))
}

boot_stot(modelo_logistico, desc_intervalo)
boot_stot(modelo_exp_neg, desc_intervalo)
boot_stot(modelo_joppa, desc_intervalo)
boot_stot(modelo_luhe, desc_intervalo)


ci_stot_df <- purrr::imap_dfr(modelos_lista, ~{
  
  ci <- boot_stot(.x, desc_intervalo, nboot = 1000)
  
  tibble(
    Modelo = .y,
    Stot = coef(.x)["Stot"],
    CI_low = ci[1],
    CI_high = ci[2]
  )
})

ci_stot_df

## Figure 2 ---------------------------------------
simular_full <- function(modelo, df_obs, ano_futuro = 2100, passo = 10) {
  if (is.null(modelo)) return(NULL)
  
  ano_ini <- min(df_obs$Ano_médio, na.rm = TRUE)
  anos_full <- seq(ano_ini, ano_futuro, by = passo)
  
  base <- tibble(
    Ano_médio = anos_full,
    Tempo     = anos_full - ano_ini
  ) |>
    dplyr::left_join(
      df_obs |>
        dplyr::select(Ano_médio, Tt, Acumulado_Total),
      by = "Ano_médio"
    )
  
  if (inherits(modelo, "gnls")) {
    cf <- coef(modelo)
    if (!all(c("Stot","a","b") %in% names(cf))) return(NULL)
    
    Stot <- cf[["Stot"]]
    a    <- cf[["a"]]
    b    <- cf[["b"]]
    
    tt_recent <- tail(stats::na.omit(df_obs$Tt), 3)
    tt_fut <- if (length(tt_recent) > 0) mean(tt_recent) else mean(df_obs$Tt, na.rm = TRUE)
    base$Tt[is.na(base$Tt)] <- tt_fut
    
    St_prev <- df_obs$Acumulado_Total[df_obs$Ano_médio == ano_ini][1]
    if (is.na(St_prev)) St_prev <- 0
    
    is_joppa <- any(grepl("Tempo", deparse(modelo$modelStruct$form))) ||
      any(grepl("Tempo", deparse(modelo$call$model)))
    
    St_sim <- numeric(nrow(base))
    
    for (i in seq_len(nrow(base))) {
      Tt_i <- base$Tt[i]
      t_i  <- base$Tempo[i]
      
      if (is_joppa) {
        Delta <- Tt_i * (a + b * t_i) * (Stot - St_prev)
      } else {
        denom <- 1 - Tt_i * b * (Stot - St_prev)
        Delta <- if (denom > 1e-6) (Tt_i * a * (Stot - St_prev)) / denom else 0
      }
      
      Delta   <- max(0, min(Delta, Stot - St_prev, 50))
      St_prev <- St_prev + Delta
      St_sim[i] <- St_prev
    }
    
    out <- tibble(Ano_médio = base$Ano_médio, Acum = St_sim)
    
  } else {
    St_pred <- predict(modelo, newdata = base |> dplyr::select(Tempo))
    out <- tibble(Ano_médio = base$Ano_médio, Acum = as.numeric(St_pred))
  }
  
  out
}

dados_obs <- desc_intervalo |>
  dplyr::select(Ano_médio, Acum_obs = Acumulado_Total)

curvas_full <- list(
  "Joppa et al. (2011)"  = simular_full(modelo_joppa, desc_intervalo, ano_futuro = 2100),
  "Lu & He (2017)"       = simular_full(modelo_luhe, desc_intervalo, ano_futuro = 2100),
  "Logistic"             = simular_full(modelo_logistico, desc_intervalo, ano_futuro = 2100),
  "Negative exponential" = simular_full(modelo_exp_neg, desc_intervalo, ano_futuro = 2100)
) |> purrr::discard(is.null)

dados_grafico <- purrr::reduce(
  .x = c(
    list(dados_obs),
    lapply(names(curvas_full), function(nm) dplyr::rename(curvas_full[[nm]], !!nm := Acum))
  ),
  .f = function(x, y) dplyr::full_join(x, y, by = "Ano_médio")
)

dados_longos <- dados_grafico |>
  tidyr::pivot_longer(-Ano_médio, names_to = "Series", values_to = "Accumulated") |>
  dplyr::filter(!is.na(Accumulated)) |>
  dplyr::mutate(
    Series = ifelse(Series == "Acum_obs", "Observed data", Series)
  )

cores <- c(
  "Observed data"        = "black",
  "Joppa et al. (2011)"  = "#E41A1C",
  "Lu & He (2017)"       = "#377EB8",
  "Logistic"             = "#4DAF4A",
  "Negative exponential" = "#984EA3"
)

linhas <- c(
  "Observed data"        = "solid",
  "Joppa et al. (2011)"  = "dashed",
  "Lu & He (2017)"       = "dotted",
  "Logistic"             = "dotdash",
  "Negative exponential" = "longdash"
)

cores_ok  <- cores [intersect(names(cores),  unique(dados_longos$Series))]
linhas_ok <- linhas[intersect(names(linhas), unique(dados_longos$Series))]

dados_longos$Series <- factor(
  dados_longos$Series,
  levels = rev(unique(dados_longos$Series))
)

fig2 = ggplot() +
  geom_line(
    data = dados_longos |> dplyr::filter(Series == "Observed data"),
    aes(Ano_médio, Accumulated, color = Series, linetype = Series),
    linewidth = 1.5
  ) +
  geom_point(
    data = dados_longos |> dplyr::filter(Series == "Observed data"),
    aes(Ano_médio, Accumulated),
    size = 3, color = "black"
  ) +
  geom_line(
    data = dados_longos |> dplyr::filter(Series != "Observed data"),
    aes(Ano_médio, Accumulated, color = Series, linetype = Series),
    linewidth = 1.5
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "darkgray", linewidth = 0.7) +
  scale_x_continuous(
    breaks = seq(1750, 2100, 25),
    limits = c(min(dados_longos$Ano_médio), 2100)
  ) +
  scale_y_continuous(
    limits = c(0, max(dados_longos$Accumulated, na.rm = TRUE) * 1.1)
  ) +
  scale_color_manual(values = cores_ok) +
  scale_linetype_manual(values = linhas_ok) +
  theme_classic(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )+
  labs(y = "Cumulative number of species", x = "Year")

fig2

ggsave("Figure_2.jpg",fig2,width = 10,height = 7)

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

## Species richness  ---------------------
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

## Figure 4 ---------------------------------
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

fig4 = ggplot() +
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

fig4

ggsave("figure_4.jpg",fig4)

## Figure 5 --------------------------------

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
  theme_minimal() +
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

fig_5 = ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +
  draw_plot(p_leg, 0.78, 0.05, 0.16, 0.16)

fig_5

ggsave("Figure_5.jpg", fig_5, width = 15, height = 10)

sf_use_s2(TRUE)