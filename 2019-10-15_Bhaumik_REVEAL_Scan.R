# 2019-10-15_Bhaumik_REVEAL_Scan.R


# Load Libraries ----

library(dplyr)
library(readr)
library(stringr)

# Load config / helpers ----
source("~/Box/Documents/R_helpers/config.R")
source("~/Box/Documents/R_helpers/helpers.R")

# EXTRACT ----

# _ Studies Database ----
fields_st_raw <-
  c(
    "subject_id"
    , "study"    # study == 79 for REVEAL Scan
    , "enrolled"
  )
fields_st <- fields_st_raw %>% paste(collapse = ",")

json_st <- 
  export_redcap_records(
    uri = REDCAP_API_URI,
    token = REDCAP_API_TOKEN_STUDIES,
    fields = fields_st,
    filterLogic = "([study] = '79' AND [enrolled] = '1')"
  )

df_st <-
  jsonlite::fromJSON(json_st) %>% 
  as_tibble() %>% 
  na_if("")


# _ MiNDSet Registry ----

fields_ms_raw <-
  c(
    "subject_id"
    , "exam_date"
    , "race_value" # 1 = White; 2 = Black
    , "uds_dx"
  )
fields_ms <- fields_ms_raw %>% paste(collapse = ",")

json_ms <-
  export_redcap_records(
    uri = REDCAP_API_URI,
    token = REDCAP_API_TOKEN_MINDSET,
    fields = fields_ms
  )
df_ms <-
  json_ms %>% 
  jsonlite::fromJSON() %>% 
  as_tibble() %>% 
  na_if("")


# _ UMMAP - UDS 3

fields_u3_hd_raw <-
  c(
    "ptid"
    , "form_date"
  )
fields_u3_d1_raw <-
  c(
    # Diagnosis
    "normcog"    # NL
    , "demented" # Demented
    , "amndem"   # Amnestic multidomain dementia syndrome
    , "pca"      # Posterior cortical atrophy syndrome
    , "ppasyn"   # Primary progressive aphasia (PPA) syndrome
    , "ftdsyn"   # Behavioral variant FTD (bvFTD) syndrome
    , "lbdsyn"   # Lewy body dementia syndrome
    , "namndem"  # Non-amnestic multidomain dementia syndrome
    , "mciamem"  # Amnestic MCI, single domain (aMCI SD) 
    , "mciaplus" # Amnestic MCI, multiple domains (aMCI MD)
    , "mcinon1"  # Non-amnestic MCI, single domain (naMCI SD)
    , "mcinon2"  # Non-amnestic MCI, multiple domains (naMCI MD)
    , "impnomci" # Cognitively impaired, not MCI
    # Etiology
    , "alzdis"   # Alzheimer's disease
    , "alzdisif" 
    , "lbdis"    # Lewy body disease
    , "lbdif" 
    , "msa"      # Multiple system atrophy
    , "msaif"
    , "psp"      # Progressive supranuclear palsy (PSP)
    , "pspif"
    , "cort"     # Corticobasal degeneration (CBD)
    , "cortif"
    , "ftldmo"   # FTLD with motor neuron disease
    , "ftldmoif"
    , "ftldnos"  # FTLD NOS
    , "ftldnoif"
    , "cvd"      # Vascular Brain injury (clinical or imaging evidence)
    , "cvdif" 
    , "esstrem"  # Essential tremor
    , "esstreif"
    , "downs"    # Down syndrome
    , "downsif"
    , "hunt"     # Huntington's disease
    , "huntif"
    , "prion"    # Prion disease (CJD, other)
    , "prionif"
    , "brninj"   # Traumatic brain injury
    , "brninjif"
    , "hyceph"   # Normal-pressure hydrocephalus
    , "hycephif"
    , "epilep"   # Epilepsy
    , "epilepif"
    , "neop"     # CNS neoplasm
    , "neopif" 
    , "hiv"      # Human immunodeficiency virus (HIV)
    , "hivif"
    # Condition
    , "dep"      # Active depression
    , "depif" 
    , "bipoldx"  # Bipolar disorder
    , "bipoldif"
    , "schizop"  # Schizophrenia or other psychosis
    , "schizoif"
    , "anxiet"   # Anxiety disorder
    , "anxietif"
    , "delir"    # Delirium
    , "delirif"
    , "ptsddx"   # Post-traumatic stress disorder (PTSD)
    , "ptsddxif"
    , "othpsy"   # Other psychiatric disease
    , "othpsyif"
    , "alcdem"   # Cognitive impairment due to alcohol abuse
    , "alcdemif"
    , "impsub"   # Cognitive impairment due to other substance abuse
    , "impsubif"
    , "dysill"   # Cognitive impairment due to systemic disease/medical illness
    , "dysillif"
    , "meds"     # Cognitive impairment due to medications
    , "medsif"
  ) %>% c(., paste0("fu_", .))
fields_u3_raw <-
  c(
    fields_u3_hd_raw
    , fields_u3_d1_raw
  )
fields_u3 <- fields_u3_raw %>% paste(collapse = ",")

json_u3 <-
  export_redcap_records(
    uri = REDCAP_API_URI,
    token = REDCAP_API_TOKEN_UDS3n,
    fields = fields_u3
  )
df_u3 <-
  json_u3 %>% 
  jsonlite::fromJSON() %>% 
  as_tibble() %>% 
  na_if("")


# TRANSFORM ---- 

df_st_cln <-
  df_st %>% 
  select(-redcap_repeat_instrument, -redcap_repeat_instance) %>% 
  filter(str_detect(subject_id, "^UM\\d{8}$"))

df_ms_cln <-
  df_ms %>% 
  select(-redcap_event_name) %>% 
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>% 
  filter(!is.na(exam_date)) %>%
  get_visit_n(id_field = subject_id, date_field = exam_date, Inf) 

df_u3_cln <-
  df_u3 %>% 
  select(-redcap_event_name) %>% 
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  filter(!is.na(form_date)) %>% 
  get_visit_n(id_field = ptid, date_field = form_date, Inf) %>% 
  coalesce_ift_cols()

df_u3_cln_mut <-
  df_u3_cln %>% 
  type_convert(col_types = cols(.default  = col_integer(),
                                ptid      = col_character(),
                                form_date = col_date())) %>% 
  # create MADC diagnosis field
  rowwise() %>% 
  mutate(madc_dx = case_when(
    sum(amndem, pca, ppasyn, ftdsyn, lbdsyn, namndem, na.rm = TRUE) > 1 ~
      "Mixed dementia",
    normcog == 1 ~ "NL",
    normcog == 0 & demented == 0 & 
      (mciamem == 1 | mciaplus == 1 | mcinon1 == 1 | mcinon2 == 1) ~ "MCI",
    normcog == 0 & demented == 0 &
      impnomci == 1 ~ "Impaired not MCI",
    normcog == 0 & demented == 1 & (amndem == 1 | namndem == 1) &
      alzdis == 1 & alzdisif == 1 ~ "AD",
    normcog == 0 & demented == 1 & lbdsyn == 1 ~ "LBD",
    normcog == 0 & demented == 1 & 
      (ftdsyn == 1 | 
         (psp == 1 & pspif == 1) |
         (cort == 1 & cortif == 1) |
         (ftldmo == 1 & ftldmoif == 1) | 
         (ftldnos == 1 & ftldnoif == 1)) ~ "FTD",
    normcog == 0 & demented == 1 &
      cvd == 1 & cvdif == 1 ~ "Vascular dementia",
    normcog == 0 & demented == 1 &
      ppasyn == 1 &
      (is.na(psp) |
         is.na(cort) |
         is.na(ftldmo) |
         is.na(ftldnos)) ~ "PPA",
    demented == 1 ~ "Other dementia",
    TRUE ~ NA_character_
  )) %>% 
  ungroup() %>% 
  select(ptid, form_date, madc_dx)

df_st_ms_u3 <-
  df_st_cln %>% 
  left_join(., df_ms_cln, by = "subject_id") %>% 
  left_join(., df_u3_cln_mut, by = c("subject_id" = "ptid")) %>% 
  mutate(madc_dx = case_when(
    !is.na(madc_dx) ~ madc_dx,
    is.na(madc_dx) & uds_dx == 26 ~ "NL",
    TRUE ~ madc_dx
  )) %>% 
  mutate(race_value = case_when(
    race_value == 1 ~ "White",
    race_value == 2 ~ "Black",
    TRUE ~ race_value
  ))

# LOAD ----

df_st_ms_u3 %>% 
  group_by(race_value, madc_dx) %>% 
  summarize(n = n())

df_st_ms_u3 %>% 
  group_by(race_value, madc_dx) %>% 
  summarize(n = n()) %>% 
  tidyr::spread(key = race_value, value = n)
