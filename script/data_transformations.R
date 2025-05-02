# libraries --------------------------------------------------------------------
library(here)
library(janitor)
library(tidyverse)
library(openxlsx)

# objects ----------------------------------------------------------------------
repeated_cols <- c(
  "annee_de_lappel_a_projets", "appel_a_projets", "code_des_thematiques", 
  "code_des_destinations", "action", "acronyme_du_projet", 
  "description_du_projet", "identifiant_de_projet", "flag_projet_coordonne", 
  "nom_de_actions_groupees", "free_keywords", "thematique_anglais", 
  "thematique_en_francais", "topic")

unique_cols <- c(
  "nom_de_lentite_participante", "pays", "role_du_participant", 
  "identifiant_du_regroupement_de_pays", "nom_type_dentite_cordis_en_francais", 
  "participation_linked", "pays_groupe", "statut_detaille_du_projet_laureat", 
  "montant_des_subventions_en", "nombre_de_coordinations", 
  "nombre_de_participants")

# data -------------------------------------------------------------------------
df <- read.xlsx(here("data/extrait projet européen 110425 15h00.xlsx")) |> 
  clean_names()

df_filtered <- df |> 
  filter(pays %in% c("France", "Irlande"))

projects_with_both <- df_filtered |>
  group_by(identifiant_de_projet) |>
  summarise(has_fr = any(pays == "France"),
            has_ie = any(pays == "Irlande")) |> 
  filter(has_fr & has_ie) |>
  pull(identifiant_de_projet)

coordinator_per_project <- df |> 
  filter(role_du_participant == "coordinator") |> 
  select(identifiant_de_projet, coordinator = pays)

partners_per_project <- df |> 
  filter(role_du_participant != "coordinator") |> 
  group_by(identifiant_de_projet) |> 
  summarise(all_partners = paste(pays, collapse = ", ")) |> 
  select(identifiant_de_projet, all_partners)

result <- df |>
  filter(identifiant_de_projet %in% projects_with_both) |> 
  select(all_of(repeated_cols)) |> 
  distinct() |> 
  left_join(coordinator_per_project, by = join_by(identifiant_de_projet)) |> 
  left_join(partners_per_project, by = join_by(identifiant_de_projet))

write_csv(result, here("data/data_fr_ie.csv"))
