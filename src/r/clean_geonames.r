#!/usr/bin/env Rscript
# ============================================================
# AFRO LQAS Geoname Cleaning & Date Harmonization Script
# GitHub Actions Compatible Version - COMPLETE
# Part 1/5: Header and Helper Functions
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
  library(readxl)
  library(logger)
  library(fs)
  library(argparse)
  library(stringr)
  library(stringi)
  library(janitor)
})

# Configure logging
log_appender(appender_file("logs/geoname_clean.log"))
log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("Starting AFRO LQAS Data Cleaning Pipeline")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Parse arguments
parser <- ArgumentParser()
parser$add_argument("--input-dir", default = "data/output", 
                   help = "Input directory with CSV files from level 1 processing")
parser$add_argument("--lookup-file", default = "data/lookup/lqas_lookup.xlsx", 
                   help = "Lookup Excel file path for date harmonization")
parser$add_argument("--output-file", default = "data/lqas_final.csv", 
                   help = "Final output file path")
parser$add_argument("--start-date", default = "2019-10-01", 
                   help = "Minimum start date filter")
args <- parser$parse_args()

log_info("Arguments:")
log_info("  input-dir: {args$input_dir}")
log_info("  lookup-file: {args$lookup_file}")
log_info("  output-file: {args$output_file}")
log_info("  start-date: {args$start_date}")

# ============================================================
# FUNCTION: APPLY DISTRICT MAPPINGS
# ============================================================

apply_district_mappings <- function(data) {
  data %>%
    mutate(
      district = case_when(
        # ============================================================
        # MAURITANIA DISTRICT CORRECTIONS
        # ============================================================
        country == "MAURITANIA" & district == "BARKÉOL" ~ "BARKÉOLE",
        country == "MAURITANIA" & district == "GUÉROU" ~ "GUÉRROU",
        country == "MAURITANIA" & district == "BARKÃ‰OLE" ~ "BARKÉOLE",
        country == "MAURITANIA" & district == "GUÃ‰RROU" ~ "GUÉRROU",
        country == "MAURITANIA" & district == "MAGTAA LAHJAR" ~ "MAGTA LAHJAR",
        country == "MAURITANIA" & district == "MBAGNE" ~ "M'BAGNE",
        country == "MAURITANIA" & district == "BABABÃ‰" ~ "BABABÉ",
        country == "MAURITANIA" & district == "BOGHÃ‰" ~ "BOGHÉ",
        country == "MAURITANIA" & district == "KAÉDI" ~ "KAEDI",
        country == "MAURITANIA" & district == "KHABOU" ~ "GHABOU",
        country == "MAURITANIA" & district == "OULD YENGÉ" ~ "OULD YENGE",
        country == "MAURITANIA" & district == "DJIGUENNI" ~ "DJIGUENI",
        country == "MAURITANIA" & district == "TEMBEDRA" ~ "TIMBÉDRA",
        country == "MAURITANIA" & district == "NÃ‰MA" ~ "NÉMA",
        country == "MAURITANIA" & district == "TIMBÃ‰DRA" ~ "TIMBÉDRA",
        country == "MAURITANIA" & district == "BENECHAB" ~ "BENECHABE",
        country == "MAURITANIA" & district == "MOUDJÃ‰RIA" ~ "MOUDJÉRIA",
        country == "MAURITANIA" & district == "BIR MOGREIN" ~ "BIR MOGHREN",
        country == "MAURITANIA" & district == "FDÉRIK" ~ "F'DERICK",
        country == "MAURITANIA" & district == "ZOUÉRATT" ~ "ZOUÉRAT",
        country == "MAURITANIA" & district == "ZOUÃ‰RAT" ~ "ZOUÉRAT",
        country == "MAURITANIA" & district == "KEUR MACÈNE" ~ "KEUR MACENE",
        country == "MAURITANIA" & district == "THÉIKANE" ~ "TEIKANE",
        country == "MAURITANIA" & district == "RIYADH" ~ "RIYAD",
        country == "MAURITANIA" & district == "LEKSEIBE" ~ "LEXEIBA",
        country == "MAURITANIA" & district == "WOMPOU" ~ "WOMPO",
        country == "MAURITANIA" & district == "ADEL BAGHROU" ~ "ADEL BEGROU",
        country == "MAURITANIA" & district == "AKJOUJET" ~ "AKJOUJT",
        country == "MAURITANIA" & district == "BABABE" ~ "BABABÉ",
        country == "MAURITANIA" & district == "BIR OUMGREINE" ~ "BIR MOGHREN",
        country == "MAURITANIA" & district == "BOGHE" ~ "BOGHÉ",
        country == "MAURITANIA" & district == "BARKEOL" ~ "BARKÉOLE",
        country == "MAURITANIA" & district == "CHINGUITTI" ~ "CHINGUITTY",
        country == "MAURITANIA" & district == "D_HAR" ~ "D'HAR",
        country == "MAURITANIA" & district == "BOUTILIMITT" ~ "BOUTILIMIT",
        country == "MAURITANIA" & district == "F_DERIK" ~ "F'DERICK",
        country == "MAURITANIA" & district == "GUERROU" ~ "GUÉRROU",
        country == "MAURITANIA" & district == "KANKOUSSA" ~ "KANKOSSA",
        country == "MAURITANIA" & district == "KOBENNI" ~ "KOBENI",
        country == "MAURITANIA" & district == "M_BAGNE" ~ "M'BAGNE",
        country == "MAURITANIA" & district == "M_BOUT" ~ "M'BOUT",
        country == "MAURITANIA" & district == "MAGHTA LEHJAR" ~ "MAGTA LAHJAR",
        country == "MAURITANIA" & district == "MOUDJRIA" ~ "MOUDJÉRIA",
        country == "MAURITANIA" & district == "NEMA" ~ "NÉMA",
        country == "MAURITANIA" & district == "OUAD-NAGA" ~ "OUAD NAGA",
        country == "MAURITANIA" & district == "R_KIZ" ~ "R'KIZ",
        country == "MAURITANIA" & district == "SEILIBABY" ~ "SELIBABY",
        country == "MAURITANIA" & district == "TAMCHEKETT" ~ "TAMCHAKET",
        country == "MAURITANIA" & district == "TEVRAGH ZEINE" ~ "TEVRAGH ZEINA",
        country == "MAURITANIA" & district == "TICHITT" ~ "TICHIT",
        country == "MAURITANIA" & district == "TIMBEDRA" ~ "TIMBÉDRA",
        country == "MAURITANIA" & district == "ZOUERATE" ~ "ZOUÉRAT",
        country == "MAURITANIA" & district == "BASSEKNOU" ~ "BASSIKNOU",
        country == "MAURITANIA" & district == "BARKEIWEL" ~ "BARKÉOLE",
        country == "MAURITANIA" & district == "BIRMOUGREIN" ~ "BIR MOGHREN",
        country == "MAURITANIA" & district == "TIJIKJA" ~ "TIDJIKJA",
        country == "MAURITANIA" & district == "BARKEOLE" ~ "BARKÉOLE",
        country == "MAURITANIA" & district == "GUERROU" ~ "GUÉRROU",
        country == "MAURITANIA" & district == "BABABE" ~ "BABABÉ",
        country == "MAURITANIA" & district == "BOGHE" ~ "BOGHÉ",
        country == "MAURITANIA" & district == "NEMA" ~ "NÉMA",
        country == "MAURITANIA" & district == "MOUDJERIA" ~ "MOUDJÉRIA",
        country == "MAURITANIA" & district == "ZOUERATE" ~ "ZOUÉRAT",
        
        # ============================================================
        # GAMBIA DISTRICT CORRECTIONS
        # ============================================================
        country == "GAMBIA" & district == "CENTRAL RIVER Province" ~ "CENTRAL RIVER REGION",
        country == "GAMBIA" & district == "NIAMINA DANKUNKU" ~ "NIAMNA DANKUNKU",
        country == "GAMBIA" & district == "SABAKH SANJAL" ~ "SABACH",
        country == "GAMBIA" & district == "FULLADU EAST" ~ "BASSE",
        country == "GAMBIA" & district == "LOWER FULLADU WEST" ~ "CENTRAL RIVER province",
        
        # ============================================================
        # BURKINA FASO DISTRICT CORRECTIONS
        # ============================================================
        country == "BURKINA FASO" & district == "GOROM GOROM" ~ "GOROM",
        country == "BURKINA FASO" & district == "PÃ”" ~ "PÔ",
        country == "BURKINA FASO" & district == "NONGR-MASSOM" ~ "NONGR MASSOM",
        country == "BURKINA FASO" & district == "SIGH-NOGHIN" ~ "SIG NOGHIN",
        country == "BURKINA FASO" & district == "NDOROLA" ~ "N'DOROLA",
        country == "BURKINA FASO" & district == "PO" ~ "PÔ",
        
        # ============================================================
        # GUINEA DISTRICT CORRECTIONS
        # ============================================================
        country == "GUINEA" & district == "KÉROUANÉ" ~ "KÉROUANE",
        country == "GUINEA" & district == "FORECAREAH" ~ "FORÉCARIAH",
        country == "GUINEA" & district == "N'ZEREKORÉ" ~ "N'ZÉRÉKORÉ",
        country == "GUINEA" & district == "LABÃ‰" ~ "LABÉ",
        country == "GUINEA" & district == "TOUGUÃ‰" ~ "TOUGUÉ",
        country == "GUINEA" & district == "NZEREKORE" ~ "N'ZÉRÉKORÉ",
        country == "GUINEA" & district == "DUBREKA" ~ "DUBRÉKA",
        country == "GUINEA" & district == "GUECKEDOU" ~ "GUECKÉDOU",
        country == "GUINEA" & district == "LELOUMA" ~ "LÉLOUMA",
        country == "GUINEA" & district == "BOKE" ~ "BOKÉ",
        country == "GUINEA" & district == "KEROUANE" ~ "KÉROUANE",
        country == "GUINEA" & district == "FORECARIAH" ~ "FORÉCARIAH",
        country == "GUINEA" & district == "TELIMELE" ~ "TÉLIMÉLÉ",
        country == "GUINEA" & district == "LABE" ~ "LABÉ",
        country == "GUINEA" & district == "TOUGUE" ~ "TOUGUÉ",
        country == "GUINEA" & district == "N'ZÃ‰RÃ‰KORÃ‰" ~ "N'ZÉRÉKORÉ",
        
        # ============================================================
        # SIERRA LEONE DISTRICT CORRECTIONS
        # ============================================================
        country == "SIERRA LEONE" & district == "PORTLOKO" ~ "PORT LOKO",
        country == "SIERRA LEONE" & district == "WESTERN AREA RURAL" ~ "WESTERN RURAL",
        country == "SIERRA LEONE" & district == "WESTERN AREA URBAN" ~ "WESTERN URBAN",
        country == "SIERRA LEONE" & district == "WESTERN RUR" ~ "WESTERN RURAL",
        country == "SIERRA LEONE" & district == "WESTERN URB" ~ "WESTERN URBAN",
        
        # ============================================================
        # GHANA DISTRICT CORRECTIONS
        # ============================================================
        country == "GHANA" & district == "LOWER-MANYA-KROBO" ~ "LOWER-MANYA KROBO",
        country == "GHANA" & district == "ASANTE MAMPONG" ~ "ASANTE-MAMPONG",
        country == "GHANA" & district == "ASOKORE MAMPONG" ~ "ASOKORE-MAMPONG",
        country == "GHANA" & district == "ABUAKWA SOUTH" ~ "EAST AKIM - ABUAKWA SOUTH",
        country == "GHANA" & district == "TWIFO ATI MORKWA" ~ "TWIFO ATI-MORKWA",
        country == "GHANA" & district == "LOWER MANYA-KROBO" ~ "LOWER-MANYA-KROBO",
        
        # ============================================================
        # TOGO DISTRICT CORRECTIONS
        # ============================================================
        country == "TOGO" & district == "EST_MONO" ~ "EST-MONO",
        country == "TOGO" & district == "MOYEN_MONO" ~ "MOYEN-MONO",
        country == "TOGO" & district == "TÔNE" ~ "TONE",
        country == "TOGO" & district == "MÃ”" ~ "MÔ",
        country == "TOGO" & district == "KÃ‰RAN" ~ "KÉRAN",
        country == "TOGO" & district == "AGOÃˆ" ~ "AGOÈ",
        country == "TOGO" & district == "AVÃ‰" ~ "AVÉ",
        country == "TOGO" & district == "AKÃ‰BOU" ~ "AKÉBOU",
        country == "TOGO" & district == "ANIÃ‰" ~ "ANIÉ",
        country == "TOGO" & district == "KPÃ‰LÃ‰" ~ "KPÉLÉ",
        country == "TOGO" & district == "CINKASSÃ‰" ~ "CINKASSÉ",
        country == "TOGO" & district == "BAS MONO" ~ "BAS-MONO",
        country == "TOGO" & district == "AGOE NYIVE" ~ "AGOE",
        country == "TOGO" & district == "EST MONO" ~ "EST-MONO",
        country == "TOGO" & district == "MOYEN MONO" ~ "MOYEN-MONO",
        
        # ============================================================
        # BENIN DISTRICT CORRECTIONS
        # ============================================================
        country == "BENIN" & district == "DASSA-ZOUME" ~ "DASSA-ZOUNME",
        country == "BENIN" & district == "KARIMAMA" ~ "KARMAMA",
        country == "BENIN" & district == "BOUKOUMBE" ~ "BOUKOMBE",
        country == "BENIN" & district == "DASSA" ~ "DASSA-ZOUNME",
        country == "BENIN" & district == "DJAKOTOMEY" ~ "DJAKOTOME",
        country == "BENIN" & district == "ZA KPOTA" ~ "ZA-KPOTA",
        country == "BENIN" & district == "Cotonou I" ~ "COTONOU 1",
        country == "BENIN" & district == "Abomey-Calavi 1" ~ "ABOMEY-CALAVI 1",
        country == "BENIN" & district == "GODOMEY" ~ "ABOMEY-CALAVI 1",
        country == "BENIN" & district == "COTONOU I" ~ "COTONOU 1",
        country == "BENIN" & district == "COTONOU II" ~ "COTONOU 2",
        country == "BENIN" & district == "COTONOU III" ~ "COTONOU 3",
        country == "BENIN" & district == "COTONOU IV" ~ "COTONOU 4",
        country == "BENIN" & district == "COTONOU V" ~ "COTONOU 5",
        country == "BENIN" & district == "COTONOU VI" ~ "COTONOU 6",
        country == "BENIN" & district == "SEME-PODJI" ~ "SEME-KPODJI",
        country == "BENIN" & district == "PORTO-NOVO" ~ "PORTO-NOVO 1",
        country == "BENIN" & district == "PORTO-NOVO 1" ~ "PORTO-NOVO 1",
        country == "BENIN" & district == "BEINI" ~ "BENIN",
        
        # ============================================================
        # SENEGAL DISTRICT CORRECTIONS
        # ============================================================
        country == "SENEGAL" & district == "KÃ‰DOUGOU" ~ "KEDOUGOU",
        country == "SENEGAL" & district == "SALÃ‰MATA" ~ "SALEMATA",
        country == "SENEGAL" & district == "SÃ‰DHIOU" ~ "SEDHIOU",
        country == "SENEGAL" & district == "SARAYA" ~ "SARAYA",
        country == "SENEGAL" & district == "BIRKELANE" ~ "BIRKILANE",
        country == "SENEGAL" & district == "MALEM HODAR" ~ "MALEM HODDAR",
        country == "SENEGAL" & district == "DAROU-MOUSTY" ~ "DAROU MOUSTY",
        country == "SENEGAL" & district == "KOKI" ~ "COKI",
        country == "SENEGAL" & district == "SAINT-LOUIS" ~ "SAINT LOUIS",
        country == "SENEGAL" & district == "DIANKHE MAKHAN" ~ "DIANKE MAKHA",
        country == "SENEGAL" & district == "MAKACOLIBANTANG" ~ "MAKA COLIBANTANG",
        country == "SENEGAL" & district == "THIONCK-ESSYL" ~ "THIONCK ESSYL",
        
        # ============================================================
        # MALI DISTRICT CORRECTIONS
        # ============================================================
        country == "MALI" & district == "district KOULIKORO" ~ "KOULIKORO",
        country == "MALI" & district == "district SIKASSO" ~ "SIKASSO",
        country == "MALI" & district == "DIOLA" ~ "DIOILA",
        country == "MALI" & district == "KALABAN" ~ "KALABANCORO",
        country == "MALI" & district == "KOLONTIEBA" ~ "KOLONDIEBA",
        country == "MALI" & district == "KOULIKOROO" ~ "KOULIKORO",
        country == "MALI" & district == "SOFETO" ~ "SEFETO",
        country == "MALI" & district == "KALABAN CORO" ~ "KALABANCORO",
        country == "MALI" & district == "TIN-ESSAKO" ~ "TINESSAKO",
        country == "MALI" & district == "ALOURCHE" ~ "AL-OURCHE",
        country == "MALI" & district == "KALABAN-CORO" ~ "KALABANCORO",
        country == "MALI" & district == "TAOUDENIT" ~ "TAOUDENI",
        country == "MALI" & district == "ALMOUSTRAT" ~ "ALMOUSTARAT",
        country == "MALI" & district == "COMMUNE 1" ~ "COMMUNE I",
        country == "MALI" & district == "COMMUNE 2" ~ "COMMUNE II",
        country == "MALI" & district == "COMMUNE 3" ~ "COMMUNE III",
        country == "MALI" & district == "COMMUNE 4" ~ "COMMUNE IV",
        country == "MALI" & district == "COMMUNE 5" ~ "COMMUNE V",
        country == "MALI" & district == "COMMUNE 6" ~ "COMMUNE VI",
        country == "MALI" & district == "DJENNE" ~ "DJENNÉ",
        country == "MALI" & district == "SAGABARY" ~ "SAGABARI",
        country == "MALI" & district == "OUSSOUBIDIAGNIA" ~ "OUSSOUBIDIAGNA",
        
        # ============================================================
        # NIGER DISTRICT CORRECTIONS
        # ============================================================
        country == "NIGER" & district == "AGADEZ COMMUNE" ~ "AGADEZ",
        country == "NIGER" & district == "DIFFA COMMUNE" ~ "DIFFA",
        country == "NIGER" & district == "MAINÉ SOROA" ~ "MAINE SOROA",
        country == "NIGER" & district == "DOGON-DOUTCHI" ~ "DOGON DOUTCHI",
        country == "NIGER" & district == "FALMEYE" ~ "FALMEY",
        country == "NIGER" & district == "TAHOUA COMMUNE" ~ "TAHOUA COM",
        country == "NIGER" & district == "TAHOUA DÉPARTEMENT" ~ "TAHOUA DEP",
        country == "NIGER" & district == "MATAMAYE" ~ "MATAMÈYE",
        country == "NIGER" & district == "TIBIRI (DOUTCHI)" ~ "TIBIRI",
        country == "NIGER" & district == "NIAMEY  I" ~ "NIAMEY 1",
        country == "NIGER" & district == "NIAMEY  II" ~ "NIAMEY 2",
        country == "NIGER" & district == "NIAMEY  III" ~ "NIAMEY 3",
        country == "NIGER" & district == "NIAMEY  IV" ~ "NIAMEY 4",
        country == "NIGER" & district == "NIAMEY  V" ~ "NIAMEY 5",
        country == "NIGER" & district == "TAHOUA VILLE" ~ "TAHOUA COM",
        country == "NIGER" & district == "BALLAYARA" ~ "BALLEYARA",
        country == "NIGER" & district == "GOTHEYE" ~ "GOTHÈYE",
        country == "NIGER" & district == "OULLAM" ~ "OUALLAM",
        country == "NIGER" & district == "TILLABÉRY" ~ "DS TILLABERI",
        country == "NIGER" & district == "BELBÉDJI" ~ "BELBEDJI",
        country == "NIGER" & district == "TCHIROZÃ‰RINE" ~ "TCHIROZÉRINE",
        country == "NIGER" & district == "KANTCHE" ~ "KANTCHÉ",
        country == "NIGER" & district == "IFEROUANE" ~ "IFÉROUANE",
        country == "NIGER" & district == "DAMAGARAM TAKAYYA" ~ "DAMAGARAM TAKAYA",
        country == "NIGER" & district == "NGOURTI" ~ "N'GOURTI",
        country == "NIGER" & district == "DOGONDOUTCHI" ~ "DOGON DOUTCHI",
        country == "NIGER" & district == "GUIDAN ROUMDJI" ~ "G. ROUMDJI",
        country == "NIGER" & district == "TAHOUA DEPT" ~ "TAHOUA DEP",
        country == "NIGER" & district == "TILLABERY" ~ "TILLABERI",
        country == "NIGER" & district == "TAKIETA" ~ "TAKEITA",
        country == "NIGER" & district == "TARKA (BELBEJI)" ~ "BELBEDJI",
        country == "NIGER" & district == "ZINDER VILLE" ~ "ZINDER",
        country == "NIGER" & district == "AGUIÃ‰" ~ "AGUIÉ",
        country == "NIGER" & district == "TÃ‰RA" ~ "TERA",
        country == "NIGER" & district == "GOURÃ‰" ~ "GOURÉ",
        country == "NIGER" & district == "IFÃ‰ROUANE" ~ "IFÉROUANE",
        country == "NIGER" & district == "ILLÃ‰LA" ~ "ILLÉLA",
        country == "NIGER" & district == "MATAMÃˆYE" ~ "MATAMEYE",
        country == "NIGER" & district == "FILINGUÃ‰" ~ "FILINGUE",
        country == "NIGER" & district == "KANTCHÃ‰" ~ "KANTCHÉ",
        country == "NIGER" & district == "GOTHÃˆYE" ~ "GOTHÈYE",
        
        # ============================================================
        # CAMEROON DISTRICT CORRECTIONS
        # ============================================================
        country == "CAMEROON" & district == "BIYEM_ASSI" ~ "BIYEM ASSI",
        country == "CAMEROON" & district == "CITE_VERTE" ~ "CITE VERTE",
        country == "CAMEROON" & district == "ELIG_MFOMO" ~ "ELIG MFOMO",
        country == "CAMEROON" & district == "NANGA_EBOKO" ~ "NANGA EBOKO",
        country == "CAMEROON" & district == "NGOG_MAPUBI" ~ "NGOG MAPUBI",
        country == "CAMEROON" & district == "ABONG_MBANG" ~ "ABONG MBANG",
        country == "CAMEROON" & district == "BETARE_OYA" ~ "BETARE OYA",
        country == "CAMEROON" & district == "GAROUA-BOULAI" ~ "GAROUA BOULAI",
        country == "CAMEROON" & district == "NGUELEMENDOUGA" ~ "NGUELEMENDOUKA",
        country == "CAMEROON" & district == "KAR_HAY" ~ "KAR HAY",
        country == "CAMEROON" & district == "MAROUA1" ~ "MAROUA 1",
        country == "CAMEROON" & district == "MAROUA2" ~ "MAROUA 2",
        country == "CAMEROON" & district == "MAROUA3" ~ "MAROUA 3",
        country == "CAMEROON" & district == "CITE_DES_PALMIERS" ~ "CITE PALMIERS",
        country == "CAMEROON" & district == "NJOMBE_PENJA" ~ "NJOMBE PENJA",
        country == "CAMEROON" & district == "NEWBELL" ~ "NEW BELL",
        country == "CAMEROON" & district == "BAMENDA 3" ~ "BAMENDA",
        country == "CAMEROON" & district == "BAMENDA III" ~ "BAMENDA",
        country == "CAMEROON" & district == "KUMBOEAST" ~ "KUMBO EAST",
        country == "CAMEROON" & district == "KUMBOWEST" ~ "KUMBO WEST",
        country == "CAMEROON" & district == "GAROUA I" ~ "GAROUA 1",
        country == "CAMEROON" & district == "GAROUA II" ~ "GAROUA 2",
        country == "CAMEROON" & district == "GASHIGA" ~ "GASCHIGA",
        country == "CAMEROON" & district == "MALANTOUEN" ~ "MALENTOUEN",
        country == "CAMEROON" & district == "PENKAMICHEL" ~ "PENKA MICHEL",
        country == "CAMEROON" & district == "EKONDO_TITI" ~ "EKONDO TITI",
        country == "CAMEROON" & district == "EYUMOJOCK" ~ "EYUMODJOCK",
        country == "CAMEROON" & district == "KUMBA NORD" ~ "KUMBA",
        country == "CAMEROON" & district == "KUMBA SUD" ~ "KUMBA",
        country == "CAMEROON" & district == "KUMBA NORTH" ~ "KUMBA",
        country == "CAMEROON" & district == "KUMBA SOUTH" ~ "KUMBA",
        country == "CAMEROON" & district == "MOZOGO" ~ "MOZONGO",
        
        # ============================================================
        # CHAD DISTRICT CORRECTIONS
        # ============================================================
        country == "CHAD" & district == "OUM-HADJER" ~ "OUM HADJER",
        country == "CHAD" & district == "KOUBA OLANGA5" ~ "KOUBA OLANGA",
        country == "CHAD" & district == "BAILLI" ~ "BA ILLI",
        country == "CHAD" & district == "MOURDI" ~ "MOURDI DJONA",
        country == "CHAD" & district == "OUNIANGA" ~ "OUNIANGA KEBIR",
        country == "CHAD" & district == "LAOKASSI" ~ "LAOKASSY",
        country == "CHAD" & district == "NDJAMENA CENTRE" ~ "N'DJAMENA CENTRE",
        country == "CHAD" & district == "NDJAMENA EST" ~ "N'DJAMENA EST",
        country == "CHAD" & district == "NDJAMENA NORD" ~ "N'DJAMENA NORD",
        country == "CHAD" & district == "NDJAMENA SUD" ~ "N'DJAMENA SUD",
        country == "CHAD" & district == "AMADAM" ~ "AM DAM",
        country == "CHAD" & district == "AMTIMAN" ~ "AM TIMAN",
        country == "CHAD" & district == "MANGUEIGNE" ~ "HARAZE MANGUEIGNE",
        country == "CHAD" & district == "GOZ-BEIDA" ~ "GOZ BEIDA",
        country == "CHAD" & district == "N’TIONA" ~ "N'TIONA",
        country == "CHAD" & district == "DS CHADDRA" ~ "CHADRA",
        country == "CHAD" & district == "DS MOUSSORO" ~ "MOUSSORO",
        country == "CHAD" & district == "DJEDA" ~ "DJEDDA",
        country == "CHAD" & district == "OUMHADJER" ~ "OUM HADJER",
        country == "CHAD" & district == "BAGASOLA" ~ "BAGASSOLA",
        country == "CHAD" & district == "BA-ILLI" ~ "BA ILLI",
        country == "CHAD" & district == "PONT_CAROL" ~ "PONT CAROL",
        country == "CHAD" & district == "HARAZE_MANGUEIGNE" ~ "HARAZE MANGUEIGNE",
        country == "CHAD" & district == "AM_TIMAN" ~ "AM TIMAN",
        country == "CHAD" & district == "NDJAMENA_9AR" ~ "N'DJAMENA SUD",
        country == "CHAD" & district == "BA_ILLI" ~ "BA ILLI",
        country == "CHAD" & district == "OUM_HADJER" ~ "OUM HADJER",
        country == "CHAD" & district == "RIG_RIG" ~ "RIG RIG",
        country == "CHAD" & district == "N'DJAMENA-CENTRE" ~ "N'DJAMENA CENTRE",
        country == "CHAD" & district == "N'DJAMENA-SUD" ~ "N'DJAMENA SUD",
        country == "CHAD" & district == "N'DJAMENA-EST" ~ "N'DJAMENA EST",
        country == "CHAD" & district == "N'DJAMENA-NORD" ~ "N'DJAMENA NORD",
        country == "CHAD" & district == "HADJER-HADID" ~ "OUM HADJER",
        country == "CHAD" & district == "TINE" ~ "BILTINE",
        country == "CHAD" & district == "MICHEMERE" ~ "MICHEMIRE",
        country == "CHAD" & district == "MOUNDOU EST" ~ "MOUNDOU",
        country == "CHAD" & district == "MOUNDOU CENTRE" ~ "MOUNDOU",
        country == "CHAD" & district == "GOUNOUGAYA" ~ "GOUNOU GAYA",
        country == "CHAD" & district == "BARDAÏ" ~ "BARDAI",
        country == "CHAD" & district == "MOUNDOU OUEST" ~ "MOUNDOU",
        country == "CHAD" & district == "9E ARRONDISSEMENT" ~ "N'DJAMENA SUD",
        country == "CHAD" & district == "BIOBE" ~ "BIOBE SINGAKO",
        country == "CHAD" & district == "GOZ_BEIDA" ~ "GOZ BEIDA",
        country == "CHAD" & district == "KOUKOU" ~ "KOUKOU ANGARANA",
        country == "CHAD" & district == "NOUKOU" ~ "NOKOU",
        country == "CHAD" & district == "NTIONA" ~ "N'TIONA",
        country == "CHAD" & district == "RIG-RIG" ~ "RIG RIG",
        country == "CHAD" & district == "GUELO" ~ "GUELAO",
        country == "CHAD" & district == "NDJAMENA_CENTRE" ~ "N'DJAMENA CENTRE",
        country == "CHAD" & district == "NDJAMENA_EST" ~ "N'DJAMENA EST",
        country == "CHAD" & district == "BEBIDJA" ~ "BEBEDJIA",
        country == "CHAD" & district == "NDJAMENA_NORD" ~ "N'DJAMENA NORD",
        country == "CHAD" & district == "NDJAMENA_SUD" ~ "N'DJAMENA SUD",
        country == "CHAD" & district == "AM-TIMAN" ~ "AM TIMAN",
        country == "CHAD" & district == "BAKTCHORO" ~ "BAKCTCHORO",
        country == "CHAD" & district == "BAGA SOLA" ~ "BAGASSOLA",
        
        # ============================================================
        # CENTRAL AFRICAN REPUBLIC DISTRICT CORRECTIONS
        # ============================================================
        country == "CENTRAL AFRICAN REPUBLIC" & district == "BABOUA" ~ "BABOUA-ABBA",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "BOUAR" ~ "BOUAR-BAORO",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "CARNOT" ~ "CARNOT-GADZI",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "BOCARANGA" ~ "BOCARANGA-KOUI",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "BOGUILA" ~ "NANGA-BOGUILA",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "BOZOUM" ~ "BOZOUM-BOSSEMPTELE",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "GRIMARI" ~ "KOUANGO-GRIMARI",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "ALINDAO" ~ "ALINDAO-MINGALA",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "KEMBE" ~ "KEMBE-SATEMA",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "MOBAYE" ~ "MOBAYE-ZANGBA",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "OUANGO" ~ "OUANGO-GAMBO",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "NANGHA-BOGUILA" ~ "NANGA-BOGUILA",
        country == "CENTRAL AFRICAN REPUBLIC" & district == "NANA-GRIBIZI" ~ "NANA-GREBIZI",
        
        # ============================================================
        # ANGOLA DISTRICT CORRECTIONS
        # ============================================================
        country == "ANGOLA" & district == "N'HARÃŠA" ~ "NHAREA",
        country == "ANGOLA" & district == "NGONGUEMBO" ~ "GONGUEMBO",
        country == "ANGOLA" & district == "NÃ“QUI" ~ "NOQUI",
        country == "ANGOLA" & district == "PANGO-ALUQUEM" ~ "PANGO ALUQUEM",
        country == "ANGOLA" & district == "TÃ”MBUA (EX. PORTO ALEXANDRE)" ~ "TOMBUA",
        country == "ANGOLA" & district == "UCUMA" ~ "UKUMA",
        country == "ANGOLA" & district == "UÃGE" ~ "UIGE",
        country == "ANGOLA" & district == "XÃ-MUTEBA" ~ "XA MUTEBA",
        country == "ANGOLA" & district == "NZETU" ~ "NZETO",
        country == "ANGOLA" & district == "CELA (EX. UACU-CUNGO)" ~ "CELA",
        country == "ANGOLA" & district == "OMBADJA (EX. CUAMATO)" ~ "OMBADJA",
        country == "ANGOLA" & district == "TCHICALA TCHOLOHANGA" ~ "TCHIKALA-TCHOLOHAN",
        country == "ANGOLA" & district == "BUNDAS" ~ "LUMBALA NGUIMBO (BUNDAS)",
        country == "ANGOLA" & district == "AMBOIM (EX. GABELA)" ~ "AMBOIM",
        country == "ANGOLA" & district == "AMBUÃLA" ~ "AMBUILA",
        country == "ANGOLA" & district == "BAÃA FARTA" ~ "BAIA FARTA",
        country == "ANGOLA" & district == "BUENGAS (EX. NOVA ESPERANÃ‡A)" ~ "BUENGAS",
        country == "ANGOLA" & district == "BULA-ATUMBA" ~ "BULA ATUMBA",
        country == "ANGOLA" & district == "QUIUABA-N'ZOGI" ~ "KIWABA NZOGI",
        country == "ANGOLA" & district == "SAMBA CAJÃš" ~ "SAMBA CAJU",
        country == "ANGOLA" & district == "SELES (EX. UCU SELES)" ~ "SELES",
        country == "ANGOLA" & district == "SUMBE (EX. NGUNZA)" ~ "SUMBE",
        country == "ANGOLA" & district == "CAMEIA" ~ "LUMEJE (CAMEIA)",
        country == "ANGOLA" & district == "CATABOLA (EX. NOVA SINTRA)" ~ "CATABOLA",
        country == "ANGOLA" & district == "LÃ‰UA" ~ "LEUA",
        country == "ANGOLA" & district == "LIBOLO (EX. CALULO)" ~ "LIBOLO",
        country == "ANGOLA" & district == "LÃ“VUA" ~ "LOVUA",
        country == "ANGOLA" & district == "BUNDAS-LUMBALA-NGUIMBO" ~ "LUMBALA NGUIMBO (BUNDAS)",
        country == "ANGOLA" & district == "CAÃLA" ~ "CAALA",
        country == "ANGOLA" & district == "CACONGO (EX. LÃ‚NDANA)" ~ "CACONGO",
        country == "ANGOLA" & district == "DANDE (CAXITO)" ~ "DANDE",
        country == "ANGOLA" & district == "DEMBOS-QUIBAXE" ~ "DEMBOS (QUIBAXE)",
        country == "ANGOLA" & district == "GAMBOS (EX. CHIANGE)" ~ "GAMBOS",
        country == "ANGOLA" & district == "CUNDA-DIA-BAZE" ~ "KUNDA-DIA-BAZE",
        country == "ANGOLA" & district == "CUNHINGA (VOUGA)" ~ "CUNHINGA",
        country == "ANGOLA" & district == "MUCABA (EX. QUINZALA)" ~ "MUCABA",
        country == "ANGOLA" & district == "MUCARI" ~ "CACULAMA (MUCARI)",
        country == "ANGOLA" & district == "TCHIKALA TCHOLOHANG" ~ "TCHIKALA-TCHOLOHAN",
        country == "ANGOLA" & district == "CUROCA (EX. ONCOCUA)" ~ "CUROCA",
        country == "ANGOLA" & district == "MILUNGA (SANTA CRUZ)" ~ "MILUNGA",
        country == "ANGOLA" & district == "LUENA" ~ "MOXICO (LUENA)",
        country == "ANGOLA" & district == "TCHIKALA TCHOLOHANG" ~ "TCHIKALA TCHOLOHANGA",
        country == "ANGOLA" & district == "NGOLA KILUANGE" ~ "NGOLA QUILUANGE",
        country == "ANGOLA" & district == "CACULAMA" ~ "CACULAMA (MUCARI)",
        country == "ANGOLA" & district == "BUNDAS" ~ "BUENGAS",
        country == "ANGOLA" & district == "LUMBALA NGUIMBO" ~ "LUMBALA NGUIMBO (BUNDAS)",
        
        # ============================================================
        # MOZAMBIQUE DISTRICT CORRECTIONS
        # ============================================================
        country == "MOZAMBIQUE" & district == "CHIÃšRE" ~ "CHIÚRE",
        country == "MOZAMBIQUE" & district == "MARÃVIA" ~ "MARÁVIA",
        country == "MOZAMBIQUE" & district == "MAÃšA" ~ "MAUA",
        country == "MOZAMBIQUE" & district == "ALTO MOLÃ“CUÃˆ" ~ "ALTO MOLOCUE",
        country == "MOZAMBIQUE" & district == "ANGÃ“NIA" ~ "ANGONIA",
        country == "MOZAMBIQUE" & district == "MOCÃMBOA DA PRAIA" ~ "MACIMBOA DA PRAI",
        country == "MOZAMBIQUE" & district == "MÃGOÃˆ" ~ "MÁGOÈ",
        country == "MOZAMBIQUE" & district == "GURUÃ‰" ~ "GURUE",
        country == "MOZAMBIQUE" & district == "GILÃ‰" ~ "GILÉ",
        country == "MOZAMBIQUE" & district == "NGAÃšMA" ~ "NGAÚMA",
        country == "MOZAMBIQUE" & district == "PEMBA" ~ "PEMBA-METUGE",
        country == "MOZAMBIQUE" & district == "CHIMOIO" ~ "CIDADE DE CHIMOIO",
        country == "MOZAMBIQUE" & district == "NACALA PORTO" ~ "NACALA PORTO",
        country == "MOZAMBIQUE" & district == "GORONGOZA" ~ "GORONGOSA",
        country == "MOZAMBIQUE" & district == "TETE" ~ "CIDADE DE TETE",
        
        # ============================================================
        # ALGERIA DISTRICT CORRECTIONS
        # ============================================================
        country == "ALGERIA" & district == "EPSP ADRAR" ~ "Adrar",
        country == "ALGERIA" & district == "EPSP AOULEF" ~ "Aoulef",
        country == "ALGERIA" & district == "EPSP BADJI MOKHTAR" ~ "Bordj Badji Mokhtar",
        country == "ALGERIA" & district == "EPSP REGGANE" ~ "Reggana",
        country == "ALGERIA" & district == "EPSP TIMIMOUN" ~ "Timimmoun",
        country == "ALGERIA" & district == "EPSP TINERKOUK" ~ "Tinerkouk",
        country == "ALGERIA" & district == "EPSP ABADLA" ~ "Abadla",
        country == "ALGERIA" & district == "EPSP BECHAR" ~ "Bechar",
        country == "ALGERIA" & district == "EPSP BENI ABBES" ~ "Benni Abbes",
        country == "ALGERIA" & district == "EPSP BENI OUNIF" ~ "Beni Ounif",
        country == "ALGERIA" & district == "EPSP KERZAZ" ~ "Kerzaz",
        country == "ALGERIA" & district == "EPSP TABELBALA" ~ "Tabelbala",
        country == "ALGERIA" & district == "EPSP TAGHIT" ~ "Taghit",
        country == "ALGERIA" & district == "EPSP BREZINA" ~ "Breizina",
        country == "ALGERIA" & district == "EPSP CHELLALA" ~ "Chellala",
        country == "ALGERIA" & district == "EPSP EL BAYADH" ~ "El Baydh",
        country == "ALGERIA" & district == "EPSP KHEITER" ~ "Kheiter",
        country == "ALGERIA" & district == "EPSP DEBILA" ~ "Debila",
        country == "ALGERIA" & district == "EPSP DJEMAA" ~ "Djemaa",
        country == "ALGERIA" & district == "EPSP EL MEGHAIER" ~ "El Meghaeir",
        country == "ALGERIA" & district == "EPSP EL OUED" ~ "El Oued",
        country == "ALGERIA" & district == "EPSP GUEMAR" ~ "Guemar",
        country == "ALGERIA" & district == "EPSP TALEB LARBI" ~ "Taleb Arby",
        country == "ALGERIA" & district == "EPSP BERIANE" ~ "Berriane",
        country == "ALGERIA" & district == "EPSP EL MENEA" ~ "El Menea",
        country == "ALGERIA" & district == "EPSP GUERRARA" ~ "Guerrara",
        country == "ALGERIA" & district == "EPSP METLILI" ~ "Metlili",
        country == "ALGERIA" & district == "EPSP BORDJ OMAR IDRISS" ~ "Borj Omar Idriss",
        country == "ALGERIA" & district == "EPSP BORDJ-EL-HAOUESS" ~ "Borj El Haoues",
        country == "ALGERIA" & district == "EPSP DEBDEB" ~ "Deb Deb",
        country == "ALGERIA" & district == "EPSP DJANET" ~ "Djanet",
        country == "ALGERIA" & district == "EPSP ILLIZI" ~ "Illizi",
        country == "ALGERIA" & district == "EPSP IN AMENAS" ~ "In Amenas",
        country == "ALGERIA" & district == "EPSP AIN SEFRA" ~ "Ain Sefra",
        country == "ALGERIA" & district == "EPSP MECHERIA" ~ "Mecheria",
        country == "ALGERIA" & district == "EPSP MEKMEN BENAMER" ~ "Mekmen benamer",
        country == "ALGERIA" & district == "EPSP NAAMA" ~ "Naama",
        country == "ALGERIA" & district == "EPSP EL BORMA" ~ "El Borma",
        country == "ALGERIA" & district == "EPSP EL HADJIRA" ~ "El Hadjira",
        country == "ALGERIA" & district == "EPSP HASSI MESSAOUD" ~ "Hassi Messaoud",
        country == "ALGERIA" & district == "EPSP OUARGLA" ~ "Ouargla",
        country == "ALGERIA" & district == "EPSP TOUGGOURT" ~ "Touggourt",
        country == "ALGERIA" & district == "EPSP ABALESSA (SILET)" ~ "Abalessa",
        country == "ALGERIA" & district == "EPSP IN GUEZZAM" ~ "In Guezzam",
        country == "ALGERIA" & district == "EPSP IN MGUEL" ~ "In Amgueul",
        country == "ALGERIA" & district == "EPSP IN SALAH" ~ "In Salah",
        country == "ALGERIA" & district == "EPSP TAMENRASSET" ~ "Tamanrasset",
        country == "ALGERIA" & district == "EPSP TAZROUK" ~ "Tazrouk",
        country == "ALGERIA" & district == "EPSP TIN ZAOUATINE" ~ "Tin Zaouatine",
        country == "ALGERIA" & district == "EPSP OUM EL ASSEL" ~ "Oum El Assel",
        country == "ALGERIA" & district == "EPSP TINDOUF" ~ "Tindouf",
        
        # ============================================================
        # ETHIOPIA DISTRICT CORRECTIONS
        # ============================================================
        country == "ETHIOPIA" & district == "Abiy Adi" ~ "Abi Adi Town",
        country == "ETHIOPIA" & district == "Adet" ~ "Naeder Adet",
        country == "ETHIOPIA" & district == "Adwa Town" ~ "Adwa Town",
        country == "ETHIOPIA" & district == "Adwa Zuria" ~ "Adwa",
        country == "ETHIOPIA" & district == "Ahiferom" ~ "Aheferom",
        country == "ETHIOPIA" & district == "Axum Town" ~ "Axum Town",
        country == "ETHIOPIA" & district == "Laelay Maichew" ~ "Laelay Maychew",
        country == "ETHIOPIA" & district == "Tahitay Maichew" ~ "Tahtay Mayechew",
        country == "ETHIOPIA" & district == "Tankua Milash" ~ "Tanqua Abergele",
        country == "ETHIOPIA" & district == "Adigrat" ~ "Adigrat Town",
        country == "ETHIOPIA" & district == "Atsbi" ~ "Atsbi Wenberta",
        country == "ETHIOPIA" & district == "Ganta Afeshum" ~ "Ganta Afeshum",
        country == "ETHIOPIA" & district == "Gulomekeda" ~ "Gulo Mekeda",
        country == "ETHIOPIA" & district == "Hawzen" ~ "Hawzen",
        country == "ETHIOPIA" & district == "Kilte Awlaelo" ~ "Kelete Awelallo",
        country == "ETHIOPIA" & district == "Tsaeda Emba" ~ "Saesie Tsaedamba",
        country == "ETHIOPIA" & district == "Wukro" ~ "Wukro Town",
        country == "ETHIOPIA" & district == "Adi Haki" ~ "Adhaki",
        country == "ETHIOPIA" & district == "Ayder" ~ "Ayder",
        country == "ETHIOPIA" & district == "Hawolti" ~ "Hawelti",
        country == "ETHIOPIA" & district == "Quiha" ~ "Kuha",
        country == "ETHIOPIA" & district == "Asgede" ~ "Tsegede (Tigray)",
        country == "ETHIOPIA" & district == "Seyemti Adiyabo" ~ "Laelay Adiabo",
        country == "ETHIOPIA" & district == "Shire Town" ~ "Sheraro Town",
        country == "ETHIOPIA" & district == "Tahitay Koraro" ~ "Tahtay Koraro",
        country == "ETHIOPIA" & district == "Tsimbla" ~ "Asgede Tsimbila",
        country == "ETHIOPIA" & district == "Degua Temben" ~ "Dega Temben",
        country == "ETHIOPIA" & district == "Enderta" ~ "Enderta",
        country == "ETHIOPIA" & district == "Samre" ~ "Saharti Samre",
        country == "ETHIOPIA" & district == "Endamekoni" ~ "Endamehoni",
        country == "ETHIOPIA" & district == "Maichew Town" ~ "Maychew Town",
        country == "ETHIOPIA" & district == "Raya Azebo" ~ "Raya Azebo",
        
        # ============================================================
        # COTE D IVOIRE DISTRICT CORRECTIONS
        # ============================================================
        country == "COTE D IVOIRE" & district == "YOPOUGON-EST" ~ "YOPOUGON EST",
        country == "COTE D IVOIRE" & district == "YOPOUGON-OUEST SONGON" ~ "YOPOUGON OUEST-SONGON",
        country == "COTE D IVOIRE" & district == "COCODY BINGERVILLE" ~ "COCODY-BINGERVILLE",
        country == "COTE D IVOIRE" & district == "PORT-BOUET-VRIDI" ~ "PORT BOUET-VRIDI",
        country == "COTE D IVOIRE" & district == "BOUAKE-SUD" ~ "BOUAKE SUD",
        country == "COTE D IVOIRE" & district == "M'BATTO" ~ "MBATTO",
        country == "COTE D IVOIRE" & district == "KOUASSI KOUASSIKRO" ~ "KOUASSI-KOUASSIKRO",
        country == "COTE D IVOIRE" & district == "M'BENGUE" ~ "MBENGUE",
        country == "COTE D IVOIRE" & district == "SAN-PEDRO" ~ "SANPEDRO",
        country == "COTE D IVOIRE" & district == "ABOBO-EST" ~ "ABOBO EST",
        country == "COTE D IVOIRE" & district == "ABOBO-OUEST" ~ "ABOBO OUEST",
        country == "COTE D IVOIRE" & district == "YOPOUGON-OUEST" ~ "YOPOUGON EST",
        country == "COTE D IVOIRE" & district == "ADJAME-ATTECOUBE-PLATEAU" ~ "ADJAME-PLATEAU-ATTECOUBE",
        country == "COTE D IVOIRE" & district == "M'BAHIAKRO" ~ "MBAHIAKRO",
        country == "COTE D IVOIRE" & district == "DS BOUNA" ~ "BOUNA",
        country == "COTE D IVOIRE" & district == "DS DOROPO" ~ "DOROPO",
        country == "COTE D IVOIRE" & district == "DS NASSIAN" ~ "NASSIAN",
        country == "COTE D IVOIRE" & district == "DS TEHINI" ~ "TEHINI",
        country == "COTE D IVOIRE" & district == "DS BONDOUKOU (DR TEFRODOUO)" ~ "BONDOUKOU",
        country == "COTE D IVOIRE" & district == "ADJAME_PLATEAU_ATTECOUBE" ~ "ADJAME-PLATEAU-ATTECOUBE",
        country == "COTE D IVOIRE" & district == "TREICHVILLE_MARCORY" ~ "TREICHVILLE-MARCORY",
        country == "COTE D IVOIRE" & district == "PORT-BOUET-VRIDI" ~ "PORT BOUET-VRIDI",
        country == "COTE D IVOIRE" & district == "BOUAKE-SUD" ~ "BOUAKE SUD",
        country == "COTE D IVOIRE" & district == "SAN-PEDRO" ~ "SAN PEDRO",
        country == "COTE D IVOIRE" & district == "YOPOUGON-EST" ~ "YOPOUGON EST",
        country == "COTE D IVOIRE" & district == "YOPOUGON-OUEST SONGON" ~ "YOPOUGON OUEST-SONGON",
        country == "COTE D IVOIRE" & district == "KOUASSI KOUASSIKRO" ~ "KOUASSI-KOUASSIKRO",
        country == "COTE D IVOIRE" & district == "COCODY BINGERVILLE" ~ "COCODY-BINGERVILLE",
        country == "COTE D IVOIRE" & district == "GAGNOA1" ~ "GAGNOA 1",
        country == "COTE D IVOIRE" & district == "M'BENGUE" ~ "MBENGUE",
        country == "COTE D IVOIRE" & district == "BOUAKE-SUD" ~ "BOUAKE SUD",
        country == "COTE D IVOIRE" & district == "GAGNOA2" ~ "GAGNOA 2",
        country == "COTE D IVOIRE" & district == "GRAND_LAHOU" ~ "GRAND-LAHOU",
        country == "COTE D IVOIRE" & district == "YAKASSE_ATTOBROU" ~ "YAKASSE-ATTOBROU",
        country == "COTE D IVOIRE" & district == "KOUASSI KOUASSIKRO" ~ "KOUASSI-KOUASSIKRO",
        country == "COTE D IVOIRE" & district == "GRAND_BASSAM" ~ "GRAND-BASSAM",
        country == "COTE D IVOIRE" & district == "ZOUAN_HOUNIEN" ~ "ZOUAN-HOUNIEN",
        country == "COTE D IVOIRE" & district == "YOPOUGON-EST" ~ "YOPOUGON EST",
        country == "COTE D IVOIRE" & district == "YOPOUGON-OUEST SONGON" ~ "YOPOUGON OUEST-SONGON",
        country == "COTE D IVOIRE" & district == "COCODY BINGERVILLE" ~ "COCODY-BINGERVILLE",
        country == "COTE D IVOIRE" & district == "PORT-BOUET-VRIDI" ~ "PORT BOUET-VRIDI",
        country == "COTE D IVOIRE" & district == "SAN-PEDRO" ~ "SAN PEDRO",
        
        # ============================================================
        # KENYA DISTRICT CORRECTIONS
        # ============================================================
        country == "KENYA" & province == "BUSIA" & district == "TESO-CENTRAL" ~ "TESO CENTRAL",
        country == "KENYA" & province == "BUSIA" & district == "TESO-NORTH" ~ "TESO NORTH",
        country == "KENYA" & province == "BUSIA" & district == "TESO-SOUTH" ~ "TESO SOUTH",
        country == "KENYA" & province == "KAJIADO" & district == "KAJIADO EAST" ~ "KAJIADO EAST",
        country == "KENYA" & province == "KAJIADO" & district == "KAJIADO NORTH" ~ "KAJIADO NORTH",
        country == "KENYA" & province == "KITUI" & district == "KITUI CENTAL" ~ "KITUI CENTRAL",
        country == "KENYA" & province == "MANDERA" & district == "KOTULO" ~ "KUTULO",
        
        # ============================================================
        # UGANDA DISTRICT CORRECTIONS
        # ============================================================
        country == "UGANDA" & district == "ADJUMANI district" ~ "ADJUMANI",
        country == "UGANDA" & district == "SOROTI DISTRICT" ~ "SOROTI",
        country == "UGANDA" & district == "ARUA CITY" ~ "ARUA",
        country == "UGANDA" & district == "ARUA district" ~ "ARUA",
        country == "UGANDA" & district == "KOBOKO district" ~ "KOBOKO",
        country == "UGANDA" & district == "MADI-OKOLLO district" ~ "MADI-OKOLLO",
        country == "UGANDA" & district == "MARACHA district" ~ "MARACHA",
        country == "UGANDA" & district == "MOYO district" ~ "MOYO",
        country == "UGANDA" & district == "TEREGO" ~ "TEREGO",
        country == "UGANDA" & district == "ZOMBO district" ~ "ZOMBO",
        country == "UGANDA" & district == "BUIKWE district" ~ "BUIKWE",
        country == "UGANDA" & district == "BUTAMBALA district" ~ "BUTAMBALA",
        country == "UGANDA" & district == "BUVUMA district" ~ "BUVUMA",
        country == "UGANDA" & district == "GOMBA district" ~ "GOMBA",
        country == "UGANDA" & district == "KAYUNGA district" ~ "KAYUNGA",
        country == "UGANDA" & district == "AMURU district" ~ "AMURU",
        country == "UGANDA" & district == "GULU CITY" ~ "GULU",
        country == "UGANDA" & district == "KITGUM district" ~ "KITGUM",
        country == "UGANDA" & district == "LAMWO district" ~ "LAMWO",
        country == "UGANDA" & district == "NWOYA district" ~ "NWOYA",
        country == "UGANDA" & district == "BULIISA district" ~ "BULIISA",
        country == "UGANDA" & district == "HOIMA CITY" ~ "HOIMA",
        country == "UGANDA" & district == "HOIMA district" ~ "HOIMA",
        country == "UGANDA" & district == "KAKUMIRO district" ~ "KAKUMIRO",
        country == "UGANDA" & district == "KIKUUBE district" ~ "KIKUUBE",
        country == "UGANDA" & district == "MASINDI district" ~ "MASINDI",
        country == "UGANDA" & district == "BUGWERI district" ~ "BUGWERI",
        country == "UGANDA" & district == "BUYENDE district" ~ "BUYENDE",
        country == "UGANDA" & district == "IGANGA district" ~ "IGANGA",
        country == "UGANDA" & district == "JINJA CITY" ~ "JINJA",
        country == "UGANDA" & district == "JINJA district" ~ "JINJA",
        country == "UGANDA" & district == "KAMULI district" ~ "KAMULI",
        country == "UGANDA" & district == "LUUKA district" ~ "LUUKA",
        country == "UGANDA" & district == "NAMAYINGO district" ~ "NAMAYINGO",
        country == "UGANDA" & district == "NAMUTUMBA district" ~ "NAMUTUMBA",
        country == "UGANDA" & district == "KABALE district" ~ "KABALE",
        country == "UGANDA" & district == "KANUNGU district" ~ "KANUNGU",
        country == "UGANDA" & district == "KISORO district" ~ "KISORO",
        country == "UGANDA" & district == "RUBANDA district" ~ "RUBANDA",
        country == "UGANDA" & district == "RUKUNGIRI district" ~ "RUKUNGIRI",
        country == "UGANDA" & district == "BUNDIBUGYO district" ~ "BUNDIBUGYO",
        country == "UGANDA" & district == "BUNYANGABU district" ~ "BUNYANGABU",
        country == "UGANDA" & district == "FORT PORTAL CITY" ~ "FORT PORTAL",
        country == "UGANDA" & district == "KABAROLE district" ~ "KABAROLE",
        country == "UGANDA" & district == "KASESE district" ~ "KASESE",
        country == "UGANDA" & district == "KITAGWENDA district" ~ "KITAGWENDA",
        country == "UGANDA" & district == "KYEGEGWA district" ~ "KYEGEGWA",
        country == "UGANDA" & district == "KYENJOJO district" ~ "KYENJOJO",
        country == "UGANDA" & district == "NTOROKO district" ~ "NTOROKO",
        country == "UGANDA" & district == "ALEBTONG district" ~ "ALEBTONG",
        country == "UGANDA" & district == "APAC district" ~ "APAC",
        country == "UGANDA" & district == "KOLE district" ~ "KOLE",
        country == "UGANDA" & district == "KWANIA district" ~ "KWANIA",
        country == "UGANDA" & district == "LIRA CITY" ~ "LIRA",
        country == "UGANDA" & district == "LIRA district" ~ "LIRA",
        country == "UGANDA" & district == "OTUKE district" ~ "OTUKE",
        country == "UGANDA" & district == "BUKOMANSIMBI district" ~ "BUKOMANSIMBI",
        country == "UGANDA" & district == "KALANGALA district" ~ "KALANGALA",
        country == "UGANDA" & district == "KYOTERA district" ~ "KYOTERA",
        country == "UGANDA" & district == "LYANTONDE district" ~ "LYANTONDE",
        country == "UGANDA" & district == "MASAKA CITY" ~ "MASAKA",
        country == "UGANDA" & district == "MASAKA district" ~ "MASAKA",
        country == "UGANDA" & district == "SSEMBABAULE" ~ "SSEMBABAULE",
        country == "UGANDA" & district == "BUDUDA district" ~ "BUDUDA",
        country == "UGANDA" & district == "BULAMBULI district" ~ "BULAMBULI",
        country == "UGANDA" & district == "BUTEBO district" ~ "BUTEBO",
        country == "UGANDA" & district == "KAPCHORWA district" ~ "KAPCHORWA",
        country == "UGANDA" & district == "KIBUKU district" ~ "KIBUKU",
        country == "UGANDA" & district == "MBALE CITY" ~ "MBALE",
        country == "UGANDA" & district == "MBALE district" ~ "MBALE",
        country == "UGANDA" & district == "TORORO district" ~ "TORORO",
        country == "UGANDA" & district == "BUHWEJU district" ~ "BUHWEJU",
        country == "UGANDA" & district == "IBANDA district" ~ "IBANDA",
        country == "UGANDA" & district == "ISINGIRO district" ~ "ISINGIRO",
        country == "UGANDA" & district == "KAZO district" ~ "KAZO",
        country == "UGANDA" & district == "KIRUHURA district" ~ "KIRUHURA",
        country == "UGANDA" & district == "MBARARA CITY" ~ "MBARARA",
        country == "UGANDA" & district == "MBARARA district" ~ "MBARARA",
        country == "UGANDA" & district == "MITOOMA district" ~ "MITOOMA",
        country == "UGANDA" & district == "NTUNGAMO district" ~ "NTUNGAMO",
        country == "UGANDA" & district == "RUBIRIZI district" ~ "RUBIRIZI",
        country == "UGANDA" & district == "RWAMPARA district" ~ "RWAMPARA",
        country == "UGANDA" & district == "SHEEMA district" ~ "SHEEMA",
        country == "UGANDA" & district == "ABIM district" ~ "ABIM",
        country == "UGANDA" & district == "AMUDAT district" ~ "AMUDAT",
        country == "UGANDA" & district == "KARENGA district" ~ "KARENGA",
        country == "UGANDA" & district == "KOTIDO district" ~ "KOTIDO",
        country == "UGANDA" & district == "MOROTO district" ~ "MOROTO",
        country == "UGANDA" & district == "NABILATUK district" ~ "NABILATUK",
        country == "UGANDA" & district == "NAKAPIRIPIRIT district" ~ "NAKAPIRIPIRIT",
        country == "UGANDA" & district == "NAPAK district" ~ "NAPAK",
        country == "UGANDA" & district == "KIBOGA district" ~ "KIBOGA",
        country == "UGANDA" & district == "KYANKWANZI district" ~ "KYANKWANZI",
        country == "UGANDA" & district == "LUWERO district" ~ "LUWERO",
        country == "UGANDA" & district == "NAKASEKE district" ~ "NAKASEKE",
        country == "UGANDA" & district == "NAKASONGOLA district" ~ "NAKASONGOLA",
        country == "UGANDA" & district == "AMURIA district" ~ "AMURIA",
        country == "UGANDA" & district == "BUKEDEA district" ~ "BUKEDEA",
        country == "UGANDA" & district == "KABERAMAIDO district" ~ "KABERAMAIDO",
        country == "UGANDA" & district == "KALAKI district" ~ "KALAKI",
        country == "UGANDA" & district == "KATAKWI district" ~ "KATAKWI",
        country == "UGANDA" & district == "KUMI district" ~ "KUMI",
        country == "UGANDA" & district == "NGORA district" ~ "NGORA",
        country == "UGANDA" & district == "SERERE district" ~ "SERERE",
        country == "UGANDA" & district == "SOROTI CITY" ~ "SOROTI",
        country == "UGANDA" & district == "SOROTI district" ~ "SOROTI",
        country == "UGANDA" & district == "CENTRAL DIVISION" ~ "CENTRAL",
        country == "UGANDA" & district == "ENTEBBE DIVISION" ~ "ENTEBBE",
        country == "UGANDA" & district == "KAWEMPE DIVISION" ~ "KAWEMPE",
        country == "UGANDA" & district == "MAKINDYE DIVISION" ~ "MAKINDYE",
        country == "UGANDA" & district == "MUKONO district" ~ "MUKONO",
        country == "UGANDA" & district == "NAKAWA DIVISION" ~ "NAKAWA",
        country == "UGANDA" & district == "RUBAGA DIVISION" ~ "RUBAGA",
        country == "UGANDA" & district == "WAKISO district" ~ "WAKISO",
        country == "UGANDA" & district == "KASSANDA district" ~ "KASSANDA",
        country == "UGANDA" & district == "BUTALEJA district" ~ "BUTALEJA",
        country == "UGANDA" & district == "KAMPALA district" ~ "KAMPALA",
        country == "UGANDA" & district == "KIRYANDONGO district" ~ "KIRYANDONGO",
        country == "UGANDA" & district == "BUKWO district" ~ "BUKWO",
        country == "UGANDA" & district == "BUDAKA district" ~ "BUDAKA",
        country == "UGANDA" & district == "KALUNGU district" ~ "KALUNGU",
        country == "UGANDA" & district == "NEBBI district" ~ "NEBBI",
        country == "UGANDA" & district == "YUMBE district" ~ "YUMBE",
        country == "UGANDA" & district == "MPIGI district" ~ "MPIGI",
        country == "UGANDA" & district == "OMORO district" ~ "OMORO",
        country == "UGANDA" & district == "PADER district" ~ "PADER",
        country == "UGANDA" & district == "KAGADI district" ~ "KAGADI",
        country == "UGANDA" & district == "KIBAALE district" ~ "KIBAALE",
        country == "UGANDA" & district == "KALIRO district" ~ "KALIRO",
        country == "UGANDA" & district == "MAYUGE district" ~ "MAYUGE",
        country == "UGANDA" & district == "RUKIGA district" ~ "RUKIGA",
        country == "UGANDA" & district == "KAMWENGE district" ~ "KAMWENGE",
        country == "UGANDA" & district == "AMOLATAR district" ~ "AMOLATAR",
        country == "UGANDA" & district == "DOKOLO district" ~ "DOKOLO",
        country == "UGANDA" & district == "OYAM district" ~ "OYAM",
        country == "UGANDA" & district == "LWENGO district" ~ "LWENGO",
        country == "UGANDA" & district == "RAKAI district" ~ "RAKAI",
        country == "UGANDA" & district == "SEMBABULE district" ~ "SEMBABULE",
        country == "UGANDA" & district == "BUSIA district" ~ "BUSIA",
        country == "UGANDA" & district == "KWEEN district" ~ "KWEEN",
        country == "UGANDA" & district == "MANAFWA district" ~ "MANAFWA",
        country == "UGANDA" & district == "NAMISINDWA district" ~ "NAMISINDWA",
        country == "UGANDA" & district == "PALLISA district" ~ "PALLISA",
        country == "UGANDA" & district == "SIRONKO district" ~ "SIRONKO",
        country == "UGANDA" & district == "BUSHENYI district" ~ "BUSHENYI",
        country == "UGANDA" & district == "KAPELEBYONG district" ~ "KAPELEBYONG",
        country == "UGANDA" & district == "MADI-OKOLLO" ~ "MADI OKOLLO",
        country == "UGANDA" & district == "PAKWACH district" ~ "PAKWACH",
        country == "UGANDA" & district == "GULU district" ~ "GULU",
        country == "UGANDA" & district == "MITYANA district" ~ "MITYANA",
        country == "UGANDA" & district == "MUBENDE district" ~ "MUBENDE",
        country == "UGANDA" & district == "OBONGI district" ~ "OBONGI",
        country == "UGANDA" & district == "BUGIRI district" ~ "BUGIRI",
        country == "UGANDA" & district == "KAABONG district" ~ "KAABONG",
        country == "UGANDA" & district == "SSEMBABAULE" ~ "SEMBABULE",
        country == "UGANDA" & district == "KYANKWANZI" ~ "KYAKWANZI",
        country == "UGANDA" & district == "AGAGO district" ~ "AGAGO",
        country == "UGANDA" & district == "KASSANDA" ~ "KASANDA",
        
        # ============================================================
        # ZAMBIA DISTRICT CORRECTIONS
        # ============================================================
        country == "ZAMBIA" & district == "LAVUSHI" ~ "LAVUSHI MANDA",
        
        # ============================================================
        # NIGERIA DISTRICT CORRECTIONS
        # ============================================================
        country == "NIGERIA" & district == "YEWA SOUTH" ~ "EGBADO SOUTH",
        country == "NIGERIA" & district == "YEWA NORTH" ~ "EGBADO NORTH",
        country == "NIGERIA" & district == "BURUKU" ~ "BUKURU",
        country == "NIGERIA" & district == "ONUIMO" ~ "UNUIMO",
        country == "NIGERIA" & district == "MUNYA" ~ "MUYA",
        country == "NIGERIA" & district == "AYEDADE" ~ "AIYEDADE",
        country == "NIGERIA" & district == "AYEDIRE" ~ "AIYEDIRE",
        country == "NIGERIA" & district == "GIREI" ~ "GIRIE",
        country == "NIGERIA" & district == "TOUNGO" ~ "TEUNGO",
        country == "NIGERIA" & district == "KIRI KASAMMA" ~ "KIRI KASAMA",
        country == "NIGERIA" & district == "LAMURDE" ~ "LARMURDE",
        country == "NIGERIA" & district == "BIRNIWA" ~ "BIRNIN KUDU",
        country == "NIGERIA" & district == "MALAM MADORI" ~ "MALAM MADURI",
        country == "NIGERIA" & district == "SULE TANKARKAR" ~ "SULE TANKAKAR",
        country == "NIGERIA" & district == "KUBAU" ~ "KUBAN",
        country == "NIGERIA" & district == "UNGOGO" ~ "UNGONGO",
        country == "NIGERIA" & district == "WAMAKKO" ~ "WAMAKO",
        country == "NIGERIA" & district == "BADE" ~ "BARDE",
        country == "NIGERIA" & district == "BURSARI" ~ "BORSARI",
        country == "NIGERIA" & district == "TARMUWA" ~ "TARMUA",
        
        # ============================================================
        # ZIMBABWE DISTRICT CORRECTIONS
        # ============================================================
        country == "ZIMBABWE" & district == "MUTARE CITY" ~ "MUTARE",
        country == "ZIMBABWE" & district == "MT DARWIN" ~ "MOUNT DARWIN",
        country == "ZIMBABWE" & district == "MHONDORO" ~ "MHONDORO NGEZI",
        country == "ZIMBABWE" & district == "MUREWA" ~ "MUREHWA",
        
        # Keep original if no match
        TRUE ~ district
      )
    )
}

# ============================================================
# FUNCTION: APPLY PROVINCE MAPPINGS
# ============================================================

apply_province_mappings <- function(data) {
  data %>%
    mutate(
      province = case_when(
        # ============================================================
        # MOZAMBIQUE PROVINCE CORRECTIONS
        # ============================================================
        country == "MOZAMBIQUE" & province == "CABO DELGADO" & district == "MOCIMBOA DA PRAIA" ~ "MOCÍMBOA DA PRAIA",
        country == "MOZAMBIQUE" & province == "NIASSA" & district == "CIDADE DE LICHINGA" ~ "DISTRITO DE LICHINGA",
        country == "MOZAMBIQUE" & province == "NIASSA" & district == "N'GAUMA" ~ "NGAÚMA",
        country == "MOZAMBIQUE" & province == "NIASSA" & district == "NGAUMA" ~ "NGAÚMA",
        country == "MOZAMBIQUE" & province == "GAZA" & district == "GUIJA" ~ "GUIJÁ",
        country == "MOZAMBIQUE" & province == "GAZA" & district == "MANDLACAZE" ~ "MANDLAKAZI",
        country == "MOZAMBIQUE" & province == "PROVINCIA DE NAMPULA" & district == "CIDADE DE NAMPULA" ~ "DISTRITO DE NAMPULA",
        country == "MOZAMBIQUE" & province == "PROVINCIA DE SOFALA" & district == "MARINGUE" ~ "MARÍNGUE",
        country == "MOZAMBIQUE" & province == "INHAMBANE" & district == "VILANKULO" ~ "VILANKULOS",
        country == "MOZAMBIQUE" & province == "SOFALA" & district == "MARÃNGUE" ~ "MARÍNGUE",
        country == "MOZAMBIQUE" & province == "GAZA" & district == "GUIJÃ" ~ "GUIJÁ",
        country == "MOZAMBIQUE" & province == "NIASSA" & district == "NGAÃŠMA" ~ "NGAÚMA",
        
        # ============================================================
        # ZAMBIA PROVINCE CORRECTIONS
        # ============================================================
        country == "ZAMBIA" & province == "CENTRAL" & district == "KAPIRI" ~ "KAPIRI-MPOSHI",
        country == "ZAMBIA" & province == "NORTH WESTERN" & district == "IKELENGI" ~ "IKELENGE",
        country == "ZAMBIA" & province == "NORTH WESTERN" & district == "MUSHINDAMO" ~ "MUSHINDANO",
        country == "ZAMBIA" & province == "MUCHINGA" & district == "KANCHIBYA" ~ "KANCHIBIYA",
        country == "ZAMBIA" & province == "MUCHINGA" & district == "SHIWANGANDU" ~ "SHIWANG'ANDU",
        
        # ============================================================
        # UGANDA PROVINCE CORRECTIONS
        # ============================================================
        country == "UGANDA" & province == "KAMPALA CITY AUTHORITY" & district == "KAMPALA CITY AUTHORITY" ~ "KAMPALA",
        country == "UGANDA" & province == "SSEMBABULE" & district == "SSEMBABULE" ~ "SEMBABULE",
        
        # ============================================================
        # ANGOLA PROVINCE CORRECTIONS
        # ============================================================
        country == "ANGOLA" & province == "BENGUELA" & district == "BAÍA FARTA" ~ "BAIA FARTA",
        country == "ANGOLA" & province == "CABINDA" & district == "BUCO-ZAU" ~ "BUCO ZAU",
        country == "ANGOLA" & province == "CUANZA NORTE" & district == "SAMBA CAJÚ" ~ "SAMBA CAJU",
        country == "ANGOLA" & province == "HUAMBO" & district == "CAÁLA" ~ "CAALA",
        country == "ANGOLA" & province == "HUAMBO" & district == "CATCHIUNGO (EX-BELA VISTA)" ~ "KATCHIUNGO",
        country == "ANGOLA" & province == "HUAMBO" & district == "CHICALA-CHOLOANGA (EX-VILA NOVA)" ~ "TCHIKALA TCHOLOHANGA",
        country == "ANGOLA" & province == "HUAMBO" & district == "CHINJENJE" ~ "TCHINJENJE",
        country == "ANGOLA" & province == "HUAMBO" & district == "UCUMA (EX-CUMA)" ~ "UKUMA",
        country == "ANGOLA" & province == "LUANDA" & district == "ÍCOLO E BENGO" ~ "ICOLO E BENGO",
        country == "ANGOLA" & province == "LUNDA-NORTE" & district == "LÓVUA" ~ "LOVUA",
        country == "ANGOLA" & province == "LUNDA-NORTE" & district == "XÁ MUTEBA" ~ "XA MUTEBA",
        country == "ANGOLA" & province == "MALANJE" & district == "KIWABA NZOJI" ~ "KIWABA NZOGI",
        country == "ANGOLA" & province == "MALANJE" & district == "KUNDA-DYA-BASE" ~ "KUNDA-DIA-BAZE",
        country == "ANGOLA" & province == "MOXICO" & district == "LÉUA" ~ "LÉUA",
        country == "ANGOLA" & province == "MOXICO" & district == "LUMEJE CAMEIA" ~ "LUMEJE CAMEIA",
        country == "ANGOLA" & province == "NAMIBE" & district == "TÔMBUA" ~ "TOMBUA",
        country == "ANGOLA" & province == "UÍGE" & district == "ALTO CAUALE (CANGOLA)" ~ "CANGOLA",
        country == "ANGOLA" & province == "UÍGE" & district == "UÍGE" ~ "UIGE",
        country == "ANGOLA" & province == "ZAIRE" & district == "MBANZA KONGO" ~ "MBANZA CONGO",
        
        # ============================================================
        # DRC PROVINCE CORRECTIONS
        # ============================================================
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "HAUT KATANGA" & district == "KAMALONDA" ~ "KAMALONDO",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "EQUATEUR" & district == "INGENGE" ~ "INGENDE",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "EQUATEUR" & district == "LILANGA BOGANGI" ~ "LILANGA BOBANGI",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "ITURI" & district == "BAMBU MINE" ~ "BAMBU-MINES",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KINSHASA" & district == "MASANI I" ~ "MASINA I",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KONGO CENTRAL" & district == "MANGAMBO" ~ "MANGEMBO",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KWANGO" & district == "POKOKABAKA" ~ "POPOKABAKA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KWILU" & district == "KINKONGO" ~ "KIKONGO",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "NORD KIVU" & district == "NYRIRAGONGO" ~ "NYIRAGONGO",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "NORD UBANGI" & district == "BILI 2" ~ "BILI2",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "SANKURU" & district == "DIJALO-NDJEKA" ~ "DJALO-NDJEKA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "SUD-KIVU" & district == "KAHELE" ~ "KALEHE",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "SUD-KIVU" & district == "HAUTS PLATEAU UVIRA" ~ "HAUTS PLATEAUX UVIRA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KINSHASA" & district == "BIYALA" ~ "BIYELA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "EQUATEUR" & district == "MANKANZA" ~ "MAKANZA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "ITURI" & district == "BAMBU MINES" ~ "BAMBU-MINES",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KASAI CENTRAL" & district == "LUBUNGA KOC" ~ "LUBUNGA2",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KASAI ORIENTAL" & district == "KABEYA KAMUANGA" ~ "KABEYA KAMWANGA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KASAI" & district == "NDJOKU PUNDA" ~ "NDJOKO PUNDA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KINSHASA" & district == "BINZA-MÉTÉO" ~ "BINZA-METEO",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KINSHASA" & district == "LIMETÉ" ~ "LIMETE",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KONGO CENTRAL" & district == "NSONA MPAGNU" ~ "NSONA-PANGU",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "LOMAMI" & district == "KALAMBAYI KAB" ~ "KALAMBAYI KABANGA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "NORD KIVU" & district == "MANGUREJIPA" ~ "MANGUREDJIPA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "NORD KIVU" & district == "NYIRANGONGO" ~ "NYIRAGONGO",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "SUD KIVU" & district == "HAUTS PLATEAUX D'UVIRA" ~ "HAUTS PLATEAUX UVIRA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "SUD KIVU" & district == "MITI - MURHESA" ~ "MITI-MURRHESA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "TSHOPO" & district == "MAKISO KISANGANI" ~ "MAKISO-KISANGANI",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "KASAI ORIENTAL" & district == "LUKELENGE" ~ "LUKALENGE",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "MONGALA" & district == "BOSOMONDANDA" ~ "BOSOMODANDA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "NORD KIVU" & district == "KIRISIMBI" ~ "KARISIMBI",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "SANKURU" & district == "VANGAKETE" ~ "VANGA-KETE",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "SUD KIVU" & district == "HAUT PLATEAUX D'UVIRA" ~ "HAUTS PLATEAUX UVIRA",
        country == "DEMOCRATIC REPUBLIC OF THE CONGO" & province == "TSHOPO" & district == "WANIERUKULA" ~ "WANIE-RUKULA",
        
        # ============================================================
        # BENIN PROVINCE CORRECTIONS
        # ============================================================
        country == "BENIN" & province == "ATLANTIQUE" & district == "ABOMEY-CALAVI 1" ~ "ATLANTIQUE",
        country == "BENIN" & province == "ATLANTIQUE" & district == "ABOMEY-CALAVI 2" ~ "ATLANTIQUE",
        country == "BENIN" & province == "ATLANTIQUE" & district == "ABOMEY-CALAVI 3" ~ "ATLANTIQUE",
        country == "BENIN" & province == "ATLANTIQUE" & district == "SO-AVA" ~ "ATLANTIQUE",
        country == "BENIN" & province == "ATLANTIQUE" & district == "TOFFO" ~ "ATLANTIQUE",
        country == "BENIN" & province == "BORGOU" & district == "BEMBEREKE" ~ "BORGOU",
        country == "BENIN" & province == "BORGOU" & district == "NIKKI" ~ "BORGOU",
        country == "BENIN" & province == "BORGOU" & district == "PARAKOU" ~ "BORGOU",
        country == "BENIN" & province == "BORGOU" & district == "PERERE" ~ "BORGOU",
        country == "BENIN" & province == "BORGOU" & district == "TCHAOUROU" ~ "BORGOU",
        country == "BENIN" & province == "DONGA" & district == "DJOUGOU" ~ "DONGA",
        country == "BENIN" & province == "LITTORAL" & district == "COTONOU 1" ~ "LITTORAL",
        country == "BENIN" & province == "LITTORAL" & district == "COTONOU 2" ~ "LITTORAL",
        country == "BENIN" & province == "LITTORAL" & district == "COTONOU 3" ~ "LITTORAL",
        country == "BENIN" & province == "LITTORAL" & district == "COTONOU 4" ~ "LITTORAL",
        country == "BENIN" & province == "LITTORAL" & district == "COTONOU 5" ~ "LITTORAL",
        country == "BENIN" & province == "LITTORAL" & district == "COTONOU 6" ~ "LITTORAL",
        country == "BENIN" & province == "OUEME" & district == "AGUEGUES" ~ "OUEME",
        country == "BENIN" & province == "OUEME" & district == "PORTO-NOVO 1" ~ "OUEME",
        country == "BENIN" & province == "OUEME" & district == "PORTO-NOVO 2" ~ "OUEME",
        country == "BENIN" & province == "OUEME" & district == "PORTO-NOVO 3" ~ "OUEME",
        country == "BENIN" & province == "OUEME" & district == "SEME-KPODJI" ~ "OUEME",
        
        # ============================================================
        # COTE D IVOIRE PROVINCE CORRECTIONS
        # ============================================================
        country == "COTE D IVOIRE" & province == "CAVALLY" & district == "GUIGLO" ~ "CAVALLY",
        country == "COTE D IVOIRE" & province == "ABIDJAN1" & district == "ABOBO OUEST" ~ "ABIDJAN 1",
        country == "COTE D IVOIRE" & province == "ABIDJAN1" & district == "ANYAMA" ~ "ABIDJAN 1",
        country == "COTE D IVOIRE" & province == "ABIDJAN1" & district == "YOPOUGON EST" ~ "ABIDJAN 1",
        country == "COTE D IVOIRE" & province == "ABIDJAN1" & district == "YOPOUGON OUEST-SONGON" ~ "ABIDJAN 1",
        country == "COTE D IVOIRE" & province == "ABIDJAN2" & district == "COCODY-BINGERVILLE" ~ "ABIDJAN 2",
        country == "COTE D IVOIRE" & province == "ABIDJAN2" & district == "KOUMASSI" ~ "ABIDJAN 2",
        country == "COTE D IVOIRE" & province == "ABIDJAN2" & district == "PORT BOUET-VRIDI" ~ "ABIDJAN 2",
        country == "COTE D IVOIRE" & province == "ABIDJAN2" & district == "TREICHVILLE-MARCORY" ~ "ABIDJAN 2",
        country == "COTE D IVOIRE" & province == "AGNEBY_TIASSA" & district == "AGBOVILLE" ~ "AGNEBY-TIASSA",
        country == "COTE D IVOIRE" & province == "AGNEBY_TIASSA" & district == "TIASSALE" ~ "AGNEBY-TIASSA",
        country == "COTE D IVOIRE" & province == "BAFING" & district == "KORO" ~ "BAFING",
        country == "COTE D IVOIRE" & province == "BAFING" & district == "TOUBA" ~ "BAFING",
        country == "COTE D IVOIRE" & province == "BAGOUE" & district == "BOUNDIALI" ~ "BAGOUE",
        country == "COTE D IVOIRE" & province == "BAGOUE" & district == "KOUTO" ~ "BAGOUE",
        country == "COTE D IVOIRE" & province == "BAGOUE" & district == "TENGRELA" ~ "BAGOUE",
        country == "COTE D IVOIRE" & province == "BELIER" & district == "DIDIEVI" ~ "BELIER",
        country == "COTE D IVOIRE" & province == "BELIER" & district == "TIEBISSOU" ~ "BELIER",
        country == "COTE D IVOIRE" & province == "BELIER" & district == "TOUMODI" ~ "BELIER",
        country == "COTE D IVOIRE" & province == "BELIER" & district == "YAMOUSSOUKRO" ~ "BELIER",
        country == "COTE D IVOIRE" & province == "BERE" & district == "DIANRA" ~ "BERE",
        country == "COTE D IVOIRE" & province == "BERE" & district == "KOUNAHIRI" ~ "BERE",
        country == "COTE D IVOIRE" & province == "BERE" & district == "MANKONO" ~ "BERE",
        country == "COTE D IVOIRE" & province == "BOUKANI" & district == "BOUNA" ~ "BOUNKANI",
        country == "COTE D IVOIRE" & province == "BOUKANI" & district == "DOROPO" ~ "BOUNKANI",
        country == "COTE D IVOIRE" & province == "BOUKANI" & district == "NASSIAN" ~ "BOUNKANI",
        country == "COTE D IVOIRE" & province == "BOUKANI" & district == "TEHINI" ~ "BOUNKANI",
        country == "COTE D IVOIRE" & province == "CAVALLY" & district == "BLOLEQUIN" ~ "CAVALLY",
        country == "COTE D IVOIRE" & province == "CAVALLY" & district == "DUEKOUE" ~ "MOYEN CAVALLY",
        country == "COTE D IVOIRE" & province == "CAVALLY" & district == "TAI" ~ "CAVALLY",
        country == "COTE D IVOIRE" & province == "CAVALLY" & district == "TOULEPLEU" ~ "CAVALLY",
        country == "COTE D IVOIRE" & province == "FOLON" & district == "KANIASSO" ~ "FOLON",
        country == "COTE D IVOIRE" & province == "FOLON" & district == "MINIGNAN" ~ "FOLON",
        country == "COTE D IVOIRE" & province == "GBEKE" & district == "BEOUMI" ~ "GBEKE",
        country == "COTE D IVOIRE" & province == "GBEKE" & district == "BOTRO" ~ "GBEKE",
        country == "COTE D IVOIRE" & province == "GBEKE" & district == "BOUAKE NORD-EST" ~ "GBEKE",
        country == "COTE D IVOIRE" & province == "GBEKE" & district == "BOUAKE NORD-OUEST" ~ "GBEKE",
        country == "COTE D IVOIRE" & province == "GBEKE" & district == "BOUAKE SUD" ~ "GBEKE",
        country == "COTE D IVOIRE" & province == "GBEKE" & district == "SAKASSOU" ~ "GBEKE",
        country == "COTE D IVOIRE" & province == "GBOKLE" & district == "SASSANDRA" ~ "GBOKLE",
        country == "COTE D IVOIRE" & province == "GOH" & district == "OUME" ~ "GOH",
        country == "COTE D IVOIRE" & province == "GONTOUGO" & district == "BONDOUKOU" ~ "GONTOUGO",
        country == "COTE D IVOIRE" & province == "GONTOUGO" & district == "KOUN-FAO" ~ "GONTOUGO",
        country == "COTE D IVOIRE" & province == "GONTOUGO" & district == "SANDEGUE" ~ "GONTOUGO",
        country == "COTE D IVOIRE" & province == "GONTOUGO" & district == "TANDA" ~ "GONTOUGO",
        country == "COTE D IVOIRE" & province == "GONTOUGO" & district == "TRANSUA" ~ "GONTOUGO",
        country == "COTE D IVOIRE" & province == "GRANDS_PONTS" & district == "DABOU" ~ "GRANDS PONTS",
        country == "COTE D IVOIRE" & province == "GRANDS_PONTS" & district == "GRAND-LAHOU" ~ "GRANDS PONTS",
        country == "COTE D IVOIRE" & province == "GRANDS_PONTS" & district == "JACQUEVILLE" ~ "GRANDS PONTS",
        country == "COTE D IVOIRE" & province == "GUEMON" & district == "BANGOLO" ~ "GUEMON",
        country == "COTE D IVOIRE" & province == "GUEMON" & district == "KOUIBLY" ~ "GUEMON",
        country == "COTE D IVOIRE" & province == "HAMBOL" & district == "DABAKALA" ~ "HAMBOL",
        country == "COTE D IVOIRE" & province == "HAMBOL" & district == "KATIOLA" ~ "HAMBOL",
        country == "COTE D IVOIRE" & province == "HAMBOL" & district == "NIAKARAMADOUGOU" ~ "HAMBOL",
        country == "COTE D IVOIRE" & province == "HAUT_SASSANDRA" & district == "DALOA" ~ "HAUT SASSANDRA",
        country == "COTE D IVOIRE" & province == "HAUT_SASSANDRA" & district == "ISSIA" ~ "HAUT SASSANDRA",
        country == "COTE D IVOIRE" & province == "HAUT_SASSANDRA" & district == "VAVOUA" ~ "HAUT SASSANDRA",
        country == "COTE D IVOIRE" & province == "HAUT_SASSANDRA" & district == "ZOUKOUGBEU" ~ "HAUT-SASSANDRA",
        country == "COTE D IVOIRE" & province == "IFFOU" & district == "DAOUKRO" ~ "IFOU",
        country == "COTE D IVOIRE" & province == "IFFOU" & district == "MBAHIAKRO" ~ "IFOU",
        country == "COTE D IVOIRE" & province == "IFFOU" & district == "PRIKRO" ~ "IFOU",
        country == "COTE D IVOIRE" & province == "INDENIE_DJUABLIN" & district == "AGNIBILEKROU" ~ "INDENIE-DJUABLIN",
        country == "COTE D IVOIRE" & province == "INDENIE_DJUABLIN" & district == "BETTIE" ~ "INDENIE-DJUABLIN",
        country == "COTE D IVOIRE" & province == "KABADOUGOU" & district == "MADINANI" ~ "KABADOUGOU",
        country == "COTE D IVOIRE" & province == "KABADOUGOU" & district == "ODIENNE" ~ "KABADOUGOU",
        country == "COTE D IVOIRE" & province == "LAME" & district == "YAKASSE-ATTOBROU" ~ "ME",
        country == "COTE D IVOIRE" & province == "LOH_DJIBOUA" & district == "DIVO" ~ "LOH-DJIBOUA",
        country == "COTE D IVOIRE" & province == "LOH_DJIBOUA" & district == "GUITRY" ~ "LOH-DJIBOUA",
        country == "COTE D IVOIRE" & province == "LOH_DJIBOUA" & district == "LAKOTA" ~ "LOH-DJIBOUA",
        country == "COTE D IVOIRE" & province == "MARAHOUE" & district == "BOUAFLE" ~ "MARAHOUT",
        country == "COTE D IVOIRE" & province == "MARAHOUE" & district == "SINFRA" ~ "MARAHOUT",
        country == "COTE D IVOIRE" & province == "MARAHOUE" & district == "ZUENOULA" ~ "MARAHOUT",
        country == "COTE D IVOIRE" & province == "MORONOU" & district == "ARRAH" ~ "MORONOU",
        country == "COTE D IVOIRE" & province == "MORONOU" & district == "BONGOUANOU" ~ "MORONOU",
        country == "COTE D IVOIRE" & province == "MORONOU" & district == "MBATTO" ~ "MORONOU",
        country == "COTE D IVOIRE" & province == "NAWA" & district == "BUYO" ~ "NAWA",
        country == "COTE D IVOIRE" & province == "NAWA" & district == "GUEYO" ~ "NAWA",
        country == "COTE D IVOIRE" & province == "NAWA" & district == "MEAGUI" ~ "NAWA",
        country == "COTE D IVOIRE" & province == "NAWA" & district == "SOUBRE" ~ "NAWA",
        country == "COTE D IVOIRE" & province == "NZI" & district == "BOCANDA" ~ "NZI",
        country == "COTE D IVOIRE" & province == "NZI" & district == "DIMBOKRO" ~ "NZI",
        country == "COTE D IVOIRE" & province == "NZI" & district == "KOUASSI-KOUASSIKRO" ~ "NZI",
        country == "COTE D IVOIRE" & province == "PORO" & district == "DIKODOUGOU" ~ "PORO",
        country == "COTE D IVOIRE" & province == "PORO" & district == "KORHOGO 1" ~ "PORO",
        country == "COTE D IVOIRE" & province == "PORO" & district == "KORHOGO 2" ~ "PORO",
        country == "COTE D IVOIRE" & province == "PORO" & district == "MBENGUE" ~ "PORO",
        country == "COTE D IVOIRE" & province == "PORO" & district == "SINEMATIALI" ~ "PORO",
        country == "COTE D IVOIRE" & province == "SAN_PEDRO1" & district == "SAN PEDRO" ~ "SAN-PEDRO",
        country == "COTE D IVOIRE" & province == "SAN_PEDRO1" & district == "TABOU" ~ "SAN-PEDRO",
        country == "COTE D IVOIRE" & province == "SUD_COMOE" & district == "ABOISSO" ~ "SUD-COMOE",
        country == "COTE D IVOIRE" & province == "SUD_COMOE" & district == "GRAND-BASSAM" ~ "SUD COMOE",
        country == "COTE D IVOIRE" & province == "SUD_COMOE" & district == "TIAPOUM" ~ "SUD-COMOE",
        country == "COTE D IVOIRE" & province == "TCHOLOGO" & district == "FERKESSEDOUGOU" ~ "TCHOLOGO",
        country == "COTE D IVOIRE" & province == "TCHOLOGO" & district == "KONG" ~ "TCHOLOGO",
        country == "COTE D IVOIRE" & province == "TCHOLOGO" & district == "OUANGOLODOUGOU" ~ "TCHOLOGO",
        country == "COTE D IVOIRE" & province == "TONKPI" & district == "BIANKOUMA" ~ "TONKPI",
        country == "COTE D IVOIRE" & province == "TONKPI" & district == "DANANE" ~ "TONKPI",
        country == "COTE D IVOIRE" & province == "TONKPI" & district == "MAN" ~ "TONKPI",
        country == "COTE D IVOIRE" & province == "TONKPI" & district == "ZOUAN-HOUNIEN" ~ "TONKPI",
        country == "COTE D IVOIRE" & province == "WORODOUGOU" & district == "KANI" ~ "WORODOUGOU",
        country == "COTE D IVOIRE" & province == "WORODOUGOU" & district == "SEGUELA" ~ "WORODOUGOU",
        country == "COTE D IVOIRE" & province == "ABIDJAN1" & district == "ABOBO EST" ~ "ABIDJAN 1",
        country == "COTE D IVOIRE" & province == "GBOKLE" & district == "FRESCO" ~ "GBOKLE",
        country == "COTE D IVOIRE" & province == "GOH" & district == "GAGNOA 1" ~ "GOH",
        country == "COTE D IVOIRE" & province == "SUD_COMOE" & district == "ADIAKE" ~ "SUD-COMOE",
        country == "COTE D IVOIRE" & province == "ABIDJAN2" & district == "ADJAME-PLATEAU-ATTECOUBE" ~ "ABIDJAN 2",
        country == "COTE D IVOIRE" & province == "AGNEBY_TIASSA" & district == "SIKENSI" ~ "AGNEBY-TIASSA",
        country == "COTE D IVOIRE" & province == "BAFING" & district == "OUANINOU" ~ "BAFING",
        country == "COTE D IVOIRE" & province == "GOH" & district == "GAGNOA 2" ~ "GOH",
        country == "COTE D IVOIRE" & province == "INDENIE_DJUABLIN" & district == "ABENGOUROU" ~ "INDENIE-DJUABLIN",
        country == "COTE D IVOIRE" & province == "LAME" & district == "AKOUPE" ~ "ME",
        country == "COTE D IVOIRE" & province == "LAME" & district == "ALEPE" ~ "ME",
        country == "COTE D IVOIRE" & province == "LAME" & district == "ADZOPE" ~ "ME",
        country == "COTE D IVOIRE" & province == "ME" & district == "ADZOPE" ~ "ME",
        country == "COTE D IVOIRE" & province == "ME" & district == "AKOUPE" ~ "ME",
        country == "COTE D IVOIRE" & province == "ME" & district == "YAKASSE-ATTOBROU" ~ "ME",
        country == "COTE D IVOIRE" & province == "SAN PEDRO" & district == "TABOU" ~ "SAN-PEDRO",
        country == "COTE D IVOIRE" & province == "ME" & district == "ALEPE" ~ "ME",
        country == "COTE D IVOIRE" & province == "SAN PEDRO" & district == "SAN PEDRO" ~ "SAN-PEDRO",
        
        # ============================================================
        # GUINEA PROVINCE CORRECTIONS
        # ============================================================
        country == "GUINEA" & province == "FARANAH" & district == "DABOLA" ~ "FARANAH",
        country == "GUINEA" & province == "FARANAH" & district == "DINGUIRAYE" ~ "FARANAH",
        country == "GUINEA" & province == "FARANAH" & district == "FARANAH" ~ "FARANAH",
        country == "GUINEA" & province == "FARANAH" & district == "KISSIDOUGOU" ~ "FARANAH",
        country == "GUINEA" & province == "KANKAN" & district == "KANKAN" ~ "KANKAN",
        country == "GUINEA" & province == "KANKAN" & district == "KÉROUANE" ~ "KANKAN",
        country == "GUINEA" & province == "KANKAN" & district == "KOUROUSSA" ~ "KANKAN",
        country == "GUINEA" & province == "KANKAN" & district == "MANDIANA" ~ "KANKAN",
        country == "GUINEA" & province == "KANKAN" & district == "SIGUIRI" ~ "KANKAN",
        country == "GUINEA" & province == "LABE" & district == "KOUBIA" ~ "LABE",
        country == "GUINEA" & province == "LABE" & district == "MALI" ~ "LABE",
        country == "GUINEA" & province == "LABE" & district == "TOUGUÉ" ~ "LABE",
        country == "GUINEA" & province == "NZEREKORE" & district == "BEYLA" ~ "NZEREKORE",
        country == "GUINEA" & province == "NZEREKORE" & district == "GUECKÉDOU" ~ "NZEREKORE",
        country == "GUINEA" & province == "NZEREKORE" & district == "LOLA" ~ "NZEREKORE",
        country == "GUINEA" & province == "NZEREKORE" & district == "MACENTA" ~ "NZEREKORE",
        country == "GUINEA" & province == "NZEREKORE" & district == "N'ZÉRÉKORÉ" ~ "NZEREKORE",
        country == "GUINEA" & province == "NZEREKORE" & district == "YOMOU" ~ "NZEREKORE",
        country == "GUINEA" & province == "LABE" & district == "LABÉ" ~ "LABE",
        country == "GUINEA" & province == "BOKE" & district == "BOFFA" ~ "BOKE",
        country == "GUINEA" & province == "BOKE" & district == "BOKÉ" ~ "BOKE",
        country == "GUINEA" & province == "BOKE" & district == "FRIA" ~ "BOKE",
        country == "GUINEA" & province == "BOKE" & district == "GAOUAL" ~ "BOKE",
        country == "GUINEA" & province == "BOKE" & district == "KOUNDARA" ~ "BOKE",
        country == "GUINEA" & province == "CONAKRY" & district == "DIXINN" ~ "CONAKRY",
        country == "GUINEA" & province == "CONAKRY" & district == "KALOUM" ~ "CONAKRY",
        country == "GUINEA" & province == "CONAKRY" & district == "MATAM" ~ "CONAKRY",
        country == "GUINEA" & province == "CONAKRY" & district == "MATOTO" ~ "CONAKRY",
        country == "GUINEA" & province == "CONAKRY" & district == "RATOMA" ~ "CONAKRY",
        country == "GUINEA" & province == "KINDIA" & district == "COYAH" ~ "KINDIA",
        country == "GUINEA" & province == "KINDIA" & district == "DUBRÉKA" ~ "KINDIA",
        country == "GUINEA" & province == "KINDIA" & district == "FORÉCARIAH" ~ "KINDIA",
        country == "GUINEA" & province == "KINDIA" & district == "KINDIA" ~ "KINDIA",
        country == "GUINEA" & province == "KINDIA" & district == "TÉLIMÉLÉ" ~ "KINDIA",
        country == "GUINEA" & province == "LABE" & district == "LÉLOUMA" ~ "LABE",
        country == "GUINEA" & province == "MAMOU" & district == "DALABA" ~ "MAMOU",
        country == "GUINEA" & province == "MAMOU" & district == "MAMOU" ~ "MAMOU",
        country == "GUINEA" & province == "MAMOU" & district == "PITA" ~ "MAMOU",
        
        # ============================================================
        # BURKINA FASO PROVINCE CORRECTIONS
        # ============================================================
        country == "BURKINA FASO" & province == "BOUCLE_DU_MOUHOUN" ~ "DEDOUGOU",
        country == "BURKINA FASO" & province == "CENTRE-SUD" ~ "MANGA",
        country == "BURKINA FASO" & province == "CENTRE-OUEST" ~ "KOUDOUGOU",
        country == "BURKINA FASO" & province == "HAUTS-BASSINS" ~ "BOBO",
        country == "BURKINA FASO" & province == "NORD" ~ "OUAHIGOUYA",
        country == "BURKINA FASO" & province == "CENTRE" ~ "OUAGADOUGOU",
        country == "BURKINA FASO" & province == "CENTRE-NORD" ~ "KAYA",
        country == "BURKINA FASO" & province == "PLATEAU_CENTRAL" ~ "ZINIARE",
        country == "BURKINA FASO" & province == "SAHEL" ~ "DORI",
        country == "BURKINA FASO" & province == "SUD-OUEST" ~ "GAOUA",
        country == "BURKINA FASO" & province == "CENTRE-EST" ~ "TENKODOGO",
        country == "BURKINA FASO" & province == "CASCADES" ~ "BANFORA",
        country == "BURKINA FASO" & province == "EST" ~ "FADA",
        
        # ============================================================
        # MAURITANIA PROVINCE CORRECTIONS
        # ============================================================
        country == "MAURITANIA" & province == "HODH EL GHARBI" & district == "TAMCHAKET" ~ "HODH EL GHARBI",
        country == "MAURITANIA" & province == "ASSABA" & district == "GUÉRROU" ~ "ASSABA",
        country == "MAURITANIA" & province == "TIRIS ZEMMOUR" & district == "F'DERICK" ~ "TIRIS ZEMMOUR",
        country == "MAURITANIA" & province == "TRARZA" & district == "ROSSO" ~ "TRARZA",
        country == "MAURITANIA" & province == "HODH EL GHARBI" & district == "TINTANE" ~ "HODH EL GHARBI",
        country == "MAURITANIA" & province == "HODH EL CHARGUI" & district == "BASSIKNOU" ~ "HODH ECHARGUI",
        country == "MAURITANIA" & province == "TRARZA" & district == "MEDERDRA" ~ "TRARZA",
        country == "MAURITANIA" & province == "BRAKNA" & district == "ALEG" ~ "BRAKNA",
        country == "MAURITANIA" & province == "HODH EL CHARGUI" & district == "AMOURJ" ~ "HODH ECHARGUI",
        country == "MAURITANIA" & province == "HODH EL CHARGUI" & district == "DJIGUENI" ~ "HODH ECHARGUI",
        country == "MAURITANIA" & province == "NOUAKCHOTT NORD" & district == "TEYARETT" ~ "NOUAKCHOTT",
        country == "MAURITANIA" & province == "ASSABA" & district == "BARKÉOLE" ~ "ASSABA",
        country == "MAURITANIA" & province == "ASSABA" & district == "KIFFA" ~ "ASSABA",
        country == "MAURITANIA" & province == "BRAKNA" & district == "BABABÉ" ~ "BRAKNA",
        country == "MAURITANIA" & province == "BRAKNA" & district == "BOGHÉ" ~ "BRAKNA",
        country == "MAURITANIA" & province == "DAKHLET NOUADHIBOU" & district == "NOUADHIBOU" ~ "DAKHLET NOUADHIBOU",
        country == "MAURITANIA" & province == "GORGOL" & district == "KAEDI" ~ "GORGOL",
        country == "MAURITANIA" & province == "GORGOL" & district == "MAGHAMA" ~ "GORGOL",
        country == "MAURITANIA" & province == "GUIDIMAKHA" & district == "GHABOU" ~ "GUIODIMAKHA",
        country == "MAURITANIA" & province == "GUIDIMAKHA" & district == "OULD YENGE" ~ "GUIODIMAKHA",
        country == "MAURITANIA" & province == "GUIDIMAKHA" & district == "SELIBABY" ~ "GUIODIMAKHA",
        country == "MAURITANIA" & province == "HODH EL GHARBI" & district == "AIOUN" ~ "HODH EL GHARBI",
        country == "MAURITANIA" & province == "HODH EL CHARGUI" & district == "NÉMA" ~ "HODH ECHARGUI",
        country == "MAURITANIA" & province == "INCHIRI" & district == "AKJOUJT" ~ "INCHIRI",
        country == "MAURITANIA" & province == "NOUAKCHOTT OUEST" & district == "TEVRAGH ZEINA" ~ "NOUAKCHOTT",
        country == "MAURITANIA" & province == "NOUAKCHOTT SUD" & district == "ARAFAT" ~ "NOUAKCHOTT SUD",
        country == "MAURITANIA" & province == "NOUAKCHOTT SUD" & district == "EL MINA" ~ "NOUAKCHOTT SUD",
        country == "MAURITANIA" & province == "NOUAKCHOTT SUD" & district == "RIYAD" ~ "NOUAKCHOTT SUD",
        country == "MAURITANIA" & province == "TIRIS ZEMMOUR" & district == "BIR MOGHREN" ~ "TIRIS ZEMMOUR",
        country == "MAURITANIA" & province == "TRARZA" & district == "OUAD NAGA" ~ "TRARZA",
        country == "MAURITANIA" & province == "ADRAR" & district == "ATAR" ~ "ADRAR",
        country == "MAURITANIA" & province == "BRAKNA" & district == "MAGTA LAHJAR" ~ "BRAKNA",
        country == "MAURITANIA" & province == "DAKHLET NOUADHIBOU" & district == "CHAMI" ~ "DAKHLET NOUADHIBOU",
        country == "MAURITANIA" & province == "HODH EL GHARBI" & district == "KOBENI" ~ "HODH EL GHARBI",
        country == "MAURITANIA" & province == "NOUAKCHOTT OUEST" & district == "KSAR" ~ "NOUAKCHOTT OUEST",
        country == "MAURITANIA" & province == "TAGANT" & district == "TIDJIKJA" ~ "TAGANT",
        country == "MAURITANIA" & province == "ASSABA" & district == "BOUMDEID" ~ "ASSABA",
        country == "MAURITANIA" & province == "ADRAR" & district == "AOUJEFT" ~ "ADRAR",
        country == "MAURITANIA" & province == "ADRAR" & district == "CHINGUITTY" ~ "ADRAR",
        country == "MAURITANIA" & province == "ADRAR" & district == "OUADANE" ~ "ADRAR",
        country == "MAURITANIA" & province == "ASSABA" & district == "KANKOSSA" ~ "ASSABA",
        country == "MAURITANIA" & province == "GORGOL" & district == "LEXEIBA" ~ "GORGOL",
        country == "MAURITANIA" & province == "GORGOL" & district == "M'BOUT" ~ "GORGOL",
        country == "MAURITANIA" & province == "GORGOL" & district == "MONGUEL" ~ "GORGOL",
        country == "MAURITANIA" & province == "GUIDIMAGHA" & district == "WOMPO" ~ "GUIODIMAKHA",
        country == "MAURITANIA" & province == "HODH ECHARGHI" & district == "ADEL BEGROU" ~ "HODH ECHARGUI",
        country == "MAURITANIA" & province == "INCHIRI" & district == "BENECHABE" ~ "INCHIRI",
        country == "MAURITANIA" & province == "TAGANT" & district == "TICHIT" ~ "TAGANT",
        country == "MAURITANIA" & province == "BRAKNA" & district == "M'BAGNE" ~ "BRAKNA",
        country == "MAURITANIA" & province == "BRAKNA" & district == "MALE" ~ "BRAKNA",
        country == "MAURITANIA" & province == "TAGANT" & district == "MOUDJÉRIA" ~ "TAGANT",
        country == "MAURITANIA" & province == "TRARZA" & district == "R'KIZ" ~ "TRARZA",
        country == "MAURITANIA" & province == "HODH ECHARGHI" & district == "TIMBÉDRA" ~ "HODH ECHARGUI",
        country == "MAURITANIA" & province == "NOUAKCHOTT NORD" & district == "DAR NAIM" ~ "NOUAKCHOTT NORD",
        country == "MAURITANIA" & province == "NOUAKCHOTT NORD" & district == "TOUJOUNINE" ~ "NOUAKCHOTT",
        country == "MAURITANIA" & province == "NOUAKCHOTT OUEST" & district == "SEBKHA" ~ "NOUAKCHOTT",
        country == "MAURITANIA" & province == "TIRIS EZMMOUR" & district == "ZOUÉRAT" ~ "TIRIS ZEMMOUR",
        country == "MAURITANIA" & province == "TRARZA" & district == "BOUTILIMIT" ~ "TRARZA",
        country == "MAURITANIA" & province == "TRARZA" & district == "KEUR MACENE" ~ "TRARZA",
        country == "MAURITANIA" & province == "HODH EL GHARBI" & district == "TOUIL" ~ "HODH EL GHARBI",
        country == "MAURITANIA" & province == "TRARZA" & district == "TEIKANE" ~ "TRARZA",
        country == "MAURITANIA" & province == "HODH ECHARGHI" & district == "D'HAR" ~ "HODH ECHARGUI",
        country == "MAURITANIA" & province == "HODH ECHARGHI" & district == "OUALATA" ~ "HODH ECHARGUI",
        
        # ============================================================
        # MALI PROVINCE CORRECTIONS
        # ============================================================
        country == "MALI" & province == "GAO" & district == "ALMOUSTARAT" ~ "GAO",
        country == "MALI" & province == "GAO" & district == "ANSONGO" ~ "GAO",
        country == "MALI" & province == "GAO" & district == "BOUREM" ~ "GAO",
        country == "MALI" & province == "MENAKA" & district == "INEKAR" ~ "MENAKA",
        country == "MALI" & province == "GAO" & district == "GAO" ~ "GAO",
        country == "MALI" & province == "MENAKA" & district == "ANDERAMBOUKANE" ~ "MENAKA",
        country == "MALI" & province == "MENAKA" & district == "MENAKA" ~ "GAO",
        country == "MALI" & province == "MENAKA" & district == "TIDERMENE" ~ "MENAKA",
        country == "MALI" & province == "SEGOU" & district == "SAN" ~ "SÉGOU",
        country == "MALI" & province == "KOULIKORO" & district == "OUELESSEBOUGOU" ~ "KOULIKORO",
        country == "MALI" & province == "MOPTI" & district == "DJENNÉ" ~ "MOPTI",
        country == "MALI" & province == "SEGOU" & district == "NIONO" ~ "SÉGOU",
        country == "MALI" & province == "SEGOU" & district == "MARKALA" ~ "SÉGOU",
        country == "MALI" & province == "SIKASSO" & district == "KOLONDIEBA" ~ "SIKASSO",
        country == "MALI" & province == "KAYES" & district == "KITA" ~ "KAYES",
        country == "MALI" & province == "KAYES" & district == "SAGABARI" ~ "KAYES",
        country == "MALI" & province == "KOULIKORO" & district == "KOULIKORO" ~ "KOULIKORO",
        country == "MALI" & province == "MOPTI" & district == "DOUENTZA" ~ "MOPTI",
        country == "MALI" & province == "SIKASSO" & district == "BOUGOUNI" ~ "SIKASSO",
        country == "MALI" & province == "KAYES" & district == "KENIEBA" ~ "KAYES",
        country == "MALI" & province == "KOULIKORO" & district == "DIOILA" ~ "KOULIKORO",
        country == "MALI" & province == "KOULIKORO" & district == "FANA" ~ "KOULIKORO",
        country == "MALI" & province == "KOULIKORO" & district == "KALABANCORO" ~ "KOULIKORO",
        country == "MALI" & province == "KOULIKORO" & district == "KANGABA" ~ "KOULIKORO",
        country == "MALI" & province == "KOULIKORO" & district == "KATI" ~ "KOULIKORO",
        country == "MALI" & province == "MOPTI" & district == "BANDIAGARA" ~ "MOPTI",
        country == "MALI" & province == "MOPTI" & district == "BANKASS" ~ "MOPTI",
        country == "MALI" & province == "MOPTI" & district == "KORO" ~ "MOPTI",
        country == "MALI" & province == "MOPTI" & district == "MOPTI" ~ "MOPTI",
        country == "MALI" & province == "MOPTI" & district == "TENENKOU" ~ "MOPTI",
        country == "MALI" & province == "SEGOU" & district == "BAROUELI" ~ "SÉGOU",
        country == "MALI" & province == "SEGOU" & district == "BLA" ~ "SÉGOU",
        country == "MALI" & province == "SEGOU" & district == "MACINA" ~ "SÉGOU",
        country == "MALI" & province == "SEGOU" & district == "SEGOU" ~ "SÉGOU",
        country == "MALI" & province == "SEGOU" & district == "TOMINIAN" ~ "SÉGOU",
        country == "MALI" & province == "SIKASSO" & district == "KADIOLO" ~ "SIKASSO",
        country == "MALI" & province == "SIKASSO" & district == "KIGNAN" ~ "SIKASSO",
        country == "MALI" & province == "SIKASSO" & district == "NIENA" ~ "SIKASSO",
        country == "MALI" & province == "SIKASSO" & district == "SELINGUE" ~ "SIKASSO",
        country == "MALI" & province == "SIKASSO" & district == "YANFOLILA" ~ "SIKASSO",
        country == "MALI" & province == "SIKASSO" & district == "YOROSSO" ~ "SIKASSO",
        country == "MALI" & province == "TOMBOUCTOU" & district == "GOURMA-RHAROUS" ~ "TOMBOUCTOU",
        country == "MALI" & province == "BAMAKO" & district == "COMMUNE I" ~ "BAMAKO",
        country == "MALI" & province == "BAMAKO" & district == "COMMUNE II" ~ "BAMAKO",
        country == "MALI" & province == "BAMAKO" & district == "COMMUNE III" ~ "BAMAKO",
        country == "MALI" & province == "BAMAKO" & district == "COMMUNE IV" ~ "BAMAKO",
        country == "MALI" & province == "BAMAKO" & district == "COMMUNE V" ~ "BAMAKO",
        country == "MALI" & province == "BAMAKO" & district == "COMMUNE VI" ~ "BAMAKO",
        country == "MALI" & province == "SIKASSO" & district == "KOUTIALA" ~ "SIKASSO",
        country == "MALI" & province == "MOPTI" & district == "YOUWAROU" ~ "MOPTI",
        country == "MALI" & province == "SIKASSO" & district == "SIKASSO" ~ "SIKASSO",
        country == "MALI" & province == "KOULIKORO" & district == "BANAMBA" ~ "KOULIKORO",
        country == "MALI" & province == "KAYES" & district == "YELIMANE" ~ "KAYES",
        country == "MALI" & province == "KAYES" & district == "NIORO" ~ "KAYES",
        country == "MALI" & province == "KOULIKORO" & district == "NARA" ~ "KOULIKORO",
        country == "MALI" & province == "TOMBOUCTOU" & district == "DIRE" ~ "TOMBOUCTOU",
        country == "MALI" & province == "KAYES" & district == "BAFOULABE" ~ "KAYES",
        country == "MALI" & province == "KAYES" & district == "DIEMA" ~ "KAYES",
        country == "MALI" & province == "KAYES" & district == "OUSSOUBIDIAGNA" ~ "KAYES",
        country == "MALI" & province == "KAYES" & district == "SEFETO" ~ "KAYES",
        country == "MALI" & province == "KOULIKORO" & district == "KOLOKANI" ~ "KOULIKORO",
        country == "MALI" & province == "TOMBOUCTOU" & district == "NIAFUNKE" ~ "TOMBOUCTOU",
        country == "MALI" & province == "KAYES" & district == "KAYES" ~ "KAYES",
        country == "MALI" & province == "TAOUDENI" & district == "ACHOURATT" ~ "TAOUDENIT",
        country == "MALI" & province == "TAOUDENI" & district == "AL-OURCHE" ~ "TAOUDENIT",
        country == "MALI" & province == "TAOUDENI" & district == "BOUJBEHA" ~ "TAOUDENIT",
        country == "MALI" & province == "TOMBOUCTOU" & district == "GOUNDAM" ~ "TOMBOUCTOU",
        country == "MALI" & province == "TOMBOUCTOU" & district == "TOMBOUCTOU" ~ "TOMBOUCTOU",
        country == "MALI" & province == "KIDAL" & district == "KIDAL" ~ "KIDAL",
        country == "MALI" & province == "TAOUDENIT" & district == "ARAWANE" ~ "TAOUDENIT",
        country == "MALI" & province == "TAOUDENIT" & district == "TAOUDENI" ~ "TAOUDENIT",
        country == "MALI" & province == "KIDAL" & district == "ABEIBARA" ~ "KIDAL",
        country == "MALI" & province == "TAOUDENIT" & district == "FOUM-ALBA" ~ "TAOUDENIT",
        country == "MALI" & province == "KIDAL" & district == "TESSALIT" ~ "KIDAL",
        country == "MALI" & province == "KIDAL" & district == "TINESSAKO" ~ "KIDAL",
        
        # ============================================================
        # NIGER PROVINCE CORRECTIONS
        # ============================================================
        country == "NIGER" & province == "DOSSO" & district == "DOSSO" ~ "DOSSO",
        country == "NIGER" & province == "DOSSO" & district == "FALMEY" ~ "DOSSO",
        country == "NIGER" & province == "DOSSO" & district == "DIOUNDOU" ~ "DOSSO",
        country == "NIGER" & province == "DOSSO" & district == "GAYA" ~ "DOSSO",
        country == "NIGER" & province == "DOSSO" & district == "LOGA" ~ "DOSSO",
        country == "NIGER" & province == "TILLABERI" & district == "KOLLO" ~ "TILLABTRI",
        country == "NIGER" & province == "TILLABERI" & district == "TILLABERI" ~ "TILLABTRI",
        country == "NIGER" & province == "DOSSO" & district == "DOGON DOUTCHI" ~ "DOSSO",
        country == "NIGER" & province == "DOSSO" & district == "TIBIRI" ~ "DOSSO",
        country == "NIGER" & province == "NIAMEY" & district == "NIAMEY 1" ~ "NIAMEY",
        country == "NIGER" & province == "NIAMEY" & district == "NIAMEY 2" ~ "NIAMEY",
        country == "NIGER" & province == "NIAMEY" & district == "NIAMEY 4" ~ "NIAMEY",
        country == "NIGER" & province == "NIAMEY" & district == "NIAMEY 5" ~ "NIAMEY",
        country == "NIGER" & province == "TAHOUA" & district == "BAGAROUA" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "BIRNI N'KONNI" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "BOUZA" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "ILLÉLA" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "KEITA" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "MADAOUA" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "MALBAZA" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "TAHOUA DEP" ~ "TAHOUA",
        country == "NIGER" & province == "TILLABERI" & district == "BALLEYARA" ~ "TILLABERI",
        country == "NIGER" & province == "DOSSO" & district == "BOBOYE" ~ "DOSSO",
        country == "NIGER" & province == "NIAMEY" & district == "NIAMEY 3" ~ "NIAMEY",
        country == "NIGER" & province == "TAHOUA" & district == "TAHOUA COM" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "TASSARA" ~ "TAHOUA",
        country == "NIGER" & province == "ZINDER" & district == "TAKEITA" ~ "ZINDER",
        country == "NIGER" & province == "MARADI" & district == "DAKORO" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "GAZAOUA" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "G. ROUMDJI" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "MADAROUNFA" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "MAYAHI" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "TESSAOUA" ~ "MARADI",
        country == "NIGER" & province == "TAHOUA" & district == "ABALAK" ~ "TAHOUA",
        country == "NIGER" & province == "TAHOUA" & district == "TCHINTABARADEN" ~ "TAHOUA",
        country == "NIGER" & province == "ZINDER" & district == "KANTCHÉ" ~ "ZINDER",
        country == "NIGER" & province == "MARADI" & district == "AGUIÉ" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "MARADI VILLE" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "AGUIE" ~ "MARADI",
        country == "NIGER" & province == "MARADI" & district == "BERMO" ~ "MARADI",
        country == "NIGER" & province == "TAHOUA" & district == "ILLELA" ~ "TAHOUA",
        country == "NIGER" & province == "ZINDER" & district == "ZINDER" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "DAMAGARAM TAKAYA" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "DUNGASS" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "GOURE" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "MAGARIA" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "MIRRIAH" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "TANOUT" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "BELBEDJI" ~ "ZINDER",
        country == "NIGER" & province == "ZINDER" & district == "TESKER" ~ "ZINDER",
        country == "NIGER" & province == "AGADEZ" & district == "ADERBISSANAT" ~ "AGADEZ",
        country == "NIGER" & province == "AGADEZ" & district == "AGADEZ" ~ "AGADEZ",
        country == "NIGER" & province == "AGADEZ" & district == "ARLIT" ~ "AGADEZ",
        country == "NIGER" & province == "AGADEZ" & district == "IFÉROUANE" ~ "AGADEZ",
        country == "NIGER" & province == "AGADEZ" & district == "INGALL" ~ "AGADEZ",
        country == "NIGER" & province == "AGADEZ" & district == "TCHIROZÉRINE" ~ "AGADEZ",
        country == "NIGER" & province == "DIFFA" & district == "DIFFA" ~ "DIFFA",
        country == "NIGER" & province == "DIFFA" & district == "GOUDOUMARIA" ~ "DIFFA",
        country == "NIGER" & province == "DIFFA" & district == "MAINE SOROA" ~ "DIFFA",
        country == "NIGER" & province == "DIFFA" & district == "N'GUIGMI" ~ "DIFFA",
        country == "NIGER" & province == "TILLABERI" & district == "SAY" ~ "TILLABTRI",
        country == "NIGER" & province == "DIFFA" & district == "N'GOURTI" ~ "DIFFA",
        country == "NIGER" & province == "DIFFA" & district == "NGUIGMI" ~ "DIFFA",
        country == "NIGER" & province == "TAHOUA" & district == "BIRNI NKONNI" ~ "TAHOUA",
        country == "NIGER" & province == "DIFFA" & district == "BOSSO" ~ "DIFFA",
        
        # Keep original if no match
        TRUE ~ province
      )
    )
}

# ============================================================
# CONTINUES IN PART 3...
# ============================================================

# ============================================================
# FUNCTION: APPLY AFRO BLOCK CLASSIFICATION
# ============================================================

apply_afro_block <- function(data) {
  data %>%
    mutate(
      AFRO_block = case_when(
        country %in% c("NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC") ~ "LCB",
        country %in% c("ALGERIA", "BURKINA FASO", "MAURITANIA", "MALI", "GUINEA", "GHANA", 
                       "TOGO", "BENIN", "COTE D IVOIRE", "SIERRA LEONE", "LIBERIA", 
                       "GUINEA-BISSAU", "GAMBIA", "SENEGAL") ~ "WA",
        country %in% c("ANGOLA", "DEMOCRATIC REPUBLIC OF THE CONGO") ~ "DRC",
        country %in% c("CONGO", "GABON", "EQUATORIAL GUINEA") ~ "CEA",
        country %in% c("RWANDA", "BURUNDI", "KENYA", "ERITREA", "ETHIOPIA", 
                       "SOUTH SUDAN", "UGANDA", "UNITED REPUBLIC OF TANZANIA", "MALAWI", 
                       "ZAMBIA", "MOZAMBIQUE", "ZIMBABWE", "BOTSWANA", "NAMIBIA", 
                       "ESWATINI (KINGDOM)", "LESOTHO(KINGDOM", "MADAGASCAR", "COMOROS", 
                       "SEYCHELLES", "MAURITIUS", "CAPE VERDE", "SAO TOME AND PRINCIPE") ~ "ESA",
        TRUE ~ "OTHER"
      )
    )
}

# ============================================================
# FUNCTION: HARMONIZE DATES WITH LOOKUP TABLE
# ============================================================

harmonize_dates <- function(data, lookup_file) {
  
  log_info("Reading and harmonizing dates with lookup file...")
  
  # Check if lookup file exists
  if (!file.exists(lookup_file)) {
    log_warn("Lookup file not found: {lookup_file}")
    log_info("Creating minimal lookup from existing data...")
    
    # Create minimal lookup from existing data
    date <- data %>%
      select(response, vaccine.type, roundNumber, start_date, end_date) %>%
      distinct() %>%
      rename(`OBR Name` = response, 
             Vaccines = vaccine.type, 
             `Round Number` = roundNumber,
             `Round Start Date` = start_date) %>%
      mutate(Country = NA_character_,
             `Round Start Date` = as_date(`Round Start Date`)) %>%
      filter(!is.na(`Round Start Date`))
    
    log_warn("Created minimal lookup with {nrow(date)} entries")
  } else {
    # Read the Excel file
    date <- read_excel(lookup_file)
    log_info("Read lookup file from {lookup_file}")
    log_info("Lookup contains {nrow(date)} rows")
  }
  
  # Transform the "Round Number" column
  date <- date %>%
    mutate(`Round Number` = case_when(
      `Round Number` == "Round 0" ~ "Rnd0",
      `Round Number` == "Round 1" ~ "Rnd1",
      `Round Number` == "Round 2" ~ "Rnd2",
      `Round Number` == "Round 3" ~ "Rnd3",
      `Round Number` == "Round 4" ~ "Rnd4",
      `Round Number` == "Round 5" ~ "Rnd5",
      `Round Number` == "Round 6" ~ "Rnd6",
      TRUE ~ `Round Number`
    ))
  
  # Prepare the lookup table
  lookup_data <- date %>%
    rename(
      response = `OBR Name`,
      vaccine.type = Vaccines,
      roundNumber = `Round Number`
    ) %>%
    mutate(
      round_start_date = as_date(`Round Start Date`)
    ) %>%
    mutate(round_start_date = case_when(
      Country == "ALGERIA" & response == "ALG-2024-01-01_nOPV" & roundNumber == "Rnd1" ~ as_date("2024-02-18"),
      TRUE ~ round_start_date
    )) %>%
    mutate(
      start_date = round_start_date + 4,
      end_date = as_date(start_date) + 1
    )
  
  # Prepare lookup table for joining
  prep_data <- lookup_data %>%
    select(response, vaccine.type, roundNumber, round_start_date, start_date, end_date)
  
  lookup_table <- as_tibble(prep_data) %>%
    mutate(
      start_date = as_date(start_date),
      end_date = as_date(end_date),
      round_start_date = as_date(round_start_date)
    ) %>%
    rename(
      prep_round_start_date = round_start_date,
      prep_start_date = start_date,
      prep_end_date = end_date
    )
  
  log_info("Lookup table has {nrow(lookup_table)} entries")
  
  # Join with main data
  result <- data %>%
    left_join(lookup_table, by = c("response", "vaccine.type", "roundNumber")) %>%
    mutate(
      lqas_start_date = coalesce(prep_start_date, as_date(start_date)),
      lqas_end_date = as_date(lqas_start_date) + 1,
      round_start_date = coalesce(prep_round_start_date, as_date(round_start_date), lqas_start_date - days(4))
    ) %>%
    select(-prep_start_date, -prep_end_date, -prep_round_start_date) %>%
    filter(!is.na(district))
  
  log_info("After date harmonization: {nrow(result)} rows")
  
  return(result)
}

# ============================================================
# FUNCTION: APPLY FINAL TRANSFORMATIONS
# ============================================================

apply_final_transformations <- function(data) {
  
  log_info("Applying final transformations...")
  
  result <- data %>%
    mutate(
      lqas_start_date = as_date(lqas_start_date),
      year = year(lqas_start_date)
    ) %>%
    select(
      any_of(c(
        "AFRO_block", "country_code", "country", "province", "district",
        "response", "vaccine.type", "roundNumber", "numbercluster",
        "round_start_date", "lqas_start_date", "lqas_end_date", "year",
        "male_sampled", "female_sampled", "total_sampled",
        "male_vaccinated", "female_vaccinated", "total_vaccinated",
        "total_missed", "status", "performance",
        "r_non_compliance", "r_house_not_visited", "r_childabsent",
        "r_child_was_asleep", "r_child_is_a_visitor", "r_vaccinated_but_not_FM",
        "r_childnotborn", "r_security", "other_r",
        "prct_care_giver_informed_SIA", "prct_non_compliance",
        "prct_house_not_visited", "prct_childabsent", "prct_child_was_asleep",
        "prct_child_is_a_visitor", "prct_vaccinated_but_not_FM",
        "prct_security", "prct_childnotborn", "prct_other_r"
      )),
      starts_with("abs_reason_"),
      starts_with("nc_reason_")
    ) %>%
    select(-any_of(c("abs_reason_diff", "abs_reason_consistent_pct", "abs_reason_check", 
                     "abs_reason_total", "nc_reason_diff", "nc_reason_consistent_pct", 
                     "nc_reason_check", "nc_reason_total"))) %>%
    arrange(lqas_start_date)
  
  # Add distinct key and remove duplicates
  result <- result %>%
    mutate(rnd_distinct = paste(country, province, district, response, roundNumber, sep = "_")) %>%
    distinct(rnd_distinct, .keep_all = TRUE) %>%
    select(-rnd_distinct)
  
  # Special case adjustments
  result <- result %>%
    mutate(
      round_start_date = case_when(
        country == "ALGERIA" & response == "ALG-2023-09-01_nOPV" & roundNumber == "Rnd1" ~ as_date("2024-01-28"),
        country == "CENTRAL AFRICAN REPUBLIC" & response == "RCA-2025-10-01_nOPV_NIDs" & roundNumber == "Rnd1" ~ as_date("2025-11-28"),
        TRUE ~ round_start_date
      ),
      response = case_when(
        country == "ALGERIA" & response == "nOPV2022" ~ "Algeria Outbreak",
        response == "SEN_VPOn" ~ "SEN-79DS-01-2021",
        response == "Liberia" ~ "LBR-15DS-10-2020",
        response == "RSSmOPV10C2021" ~ "SSD-79DS-09-2020",
        response == "CONAKRY-mOPV" ~ "GUE-20DS-02-2021",
        TRUE ~ response
      )
    )
  
  log_info("Final dataset: {nrow(result)} rows, {ncol(result)} columns")
  
  return(result)
}

# ============================================================
# FUNCTION: GENERATE SUMMARY STATISTICS
# ============================================================

generate_summary_stats <- function(data) {
  
  log_info("\n📊 GENERATING SUMMARY STATISTICS")
  log_info("=" %>% paste(rep("=", 50), collapse = ""))
  
  summary_stats <- data %>%
    summarise(
      total_records = n(),
      total_countries = n_distinct(country),
      total_districts = n_distinct(district),
      date_range = paste(min(lqas_start_date, na.rm = TRUE), "to", max(lqas_start_date, na.rm = TRUE)),
      total_sampled = sum(total_sampled, na.rm = TRUE),
      total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
      coverage = round(total_vaccinated / total_sampled * 100, 1)
    )
  
  log_info("  Total records: {format(summary_stats$total_records, big.mark = ',')}")
  log_info("  Total countries: {summary_stats$total_countries}")
  log_info("  Total districts: {summary_stats$total_districts}")
  log_info("  Date range: {summary_stats$date_range}")
  log_info("  Total sampled: {format(summary_stats$total_sampled, big.mark = ',')}")
  log_info("  Total vaccinated: {format(summary_stats$total_vaccinated, big.mark = ',')}")
  log_info("  Overall coverage: {summary_stats$coverage}%")
  
  # Country breakdown
  log_info("\n  Country breakdown:")
  country_summary <- data %>%
    group_by(country) %>%
    summarise(
      records = n(),
      coverage = round(sum(total_vaccinated, na.rm = TRUE) / sum(total_sampled, na.rm = TRUE) * 100, 1)
    ) %>%
    arrange(desc(records))
  
  for (i in 1:nrow(country_summary)) {
    log_info("    {country_summary$country[i]}: {country_summary$records[i]} records, {country_summary$coverage[i]}% coverage")
  }
  
  return(summary_stats)
}

# ============================================================
# FUNCTION: WRITE OUTPUT FILES
# ============================================================

write_output_files <- function(data, output_file) {
  
  log_info("\n💾 Writing output files...")
  
  # Create output directory if needed
  dir_create(dirname(output_file))
  
  # Write final CSV
  write_csv(data, output_file)
  log_info("✅ Saved final data to {output_file}")
  
  # Also save to reports directory for dashboard
  reports_dir <- "reports"
  dir_create(reports_dir)
  report_file <- file.path(reports_dir, "lqas_final_data.csv")
  write_csv(data, report_file)
  log_info("✅ Also saved to {report_file}")
  
  # Save a backup with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path("data/archive", paste0("lqas_final_data_", timestamp, ".csv"))
  dir_create("data/archive")
  write_csv(data, backup_file)
  log_info("✅ Backup saved to {backup_file}")
  
  return(invisible(TRUE))
}

# ============================================================
# MAIN PIPELINE FUNCTION
# ============================================================

run_pipeline <- function(input_dir, lookup_file, output_file, start_date) {
  
  log_info("\n🚀 RUNNING LQAS DATA CLEANING PIPELINE")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))
  
  # STEP 1: Read and combine files
  log_info("\n📁 STEP 1: Reading files from {input_dir}")
  
  list_of_files <- list.files(
    input_dir,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    .[!str_detect(., "QA_unmapped_reasons")] %>%
    .[!str_detect(basename(.), "^processed_files_log\\.csv$")]
  
  log_info("Found {length(list_of_files)} CSV files to combine")
  
  if (length(list_of_files) == 0) {
    log_error("No CSV files found in {input_dir}")
    return(NULL)
  }
  
  # STEP 2: Combine all files
  log_info("\n🔗 STEP 2: Combining all files")
  
  combined_data <- lapply(list_of_files, function(x) {
    tryCatch({
      readr::read_csv(x, show_col_types = FALSE)
    }, error = function(e) {
      log_warn("Failed to read {basename(x)}: {e$message}")
      return(NULL)
    })
  }) %>%
    bind_rows() %>%
    filter(!is.na(response), response != "NA")
  
  log_info("Combined {nrow(combined_data)} rows")
  
  # STEP 3: Basic data cleaning
  log_info("\n🧹 STEP 3: Basic data cleaning")
  
  clean_data <- combined_data %>%
    filter(start_date > as_date(start_date)) %>%
    arrange(start_date) %>%
    mutate(
      performance = case_when(
        performance == "High" ~ "high",
        performance == "Moderate" ~ "moderate",
        performance == "Very_poor" ~ "very poor",
        performance == "Poor" ~ "poor",
        TRUE ~ performance
      ),
      country = case_when(
        country == "Ethiopia" ~ "ETH",
        TRUE ~ country
      ),
      roundNumber = case_when(
        roundNumber == "RND2" ~ "Rnd2",
        roundNumber == "RND1" ~ "Rnd1",
        roundNumber == "RND3" ~ "Rnd3",
        TRUE ~ roundNumber
      )
    )
  
  log_info("After cleaning: {nrow(clean_data)} rows")
  
  # STEP 4: Standardize country names
  log_info("\n🌍 STEP 4: Standardizing country names")
  
  clean_data <- clean_data %>%
    mutate(
      province = toupper(province),
      district = toupper(district)
    ) %>%
    mutate(country_code = country) %>%
    mutate(
      country = case_when(
        country == "NAM" ~ "NAMIBIA",
        country == "GAM" ~ "GAMBIA",
        country == "GHA" ~ "GHANA",
        country == "ALG" ~ "ALGERIA",
        country == "ETH" ~ "ETHIOPIA",
        country == "ANG" ~ "ANGOLA",
        country == "BEN" ~ "BENIN",
        country == "BUI" ~ "BURUNDI",
        country == "BURKINA_FASO" ~ "BURKINA FASO",
        country == "BFA" ~ "BURKINA FASO",
        country == "BWA" ~ "BOTSWANA",
        country == "CHD" ~ "CHAD",
        country == "CAE" ~ "CAMEROON",
        country == "CIV" ~ "COTE D IVOIRE",
        country == "COG" ~ "CONGO",
        country == "GUI" ~ "GUINEA",
        country == "KEN" ~ "KENYA",
        country == "LIB" ~ "LIBERIA",
        country == "MAL" ~ "MALI",
        country == "MAU" ~ "MAURITANIA",
        country == "MDG" ~ "MADAGASCAR",
        country == "MLW" ~ "MALAWI",
        country == "MOZ" ~ "MOZAMBIQUE",
        country == "NIE" ~ "NIGERIA",
        country == "NIG" ~ "NIGER",
        country == "RCA" ~ "CENTRAL AFRICAN REPUBLIC",
        country == "RDC" ~ "DEMOCRATIC REPUBLIC OF THE CONGO",
        country == "RWA" ~ "RWANDA",
        country == "SEN" ~ "SENEGAL",
        country == "SIL" ~ "SIERRA LEONE",
        country == "SSUD" ~ "SOUTH SUDAN",
        country == "TNZ" ~ "UNITED REPUBLIC OF TANZANIA",
        country == "TOG" ~ "TOGO",
        country == "UGA" ~ "UGANDA",
        country == "ZIM" ~ "ZIMBABWE",
        country == "ZMB" ~ "ZAMBIA",
        TRUE ~ country
      )
    )
  
  # STEP 5: Apply district mappings
  log_info("\n🏙️ STEP 5: Applying district name corrections")
  clean_data <- apply_district_mappings(clean_data)
  
  # STEP 6: Apply province mappings
  log_info("\n🗺️ STEP 6: Applying province name corrections")
  clean_data <- apply_province_mappings(clean_data)
  
  # STEP 7: Apply AFRO block classification
  log_info("\n🌍 STEP 7: Applying AFRO block classification")
  clean_data <- apply_afro_block(clean_data)
  
  # STEP 8: Harmonize dates
  log_info("\n📅 STEP 8: Harmonizing dates with lookup file")
  clean_data <- harmonize_dates(clean_data, lookup_file)
  
  # STEP 9: Apply final transformations
  log_info("\n🎯 STEP 9: Applying final transformations")
  final_data <- apply_final_transformations(clean_data)
  
  # STEP 10: Generate summary statistics
  summary_stats <- generate_summary_stats(final_data)
  
  # STEP 11: Write output files
  write_output_files(final_data, output_file)
  
  log_info("\n✅ PIPELINE COMPLETED SUCCESSFULLY!")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))
  
  return(final_data)
}

# ============================================================
# SCRIPT EXECUTION
# ============================================================

# Run the pipeline
result <- run_pipeline(
  input_dir = args$input_dir,
  lookup_file = args$lookup_file,
  output_file = args$output_file,
  start_date = args$start_date
)

# Exit with appropriate status
if (is.null(result)) {
  log_error("Pipeline failed")
  quit(status = 1)
} else {
  log_info("Pipeline completed with {nrow(result)} records")
  quit(status = 0)
}