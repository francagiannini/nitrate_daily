# Nitagro WP.2 ----

# Aim
#Design and implement a database infrastructure for the modelling of nitrate 
#leaching with a description of the driving parameters following the FAIR concept (Findable, Accessible, Interoperable and Reusable).

#Tasks
# 2.1: Design and tabulation of the AGRI-NITRATE database infrastructure. ----
# 2.2: Documentation of metadata in the AGRI-NITRATE database infrastructure components
# A data base of experimental sites by AGRO

# Milestones
# M2.1 Conceptual structure of the AGRI-NITRATE database infrastructure.
# Infrastructure composed by 4 databases components

# experiment unit 
# raw measured concentration experimental units
# spatial experimental sites
# percolation unit

#M2.2. AGRI-NITRATE database metadata documentation.

# Deliverables
#D2.1 Report describing the AGRI-NITRATE database infrastructure.
#D2.2 Protocol documenting the AGRI-NITRATE metadata.


# Code workflow ----
# packages 
library(tidyverse)
library(sf)
library(RColorBrewer)

# experiment unit 

mine <- readxl::read_xlsx(
  "C:/Users/au710823/OneDrive - Aarhus universitet/NITAGRO/nit_complete_may.XLSX") |> 
  mutate(site_eng=gsub(x=site_eng,pattern="[\\]", replacement="" )) |> 
  filter(!site_eng %in% "Hojvads Rende")

filt <- mine |> 
  filter(#harvest_year.x>1994,
    ExpNo==103) 
  #filter(ident==24)

summary(filt)

table(filt$site_eng,
      #filt$harvest_year.x,
      filt$ident
      )

# raw concentrations in daily basis 

c_mess <- 
  read.table("O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/data_preproc/conc_nov_site.txt",sep="\t",header = TRUE) |> 
  mutate(site_eng=gsub(x=site_eng,pattern="[\\]", replacement="" ))

c_mess |> filter()

# Filter experimental sites leaving loop and SEGES out
#raw_con_agro <- c_mess 



sites <- read.table(
  "O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/data_preproc/sites_navn.txt",
  sep = "\t",
  header = TRUE,
  dec = ".",
  na.strings = ".",
  quote = ""
) |>  mutate(site_eng=gsub(x=site_eng,pattern="[\\]", replacement="" )) |>  
  drop_na(site_eng) |> 
  st_as_sf(coords = c("X", "Y"), crs = 25832
           #st_crs(all_sites_pont)
  ) |> 
  select(!sites_dk) |> 
  group_by(site_eng)

#sites_agro <- sites

# experimental unit ---