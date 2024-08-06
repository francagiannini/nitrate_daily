library(tidyverse)
library(sf)
library(RColorBrewer)

c_mess <- 
  read.table("O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/data_preproc/conc_nov_site.txt",sep="\t",header = TRUE) |> 
  mutate(site_eng=gsub(x=site_eng,pattern="[\\]", replacement="" ))

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

# read poligonos sf file
dk <- read_rds("O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/data_raw/gadm36_DNK_2_sf.rds")
dk <- st_transform(dk,crs=st_crs(sites))

ggplot(dk)+
  geom_sf(color="#b0b0b0") +
  geom_sf(data=sites, aes(fill = site_eng))+
  geom_sf_label(data=sites,aes(label=site_eng))+
  labs(x="Longitude",y= "Latitude")


# With elly 

elly <- readxl::read_xlsx("C:/Users/au710823/OneDrive - Aarhus universitet/NyMarkmodel/raw_id_master_site_emh_01_to_Franca.xlsx")

table(elly$site_eng)


# mine may

mine <- readxl::read_xlsx("C:/Users/au710823/OneDrive - Aarhus universitet/NITAGRO/nit_complete_may.XLSX")

table(mine$site_eng,mine$harvest_year.x) |> 
  as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Greens")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "harvest year") +
  scale_y_discrete(name = "Site")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

mine_lost <- readxl::read_xlsx("C:/Users/au710823/OneDrive - Aarhus universitet/NITAGRO/nit_lost_searchable.XLSX")

table(mine_lost$site_eng,mine_lost$harvest_year.x) |> 
  as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Purples")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "harvest year") +
  scale_y_discrete(name = "Site")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


