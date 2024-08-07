## packages ----
library(tidyverse)
library(lubridate)
library(readxl)
library(utils)
library(sf)
library(tmap)

# Percolation Calibration ----
wea <- read.table("O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/data_raw/wea_txt.txt",sep = "\t",header = T) |> 
  mutate(date=lubridate::make_date(day=mday, month=month, year=year)) |> 
  rename('sted' ='eksponr') |> 
  select(!c(saedidentnr, 
            drain,
            Intpol_newconc,
            udvaskday,
            sumudvask))
head(wea)
hist(wea$afstroemning)

# wea |> ggplot(aes(x=afstroemning))+
#   geom_histogram(aes(position="identity"), colour = "black",bins=80)+
#   #stat_density(geom = "line", aes(colour = "bla"))+
#   ylab("Count")+ xlab("daily afstroemning (mm)" )+
#   geom_rug() +
#   theme_bw()

# summary(wea$afstroemning)
# summary(wea[which(wea$afstroemning>50),])
# 
# hist(wea$Maaltkonc)
# summary(wea$Maaltkonc)
# 
# summary(wea[which(wea$Maaltkonc>200),])

# #temporal scatter plot

# wea |> 
#   #filter(Id<20) |> 
#   ggplot(aes(x = month, y = afstroemning)) +
#   geom_point() +
#   geom_smooth(method = "loess", se = F) +
#   #coord_cartesian(ylim = c(0,0.9)) +
#   theme(panel.grid = element_blank()) +
#   facet_wrap(~year, nrow=4)+
#   #scale_x_continuous(breaks = seq(1,12,1))+
#   theme_bw() 

wea_c <- wea |> mutate(
  # meas_day_inter= Maaltkonc/Intpol_newconc,
  # meas_day=ifelse(Maaltkonc/Intpol_newconc>0,TRUE, FALSE),
  obs_id = paste(sted,date, sep = "_"),
  drain_day = ifelse(afstroemning >0.3, 1,0)
)

# Concentration ----

#' conc <- read.fwf(
#'   "data_raw/daily_leaching.txt",
#'   #row.names = FALSE,
#'   #header = FALSE,
#'   sep ="\t",
#'   skip = 1,
#'   widths=#137
#'     c(5,
#'       6, 6, 4, 10,
#'       8, 8,
#'       10,
#'       8, 8, 8, 8, 8)
#' )
#' 
#' colnames(conc) <- c('saedidentnr1',
#'                     'year',
#'                     'month',
#'                     'mday',
#'                     'year_juliandayprop',
#'                     'drain',
#'                     'udvaskday',
#'                     'newconc',
#'                     'n_kooncstart',
#'                     'n_koncslut',
#'                     'sumdrain',
#'                     'sumudleach',
#'                     'weigth_year'
#' )
#' 
#' conc_m <- conc |>
#'   mutate(date=lubridate::make_date(day= mday, month=month, year=year)) |>
#'   group_by(n_kooncstart,saedidentnr1,year) |>
#'   mutate(measure_grp=ifelse(min(date)==date,T,F))
#'   #mutate(id_grp=ifelse(n_kooncstart!=n_koncslut & n_kooncstart==newconc ,TRUE,FALSE))
#' 
#' 
#' obs_sted_year <-
#'  table(conc_m$measure_grp,fct_cross(
#'      as.character(conc_m$saedidentnr1),
#'      as.character(conc_m$year))
#'    ) |> as.data.frame() |> filter(Var1==TRUE & Freq<4)
#' 
#' #'2080.2003' <-  conc_m |> filter(saedidentnr1==2080 & year==2003)  
#'   
#' data.frame(obs_sted_year)
#' 
#' write.table(conc_m, "data_raw/conc_txt.txt", sep = "\t")

conc_c <- 
  readxl::read_excel(
  "O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/data_preproc/conc_raw.xlsx")|> 
  mutate(date=lubridate::make_date(day=day, month=month, year=year)) |>
  mutate(
    obs_id = paste(ident,date, sep = "_")
  )

head(conc_c)

wea_c |> 
  ggplot(aes(x = month, fill = as.factor(drain_day))) +
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = seq(1,12,1))+
  theme_bw()+
  ylab("Proportion")


hist(wea_c$drain_day)

table(wea_c$drain_day)
head(wea_c)

c_mess <- merge(wea_c,
                conc_c,
                by = 'obs_id',
                .name_repair="unique",
                suffixes = c("",".y")
                #incomparables = NA#,
                #all.x = TRUE
) |>
  #mutate(check_drain = drain - afstroemning)|>
  select('obs_id',
         'sted',
         'year','month','mday',#'year_juliandayprop',
         'nedboer',
         'ep',
         'ea',
         'afstroemning',
         'date',
         'drain_day'#,
         #'newconc',
         #'sumdrain',
         #'measure_grp'
  ) #|> rename('sumdrain'='sumdrain.1')
# 
# 
# c_mess |> write.table("data_preproc/my_mess_nless.txt" ,sep="\t")
# 
# 
# wea_c[which((
#   interaction(wea_c$sted, wea_c$year) %in% interaction(conc_c$sted, conc_c$year)
# ) == FALSE,
# arr.ind = TRUE),]

# ***Start bis***------ 

c_mess <- read.table("O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/data_preproc/conc_raw.txt",sep="\t",header = TRUE)

c_mess <- c_mess |> 
  mutate(leach_year = ifelse(month<8,
                             paste(year-1),
                             paste(year)
  )) |> 
  group_by(sted,leach_year) |> 
  arrange(date) |> 
  mutate(afstro_sum=cumsum(afstroemning))

#table(c_mess$measure_grp)

c_mess |> filter(measure_grp==TRUE) |> 
  ggplot(aes(x = afstro_sum, y =newconc )) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  theme(panel.grid = element_blank()) +
  geom_vline(aes(xintercept = 90,
                 size = 2, colour = "red", alpha=0.6))+
  #facet_wrap(~year, nrow=4,scales = "free_x")+
  labs(y="Concenration", x="Cummulitive drain")+
  theme_bw()


#c_problem <- c_mess |> filter(is.na(sted)) 

# table_check <- as.data.frame(table(c_problem$sted.x,c_problem$year.x)) |> 
#   filter(Freq>0)

# unique(ytteborg_conc$sted)
# 
# wea_ytteborg <- wea_c |> filter(
#   saedidentnr %in% unique(ytteborg_conc$sted))

### sites ----

sites <- read.table(
  "data_preproc/sites_navn.txt",
  sep = "\t",
  header = TRUE,
  dec = ".",
  na.strings = ".",
  quote = ""
) |> #mutate(X=as.numeric(as.factor('X')),Y=as.numeric(as.factor('Y'))) |>  
  drop_na(site_eng) |> 
  st_as_sf(coords = c("X", "Y"), crs = 25832
           #st_crs(all_sites_pont)
  )

tmap_mode("view")

sites |>  select(!c(strno,StedNavn)) |> unique() |> 
  tm_shape() + 
  tm_dots()+
  tm_text("site_eng", size = 1)

### concentration, percolations, site ----
c_mess_site <- merge(c_mess,
                     sites,
                     by.x = 'sted',
                     by.y = 'strno',
                     all.x = TRUE
) |> 
  mutate(harvest_year = ifelse(month < 4, year - 1, year)) |>
  mutate(merge_id = fct_cross(as.character(sted),
                              as.character(harvest_year),#as.character(year)
                              sep = "_"))


c_mess_site_problems <- c_mess_site |> filter(is.na(site_eng))

summary(c_mess_site_problems)

### concentration, percolations, site, nless data ----

master <- read_excel("data_raw/masterNLESS_Franka100822.xls"
                     , sheet = "master_engl2"
                     #,.name_repair = "minimal"
) |>  mutate(merge_id=fct_cross(as.character(Id),
                                as.character(harvest_year),#year
                                sep="_")) |> select(!year)


c_mess_master <- merge(c_mess_site,
                       master,
                       by='merge_id',
                       all.x = TRUE
) |> 
  # mutate(
  #   start_date=ymd(ifelse(month<4, 
  #                     paste(year-1, "04", "01",sep="-"), 
  #                     paste(year, "04", "01",sep="-")))#, 
  #   #end=ifelse(month<4, make_date(month = 3, day=31, year = year), make_date(month = 3, day=31, year = year+1))
  #   ) 
  # |>  
mutate(
  day_harv=as.numeric(as.Date(date)-as.Date(paste(harvest_year.x, "04", "01",sep="-"))),
  day_leach=as.numeric(as.Date(date)-as.Date(ifelse(month<8,
                                                    paste(year-1, "08", "01",sep="-"),
                                                    paste(year, "08", "01",sep="-")
  ))))


c_mess_master_problems <- c_mess_master |>  filter(is.na(harvest_year.y)) |> filter(measure_grp==T)

writexl::write_xlsx(c_mess_master_problems, "c_mess_master_problems.xlsx")

c_mess_measure_complete <- c_mess_master |>  filter(!is.na(harvest_year.y)) |> filter(measure_grp==T)


write.table(c_mess_master,"c_mess_master.txt", sep="\t")
