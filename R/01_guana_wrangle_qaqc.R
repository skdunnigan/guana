# run code before
# source('R/00_loadpackages.R')
# source('R/01_load_wrangle.R')
# source('R/01_guana_wrangle_tidy.R')

# this code is to reformat datafile for QAQC purposes
# ---------------------------------------------------
# split into multiple dataframes
# ---------------------------------------------------
dat_alpha <- dat4 %>%
  select(site, station_code, date_sampled,
         component_short, result) # just nutrient analysis data
dat_beta <- dat4 %>%
  select(site, station_code, date_sampled,
         component_short, remark) # FDEP lab QAQC codes

# add F_ at the start of components in remark dataset
dat_beta$flagcomponent <- paste("F", dat_beta$component_short, sep="_")

dat_beta <- dat_beta %>% dplyr::select(-component_short)

# ---------------------------------------------------
# making data wide
# ---------------------------------------------------
dat_alphawide <- dat_alpha %>%
  group_by(site, date_sampled) %>%
  spread(key = component_short,
         value = result) %>%
  ungroup()

dat_betawide <- dat_beta %>%
  group_by(site, date_sampled) %>%
  spread(key = flagcomponent,
         value = remark) %>%
  ungroup()

dat_betawide  <- dat_betawide %>%
  group_by(site, date_sampled) %>%
  unique()

# bind the columns from one into the next
dat_qaqcwide <- full_join(dat_alphawide, dat_betawide,
                          by = c("site", "date_sampled"))

rm(dat_alpha, dat_beta, dat_alphawide, dat_betawide)

# ---------------------------------------------------
# add calculated parameters
# ---------------------------------------------------

# calculate total n, din, ton
dat_qaqcwide$tn_calc <- dat_qaqcwide$TKN + dat_qaqcwide$NO23F
dat_qaqcwide$f_tn_calc <- NA # blank column
dat_qaqcwide$din <- dat_qaqcwide$NH4_N + dat_qaqcwide$NO23F
dat_qaqcwide$f_din <- NA # blank column
dat_qaqcwide$ton <- dat_qaqcwide$TKN - dat_qaqcwide$NH4_N
dat_qaqcwide$f_ton <- NA # blank column
dat_qaqcwide$f_record <- NA # blank column
dat_qaqcwide$don <- dat_qaqcwide$DTKN - dat_qaqcwide$NH4_N
dat_qaqcwide$f_don <- NA # blank column

# clean up the column headers
dat_qaqcwide <- janitor::clean_names(dat_qaqcwide)

# ---------------------------------------------------
# reorder data frame to match values to remark columns
# ---------------------------------------------------

dat_qaqc <- dat_qaqcwide %>%
  select(site, date_sampled,
         f_record,
         x2_4_d, f_2_4_d,
         aceta, f_aceta,
         alkalinity, f_alkalinity,
         atemp, f_atemp,
         bac_r, f_bac_r,
         bentazon, f_bentazon,
         carbamazepine, f_carbamazepine,
         ch_la_c, f_ch_la_c,
         ch_la_un_c, f_ch_la_un_c,
         ch_lb_tri_n, f_ch_lb_tri_n,
         ch_lc_tri_n, f_ch_lc_tri_n,
         phea, f_phea,
         dg3, f_dg3,
         diuron, f_diuron,
         do, f_do,
         do_p, f_do_p,
         entero, f_entero,
         feccol, f_feccol,
         fenuron, f_fenuron,
         fluoride, f_fluoride,
         fluridone, f_fluridone,
         gfd, f_gfd,
         gull2, f_gull2,
         hf183, f_hf183,
         hydrocodone, f_hydrocodone,
         ibuprofen, f_ibuprofen,
         imazapyr, f_imazapyr,
         imidacloprid, f_imidacloprid,
         linuron, f_linuron,
         mcpp, f_mcpp,
         naproxen, f_naproxen,
         nh4_n, f_nh4_n,
         no23f, f_no23f,
         tn_calc, f_tn_calc,
         din, f_din,
         ton, f_ton,
         don, f_don,
         tkn, f_tkn,
         tn, f_tn,
         dtkn, f_dtkn,
         od664b_od665a, f_od664b_od665a,
         p_h, f_p_h,
         primidone, f_primidone,
         pyraclostrobin, f_pyraclostrobin,
         salt, f_salt,
         secchi, f_secchi,
         sp_cond, f_sp_cond,
         sucra, f_sucra,
         tp, f_tp,
         triclopyr, f_triclopyr,
         tss, f_tss,
         turbidity, f_turbidity,
         w_br_ic, f_w_br_ic,
         w_cl_ic, f_w_cl_ic,
         w_color, f_w_color,
         w_f, f_w_f,
         w_so4_ic, f_w_so4_ic,
         w_tds, f_w_tds,
         w_toc, f_w_toc,
         wdepth, f_wdepth,
         wind_d, f_wind_d,
         wind_s, f_wind_s,
         wtem, f_wtem)

dat_qaqc2 <- dat_qaqc %>%
  unique() # remove duplicates

# ---------------------------------------------------
# replace all NAs with blanks in csv output file
# ---------------------------------------------------

# replace all NAs with blanks
dat_qaqc2 <- sapply(dat_qaqc2, as.character)
dat_qaqc2[is.na(dat_qaqc2)] <- " "
dat_qaqc2 <- as.data.frame(dat_qaqc2)
write.csv(dat_qaqc2, "output/dataforqaqc.csv")

rm(dat_qaqc, dat_qaqc2, dat_qaqcwide)
