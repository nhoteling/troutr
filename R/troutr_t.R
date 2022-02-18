df.deqt <- readRDS("data/rds/df_deq_t.rds")


#
d <- lapply(1:nrow(df.trt), function(i) {
  df.deqt %>% 
    filter(county == df.trt$county[i]) %>%
    mutate(waterbody1 = df.trt$waterbody1[i],
           jc = stringdist::stringdist(waterbody1, WATER_NAME, method="jaccard",q=3),
           id = df.trt$id[i]) %>%
    filter(jc<0.50) %>%
    arrange(id, jc) %>%
    dplyr::select(id, waterbody1, WQS_ID, WATER_NAME, county, jc, src, coord)
})
df.cndt <- do.call(rbind, d)




#
# Pairwise data
df.adjt <- df.cndt %>% 
  tidyr::nest(data=c(WQS_ID, WATER_NAME, src,coord, jc)) %>%
  mutate(data = purrr::map(data,bsn)) %>%
  tidyr::unnest(cols=c(data))



df.mxt1 <- df.adjt %>%
  dplyr::select(id, waterbody1,WQS_ID, WATER_NAME, source,coord, jc,P) %>%
  tidyr::nest(data=c(WQS_ID, WATER_NAME, source,coord, jc,P)) %>%
  mutate(data = purrr::map(data, maxonly),
         len  = purrr::map_int(data,nrow)) %>%
  filter(len>1) %>%
  tidyr::unnest(cols=c(data)) 

df.mxt2 <- df.mxt1 %>%
  group_by(id) %>%
  summarise(m = sf::st_coordinates(coord)) %>% 
  arrange(id,m) %>%
  tidyr::nest() %>%              
  mutate(ln = purrr::map(data, make_line) %>% sf::st_sfc()) 
```