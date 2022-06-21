mean((mrf_object$nit_max - mrf_expected$nit_max)/mrf_expected$nit_max, na.rm =TRUE)

cbind(mrf_object$year, mrf_object$nit_max , mrf_expected$nit_max)

toto <- mrf_object %>% 
  inner_join(mrf_expected,
             by = c("basin_name", "year", "source"),
             suffix = c("_object", "_expected")) %>% 
  mutate(pb =abs(mrf_object$nit_max - mrf_expected$nit_max) >10-6) %>% 
  filter(pb)

nit_feature_species(
  Nit_list = Nit_list,
  reference_results = reference_results,
  selected_latin_name = selected_latin_name) %>% 
  filter(basin_name == basin,
         year == 2012) 

mrf_expected %>%   filter(basin_name == basin,
                          year == 2012) 
  
  