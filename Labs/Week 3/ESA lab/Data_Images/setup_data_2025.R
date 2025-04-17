# Create data files
# remotes:::install_github("nwfsc-cb/rCAX@*release")
library(rCAX)
library(tidyr)
x <-  rCAX:::caxesu
valid_esu <- which(!str_detect(x, "XN") & !str_detect(x,"N/A"))
esa.salmon <- NULL
for(i in valid_esu){
esuname <- rCAX:::caxesu[i]
print(esuname)
a <- rcax_hli("NOSA", type="colnames")
tab <- rcax_hli("NOSA", flist = list(esu_dps = esuname))
# error no data
if (!is.data.frame(tab)) next
# find the pops with no data and remove
tab <- tab %>% 
  subset((datastatus == "Final" | datastatus == "Reviewed") & bestvalue=="Yes")
if(tab$tsaej[1]=="") tab$value <- tab$tsaij else tab$value <- tab$tsaej
aa <- tab %>% 
  group_by(esapopname, run) %>% 
  summarize(n = sum(value!= "" & value!="0" & majorpopgroup != ""))
bad <- aa[which(aa$n==0),]
aa <- tab %>% 
  group_by(esapopname, run) %>% 
  summarize(n = any(duplicated(spawningyear)))
df <- tab %>% 
  subset(!(esapopname %in% bad$esapopname & run %in% bad$run)) %>%
  mutate(value = as.numeric(value))

# get the min and max years in data
years <- min(df$spawningyear[!is.na(df$value)]):max(df$spawningyear[!is.na(df$value)])
# fill out the missing years with NAs
df <- df %>%
  select(species, esu_dps, majorpopgroup, esapopname, commonpopname, spawningyear, value, run) %>% 
  group_by(species, esu_dps, majorpopgroup, esapopname, commonpopname, run) %>% 
  complete(spawningyear=years, fill=list(value=NA))

# Deal with pops with multiple data
if(any(aa$n)){
  cat(aa$esapopname[aa$n], "has duplicated years\n")
  df <- df %>% ungroup() %>%
    group_by(species, esu_dps, majorpopgroup, esapopname, run, spawningyear) %>%
    summarize(value = mean(value, na.rm = TRUE,
              commonpopname = commonpopname[1]))
}
#if(i == 17 | i == 20) df$value_type <- "tsaij" else df$value_type <- "tsaej"
esa.salmon <- bind_rows(esa.salmon, df)
}
esa.salmon <- esa.salmon %>% subset(species != "") %>% ungroup()
save(esa.salmon, file = file.path("Lab-2", "Data_Images/esa-salmon.rda"))

unique(esa.salmon$esu_dps)
df <- esa.salmon %>% subset(species == "Steelhead" & run == "Winter")
ggplot(df, aes(x=spawningyear, y=log(value), color=majorpopgroup)) + 
  geom_point(size=0.2) + 
  theme(strip.text.x = element_text(size = 2)) +
  theme(axis.text.x = element_text(size = 5, angle = 90)) +
  facet_wrap(~esapopname)