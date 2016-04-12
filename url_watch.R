library(rvest)
library(methods)
library(digest)
raw_html = read_html("http://www.cdc.gov/zika/geo/united-states.html")
html_data = raw_html %>% html_nodes("#body") %>% html_text()
hash = digest(html_data, algo="md5")
html_repo_file = "last_page.dat"
old_html = NULL
tryCatch({
  old_html = trimws(readChar(html_repo_file, file.info(html_repo_file)$size))
}, error=function(err){
  print("Could not find repo file")
})
write(hash, html_repo_file)
if(is.null(old_html) || hash != old_html){
  library(RSQLite)
  library(stringr)
  con = dbConnect(SQLite(), dbname="zika_data.sqlite")
  print("HTML has changed. Fetching new data.")
  update_time = (raw_html %>% html_nodes(".syndicate p") %>% html_text())[1]
  syndication_vals = raw_html %>% html_nodes(".syndicate ul li") %>% html_text()
  table_vals = raw_html %>% html_nodes("table tr") %>% html_text()
  raw_data = lapply(table_vals, function(x){
    split_str = strsplit(trimws(x), "\n")
    loc_name = split_str[[1]][1]
    x = lapply(split_str[[1]][-1], function(x){
      v = as.numeric(str_extract(x, "^\\d+"));
      return(v)
    })
    final = unlist(c(loc_name, x));
    if(length(final) == 3 && !loc_name %in% c("Territories", "States")){
      return(final)
    }
  })
  raw_data = raw_data[!sapply(raw_data, is.null)]
  data_repo = t(as.data.frame(raw_data))
  data_repo = data_repo[complete.cases(data_repo),]
  nrows = dim(data_repo)[1];
  rownames(data_repo) = c(1:nrows)
  colnames(data_repo) = c("Location", "TravelCases", "LocalCases")
  now = date()
  print("Data sucessfully fetched and processed")
  show(head(data_repo))
  data_repo = cbind(data_repo, data.frame(time=rep(now, nrows)))
  dbWriteTable(con, "diseasect", data_repo, append=TRUE)
  metadata = data.frame(time = now, update_time = update_time, summary = paste(syndication_vals,collapse = ";"))
  dbWriteTable(con, "metadata", metadata, append=TRUE)
  write.csv(data_repo, "latest_value.csv")
} else{
  print("HTML has not changed.")
}
