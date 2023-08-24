pacman::p_load(rvest, httr, tidyverse)

# Settings
years = 2010:as.numeric(format(Sys.Date(), format = "%Y"))
searches = list(
     R = '"the R software" OR "the R project" OR "r-project.org" OR "R development core" OR "bioconductor" OR "lme4" OR "nlme" OR "lmeR function" OR "ggplot2" OR "Hmisc" OR "r function" OR  "r package" OR "mass package" OR "plyr package" OR "mvtnorm"',
     SPSS = 'SPSS -"SPSS Modeler" -"Amos"',
     SAS = '"SAS Institute" -JMP -"Enterprise Miner"',
     STATA = '("stata" "college station") OR "StataCorp" OR "Stata Corp" OR "Stata Journal" OR "Stata Press" OR "stata command" OR "stata module"',
     Prism = 'GraphPad Prism',
     JASP = '("jasp" (bayesian OR bayes OR wagenmakers) OR ("jasp package" OR "jasp software" OR "jasp team" OR "jasp-stats") -"jasper" -"joint attention symbolic" -EURASIP -"Journal of Applied School Psychology" -"Journal of Applied social psychology"'
     )
sleep_interval = c(1, 10)  # Uniformly break between searches in this interval to prevent scholar from rejecting searches
scholar_prefix = 'https://scholar.google.dk/scholar?hl=en&as_sdt=0%2C5&as_ylo=9999&as_yhi=9999&q='


###################
# HANDY FUNCTIONS #
###################

# Build the URL string
get_url = function(software, year) {
     url_prefix = gsub('9999', as.character(year), scholar_prefix)  # Enter year
     url_search = gsub(' ', '+', searches[[software]])  # Escape spaces
     url_search = gsub('\"', '%22', url_search)  # Escape quotes
     url = paste(url_prefix, url_search, sep='')
     url
}

# Do the web search
get_html = function(url) {
     html = read_html(url)
     #html = content(GET(url))
     html
}

extract_citations = function(html) {
     # Extract the citation number
     hits_strings = html %>%
          html_nodes(css='.gs_ab_mdw') %>%  # Name of the class where we can find citation number
          html_text()
     hits_string = strsplit(hits_strings[2], ' ')[[1]][2]  # Second hit, second "word"
     hits_numeric = as.numeric(gsub(',', '', hits_string))  # As numeric, not string
     hits_numeric
}

get_citations = function(software, year) {
     # Sleep to prevent HTTP error 503
     sleep_duration = runif(1, sleep_interval[1], sleep_interval[2])
     Sys.sleep(sleep_duration)
     
     # Do the search
     url = get_url(software, year)
     html = get_html(url)
     citations =  extract_citations(html)
     
     # Status and return
     print(sprintf('Got %i scholar citations in %i for %s', citations, year, software))
     citations
}


#################
# DO THE SEARCH #
#################
citation_history = expand.grid(years, names(searches))
names(citation_history) = c('year', 'software')

citation_history_sas = citation_history %>%
     filter(software == 'SAS') %>%
     rowwise() %>%
     mutate(
          citations = get_citations(software, year)
     )

# Save it so you don't have to repeat in case Scholar locks you out
write.csv(citation_history, 'citations.csv', row.names = FALSE)


manual_entry_rows <- tibble::tribble(
     ~year, ~software, ~citations,
     2010, "R", 23700,
     2011, "R", 26300,
     2012, "R", 28500,
     2013, "R", 31000,
     2014, "R", 33000,
     2015, "R", 38400,
     2016, "R", 47000,
     2017, "R", 63400,
     2018, "R", 72000,
     2019, "R", 73600,
     2020, "R", 59000,
     2010, "SPSS", 547000 ,
     2011, "SPSS", 445000,
     2012, "SPSS", 429000,
     2013, "SPSS", 505000,
     2014, "SPSS", 436000,
     2015, "SPSS", 405000,
     2016, "SPSS", 322000,
     2017, "SPSS", 282000,
     2018, "SPSS", 220000,
     2019, "SPSS", 147000,
     2020, "SPSS", 87500,
     2010, "SAS", 95500,
     2011, "SAS", 102000,
     2012, "SAS", 102000,
     2013, "SAS", 99200,
     2014, "SAS", 93000,
     2015, "SAS", 86700,
     2016, "SAS", 77600,
     2017, "SAS", 67600,
     2018, "SAS", 57700,
     2019, "SAS", 46900,
     2020, "SAS", NA,
     2010, "STATA", 13100,
     2011, "STATA", 17400,
     2012, "STATA", 19300,
     2013, "STATA", 21900,
     2014, "STATA", 23900,
     2015, "STATA", 29500,
     2016, "STATA", 38500,
     2017, "STATA", 45600,
     2018, "STATA", 44500,
     2019, "STATA", 36500,
     2020, "STATA", 35800
)



# number

options(scipen=999)

plot_data <- manual_entry_rows %>% 
     group_by(year) %>% 
     mutate(prop = citations / sum(citations)) %>% 
     ungroup() %>% 
     filter(!year %in% c(2010, 2011, 2012, 2013)) %>% 
     mutate(year = lubridate::make_date(year = year, month = 01, day = 01)) %>%
     rename("Software" = "software") 


ggplot(data = plot_data, mapping = aes(x = year, y = citations))+
     geom_line(
          mapping = aes(color = Software),
          size = 2)+
     # geom_point(
     #      mapping = aes(
     #           shape = Software,
     #           color = Software),
     #      size = 2)+
     scale_x_date()+
     scale_y_continuous(labels = scales::number_format(),
                        expand = c(0,0),
                        trans = "log2",
                        breaks = c(50000, 10000, 100000, 200000, 300000, 400000, 500000))+
     coord_cartesian(xlim = c(as.Date("2015-01-01"), as.Date("2019-01-01")))+
     theme_light(base_size = 16)+
     theme(plot.caption = element_text(hjust = 0))+
     gghighlight::gghighlight(label_params = list(segment.linetype = 0))+
     #geom_label(aes(label = Software), x = 2019.2, hjust = 0)+
     # ggrepel::geom_text_repel(
     #      data = subset(plot_data, year == 2019),
     #      aes(label = Software),
     #      size = 6,
     #      nudge_x = 45,
     #      segment.color = NA
     # ) +
     #ggrepel::geom_label_repel(mapping = aes(label = Software))+
     labs(
          title = "Google Scholar publications citing\nR, SPSS, SAS, or STATA",
          #subtitle = "",
          x = "Year",
          y = "Publications",
          caption = "Analysis by N. Batra on 22 June 2021\nUsing Google Scholar scraper and methodology at:\nhttps://github.com/lindeloev/spss-is-dying/"
     )

ggsave("R_popularity_num.png", width = 6, height = 6)


# proportion

plot_data %>% 
     ggplot(mapping = aes(x = year, y = prop))+
     geom_line(
          mapping = aes(color = Software),
          size = 2)+
     scale_x_date()+
     scale_y_continuous(labels = scales::percent,
                        expand = c(0,0))+
     theme_light(base_size = 16)+
     theme(plot.caption = element_text(hjust = 0))+
     gghighlight::gghighlight(label_params = list(segment.linetype = 0))+
     labs(
          title = "Popularity of statistical software",
          subtitle = "Share of Google Scholar citations among \nR, SPSS, SAS, or STATA",
          x = "Year",
          y = "Share of citations",
          caption = "Analysis by N. Batra on 22 June 2021\nUsing Google Scholar scraper and methodology at:\nhttps://github.com/lindeloev/spss-is-dying/"
     )


ggsave("R_popularity.png", width = 6, height = 6)
