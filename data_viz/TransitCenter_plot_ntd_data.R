
# bus change small multiples
pct_change_maps <- function(variable) {
  t <- df %>%
    select(one_of(c('name_msa', 'year', variable))) %>%
    mutate(name_msa = gsub('-.*', '', name_msa))
    na.omit
  names(t) <- c('name_msa', 'year', 'var')
  tt <- t %>%
    group_by(name_msa) %>%
    mutate(chg = change(var)) %>%
    na.omit
  
  m <- mean(tt$chg)
  sdv <- sd(tt$chg)
  ll <- m - (2 * sdv)
  ul <- m + (2 * sdv)
  limits <- c(ll, ul)
  
  if (ll < -0.75) {
    limits <- c(-0.75, ul)
  }
  if (ul > 0.75) {
    limits <- c(limits[1], 0.75)
  }
  
  p <- ggplot(tt, aes(x = year, y = chg)) +
    geom_line() +
    facet_wrap(~name_msa) +
    scale_y_continuous(limits = limits) +
    geom_hline(yintercept = 0, color = "#0072bb", linetype = "longdash") +
    scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015), 
                       labels = c("'07", "'09", "'11", "'13", "'15")) +
    labs(title = variable, y = '% Change from 2006') +
    theme(axis.text.x = element_text(angle = 45), plot.title = element_text(color = "#0072bb", face = "bold", size = 14))
  return(p)
}

n <- names(df)[-c(1:3,14:16)]

for (v in n) {
  p <- pct_change_maps(v)
  ggsave(plot = p, filename = paste0("change_since_06_", v, '.png'), width = 10, height = 8, units = 'in')
}

# plot mean absolute percent change
mapc <- change_df %>%
  select(revenue_miles:farebox_recovery) %>%
  summarise_all(function(x){mean(abs(na.omit(x)))}) %>%
  gather(variable, change) %>%
  filter(variable != 'upt_rail')

ggplot(mapc, aes(x = variable, y = change)) + geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x ='', y = 'Mean absolute % Change (2006 - 2016)',
       title = 'Average % Change in each NTD variable (2006 - 2016)',
       subtitle = 'Among all 55 msas') +
  theme(axis.text.x = element_text(angle = 45))