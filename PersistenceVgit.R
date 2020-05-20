#' --------------------------------------
#' A BAYESIAN COINTEGRATION TESTING PROCEDURE WITH CONDITIONAL PRIOR ODDS - DATA VISUALIZATION
#' --------------------------------------

# Load or install and load packages
'package' <- function(library, repo = getOption('repos')){ 
  if(!is.element(library, .packages(all.available = TRUE))) {install.packages(library)}
  library(library,character.only = TRUE)}

'package'('dplyr')
'package'('ggplot2')
'package'('lubridate')
'package'('Rfast')
'package'('data.table')
'package'('bit64')
'package'('ff')
'package'('reshape2')
'package'('psych')

#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------

dashline <- '--------------------------------------------------' # Used as a separator when printing out Script execution status 
GLOBAL.uselog = FALSE
p.freq = 6
N <- 9
thresholds <- seq(from = 1, to = 50, by = 0.5)
N2 <- thresholds %>% length()

if (GLOBAL.uselog){
  dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/LogTruePrice/', p.freq, 'm')  
} else {
  dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/TruePrice/', p.freq, 'm')  
}
setwd(dir)
stopifnot(all.equal(getwd(), dir))

#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA 
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------

filename <- paste0('ReTest_NewCoef_1.csv')
master <- fread(filename, sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data

for (i in 2:N){
  filename <- paste0('ReTest_NewCoef_', i, '.csv')
  sample <- fread(filename, sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data
  master <- rbind(master, sample)
}

master %>% head()
master <- master %>% as_tibble() %>% mutate(CPO = BF1 * scaledodds) %>% as.data.frame()

#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' PLOTTING FUNCTION
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------

'plotify' <- function(FUN, vari1, vari2){
  
  coint.vec1 <- coint.vec2 <- N2 %>% numeric()
  rw.vec1 <- rw.vec2 <- N2 %>% numeric()
  se.vec1 <- se.vec2 <- N2 %>% numeric()
  
  fun.str <- as.character(substitute(FUN))
  
  for (i in 1:N2){
    
    TH <- thresholds[i]
    
    coint1 <- master %>% dplyr::filter(!!enquo(vari1) > TH) %>% pull(BF2)
    rw1 <- master %>% dplyr::filter(!!enquo(vari1) <= TH) %>% pull(BF2)
    
    coint2 <- master %>% dplyr::filter(!!enquo(vari2) > TH) %>% pull(BF2)
    rw2 <- master %>% dplyr::filter(!!enquo(vari2) <= TH) %>% pull(BF2)
    
    coint.vec1[i] <- p1 <- (coint1 %>% FUN)
    coint.vec2[i] <- p2 <- (coint2 %>% FUN)
    rw.vec1[i] <- rw1 %>%  FUN
    rw.vec2[i] <- rw2 %>%  FUN
    
    if (fun.str != 'C.Ratio'){
      s1 <- coint1 %>% sd()
      s2 <- coint2 %>% sd()
      sqrt.N1 <- coint1 %>% length() %>% sqrt()
      sqrt.N2 <- coint2 %>% length() %>% sqrt()
      se.vec1[i] <- s1 / sqrt.N1 
      se.vec2[i] <- s2 / sqrt.N2 
    } else {
      se.vec1[i] <- (p1*(1-p1)/(coint1 %>% length())) %>% sqrt()
      se.vec2[i] <- (p2*(1-p2)/(coint2 %>% length())) %>% sqrt()
    }
    
    
  }
  
  plot.data <- data.frame(Threshold = thresholds,
                          'CPO - Cointegrated' = coint.vec1,
                          'CPO - Random walk' = rw.vec1,
                          'BF Cointegrated' = coint.vec2,
                          'BF Random walk' = rw.vec2) %>% {melt(., id = 'Threshold')}
  
  ribbon.data1 <- data.frame(Threshold = thresholds,
                           coint.025 = qnorm(0.025) * se.vec1 + coint.vec1,
                           coint.25 = qnorm(0.25) * se.vec1 + coint.vec1,
                           coint.75 = qnorm(0.75) * se.vec1 + coint.vec1,
                           coint.975 = qnorm(0.975) * se.vec1 + coint.vec1)
  
  ribbon.data2 <- data.frame(Threshold = thresholds,
                             coint.025 = qnorm(0.025) * se.vec2 + coint.vec2,
                             coint.25 = qnorm(0.25) * se.vec2 + coint.vec2,
                             coint.75 = qnorm(0.75) * se.vec2 + coint.vec2,
                             coint.975 = qnorm(0.975) * se.vec2 + coint.vec2)
  
  plot_ <- ggplot() +
            geom_line(data = plot.data, aes(x = Threshold, y = value, color = variable, linetype = variable, size = variable)) + 
            scale_color_manual(values = c('darkgreen', 'darkgreen', 'darkorange', 'darkorange')) +
            scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted")) +
            scale_size_manual(values = c(1, 1.2, 1, 1.2)) +
            geom_ribbon(data = ribbon.data1, aes(x = Threshold, ymin = coint.025, ymax = coint.975), fill = 'darkgreen', alpha = 0.2) +
            geom_ribbon(data = ribbon.data1, aes(x = Threshold, ymin = coint.25, ymax = coint.75), fill = 'darkgreen', alpha = 0.2) +
            geom_ribbon(data = ribbon.data2, aes(x = Threshold, ymin = coint.025, ymax = coint.975), fill = 'darkorange', alpha = 0.2) +
            geom_ribbon(data = ribbon.data2, aes(x = Threshold, ymin = coint.25, ymax = coint.75), fill = 'darkorange', alpha = 0.2) +
            theme_bw() + xlab('Classification threshold') + ylab('Testing threshold\nexceedance proportion') +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            guides(color = guide_legend(nrow = 2)) +
            theme(legend.position = 'bottom', legend.title = element_blank(), legend.text=element_text(size=11), text=element_text(family="serif"))
  
  plot_ %>% return()
}

# Choose threshold for plotting
#R.TH <- 1
#R.TH <- 5
#R.TH <- 10
R.TH <- 20

C.Ratio <- function(vec, TH = R.TH){
  ((ifelse((vec %>% as.numeric) >= TH, 1, 0) %>% sum()) / (vec %>% length())) %>% return()
}

plot_ <- 'plotify'(FUN = C.Ratio, vari1 = SPO, vari2 = BF1)
plot_


#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' PROPORTION TEST
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------

'ztest' <- function(p1, p2, n1, n2){
  n <- p1 - p2 #numerator
  p <- (p1*n1 + p2*n2)/(n1 + n2) #pooled proportion
  d <- sqrt(p*(1-p)/n1 + p*(1-p)/n2)
  n/d %>% return()
}

'proptest' <- function(vari1, vari2, test.type = 'CPO', maxtest = FALSE){
  
  p1.vec <- N2 %>% numeric()
  p2.vec <- N2 %>% numeric()
  n1.vec <- N2 %>% numeric()
  n2.vec <- N2 %>% numeric()
  z.vec <- N2 %>% numeric()

  for (i in 1:N2){
    
    TH <- thresholds[i]
    
    coint1 <- master %>% dplyr::filter(!!enquo(vari1) > TH) %>% pull(BF2)
    coint2 <- master %>% dplyr::filter(!!enquo(vari2) > TH) %>% pull(BF2)
    rw1 <- master %>% dplyr::filter(!!enquo(vari1) <= TH) %>% pull(BF2)
    
    p1.vec[i] <- p1 <- coint1 %>% C.Ratio
    n1.vec[i] <- n1 <- coint1 %>% length()
    
    if (test.type == 'CPO'){
      p2.vec[i] <- p2 <- coint2 %>% C.Ratio 
      n2.vec[i] <- n2 <- coint2 %>% length()
    } else {
      p2.vec[i] <- p2 <- rw1 %>% C.Ratio
      n2.vec[i] <- n2 <- rw1 %>% length()
    }   
    
    z.vec[i] <- 'ztest'(p1, p2, n1, n2)
    
  }
  
  if (maxtest){
    out.df <- data.frame(Threshold = thresholds,
                         'p1' = p1.vec,
                         'p2' = p2.vec,
                         'n1' = n1.vec,
                         'n2' = n2.vec,
                         'z' = z.vec)
  } else {
    out.df <- data.frame(Threshold = thresholds,
                         'z' = z.vec)
  }
  
  
  out.df %>% return()
}

#tt = 'BF'
tt = 'CPO'

th.list <- c(1,5,10,20)

for (i in 1:length(th.list)){
  R.TH <- th.list[i]
  C.Ratio <- function(vec, TH = R.TH){((ifelse((vec %>% as.numeric) >= TH, 1, 0) %>% sum()) / (vec %>% length())) %>% return()}
  Z <- 'proptest'(vari1 = SPO, vari2 = BF1, test.type = tt)
  if (i == 1){Z.BF <- Z} else {Z.BF <- rbind(Z.BF, Z)}
}

Z.BF$Testing_Threshold <- c(rep(1, N2), rep(5, N2), rep(10, N2), rep(20, N2)) %>% as.character()

linesize <- 0.75
plot_ <- ggplot() +
  geom_line(data = Z.BF, size = linesize, aes(x = Threshold, y = z, color = Testing_Threshold)) + 
  scale_color_manual(values = c('darkgreen', 'darkorange', 'blueviolet', 'turquoise1')) +
  geom_hline(aes(yintercept=qnorm(0.975), linetype = '5%'), color='grey60', size = linesize) +
  geom_hline(aes(yintercept=qnorm(0.995), linetype = '1%'), color='grey40', size = linesize) +
  geom_hline(aes(yintercept=qnorm(0.9995), linetype = '0.1%'), color='grey20', size = linesize) +
  theme_bw() +  theme(legend.position = 'bottom', legend.text=element_text(size=11), text=element_text(family="serif")) +
  guides(color=guide_legend(title="Testing threshold")) +
  xlab('Classification threshold') + ylab('Z-score') +
  scale_linetype_manual(name = "Critical values", values = c(3, 2, 5), guide = guide_legend(override.aes = list(color = c("grey20", "grey30", "grey40"))))
plot_




for (i in 1:length(th.list)){
  R.TH <- th.list[i]
  C.Ratio <- function(vec, TH = R.TH){((ifelse((vec %>% as.numeric) >= TH, 1, 0) %>% sum()) / (vec %>% length())) %>% return()}
  max.df <- 'proptest'(vari1 = SPO, vari2 = BF1, test.type = tt, maxtest = TRUE)
  bf.max <- max.df %>% {arrange(., p2)} %>% tail(1)
  max.z.vec <- N2 %>% numeric()
  for (i in 1:N2){max.z.vec[i] <- 'ztest'(max.df[i,]$p1, bf.max$p2, max.df[i,]$n1, bf.max$n2)}
  if (i == 1){z.vec <- max.z.vec} else {z.vec <- c(z.vec, max.z.vec)}
}


z.vec <- data.frame(z = z.vec)
z.vec$Testing_Threshold <- c(rep(1, N2), rep(5, N2), rep(10, N2), rep(20, N2)) %>% as.character()
z.vec$Threshold <- rep(seq(1,50, by= 0.5), 4)

linesize <- 0.75
plot_ <- ggplot() +
  geom_line(data = z.vec, size = linesize, aes(x = Threshold, y = z, color = Testing_Threshold)) + 
  scale_color_manual(values = c('darkgreen', 'darkorange', 'blueviolet', 'turquoise1')) +
  geom_hline(aes(yintercept=qnorm(0.975), linetype = '5%'), color='grey60', size = linesize) +
  geom_hline(aes(yintercept=qnorm(0.995), linetype = '1%'), color='grey40', size = linesize) +
  geom_hline(aes(yintercept=qnorm(0.9995), linetype = '0.1%'), color='grey20', size = linesize) +
  theme_bw() +  theme(legend.position = 'bottom', legend.text=element_text(size=11), text=element_text(family="serif")) +
  guides(color=guide_legend(title="Testing threshold")) +
  xlab('Classification threshold') + ylab('Z-score') + ylim(-4,4) +
  scale_linetype_manual(name = "Critical values", values = c(3, 2, 5), guide = guide_legend(override.aes = list(color = c("grey20", "grey30", "grey40"))))
plot_
