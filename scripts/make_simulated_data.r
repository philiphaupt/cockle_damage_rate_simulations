library(tidyverse)
#aim create simulate data
vessels = 14
ntrails = 1
probs = seq(from = 0.1, to = 0.95, by = 0.05)#c(0.4, 0.55, 0.7) 
replicates = vessels*ntrails
TAC <- c(8008, 7392, 6160)# years 2020, 2021, 2022
vessel_quota = TAC[3]/vessels

#----------------------------------
# NORMAL DISTRIBUTION
# set.seed(198911)
# vecnorm <- rnorm(replicates, mean = c(7.5,10, 20,30,50), sd = 25, lower.tail = FALSE)
# hist(vecnorm)


# UNIFORM DISTRIBUTIN
# set.seed(198911)
# vecunif <- runif(replicates, min = 2, max = 50)
# hist(vecunif)


# 
# set.seed(198911)
# vecpoisson=rpois(replicates,lambda = c(10))
# mean(vecpoisson)
# hist(vecpoisson)
# 
# 
# dfpoisson <- data.frame(vecpoisson)
# dfpoisson <- vecpoisson %>% as_tibble() %>% 
#   mutate(Inspection = if_else(vecpoisson > 10, "FAIL", "PASS"))
# 
# require(ggplot2)
# ggplot(dfpoisson,aes(x = vecpoisson)) + 
#   geom_histogram(aes(y=..density.., fill = Inspection)) +
#   stat_function(fun=function(x)dpois(x, lambda = c(10)),
#                 color="blue", size=2)
# 
# vecpoisson
# 
# require(ggplot2)
# ggplot(dfpoisson) + 
#   geom_histogram(aes(x=vecpoisson, fill = Inspection), binwidth = 1)
#   
# 
# 
# vecexp= rexp(100,0.1)
# hist(vecexp)
# 
# require(ggplot2)
# ggplot(data.frame(vecpoisson), aes(vecpoisson)) + 
#   geom_histogram(aes(y=..density..)) +
#   stat_function(fun=function(x)dgamma(x, shape=nexps, scale=1/rate),
#                 color="red", size=2)
# 
# 
#----------------------------------

# NEGATIVE BINOMIAL

# For loop that will generate list
dfnbinom_list <- list()
for (i in seq_along(probs)) {
  set.seed(198911) # consistent set of results
  # rnbinom(n, size, prob, mu)
  vecnbinom <- rnbinom(n = vessels, size = 14, prob = probs[i])
  # hist(vecnbinom)
  
  dfnbinom <- vecnbinom  %>% 
    as_tibble() %>% 
    rename(damage_rate = value) %>% 
    mutate(probability_pass = probs[i], 
           Inspection = if_else(vecnbinom  > 10, "FAIL", "PASS"),
           tonnage_damaged = TAC[3]/vessels*(damage_rate/100),
           inspection_number = i)# in 2022
  
   dfnbinom_list[i] = list(dfnbinom)
}



# plot
# ggplot(data.frame(dfnbinom_list[2])) + 
#   geom_histogram(aes(x=damage_rate, fill = Inspection), binwidth = 1)+
#   theme_minimal()




#-------------------------------------
# Tonnage damaged
dfnbinom <- do.call(rbind.data.frame, dfnbinom_list)
# PLOT Damage rate pass fail

plot_dat <- dfnbinom %>% 
  group_by(probability_pass) %>% 
  summarise(total_damage = sum(tonnage_damaged), 
            se_damage = plotrix::std.error(tonnage_damaged))


  ggplot(data = plot_dat) + 
    annotate("rect", xmin = 0.4, xmax = 0.7, ymin = 0, ymax = 8000,
             alpha = .1,fill = "blue", col = "black")+
    geom_col(aes(y=total_damage, x = probability_pass, fill = probability_pass))+
  #  stat_function(fun= pnbinom(x = plot_dat$total_damage, size = 14, prob = plot_dat$probability_pass, log = FALSE))+
  geom_errorbar(aes(probability_pass, ymin = (total_damage - se_damage),
                    ymax = (total_damage + se_damage)
                    ),
                #position = "dodge",
                width = 0.05,
                col = "grey33"
                )  +
  scale_fill_continuous(type = "viridis")+
    #xlim(0, 1.2)+
    ylab("Damage (tonnes)")+
    #ylim(0,1.2)+
    xlab("Probability that all vessels will pass inspection")+
    #scale_fill_manual()+
    theme_minimal()+
    labs(title = paste0("Total damage in tons with increased number of inspections \nwith TAC of 6160 tonnes in 2022: "))+
     geom_vline(xintercept = 0.4, col = "black")+
     geom_vline(xintercept = 0.5, col = "black", lty = "dashed")+
     geom_vline(xintercept = 0.6, col = "black", lty = "dashed")+
    geom_vline(xintercept = 0.7, col = "black")+
    geom_label(y =  7500, x = 0.55, aes(label =  "realistic range"))+
    geom_label(y =  3200, x = 0.55, aes(label =  "Inspections:"))+
    geom_label(y =  2700, x = 0.45, aes(label =  "1"))+
    geom_label(y =  2700, x = 0.55, aes(label =  "2"))+
    geom_label(y =  2700, x = 0.65, aes(label =  "3"))+
    geom_text(y =  1500, x = 0.45, aes(label =  "1350 t (22%)"), angle = 90)+
    geom_text(y =  1200, x = 0.55, aes(label =  "783 t (13%)", angle = 90))+
    geom_text(y =  1000, x = 0.65, aes(label =  "308 t (5%)", angle = 90))+
    #geom_hline(yintercept = 1350.8, col = "black", lty = "dashed")+
    #geom_hline(yintercept = 308, col = "black", lty = "dashed")+
    geom_hline(yintercept = 616, col = "red", lty = "dashed")+
    geom_label(y =  616, x = 0.9, aes(label =  "616 t (10 %)"), col = "red")
  
    # 
  
#-----------------------------
  # PLOT Damage rate pass fail
  
  probs = seq(from = 0.3, to = 0.7, by = 0.15)#c(0.4, 0.55, 0.7) 
  
  plot_dam_rates_fn <- function(input_dat){
    ggplot(data.frame(input_dat)) + 
      geom_histogram(aes(x=damage_rate, fill = Inspection), binwidth = 1)+
      scale_fill_manual(values = c("FAIL" = "goldenrod1", "PASS" = "skyblue"))+
      xlim(0, 60)+
      xlab("Damage rate (%)")+
      ylim(0,10)+
      ylab("Number of vessels")+
      #scale_fill_manual()+
      theme_minimal()+
      labs(title = paste0("Annual inspection number: ",unique(input_dat$inspection_number), 
                          #"\nProbability to pass increased to: ",unique(input_dat$probability_pass), 
                          "\nMean damage rate of fleet: ", round(mean(input_dat$damage_rate),0), "%"))+
      geom_vline(xintercept = 10, col = "red", lty = "dashed")
    
  }
  
  # plot each
  lapply(dfnbinom_list,plot_dam_rates_fn)


#-------------------
# 
# 
# 
# 
# set.seed(198911) # consistent set of results
# # rnbinom(n, size, prob, mu)
# vecnbinom <- rnbinom(n = vessels, size = 14, prob = probs[i])
# hist(vecnbinom)
# 
# # require(ggplot2)
# # ggplot(data.frame(vecnbinom), aes(vecnbinom)) + 
# #   geom_histogram(aes(y=..density..), binwidth = 1)
# 
# 
# dfnbinom <- vecnbinom  %>% 
#   as_tibble() %>% 
#   mutate(Inspection = if_else(vecnbinom  > 10, "FAIL", "PASS"))
# 
# require(ggplot2)
# ggplot(dfnbinom) + 
#   geom_histogram(aes(x=value, fill = Inspection), binwidth = 1)
# 
# 
# 
# 
# ### REPEATEDLY SIMULATE
# reps <- 100
# nexps <- 5
# rate <- 0.1
# set.seed(0)
# system.time(
#   x1 <- replicate(reps, sum(rexp(n=nexps, rate=rate)))
# ) # replicate
# 
# 
# head(x1)
# 
# 
# 
# require(ggplot2)
# ggplot(data.frame(x1), aes(x1)) + 
#   geom_histogram(aes(y=..density..)) +
#   stat_function(fun=function(x)dgamma(x, shape=nexps, scale=1/rate),
#                 color="red", size=2)
# 
# 
# 
# # Normal distribution of data between 5 and 10 %
# 
# # Poison distribution between 5 and 50 %
# rnrom