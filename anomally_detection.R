# NI load-unload curves: Instant Hardness calculation
######## Data Preparation - Phase #1

#setwd("C:/R_docs/ageing/for_anomaly_testing/")

#mypath = "C:/...." - Choose your path to data

setwd(mypath)

########################################################################### merge a list of files

# Create list of text files
txt_files_ls = list.files(path=mypath, pattern="*.txt") 
# Read the files in, assuming comma separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.delim(file = x, header = T, sep ="\t", dec=".", skip=5)})
# Combine them
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 

a1 <- select(combined_df, Depth..nm., Load..µN., Time..s.) #chooose the relevant parameters to your problem
a2 <- select(combined_df, Depth..nm., Load..µN., Time..s.)
a <- rbind(a1, a2)

# functions and calculations for the preparation of NI data
f <- function(x) {
  y = 24.5*x^2+ 267894.5*x - 3.82479*x^0.5
}

a <- mutate(a, Instant_H=(1000*a$Load..µN./f(a$Depth..nm.)))
a <- mutate(a, strain=(a$Depth..nm./200))

a <- as_tibble(a) 
a$Time..s. <- round(a$Time..s.,digits=2)

#a$Instant_H <- round(b$Instant_H,digits=3)
#a$strain <- round(b$strain, digits = 3)

aa <- a[,3:5]
bb= filter(aa, Time..s.==1.000)
for (i in 1:40) {
  bb[(1+200*(i-1)):(200*i),] <- filter(aa, Time..s.== i) 
}

bb %>% select(contains("Instant")) %>% round(digits = 3)
bb %>% select(contains("strain")) %>% round(digits = 3)

cc <- bb %>% select(-contains("Time")) 

#dd <- t(cc)

head(cc)

######################################### Unsupervised Classification - Phase #2

library(h2o)
h2o.init(nthreads = -1)

RNGversion("3.5.2"); set.seed(300) # satisfy reproducibility in multiple machines

# loading data

head(bb)

library(h2o)
h2o.init(max_mem_size = "2G", 
         nthreads = 2, 
         ip = "localhost", 
         port = 54321)

d.hex = as.h2o(bb, destination_frame= "d.hex")
##hex file is h2o compatible

head(d.hex)
str(d.hex)

NN_model = h2o.deeplearning(
  x = 1:ncol(d.hex),
  training_frame = d.hex,
  hidden = c(400, 200, 2, 200, 400 ),
  epochs = 600,
  activation = "Tanh",
  autoencoder = TRUE
)

train_supervised_features2 = h2o.deepfeatures(NN_model, d.hex, layer=3)

plotdata2 = as.data.frame(train_supervised_features2)
plotdata2$label = as.character(as.vector(d.hex[,1]))

# adjust the i and the condition in accordance to your problem
for (i in 1:8000) {
  if (plotdata2[i, 3] < 30) {
    plotdata2[i,3] <- 400
  } else if (plotdata2[i, 3] < 31 & plotdata2[i, 3] >20) {
    plotdata2[i,3] <- 300
  } else if (plotdata2[i, 3] < 21 & plotdata2[i, 3] >10) {
    plotdata2[i,3] <- 200
  } else  {
    plotdata2[i,3] <- 100
  }
}

qplot(DF.L3.C1, DF.L3.C2, data = plotdata2, color = label, main = "Neural network: 400 - 200 - 2 - 200 - 4000 ")

# split data
g.split = h2o.splitFrame(data = d.hex,ratios = 0.75)
train = g.split[[1]]#75% training data
test = g.split[[2]]

##3 hidden layers
model_unsup = h2o.deeplearning(x = 1:3,
                               training_frame = train,
                               model_id = "model_unsup",
                               autoencoder = TRUE,
                               reproducible = TRUE, #slow - turn off for real problems
                               ignore_const_cols = FALSE,
                               seed = 42,
                               hidden = c(10, 10, 10), 
                               epochs = 100,
                               activation = "Tanh")

preds = h2o.predict(model_unsup, test)
head(preds)

model.anon = h2o.anomaly(model_unsup, d.hex, per_feature = TRUE)
## cases of fraud

# anomaly detection
anomaly = h2o.anomaly(model_unsup, test) %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  mutate(Class = as.vector(test[, 3]))


for (i in 1:2000) {
  if (anomaly[i, 3] > 0.75) {
    anomaly[i,3] <- 1
  } else {
    anomaly[i,3] <- 0
  }
}

mean_mse = anomaly %>%
#  group_by(Class) %>%
  summarise(mean = mean(Reconstruction.MSE))
# model error

mean_mse

#anomaly$Class = as.factor(anomaly$Class)

# plot the results
ggplot(anomaly, aes(x = as.numeric(rowname), y= Reconstruction.MSE, color = "Class")) +
       geom_point(alpha = 0.3) +
       geom_hline(data = mean_mse, aes(yintercept = mean, color = "Class")) +
       scale_color_brewer(palette = "Set1") +
       labs(x = "instance number",
            color = "Class")
