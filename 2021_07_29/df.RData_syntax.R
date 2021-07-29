
load("df.RData")

library(tidyverse)

np_bp_co <- df %>% select(Pde:Peq,nw_prof) %>% mutate_each_(funs(scale),vars=c(names(df[,6:15]))) %>% group_by(nw_prof) %>% summarize_all(mean) %>%
  tidyr::gather(key, val, -nw_prof) %>% ggplot(aes(x = nw_prof, 
                                                   y = val, fill = key)) + geom_col(position = "dodge") + 
  scale_x_discrete("Network Profiles", labels = c("nwP0" = "Profile 0", "NWp1" = "Profile 1",
                                                  "NWp2" = "Profile 2", "NWp3" = "Profile 3",
                                                  "NWp4" = "Profile 4")) +
  theme_bw() + scale_color_grey() + theme(legend.position = "none") +
  labs(title = "Estimated with attitude network analysis (CCA)")


lp_bp_co <- df %>% select(Pde:Peq,LPA_prof) %>% mutate_each_(funs(scale),vars=c(names(df[,6:15]))) %>% group_by(LPA_prof) %>% summarize_all(mean) %>%
  tidyr::gather(key, val, -LPA_prof) %>% ggplot(aes(x = LPA_prof, 
                                                    y = val, fill = key)) + geom_col(position = "dodge") + 
  scale_x_discrete("Latent profiles", labels = c("LP1" = "Profile 1", "LP2" = "Profile 2",
                                                 "LP3" = "Profile 3", "LP4" = "Profile 4",
                                                 "LP5" = "Profile 5")) +
  theme_bw() + scale_color_grey() +
  labs(title = "Estimated with latent profile analysis")

gridExtra::grid.arrange(np_bp_co, lp_bp_co, ncol = 2, top = "Scaled national pride items")

set.seed(666)
train <- df %>% sample_frac(.70) # 2134
test  <- anti_join(df, train, by = 'ID') # 915

#####################################################
#### predict network profile from socio-demographics
#####################################################

x_train <- train %>% select(25:49)
x_test  <- test %>% select(25:49)

y_train <- train %>% select(23)
y_train$nw_prof <- as.numeric(y_train$nw_prof) - 1

y_test <- test %>% select(23)
y_test$nw_prof <- as.numeric(y_test$nw_prof) - 1

#### verifying environment may or may not be necessary -- machine/system specific; RStudio sometimes does the following by default
library(reticulate)
conda_list()
use_condaenv("r-reticulate")

library(keras)
y_train <- to_categorical(y_train, 5)
y_test  <- to_categorical(y_test,  5)

x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
x_test <- as.matrix(x_test)
y_test <- as.matrix(y_test)


build_model <- function() {
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 25,
                input_shape = dim(x_train)[2],
                kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_activation_relu() %>%
    layer_dense(units = 100,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>% 
    layer_dropout(0.5) %>% 
    layer_dense(units = 50,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
    layer_dropout(0.4) %>%
    layer_dense(units = 20,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>% 
    layer_dropout(0.3) %>% 
    layer_dense(units = 5, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = list("accuracy")
  )
  model
}

model <- build_model()
model %>% summary()

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

epochs <- 100

history_nw_profile <- model %>% fit(
  x_train,
  y_train,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop, print_dot_callback)
)

score <- evaluate(model, x_test, y_test)

sd_pred_nw_p <- plot(history_nw_profile) + theme_bw() + xlab("") +
  labs(subtitle = "Socio-demographics + political orientation predicting the network profile | accuracy in the test set = 34 %")

#####################################################
#### predict latent profile from socio-demographics
#####################################################
###### same principle as before - same network architecture

y_train <- train %>% select(24)
y_train$LPA_prof <- as.numeric(y_train$LPA_prof) - 1

y_test <- test %>% select(24)
y_test$LPA_prof <- as.numeric(y_test$LPA_prof) - 1

y_train <- to_categorical(y_train, 5)
y_test  <- to_categorical(y_test,  5)

y_train <- as.matrix(y_train)
y_test <- as.matrix(y_test)

history_lpa <- model %>% fit(
  x_train,
  y_train,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop, print_dot_callback)
)

score <- evaluate(model, x_test, y_test)

sd_pred_lp_p <- plot(history_lpa) + theme_bw() + ylab("") + 
  labs(subtitle = "Socio-demographics + political orientation predicting the latent profile | accuracy in the test set = 52 %")

gdata::keep(list = c("df", "sd_pred_nw_p", "sd_pred_lp_p"), sure = T)

#####################################################
#### predict network profile from lpa
#####################################################

y_new_prof <- df %>% select(ID, nw_prof)
library(fastDummies)
x_lpa_dummy <- dummy_columns(df[, 24], remove_selected_columns = T)
prof_df <- bind_cols(x_lpa_dummy, y_new_prof)
prof_df$ID <- df$ID

set.seed(666)
train <- prof_df %>% sample_frac(.70)
test  <- anti_join(prof_df, train, by = 'ID')

x_train <- train %>% select(1:5)
x_test <- test %>% select(1:5)

y_train <- train %>% select(7)
y_train$nw_prof <- as.numeric(y_train$nw_prof) - 1

y_test <- test %>% select(7)
y_test$nw_prof <- as.numeric(y_test$nw_prof) - 1

y_train <- to_categorical(y_train, 5)
y_test  <- to_categorical(y_test,  5)

x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
x_test <- as.matrix(x_test)
y_test <- as.matrix(y_test)

build_model <- function() {
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 25,
                input_shape = dim(x_train)[2],
                kernel_regularizer = regularizer_l2(l = 0.001)) %>% 
    layer_activation_relu() %>% 
    layer_dense(units = 100,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
    layer_dropout(0.5) %>% 
    layer_dense(units = 50,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
    layer_dropout(0.4) %>% 
    layer_dense(units = 20,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
    layer_dropout(0.3) %>% 
    layer_dense(units = 5, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = list("accuracy")
  )
  model
}

model <- build_model()
model %>% summary()

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

epochs <- 100

history_nw_prof_lp <- model %>% fit(
  x_train,
  y_train,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop, print_dot_callback)
)

score <- evaluate(model, x_test, y_test)

lp_pred_nw_p <- plot(history_nw_prof_lp) + theme_bw() + xlab("") + ylab("") +
  labs(subtitle = "Latent profile predicting the network profile | accuracy in the test set = 34 %")

gridExtra::grid.arrange(lp_pred_nw_p, sd_pred_nw_p, sd_pred_lp_p, nrow=3, top = "Deep neural network results")
