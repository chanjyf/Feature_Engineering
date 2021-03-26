library(dplyr)
library(rsample)
library(tidymodels)

#  Reading Melbourne data
df = read.csv("/path/Melbourne_housing_FULL.csv")

sapply(df, class)
sapply(df, function(x) sum(is.na(x)))

#removing NA
df = df %>% dplyr::select(Suburb,
                          Rooms,
                          Type,
                          Price,
                          Distance,
                          Bathroom,
                          Car,
                          Lattitude,
                          Longtitude,
                          Regionname,
                          Propertycount) %>% na.omit()
df$Propertycount = as.numeric(df$Propertycount)
df$Distance = as.numeric(df$Distance)

set.seed(123456789)

df_split = initial_split(df, prop = 0.75)

df_train = df_split %>% training()
df_test = df_split %>% testing()


recipe_df = recipe(Price ~ .,
                   data = df_train) %>%
  step_YeoJohnson(all_numeric(),
                  -all_outcomes()) %>%
  step_normalize(all_numeric(),
                 -all_outcomes()) %>%
  step_other(Suburb, Type, Regionname, threshold = 0.01) %>%
  step_dummy(all_nominal(), - all_outcomes()) %>%
  prep()


df_train_baked = recipe_df %>% bake(new_data = df_train)
df_test_baked = recipe_df %>% bake(new_data = df_test)


df_train_baked = recipe_df %>% bake(new_data = df_train)
df_test_baked = recipe_df %>% bake(new_data = df_test)

