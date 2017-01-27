# TalkingDataMobile

library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(nnet)
library(foreach)
library(randomForest)
library(doParallel)
library(jsonlite)
set.seed(123)

ReadData  = function(x) fread(x,colClasses = "character",integer64=getOption("datatable.integer64"))
toStr  = function(x) paste(x, collapse = ",")
#imporation
gender_age_train <- ReadData("~/data/mobile/gender_age_train.csv")
gender_age_test <- ReadData("~/data/mobile/gender_age_test.csv")
app_labels <- ReadData("~/data/mobile/app_labels.csv")
events <- ReadData("~/data/mobile/events.csv")
label_categories <- ReadData("~/data/mobile/label_categories.csv")
phone_brand <- ReadData("~/data/mobile/phone_brand_device_model.csv")
app_events <- ReadData("~/data/mobile/app_events.csv")

#gender_age
head(gender_age_train)
doublon <- gender_age_train %>% group_by(device_id) %>% filter(row_number()>1)
ggplot(data = gender_age_train, aes(x=group,fill=gender)) + geom_bar()
ggplot(data = gender_age_train, aes(x=gender)) + geom_bar()
ggplot(data = gender_age_train, aes(x=age)) + geom_bar() + facet_grid(gender~.)

gender_age_train$id <- "train"
gender_age_test$gender <- gender_age_test$age <- gender_age_test$group <- NA
gender_age_test$id <- "test"
gender_age <- rbind(gender_age_train, gender_age_test)

#phone_brand
head(phone_brand)
str(phone_brand)
doublon <- phone_brand %>% filter(duplicated(phone_brand$device_id)==TRUE)
##supprimer les doublons
phone_brand_cat <- phone_brand %>%
  filter(duplicated(phone_brand$device_id)==FALSE)

#Merge brands et gender
gender_brand <- merge(gender_age, phone_brand_cat, by="device_id", all.x = TRUE)

#events
head(events,10)
summary(events)
doublon <- events %>% filter(duplicated(events$event_id)==TRUE)
str(events)
events$dates <- as.Date(substr(events$timestamp, 0, 10))
events$days <- weekdays(events$dates)
events$times <- substr(events$timestamp,12,length(events$timestamp))
events$hours <- hour(as.POSIXct(events$times,format="%H:%M:%S"))
events <- events %>% 
  mutate(morning = as.numeric(ifelse (hours>=6 & hours < 12, 1, 0)))
events <- events %>% 
  mutate(day = as.numeric(ifelse(hours >= 12 & hours < 20, 1, 0)))
events <- events %>% 
  mutate(night = as.numeric(ifelse(hours >= 20 | hours == 0, 1, 0)))
nb_events <- events %>%
  group_by(device_id) %>%
  summarise(events_morning=sum(morning), events_day=sum(day), events_night=sum(night))

#merge nb_events et gender_brand
mydata <- merge(gender_brand, nb_events, by="device_id", all.x = TRUE)

#cherche events par device
events <- events %>%
  select(event_id, device_id)
test <- merge(events, mydata, by = "device_id", all.x = TRUE)

#app_events
head(app_events)
str(app_events)
app_events$is_installed <- as.numeric(app_events$is_installed)
app_events$is_active <- as.numeric(app_events$is_active)
nb_app <- app_events %>% 
  group_by(event_id) %>%
  summarise(nb_installed_app=sum(is_installed),nb_active_app=sum(is_active))
app_events <- app_events %>%
  select(event_id,app_id)

nb_app <- merge(app_events, nb_app, by="event_id", all.x = TRUE)
head(nb_app)


#app_labels et label categories
head(app_labels)
app_labels <- merge(app_labels, label_categories, by="label_id", all.x = TRUE)
app_labels <- app_labels %>%
  select(app_id, new_category)
dummy_categories <- as.data.frame(class.ind(app_labels$new_category))
dummy_categories <- cbind(app_labels$app_id, dummy_categories)
colnames(dummy_categories)[1] <- "app_id"
colnames(dummy_categories)[2] <- "art_and_culture"
str(dummy_categories)
app_labels <- dummy_categories %>%
  group_by(app_id) %>%
  summarise(nb_art_cul=sum(art_and_culture), nb_bussniess=sum(busniess), nb_game=sum(game),
            nb_health=sum(health), nb_life=sum(life), nb_media=sum(media), nb_shoping=sum(shoping),
            nb_sport=sum(sport), nb_system=sum(system), nb_travel=sum(travel), nb_unknown=sum(unknown))

#merge nb_app et app_label
events_to_app <- merge(nb_app, app_labels, by="app_id", all.x = TRUE)
head(events_to_app)
titi <- events_to_app %>%
  group_by(event_id) %>%
  summarise(nb_art_ev=sum(nb_art_cul), nb_bussniess_ev=sum(nb_bussniess), 
            nb_game_ev=sum(nb_game), nb_health_ev=sum(nb_health), nb_life_ev=sum(nb_life),
            nb_media_ev=sum(nb_media), nb_shoping_ev=sum(nb_shoping),
            nb_sport_ev=sum(nb_sport), nb_system_ev=sum(nb_system),nb_travel_ev=sum(nb_travel),
            nb_unknown_ev=sum(nb_unknown))
toto <- events_to_app %>%
  select(event_id, nb_installed_app, nb_active_app)%>%
  filter(duplicated(event_id)==FALSE)
events_to_app <- merge(toto, titi, by="event_id", all.x = TRUE)

#merge events et events to app
events_to_device <- merge(events, events_to_app, by="event_id", all.x = TRUE)
colnames(events_to_device)
info_app <- events_to_device %>%
  group_by(device_id) %>%
  summarise(installe_app_dev=sum(nb_installed_app), active_app_dev=sum(nb_active_app),
            art_app_dev=sum(nb_art_ev), buss_app_dev=sum(nb_bussniess_ev),
            health_app_dev=sum(nb_health_ev), life_app_dev=sum(nb_life_ev),
            media_app_dev=sum(nb_media_ev), shop_app_dev=sum(nb_shoping_ev),
            sport_app_dev=sum(nb_sport_ev), system_app=sum(nb_system_ev),
            travel_app_dev=sum(nb_travel_ev), unknown_app=sum(nb_unknown_ev)
            )


#merge mydata et info_app
data <- merge(mydata, info_app, by="device_id", all.x = TRUE)
variable <- c("events_morning","events_day",
              "events_night","installe_app_dev","active_app_dev",
              "art_app_dev","buss_app_dev","health_app_dev","life_app_dev",
              "media_app_dev","shop_app_dev","sport_app_dev","system_app",
              "travel_app_dev","unknown_app")
tmp <- as.matrix(select(data, one_of(variable)))
tmp[is.na(tmp)==TRUE] <- 0

data <- cbind(select(data, one_of(c("device_id", "group", "phone_brand", "device_model", "id"))),
      tmp)
       
#Creer les apprentissage
data_train <- data %>% 
  filter(id=="train") %>%
  select(-id)

data_pred <- data %>%
  filter(id == "test") %>%
  select(-id)

ind <- sample(2, nrow(data_train), replace=TRUE, prob=c(0.7, 0.3))
train <- data_train[ind==1, ]
test <- data_train[ind==2, ]
toJSON(colnames(train))
selection <- c("phone_brand", "device_model","events_morning","events_day",
               "events_night","installe_app_dev","active_app_dev",
               "art_app_dev","buss_app_dev","health_app_dev","life_app_dev",
               "media_app_dev","shop_app_dev","sport_app_dev","system_app",
               "travel_app_dev","unknown_app")
train$group <- as.factor(train$group)
train$phone_brand <- as.character(train$phone_brand)
train$device_model <- as.character(train$device_model)
str(train)

cl <- makeCluster(4)
registerDoParallel(cl)
rf <- foreach(ntree=rep(25, 4), 
              .combine=combine,
              .packages='randomForest') %dopar%
  randomForest(train[,selection],train$group, ntree=ntree, na.rm=TRUE)
stopCluster(cl)
is.na(train[,selection])





