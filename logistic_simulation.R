# Simulation of how simple logistic regression can fail to model
# binomial response when the underlying function is more complicated
install.packages(c('data.table', 'plyr', 'ggplot2', 'drc'))
library(data.table)
library(plyr)
library(ggplot2)
library(drc)

# Range to explore functions
numeric_variable <- 1:20

####### DEFINE RESPONSE FUNCTIONS #######
# Generalised logistic function
logistic_function <- function(x) {
  0.25+0.5/(1+exp(-(1*(x-12)+0.1)))
}

linear_function <- function(x) {
  0.25+x*0.025
}

quadratic_function <- function(x) {
  0.25+(x^2)/800
}

log_function <- function(x) {
  0.25+0.5*log(x)/log(20)
}

response_functions <- list(
  'logistic'=logistic_function
  , 'linear'=linear_function
  , 'quadratic'=quadratic_function
  , 'log'=log_function
)

######## DEFINE PREDICTION FUNCTIONS #########

# given data=x, try to predict outcome from numeric_variable

# 2-parameter logistic
logistic_prediction_2p <- function(x) {
  predict(
    glm(outcome ~ numeric_variable, data=x, family="binomial")
    , data.frame(numeric_variable=numeric_variable)
    , type="response"
  )
}

# 4-parameter logistic
logistic_prediction_4p <- function(x) {
  x_summary <- data.table(x)[,list(outcome=sum(outcome)/.N, N=.N),keyby=numeric_variable]
  return(
    predict(
      drm(outcome ~ numeric_variable, fct=LL.4(), data=x_summary, type="binomial")
      , data.frame(numeric_variable=numeric_variable)
    )
  )
}

prediction_functions <- list(
  'logistic-2p'=logistic_prediction_2p
  , 'logistic-4p'=logistic_prediction_4p
)

######## SAMPLING #########

# Conduct sampling
sample_frame <- ldply(
  .data=response_functions
  , .id='function_type'
  ,.fun=function(x) {
    adply(
      .data=numeric_variable
      , .margins=1
      , .id=NULL
      , .fun=function(y) {
        p <- x(y)
        data.frame(
          outcome=sample(c(0,1),100,replace=TRUE,prob=(c(1-p, p)))
          , numeric_variable=y
        )
      }
    )
  }
)

######## CONVERSION & PREDICTIONS #########

# Calculate conversion at each level of numeric variable
sample_summary <- data.table(sample_frame)[
  ,list(
    outcome=sum(outcome)/.N
    , N=.N
    , actual_function=response_functions[[function_type]](numeric_variable)
  )
  ,keyby=list(
    function_type
    , numeric_variable
  )
]

# Calculate predictions for each function
sample_predictions <- ldply(
  .data=prediction_functions
  , .id='prediction_type'
  , .fun=function(x) {
    ddply(
      .data=sample_frame
      , .variables=.(function_type)
      , .fun=function(y) {
        data.frame(
          numeric_variable=numeric_variable
          , prediction=x(y)
        )
      }
    )
  }
)

##### PLOTTING #####

# Comparison plot
p1 <- ggplot(
  data=data.table(sample_predictions)[prediction_type=='logistic-2p',]
  , aes(x=numeric_variable)
)+
geom_point(data=sample_frame, aes(y=outcome), position='jitter')+
#  geom_line(data=sample_summary, aes(y=actual_function, color="Generating Function"), size=3)+
  geom_line(data=sample_summary, aes(color="Observed Conversion", y=outcome), size=2)+
  geom_line(data=data.table(sample_predictions)[prediction_type=='logistic-2p',], aes(color="Fitted Function", fill=function_type, y=prediction), size=2)+
  facet_grid(prediction_type ~ function_type)+
scale_y_continuous("Outcome")+
scale_x_continuous("Numeric Variable")+
guides(color=guide_legend(""))+ theme(legend.justification=c(1,0), legend.position=c(1,0))

png("logistic-2p.png", 600, 400, res=96)
p1
dev.off()

p2 <- ggplot(
  data=data.table(sample_predictions)[prediction_type=='logistic-4p',]
  , aes(x=numeric_variable)
)+
  geom_point(data=sample_frame, aes(y=outcome), position='jitter')+
  #  geom_line(data=sample_summary, aes(y=actual_function, color="Generating Function"), size=3)+
  geom_line(data=sample_summary, aes(color="Observed Conversion", y=outcome), size=2)+
  geom_line(data=data.table(sample_predictions)[prediction_type=='logistic-4p',], aes(color="Fitted Function", fill=function_type, y=prediction), size=2)+
  facet_grid(prediction_type ~ function_type)+
  scale_y_continuous("Outcome")+
  scale_x_continuous("Numeric Variable")+
  guides(color=guide_legend(""))+ theme(legend.justification=c(1,0), legend.position=c(1,0))

png("logistic-4p.png", 600, 400, res=96)
p2
dev.off()