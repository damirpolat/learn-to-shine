# getting started with scatterd3
library(scatterD3)
library(llama)
library(aslib)
library(mlr)

learner1 = makeImputeWrapper(learner = setHyperPars(makeLearner("regr.featureless")),
                    classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(),
                    factor = imputeConstant("NA"), character = imputeConstant("NA")))

sc = getCosealASScenario("SAT11-INDU")
llama.cv = convertToLlama(sc)
llama.cv = trainTest(llama.cv)

selector1 = regression(learner1, llama.cv)
selector2 = regression(learner1, llama.cv)

penalties1 = misclassificationPenalties(llama.cv, selector1)
penalties2 = misclassificationPenalties(llama.cv, selector1)

data = data.frame(instance_id = llama.cv$data[llama.cv$test[[1]], llama.cv$ids], x = penalties1, y = penalties2)

tooltip = paste("instance_id = ", data$instance_id, "<br>x = ", data$x, "<br>y = ", data$y)
scatterD3(data = data, x = x, y = y, tooltip_text = tooltip,
          tooltip_position = "top right",
          xlab = "regr.featureless", ylab = "regr.featureless",
          point_size = 100, point_opacity = 0.5,
          colors = "purple",
          hover_size = 3, hover_opacity = 1,
          lines = data.frame(slope = c(0, Inf, 1), intercept = c(0, 0, 0), stroke_width = 1, 
                             stroke_dasharray = 5),
          caption = list(text = paste("Misclassification Penalties for ", learner1$id, " vs. ", learner1$id),
                    title = "Misclassification Penalties"))

