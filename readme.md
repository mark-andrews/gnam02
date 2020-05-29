# Nonlinear regression using R: Generalized Linear Models, Generalized Additive Models, Basis function regression, Gaussian processes, and other methods.

This course provides a general introduction to nonlinear regression analysis,
covering major topics including, but not limited to, general and generalized
linear models, generalized additive models, spline and radial basis function
regression, and Gaussian process regression.  We approach the general topic of
nonlinear regression by showing how the powerful and flexible statistical
modelling framework of general and generalized linear models, and their
multilevel counterparts, can be extended to handle nonlinear relationships
between predictor and outcome variables.  We begin by providing a comprehensive
practical and theoretical overview of regression, including multilevel
regression, using general and generalized linear models. Here, we pay
particular attention to the many variants of general and generalized linear
models, and how these provide a very widely applicable set of tools for
statistical modelling. After this introduction, we then proceed to cover
practically and conceptually simple extensions to the general and generalized
linear models framework using parametric nonlinear models and polynomial
regression.  We will then cover more powerful and flexible extensions of this
modelling framework by way of the general concept of *basis functions*.  We'll
begin our coverage of basis function regression with the major topic of spline
regression, and then proceed to cover radial basis functions and the multilayer
perceptron, both of which are types of artificial neural networks.  We then
move on to the major topic of generalized additive models (GAMs) and
generalized additive mixed models (GAMMs), which can be viewed as the
generalization of all the basis function regression topics, but cover a wider
range of topic including nonlinear spatial and temporal models and interaction
models.  Finally, we will cover the powerful Bayesian nonlinear regression
method of Gaussian process regression.

## Intended Audience

This course is aimed at anyone who is interested to learn and apply nonlinear regression methods. These methods have major applications throughout the economics and other social sciences, life sciences, physical sciences, and machine learning.

## Teaching Format

This course will be hands-on and workshop based. Throughout each day, there will be some lecture style presentation, i.e., using slides, introducing and explaining key concepts. However, even in these cases, the topics being covered will include practical worked examples that will work through together.

The course will take place online using Zoom. On each day, the live video broadcasts will occur between (British Summer Time, UTC+1, timezone) at:

* 10am-12pm
* 1pm-3pm
* 4pm-6pm
 
All sessions will be video recorded and made available to all attendees as soon as possible, hopefully soon after each 2hr session.
  
Attendees in different time zones will be able to join in to some of these live broadcasts, even if all of them are not convenient times.
For example, attendees from North America may be able to join the live sessions at 1pm-3pm and 4pm-6pm, and then catch up with the 10am-12pm recorded session when it is uploaded (which will be soon after 6pm each day). 
By joining any live sessions that are possible, this will allow attendees to benefit from asking questions and having discussions, rather than just watching prerecorded sessions. 
In addition, most of the fifth and final day of the course is an open session that allows for any spill-over of content, any revision or repetition of topics, and will provide an opportunity for any remaining questions or discussions.

Although not strictly required, using a large monitor or preferably even a second monitor will make the learning experience better, as you will be able to see my RStudio and your own RStudio simultaneously.


## Assumed quantitative knowledge

We assume familiarity with linear regression analysis, and the major concepts of classical inferential statistics (p-values,
hypothesis testing, confidence intervals, model comparison, etc). Some
familiarity with common generalized linear models such as logistic or Poisson
regression will also be assumed.


## Assumed computer background

R experience is desirable but not essential. Although we will be using R extensively, all the code that we use will be made available, and so attendees will just to add minor modifications to this code. Attendees should install R and RStudio on their own computers before the workshops, and have some minimal familiarity with the R environment. 

## Equipment and software requirements

A computer with a working version of R or RStudio is required. R and RStudio are both available as free and open source software for PCs, Macs, and Linux computers. R may be downloaded by following the links here https://www.r-project.org/. RStudio may be downloaded by following the links here: https://www.rstudio.com/.
All the R packages that we will use in this course will be possible to download and install during the workshop itself. In some cases, some additional open-source software, for example Stan, will need to be installed to use some R packages. 

Full installation instructions can be found [here](software.md).


# Course programme 

## Monday

* Topic 1: *General and generalized linear models, including multilevel models*. In order to provide a solid foundation for the remainder of the course, we begin by providing a comprehensive practical and theoretical overview of the principles of general and generalized linear models, also covering their multilevel (or hierarchical) counterparts. General and generalized linear models provide a powerful set of tools for statistical modelling, which are extremely widely used and widely applicable. Their underlying theoretical principles are quite simple and elegant, and once understood, it becomes clear how these models can be extended in many different ways to handle different statistical modelling situations.
  
For this topic, we will use the very commonly used R tools such as `lm`, `glm`, `lme4::lmer`, `lme4::glmer`. In addition, we will also use the R based `brms` package, which uses the `Stan` probabilistic programming language. This package allows us to peform all the same analyses that are provided by `lm`, `glm`, `lmer`, `glmer`, etc., using an almost identical syntax, but also us to perform a much wider range of general and generalized linear model analyses.

## Tuesday

Having established a solid regresion modelling foundation, on the second day we may cover a range of nonlinear modelling extensions to the general and generalized linear modelling framework.

* Topic 2: *Parametric nonlinear regression*. In some cases of nonlinear
  regression, a bespoke parametric function for the relationship between the
  predictors and outcome variable is used. These are often obtained from scientific knowledge of the problem at hand. In R, we can use the package `nls` to perform parametric nonlinear regression. 

* Topic 3: *Polynomial regression*. Polynomial regression is both a conceptually and practically simple extension of linear modelling. It be easily accomplished using the `poly` function along with tools like `lm`, `glmer`,  `lme4::lmer`, `lme4::glmer`.

* Topic 4: *Spline regression*: Nonlinear regression using splines is a powerful and flexible non-parametric or semi-parametric nonlinear regression method. It is also an example of a basis function regression method. Here, we will cover spline regression using the `splines::bs` and `splines::ns` functions that can be used with `lm`, `glm`,  `lme4::lmer`, `lme4::glmer`, `brms`, etc.

* Topic 5: *Radial basis functions*. Regression using radial basis functions is a set of methods that are closely related to spline regression. They have a long history of usage in machine learning. Our coverage of radial basis function will connect to our coverage of splines, generalized additive models, and Gaussian processes.

<!--* Topic 7: *Multilayer perceptron*. Closely related to radial basis functions are multilayer perceptrons. These and their variants and extensions are major building block of deep learning (machine learning) methods. We will explore multilayer perceptron in Stan, but we will also use the powerful `Keras` library.-->

## Wednesday

* Topic 6: *Generalized additive models*. We now turn to the major topic of generalized additive models (GAMs). GAMs generalize many of concepts and topics covered so far and represent a powerful and flexible framework for nonlinear modelling. In R, the `mgcv` package provides a extensive set of tools for working with GAMs. Here, we will provide an in-depth coverage of `mgcv` including choosing smooth terms, controlling overfitting and complexity, prediction, model evaluation, and so on.
* Topic 7: *Generalized additive mixed models*. GAMs can also be used in linear mixed effects models where they are known as generalized additive mixed models (GAMMs). GAMMs can also be used with the `mgcv` package.
* Topic 8: *Interaction nonlinear regression*: A powerful feature of GAMs and GAMMs is the ability to model nonlinear interactions, whether between two continous variables, or between one continuous and one categorical variable. Amongst other things, interactions between continuous variables allow us to do spatial and spatio-temporal modelling. Interactions between categorical and continuous variables allow us to model how nonlinear relationships between a predictor and outcome change as a function of the value of different categorical variables.

## Thursday

<!--* Topic 9: *Nonlinear regression for time-series and forecasting*. One major application of nonlinear regression is for modelling time-series and forecasting. Here, we will explore the `prophet` library for time-series forecasting. This library, available for both Python and R, gives us a GAM-like framework for modelling time-series and making forecasts.-->

* Topic 9: *Statistical modelling with Stan*. Stan is a probabilistic programming language and provides us with an extremely general and powerful means to do statistical modelling. Using Stan allows us to build any statistical models that we can define formally. Here, we will cover how to implement in Stan the models that we have covered thus far. We will begin with simple models and build towards more complex ones include generalized additive models. 

## Friday

* Topic 10: *Gaussian process regression*. Our final topic deals with a type of Bayesian nonlinear regression known as Gaussian process regression. Gaussian process regression can be viewed as a kind of basis function regression, but with an infinite number of basis functions. In that sense, it generalizes spline, radial basis functions, multilayer perceptron, generalized additive models, and provides means to overcome some practically challenging problems in nonlinear regression such as selecting the number and type of smooth functions. Here, we will explore Gaussian process regression using `Stan`.

* Topic 11: *Open/free session*. The remainder of the final day will be an open session. We will
  cover any topic that attendees would like to cover. This could include going
  deeper into some topic already covered, or else introducing new Python topics
  that we did not cover. We can also deal with specific Python related problems
  that attendees have, and these can be in one-to-one meetings or with the
  entire group.

# Evaluation of the course

This is optional, but if you would like to comment on the course, that would be greatly appreciate. This [anonymous survey](https://tinyurl.com/gnam02feedback) asks just two simple questions, so could take you just a matter of seconds.