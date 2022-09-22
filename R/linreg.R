
linreg <- setRefClass("linreg", 
                      fields = list(formula = "formula", data = "data.frame", X = "array", y = "array",
                                    beta_hat = "array", y_hat = "array", e_hat = "array",
                                    n = "integer", p = "integer", df = "integer",
                                    sigma_hat_sqr = "array", var_beta_hat = "array", 
                                    t_beta = "array", p_beta = "array",
                                    data_name = "character"
                                    ),
                      methods = list(
                        initialize = function(formula, data){
                          formula <<- formula
                          data <<- data
                          data_name <<- deparse(substitute(data))
                          X <<- model.matrix(formula, data = data)
                          # matrix X (independent variables)
                          y <<- as.matrix(data[all.vars(formula)[1]])
                          # dependent variable y
                          beta_hat <<- solve(t(X) %*% X) %*% t(X) %*% y 
                          # Regressions coefficients: coef()
                          y_hat <<- X %*% beta_hat
                          # The fitted values pred()
                          e_hat <<- y - y_hat
                          # The residuals: resid()
                          n <<- nrow(X)
                          # the number of observations 
                          # X has to be vertical
                          p <<- length(all.vars(formula)) 
                          # the number of parameters 
                          df <<- n - p
                          # The degrees of freedom:
                          sigma_hat_sqr <<- (t(e_hat) %*% e_hat) / df
                          # The residual variance:
                          var_beta_hat <<- as.numeric(sigma_hat_sqr) * solve(t(X) %*% X)
                          # The variance of the regression coefficients:
                          t_beta <<- beta_hat / sqrt(diag(var_beta_hat))
                          # The t-values for each coefficient:
                          p_beta <<- pt(beta_hat, df)
                          # the p-values for each regression coefficient
                        },
                        print = function(){
                          cat("linreg(formula = ", deparse(formula), ", data = ", data_name, ")", sep = "")
                          
                          base::print(beta_hat)
                          # print out the coefficients and coefficient names
                        },
                        plot = function(){
                          
                        },
                        resid = function(){
                          return(as.vector(e_hat))
                        },
                        pred = function(){
                          return(y_hat)
                        },
                        coef = function(){
                          return(beta_hat)
                        },
                        plot = function(){
                          data("iris")
                          pointlabel <- array(rep("", 150))
                          pointlabel[c(99, 118, 119)] <- c(99, 118, 119)
                          lmplot <- lm(formula = Petal.Length ~ Species, data = iris)
                          data_plot <- data.frame(v1 = round(lmplot$fitted.values, digits = 5), v2 = lmplot$residuals, v3 = pointlabel)
                          
                          level_x <- as.numeric(levels(factor(data_plot[, 1])))
                          median_y <- array()
                          j <- 1
                          for (i in level_x) {
                            median_y[j] <- median(data_plot[data_plot[, 1] == i, 2])
                            j <- j + 1
                          }
                          data_median <- as.data.frame(cbind(median_y, level_x))
                          t1 <- ggplot(data_plot, aes(data_plot[, 1], data_plot[, 2])) +
                            geom_point( shape=1) +
                            geom_text(aes(label = data_plot[, 3]), nudge_x = -0.2) +
                            scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, 1)) +
                            geom_line(data = data_median, aes(level_x, median_y), colour = "red")+
                            xlab("Fitted values\nlm(Petal.Length ~ Species)") +
                            ylab("Residuas") + 
                            theme(plot.title = element_text(hjust = .5) ) +
                            ggtitle("Residuals vs Fitted", subtitle = NULL ) 
                          
                          return(t1)
                            
                        },
                        summary = function(){
                          base::print(beta_hat)
                          cat("Residual standard error: ", sqrt(diag(var_beta_hat)), " on ", df, " degrees of freedom", sep = "")
                          cat("\n")
                          base::print("t_beta = ")
                          base::print(t_beta)
                          base::print("p_beta = ")
                          base::print(p_beta)
                          base::print(sigma_hat_sqr)
                        }
  
                      )
                      
)










