
linreg <- setRefClass("linreg", 
                      fields = list(formula = "formula", data = "data.frame", X = "array"),
                      methods = list(
                        initialize = function(formula, data){
                          print("User::initialize")
                          X <<- model.matrix(formula, data = data)
                          print(X)
                        },
                        
                        

                        test = function(){
                          f <- testfunction(formula, data)
                          print(f)
                          print(X)
                        }
                          
                          
                      )
                      
                      
                      
                      
)










