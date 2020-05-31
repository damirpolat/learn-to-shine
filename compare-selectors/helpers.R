# helpers.R
# Damir Pulatov

# build data for scatter plot
build_data = function(ids, penalties1 = NULL, penalties2 = NULL, par1 = NULL, par2 = NULL) {
  # data for mcp 
  if(!is.null(penalties1) && !is.null(penalties2)) {
    x_mcp = cbind.data.frame(x = penalties1)
    y_mcp = cbind.data.frame(y = penalties2, method = "mcp")
    
    data = data.frame(instance_id = ids, x = penalties1, y = penalties2, method = "mcp")
  } 
  
  # data for par10
  if (!is.null(par1) && !is.null(par2)) {
    x_par = cbind.data.frame(x = par1)
    y_par = cbind.data.frame(y = par2, method = "par10")
    
    data = data.frame(instance_id = ids, x = par1, y = par2, method = "par10")
  }
  
  # data when both mcp and par10 are selected
  if(!is.null(penalties1) && !is.null(penalties2) 
     && !is.null(par1) && !is.null(par2)) {
    
    x = rbind(x_mcp, x_par)
    y = rbind(y_mcp, y_par)
    
    data = data.frame(instance_id = rep(ids, 2))
    data = cbind.data.frame(data, x, y)
  }
  
  return(data)
}
