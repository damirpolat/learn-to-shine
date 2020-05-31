# helpers.R
# Damir Pulatov

# combine both mcp and par scores 
mcp_par = function(ids, penalties1, penalties2, par1, par2) {
  x_mcp = cbind.data.frame(x = penalties1)
  y_mcp = cbind.data.frame(y = penalties2, method = "mcp")
  x_par = cbind.data.frame(x = par1)
  y_par = cbind.data.frame(y = par2, method = "par10")
  
  x = rbind(x_mcp, x_par)
  y = rbind(y_mcp, y_par)
  
  data = data.frame(instance_id = ids)
  data = cbind.data.frame(data, x, y)
  return(data)
}
