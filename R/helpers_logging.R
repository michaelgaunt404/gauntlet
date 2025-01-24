
gntlt_error_msg = function(err) {
  message("Error occurred:")
  message(err$message)
  message("Returning NA value...")
  return(NA)
}

#end
