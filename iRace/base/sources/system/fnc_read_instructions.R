#/////////////////////////////////////////////////////////////////
#
# read_instructions: script which read the parameters to the model
#
#/////////////////////////////////////////////////////////////////

fnc_read_instructions=function()
{ cat("\t\t\t- Reading instructions...\n")
  options(warn=-1)
  instructions=read_yaml("instructions.yml",fileEncoding = "UTF-8")
  options(warn=0)
  return(instructions)
}