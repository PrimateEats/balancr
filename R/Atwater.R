#' Atwater Conversion
#' 
#' This function converts fat, carbohydrate, and protein values from a DM basis to metabolizable energy based on general Atwater factors
#' All you need to do is input 3 nutrients and a grouping variable, and voila!
#' This function can be used to convert/prepare your data for further geometric analysis
#' @param data the dataframe holding your data
#' @param carb Carbohydrate/TNC values on a DM basis. Atwater factor = 4
#' @param fat Fat/Lipid values on a DM basis. Atwater factor = 9 
#' @param protein Protein/CP values on a DM basis. Atwater factor = 4
#' @param id an optional argument. Grouping variable or other indicator of multiple groups within your dataset.
#' @return dataframe of ME values from DM values, based on general Atwater factors
#' @examples 
#' Atwater(data=gorillafood,carb=TNC,fat=LIP,protein=CP,id=GROUP)
#' @export


Atwater=function(carb,fat,protein,id,data){
  if(missing(id)){
  arguments=as.list(match.call())
  carb=eval(arguments$carb,data)
  fat=eval(arguments$fat,data)
  protein=eval(arguments$protein,data)
  id=eval(arguments$id,data)
  carb=carb*4
  fat=fat*9
  protein=protein*4
  df=data.frame(id,carb,fat,protein)
  }
  else{
    arguments=as.list(match.call())
  carb=eval(arguments$carb,data)
  fat=eval(arguments$fat,data)
  protein=eval(arguments$protein,data)
  id=eval(arguments$id,data)
  carb=carb*4
  fat=fat*9
  protein=protein*4
  df=data.frame(id,carb,fat,protein)
  }
}