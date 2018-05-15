.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(paste0("Bayes Kurs ", utils::packageVersion("bayeskurs")))
}
