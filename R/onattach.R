.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(paste0("bayes kurs ", utils::packageVersion("bayeskurs")))
}
