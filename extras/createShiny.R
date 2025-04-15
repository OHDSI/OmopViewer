
devtools::load_all()
exportStaticApp(
  result = omopViewerResults,
  directory = file.path(getwd(), "extras"),
  background = file.path(getwd(), "extras", "backgroundExample.md")
)
