$compress = @{
  Path = "birdflu.nlogo", "utilities.nls", "intersections.txt", "roads.txt", "baseline.ini", "birdflu6.ps1", "birdflu-experiments.xml"
  CompressionLevel = "Fastest"
  DestinationPath = "birdflu.zip"
}
Compress-Archive -Force @compress


