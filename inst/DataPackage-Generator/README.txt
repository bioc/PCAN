The scripts and the generated package depend on the follwing packages:
    - XML
    - RSQLite
    - PNG
    - roxygen2

The generation of the package is a 2 steps process:
    - SA00-Gathering-Data.R will gather all the data from the
        different source and parse them into relevant R objects.
        These objects will be stored in a directory.
    - SB00-PackageGenerator.R will create the package as such.
        
The data model is available in the Documentation directory
and is also provided in the generated package:
run getDataModel() once the generated package is installed.
