# start app
#install.packages('devtools')
devtools::load_all(path = "../metamet")
shiny::runApp(system.file("shinyApp/metqc_app", package = "metamet"))

# cd "C:\Users\clacap\OneDrive - UKCEH\metamet"


# load main or ranches
#pak::pak("user/repo@branch-name")

# 21/04/2026, CC: fixed location of dt_ default files in extdata to avoid issues
# created new UI element and option for Custom. however at the moment this shows choices of column names (name_era5, name_icos),
# rather than the values contained in such cols

# edit the current code so that Naming convention selectInput includes an extra option called Custom.
# if custom is selected, then an extra UI element called Custom variable selection will appear.
# essentially when Custom is selected this custom variable selection table will allow the user to select a variable name
# from a dropdown menu for each column of the raw data file uploaded (the columns site and name won't be changed)
# the variable names to be included in the dropdown menu are all the unique variable names, which appear as values
# in the columns of the dt_meta file which start with name_ (exclude the columns name_dt and name_local, as these are the names of the variables in the dt and dt_qc tables respectively, and we want to avoid confusion for user name_)
# because handling of custom metadata is still to be handled, for now, if a user selects "Custom" in naming convention
# no metamet object will be created, and instead a simple table will be shown with the columns site, TIMESTAMP, and the variable names as selected by the user in the custom variable selection table, and a message will be shown to the user that custom metadata handling is still to be implemented, and that this is just a preview of how the data will look once custom metadata handling is implementedable
# once the selection of all variables is made and confirmed with a button, the table Mapped data preview will update to show the data as it currently is in the code

git checkout branch-issue-28

to confirm it has worked, type:

  git status

and the result should be: On branch my-issue-28

Check if there are updates on your current branch typing:

  git pull

if nothing has changed it will say “Already up to date.” Which it’s good and means you can go ahead and apply your updates.

If not something like this will appear:

  Updating …..

Etc etc etc

1 file changed, 5 insertions(+), 5 deletions(-)

This is also fine it means everything went well and you can go ahead

Process the updates

type this to add all updated files to the current git branch:

  git add .

describe your changes (I generally use this structure):

  git commit -m "date, initials: my brief description of changes"

“push” or apply the changes to the repo (if you are using the main branch you will type master instead of branchnamehere)L

git push -u origin my-issue-28


##### create new branch
Set path: open gitbash and navigate to pkg folder e.g. type cd your/path

cd C:/Users/clacap/Documents/ecowings

Create and name a branch

My suggestion would be to either use these conventions:
  a) for new features:

  experiment/2-new-plot

b) for addressing bugs or issues reported by users

issues/28-fix-function

the full syntax will be:

  git checkout -b issues/28-fix-function


#
# feature Implement colourblind-friendly colour scales #48

# features/48-implement-colourblind-palettes-
Make sure variable plots and validation calendars are on a colourblind friendly colour scale.
Especially relevant for multi-site, multi-variable plots.
