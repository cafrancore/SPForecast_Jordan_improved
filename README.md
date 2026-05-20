# SPForecast_Jordan_improved

Shiny application for the Jordan forecast workflow. The app starts from [app.R](app.R) and loads shared setup from [global.R](global.R).

## What you need

- R installed on your machine.
- A working internet connection the first time you set up packages.
- The data and dictionary files already included in this repository.

## First-time setup (RStudio)

For final users, use RStudio for first-time setup.

1. Open the project folder in RStudio.
2. Run:

```r
source("install_first_time.R")
```

3. After setup finishes, run:

```r
source("run_app.R")
```

This setup is one-time. The app will not install packages on every launch.

## Run from RStudio

After first-time setup, open the project in RStudio and run:

```r
source("run_app.R")
```

## Run from VS Code

The simplest way is to run the app from an R session inside VS Code and let Shiny open the browser automatically.

1. Open this folder in VS Code.
2. Open the integrated terminal.
3. Start R.
4. Run:

```r
source("run_app.R")
```

That launches the app in your default browser.

If `R` is not recognized in the terminal, start it directly with the installed Windows path:

```powershell
& "$env:LOCALAPPDATA\Programs\R\R-4.5.2\bin\x64\R.exe"
```

Then, inside R, run:

```r
source("run_app.R")
```

If you want to skip the interactive R console and launch the app directly, run:

```powershell
& "$env:LOCALAPPDATA\Programs\R\R-4.5.2\bin\x64\Rscript.exe" run_app.R
```

If you prefer to run the main app directly, use:

```r
shiny::runApp(".", launch.browser = TRUE)
```

## Helper file

[`run_app.R`](run_app.R) is included as a one-line launcher so you do not need to remember the startup command.

## Notes

- `global.R` uses the local `r-lib/` folder and checks package availability only.
- Package installation is handled by `install_first_time.R` (one-time).
- If the app stops with a missing file error, check that the dictionary file is present at `Input/Dictionary/DictionaryR.xlsx`.

## Project layout

- `app.R` - main Shiny app
- `global.R` - package setup and shared data paths
- `Input/` - UI/server scripts, dictionary files, GIS assets, logos, and models
- `Data/` - model and data inputs used by the app
- `chunks/` - supporting analysis scripts
