# Power ShinyApp

The Power ShinyApp is a web-based interactive application developed using R’s Shiny framework.
The developed app is designed to perform statistical power analysis for functional data using multiple statistical methods.
The application supports multiple data inputs, interactive visualisation, and statistical power computation to provide a flexible tool for simulation-based power analysis. 


---

## Quick Start (TL;DR)

```bash
# 1) Install R and RStudio (see detailed instructions below if you need help)

# 2) Get the code
# Option A: clone with Git
git clone https://github.com/mr-seydi/PowerShinyApp.git
cd PowerShinyApp

# Option B: download ZIP from GitHub, then unzip and cd into the folder

# 3) Open in RStudio and install packages
# In R console:
install.packages(c("shiny"))  # or use renv::restore() if renv.lock exists

# 4) Run the app

## From RStudio

1. Double-click `Power_ShinyApp.Rproj` to open the project.
2. In the Console, run: shiny::runApp()
Shiny will automatically find `ui.R` and `server.R` in the project root.

## From Terminal

# Since this app has ui.R and server.R in the project root:
R -e "shiny::runApp('.', host='127.0.0.1', port=3838)"
```

---

## Table of Contents

* [Requirements](#requirements)
* [Install R](#install-r)

  * [Windows](#windows)
  * [macOS](#macos)
  * [Linux](#linux)
* [Install RStudio](#install-rstudio)
* [Get the App Code](#get-the-app-code)
* [Set Up R Packages](#set-up-r-packages)

  * [Use `renv` (recommended)](#use-renv-recommended)
* [Run the App](#run-the-app)

  * [From RStudio](#from-rstudio)
  * [From Terminal](#from-terminal)
* [Configuration & Environment](#configuration--environment)
* [Troubleshooting](#troubleshooting)
* [How to Update](#how-to-update)
* [License](#license)

---

## Requirements

* **Operating System:** Windows 10/11, macOS 12+ (Monterey) or newer, or a modern Linux distribution
* **R version:** 4.2 or newer (older *may* work but is not supported)
* **RStudio:** current desktop release
* **Internet:** required on first run for package installation

> Check your R version:
>
> ```r
> R.version.string
> ```

---

## Install R

### Windows

1. Download R from CRAN: [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)
2. Run the installer and accept the defaults unless you need a custom location.
3. (Optional, CLI) If you use [Chocolatey](https://chocolatey.org/), open **Admin PowerShell** and run:

   ```powershell
   choco install r.project -y
   ```

### macOS

1. Download the latest R pkg from CRAN: [https://cran.r-project.org/bin/macosx/](https://cran.r-project.org/bin/macosx/)
2. Double-click the `.pkg` and follow the prompts.
3. (Optional, Homebrew):

   ```bash
   brew install --cask r
   ```

### Linux

* **Ubuntu/Debian:**

  ```bash
  sudo apt update
  sudo apt install -y r-base
  ```
* **Fedora/RHEL/CentOS:**

  ```bash
  sudo dnf install -y R
  ```
* **Arch:**

  ```bash
  sudo pacman -S --noconfirm r
  ```

> For system libraries needed by certain R packages (e.g., `libcurl`, `openssl`, `xml2`), see your distro docs.

---

## Install RStudio

* Download RStudio Desktop (Posit) for your OS: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
* Run the installer (macOS: drag app to Applications; Windows: follow the setup wizard; Linux: install the `.deb`/`.rpm`).
* Launch **RStudio**.

---

## Get the App Code

### Option A — Clone with Git (recommended)

```bash
git clone https://github.com/mr-seydi/PowerShinyApp.git
cd PowerShinyApp
```

### Option B — Download ZIP

1. Go to `https://github.com/mr-seydi/PowerShinyApp`.
2. Click **Code ▸ Download ZIP**.
3. Unzip and open the folder.

> **Project structure (example):**
>
> ```text
> PowerShinyApp/
> ├─ ui.R
> ├─ server.R
> ├─ Power_ShinyApp.Rproj
> ├─ R/                 # helper functions
> ├─ www/               # static assets (css/js/images)
> ├─ data/              # data files (if any)
> ├─ renv.lock          # pinned package versions (if using renv)
> └─ README.md
> ```

---

## Set Up R Packages

Open the project in **RStudio** (double-click `Power_ShinyApp.Rproj`) or set your working directory to the repo root:

```r
setwd("/path/to/PowerShinyApp")
```

### Use `renv` (recommended)

If the repo includes an `renv.lock` file, run:

```r
install.packages("renv")
renv::restore()  # installs exact package versions from renv.lock
```


---

## Run the App

### From RStudio

1. Double-click `Power_ShinyApp.Rproj` to open the project.
2. In the Console, run:

   ```r
   shiny::runApp()
   ```

   Shiny will automatically find `ui.R` and `server.R` in the project root.

### From Terminal

Run Shiny on a specific host/port:

```bash
R -e "shiny::runApp('.', host='0.0.0.0', port=3838)"
```

Then open: [http://localhost:3838](http://localhost:3838)



## Troubleshooting

* **R not found / wrong version**

  * Confirm in R console: `R.version.string`
  * On macOS, if using Homebrew, ensure `/usr/local/bin` or `/opt/homebrew/bin` precedes older R paths.
* **Packages fail to compile on Windows**

  * Install **Rtools** (matching your R version): [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)
* **Packages fail to compile on macOS**

  * Install Xcode Command Line Tools: `xcode-select --install`
* **Linux missing system libs**

  * Install `libcurl`, `openssl`, `libxml2`, `harfbuzz`, `fribidi`, `freetype`, etc., via your package manager.
* **Port already in use**

  * Change port: `shiny::runApp(port = 4242)`
* **App can’t find files**

  * Make sure your working directory is the **project root**. In RStudio: *Session ▸ Set Working Directory ▸ To Project Directory*.
* **White screen / nothing loads**

  * Open the R console for errors; check the browser dev tools (Network/Console). Try `options(shiny.fullstacktrace = TRUE)`.



## How to Update

```bash
cd PowerShinyApp
git pull
# If using renv:
R -e "renv::restore()"
```

---

## License

This project is licensed under the [MIT License](LICENSE) unless otherwise noted.

---

## Citation

If you use this app in academic work, please cite this paper:

Sample Size Estimation for Functional Data Analysis with Web Application

Mohammad Reza Seydi, Johan Strandberg, Todd C. Pataky, and Lina Schelin


---

## Acknowledgments

* Shiny by Posit

---

### Maintainer & Support

* **Maintainer:** \<Reza Seydi>
* **Email:** [mohammad.seydi@umu.se](mailto:mohammad.seydi@umu.se)
* **Issues:** Please open an issue here: `https://github.com/mr-seydi/PowerShinyApp/issues`

