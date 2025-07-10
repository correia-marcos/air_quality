# RSelenium Setup Guide

> ⚙️ This guide shows you how to get RSelenium running **locally** on Windows/macOS/Linux _or_ in a Docker container.

---

## 1. Download the Selenium Standalone JAR

1. Go to the official Selenium downloads page:  
   https://www.selenium.dev/downloads/

2. Under “Selenium Server (Grid)”, click the latest **Standalone Server** link.  
3. Save the JAR (e.g. `selenium-server-4.34.0.jar`) somewhere on your machine, e.g.:

   - **Windows**: `C:\selenium\selenium-server-4.34.0.jar`  
   - **macOS/Linux**: `~/Downloads/selenium-server-4.34.0.jar`

4. (Optional) Move it into a standard location, e.g.  

   ```bash
   mkdir -p ~/.selenium
   mv ~/Downloads/selenium-server-4.34.0.jar ~/.selenium/


## 2. Store your JAR path in `~/.Renviron`

Rather than manually exporting each shell session, you can add the JAR location once to your R environment file:

1. Open (or create) `~/.Renviron` in your editor.
2. Add a line with the full path to your Selenium JAR, for example:

   ```env
   SELENIUM_JAR=/Users/yourname/.selenium/selenium-server-4.34.0.jar