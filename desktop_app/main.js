const { app, BrowserWindow } = require("electron");
const { exec } = require("child_process");
const fs = require("fs");
const path = require("path");

function waitForPort(filePath, callback) {
  const interval = setInterval(() => {
    if (fs.existsSync(filePath)) {
      const port = fs.readFileSync(filePath, "utf8").trim();
      clearInterval(interval);
      callback(port);
    }
  }, 500);
}

function createWindow() {
  const batPath = path.join(__dirname, "run_app.bat");

  // Start Shiny app
  const shinyProcess = exec(`"${batPath}"`);

  // Path to port.txt
  const portFile = path.join(__dirname, "backtesting_trading_strategies_shinyapp", "port.txt");

  // Wait until port.txt exists
  waitForPort(portFile, (port) => {
    const win = new BrowserWindow({ width: 1300, height: 900 });
    win.loadURL(`http://127.0.0.1:${port}`);
    win.on("closed", () => shinyProcess.kill());
  });
}

app.whenReady().then(createWindow);

app.on("window-all-closed", () => {
  if (process.platform !== "darwin") app.quit();
});