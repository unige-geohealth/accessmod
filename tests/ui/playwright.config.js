import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "@playwright/test";

const configDirectory = path.dirname(fileURLToPath(import.meta.url));
const repositoryRoot = path.resolve(configDirectory, "../..");
const outputRoot = process.env.UI_TEST_OUTPUT_DIR
  ? path.resolve(process.env.UI_TEST_OUTPUT_DIR)
  : path.join(repositoryRoot, "tests", "_output", "ui");

export default defineConfig({
  testDir: "./specs",
  timeout: 180_000,
  expect: {
    timeout: 60_000,
  },
  fullyParallel: false,
  workers: 1,
  retries: 0,
  outputDir: `${outputRoot}/results`,
  reporter: [
    ["line"],
    ["html", { outputFolder: `${outputRoot}/report`, open: "never" }],
    ["json", { outputFile: `${outputRoot}/results.json` }],
  ],
  use: {
    baseURL: process.env.UI_TEST_BASE_URL || "http://shiny:3100",
    browserName: "chromium",
    headless: true,
    launchOptions: {
      args: ["--no-proxy-server"],
    },
    viewport: { width: 1600, height: 1000 },
    screenshot: "only-on-failure",
    trace: "retain-on-failure",
  },
});
