import { expect, test } from "@playwright/test";

async function waitForShiny(page) {
  await page.waitForFunction(
    () => {
      const socket = window.Shiny?.shinyapp?.$socket;
      return socket?.readyState === WebSocket.OPEN;
    },
    null,
    { timeout: 60_000 },
  );
  await page.waitForFunction(
    () => !document.documentElement.classList.contains("shiny-busy"),
    null,
    { timeout: 60_000 },
  );
}

async function openShiny(page) {
  let lastError;
  for (let attempt = 0; attempt < 12; attempt += 1) {
    try {
      const response = await page.goto("/", {
        waitUntil: "domcontentloaded",
        timeout: 10_000,
      });
      if (response?.ok()) {
        return;
      }
      lastError = new Error(`Shiny root returned HTTP ${response?.status()}`);
    } catch (error) {
      lastError = error;
    }
    await page.waitForTimeout(1_000);
  }
  throw lastError;
}

test("logs can be cleared after confirmation and refresh immediately", async ({
  page,
}, testInfo) => {
  await openShiny(page);
  await waitForShiny(page);
  await expect(page.locator("#projName")).toHaveText(/demo/i);
  await waitForShiny(page);

  const startedAt = Date.now();
  await page.locator('a[data-value="module_logs"]').click();
  await expect(page.locator("#logsTable .tabulator")).toBeVisible();
  await waitForShiny(page);
  const logsReadyMs = Date.now() - startedAt;

  const downloadBox = await page.locator("#downloadLogs").boundingBox();
  const clearBox = await page.locator("#clearLogs").boundingBox();
  expect(downloadBox).not.toBeNull();
  expect(clearBox).not.toBeNull();
  expect(Math.abs(downloadBox.x - clearBox.x)).toBeLessThan(2);
  expect(Math.abs(downloadBox.width - clearBox.width)).toBeLessThan(2);
  expect(
    Math.abs(downloadBox.y + downloadBox.height - clearBox.y),
  ).toBeLessThan(2);

  await page.locator('#filterLogs input[value="all"]').check();
  await waitForShiny(page);

  await page.locator("#clearLogs").click();
  await expect(page.locator("#clearLogsConfirm")).toBeVisible();
  await page.getByRole("button", { name: "Cancel", exact: true }).click();
  await expect(page.locator("#clearLogsConfirm")).toBeHidden();

  await page.locator("#clearLogs").click();
  await page.locator("#clearLogsConfirm").click();
  await expect(page.getByText("The log history was cleared.")).toBeVisible();
  await expect
    .poll(() => page.locator("#logsTable .tabulator-row").count())
    .toBe(1);
  await expect(page.locator("#logsTable .tabulator-row")).toContainText(
    "Log history cleared by user.",
  );

  await testInfo.attach("logs-timing", {
    body: JSON.stringify({ logsReadyMs }, null, 2),
    contentType: "application/json",
  });
});
