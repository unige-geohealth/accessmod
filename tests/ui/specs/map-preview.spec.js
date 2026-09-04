import { expect, test } from "@playwright/test";

test.use({ deviceScaleFactor: 2 });

const transparentPng = Buffer.from(
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M/wHwAF/gL+Xw3ZAAAAAElFTkSuQmCC",
  "base64",
);

const demo = {
  west: 33.7868,
  south: -16.5328,
  east: 35.9198,
  north: -14.7441,
};

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

async function setSelectize(page, id, value) {
  await page.waitForFunction(
    ({ inputId, inputValue }) => {
      const control = document.getElementById(inputId)?.selectize;
      return Boolean(control?.options?.[inputValue]);
    },
    { inputId: id, inputValue: value },
    { timeout: 60_000 },
  );
  await page.evaluate(
    ({ inputId, inputValue }) => {
      document.getElementById(inputId).selectize.setValue(inputValue);
    },
    { inputId: id, inputValue: value },
  );
}

async function leafletState(page) {
  return page.locator("#mapPreview").evaluate((element) => {
    const map = window.jQuery(element).data("leaflet-map");
    if (!map) {
      return null;
    }
    const bounds = map.getBounds();
    const center = map.getCenter();
    return {
      west: bounds.getWest(),
      south: bounds.getSouth(),
      east: bounds.getEast(),
      north: bounds.getNorth(),
      center: { lat: center.lat, lng: center.lng },
      zoom: map.getZoom(),
    };
  });
}

function expectDemoExtent(state) {
  expect(state).not.toBeNull();
  expect(state.zoom).toBeGreaterThanOrEqual(5);
  expect(state.west).toBeLessThanOrEqual(demo.west);
  expect(state.east).toBeGreaterThanOrEqual(demo.east);
  expect(state.south).toBeLessThanOrEqual(demo.south);
  expect(state.north).toBeGreaterThanOrEqual(demo.north);
  expect(state.east - state.west).toBeLessThan(10);
  expect(state.north - state.south).toBeLessThan(10);
  expect(state.center.lng).toBeGreaterThan(demo.west);
  expect(state.center.lng).toBeLessThan(demo.east);
  expect(state.center.lat).toBeGreaterThan(demo.south);
  expect(state.center.lat).toBeLessThan(demo.north);
}

async function expectRetinaTiles(page, mapSelector) {
  await expect
    .poll(
      () =>
        page
          .locator(`${mapSelector} img.leaflet-tile`)
          .evaluateAll((images) =>
            images.some((image) => /@2x\.(png|jpg)(?:\?|$)/.test(image.src)),
          ),
      {
        timeout: 60_000,
        message: `${mapSelector} should request MapTiler @2x tiles on a HiDPI display`,
      },
    )
    .toBe(true);
}

test("demo map keeps its project extent and renders the selected raster", async ({
  page,
}, testInfo) => {
  const liveMapTiler = process.env.UI_TEST_LIVE_MAPTILER === "1";
  const startedAt = Date.now();
  const timings = {};
  const pageErrors = [];
  const failedRequests = [];
  const mapTilerResponses = [];

  page.on("pageerror", (error) => pageErrors.push(error.message));
  page.on("requestfailed", (request) => {
    const errorText = request.failure()?.errorText || "unknown failure";
    if (
      request.url().startsWith("https://api.maptiler.com/") &&
      errorText.includes("ERR_ABORTED")
    ) {
      return;
    }
    failedRequests.push(`${request.method()} ${request.url()}: ${errorText}`);
  });
  page.on("response", (response) => {
    if (response.url().startsWith("https://api.maptiler.com/")) {
      mapTilerResponses.push({
        status: response.status(),
        url: response.url(),
      });
    }
  });

  if (!liveMapTiler) {
    await page.route("https://api.maptiler.com/**", async (route) => {
      await route.fulfill({
        status: 200,
        contentType: "image/png",
        body: transparentPng,
      });
    });
  }

  await openShiny(page);
  await waitForShiny(page);
  timings.shinyReadyMs = Date.now() - startedAt;

  await setSelectize(page, "selectProject", "demo");
  await expect(page.locator("#projName")).toHaveText("demo", {
    timeout: 60_000,
  });
  timings.projectReadyMs = Date.now() - startedAt;

  await expect(page.locator("#mapProject")).toBeVisible({ timeout: 60_000 });
  await expectRetinaTiles(page, "#mapProject");

  await page.locator('a[data-value="module_toolbox"]').click();
  await expect(page.locator("#mapPreview")).toBeVisible({ timeout: 60_000 });
  await expect
    .poll(() => leafletState(page), { timeout: 60_000 })
    .not.toBeNull();
  await expect
    .poll(async () => (await leafletState(page))?.zoom, { timeout: 60_000 })
    .toBeGreaterThanOrEqual(5);

  const initialState = await leafletState(page);
  expectDemoExtent(initialState);
  timings.projectExtentMs = Date.now() - startedAt;

  const rasterValue = await page
    .locator("#selectRasterToMap")
    .evaluate((element) => {
      const options = Object.values(element.selectize?.options || {});
      return options.find((option) => {
        const label = String(option.text || option.label || "").toLowerCase();
        return label.includes("priority") && label.includes("demo");
      })?.value;
    });
  expect(rasterValue, "priority [demo] must be available").toBeTruthy();
  await setSelectize(page, "selectRasterToMap", rasterValue);

  await expect
    .poll(async () => (await leafletState(page))?.zoom, {
      timeout: 60_000,
      message: "Selecting a raster must not reset the map to world extent",
    })
    .toBeGreaterThanOrEqual(5);

  const overlay = page.locator("#mapPreview img.leaflet-image-layer");
  await expect(overlay).toBeVisible({ timeout: 60_000 });
  await expect
    .poll(
      () =>
        overlay.evaluate((image) => image.complete && image.naturalWidth > 0),
      {
        timeout: 60_000,
      },
    )
    .toBe(true);
  timings.rasterVisibleMs = Date.now() - startedAt;

  await setSelectize(page, "selBaseMap", "dark");
  await expect
    .poll(async () => page.locator("#selBaseMap").inputValue())
    .toBe("dark");
  await expect(overlay).toBeVisible();
  const stateAfterBasemapChange = await leafletState(page);
  expectDemoExtent(stateAfterBasemapChange);
  expect(
    Math.abs(stateAfterBasemapChange.center.lat - initialState.center.lat),
  ).toBeLessThan(0.01);
  expect(
    Math.abs(stateAfterBasemapChange.center.lng - initialState.center.lng),
  ).toBeLessThan(0.01);
  expect(stateAfterBasemapChange.zoom).toBe(initialState.zoom);

  if (liveMapTiler) {
    await expect
      .poll(
        () =>
          mapTilerResponses.some(({ status }) => status >= 200 && status < 300),
        {
          timeout: 60_000,
          message: "MapTiler should return at least one successful tile",
        },
      )
      .toBe(true);
  } else {
    const tileSources = await page
      .locator("#mapPreview img.leaflet-tile")
      .evaluateAll((images) => images.map((image) => image.src));
    expect(
      tileSources.some(
        (url) =>
          url.includes("api.maptiler.com") && url.includes("key=ui-test-key"),
      ),
    ).toBe(true);
  }
  await expectRetinaTiles(page, "#mapPreview");

  await testInfo.attach("timings", {
    body: JSON.stringify(timings, null, 2),
    contentType: "application/json",
  });

  expect(
    pageErrors,
    `Uncaught browser errors:\n${pageErrors.join("\n")}`,
  ).toEqual([]);
  expect(
    failedRequests,
    `Failed browser requests:\n${failedRequests.join("\n")}`,
  ).toEqual([]);
});
