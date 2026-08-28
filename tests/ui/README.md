# Browser UI tests

The UI suite runs both AccessMod and Chromium in a dedicated Docker Compose
project. It does not use the development Compose services or their volumes.

```sh
npm run test:ui       # deterministic MapTiler response, no key or network needed
MAPTILER_API_KEY=... npm run test:ui:live  # opt-in real MapTiler request
```

The Shiny app is exposed at `http://localhost:3280` while the test is running.
Set `UI_TEST_PORT` to use another host port. Failures retain screenshots,
Playwright traces, reports, and Shiny logs under `tests/_output/ui/`.

The runner owns only the `accessmod_ui_test` Compose project. Its cleanup must
remain scoped to that project so development containers and volumes are never
removed.
