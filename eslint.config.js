import eslint from "@eslint/js";
import eslintConfigPrettier from "eslint-config-prettier";
import globals from "globals";
import tseslint from "typescript-eslint";

const javascriptFiles = ["**/*.{js,jsx,cjs,mjs}"];
const typescriptFiles = ["**/*.{ts,tsx,cts,mts}"];
const scriptExtensions = "{js,jsx,cjs,mjs,ts,tsx,cts,mts}";

const recommendedJavaScriptFiles = [
  "commitlint.config.js",
  "version.js",
  "version_manager/**/*.js",
  "tests/ui/**/*.js",
  "electron/**/*.{js,cjs,mjs}",
];

const nodeFiles = [
  `*.${scriptExtensions}`,
  `version_manager/**/*.${scriptExtensions}`,
  `tests/ui/**/*.${scriptExtensions}`,
  `electron/electron.vite.config.${scriptExtensions}`,
  `electron/src/main/**/*.${scriptExtensions}`,
];

export default [
  {
    ignores: [
      "**/node_modules/**",
      "**/_archives/**",
      "**/_shared/**",
      "**/archives/**",
      "electron/dist/**",
      "electron/out/**",
      "tests/_output/**",
      "www/modules/el/index.js",
      "www/modules/jquery-ui/jquery-ui.min.js",
      "www/modules/marked/marked.min.js",
      "electron/src/main/docker/index.js",
      "electron/src/main/modules/translate/index.js",
    ],
  },
  {
    ...eslint.configs.recommended,
    files: recommendedJavaScriptFiles,
  },
  ...tseslint.configs.recommended.map((config) => ({
    ...config,
    files: typescriptFiles,
  })),
  eslintConfigPrettier,
  {
    files: [...javascriptFiles, ...typescriptFiles],
    languageOptions: {
      ecmaVersion: "latest",
      sourceType: "module",
    },
    rules: {
      curly: ["error", "all"],
    },
  },
  {
    files: nodeFiles,
    languageOptions: {
      globals: globals.node,
    },
  },
  {
    files: [
      `electron/src/preload/**/*.${scriptExtensions}`,
      `electron/src/renderer/**/*.${scriptExtensions}`,
      `tests/ui/**/*.${scriptExtensions}`,
      `www/**/*.${scriptExtensions}`,
    ],
    languageOptions: {
      globals: globals.browser,
    },
  },
];
