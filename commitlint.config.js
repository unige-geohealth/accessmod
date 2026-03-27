export default {
  extends: ["@commitlint/config-conventional"],
  rules: {
    // Allowed types for this project
    "type-enum": [
      2,
      "always",
      [
        "feat",     // new feature
        "fix",      // bug fix
        "docs",     // documentation only
        "style",    // formatting, no logic change
        "refactor", // neither fix nor feature
        "test",     // adding or updating tests
        "chore",    // build, deps, tooling
        "perf",     // performance improvement
        "ci",       // CI/CD changes
        "revert",   // revert a previous commit
      ],
    ],
    // Body and footer are optional
    "body-max-line-length": [1, "always", 200],
  },
};
