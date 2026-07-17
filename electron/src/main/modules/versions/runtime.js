import semver from "semver";

const MODERN_RUNTIME_FIRST_VERSION = "5.9.0-alpha.4";

export function usesLegacyRuntime(version) {
  const parsed = semver.valid(version);

  return parsed
    ? semver.lt(parsed, MODERN_RUNTIME_FIRST_VERSION, {
        includePrerelease: true,
      })
    : false;
}
