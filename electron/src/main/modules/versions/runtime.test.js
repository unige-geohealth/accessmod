import assert from "node:assert/strict";
import test from "node:test";
import { usesLegacyRuntime } from "./runtime.js";

test("uses the legacy runtime for AccessMod 5.7 and 5.8", () => {
  assert.equal(usesLegacyRuntime("5.7.17"), true);
  assert.equal(usesLegacyRuntime("5.8.2"), true);
  assert.equal(usesLegacyRuntime("5.8.3-beta.1"), true);
});

test("switches to the modern runtime at 5.9.0-alpha.4", () => {
  assert.equal(usesLegacyRuntime("5.9.0-alpha.3"), true);
  assert.equal(usesLegacyRuntime("5.9.0-alpha.4"), false);
  assert.equal(usesLegacyRuntime("5.9.0"), false);
  assert.equal(usesLegacyRuntime("5.9.2-beta.1"), false);
});

test("treats aliases and invalid versions as modern", () => {
  assert.equal(usesLegacyRuntime("latest"), false);
  assert.equal(usesLegacyRuntime("5.9"), false);
  assert.equal(usesLegacyRuntime(), false);
});
