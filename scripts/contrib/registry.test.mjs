import assert from "node:assert/strict";
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

import {
  artifactChannels,
  channelIdentity,
  dependencyPins,
} from "./artifacts.mjs";
import { compact, parse } from "../contrib.mjs";
import { classifyProgress, redact } from "./diagnostics.mjs";
import { enrollBuilds } from "./enroll-builds.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { measureFiles } from "./measure.mjs";
import { devnetPlan, withPorts } from "./operations.mjs";

const root = fileURLToPath(new URL("../..", import.meta.url));
// Whether every gate names real files is scripts/ci/check-registry-paths.mjs's
// job, with the other registries.
test("every package build is guarded", () => {
  assert.deepEqual(enrollBuilds(root), []);
});

test("argument parsing rejects ambiguous shell-like options and keeps file selectors structured", () => {
  assert.deepEqual(
    parse([
      "test",
      "--package",
      "example",
      "--file",
      "tests/a.test.ts",
      "--file",
      "tests/b.test.ts",
    ]).files,
    ["tests/a.test.ts", "tests/b.test.ts"],
  );
  assert.throws(() => parse(["--plan", "--execute"]), /exclusive/u);
  assert.throws(() => parse(["--shell", "rm -rf"]), /unknown/u);
  assert.deepEqual(
    compact({ inputs: { sha256: "abc", files: { "a.ts": "xyz" } } }),
    { inputs: { sha256: "abc", fileCount: 1 } },
  );
});

test("quiet queues stay idle while eligible work can stall or hold", () => {
  const observation = {
    observedAtMs: 10000,
    eligibleWork: 0,
    lastSuccessfulTransitionMs: 0,
    expectedProgressWithinMs: 1000,
  };
  assert.equal(classifyProgress(observation).state, "idle");
  assert.equal(
    classifyProgress({ ...observation, eligibleWork: 3 }).state,
    "stalled",
  );
  assert.equal(
    classifyProgress({
      ...observation,
      eligibleWork: 3,
      safetyHold: "contradictory inclusion",
    }).state,
    "held",
  );
  assert.equal(
    classifyProgress({
      ...observation,
      eligibleWork: 3,
      lastSuccessfulTransitionMs: 9900,
    }).state,
    "working",
  );
  assert.equal(classifyProgress({ ready: true }).state, "unknown");
  assert.equal(
    classifyProgress({ ...observation, dependencyFailure: "provider outage" })
      .state,
    "dependency-outage",
  );
});

test("diagnostic redaction removes secret fields, transaction bytes and URL keys", () => {
  assert.deepEqual(
    redact({
      provider: "https://user:password@example.com/api?key=secret",
      env: { seedPhrase: "words", POSTGRES_PASSWORD: "secret" },
      signedCbor: "abc",
    }),
    {
      provider: "https://example.com/api",
      env: { seedPhrase: "[redacted]", POSTGRES_PASSWORD: "[redacted]" },
      signedCbor: "[redacted]",
    },
  );
});

test("artifact identities follow the existing registry and Git dependencies have public immutable pins", (t) => {
  const channels = artifactChannels(root).channels;
  assert.ok(channelIdentity(root, channels[0]).sha256);
  assert.ok(dependencyPins(root).length > 0);
  const scratch = fixture(t);
  const path = resolve(scratch, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.dependencies = { unavailable: "file:/tmp/private.tgz" };
  writeFileSync(path, JSON.stringify(pkg));
  assert.throws(() => dependencyPins(scratch), /nonportable/u);
  pkg.dependencies = { unavailable: "github:owner/project#main" };
  writeFileSync(path, JSON.stringify(pkg));
  assert.throws(() => dependencyPins(scratch), /immutable/u);
});

test("byte measurements deduplicate exact serialized content and refuse over-bound inputs", async (t) => {
  const scratch = fixture(t);
  const paths = [resolve(scratch, "a.bin"), resolve(scratch, "b.bin")];
  for (const path of paths) writeFileSync(path, Buffer.alloc(8192, 1));
  const measured = await measureFiles(paths);
  assert.equal(measured.totalBytes, 16384);
  assert.equal(measured.uniqueBytes, 8192);
  await assert.rejects(
    measureFiles(paths, { maximumBytes: 8191 }),
    /envelope/u,
  );
});

test("two same-checkout devnet runs receive deterministic separate identities and port bounds", async (t) => {
  const scratch = fixture(t);
  const first = devnetPlan(scratch, "first");
  assert.deepEqual(first, devnetPlan(scratch, "first"));
  assert.notDeepEqual(first.env, devnetPlan(scratch, "second").env);
  await assert.rejects(
    withPorts([5433], () => assert.fail("shared test port acquired")),
    /excluding/u,
  );
  await assert.rejects(
    withPorts([21000, 21000], () => assert.fail("duplicate port acquired")),
    /distinct/u,
  );
});
