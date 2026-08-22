#!/usr/bin/env node
// Fail-closed preflight: refuse to run gates against a stale
// @al-ft/midgard-core dist. Consumers resolve the package from its
// gitignored dist/, so a fresh checkout — or a checkout that pulled a src
// change without rebuilding — silently serves retired constants (the
// motivating incident: the 8,273 -> 12,810 maxReliableDirectCompleteItemBytes
// rebind, #597 ruling b / b4b6c488).
//
// Two independent checks, both fail-closed:
//   1. The build-time source-digest stamp (written by
//      demo/midgard-core/scripts/write-dist-source-digest.mjs as part of
//      `pnpm --filter @al-ft/midgard-core build`) must match a fresh digest
//      of src/ + the build script. A dist built before the stamp existed has
//      no stamp and fails.
//   2. The named tripwire: maxReliableDirectCompleteItemBytes parsed from
//      src/consensus-profile-v1.ts must equal the compiled dist export.
//
// Remedy for every failure mode:  pnpm --filter @al-ft/midgard-core build
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const repoRoot = resolve(fileURLToPath(new URL("../..", import.meta.url)));
const coreRoot = resolve(repoRoot, "demo/midgard-core");
const REBUILD = "pnpm --filter @al-ft/midgard-core build";

const fail = (message) => {
  process.stderr.write(
    `midgard-core dist preflight FAILED: ${message}\n  fix: ${REBUILD}\n`,
  );
  process.exit(1);
};

// Check 1: source digest stamp.
let stamp;
try {
  stamp = JSON.parse(
    await readFile(resolve(coreRoot, "dist/.source-digest-v1.json"), "utf8"),
  );
} catch {
  fail(
    "dist/.source-digest-v1.json is missing — the dist is absent or was built before the stamp existed",
  );
}
const { computeMidgardCoreSourceDigest } = await import(
  pathToFileURL(resolve(coreRoot, "scripts/write-dist-source-digest.mjs")).href
);
const expected = await computeMidgardCoreSourceDigest();
if (stamp.sha256 !== expected) {
  fail(
    `dist was built from different sources (stamped ${stamp.sha256}, src is ${expected})`,
  );
}

// Check 2: the named constant tripwire.
const profileSource = await readFile(
  resolve(coreRoot, "src/consensus-profile-v1.ts"),
  "utf8",
);
const match = profileSource.match(
  /maxReliableDirectCompleteItemBytes:\s*([0-9_]+)/,
);
if (!match) {
  fail(
    "maxReliableDirectCompleteItemBytes not found in src/consensus-profile-v1.ts — update this preflight alongside the rename",
  );
}
const sourceValue = Number(match[1].replaceAll("_", ""));
const dist = await import(
  pathToFileURL(resolve(coreRoot, "dist/consensus-profile-v1.js")).href
);
const distValue =
  dist.MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
if (distValue !== sourceValue) {
  fail(
    `maxReliableDirectCompleteItemBytes: dist exports ${distValue}, src declares ${sourceValue}`,
  );
}

process.stdout.write(
  `midgard-core dist preflight PASS (source digest ${expected.slice(0, 12)}…, maxReliableDirectCompleteItemBytes ${distValue})\n`,
);
