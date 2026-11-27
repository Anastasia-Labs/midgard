// A fault-proof workflow keeps nothing on disk about what it submitted: it
// reads its progress from the chain and holds its in-flight transactions in
// process memory. No source under `demo/*/src` names an entry kind of a stored
// workflow record (an intent, a submission, a confirmation, a retirement or a
// collateral forfeiture written ahead of or after a send). The check reads the
// files; it runs none of them.

import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const DEMO = join(ROOT, "demo");
const SOURCE = /\.(?:ts|mts|cts|js|mjs|cjs)$/u;
const SKIPPED = new Set(["node_modules", "dist", "build", ".turbo"]);

/** The entry kinds of the stored workflow record. */
export const RECORD_ENTRY_KINDS = Object.freeze([
  "submission_intent",
  "submission_ambiguous",
  "rebroadcast_intent",
  "preflight_passed",
  "reobserved",
  "signed_attempt_retired",
  "collateral_forfeited",
  "collateral_forfeiture_reversed",
]);
const NAMED = new RegExp(`\\b(?:${RECORD_ENTRY_KINDS.join("|")})\\b`, "u");

const sourceFiles = function* (directory) {
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    if (SKIPPED.has(entry.name)) continue;
    const path = join(directory, entry.name);
    if (entry.isDirectory()) yield* sourceFiles(path);
    else if (entry.isFile() && SOURCE.test(entry.name)) yield path;
  }
};

const packageSources = () =>
  readdirSync(DEMO, { withFileTypes: true })
    .filter((entry) => entry.isDirectory() && !SKIPPED.has(entry.name))
    .flatMap((entry) => {
      const src = join(DEMO, entry.name, "src");
      try {
        return [...sourceFiles(src)];
      } catch (error) {
        if (error?.code === "ENOENT") return [];
        throw error;
      }
    });

test("no demo package source names a stored workflow record's entry kind", () => {
  const files = packageSources();
  assert.ok(
    files.some((path) => path.includes("midgard-fault-proofs/src/")),
    "the check found no fault-proofs source to read",
  );
  const named = files.flatMap((path) =>
    readFileSync(path, "utf8")
      .split("\n")
      .flatMap((line, index) =>
        NAMED.test(line)
          ? [`${relative(ROOT, path)}:${(index + 1).toString()}: ${line.trim()}`]
          : [],
      ),
  );
  assert.deepEqual(named, []);
});
