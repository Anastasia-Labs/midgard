// Every node-tools watcher-journey test file either runs in node CI or needs a
// devnet. The `midgard-node-tools` job of `.github/workflows/midgard-node-ci.yml`
// lists the journey files that need no devnet by hand; a new file left off that
// list would never run unless all of its tests skip without a journey run
// directory. So each `devnet/watcher-journeys/**/*.test.{ts,mjs}` is either
// matched by a path the step names (vitest takes each as a substring of the
// file path), or devnet-gated: it reads `MIDGARD_WATCHER_JOURNEY_RUN_DIR` and
// every test it declares sits behind `skipIf`/`runIf`, on the test itself or
// on a top-level `describe`. The check reads the files; it runs none of them.

import assert from "node:assert/strict";
import { readdirSync, readFileSync, statSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join, relative, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const PACKAGE = "demo/midgard-node-tools";
const JOURNEYS = "devnet/watcher-journeys";
const WORKFLOW = ".github/workflows/midgard-node-ci.yml";
const JOB = "midgard-node-tools";
const STEP = "Test the watcher journeys that need no devnet";
const RUN_DIR = "MIDGARD_WATCHER_JOURNEY_RUN_DIR";

const loadYaml = () => {
  try {
    return createRequire(join(ROOT, "demo/package.json"))("yaml");
  } catch {
    return undefined;
  }
};
const yaml = loadYaml();
const skip =
  yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
    ? "could not check: yaml absent (run `pnpm --dir demo install`)"
    : false;

/** The journey test files, relative to the package. */
const journeyTests = (root) => {
  const walk = (dir) =>
    readdirSync(dir).flatMap((name) => {
      const path = join(dir, name);
      if (statSync(path).isDirectory()) return walk(path);
      return /\.test\.(?:ts|mjs)$/u.test(name) ? [path] : [];
    });
  const base = join(root, PACKAGE);
  return walk(join(base, JOURNEYS)).map((path) => relative(base, path));
};

/** The paths the step names, as the package-relative substrings they match. */
export const listedPaths = (run) =>
  run
    .split(/\s+/u)
    .map((token) => token.replace(/^demo\/midgard-node-tools\//u, ""))
    .filter((token) => /^(?:devnet\/)?watcher-journeys\/\S+$/u.test(token));

const TEST_CALL = /^\s*(it|test|describe)\b\s*(.*)$/u;
const GATE = /^\.\s*(?:skipIf|runIf)\s*\(/u;

/**
 * Whether every test `source` declares is gated. A call is gated when its
 * chain names `skipIf` or `runIf` (`it.skipIf(...)(`, or `it` with
 * `.skipIf(...)` on the next line); an ungated call is allowed only inside a
 * gated top-level `describe`, which ends at the next top-level statement.
 */
export const allTestsGated = (source) => {
  const lines = source.split(/\r?\n/u);
  let inGatedDescribe = false;
  let tests = 0;
  for (let index = 0; index < lines.length; index += 1) {
    const line = lines[index];
    const call = TEST_CALL.exec(line);
    const topLevel = /^[^\s)}\]/*]/u.test(line);
    if (call === null) {
      if (topLevel) inGatedDescribe = false;
      continue;
    }
    const [, kind, rest] = call;
    const chain = [rest.trim()];
    if (chain[0] === "")
      for (let next = index + 1; next < lines.length; next += 1) {
        const link = lines[next].trim();
        if (!link.startsWith(".")) break;
        chain.push(link);
      }
    const gated = chain.some((link) => GATE.test(link));
    const isCall = chain.some((link) => /^(?:\.\w+)*\s*\(/u.test(link));
    if (!isCall) {
      if (topLevel) inGatedDescribe = false;
      continue;
    }
    if (topLevel) inGatedDescribe = kind === "describe" && gated;
    if (kind !== "describe") tests += 1;
    if (!gated && !inGatedDescribe) return false;
  }
  return tests > 0;
};

/** Whether the file needs a devnet: it reads the run directory and gates every test. */
export const devnetGated = (source) =>
  source.includes(RUN_DIR) && allTestsGated(source);

/** Each journey test file neither listed in the step nor devnet-gated. */
export const unwiredJourneyTests = (root, run) => {
  const listed = listedPaths(run);
  return journeyTests(root).filter(
    (path) =>
      !listed.some((entry) => path.includes(entry)) &&
      !devnetGated(readFileSync(join(root, PACKAGE, path), "utf8")),
  );
};

const stepRun = () => {
  const workflow = yaml.parse(readFileSync(join(ROOT, WORKFLOW), "utf8"));
  const step = workflow.jobs[JOB].steps.find(({ name }) => name === STEP);
  assert.ok(step, `${WORKFLOW}: job ${JOB} has no step "${STEP}"`);
  return step.run;
};

test(
  "every journey test file runs in node CI or needs a devnet",
  { skip },
  () => {
    assert.deepEqual(unwiredJourneyTests(ROOT, stepRun()), []);
  },
);

test("every path the step names matches a journey test file", { skip }, () => {
  const files = journeyTests(ROOT);
  const listed = listedPaths(stepRun());
  assert.ok(listed.length > 10, `only ${listed.length} paths listed`);
  for (const entry of listed)
    assert.ok(
      files.some((path) => path.includes(entry)),
      `${entry} matches no journey test file`,
    );
});

test(
  "an unlisted file whose tests run without a devnet fails",
  { skip },
  () => {
    const run = stepRun();
    const [first] = listedPaths(run).filter((entry) => entry.endsWith(".ts"));
    assert.deepEqual(
      unwiredJourneyTests(ROOT, run.replace(first, "")).map((path) =>
        path.includes(first),
      ),
      [true],
    );
  },
);

test("gating is read from the test chain and a gated top-level describe", () => {
  const env = `const dir = process.env.${RUN_DIR};\n`;
  const gated = [
    'it.skipIf(dir === undefined)("a", async () => {});',
    'it\n  .skipIf(dir === undefined)\n  .each([1])("b %s", async () => {});',
    'describe.skipIf(dir === undefined)("c", () => {\n  it("d", () => {});\n  it.each([1])("e", () => {});\n});',
    'for (const x of xs) {\n  it.skipIf(dir === undefined)("f", () => {});\n}',
    'it.runIf(dir !== undefined)("g", () => {});',
  ];
  for (const body of gated) assert.equal(devnetGated(env + body), true, body);
  const ungated = [
    'it("a", () => {});',
    'it.each([1])("b", () => {});',
    'describe("c", () => {\n  it.skipIf(dir === undefined)("d", () => {});\n  it("e", () => {});\n});',
    'describe.skipIf(dir === undefined)("c", () => {\n  it("d", () => {});\n});\nit("e", () => {});',
    'test("f", () => {});',
  ];
  for (const body of ungated)
    assert.equal(devnetGated(env + body), false, body);
  // A gated file that never reads the run directory is not devnet-gated.
  assert.equal(devnetGated(gated[0]), false);
  // A file with no test is not gated: it proves nothing either way.
  assert.equal(devnetGated(env), false);
});
