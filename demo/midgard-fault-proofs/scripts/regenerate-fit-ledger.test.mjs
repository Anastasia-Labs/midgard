import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join, relative } from "node:path";
import { test } from "node:test";

import {
  discoverFitLedgerOwners,
  LEDGERS_WITHOUT_SUITE_WRITER,
  packageDirectory,
  trackedFitLedgers,
} from "./fit-ledger-owners.mjs";
import {
  regenerateFitLedgers,
  vitestArguments,
} from "./regenerate-fit-ledger.mjs";

const { ledgers: owners, unattributedWriters } = discoverFitLedgerOwners();
const tracked = trackedFitLedgers();

test("every checked-in fit ledger has an owning test file or a recorded other writer", () => {
  assert.ok(tracked.length > 0);
  const withoutOwner = tracked.filter((ledger) => !owners.get(ledger)?.length);
  // Both ways: a ledger that gains a suite writer leaves the list, and one
  // that loses it (or is written some new way) must be recorded there.
  assert.deepEqual(
    withoutOwner,
    [...LEDGERS_WITHOUT_SUITE_WRITER.keys()].sort(),
  );
  for (const ledger of LEDGERS_WITHOUT_SUITE_WRITER.keys())
    assert.ok(tracked.includes(ledger), `${ledger} is not checked in`);
});

test("every owner is a test file Vitest runs", () => {
  for (const [ledger, files] of owners)
    for (const file of files) {
      assert.match(file, /^tests\/.+\.test\.tsx?$/u, ledger);
      assert.ok(existsSync(join(packageDirectory, file)), file);
    }
});

test("discovery finds the single writer of a ledger named through a helper module", () => {
  // The literal lives in an authentication-seams module that several tests
  // import; only the lifecycle test that writes the ledger owns it.
  assert.deepEqual(owners.get("witness-script-decoding-v1-fit-ledger.json"), [
    "tests/witness-script-decoding-lifecycle.test.ts",
  ]);
});

test("discovery finds every part of a split ledger", () => {
  assert.deepEqual(owners.get("value-not-preserved-fit-ledger.json"), [
    "tests/value-conservation-lifecycle-forced-assets.test.ts",
    "tests/value-conservation-lifecycle-non-forced-assets.test.ts",
    "tests/value-conservation-lifecycle.test.ts",
  ]);
  const mint = owners.get("mint-authorization-workflow-fit-ledger.json");
  assert.ok(
    mint.includes("tests/mint-authorization-installed-lifecycle.test.ts"),
  );
  assert.ok(mint.length > 1);
});

test("every module that writes a fit ledger is attributed to the ledger it names", () => {
  // The rest take the path from their caller or from an environment
  // variable, or are unit tests writing under a temporary directory.
  assert.deepEqual(unattributedWriters, [
    "tests/min-ada-wrongful-rejection-lifecycle.test.ts",
    "tests/pinned-fit-ledger.test.ts",
    "tests/support/measured-fit-ledger.ts",
    "tests/support/pinned-fit-ledger.ts",
    "tests/support/split-fit-ledger.ts",
    "tests/support/transition-trace-final-cases.register-transition-trace-final-cases.ts",
    "tests/wave0-shared-substrate.test.ts",
  ]);
});

test("the Vitest run names exactly the owning files and no filter", () => {
  const args = vitestArguments(["/p/tests/a.test.ts"], "/r.json");
  assert.equal(args[0], "run");
  assert.equal(args.at(-1), "/p/tests/a.test.ts");
  for (const flag of args)
    assert.doesNotMatch(flag, /^(-t|--testNamePattern|--shard|--project)/u);
});

/** A size-plans directory and package root of its own, with two ledgers. */
const fixture = (t) => {
  const directory = mkdtempSync(join(tmpdir(), "fit-regenerate-test-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  const sizePlans = join(directory, "size-plans");
  const root = join(directory, "package");
  mkdirSync(sizePlans);
  mkdirSync(join(root, "tests"), { recursive: true });
  for (const name of ["a.test.ts", "b.test.ts", "c.test.ts"])
    writeFileSync(join(root, "tests", name), "");
  writeFileSync(join(sizePlans, "split-fit-ledger.json"), "old split\n");
  writeFileSync(join(sizePlans, "other-fit-ledger.json"), "old other\n");
  const owners = new Map([
    ["split-fit-ledger.json", ["tests/a.test.ts", "tests/b.test.ts"]],
    ["other-fit-ledger.json", ["tests/c.test.ts"]],
  ]);
  const read = (name) => readFileSync(join(sizePlans, name), "utf8");
  return { sizePlans, root, owners, read };
};

/** A Vitest JSON report: each file passed unless named in `failed`. */
const report = (root, files, { failed = [], skipped = [] } = {}) => ({
  testResults: files.map((file) => ({
    name: join(root, file),
    status: failed.includes(file) ? "failed" : "passed",
    message: failed.includes(file) ? "afterAll threw" : "",
    assertionResults: [
      { status: failed.includes(file) ? "failed" : "passed" },
      ...(skipped.includes(file) ? [{ status: "skipped" }] : []),
    ],
  })),
});

/**
 * A stand-in for the Vitest child, which the real end-to-end run exercises:
 * it writes what a run would, reports per file, then exits.
 */
const fakeRun =
  ({ root, write = {}, failed = [], skipped = [], status, noReport = false }) =>
  async ({ files, reportPath, env }) => {
    for (const [path, text] of Object.entries(write)) writeFileSync(path, text);
    const relativeFiles = files.map((file) => relative(root, file));
    if (!noReport)
      writeFileSync(
        reportPath,
        JSON.stringify(report(root, relativeFiles, { failed, skipped })),
      );
    fakeRun.calls.push({ files: relativeFiles, env });
    return status ?? (failed.length > 0 ? 1 : 0);
  };
fakeRun.calls = [];

test("a failing part leaves the ledger untouched and fails the command", async (t) => {
  const { sizePlans, root, owners, read } = fixture(t);
  const { status, lines } = await regenerateFitLedgers({
    ledgers: ["split-fit-ledger.json"],
    owners,
    sizePlans,
    root,
    // The passing part's file wrote the ledger before the other part failed.
    run: fakeRun({
      root,
      write: { [join(sizePlans, "split-fit-ledger.json")]: "partial\n" },
      failed: ["tests/b.test.ts"],
    }),
  });
  assert.equal(status, 1);
  assert.equal(read("split-fit-ledger.json"), "old split\n");
  assert.match(
    lines[0],
    /^not rewritten: split-fit-ledger\.json .*b\.test\.ts failed/u,
  );
});

test("a skipped case, a missing report or an unwritten ledger keeps nothing", async (t) => {
  const { sizePlans, root, owners, read } = fixture(t);
  const ledger = join(sizePlans, "other-fit-ledger.json");
  for (const [run, why] of [
    [
      fakeRun({
        root,
        write: { [ledger]: "new\n" },
        skipped: ["tests/c.test.ts"],
      }),
      /did not run to a pass/u,
    ],
    [
      fakeRun({
        root,
        write: { [ledger]: "new\n" },
        status: 1,
        noReport: true,
      }),
      /c\.test\.ts did not run/u,
    ],
    [
      fakeRun({ root, write: { [ledger]: "new\n" }, status: 1 }),
      /outside any test file/u,
    ],
    [fakeRun({ root }), /passed but did not write it/u],
  ]) {
    const { status, lines } = await regenerateFitLedgers({
      ledgers: ["other-fit-ledger.json"],
      owners,
      sizePlans,
      root,
      run,
    });
    assert.equal(status, 1);
    assert.match(lines[0], why);
    assert.equal(read("other-fit-ledger.json"), "old other\n");
  }
});

test("a passing run keeps only the requested ledger and undoes every other change", async (t) => {
  const { sizePlans, root, owners, read } = fixture(t);
  fakeRun.calls = [];
  const { status, lines } = await regenerateFitLedgers({
    ledgers: ["split-fit-ledger.json"],
    owners,
    sizePlans,
    root,
    run: fakeRun({
      root,
      write: {
        [join(sizePlans, "split-fit-ledger.json")]: "new split\n",
        [join(sizePlans, "other-fit-ledger.json")]: "stray\n",
        [join(sizePlans, "untracked-fit-ledger.json")]: "stray\n",
      },
    }),
  });
  assert.equal(status, 0);
  assert.deepEqual(lines, ["rewritten: split-fit-ledger.json"]);
  assert.equal(read("split-fit-ledger.json"), "new split\n");
  assert.equal(read("other-fit-ledger.json"), "old other\n");
  assert.deepEqual(readdirSync(sizePlans).sort(), [
    "other-fit-ledger.json",
    "split-fit-ledger.json",
  ]);
  const [{ files, env }] = fakeRun.calls;
  assert.deepEqual(files, ["tests/a.test.ts", "tests/b.test.ts"]);
  assert.equal(env.MIDGARD_WRITE_FIT_LEDGER, "1");
  assert.match(env.MIDGARD_FIT_MEASUREMENT_RUN, /^[a-zA-Z0-9_-]+$/u);
  // The fragment directory was fresh and is gone.
  assert.ok(!existsSync(env.MIDGARD_FIT_FRAGMENT_DIR));
});

test("each run gets its own fragment directory and token", async (t) => {
  const { sizePlans, root, owners } = fixture(t);
  fakeRun.calls = [];
  for (let index = 0; index < 2; index++)
    await regenerateFitLedgers({
      ledgers: ["other-fit-ledger.json"],
      owners,
      sizePlans,
      root,
      run: fakeRun({
        root,
        write: { [join(sizePlans, "other-fit-ledger.json")]: `run ${index}\n` },
      }),
    });
  const [first, second] = fakeRun.calls.map(({ env }) => env);
  assert.notEqual(
    first.MIDGARD_FIT_FRAGMENT_DIR,
    second.MIDGARD_FIT_FRAGMENT_DIR,
  );
  assert.notEqual(
    first.MIDGARD_FIT_MEASUREMENT_RUN,
    second.MIDGARD_FIT_MEASUREMENT_RUN,
  );
});

test("--all keeps the ledgers whose owners passed and restores the rest", async (t) => {
  const { sizePlans, root, owners, read } = fixture(t);
  const { status, lines } = await regenerateFitLedgers({
    ledgers: ["split-fit-ledger.json", "other-fit-ledger.json"],
    owners,
    sizePlans,
    root,
    run: fakeRun({
      root,
      write: {
        [join(sizePlans, "split-fit-ledger.json")]: "new split\n",
        [join(sizePlans, "other-fit-ledger.json")]: "partial\n",
      },
      failed: ["tests/c.test.ts"],
    }),
  });
  assert.equal(status, 1);
  assert.equal(lines[0], "rewritten: split-fit-ledger.json");
  assert.match(lines[1], /^not rewritten: other-fit-ledger\.json/u);
  assert.equal(read("split-fit-ledger.json"), "new split\n");
  assert.equal(read("other-fit-ledger.json"), "old other\n");
});

const cli = (...args) =>
  spawnSync(
    process.execPath,
    [join(packageDirectory, "scripts/regenerate-fit-ledger.mjs"), ...args],
    {
      cwd: packageDirectory,
      encoding: "utf8",
      env: { ...process.env, INIT_CWD: packageDirectory },
    },
  );

test("the command refuses a ledger no test writes, naming how it is written", () => {
  const result = cli("zero-input-wrongful-rejection-v1-fit-ledger.json");
  assert.equal(result.status, 2);
  assert.match(
    result.stderr,
    /No test writes zero-input.*verifyMeasuredFitLedger/u,
  );
});

test("the command refuses an unknown ledger, a path outside size-plans and bad usage", () => {
  assert.equal(cli("no-such-fit-ledger.json").status, 2);
  assert.equal(cli("../package.json").status, 2);
  assert.equal(cli().status, 2);
  assert.equal(cli("--all", "--list").status, 2);
  assert.equal(cli("--bogus").status, 2);
});

test("--list names every checked-in ledger and runs nothing", () => {
  const result = cli("--list");
  assert.equal(result.status, 0, result.stderr);
  for (const ledger of tracked)
    assert.match(result.stdout, new RegExp(`^${ledger}: `, "mu"));
  assert.match(
    result.stdout,
    /^value-not-preserved-fit-ledger\.json: tests\/value-conservation-lifecycle-forced-assets\.test\.ts /mu,
  );
});
