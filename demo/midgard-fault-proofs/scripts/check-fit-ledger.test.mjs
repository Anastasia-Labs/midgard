import assert from "node:assert/strict";
import {
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

import { fitLedgerDifferences } from "./fit-ledger-differences.mjs";
import { repositoryRoot, SIZE_PLANS } from "./fit-ledger-owners.mjs";
import {
  checkFitLedgers,
  main,
  regenerateFitLedgers,
} from "./regenerate-fit-ledger.mjs";

const row = (name, fields = {}) => ({
  name,
  kind: "lifecycle",
  maximumShape: "maximum",
  signedBytes: 9_000,
  memoryUnits: "9000000",
  cpuUnits: "3000000000",
  signedByteMargin: 7_384,
  memoryUnitMargin: "7500000",
  cpuUnitMargin: "7000000000",
  publicationReserveMargin: null,
  ...fields,
});

const ledgerOf = (entries, fields = {}) => ({
  schemaVersion: "midgard-van-rossem-fit-ledger-v1",
  category: "transitionTrace",
  blueprintSha256: "ab".repeat(32),
  entries,
  ledgerSha256: "01".repeat(32),
  ...fields,
});

const ROWS = [row("a/0"), row("b/1", { kind: "publication" })];
const RECORDED = ledgerOf(ROWS);
/** A fresh run that measured other budgets against another build. */
const RETAKEN = ledgerOf(
  ROWS.map((entry) => ({
    ...entry,
    memoryUnits: "9000017",
    cpuUnits: "3000000017",
    memoryUnitMargin: "7499983",
    cpuUnitMargin: "6999999983",
  })),
  { blueprintSha256: "cd".repeat(32), ledgerSha256: "02".repeat(32) },
);

const text = (ledger) => `${JSON.stringify(ledger, null, 2)}\n`;

/** A size-plans directory holding one recorded ledger, and its owner. */
const fixture = (t, recorded = RECORDED) => {
  const directory = mkdtempSync(join(tmpdir(), "fit-check-test-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  const sizePlans = join(directory, "size-plans");
  const root = join(directory, "package");
  mkdirSync(sizePlans);
  mkdirSync(join(root, "tests"), { recursive: true });
  writeFileSync(join(root, "tests", "owner.test.ts"), "");
  writeFileSync(join(sizePlans, "x-fit-ledger.json"), text(recorded));
  writeFileSync(join(sizePlans, "y-fit-ledger.json"), "untouched\n");
  const owners = new Map([["x-fit-ledger.json", ["tests/owner.test.ts"]]]);
  const bytes = () =>
    readdirSync(sizePlans)
      .sort()
      .map((name) => [name, readFileSync(join(sizePlans, name), "utf8")]);
  return { sizePlans, root, owners, bytes };
};

/**
 * A stand-in for the owner's Vitest run that writes its ledger the way
 * `writeVanRossemFitLedger` does: into `MIDGARD_FIT_LEDGER_CHECK_DIR` when
 * that is set, otherwise over the checked-in copy.
 */
const owner =
  ({ root, sizePlans, fresh, failed = false, calls = [] }) =>
  async ({ files, reportPath, env }) => {
    calls.push(env);
    if (fresh !== undefined) {
      const directory = env.MIDGARD_FIT_LEDGER_CHECK_DIR ?? sizePlans;
      mkdirSync(directory, { recursive: true });
      writeFileSync(join(directory, "x-fit-ledger.json"), text(fresh));
    }
    writeFileSync(
      reportPath,
      JSON.stringify({
        testResults: files.map((file) => ({
          name: join(root, relative(root, file)),
          status: failed ? "failed" : "passed",
          message: "",
          assertionResults: [{ status: failed ? "failed" : "passed" }],
        })),
      }),
    );
    return failed ? 1 : 0;
  };

const check = (setup, run) =>
  checkFitLedgers({
    ledgers: ["x-fit-ledger.json"],
    owners: setup.owners,
    sizePlans: setup.sizePlans,
    root: setup.root,
    run,
  });

test("a fresh run equal to the checked-in copy is current, and size-plans is untouched", async (t) => {
  const setup = fixture(t);
  const before = setup.bytes();
  const calls = [];
  const { status, lines } = await check(
    setup,
    owner({ ...setup, fresh: RECORDED, calls }),
  );
  assert.deepEqual(lines, ["current: x-fit-ledger.json"]);
  assert.equal(status, 0);
  assert.deepEqual(setup.bytes(), before);
  assert.equal(calls[0].MIDGARD_WRITE_FIT_LEDGER, "1");
  assert.ok(calls[0].MIDGARD_FIT_LEDGER_CHECK_DIR);
});

const [first, second] = ROWS;
for (const [field, recorded, expected] of [
  [
    "memoryUnits",
    ledgerOf([{ ...first, memoryUnits: "9000001" }, second]),
    'row a/0 memoryUnits: recorded "9000001", fresh "9000000"',
  ],
  [
    "cpuUnits",
    ledgerOf([first, { ...second, cpuUnits: "2999999999" }]),
    'row b/1 cpuUnits: recorded "2999999999", fresh "3000000000"',
  ],
  [
    "memoryUnitMargin",
    ledgerOf([{ ...first, memoryUnitMargin: "7499999" }, second]),
    'row a/0 memoryUnitMargin: recorded "7499999", fresh "7500000"',
  ],
  [
    "cpuUnitMargin",
    ledgerOf([first, { ...second, cpuUnitMargin: "7000000001" }]),
    'row b/1 cpuUnitMargin: recorded "7000000001", fresh "7000000000"',
  ],
  [
    "a budget measured against another blueprint",
    ledgerOf([{ ...first, cpuUnits: "3000000001" }, second], {
      blueprintSha256: "cd".repeat(32),
      ledgerSha256: "02".repeat(32),
    }),
    'row a/0 cpuUnits: recorded "3000000001", fresh "3000000000"',
  ],
  [
    "signedBytes",
    ledgerOf([{ ...first, signedBytes: 9_001 }, second]),
    "row a/0 signedBytes: recorded 9001, fresh 9000",
  ],
  [
    "signedByteMargin",
    ledgerOf([{ ...first, signedByteMargin: 7_383 }, second]),
    "row a/0 signedByteMargin: recorded 7383, fresh 7384",
  ],
  [
    "publicationReserveMargin",
    ledgerOf([first, { ...second, publicationReserveMargin: 6_872 }]),
    "row b/1 publicationReserveMargin: recorded 6872, fresh null",
  ],
  [
    "maximumShape",
    ledgerOf([{ ...first, maximumShape: "accepted" }, second]),
    'row a/0 maximumShape: recorded "accepted", fresh "maximum"',
  ],
  [
    "kind",
    ledgerOf([first, { ...second, kind: "lifecycle" }]),
    'row b/1 kind: recorded "lifecycle", fresh "publication"',
  ],
  [
    "name",
    ledgerOf([{ ...first, name: "a/9" }, second]),
    [
      "row a/9: recorded, not in the fresh run",
      "row a/0: in the fresh run, not recorded",
    ],
  ],
  [
    "a missing row",
    ledgerOf([first]),
    "row b/1: in the fresh run, not recorded",
  ],
  [
    "an extra row",
    ledgerOf([...ROWS, row("c/2")]),
    "row c/2: recorded, not in the fresh run",
  ],
  [
    "the row order",
    ledgerOf([second, first]),
    "rows: recorded in another order or more than once",
  ],
  [
    "category",
    ledgerOf(ROWS, { category: "withdrawalMistag" }),
    'category: recorded "withdrawalMistag", fresh "transitionTrace"',
  ],
  [
    "schemaVersion",
    ledgerOf(ROWS, { schemaVersion: "midgard-van-rossem-fit-ledger-v0" }),
    'schemaVersion: recorded "midgard-van-rossem-fit-ledger-v0", fresh "midgard-van-rossem-fit-ledger-v1"',
  ],
  [
    "a dropped field",
    ledgerOf(ROWS, { compilerVersion: "v1.1.24+28e4288" }),
    'compilerVersion: recorded "v1.1.24+28e4288", fresh absent',
  ],
])
  test(`a checked-in ledger stale in ${field} fails the check and stays as it was`, async (t) => {
    const setup = fixture(t, recorded);
    const before = setup.bytes();
    const { status, lines } = await check(
      setup,
      owner({ ...setup, fresh: RECORDED }),
    );
    assert.equal(status, 1);
    assert.equal(lines.length, 1);
    assert.match(
      lines[0],
      /^stale: x-fit-ledger\.json; regenerate it with `pnpm --dir demo\/midgard-fault-proofs fit:regenerate x-fit-ledger\.json`\n/u,
    );
    assert.deepEqual(
      lines[0].split("\n").slice(1),
      [expected].flat().map((line) => `  ${line}`),
    );
    assert.deepEqual(setup.bytes(), before);
  });

for (const [digests, recorded] of [
  ["blueprintSha256", ledgerOf(ROWS, { blueprintSha256: "cd".repeat(32) })],
  ["ledgerSha256", ledgerOf(ROWS, { ledgerSha256: "02".repeat(32) })],
  [
    "both digests",
    ledgerOf(ROWS, {
      blueprintSha256: "cd".repeat(32),
      ledgerSha256: "02".repeat(32),
    }),
  ],
])
  test(`a checked-in ledger that differs only in ${digests} is current`, async (t) => {
    // A blueprint change makes a ledger stale only when its numbers move.
    const setup = fixture(t, recorded);
    const before = setup.bytes();
    const { status, lines } = await check(
      setup,
      owner({ ...setup, fresh: RECORDED }),
    );
    assert.deepEqual(lines, ["current: x-fit-ledger.json"]);
    assert.equal(status, 0);
    assert.deepEqual(setup.bytes(), before);
  });

test("a run that writes over the checked-in copy fails the check and is put back", async (t) => {
  const setup = fixture(t);
  const before = setup.bytes();
  const overwriting = owner({ ...setup, fresh: RETAKEN });
  const { status, lines } = await check(setup, ({ env, ...rest }) => {
    const { MIDGARD_FIT_LEDGER_CHECK_DIR: _ignored, ...writer } = env;
    return overwriting({ ...rest, env: writer });
  });
  assert.equal(status, 1);
  assert.match(
    lines[0],
    /^not checked: x-fit-ledger\.json .*did not write it/u,
  );
  assert.match(lines[1], /^the run changed x-fit-ledger\.json under /u);
  assert.deepEqual(setup.bytes(), before);
});

test("an owner that failed, or passed without writing, leaves the ledger unchecked", async (t) => {
  const setup = fixture(t);
  for (const [run, why] of [
    [
      owner({ ...setup, fresh: RETAKEN, failed: true }),
      /owner\.test\.ts failed/u,
    ],
    [owner(setup), /its owners passed but did not write it/u],
  ]) {
    const { status, lines } = await check(setup, run);
    assert.equal(status, 1);
    assert.match(lines[0], /^not checked: x-fit-ledger\.json \(/u);
    assert.match(lines[0], why);
  }
});

test("a regeneration writes over the checked-in copy, never into an inherited check directory", async (t) => {
  const setup = fixture(t);
  const calls = [];
  process.env.MIDGARD_FIT_LEDGER_CHECK_DIR = join(setup.root, "stray");
  t.after(() => delete process.env.MIDGARD_FIT_LEDGER_CHECK_DIR);
  const { status } = await regenerateFitLedgers({
    ledgers: ["x-fit-ledger.json"],
    owners: setup.owners,
    sizePlans: setup.sizePlans,
    root: setup.root,
    run: owner({ ...setup, fresh: RETAKEN, calls }),
  });
  assert.equal(status, 0);
  assert.equal(calls[0].MIDGARD_FIT_LEDGER_CHECK_DIR, undefined);
  assert.equal(
    readFileSync(join(setup.sizePlans, "x-fit-ledger.json"), "utf8"),
    text(RETAKEN),
  );
});

test("the command's --check runs the owners with a check directory and keeps size-plans", async () => {
  const sizePlans = join(repositoryRoot, SIZE_PLANS);
  const snapshot = () =>
    readdirSync(sizePlans).map((name) => [
      name,
      readFileSync(join(sizePlans, name), "utf8"),
    ]);
  const before = snapshot();
  const calls = [];
  const run = async ({ env }) => {
    calls.push(env);
    return 0;
  };
  const ledger = "transition-trace-forced-window-fit-ledger.json";
  assert.equal(await main(["--check", ledger], { run }), 1);
  assert.equal(await main([ledger], { run }), 1);
  assert.ok(calls[0].MIDGARD_FIT_LEDGER_CHECK_DIR);
  assert.equal(calls[1].MIDGARD_FIT_LEDGER_CHECK_DIR, undefined);
  assert.deepEqual(snapshot(), before);
  await assert.rejects(main(["--check"], { run }), /Usage/u);
  await assert.rejects(main(["--check", "--list"], { run }), /Usage/u);
});

test("the comparison names a row list that is missing on either side", () => {
  assert.deepEqual(
    fitLedgerDifferences({ ...RECORDED, entries: undefined }, RECORDED),
    [
      "entries: the recorded ledger has no row list",
      "row a/0: in the fresh run, not recorded",
      "row b/1: in the fresh run, not recorded",
    ],
  );
  assert.deepEqual(fitLedgerDifferences(RECORDED, RECORDED), []);
});
