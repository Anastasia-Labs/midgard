/**
 * Regenerate or check the checked-in fault-proof fit ledgers in one command.
 *
 *   pnpm --dir demo/midgard-fault-proofs fit:regenerate <ledger>...
 *   pnpm --dir demo/midgard-fault-proofs fit:regenerate --all
 *   pnpm --dir demo/midgard-fault-proofs fit:regenerate --check <ledger>...
 *   pnpm --dir demo/midgard-fault-proofs fit:regenerate --check --all
 *   pnpm --dir demo/midgard-fault-proofs fit:regenerate --list
 *
 * A ledger is a file name in docs/fault-proofs/size-plans or a path to one.
 * The owning test files are found from the test sources
 * (scripts/fit-ledger-owners.mjs). They run once, in one Vitest run with no
 * filters, under `MIDGARD_WRITE_FIT_LEDGER=1` and a fragment directory and run
 * token made fresh for this run, so the parts of a split ledger can merge
 * only with each other. The directory is removed afterwards.
 *
 * A ledger is kept only when every one of its owning files passed with no
 * skipped case and the run rewrote it. Otherwise it is put back as it was, as
 * is every other file under size-plans the run changed, and the command exits
 * 1. Vitest's own checks, including the stale-blueprint refusal, apply as in
 * any run. `--list` prints each ledger's owners without running anything.
 *
 * `--check` runs the same owners the same way, except that every checked-in
 * ledger is written to a scratch directory (`MIDGARD_FIT_LEDGER_CHECK_DIR`,
 * read by `writeVanRossemFitLedger`). Each fresh ledger is then compared with
 * the checked-in one (scripts/fit-ledger-differences.mjs). The check passes
 * only when every owning file passed with no skipped case, every fresh ledger
 * matches, and the run left size-plans exactly as it found it. Anything the
 * run changed there is put back.
 */
import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import {
  existsSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  discoverFitLedgerOwners,
  packageDirectory,
  repositoryRoot,
  SIZE_PLANS,
  trackedFitLedgers,
} from "./fit-ledger-owners.mjs";
import { fitLedgerDifferences } from "./fit-ledger-differences.mjs";

const COMMAND = "pnpm --dir demo/midgard-fault-proofs fit:regenerate";
const USAGE = `Usage: ${COMMAND} [--check] <ledger>... | [--check] --all | --list`;
const FLAGS = ["--all", "--check", "--list"];

class UsageError extends Error {}

/** A ledger argument as its file name in size-plans. */
const ledgerName = (argument, sizePlans, cwd) => {
  if (!argument.includes("/")) return argument;
  const path = resolve(cwd, argument);
  if (dirname(path) !== sizePlans)
    throw new UsageError(`${argument} is not a ledger in ${SIZE_PLANS}`);
  return basename(path);
};

const fileNames = (directory) =>
  readdirSync(directory, { withFileTypes: true })
    .filter((entry) => entry.isFile())
    .map(({ name }) => name);

const snapshot = (directory) =>
  new Map(
    fileNames(directory).map((name) => {
      const path = join(directory, name);
      const { ino, mtimeMs } = statSync(path);
      return [name, { bytes: readFileSync(path), ino, mtimeMs }];
    }),
  );

/** Whether the run wrote the file: writers replace it, so any change counts. */
const written = (path, before) => {
  if (!existsSync(path)) return false;
  if (before === undefined) return true;
  const { ino, mtimeMs } = statSync(path);
  return (
    ino !== before.ino ||
    mtimeMs !== before.mtimeMs ||
    !readFileSync(path).equals(before.bytes)
  );
};

/** Why a file did not pass cleanly, or undefined when it did. */
const fileFailure = (report, path) => {
  const real = realpathSync(path);
  const result = report?.testResults?.find(
    ({ name }) => existsSync(name) && realpathSync(name) === real,
  );
  if (result === undefined) return "did not run";
  if (result.status !== "passed")
    return result.message ? `failed: ${result.message}` : "failed";
  if (result.assertionResults.length === 0) return "ran no cases";
  const unpassed = result.assertionResults.filter(
    ({ status }) => status !== "passed",
  );
  if (unpassed.length > 0)
    return `${unpassed.length.toString()} case(s) did not run to a pass`;
  return undefined;
};

/** Exactly these files, every case in them, and a per-file JSON report. */
export const vitestArguments = (files, reportPath) => [
  "run",
  "--reporter=verbose",
  "--reporter=json",
  `--outputFile.json=${reportPath}`,
  ...files,
];

/** One Vitest run over `files`, its output passed through. */
export const runVitest = ({ files, env, reportPath }) =>
  new Promise((resolveRun, rejectRun) => {
    const child = spawn(
      resolve(packageDirectory, "node_modules/.bin/vitest"),
      vitestArguments(files, reportPath),
      { cwd: packageDirectory, env, stdio: "inherit" },
    );
    // Ctrl-C reaches Vitest too; wait for it so the ledgers are restored.
    const ignore = () => {};
    process.on("SIGINT", ignore);
    child.on("error", (error) => {
      process.off("SIGINT", ignore);
      rejectRun(error);
    });
    child.on("close", (status, signal) => {
      process.off("SIGINT", ignore);
      resolveRun(status ?? (signal ? 1 : 0));
    });
  });

/**
 * Runs the owners of `ledgers` once under the write flag. With
 * `checkDirectory`, the owners write checked-in ledgers there instead. Returns
 * size-plans as it was before the run, Vitest's status, and why each ledger's
 * owners did not all pass (undefined when they did).
 */
const runOwners = async ({ ledgers, owners, sizePlans, root, run, check }) => {
  // Vitest given no file runs the whole suite.
  if (ledgers.length === 0 || ledgers.some((l) => !owners.get(l)?.length))
    throw new Error("every ledger to regenerate needs an owning test file");
  const files = [...new Set(ledgers.flatMap((ledger) => owners.get(ledger)))];
  const before = snapshot(sizePlans);
  const scratch = mkdtempSync(join(tmpdir(), "midgard-fit-regenerate-"));
  const reportPath = join(scratch, "report.json");
  const checkDirectory = join(scratch, "ledgers");
  const { MIDGARD_FIT_LEDGER_CHECK_DIR: _inherited, ...inherited } =
    process.env;
  const fresh = new Map();
  let status;
  let report;
  try {
    status = await run({
      files: files.map((file) => join(root, file)),
      reportPath,
      env: {
        ...inherited,
        MIDGARD_WRITE_FIT_LEDGER: "1",
        MIDGARD_FIT_FRAGMENT_DIR: join(scratch, "fragments"),
        MIDGARD_FIT_MEASUREMENT_RUN: `regenerate-${randomUUID()}`,
        ...(check ? { MIDGARD_FIT_LEDGER_CHECK_DIR: checkDirectory } : {}),
      },
    });
    if (existsSync(reportPath))
      report = JSON.parse(readFileSync(reportPath, "utf8"));
    for (const ledger of check ? ledgers : []) {
      const path = join(checkDirectory, ledger);
      if (existsSync(path)) fresh.set(ledger, readFileSync(path, "utf8"));
    }
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
  const failures = new Map(
    files.map((file) => [file, fileFailure(report, join(root, file))]),
  );
  // A failed run with every file passing failed outside any file.
  const unexplained =
    status !== 0 && [...failures.values()].every((why) => why === undefined);
  const notPassed = (ledger) => {
    if (unexplained)
      return `vitest exited ${status.toString()} outside any test file`;
    const failed = owners
      .get(ledger)
      .filter((file) => failures.get(file) !== undefined)
      .map((file) => `${file} ${failures.get(file)}`);
    return failed.length > 0 ? failed.join("; ") : undefined;
  };
  return { before, status, notPassed, fresh };
};

/** Puts back every file under size-plans the run changed, except `kept`. */
const restore = (sizePlans, before, kept) => {
  const restored = [];
  for (const name of fileNames(sizePlans)) {
    if (kept.has(name)) continue;
    const path = join(sizePlans, name);
    const original = before.get(name);
    if (original === undefined) rmSync(path, { force: true });
    else if (written(path, original)) writeFileSync(path, original.bytes);
    else continue;
    restored.push(name);
  }
  for (const [name, { bytes }] of before)
    if (!existsSync(join(sizePlans, name))) {
      writeFileSync(join(sizePlans, name), bytes);
      restored.push(name);
    }
  return restored;
};

/**
 * Runs the owners of `ledgers` once and keeps each ledger only if its owners
 * all passed and it was rewritten; every other change under size-plans is
 * undone. Returns the exit status and one line per ledger.
 */
export const regenerateFitLedgers = async ({
  ledgers,
  owners,
  sizePlans = join(repositoryRoot, SIZE_PLANS),
  root = packageDirectory,
  run = runVitest,
}) => {
  const { before, status, notPassed } = await runOwners({
    ledgers,
    owners,
    sizePlans,
    root,
    run,
    check: false,
  });
  const kept = new Set();
  const lines = ledgers.map((ledger) => {
    const why = notPassed(ledger);
    if (why !== undefined) return `not rewritten: ${ledger} (${why})`;
    if (!written(join(sizePlans, ledger), before.get(ledger)))
      return `not rewritten: ${ledger} (its owners passed but did not write it: ${owners.get(ledger).join(", ")})`;
    kept.add(ledger);
    return readFileSync(join(sizePlans, ledger)).equals(
      before.get(ledger)?.bytes ?? Buffer.alloc(0),
    )
      ? `rewritten: ${ledger} (same bytes as before)`
      : `rewritten: ${ledger}`;
  });
  restore(sizePlans, before, kept);
  return {
    status: kept.size === ledgers.length && status === 0 ? 0 : 1,
    lines,
  };
};

const parsed = (text) => {
  try {
    return JSON.parse(text);
  } catch {
    return undefined;
  }
};

/**
 * Runs the owners of `ledgers` once with checked-in ledgers written to a
 * scratch directory, and compares each fresh ledger with the checked-in one.
 * Returns the exit status and the lines describing each ledger.
 */
export const checkFitLedgers = async ({
  ledgers,
  owners,
  sizePlans = join(repositoryRoot, SIZE_PLANS),
  root = packageDirectory,
  run = runVitest,
}) => {
  const { before, status, notPassed, fresh } = await runOwners({
    ledgers,
    owners,
    sizePlans,
    root,
    run,
    check: true,
  });
  let current = 0;
  const lines = ledgers.map((ledger) => {
    const why = notPassed(ledger);
    if (why !== undefined) return `not checked: ${ledger} (${why})`;
    if (!fresh.has(ledger))
      return `not checked: ${ledger} (its owners passed but did not write it: ${owners.get(ledger).join(", ")})`;
    const recorded = before.get(ledger);
    if (recorded === undefined)
      return `stale: ${ledger} (no checked-in copy; regenerate it)`;
    const [saved, measured] = [
      recorded.bytes.toString("utf8"),
      fresh.get(ledger),
    ].map(parsed);
    if (saved === undefined || measured === undefined)
      return `stale: ${ledger} (${saved === undefined ? "the checked-in copy" : "the fresh ledger"} is not JSON)`;
    const differences = fitLedgerDifferences(saved, measured);
    if (differences.length === 0) {
      current++;
      return `current: ${ledger}`;
    }
    return [
      `stale: ${ledger}; regenerate it with \`${COMMAND} ${ledger}\``,
      ...differences.map((difference) => `  ${difference}`),
    ].join("\n");
  });
  const touched = restore(sizePlans, before, new Set());
  for (const name of touched)
    lines.push(
      `the run changed ${name} under ${SIZE_PLANS}; a check never writes there, so it was put back`,
    );
  return {
    status:
      current === ledgers.length && status === 0 && touched.length === 0
        ? 0
        : 1,
    lines,
  };
};

const listing = (tracked, owners) =>
  tracked.map((ledger) =>
    owners.get(ledger)?.length
      ? `${ledger}: ${owners.get(ledger).join(" ")}`
      : `${ledger}: no test writes it`,
  );

export const main = async (argv, { run = runVitest } = {}) => {
  const sizePlans = join(repositoryRoot, SIZE_PLANS);
  const { ledgers: owners } = discoverFitLedgerOwners();
  const tracked = trackedFitLedgers();
  const all = argv.includes("--all");
  const check = argv.includes("--check");
  const list = argv.includes("--list");
  const named = argv.filter((argument) => !argument.startsWith("--"));
  const unknownFlags = argv.filter(
    (argument) => argument.startsWith("--") && !FLAGS.includes(argument),
  );
  if (
    unknownFlags.length > 0 ||
    Number(all) + Number(list) + Number(named.length > 0) !== 1 ||
    (check && list)
  )
    throw new UsageError(USAGE);
  if (list) {
    console.log(listing(tracked, owners).join("\n"));
    return 0;
  }
  const cwd = process.env.INIT_CWD ?? process.cwd();
  const requested = all
    ? tracked.filter((ledger) => owners.get(ledger)?.length)
    : [...new Set(named.map((name) => ledgerName(name, sizePlans, cwd)))];
  for (const ledger of requested) {
    if (owners.get(ledger)?.length) continue;
    throw new UsageError(
      `${ledger} is not a fit ledger any test names; ${COMMAND} --list shows them`,
    );
  }
  if (all)
    for (const ledger of tracked.filter((name) => !requested.includes(name)))
      console.log(`skipped: ${ledger} (no test writes it)`);
  console.log(
    `${check ? "Checking" : "Regenerating"} ${requested.join(", ")}\nby running ${[
      ...new Set(requested.flatMap((ledger) => owners.get(ledger))),
    ].join(" ")}`,
  );
  const { status, lines } = await (
    check ? checkFitLedgers : regenerateFitLedgers
  )({
    ledgers: requested,
    owners,
    sizePlans,
    run,
  });
  console.log(`\n${lines.join("\n")}`);
  return status;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  try {
    process.exitCode = await main(process.argv.slice(2));
  } catch (error) {
    if (!(error instanceof UsageError)) throw error;
    console.error(error.message);
    process.exitCode = 2;
  }
}
