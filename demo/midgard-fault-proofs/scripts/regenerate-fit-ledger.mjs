/**
 * Regenerate checked-in fault-proof fit ledgers in one command.
 *
 *   pnpm --dir demo/midgard-fault-proofs fit:regenerate <ledger>...
 *   pnpm --dir demo/midgard-fault-proofs fit:regenerate --all
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
  LEDGERS_WITHOUT_SUITE_WRITER,
  packageDirectory,
  repositoryRoot,
  SIZE_PLANS,
  trackedFitLedgers,
} from "./fit-ledger-owners.mjs";

const COMMAND = "pnpm --dir demo/midgard-fault-proofs fit:regenerate";
const USAGE = `Usage: ${COMMAND} <ledger>... | --all | --list`;

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
  // Vitest given no file runs the whole suite.
  if (ledgers.length === 0 || ledgers.some((l) => !owners.get(l)?.length))
    throw new Error("every ledger to regenerate needs an owning test file");
  const files = [...new Set(ledgers.flatMap((ledger) => owners.get(ledger)))];
  const before = snapshot(sizePlans);
  const scratch = mkdtempSync(join(tmpdir(), "midgard-fit-regenerate-"));
  const reportPath = join(scratch, "report.json");
  let status;
  let report;
  try {
    status = await run({
      files: files.map((file) => join(root, file)),
      reportPath,
      env: {
        ...process.env,
        MIDGARD_WRITE_FIT_LEDGER: "1",
        MIDGARD_FIT_FRAGMENT_DIR: join(scratch, "fragments"),
        MIDGARD_FIT_MEASUREMENT_RUN: `regenerate-${randomUUID()}`,
      },
    });
    if (existsSync(reportPath))
      report = JSON.parse(readFileSync(reportPath, "utf8"));
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
  const failures = new Map(
    files.map((file) => [file, fileFailure(report, join(root, file))]),
  );
  // A failed run with every file passing failed outside any file.
  const unexplained =
    status !== 0 && [...failures.values()].every((why) => why === undefined);
  const kept = new Set();
  const lines = ledgers.map((ledger) => {
    const failed = owners
      .get(ledger)
      .filter((file) => failures.get(file) !== undefined)
      .map((file) => `${file} ${failures.get(file)}`);
    if (unexplained)
      return `not rewritten: ${ledger} (vitest exited ${status.toString()} outside any test file)`;
    if (failed.length > 0)
      return `not rewritten: ${ledger} (${failed.join("; ")})`;
    if (!written(join(sizePlans, ledger), before.get(ledger)))
      return `not rewritten: ${ledger} (its owners passed but did not write it: ${owners.get(ledger).join(", ")})`;
    kept.add(ledger);
    return readFileSync(join(sizePlans, ledger)).equals(
      before.get(ledger)?.bytes ?? Buffer.alloc(0),
    )
      ? `rewritten: ${ledger} (same bytes as before)`
      : `rewritten: ${ledger}`;
  });
  for (const name of fileNames(sizePlans)) {
    if (kept.has(name)) continue;
    const path = join(sizePlans, name);
    const original = before.get(name);
    if (original === undefined) rmSync(path, { force: true });
    else if (written(path, original)) writeFileSync(path, original.bytes);
  }
  for (const [name, { bytes }] of before)
    if (!existsSync(join(sizePlans, name)))
      writeFileSync(join(sizePlans, name), bytes);
  return {
    status: kept.size === ledgers.length && status === 0 ? 0 : 1,
    lines,
  };
};

const listing = (tracked, owners) =>
  tracked.map((ledger) =>
    owners.get(ledger)?.length
      ? `${ledger}: ${owners.get(ledger).join(" ")}`
      : `${ledger}: no test writes it; ${LEDGERS_WITHOUT_SUITE_WRITER.get(ledger) ?? "unknown"}`,
  );

const main = async (argv) => {
  const sizePlans = join(repositoryRoot, SIZE_PLANS);
  const { ledgers: owners } = discoverFitLedgerOwners();
  const tracked = trackedFitLedgers();
  const all = argv.includes("--all");
  const list = argv.includes("--list");
  const named = argv.filter((argument) => !argument.startsWith("--"));
  const unknownFlags = argv.filter(
    (argument) =>
      argument.startsWith("--") && !["--all", "--list"].includes(argument),
  );
  if (
    unknownFlags.length > 0 ||
    Number(all) + Number(list) + Number(named.length > 0) !== 1
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
    if (LEDGERS_WITHOUT_SUITE_WRITER.has(ledger))
      throw new UsageError(
        `No test writes ${ledger}: ${LEDGERS_WITHOUT_SUITE_WRITER.get(ledger)}`,
      );
    throw new UsageError(
      `${ledger} is not a fit ledger any test names; ${COMMAND} --list shows them`,
    );
  }
  if (all)
    for (const ledger of tracked.filter((name) => !requested.includes(name)))
      console.log(
        `skipped: ${ledger} (no test writes it; ${LEDGERS_WITHOUT_SUITE_WRITER.get(ledger)})`,
      );
  console.log(
    `Regenerating ${requested.join(", ")}\nby running ${[
      ...new Set(requested.flatMap((ledger) => owners.get(ledger))),
    ].join(" ")}`,
  );
  const { status, lines } = await regenerateFitLedgers({
    ledgers: requested,
    owners,
    sizePlans,
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
