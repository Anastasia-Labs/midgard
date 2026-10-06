/**
 * Which test files write each checked-in fault-proof fit ledger, read from the
 * test sources rather than from a list kept by hand.
 *
 * A ledger is named by a string literal that resolves into
 * docs/fault-proofs/size-plans (`new URL("../../../docs/fault-proofs/size-plans/
 * <name>-fit-ledger.json", import.meta.url)`, or the `path` of a
 * `SplitFitLedger`). The module holding that literal is the ledger's site. A
 * writer is a module that calls `writeVanRossemFitLedger`,
 * `writeOrVerifyPinnedFitLedger`, `createSplitFitLedgerPart` or
 * `verifyMeasuredFitLedger` and either is a site of the ledger or imports one
 * directly. The ledger's owners are the test
 * files that import such a writer, directly or through other modules, or are
 * one. For a split ledger that is every part file, because each part imports
 * the module that declares the `SplitFitLedger`.
 *
 * Only relative imports are followed: the package's tests reach each other
 * through relative paths, and its self-referencing exports are for other
 * packages.
 */
import { execFileSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { basename, dirname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

export const packageDirectory = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "..",
);
export const repositoryRoot = resolve(packageDirectory, "../..");
export const SIZE_PLANS = "docs/fault-proofs/size-plans";

/**
 * Checked-in ledgers that no test in this package writes, with how each one is
 * written instead. A test fails when this list and the discovered set differ,
 * so a ledger that gains or loses a suite writer must be moved in or out here.
 */
const MEASURED_FRAGMENTS_ONLY =
  "its suite records measured-fit fragments (tests/support/measured-fit-ledger.ts), and nothing merges them into this ledger: verifyMeasuredFitLedger has no caller";
export const LEDGERS_WITHOUT_SUITE_WRITER = new Map([
  [
    "field-preimage-length-mismatch-v1-fit-ledger.json",
    MEASURED_FRAGMENTS_ONLY,
  ],
  [
    "input-set-uniqueness-wrongful-rejection-v1-fit-ledger.json",
    MEASURED_FRAGMENTS_ONLY,
  ],
  [
    "invalid-range-wrongful-rejection-v1-fit-ledger.json",
    MEASURED_FRAGMENTS_ONLY,
  ],
  [
    "invalid-signature-wrongful-rejection-v1-fit-ledger.json",
    "scripts/write-invalid-signature-fit-ledger.mjs writes it from the log of a passing run; see that script's usage",
  ],
  [
    "min-ada-wrongful-rejection-v1-fit-ledger.json",
    "tests/min-ada-wrongful-rejection-lifecycle.test.ts writes only the path named by MIN_ADA_FIT_LEDGER_PATH",
  ],
  ["mint-declared-asset-limit-v1-fit-ledger.json", MEASURED_FRAGMENTS_ONLY],
  ["missing-redeemer-v1-fit-ledger.json", MEASURED_FRAGMENTS_ONLY],
  ["missing-script-source-v1-fit-ledger.json", MEASURED_FRAGMENTS_ONLY],
  ["network-id-wrongful-rejection-v1-fit-ledger.json", MEASURED_FRAGMENTS_ONLY],
  [
    "protected-output-signer-missing-v1-fit-ledger.json",
    MEASURED_FRAGMENTS_ONLY,
  ],
  ["redeemer-canonicity-v1-fit-ledger.json", MEASURED_FRAGMENTS_ONLY],
  [
    "transaction-output-non-canonical-v1-fit-ledger.json",
    MEASURED_FRAGMENTS_ONLY,
  ],
  ["zero-input-wrongful-rejection-v1-fit-ledger.json", MEASURED_FRAGMENTS_ONLY],
]);

const SOURCE_FILE = /\.(?:ts|tsx|mts)$/u;
const TEST_FILE = /\.test\.tsx?$/u;
const WRITER_CALL =
  /\b(?:writeVanRossemFitLedger|writeOrVerifyPinnedFitLedger|createSplitFitLedgerPart|verifyMeasuredFitLedger)\(/u;
const RELATIVE_IMPORT =
  /(?:\bfrom\s+|\bimport\s*\(\s*|\bimport\s+)["'](\.\.?\/[^"']+)["']/gu;
const LEDGER_LITERAL =
  /["']((?:\.\.\/)+docs\/fault-proofs\/size-plans\/[\w.-]+-fit-ledger\.json)["']/gu;

const toPosix = (path) => path.split(sep).join("/");

const sourceFiles = (directory) =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) return sourceFiles(path);
    return SOURCE_FILE.test(entry.name) ? [path] : [];
  });

/** `./x.js` names `./x.ts` in this package's sources. */
const resolveImport = (from, specifier) => {
  const target = resolve(dirname(from), specifier);
  return [
    target,
    target.replace(/\.js$/u, ".ts"),
    target.replace(/\.js$/u, ".tsx"),
    `${target}.ts`,
  ].find((candidate) => SOURCE_FILE.test(candidate) && existsSync(candidate));
};

/**
 * Every fit ledger the package's sources name, with its owning test files
 * (package-relative), plus the writer modules that name no ledger at all.
 */
export const discoverFitLedgerOwners = (root = packageDirectory) => {
  const sizePlans = resolve(root, "../..", SIZE_PLANS);
  const files = ["tests", "src"]
    .map((directory) => join(root, directory))
    .filter(existsSync)
    .flatMap(sourceFiles);
  const imports = new Map();
  const sites = new Map();
  const writers = new Set();
  for (const file of files) {
    const text = readFileSync(file, "utf8");
    imports.set(
      file,
      [...text.matchAll(RELATIVE_IMPORT)]
        .map(([, specifier]) => resolveImport(file, specifier))
        .filter((target) => target !== undefined),
    );
    if (WRITER_CALL.test(text)) writers.add(file);
    for (const [, literal] of text.matchAll(LEDGER_LITERAL)) {
      const path = resolve(dirname(file), literal);
      if (dirname(path) !== sizePlans) continue;
      const ledger = basename(path);
      sites.set(ledger, new Set([...(sites.get(ledger) ?? []), file]));
    }
  }
  const importers = new Map();
  for (const [file, targets] of imports)
    for (const target of targets)
      importers.set(target, [...(importers.get(target) ?? []), file]);
  const testFilesReaching = (start) => {
    const seen = new Set([start]);
    const pending = [start];
    while (pending.length > 0)
      for (const importer of importers.get(pending.pop()) ?? [])
        if (!seen.has(importer)) {
          seen.add(importer);
          pending.push(importer);
        }
    return [...seen].filter((file) => TEST_FILE.test(file));
  };
  const relativeTo = (file) => toPosix(relative(root, file));
  const ledgers = new Map();
  const attributed = new Set();
  for (const [ledger, ledgerSites] of sites) {
    const ledgerWriters = [...writers].filter(
      (file) =>
        ledgerSites.has(file) ||
        imports.get(file).some((target) => ledgerSites.has(target)),
    );
    ledgerWriters.forEach((file) => attributed.add(file));
    ledgers.set(
      ledger,
      [...new Set(ledgerWriters.flatMap(testFilesReaching))]
        .map(relativeTo)
        .sort(),
    );
  }
  return {
    ledgers,
    unattributedWriters: [...writers]
      .filter((file) => !attributed.has(file))
      .map(relativeTo)
      .sort(),
  };
};

/** The fit ledgers checked in under docs/fault-proofs/size-plans. */
export const trackedFitLedgers = (root = repositoryRoot) =>
  execFileSync("git", ["ls-files", "-z", "--", SIZE_PLANS], {
    cwd: root,
    encoding: "utf8",
  })
    .split("\0")
    .filter((path) => path.endsWith("-fit-ledger.json"))
    .map((path) => basename(path))
    .sort();
