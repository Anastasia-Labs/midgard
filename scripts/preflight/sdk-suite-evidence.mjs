// Same-run evidence for exactly the two complete SDK package test recipes.
// This is not a cache or evidence imported from another invocation.
import {
  existsSync,
  lstatSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  realpathSync,
} from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";
import {
  filesUnder,
  hashFiles,
  inputIdentity,
  sha256,
} from "../contrib/files.mjs";
import { countsFromVitest } from "../contrib/receipts.mjs";

export const SDK_SUITES = {
  "@al-ft/lucid-midgard": "demo/lucid-midgard",
  "@al-ft/midgard-sdk": "demo/midgard-sdk",
};
const SDK_SCRIPT =
  "pnpm --filter @al-ft/lucid-midgard test && pnpm --filter @al-ft/midgard-sdk test";
const json = (root, path) =>
  JSON.parse(readFileSync(resolve(root, path), "utf8"));
export const sdkSuiteStep = (step) =>
  Object.hasOwn(SDK_SUITES, step.sdkSuite ?? "") &&
  step.cwd === "demo" &&
  !step.env &&
  JSON.stringify(step.argv) ===
    JSON.stringify(["pnpm", "--filter", step.sdkSuite, "test"]);
export const sdkLaneStep = (steps) =>
  steps.length === 1 &&
  (!steps[0].cwd || steps[0].cwd === ".") &&
  !steps[0].env &&
  JSON.stringify(steps[0].argv) ===
    JSON.stringify(["pnpm", "--dir", "demo", "run", "test:tx-prep:sdk"]);

// Bind actual installed bytes and link routing, not just the lockfile. Do not
// follow links: walk the entire physical demo tree, including ignored outputs,
// so any admitted target has its bytes bound. Only known Vitest timing-result
// files are outputs; executable cache contents remain inputs.
const installedIdentity = (root, packages) => {
  const files = [];
  const links = [];
  const roots = new Set([
    resolve(root, "demo/node_modules"),
    ...packages.map((pkg) => resolve(root, pkg.directory, "node_modules")),
  ]);
  const demo = resolve(root, "demo");
  const caches = [...roots].flatMap((path) =>
    [".vite", ".vitest"].map((name) => resolve(path, name)),
  );
  const results = new Set(
    caches.flatMap((path) => [
      resolve(path, "results.json"),
      resolve(path, "vitest/results.json"),
    ]),
  );
  const within = (base, path) =>
    path === base || path.startsWith(`${base}${sep}`);
  const bound = (path) =>
    within(demo, path) && !caches.some((cache) => within(cache, path));
  const visit = (path) => {
    if (results.has(path)) return;
    const stat = lstatSync(path, { throwIfNoEntry: false });
    if (!stat) {
      links.push([relative(root, path), "missing"]);
      return;
    }
    if (stat.isSymbolicLink()) {
      const target = readlinkSync(path);
      const absolute = resolve(dirname(path), target);
      // Resolve the link itself: lexical normalization of its target would
      // erase '..' after a symlink and can check a different filesystem route.
      if (!bound(absolute) || !bound(realpathSync.native(path)))
        throw new Error("installed dependency link leaves the bound closure");
      links.push([relative(root, path), target]);
    } else if (stat.isDirectory()) {
      for (const entry of readdirSync(path).sort()) {
        visit(resolve(path, entry));
      }
    } else if (stat.isFile()) files.push(path);
    else throw new Error("unsupported installed dependency input");
  };
  if (
    !lstatSync(resolve(root, "demo/node_modules"), {
      throwIfNoEntry: false,
    })?.isDirectory()
  )
    throw new Error("missing local installed dependencies");
  visit(demo);
  return [hashFiles(root, files).sha256, links];
};

export const sdkContext = (root, env) => {
  if (
    json(root, "demo/package.json").scripts?.["test:tx-prep:sdk"] !== SDK_SCRIPT
  )
    throw new Error("SDK lane recipe changed");
  // Physical discovery prevents manifest.directory metadata from redirecting
  // the fingerprint away from the files and compiled outputs actually read.
  const closure = readdirSync(resolve(root, "demo"), { withFileTypes: true })
    .filter(
      (entry) =>
        entry.isDirectory() &&
        existsSync(resolve(root, "demo", entry.name, "package.json")),
    )
    .map((entry) => ({
      directory: `demo/${entry.name}`,
      name: json(root, `demo/${entry.name}/package.json`).name,
    }));
  if (env.MIDGARD_REAL_BLUEPRINT_PATH || env.MIDGARD_BLUEPRINT_STAMP === "warn")
    throw new Error("unsupported SDK blueprint profile");
  const inputs = [inputIdentity(root, "@repository").sha256];
  for (const [name, directory] of Object.entries(SDK_SUITES)) {
    const manifest = json(root, `${directory}/package.json`);
    if (manifest.name !== name || manifest.scripts?.test !== "vitest run")
      throw new Error("SDK full-suite recipe missing or changed");
    if (closure.filter((pkg) => pkg.name === name).length !== 1)
      throw new Error("ambiguous SDK package inventory");
    const identity = inputIdentity(root, name);
    if (
      identity.missing.some((path) =>
        path.startsWith("onchain/aiken/plutus.json"),
      )
    )
      throw new Error("missing blueprint or stamp");
    inputs.push(identity.sha256);
  }
  return sha256(
    JSON.stringify([
      inputs,
      installedIdentity(root, closure),
      process.version,
      process.platform,
      process.arch,
      process.execArgv,
      sha256(readFileSync(process.execPath)),
      Object.entries(env).sort(([a], [b]) => a.localeCompare(b)),
    ]),
  );
};

export const sdkSelectedFiles = (root, name) =>
  filesUnder(resolve(root, SDK_SUITES[name], "tests")).filter((path) =>
    /\.test\.tsx?$/u.test(path),
  );
export const sdkReportCounts = (root, name, report) => {
  const files = sdkSelectedFiles(root, name);
  const counts = countsFromVitest(report, undefined, files);
  if (
    !files.length ||
    report.success !== true ||
    counts.executed === 0 ||
    counts.failed ||
    counts.skipped ||
    counts.todo ||
    counts.setupErrors ||
    Number(report.numTotalTests) !== counts.passed ||
    report.testResults.some(
      (suite) => suite.status !== "passed" || !suite.assertionResults?.length,
    )
  )
    throw new Error(
      "SDK suite has missing, failed, skipped, todo or setup-refused coverage",
    );
  return counts;
};
