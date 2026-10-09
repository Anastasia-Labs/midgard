import { existsSync, realpathSync, readFileSync } from "node:fs";
import { relative, resolve } from "node:path";
import { randomUUID } from "node:crypto";
import { createRequire } from "node:module";

import { packageNeedsPostgres } from "../preflight/derive.mjs";
import { probePostgres } from "../preflight/probes.mjs";
import { ensureBlueprint } from "./blueprint.mjs";
import { dropTestDatabases, invocationDatabasePrefix } from "./databases.mjs";
import { buildPackage, checkBuild, runDirectory } from "./build.mjs";
import {
  atomicJson,
  inputIdentity,
  inside,
  packageByName,
  packageClosure,
  runtimeBuildClosure,
  workspacePackages,
  sha256,
  outputIdentity,
  hashFiles,
} from "./files.mjs";
import { runProcess } from "./process.mjs";
import { writeReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";
import { buildNative, checkNative } from "./native.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import {
  failures,
  parseTestScript,
  vitestFlags,
  withCallerFlags,
} from "./vitest-command.mjs";

// Whether a package's suites read the compiled blueprint: its own Vitest
// config runs the blueprint stamp (or interactive-emulator) global setup, or
// it depends at runtime on a package whose config does.
const runsBlueprintSetup = (root, pkg) =>
  ["vitest.config.ts", "vitest.config.mts", "vitest.config.js"].some(
    (file) =>
      existsSync(resolve(root, pkg.directory, file)) &&
      /\b(?:blueprintStampGlobalSetup|interactiveEmulatorSetup)\b/u.test(
        readFileSync(resolve(root, pkg.directory, file), "utf8"),
      ),
  );

const readsBlueprint = (root, name, seen = new Set()) => {
  const pkg = packageByName(root, name);
  if (seen.has(pkg.name)) return false;
  seen.add(pkg.name);
  const workspace = new Set(workspacePackages(root).map(({ name }) => name));
  return (
    runsBlueprintSetup(root, pkg) ||
    Object.keys(pkg.dependencies ?? {}).some(
      (dependency) =>
        workspace.has(dependency) && readsBlueprint(root, dependency, seen),
    )
  );
};

export const preparationPlan = (root, name, { sourceOnly = false } = {}) => {
  const pkg = packageByName(root, name);
  const closure = packageClosure(root, pkg.name);
  const pretest = pkg.scripts?.pretest;
  const prerequisites = new Set();
  // Build core first: several plain-Node generators and tsup configs load it.
  if (!sourceOnly) {
    if (closure.some((dependency) => dependency.name === "@al-ft/midgard-core"))
      prerequisites.add("@al-ft/midgard-core");
    for (const [, directory] of (pretest ?? "").matchAll(
      /pnpm --dir (\.\.\/[^ ]+) run build/gu,
    )) {
      prerequisites.add(
        packageByName(root, resolve(pkg.directory, directory).split("/").at(-1))
          .name,
      );
    }
    if (pretest?.includes("pnpm run build")) prerequisites.add(pkg.name);
  }
  const ordered = [
    ...new Set(
      [...prerequisites].flatMap((name) =>
        runtimeBuildClosure(root, name).map((pkg) => pkg.name),
      ),
    ),
  ];
  return {
    schema: "midgard-contrib-plan/v1",
    package: pkg.name,
    directory: pkg.directory,
    sourceOnly,
    postgres: packageNeedsPostgres(root, pkg),
    blueprint: readsBlueprint(root, pkg.name),
    prerequisites: ordered.map((name) => ({ name, ...checkBuild(root, name) })),
    pretest,
    inputSha256: inputIdentity(root, pkg.name).sha256,
  };
};

/**
 * Build what the named packages' suites need before they start: a ready
 * blueprint where they read it (copied from a checkout with identical inputs
 * and profile, else built; never a stale or other-profile one) and every
 * stale prerequisite dist, in dependency order.
 */
export const prepareArtifacts = async (
  root,
  names,
  { signal, env = process.env, sourceOnly = false } = {},
) => {
  const plans = names.map((name) =>
    preparationPlan(root, name, { sourceOnly }),
  );
  const blueprintAction = plans.some((plan) => plan.blueprint)
    ? await ensureBlueprint(root, { signal, env })
    : undefined;
  const prerequisites = [
    ...new Set(
      plans.flatMap((plan) => plan.prerequisites.map(({ name }) => name)),
    ),
  ];
  const built = [];
  for (const prerequisite of prerequisites) {
    if (checkBuild(root, prerequisite).status !== "fresh") {
      const result = await buildPackage(root, prerequisite, { signal, env });
      if (result.exitCode !== 0)
        throw new Error(`prerequisite build failed: ${result.path}`);
      built.push(prerequisite);
    }
  }
  return { blueprintAction, built };
};

export const prepare = async (
  root,
  name,
  { signal, env = process.env, sourceOnly = false } = {},
) => {
  const plan = preparationPlan(root, name, { sourceOnly });
  if (env.MIDGARD_REAL_BLUEPRINT_PATH || env.MIDGARD_BLUEPRINT_STAMP === "warn")
    throw new Error(
      "guarded runs refuse MIDGARD_REAL_BLUEPRINT_PATH and MIDGARD_BLUEPRINT_STAMP=warn: they test this checkout's own blueprint, which contrib prepare copies or builds; unset both",
    );
  if (plan.postgres && env.MIDGARD_SKIP_DB_TESTS !== "1") {
    if (
      env.POSTGRES_HOST &&
      !["127.0.0.1", "localhost"].includes(env.POSTGRES_HOST)
    )
      throw new Error("guarded tests require the local test Postgres on 5433");
    const postgres = await probePostgres({ env });
    if (postgres.status !== "available")
      throw new Error(`${postgres.detail}; ${postgres.fix}`);
  }
  const { blueprintAction } = await prepareArtifacts(root, [name], {
    signal,
    env,
    sourceOnly,
  });
  return { ...preparationPlan(root, name, { sourceOnly }), blueprintAction };
};

/** The package's own `test` script, read as one Vitest command. */
export const testCommand = (pkg) => {
  if (!pkg.scripts?.test) throw new Error(`${pkg.name} has no test script`);
  try {
    return parseTestScript(pkg.scripts.test);
  } catch (error) {
    if (!/\bvitest\b/u.test(pkg.scripts.test))
      throw new Error(
        `${pkg.name} has no Vitest suites; its test script is: ${pkg.scripts.test}`,
      );
    throw error;
  }
};

const blueprintHash = (root) => {
  const path = resolve(root, "onchain/aiken/plutus.json");
  return existsSync(path) ? sha256(readFileSync(path)) : undefined;
};

/**
 * Run a package's Vitest suites as its `test` script does (environment,
 * Vitest flags, and for a whole-package run its plain-Node preludes), plus
 * the caller's allowed flags. `files` narrows the run to those test files;
 * without them it is the whole package, as CI runs it.
 *
 * The workspace lease is held only while prerequisites (blueprint, dist,
 * native) are written; the suites themselves run beside other runs, each
 * with its own database prefix. Anything those writes would change under a
 * running suite is caught afterwards and fails the receipt.
 */
export const runTests = async (
  root,
  name,
  {
    files = [],
    testName,
    seed = 1,
    signal,
    env = process.env,
    sourceOnly = false,
    proofKind,
    flags = {},
  } = {},
) => {
  const pkg = packageByName(root, name);
  const command = withCallerFlags(testCommand(pkg), flags);
  const cwd = resolve(root, pkg.directory);
  for (const file of files) {
    const absolute = inside(cwd, file);
    if (!existsSync(absolute) || !/\.test\.[cm]?[jt]sx?$/u.test(file))
      throw new Error(`not a test file: ${file}`);
  }
  if (!Number.isSafeInteger(seed) || seed < 0)
    throw new Error("--seed must be a nonnegative integer");
  if (
    (env.POSTGRES_PORT && env.POSTGRES_PORT !== "5433") ||
    (env.POSTGRES_HOST &&
      !["localhost", "127.0.0.1"].includes(env.POSTGRES_HOST))
  )
    throw new Error(
      "unset non-test Postgres destination overrides before guarded tests",
    );
  // Refuse ambient destructive destinations even though we choose our own.
  if (
    env.POSTGRES_DB &&
    !/^midgard_(?:test|tools_test|contrib)_/u.test(env.POSTGRES_DB)
  )
    throw new Error(
      `refusing ambient POSTGRES_DB=${env.POSTGRES_DB}; unset it before tests`,
    );
  if (proofKind === "live-acceptance")
    throw new Error("synthetic tests cannot be labeled live acceptance");
  const directory = runDirectory();
  const overrides = {
    // The script's own environment, as `pnpm test` would set it; suites
    // that set none need Vitest's test runtime for test-only constructors.
    NODE_ENV: "test",
    ...command.env,
    MIDGARD_TEST_DATABASE_PREFIX: invocationDatabasePrefix(
      root,
      randomUUID().replaceAll("-", "").slice(0, 8),
    ),
    POSTGRES_HOST: "127.0.0.1",
    POSTGRES_PORT: "5433",
  };
  if (files.includes("tests/scratch-cg1-publication-fit.test.ts"))
    overrides.MIDGARD_CG1_EMIT = resolve(
      directory,
      "signed-publication-fit.json",
    );
  if (files.includes("tests/published-workflow-deployment.test.ts"))
    overrides.MIDGARD_PUBLISHED_WORKFLOW_RECEIPT_PATH = resolve(
      directory,
      "published-workflow.json",
    );
  const runEnv = { ...env, ...overrides };
  const { prepared, native } = await withResource(
    `workspace:${realpathSync(root)}`,
    async (ownedEnv) => {
      const leased = { ...ownedEnv, ...overrides };
      const prepared = await prepare(root, pkg.name, {
        signal,
        env: leased,
        sourceOnly,
      });
      if (
        !["midgard-node", "midgard-node-tools"].includes(pkg.name) ||
        runEnv.MIDGARD_SKIP_NATIVE_BUILD === "1"
      )
        return { prepared };
      if (checkNative(root, "midgard-node").status !== "fresh") {
        const built = await buildNative(root, "midgard-node", {
          signal,
          env: leased,
        });
        if (built.exitCode)
          throw new Error(`native prerequisite failed: ${built.path}`);
      }
      return {
        prepared,
        native: {
          name: "midgard-node",
          outputs: checkNative(root, "midgard-node").stamp.outputs,
        },
      };
    },
    { signal, env },
  );
  const require = createRequire(resolve(cwd, "package.json"));
  const runner = resolve(
    require.resolve("vitest/package.json"),
    "../vitest.mjs",
  );
  if (
    !realpathSync(runner).startsWith(
      `${realpathSync(resolve(root, "demo/node_modules"))}/`,
    )
  )
    throw new Error(
      "vitest resolves outside this checkout; install its locked workspace dependencies",
    );
  const selectedFiles = await collect({
    runner,
    cwd,
    env: runEnv,
    files,
    command,
    directory,
    signal,
  });
  const whole = files.length === 0;
  const blueprintBefore = prepared.blueprint ? blueprintHash(root) : undefined;
  const before = inputIdentity(root, pkg.name);
  const reportPath = resolve(directory, "vitest.json");
  const steps = [];
  let databaseCleanup;
  try {
    // A focused run is the package's Vitest suites only; the plain-Node
    // preludes belong to the whole-package run CI makes.
    if (whole && !testName)
      for (const [index, prelude] of command.preludes.entries()) {
        const [tool, ...args] = prelude.split(/\s+/u);
        if (tool !== "pnpm")
          throw new Error(`cannot run test script step: ${prelude}`);
        steps.push(
          await runProcess({
            ...pinnedPnpm(cwd, args),
            env: runEnv,
            signal,
            logPath: resolve(directory, `prelude-${index}.log`),
          }),
        );
      }
    steps.push(
      await runProcess({
        argv: [
          process.execPath,
          runner,
          "run",
          ...files,
          ...vitestFlags(command),
          "--reporter=default",
          "--reporter=json",
          `--outputFile=${reportPath}`,
          `--sequence.seed=${seed}`,
          "--sequence.shuffle.files",
          "--sequence.shuffle.tests",
          ...(testName ? ["--testNamePattern", testName] : []),
        ],
        cwd,
        env: runEnv,
        signal,
        logPath: resolve(directory, "test.log"),
        echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
        // A whole package is what CI shards; give it room to finish.
        ...(whole ? { timeoutMs: 3 * 3_600_000, maxBytes: 256 << 20 } : {}),
      }),
    );
  } finally {
    // The invocation's databases and schemas die with it; the prefix is
    // fresh, so nothing else can be using them.
    databaseCleanup = await dropTestDatabases(
      root,
      [runEnv.MIDGARD_TEST_DATABASE_PREFIX],
      { env: runEnv },
    ).catch((error) => ({ status: "failed", detail: error.message }));
  }
  const after = inputIdentity(root, pkg.name);
  const receipt = writeReceipt({
    root,
    pkg,
    directory,
    kind: "test",
    before,
    after,
    steps,
    reportPath,
    proofKind,
    testName,
    selectedFiles,
  });
  const fail = (reason) => {
    receipt.status = "failed";
    receipt.exitCode = 1;
    receipt.reason = reason;
  };
  // Stamp validation catches dist replacement even with identical sources.
  const changedArtifacts = prepared.prerequisites.filter(
    ({ name, stamp }) =>
      checkBuild(root, name).status !== "fresh" ||
      stamp.outputs.sha256 !==
        outputIdentity(root, `${packageByName(root, name).directory}/dist`)
          .sha256,
  );
  if (changedArtifacts.length)
    fail(
      `artifacts changed during test: ${changedArtifacts.map(({ name }) => name).join(", ")}`,
    );
  if (
    native &&
    (checkNative(root, native.name).status !== "fresh" ||
      native.outputs.sha256 !==
        hashFiles(root, Object.keys(native.outputs.files)).sha256)
  )
    fail("native artifact changed during test");
  if (prepared.blueprint && blueprintHash(root) !== blueprintBefore)
    fail("the blueprint changed during test");
  let failed = [];
  try {
    failed = failures(JSON.parse(readFileSync(reportPath, "utf8")), cwd);
  } catch {
    // No report: the receipt's reportError and step logs say why.
  }
  const final = {
    ...receipt,
    seed,
    flags: vitestFlags(command),
    failures: failed,
    blueprintAction: prepared.blueprintAction,
    databasePrefix: runEnv.MIDGARD_TEST_DATABASE_PREFIX,
    databaseCleanup,
    sourceOnly,
    nativeArtifacts: native ? [native] : [],
    evidenceFiles: [
      runEnv.MIDGARD_CG1_EMIT,
      runEnv.MIDGARD_PUBLISHED_WORKFLOW_RECEIPT_PATH,
    ]
      .filter(Boolean)
      .map((path) => ({
        path,
        sha256: existsSync(path) ? sha256(readFileSync(path)) : undefined,
      })),
    artifacts: prepared.prerequisites.map(({ name, stamp }) => ({
      name,
      outputs: stamp?.outputs,
      inputs: stamp?.inputs.sha256,
    })),
  };
  if (final.evidenceFiles.some((entry) => !entry.sha256)) {
    final.status = "failed";
    final.exitCode = 1;
    final.reason =
      "selected publication driver did not emit its measurement evidence";
  }
  atomicJson(receipt.path, final);
  return final;
};

/**
 * The files the run will execute, as Vitest itself collects them with the
 * same configuration, environment and flags. Named files must each be
 * collected, and nothing else: Vitest's file arguments are substring filters,
 * and an `--exclude` can drop a named file without a word.
 */
const collect = async ({
  runner,
  cwd,
  env,
  files,
  command,
  directory,
  signal,
}) => {
  const listPath = resolve(directory, "vitest-list.json");
  const listed = await runProcess({
    argv: [
      process.execPath,
      runner,
      "list",
      ...files,
      ...vitestFlags(command),
      "--filesOnly",
      `--json=${listPath}`,
    ],
    cwd,
    env,
    signal,
    logPath: resolve(directory, "list.log"),
  });
  if (listed.exitCode !== 0 || !existsSync(listPath))
    throw new Error(`vitest list failed; log: ${listed.logPath}`);
  const collected = [
    ...new Set(
      JSON.parse(readFileSync(listPath, "utf8")).map(({ file }) => file),
    ),
  ].sort();
  if (!files.length) {
    if (!collected.length) throw new Error("vitest collected no test files");
    return collected;
  }
  const named = [...new Set(files.map((file) => resolve(cwd, file)))].sort();
  const missing = named.filter((file) => !collected.includes(file));
  const extra = collected.filter((file) => !named.includes(file));
  if (missing.length)
    throw new Error(
      `vitest does not collect ${missing.map((file) => relative(cwd, file)).join(", ")} (excluded by the package's Vitest config or an --exclude)`,
    );
  if (extra.length)
    throw new Error(
      `the file filters also select ${extra.map((file) => relative(cwd, file)).join(", ")}; name a path that selects only the intended file`,
    );
  return named;
};
