import {
  packageBuildTask,
  packageTestTask,
  taskEnvironment,
} from "../../demo/scripts/package-tasks.mjs";
import { existsSync, realpathSync, readFileSync } from "node:fs";
import { relative, resolve } from "node:path";
import { randomUUID } from "node:crypto";
import { createRequire } from "node:module";

import { probePostgres } from "../preflight/probes.mjs";
import { blueprintReadiness, ensureBlueprint } from "./blueprint.mjs";
import { dropTestDatabases, invocationDatabasePrefix } from "./databases.mjs";
import { buildPackage, checkBuild, runDirectory } from "./build.mjs";
import {
  atomicJson,
  inputIdentity,
  inside,
  packageByName,
  buildClosure,
  sha256,
  buildOutputIdentity,
  hashFiles,
} from "./files.mjs";
import { runProcess } from "./process.mjs";
import { writeReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";
import { buildNative, checkNative } from "./native.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { reachedTests } from "./reached.mjs";
import {
  failures,
  declaredTestCommand,
  vitestFlags,
  withCallerFlags,
} from "./vitest-command.mjs";

export const preparationPlan = (root, name, { sourceOnly = false } = {}) => {
  const pkg = packageByName(root, name);
  const task = packageTestTask(pkg);
  const declared = task?.prerequisites ?? [];
  for (const entry of declared.filter((entry) => entry.kind === "build"))
    if (!packageBuildTask(packageByName(root, entry.package)))
      throw new Error(
        `${pkg.name}: test build prerequisite ${entry.package} has no build task`,
      );
  const ordered = sourceOnly
    ? []
    : [
        ...new Set(
          declared
            .filter((entry) => entry.kind === "build")
            .flatMap((entry) =>
              buildClosure(root, entry.package)
                .filter(packageBuildTask)
                .map((pkg) => pkg.name),
            ),
        ),
      ];
  return {
    schema: "midgard-contrib-plan/v1",
    package: pkg.name,
    directory: pkg.directory,
    sourceOnly,
    postgres: declared.some((entry) => entry.kind === "postgres"),
    blueprint: declared.some((entry) => entry.kind === "blueprint"),
    native: declared
      .filter((entry) => entry.kind === "native")
      .map((entry) => entry.package),
    prerequisites: ordered.map((name) => ({ name, ...checkBuild(root, name) })),
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

/** Native artifacts share their owner across ordinary and guarded tests. */
export const prepareNativeArtifacts = async (
  root,
  packageName,
  { signal, env = process.env } = {},
) => {
  const artifacts = [];
  if (env.MIDGARD_SKIP_NATIVE_BUILD !== "1")
    for (const name of preparationPlan(root, packageName).native) {
      if (checkNative(root, name).status !== "fresh") {
        const built = await buildNative(root, name, { signal, env });
        if (built.exitCode)
          throw new Error(`native prerequisite failed: ${built.path}`);
      }
      artifacts.push({ name, outputs: checkNative(root, name).stamp.outputs });
    }
  return artifacts;
};

// A blueprint override passes only as a byte-identical copy of this
// checkout's own ready blueprint: a devnet campaign freezes one for the
// journeys, which require the variable. Anything else tests other contracts.
const requireOwnBlueprint = (root, env) => {
  const override = resolve(env.MIDGARD_REAL_BLUEPRINT_PATH);
  const readiness = blueprintReadiness(root);
  if (
    !readiness.ready ||
    !existsSync(override) ||
    sha256(readFileSync(override)) !== blueprintHash(root)
  )
    throw new Error(
      `guarded runs refuse MIDGARD_REAL_BLUEPRINT_PATH=${override} unless it is a byte-identical copy of this checkout's ready blueprint: they test this checkout's own contracts${readiness.ready ? "" : ` (${readiness.reason})`}; unset it`,
    );
};

export const prepare = async (
  root,
  name,
  { signal, env = process.env, sourceOnly = false } = {},
) => {
  const plan = preparationPlan(root, name, { sourceOnly });
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
  if (env.MIDGARD_REAL_BLUEPRINT_PATH) requireOwnBlueprint(root, env);
  return { ...preparationPlan(root, name, { sourceOnly }), blueprintAction };
};

/** The package's declared test command, shared with ordinary package tests. */
export const testCommand = (pkg) => {
  const task = packageTestTask(pkg);
  if (!task)
    throw new Error(
      `${pkg.name} has no Vitest suites; declare midgardTasks.test`,
    );
  return declaredTestCommand(task);
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
    related = [],
    testName,
    seed = 1,
    signal,
    env = process.env,
    sourceOnly = false,
    proofKind,
    flags = {},
    timeoutMs,
    reach: findReach = reachedTests,
  } = {},
) => {
  const pkg = packageByName(root, name);
  const command = withCallerFlags(testCommand(pkg), flags);
  const cwd = resolve(root, pkg.directory);
  if (related.length && files.length)
    throw new Error("--related and --file are exclusive");
  // Another config is for a selection the default run leaves out on purpose
  // (the watcher journeys, some of which need a live devnet): name its files.
  if (flags.config !== undefined) {
    if (
      !/^vitest\.[\w-]+\.config\.[cm]?[jt]s$/u.test(flags.config) ||
      !existsSync(resolve(cwd, flags.config))
    )
      throw new Error(
        `--config takes a Vitest config file in ${pkg.directory}, such as vitest.watcher-journeys.config.ts: ${flags.config}`,
      );
    if (!files.length)
      throw new Error("--config runs only the files --file names");
  }
  for (const file of files) {
    const absolute = inside(cwd, file);
    if (!existsSync(absolute) || !/\.test\.[cm]?[jt]sx?$/u.test(file))
      throw new Error(`not a test file: ${file}`);
  }
  if (!Number.isSafeInteger(seed) || seed < 0)
    throw new Error("--seed must be a nonnegative integer");
  // Node timers fire at once beyond 2^31-1 ms (about 24.8 days).
  if (
    timeoutMs !== undefined &&
    !(Number.isSafeInteger(timeoutMs) && timeoutMs > 0 && timeoutMs < 2 ** 31)
  )
    throw new Error(
      "--timeout-seconds must be a positive whole number of seconds below 2147484",
    );
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
  // The tests a set of changed files reaches (reached.mjs): none skips the
  // run, an unsure answer runs the whole package.
  let reach;
  if (related.length) {
    const base = realpathSync(root);
    const changed = [
      ...new Set(related.map((path) => relative(base, inside(base, path)))),
    ].sort();
    reach = {
      changed,
      ...(await findReach(root, pkg, command, changed, {
        env,
        signal,
        directory,
      })),
    };
    // Plain-Node preludes are not graphed: a change that can reach the
    // package runs them, and with no Vitest file reached, the whole script.
    if (
      !reach.whole &&
      !reach.files.length &&
      command.preludes.length &&
      reach.relevant.length
    ) {
      reach.whole = true;
      reach.why.push("its plain-Node test steps are not graphed");
    }
    if (!reach.whole && !reach.files.length)
      return {
        schema: "midgard-contrib-reach/v1",
        kind: "test",
        package: pkg.name,
        status: "not-reached",
        exitCode: 0,
        reach,
      };
    files = reach.whole ? [] : reach.files;
  }
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
      const native = await prepareNativeArtifacts(root, pkg.name, {
        signal,
        env: leased,
      });
      return { prepared, native };
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
    // Reached files are Vitest's own paths; whatever else their substring
    // filters select only widens the run.
    widen: Boolean(reach),
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
    if ((whole || reach) && !testName)
      for (const [index, prelude] of command.preludes.entries()) {
        const [tool, ...args] = prelude.command;
        if (tool !== "pnpm")
          throw new Error(`cannot run test script step: ${prelude}`);
        steps.push(
          await runProcess({
            ...pinnedPnpm(cwd, args),
            env: taskEnvironment(prelude, runEnv),
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
          // Files run in seeded order, which exposes state leaking between
          // files; tests in a file keep their declared order, as in CI, since
          // a lifecycle suite's last test checks what the earlier ones did.
          `--sequence.seed=${seed}`,
          "--sequence.shuffle.files",
          ...(testName ? ["--testNamePattern", testName] : []),
        ],
        cwd,
        env: runEnv,
        signal,
        logPath: resolve(directory, "test.log"),
        echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
        // A whole package is what CI shards, and a caller's deadline (a
        // devnet journey) can be longer still; give either room to finish.
        ...(timeoutMs !== undefined
          ? { timeoutMs, maxBytes: 256 << 20 }
          : whole
            ? { timeoutMs: 3 * 3_600_000, maxBytes: 256 << 20 }
            : {}),
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
        buildOutputIdentity(root, packageByName(root, name)).sha256,
  );
  if (changedArtifacts.length)
    fail(
      `artifacts changed during test: ${changedArtifacts.map(({ name }) => name).join(", ")}`,
    );
  if (
    native.some(
      (artifact) =>
        checkNative(root, artifact.name).status !== "fresh" ||
        artifact.outputs.sha256 !==
          hashFiles(root, Object.keys(artifact.outputs.files)).sha256,
    )
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
    ...(reach ? { reach } : {}),
    databasePrefix: runEnv.MIDGARD_TEST_DATABASE_PREFIX,
    databaseCleanup,
    sourceOnly,
    nativeArtifacts: native,
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
  widen = false,
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
  if (widen) return collected;
  if (extra.length)
    throw new Error(
      `the file filters also select ${extra.map((file) => relative(cwd, file)).join(", ")}; name a path that selects only the intended file`,
    );
  return named;
};
