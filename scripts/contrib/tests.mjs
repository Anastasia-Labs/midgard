import { existsSync, realpathSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { randomUUID } from "node:crypto";
import { createRequire } from "node:module";

import { probeBlueprintStamp, probePostgres } from "../preflight/probes.mjs";
import { buildPackage, checkBuild, runDirectory } from "./build.mjs";
import {
  atomicJson,
  inputIdentity,
  inside,
  packageByName,
  packageClosure,
  runtimeBuildClosure,
  sha256,
  outputIdentity,
  hashFiles,
} from "./files.mjs";
import { runProcess } from "./process.mjs";
import { writeReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";
import { buildNative, checkNative } from "./native.mjs";

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
    postgres: [
      "midgard-node",
      "midgard-node-tools",
      "da-committee-node",
    ].includes(pkg.name),
    blueprint: [
      "midgard-node",
      "@al-ft/midgard-sdk",
      "@al-ft/midgard-fault-proofs",
      "midgard-watcher",
      "da-committee-node",
    ].includes(pkg.name),
    prerequisites: ordered.map((name) => ({ name, ...checkBuild(root, name) })),
    pretest,
    inputSha256: inputIdentity(root, pkg.name).sha256,
  };
};

export const prepare = async (
  root,
  name,
  { signal, env = process.env, sourceOnly = false } = {},
) => {
  const plan = preparationPlan(root, name, { sourceOnly });
  if (env.MIDGARD_REAL_BLUEPRINT_PATH || env.MIDGARD_BLUEPRINT_STAMP === "warn")
    throw new Error(
      "guarded runs refuse blueprint path/warning overrides; build the selected checkout profile with pnpm --dir demo deployment:build preprod-testing",
    );
  if (plan.blueprint) {
    const blueprint = await probeBlueprintStamp({ root });
    if (blueprint.status !== "available")
      throw new Error(`${blueprint.detail}; ${blueprint.fix}`);
  }
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
  for (const prerequisite of plan.prerequisites) {
    if (checkBuild(root, prerequisite.name).status !== "fresh") {
      const built = await buildPackage(root, prerequisite.name, {
        signal,
        env,
      });
      if (built.exitCode !== 0)
        throw new Error(`prerequisite build failed: ${built.path}`);
    }
  }
  return preparationPlan(root, name, { sourceOnly });
};

export const runTests = async (
  root,
  name,
  {
    files,
    testName,
    seed = 1,
    signal,
    env = process.env,
    sourceOnly = false,
    proofKind,
  } = {},
) => {
  const pkg = packageByName(root, name);
  if (!files?.length)
    throw new Error(
      "focused tests require at least one --file, relative to the package; use contrib gate for named complete lanes",
    );
  for (const file of files) {
    const absolute = inside(resolve(root, pkg.directory), file);
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
  if (proofKind === "live-acceptance")
    throw new Error("synthetic tests cannot be labeled live acceptance");
  return withResource(
    `workspace:${realpathSync(root)}`,
    async (ownedEnv) => {
      const directory = runDirectory();
      const suffix = sha256(`${realpathSync(root)}:${randomUUID()}`).slice(
        0,
        16,
      );
      const runEnv = {
        ...ownedEnv,
        // Match package test recipes: node suites use emulator configuration;
        // other suites require Vitest's test runtime for test-only constructors.
        NODE_ENV: ["midgard-node", "midgard-node-tools"].includes(pkg.name)
          ? "emulator"
          : "test",
        MIDGARD_TEST_DATABASE_PREFIX: `midgard_contrib_${suffix}`,
        POSTGRES_HOST: "127.0.0.1",
        POSTGRES_PORT: "5433",
      };
      if (files.includes("tests/scratch-cg1-publication-fit.test.ts"))
        runEnv.MIDGARD_CG1_EMIT = resolve(
          directory,
          "signed-publication-fit.json",
        );
      if (files.includes("tests/published-workflow-deployment.test.ts"))
        runEnv.MIDGARD_PUBLISHED_WORKFLOW_RECEIPT_PATH = resolve(
          directory,
          "published-workflow.json",
        );
      // Refuse ambient destructive destinations even though we choose our own.
      if (
        env.POSTGRES_DB &&
        !/^midgard_(?:test|tools_test|contrib)_/u.test(env.POSTGRES_DB)
      )
        throw new Error(
          `refusing ambient POSTGRES_DB=${env.POSTGRES_DB}; unset it before tests`,
        );
      const prepared = await prepare(root, pkg.name, {
        signal,
        env: runEnv,
        sourceOnly,
      });
      let native;
      if (
        ["midgard-node", "midgard-node-tools"].includes(pkg.name) &&
        runEnv.MIDGARD_SKIP_NATIVE_BUILD !== "1"
      ) {
        if (checkNative(root, "midgard-node").status !== "fresh") {
          const built = await buildNative(root, "midgard-node", {
            signal,
            env: runEnv,
          });
          if (built.exitCode)
            throw new Error(`native prerequisite failed: ${built.path}`);
        }
        native = {
          name: "midgard-node",
          outputs: checkNative(root, "midgard-node").stamp.outputs,
        };
      }
      const cwd = resolve(root, pkg.directory);
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
      const before = inputIdentity(root, pkg.name);
      const reportPath = resolve(directory, "vitest.json");
      const argv = [
        process.execPath,
        runner,
        "run",
        ...files,
        "--reporter=default",
        "--reporter=json",
        `--outputFile=${reportPath}`,
        `--sequence.seed=${seed}`,
        "--sequence.shuffle.files",
        "--sequence.shuffle.tests",
        ...(testName ? ["--testNamePattern", testName] : []),
      ];
      const step = await runProcess({
        argv,
        cwd,
        env: runEnv,
        signal,
        logPath: resolve(directory, "test.log"),
        echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
      });
      const after = inputIdentity(root, pkg.name);
      const receipt = writeReceipt({
        root,
        pkg,
        directory,
        kind: "test",
        before,
        after,
        steps: [step],
        reportPath,
        proofKind,
        testName,
        selectedFiles: files.map((file) => resolve(cwd, file)),
      });
      // Stamp validation catches dist replacement even with identical sources.
      const changedArtifacts = prepared.prerequisites.filter(
        ({ name, stamp }) =>
          checkBuild(root, name).status !== "fresh" ||
          stamp.outputs.sha256 !==
            outputIdentity(root, `${packageByName(root, name).directory}/dist`)
              .sha256,
      );
      if (changedArtifacts.length) {
        receipt.status = "failed";
        receipt.exitCode = 1;
        receipt.reason = `artifacts changed during test: ${changedArtifacts.map(({ name }) => name).join(", ")}`;
      }
      if (
        native &&
        (checkNative(root, native.name).status !== "fresh" ||
          native.outputs.sha256 !==
            hashFiles(root, Object.keys(native.outputs.files)).sha256)
      ) {
        receipt.status = "failed";
        receipt.exitCode = 1;
        receipt.reason = "native artifact changed during test";
      }
      const final = {
        ...receipt,
        seed,
        databasePrefix: runEnv.MIDGARD_TEST_DATABASE_PREFIX,
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
    },
    { signal, env },
  );
};
