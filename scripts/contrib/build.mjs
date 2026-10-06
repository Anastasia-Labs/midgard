import {
  existsSync,
  mkdirSync,
  readFileSync,
  realpathSync,
  rmSync,
} from "node:fs";
import { resolve } from "node:path";
import { randomUUID } from "node:crypto";

import {
  BUILD_TRACE,
  TRACER,
  buildEnvironment,
  buildRefusals,
  unboundReads,
} from "./build-inputs.mjs";
import {
  atomicJson,
  compiledDependencies,
  inputIdentity,
  json,
  outputIdentity,
  packageByName,
  runtimeBuildClosure,
  withIdentityScope,
} from "./files.mjs";
import { runProcess } from "./process.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { writeReceipt } from "./receipts.mjs";
import { resourceDirectory, withResource } from "./resources.mjs";

export const runDirectory = () => {
  const directory = resolve(resourceDirectory(), "runs", randomUUID());
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  return directory;
};

export const checkBuild = (root, name, { env = process.env } = {}) =>
  withIdentityScope(() => {
    const pkg = packageByName(root, name);
    const path = resolve(root, pkg.directory, "dist/.contrib-build-v1.json");
    if (!existsSync(path))
      return {
        status: "missing",
        reason: `missing digest stamp for ${pkg.name}; run contrib build --package ${pkg.name}`,
      };
    try {
      const stamp = json(path);
      if (
        stamp.schema !== "midgard-contrib-build/v1" ||
        stamp.package !== pkg.name ||
        stamp.root !== realpathSync(root)
      )
        throw new Error("build belongs to another package/checkout");
      if (stamp.reads !== BUILD_TRACE)
        throw new Error("stamp predates traced builds");
      const refusals = buildRefusals(root, pkg.name);
      if (refusals.length)
        throw new Error(
          `${refusals.join("; ")}, so it is never provably fresh`,
        );
      if (stamp.inputs.sha256 !== inputIdentity(root, pkg.name).sha256)
        throw new Error("build input closure changed");
      const environment = buildEnvironment(root, pkg.name, env);
      if (
        JSON.stringify(stamp.environment?.variables) !==
        JSON.stringify(environment.variables)
      )
        throw new Error("build environment changed");
      if (
        stamp.outputs.sha256 !==
          outputIdentity(root, `${pkg.directory}/dist`).sha256 ||
        Object.keys(stamp.outputs.files).length === 0
      )
        throw new Error("compiled artifact contents changed or are empty");
      const dependencies = compiledDependencies(root, pkg.name);
      if (
        JSON.stringify(
          dependencies.map(({ name, outputs }) => [name, outputs.sha256]),
        ) !==
        JSON.stringify(
          (stamp.dependencies ?? []).map(({ name, outputs }) => [
            name,
            outputs.sha256,
          ]),
        )
      )
        throw new Error("compiled dependency contents changed");
      // Inlined non-runtime dists are bound by bytes above. Recursing into
      // them could cycle through a devDependency back to this package.
      for (const dependency of runtimeBuildClosure(root, pkg.name).filter(
        (entry) => entry.name !== pkg.name,
      )) {
        const verdict = checkBuild(root, dependency.name, { env });
        if (verdict.status !== "fresh")
          throw new Error(`dependency ${dependency.name} is ${verdict.status}`);
      }
      return { status: "fresh", stamp };
    } catch (error) {
      return {
        status: "stale",
        reason: `${pkg.name}: ${error.message}; run contrib build --package ${pkg.name}`,
      };
    }
  });

// A fresh dist is a verified no-op: same checkout, input closure, named build
// environment, compiled dependency bytes and emitted bytes as its stamp. The
// receipt says so and points at the receipt of the build that made it.
const freshReceipt = (root, pkg, stamp) => {
  const receipt = {
    schema: "midgard-contrib-receipt/v1",
    kind: "build",
    package: pkg.name,
    root: realpathSync(root),
    status: "fresh",
    fresh: "skipped",
    reason:
      "dist matches its digest stamp; --force or MIDGARD_CONTRIB_FORCE_BUILD=1 rebuilds it",
    inputs: stamp.inputs,
    builtBy: stamp.receipt,
    steps: [],
    artifacts: [
      { name: pkg.name, outputs: stamp.outputs },
      ...(stamp.dependencies ?? []),
    ],
    createdAt: new Date().toISOString(),
    path: resolve(runDirectory(), "receipt.json"),
  };
  atomicJson(receipt.path, receipt);
  return { ...receipt, exitCode: 0 };
};

// The trace is complete once the recipe exits; nothing builds while the
// workspace resource is held, so one identity scope covers the check.
const readsOutsideClosure = (root, pkg, trace, dependencies) =>
  existsSync(trace)
    ? withIdentityScope(() =>
        unboundReads(
          root,
          pkg.name,
          readFileSync(trace, "utf8")
            .split("\n")
            .filter(Boolean)
            .map((line) => JSON.parse(line)),
          { dependencies },
        ),
      )
    : ["the build left no read trace"];

export const buildPackage = async (
  root,
  name,
  { signal, env = process.env, force = false } = {},
) => {
  const pkg = packageByName(root, name);
  if (!pkg.scripts?.["build:contrib-raw"])
    throw new Error(`${pkg.name} has no guarded build recipe`);
  const forced = force || env.MIDGARD_CONTRIB_FORCE_BUILD === "1";
  return withResource(
    `workspace:${realpathSync(root)}`,
    async (ownedEnv) => {
      for (const dependency of runtimeBuildClosure(root, pkg.name).filter(
        (entry) => entry.name !== pkg.name,
      )) {
        if (checkBuild(root, dependency.name, { env }).status !== "fresh") {
          const built = await buildPackage(root, dependency.name, {
            signal,
            env: ownedEnv,
          });
          if (built.exitCode !== 0) return built;
        }
      }
      if (!forced) {
        const verdict = checkBuild(root, pkg.name, { env });
        if (verdict.status === "fresh")
          return freshReceipt(root, pkg, verdict.stamp);
      }
      return withResource(
        "memory-heavy-build",
        async (buildEnv) => {
          const directory = runDirectory();
          const before = inputIdentity(root, pkg.name);
          const dependencies = compiledDependencies(root, pkg.name);
          const environment = buildEnvironment(root, pkg.name, buildEnv);
          rmSync(resolve(root, pkg.directory, "dist/.contrib-build-v1.json"), {
            force: true,
          });
          // Every Node process of the recipe records what it reads.
          const trace = resolve(directory, "reads.jsonl");
          const step = await runProcess({
            ...pinnedPnpm(resolve(root, pkg.directory), [
              "run",
              "build:contrib-raw",
            ]),
            env: {
              ...buildEnv,
              MIDGARD_CONTRIB_BUILD_TRACE: trace,
              NODE_OPTIONS:
                `${buildEnv.NODE_OPTIONS ?? ""} --require ${JSON.stringify(TRACER)}`.trim(),
            },
            signal,
            logPath: resolve(directory, "build.log"),
            echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
          });
          const after = inputIdentity(root, pkg.name);
          const receipt = writeReceipt({
            root,
            pkg,
            directory,
            kind: "build",
            before,
            after,
            steps: [step],
          });
          const outputs = outputIdentity(root, `${pkg.directory}/dist`);
          receipt.artifacts = [{ name: pkg.name, outputs }, ...dependencies];
          const unstamped =
            receipt.exitCode === 0
              ? readsOutsideClosure(root, pkg, trace, dependencies)
              : [];
          if (receipt.exitCode === 0) {
            if (
              !Object.keys(outputs.files).length ||
              JSON.stringify(dependencies) !==
                JSON.stringify(compiledDependencies(root, pkg.name))
            ) {
              receipt.status = "failed";
              receipt.exitCode = 1;
              receipt.reason =
                "build produced no outputs or compiled dependencies changed during execution";
            } else if (unstamped.length) {
              // The dist is usable, but it read a file its stamp would not
              // bind, so it stays unstamped and the next check rebuilds it.
              receipt.unstamped = unstamped;
              receipt.reason = `dist left unstamped: ${unstamped.slice(0, 5).join("; ")}`;
            } else
              atomicJson(
                resolve(root, pkg.directory, "dist/.contrib-build-v1.json"),
                {
                  schema: "midgard-contrib-build/v1",
                  root: realpathSync(root),
                  package: pkg.name,
                  reads: BUILD_TRACE,
                  inputs: before,
                  environment,
                  outputs,
                  dependencies,
                  receipt: receipt.path,
                },
              );
          }
          atomicJson(receipt.path, receipt);
          return receipt;
        },
        { signal, env: ownedEnv },
      );
    },
    { signal, env },
  );
};
