#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  realpathSync,
  writeFileSync,
} from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  assertPinnedAiken,
  pinnedAikenFork,
} from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import { runProcess } from "../contrib/process.mjs";
import { withResource } from "../contrib/resources.mjs";

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const git = (root, args) => {
  const result = spawnSync("git", ["-C", root, ...args], {
    encoding: "utf8",
    maxBuffer: 4 * 1024 * 1024,
  });
  if (result.error || result.status !== 0)
    throw new Error(
      `git ${args[0]} failed: ${result.stderr ?? result.error?.message}`,
    );
  return result.stdout.trim();
};

/** Both real compiler executions must agree with the separately retained release. */
export const compareBlueprintRebuilds = (builds, releaseBytes) => {
  if (
    builds.length !== 2 ||
    builds.some((build) => build.compilerExitCode !== 0)
  )
    throw new Error(
      "Two successful independent compiler executions are required",
    );
  if (builds[0].root === builds[1].root)
    throw new Error("Independent rebuild roots must differ");
  const releaseHash = sha256(releaseBytes);
  const expected = builds[0];
  for (const build of builds) {
    const blueprint = JSON.parse(build.bytes.toString("utf8"));
    if (
      !Array.isArray(blueprint.validators) ||
      blueprint.validators.length === 0
    )
      throw new Error("Rebuild contains no validators");
    const titles = blueprint.validators.map((validator) => validator.title);
    if (
      new Set(titles).size !== titles.length ||
      titles.some((title) => typeof title !== "string")
    )
      throw new Error("Rebuild validator titles are malformed or duplicated");
    if (
      sha256(build.bytes) !== releaseHash ||
      build.stamp.blueprintHash !== releaseHash ||
      build.stamp.sourceHash !== expected.stamp.sourceHash ||
      build.stamp.profileDigest !== expected.stamp.profileDigest ||
      build.stamp.compiler !== expected.stamp.compiler
    )
      throw new Error(
        "Independent rebuild/source/profile/compiler differs from release blueprint",
      );
  }
  return {
    blueprintHash: releaseHash,
    sourceHash: expected.stamp.sourceHash,
    profileDigest: expected.stamp.profileDigest,
    compiler: expected.stamp.compiler,
    validatorCount: JSON.parse(releaseBytes.toString("utf8")).validators.length,
  };
};

export const parseBlueprintRebuildArguments = (args) => {
  const values = {};
  const allowed = new Set([
    "--root",
    "--revision",
    "--profile",
    "--compiler",
    "--release-blueprint",
    "--output",
  ]);
  for (let index = 0; index < args.length; index += 2) {
    const key = args[index];
    if (
      !allowed.has(key) ||
      Object.hasOwn(values, key) ||
      !args[index + 1] ||
      args[index + 1].startsWith("--")
    )
      throw new Error(
        "Usage: blueprint-rebuild.mjs --root DIR --revision FULL_SHA --profile preprod-public|mainnet --compiler ABSOLUTE_BINARY --release-blueprint FILE --output NEW_DIRECTORY",
      );
    values[key] = args[index + 1];
  }
  if (
    allowed.size !== Object.keys(values).length ||
    !/^[0-9a-f]{40}$/u.test(values["--revision"]) ||
    !["preprod-public", "mainnet"].includes(values["--profile"]) ||
    !values["--compiler"].startsWith("/")
  )
    throw new Error(
      "A frozen full source revision, public profile and absolute compiler are required",
    );
  return {
    root: realpathSync(values["--root"]),
    revision: values["--revision"],
    profile: values["--profile"],
    compiler: realpathSync(values["--compiler"]),
    releaseBlueprint: realpathSync(values["--release-blueprint"]),
    output: resolve(values["--output"]),
  };
};

export const rebuildReleaseBlueprint = async (options, signal) => {
  if (Number(process.versions.node.split(".")[0]) !== 22)
    throw new Error(
      "Release rebuilding requires the declared Node22 toolchain",
    );
  const revision = git(options.root, [
    "rev-parse",
    `${options.revision}^{commit}`,
  ]);
  if (revision !== options.revision)
    throw new Error("Release revision must name an exact commit");
  if (existsSync(options.output))
    throw new Error(
      "Release evidence destination must be new; refusing overwrite",
    );
  mkdirSync(options.output, { mode: 0o700 });
  const releaseBytes = readFileSync(options.releaseBlueprint);
  writeFileSync(resolve(options.output, "release-plutus.json"), releaseBytes, {
    flag: "wx",
    mode: 0o600,
  });
  const compilerHash = sha256(readFileSync(options.compiler));
  const report = {
    schemaVersion: "midgard-independent-blueprint-rebuild-v1",
    sourceRevision: revision,
    profile: options.profile,
    releaseBlueprintHash: sha256(releaseBytes),
    compilerBinaryHash: compilerHash,
    status: "running",
    builds: [],
    startedAt: new Date().toISOString(),
  };
  try {
    for (const ordinal of [1, 2]) {
      const root = resolve(options.output, `build-${ordinal}`);
      mkdirSync(root, { mode: 0o700 });
      const archive = resolve(options.output, `source-${ordinal}.tar`);
      git(options.root, [
        "archive",
        "--format=tar",
        `--output=${archive}`,
        revision,
      ]);
      const extract = spawnSync("tar", ["-xf", archive, "-C", root], {
        encoding: "utf8",
      });
      if (extract.error || extract.status !== 0)
        throw new Error("Could not extract exact release source archive");
      if (
        existsSync(resolve(root, "onchain/aiken/plutus.json")) ||
        existsSync(resolve(root, "onchain/aiken/build"))
      )
        throw new Error(
          "Committed source must not contain a cached blueprint or Aiken build directory",
        );
      const pin = pinnedAikenFork(root);
      const compiler = assertPinnedAiken(options.compiler);
      if (compiler !== pin.version)
        throw new Error("Compiler differs from frozen source pin");
      const step = async (name, argv, cwd, env) => {
        const result = await runProcess({
          argv,
          cwd,
          env,
          signal,
          logPath: resolve(options.output, `build-${ordinal}-${name}.log`),
        });
        report.builds[ordinal - 1].steps.push({ name, argv, cwd, ...result });
        if (result.exitCode !== 0)
          throw new Error(`Independent build ${ordinal} ${name} failed`);
        return result;
      };
      const record = {
        ordinal,
        root,
        archiveHash: sha256(readFileSync(archive)),
        compiler,
        pin,
        steps: [],
      };
      report.builds.push(record);
      const demo = resolve(root, "demo");
      const packageManager = JSON.parse(
        readFileSync(resolve(demo, "package.json"), "utf8"),
      ).packageManager;
      if (!/^pnpm@[0-9]+\.[0-9]+\.[0-9]+(?:\+.*)?$/u.test(packageManager))
        throw new Error("Frozen source must pin pnpm");
      await withResource(
        "memory-heavy-build",
        async (env) => {
          await step(
            "install",
            [
              "corepack",
              "pnpm",
              "install",
              "--frozen-lockfile",
              "--ignore-scripts",
            ],
            demo,
            env,
          );
          // Direct producer invocation always executes aiken build; readiness/cache synchronization is not used.
          const lockBefore = sha256(
            readFileSync(resolve(root, "onchain/aiken/aiken.lock")),
          );
          const compiled = await step(
            "compiler",
            [
              process.execPath,
              "scripts/deployment-profiles.mjs",
              "build",
              options.profile,
            ],
            demo,
            { ...env, MIDGARD_AIKEN_BIN: options.compiler },
          );
          record.compilerExitCode = compiled.exitCode;
          if (
            lockBefore !==
            sha256(readFileSync(resolve(root, "onchain/aiken/aiken.lock")))
          )
            throw new Error(
              "Compiler changed the frozen Aiken dependency lock",
            );
        },
        { signal },
      );
      record.stamp = JSON.parse(
        readFileSync(
          resolve(root, "onchain/aiken/plutus.json.deployment.json"),
          "utf8",
        ),
      );
      if (record.stamp.profile.name !== options.profile)
        throw new Error("Compiler built a different deployment profile");
      record.blueprintHash = sha256(
        readFileSync(resolve(root, "onchain/aiken/plutus.json")),
      );
      record.selectedCoreProfileHash = sha256(
        readFileSync(
          resolve(
            root,
            "demo/midgard-core/src/generated-deployment-profiles.ts",
          ),
        ),
      );
      record.pnpmLockHash = sha256(
        readFileSync(resolve(demo, "pnpm-lock.yaml")),
      );
      if (sha256(readFileSync(options.compiler)) !== compilerHash)
        throw new Error("Compiler binary changed during rebuilding");
    }
    if (
      report.builds[0].archiveHash !== report.builds[1].archiveHash ||
      report.builds[0].selectedCoreProfileHash !==
        report.builds[1].selectedCoreProfileHash ||
      report.builds[0].pnpmLockHash !== report.builds[1].pnpmLockHash
    )
      throw new Error("Independent source/profile/lock bytes differ");
    report.identity = compareBlueprintRebuilds(
      report.builds.map((build) => ({
        ...build,
        bytes: readFileSync(resolve(build.root, "onchain/aiken/plutus.json")),
      })),
      releaseBytes,
    );
    report.status = "passed";
    return report;
  } catch (error) {
    report.status = "failed";
    report.error = error.message;
    throw error;
  } finally {
    report.finishedAt = new Date().toISOString();
    writeFileSync(
      resolve(options.output, "receipt.json"),
      `${JSON.stringify(report, null, 2)}\n`,
      { flag: "wx", mode: 0o600 },
    );
  }
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const abort = new AbortController();
  process.once("SIGINT", () => abort.abort());
  process.once("SIGTERM", () => abort.abort());
  try {
    const report = await rebuildReleaseBlueprint(
      parseBlueprintRebuildArguments(process.argv.slice(2)),
      abort.signal,
    );
    console.log(
      JSON.stringify({
        status: report.status,
        sourceRevision: report.sourceRevision,
        ...report.identity,
      }),
    );
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
