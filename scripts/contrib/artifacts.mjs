import { execFileSync } from "node:child_process";
import {
  cpSync,
  existsSync,
  mkdirSync,
  realpathSync,
  rmSync,
  lstatSync,
} from "node:fs";
import { relative, resolve } from "node:path";

import { matchesAny } from "../preflight/derive.mjs";
import { buildPackage, checkBuild, runDirectory } from "./build.mjs";
import { runTests } from "./tests.mjs";
import {
  atomicJson,
  filesUnder,
  hashFiles,
  inputIdentity,
  inside,
  json,
  packageByName,
  packageClosure,
  outputIdentity,
  workspacePackages,
  sourceInput,
} from "./files.mjs";
import { runProcess } from "./process.mjs";
import { writeReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";
import { applyPacket, createPacket, git } from "./workspace.mjs";

import { artifactChannels, channelIdentity } from "./channels.mjs";
export { artifactChannels, channelIdentity } from "./channels.mjs";

export const dependencyPins = (root) => {
  const pins = new Map();
  const packages = [
    ...workspacePackages(root),
    ...(existsSync(resolve(root, "demo/package.json"))
      ? [json(resolve(root, "demo/package.json"))]
      : []),
  ];
  for (const pkg of packages)
    for (const [name, value] of Object.entries({
      ...pkg.dependencies,
      ...pkg.devDependencies,
      ...pkg.pnpm?.overrides,
    })) {
      if (typeof value !== "string") continue;
      if (
        /^(?:file:|link:)\//u.test(value) ||
        /(?:\/tmp\/|\/home\/)/u.test(value)
      )
        throw new Error(
          `nonportable dependency ${pkg.name}:${name}=${value}; vendor/publish it and update the lockfile`,
        );
      if (value.startsWith("github:")) {
        const match = value.match(
          /^github:([^#]+)#([a-f0-9]{40})(?:&path:.*)?$/u,
        );
        if (!match)
          throw new Error(
            `Git dependency must name an immutable commit: ${name}=${value}`,
          );
        pins.set(value, {
          name,
          url: `https://github.com/${match[1]}.git`,
          commit: match[2],
        });
      } else if (/^(?:git[+:]|https:\/\/.*\.git)/u.test(value))
        throw new Error(
          `use an auditable github:owner/repository#40-character-commit pin: ${name}=${value}`,
        );
    }
  return [...pins.values()];
};

// A detached worktree makes the existing path-relative generators usable in a
// disposable checkout. Overlay source edits, never dependency stores, secrets
// or outputs; frozen install creates proper workspace links in that checkout.
export const withScratchCheckout = async (root, directory, action) => {
  const scratch = resolve(directory, "checkout");
  execFileSync("git", ["worktree", "add", "--detach", scratch, "HEAD"], {
    cwd: root,
    stdio: "pipe",
  });
  try {
    const headEntries = execFileSync("git", ["ls-tree", "-r", "-z", "HEAD"], {
      cwd: root,
      encoding: "utf8",
    })
      .split("\0")
      .filter(Boolean);
    const gitlinks = new Set(
      [
        ...headEntries,
        ...execFileSync("git", ["ls-files", "--stage", "-z"], {
          cwd: root,
          encoding: "utf8",
        }).split("\0"),
      ]
        .filter((entry) => entry.startsWith("160000 "))
        .map((entry) => entry.slice(entry.indexOf("\t") + 1)),
    );
    // Index enumeration omits staged deletions and the old side of renames.
    // Include HEAD paths so missing candidate files remove those scratch copies.
    const paths = new Set([
      ...headEntries.map((entry) => entry.slice(entry.indexOf("\t") + 1)),
      ...execFileSync(
        "git",
        ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
        { cwd: root, encoding: "utf8" },
      )
        .split("\0")
        .filter(Boolean),
    ]);
    for (const path of paths) {
      // Gitlinks are repository boundaries, not source files. Midgard's Lean
      // submodule is intentionally uninitialized and no owner builds it.
      if (gitlinks.has(path)) continue;
      if (
        !sourceInput(root, path) ||
        /(?:^|\/)(?:secrets)(?:\/|$)|(?:^|\/)\.env(?:\.|$)|^onchain\/aiken\/plutus\.json(?:\.|$)/u.test(
          path,
        )
      )
        continue;
      const source = inside(root, path);
      const destination = inside(scratch, path);
      if (!existsSync(source)) {
        rmSync(destination, { force: true });
        continue;
      }
      if (lstatSync(source).isSymbolicLink()) continue; // tracked Claude router remains the checkout's symlink
      if (!lstatSync(source).isFile())
        throw new Error(`scratch input is not a file: ${path}`);
      mkdirSync(resolve(destination, ".."), { recursive: true });
      cpSync(source, destination);
    }
    return await action(scratch);
  } finally {
    execFileSync("git", ["worktree", "remove", "--force", scratch], {
      cwd: root,
      stdio: "pipe",
    });
  }
};

export const runArtifact = async (
  root,
  id,
  { sync = false, signal, env = process.env } = {},
) => {
  const channel = artifactChannels(root).channels.find(
    (entry) => entry.id === id,
  );
  if (!channel)
    throw new Error(
      `unknown artifact channel ${id}; use contrib artifacts list`,
    );
  const command = sync ? channel.sync : channel.check;
  if (!command?.run || !channel.check?.run)
    throw new Error(
      `${id} has no executable ${sync ? "sync and validation" : "check"}: ${command?.manual ?? command?.none ?? channel.check?.none}`,
    );
  const pkg =
    workspacePackages(root).find((entry) =>
      channel.generators.some((file) => file.startsWith(`${entry.directory}/`)),
    ) ?? packageByName(root, "midgard-core");
  return withResource(
    `workspace:${realpathSync(root)}`,
    async (ownedEnv) => {
      const directory = runDirectory();
      const steps = [];
      const run = async (argv, cwd, environment = ownedEnv) => {
        const step = await runProcess({
          argv,
          cwd,
          env: environment,
          signal,
          logPath: resolve(directory, `step-${steps.length}.log`),
          echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
        });
        steps.push(step);
        if (step.exitCode !== 0 || step.reason)
          throw new Error(`artifact step failed: ${step.logPath}`);
      };
      const before = inputIdentity(root, pkg.name);
      let artifacts = [];
      const retainedArtifacts = [];
      const prepareArtifacts = async (checkout) => {
        const packages = packageClosure(checkout, pkg.name).filter(
          (entry) => entry.scripts?.build,
        );
        for (const dependency of packages) {
          if (checkBuild(checkout, dependency.name).status !== "fresh") {
            const built = await buildPackage(checkout, dependency.name, {
              signal,
              env: ownedEnv,
            });
            if (built.exitCode)
              throw new Error(`artifact prerequisite failed: ${built.path}`);
          }
        }
        return packages.map((entry) => ({
          name: entry.name,
          outputs: outputIdentity(checkout, `${entry.directory}/dist`),
        }));
      };
      const unchangedArtifacts = (checkout, snapshot) =>
        snapshot.every(
          (entry) =>
            entry.outputs.sha256 ===
            outputIdentity(
              checkout,
              `${packageByName(checkout, entry.name).directory}/dist`,
            ).sha256,
        );
      const testExecutions = [];
      const sourceSnapshot = (checkout) =>
        hashFiles(checkout, filesUnder(checkout)).files;
      const initialSources = sync ? sourceSnapshot(root) : undefined;
      // Generation runs in a different checkout. Only its declared output
      // changes may explain a new destination identity; never rebind a pass
      // to a concurrent source edit that the scratch execution did not see.
      const assertSyncDestination = (checkedInputs, published = false) => {
        const current = sourceSnapshot(root);
        const changed = [
          ...new Set([...Object.keys(initialSources), ...Object.keys(current)]),
        ].filter((file) => initialSources[file] !== current[file]);
        if (
          changed.some(
            (file) => !published || !matchesAny(file, channel.outputs),
          )
        )
          throw new Error("destination inputs changed during artifact sync");
        if (
          checkedInputs &&
          channelIdentity(root, channel).sha256 !== checkedInputs.sha256
        )
          throw new Error(
            "published artifact channel inputs differ from checked scratch inputs",
          );
      };
      let checkedInputs;
      let checkedOutputs;
      const runCheck = async (checkout) => {
        const direct = channel.check.run.match(
          /^pnpm --dir (demo\/[\w-]+) exec vitest run ((?:tests\/[\w./-]+\.test\.ts ?)+)$/u,
        );
        if (!direct)
          return run(
            ["bash", "-euo", "pipefail", "-c", channel.check.run],
            checkout,
          );
        const selected = packageByName(checkout, direct[1].slice(5));
        const receipt = await runTests(checkout, selected.name, {
          files: direct[2].trim().split(" "),
          signal,
          env: ownedEnv,
        });
        steps.push(...receipt.steps);
        testExecutions.push({
          report: receipt.report,
          counts: receipt.counts,
          selectedFiles: receipt.selectedFiles,
        });
        if (receipt.exitCode)
          throw new Error(`artifact validation tests failed: ${receipt.path}`);
      };
      let error;
      try {
        if (sync) {
          await withScratchCheckout(root, directory, async (scratch) => {
            // Do not inherit the destination's workspace lease into a different
            // root; memory admission remains reentrant only for actual children.
            await run(
              ["corepack", "pnpm", "install", "--frozen-lockfile"],
              resolve(scratch, "demo"),
            );
            if (existsSync(resolve(root, "onchain/aiken/plutus.json")))
              await run(
                [process.execPath, "scripts/sync-blueprint-from.mjs", root],
                scratch,
              );
            if (inputIdentity(scratch, pkg.name).sha256 !== before.sha256)
              throw new Error(
                "scratch build inputs differ from destination inputs",
              );
            const scratchArtifacts = await prepareArtifacts(scratch);
            const retainedRoot = resolve(directory, "compiled-inputs");
            for (const entry of scratchArtifacts) {
              const pkg = packageByName(scratch, entry.name);
              cpSync(
                resolve(scratch, pkg.directory, "dist"),
                resolve(retainedRoot, pkg.directory, "dist"),
                { recursive: true },
              );
              retainedArtifacts.push({
                ...entry,
                root: retainedRoot,
                directory: `${pkg.directory}/dist`,
                inputs: inputIdentity(scratch, entry.name),
              });
            }
            const original = sourceSnapshot(scratch);
            await run(["bash", "-euo", "pipefail", "-c", command.run], scratch);
            await runCheck(scratch);
            if (!unchangedArtifacts(scratch, scratchArtifacts))
              throw new Error(
                "compiled artifacts changed during artifact generation/check",
              );
            const final = sourceSnapshot(scratch);
            const changed = [
              ...new Set([...Object.keys(original), ...Object.keys(final)]),
            ].filter((file) => original[file] !== final[file]);
            const unexpected = changed.filter(
              (file) => !matchesAny(file, channel.outputs),
            );
            if (unexpected.length)
              throw new Error(
                `generator changed paths outside its declared outputs: ${unexpected.join(", ")}`,
              );
            checkedInputs = channelIdentity(scratch, channel);
            checkedOutputs = hashFiles(
              scratch,
              filesUnder(scratch).filter((path) =>
                matchesAny(relative(scratch, path), channel.outputs),
              ),
            );
            assertSyncDestination();
            const outputs = changed;
            if (outputs.length) {
              const packet = resolve(directory, "outputs.packet.json");
              createPacket(scratch, {
                base: "HEAD",
                files: outputs,
                output: packet,
              });
              await applyPacket(root, packet, { signal, env: ownedEnv });
            }
            assertSyncDestination(checkedInputs, true);
          });
          assertSyncDestination(checkedInputs, true);
        } else {
          artifacts = await prepareArtifacts(root);
          const original = sourceSnapshot(root);
          await runCheck(root);
          if (!unchangedArtifacts(root, artifacts))
            throw new Error("compiled artifacts changed during artifact check");
          if (JSON.stringify(original) !== JSON.stringify(sourceSnapshot(root)))
            throw new Error(
              "artifact check changed source/output files; use sync for generation",
            );
        }
      } catch (caught) {
        error = caught.message;
      }
      const after = inputIdentity(root, pkg.name);
      if (sync && !error) {
        const changed = [
          ...new Set([
            ...Object.keys(before.files),
            ...Object.keys(after.files),
          ]),
        ].filter((file) => before.files[file] !== after.files[file]);
        if (
          changed.some(
            (file) =>
              !matchesAny(file, channel.outputs) ||
              after.files[file] !== checkedOutputs.files[file],
          ) ||
          JSON.stringify(before.missing) !== JSON.stringify(after.missing)
        )
          error =
            "destination receipt inputs differ from executed inputs and verified outputs";
      }
      const receipt = writeReceipt({
        root,
        pkg,
        directory,
        kind: "artifact",
        before: sync && !error ? after : before,
        after,
        steps,
      });
      receipt.channel = {
        id,
        inputs: checkedInputs ?? channelIdentity(root, channel),
        outputs: channel.outputs,
        sync,
        initialInputSha256: before.sha256,
      };
      receipt.testExecutions = testExecutions;
      receipt.artifacts = artifacts;
      receipt.retainedArtifacts = retainedArtifacts;
      receipt.generatedOutputs =
        checkedOutputs ??
        hashFiles(
          root,
          filesUnder(root).filter((path) =>
            matchesAny(relative(root, path), channel.outputs),
          ),
        );
      if (error) {
        receipt.status = "failed";
        receipt.exitCode = 1;
        receipt.reason = error;
      }
      atomicJson(receipt.path, receipt);
      return receipt;
    },
    { signal, env },
  );
};
