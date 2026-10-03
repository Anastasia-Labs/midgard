import { resolve } from "node:path";

import {
  artifactChannels,
  dependencyPins,
  runArtifact,
  withScratchCheckout,
} from "./artifacts.mjs";
import { runDirectory } from "./build.mjs";
import { atomicJson, inputIdentity } from "./files.mjs";
import { buildNative, NATIVE_RECIPES } from "./native.mjs";
import { runProcess } from "./process.mjs";
import { writeReceipt } from "./receipts.mjs";
import { git } from "./workspace.mjs";

export const reproduce = async (
  root,
  { execute = false, signal, env = process.env } = {},
) => {
  const channels = artifactChannels(root).channels;
  const plan = {
    pins: dependencyPins(root),
    commands: [
      ["corepack", "pnpm", "install", "--frozen-lockfile"],
      ["corepack", "pnpm", "deployment:build", "preprod-testing"],
      ["corepack", "pnpm", "run", "build"],
    ],
    nativePackages: Object.keys(NATIVE_RECIPES),
    artifactChecks: channels
      .filter((channel) => channel.check?.run)
      .map((channel) => channel.id),
    manualChannels: channels
      .filter((channel) => !channel.check?.run)
      .map((channel) => ({
        id: channel.id,
        reason: channel.check?.manual ?? channel.check?.none,
      })),
  };
  if (!execute) return plan;
  const directory = runDirectory();
  const pkg = { name: "@repository" };
  const before = inputIdentity(root, pkg.name);
  const steps = [];
  const childReceipts = [];
  let error;
  try {
    await withScratchCheckout(root, directory, async (scratch) => {
      if (inputIdentity(scratch, pkg.name).sha256 !== before.sha256)
        throw new Error("scratch repository inputs differ from candidate");
      const run = async (argv, cwd = scratch) => {
        const step = await runProcess({
          argv,
          cwd,
          env,
          signal,
          logPath: resolve(directory, `step-${steps.length}.log`),
          echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
        });
        steps.push(step);
        if (step.exitCode !== 0 || step.reason)
          throw new Error(`clean reproducibility failed: ${step.logPath}`);
      };
      for (let index = 0; index < plan.pins.length; index += 1) {
        const pin = plan.pins[index];
        const bare = resolve(directory, `pin-${index}.git`);
        await run(["git", "init", "--bare", bare]);
        await run([
          "git",
          "-C",
          bare,
          "fetch",
          "--depth=1",
          pin.url,
          pin.commit,
        ]);
        if (git(bare, "rev-parse", "FETCH_HEAD") !== pin.commit)
          throw new Error("resolved dependency commit differs from pin");
      }
      for (const command of plan.commands)
        await run(command, resolve(scratch, "demo"));
      for (const name of plan.nativePackages) {
        const receipt = await buildNative(scratch, name, { signal, env });
        childReceipts.push(receipt.path);
        steps.push(...receipt.steps);
        if (receipt.exitCode)
          throw new Error(`clean native build failed: ${receipt.path}`);
      }
      for (const id of plan.artifactChecks) {
        const receipt = await runArtifact(scratch, id, { signal, env });
        childReceipts.push(receipt.path);
        steps.push(...receipt.steps);
        if (receipt.exitCode)
          throw new Error(`clean artifact validation failed: ${receipt.path}`);
      }
    });
  } catch (caught) {
    error = caught.message;
  }
  const receipt = writeReceipt({
    root,
    pkg,
    directory,
    kind: "reproduce",
    before,
    after: inputIdentity(root, pkg.name),
    steps,
  });
  receipt.plan = plan;
  receipt.childReceipts = childReceipts;
  if (error) {
    receipt.status = "failed";
    receipt.exitCode = 1;
    receipt.reason = error;
  }
  atomicJson(receipt.path, receipt);
  return receipt;
};
