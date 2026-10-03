import { existsSync, realpathSync, rmSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { resolve } from "node:path";

import {
  atomicJson,
  hashFiles,
  inputIdentity,
  json,
  packageByName,
} from "./files.mjs";
import { runDirectory } from "./build.mjs";
import { runProcess } from "./process.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { writeReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";

export const NATIVE_RECIPES = {
  "midgard-node": {
    recipe: "native:mpf-owner:build",
    output: "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
    tools: [
      ["cargo", "--version"],
      ["rustc", "--version"],
    ],
  },
  "midgard-watcher": {
    recipe: "native:build",
    output: "dist/native/midgard-chain-sync",
    tools: [["go", "version"]],
  },
};

const versions = (commands) =>
  commands.map((argv) => ({
    argv,
    version: execFileSync(argv[0], argv.slice(1), {
      encoding: "utf8",
      timeout: 5000,
    }).trim(),
  }));
export const checkNative = (root, name) => {
  const pkg = packageByName(root, name);
  const recipe = NATIVE_RECIPES[pkg.name];
  if (!recipe) throw new Error(`no native recipe for ${name}`);
  const output = resolve(root, pkg.directory, recipe.output);
  const stampPath = `${output}.contrib-native-v1.json`;
  if (!existsSync(output) || !existsSync(stampPath))
    return {
      status: "missing",
      reason: `run contrib native --package ${name}`,
    };
  try {
    const stamp = json(stampPath);
    if (
      stamp.schema !== "midgard-contrib-native/v1" ||
      stamp.root !== realpathSync(root) ||
      stamp.inputs.sha256 !== inputIdentity(root, name).sha256 ||
      stamp.outputs.sha256 !== hashFiles(root, [output]).sha256 ||
      JSON.stringify(stamp.tools) !== JSON.stringify(versions(recipe.tools))
    )
      throw new Error(
        "native binary sources, contents, checkout or compiler changed",
      );
    return { status: "fresh", stamp };
  } catch (error) {
    return {
      status: "stale",
      reason: `${error.message}; run contrib native --package ${name}`,
    };
  }
};

export const buildNative = async (
  root,
  name,
  { signal, env = process.env } = {},
) => {
  const pkg = packageByName(root, name);
  const recipe = NATIVE_RECIPES[pkg.name];
  if (!recipe || !pkg.scripts?.[`${recipe.recipe}:contrib-raw`])
    throw new Error(`${name} has no guarded native recipe`);
  return withResource(
    `workspace:${realpathSync(root)}`,
    (ownedEnv) =>
      withResource(
        "memory-heavy-build",
        async (buildEnv) => {
          const directory = runDirectory();
          const before = inputIdentity(root, pkg.name);
          const tools = versions(recipe.tools);
          rmSync(
            `${resolve(root, pkg.directory, recipe.output)}.contrib-native-v1.json`,
            { force: true },
          );
          const step = await runProcess({
            ...pinnedPnpm(resolve(root, pkg.directory), [
              "run",
              `${recipe.recipe}:contrib-raw`,
            ]),
            env: buildEnv,
            signal,
            logPath: resolve(directory, "native.log"),
            echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
          });
          const receipt = writeReceipt({
            root,
            pkg,
            directory,
            kind: "native-build",
            before,
            after: inputIdentity(root, pkg.name),
            steps: [step],
          });
          const output = resolve(root, pkg.directory, recipe.output);
          if (receipt.exitCode === 0 && !existsSync(output)) {
            receipt.exitCode = 1;
            receipt.status = "failed";
            receipt.reason = "native build produced no declared binary";
          }
          if (receipt.exitCode === 0) {
            receipt.outputs = hashFiles(root, [output]);
            receipt.tools = tools;
            atomicJson(`${output}.contrib-native-v1.json`, {
              schema: "midgard-contrib-native/v1",
              root: realpathSync(root),
              inputs: before,
              outputs: receipt.outputs,
              tools,
              receipt: receipt.path,
            });
          }
          atomicJson(receipt.path, receipt);
          return receipt;
        },
        { signal, env: ownedEnv },
      ),
    { signal, env },
  );
};
