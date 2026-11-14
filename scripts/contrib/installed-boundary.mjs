import {
  lstatSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  readlinkSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { isAbsolute, relative, resolve } from "node:path";
import { pathToFileURL } from "node:url";
import { atomicJson, sha256 } from "./files.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { runProcess } from "./process.mjs";
import { preparePeerConsumer } from "./boundary-peers.mjs";

// Classify the standard Node conditions; Node itself resolves each package
// specifier. Inactive source/type/browser conditions are recorded, not loaded.
const runtimeTarget = (value, mode) => {
  if (typeof value === "string" || value === null) return value;
  if (Array.isArray(value)) {
    for (const alternative of value) {
      const target = runtimeTarget(alternative, mode);
      if (target !== undefined && target !== null) return target;
    }
    return null;
  }
  // Invalid primitive targets still require a Node probe. In particular, an
  // invalid array cannot be mistaken for a source-only export beside a CLI.
  if (typeof value !== "object") return value;
  if (value && typeof value === "object")
    for (const [condition, branch] of Object.entries(value))
      if (
        ["node", "node-addons", "module-sync", "default", mode].includes(
          condition,
        )
      ) {
        const target = runtimeTarget(branch, mode);
        if (target !== undefined) return target;
      }
  return undefined;
};

export const runtimeSurfaces = (pkg) => {
  const bins =
    typeof pkg.bin === "string" ? [pkg.bin] : Object.values(pkg.bin ?? {});
  const entry = pkg.exports;
  const exports =
    entry === undefined
      ? pkg.main &&
        !bins.some((bin) => resolve("/", bin) === resolve("/", pkg.main))
        ? [[".", pkg.main]]
        : []
      : entry &&
          typeof entry === "object" &&
          !Array.isArray(entry) &&
          Object.keys(entry).some((key) => key.startsWith("."))
        ? Object.entries(entry)
        : [[".", entry]];
  const modes = { import: [], require: [] };
  const inactive = [];
  for (const [subpath, value] of exports) {
    let active = false;
    for (const mode of Object.keys(modes)) {
      const target = runtimeTarget(value, mode);
      if (target === undefined || target === null) continue;
      if (subpath.includes("*"))
        throw new Error(
          `runtime wildcard export ${pkg.name}/${subpath} needs concrete boundary probes; refusing incomplete coverage`,
        );
      if (subpath !== "." && !subpath.startsWith("./"))
        throw new Error(`invalid package export subpath: ${subpath}`);
      modes[mode].push(pkg.name + (subpath === "." ? "" : subpath.slice(1)));
      active = true;
    }
    if (!active)
      inactive.push({
        subpath,
        reason: "no target under ordinary Node import/require conditions",
      });
  }
  if (!modes.import.length && !modes.require.length && !bins.length)
    throw new Error(`no declared Node runtime exports or CLI for ${pkg.name}`);
  return { ...modes, bins, inactive };
};

const inside = (root, path) => {
  const local = relative(root, path);
  if (local === ".." || local.startsWith("../") || isAbsolute(local))
    throw new Error(`installed package escapes consumer: ${path}`);
  return path;
};

// Bind bytes and symlink destinations without following pnpm's links twice.
// pnpm deploy uses copy so package initialization cannot alter the shared store.
const inventory = (root) => {
  const files = {};
  const visit = (directory) => {
    for (const entry of readdirSync(directory, { withFileTypes: true }).sort(
      (a, b) => a.name.localeCompare(b.name),
    )) {
      const path = resolve(directory, entry.name);
      const key = relative(root, path);
      const stat = lstatSync(path);
      if (stat.isSymbolicLink()) {
        try {
          inside(root, realpathSync(path));
        } catch (error) {
          throw new Error(`${error.message} (link ${key})`);
        }
        files[key] = { link: readlinkSync(path) };
      } else if (stat.isDirectory()) visit(path);
      else if (stat.isFile()) files[key] = sha256(readFileSync(path));
      else throw new Error(`unsupported installed file: ${path}`);
    }
  };
  visit(root);
  return { files, sha256: sha256(JSON.stringify(files)) };
};

export const probeInstalledPackage = async ({
  root,
  pkg,
  directory,
  env,
  signal,
}) => {
  const surfaces = runtimeSurfaces(pkg);
  const consumer = resolve(directory, "consumer");
  const installed = inside(
    consumer,
    resolve(consumer, "node_modules", pkg.name),
  );
  const probe = resolve(consumer, "probe.mjs");
  mkdirSync(consumer, { recursive: true });
  writeFileSync(
    probe,
    readFileSync(new URL("./boundary-probe.mjs", import.meta.url)),
  );
  const steps = [];
  const evidenceFiles = [];
  let reason;
  try {
    const peers = preparePeerConsumer(root, pkg, consumer);
    if (peers.external.length) {
      steps.push(
        await runProcess({
          ...pinnedPnpm(consumer, [
            "install",
            "--prod",
            "--offline",
            "--frozen-lockfile",
            "--ignore-scripts",
            "--config.package-import-method=copy",
            "--config.node-linker=isolated",
          ]),
          env,
          signal,
          logPath: resolve(directory, "peers.log"),
        }),
      );
      if (steps.at(-1).exitCode !== 0 || steps.at(-1).reason)
        return { steps, surfaces, evidenceFiles };
    }
    for (const [index, name] of [pkg.name, ...peers.local].entries()) {
      const target = inside(consumer, resolve(consumer, "node_modules", name));
      steps.push(
        await runProcess({
          ...pinnedPnpm(resolve(root, "demo"), [
            "--filter",
            name,
            "--config.package-import-method=copy",
            "--config.node-linker=isolated",
            "--config.enable-pre-post-scripts=false",
            "--config.hoist=true",
            "--config.hoist-workspace-packages=false",
            "deploy",
            "--prod",
            "--ignore-scripts",
            target,
          ]),
          env,
          signal,
          logPath: resolve(directory, `install-${index}.log`),
          echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
        }),
      );
      if (steps.at(-1).exitCode !== 0 || steps.at(-1).reason)
        return { steps, surfaces, evidenceFiles };
    }
    const before = inventory(consumer);
    const probeEnv = {
      ...env,
      NODE_OPTIONS: "",
      NODE_PATH: "",
      MIDGARD_BOUNDARY_CONSUMER: consumer,
      MIDGARD_BOUNDARY_MODE: "",
      MIDGARD_BOUNDARY_EXPORTS: "",
      MIDGARD_BOUNDARY_COMPLETION: "",
    };
    for (const mode of ["import", "require"])
      if (surfaces[mode].length) {
        const completion = resolve(directory, `${mode}-complete.json`);
        const result = await runProcess({
          argv: [process.execPath, probe],
          cwd: consumer,
          env: {
            ...probeEnv,
            MIDGARD_BOUNDARY_MODE: mode,
            MIDGARD_BOUNDARY_EXPORTS: JSON.stringify(surfaces[mode]),
            MIDGARD_BOUNDARY_COMPLETION: completion,
          },
          signal,
          timeoutMs: 60_000,
          logPath: resolve(directory, `${mode}.log`),
          echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
        });
        try {
          const bytes = readFileSync(completion);
          const proof = JSON.parse(bytes);
          if (
            proof.mode !== mode ||
            !Array.isArray(proof.loaded) ||
            JSON.stringify(proof.loaded.map((entry) => entry.specifier)) !==
              JSON.stringify(surfaces[mode]) ||
            proof.loaded.some((entry) => typeof entry.resolved !== "string")
          )
            throw new Error("incomplete export list");
          evidenceFiles.push({ path: completion, sha256: sha256(bytes) });
        } catch {
          result.reason ??= `${mode} probe did not complete every requested export`;
        }
        steps.push(result);
      }
    for (const [index, bin] of surfaces.bins.entries())
      steps.push(
        await runProcess({
          argv: [
            process.execPath,
            "--import",
            pathToFileURL(probe).href,
            inside(installed, resolve(installed, bin)),
            "--help",
          ],
          cwd: consumer,
          env: probeEnv,
          signal,
          timeoutMs: 60_000,
          logPath: resolve(directory, `cli-${index}.log`),
          echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
        }),
      );
    const after = inventory(consumer);
    if (before.sha256 !== after.sha256)
      reason = "installed package artifacts changed during boundary execution";
    const path = resolve(directory, "installation.json");
    atomicJson(path, {
      package: pkg.name,
      surfaces,
      before,
      finalSha256: after.sha256,
    });
    evidenceFiles.push({ path, sha256: sha256(readFileSync(path)) });
  } finally {
    // Keep the installation inventory and logs as receipt evidence, not copies
    // of the entire production dependency closure for every contributor run.
    rmSync(consumer, { recursive: true, force: true });
  }
  return { steps, surfaces, evidenceFiles, reason };
};
