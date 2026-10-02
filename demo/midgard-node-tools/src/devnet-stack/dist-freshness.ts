import { createHash } from "node:crypto";
import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { dirname, join, relative } from "node:path";

import type { Layout } from "./layout.js";

/** One built output and every source directory it is built from. */
export type DistTarget = {
  readonly packageName: string;
  /** The output file a rebuild always rewrites. */
  readonly dist: string;
  readonly sources: readonly string[];
};

export type StaleDist = {
  readonly packageName: string;
  readonly dist: string;
  /** Absent when the dist does not exist at all. */
  readonly newestSource?: string;
};

/** The newest non-hidden file at or under `source`, by modification time. */
const newestFile = (
  source: string,
): { path: string; mtimeMs: number } | undefined => {
  if (existsSync(source) && statSync(source).isFile())
    return { path: source, mtimeMs: statSync(source).mtimeMs };
  let newest: { path: string; mtimeMs: number } | undefined;
  const walk = (current: string) => {
    for (const entry of readdirSync(current, { withFileTypes: true })) {
      if (entry.name.startsWith(".")) continue;
      const path = join(current, entry.name);
      if (entry.isDirectory()) walk(path);
      else if (entry.isFile()) {
        const { mtimeMs } = statSync(path);
        if (newest === undefined || mtimeMs > newest.mtimeMs)
          newest = { path, mtimeMs };
      }
    }
  };
  if (existsSync(source)) walk(source);
  return newest;
};

/**
 * The rule: a dist is stale when any source file it is built from was
 * modified after it. A build rewrites its outputs after reading every
 * source, so an output older than a source cannot contain that source's
 * current text. An mtime walk, no hashing: it stays cheap enough to run
 * before every command.
 */
export const staleDists = (targets: readonly DistTarget[]): StaleDist[] =>
  targets.flatMap((target): StaleDist[] => {
    if (!existsSync(target.dist))
      return [{ packageName: target.packageName, dist: target.dist }];
    const built = statSync(target.dist).mtimeMs;
    const newest = target.sources
      .map(newestFile)
      .filter((file) => file !== undefined)
      .reduce<{ path: string; mtimeMs: number } | undefined>(
        (best, file) =>
          best === undefined || file.mtimeMs > best.mtimeMs ? file : best,
        undefined,
      );
    return newest !== undefined && newest.mtimeMs > built
      ? [
          {
            packageName: target.packageName,
            dist: target.dist,
            newestSource: newest.path,
          },
        ]
      : [];
  });

/**
 * Every dist the stack executes: the controller bundle (which inlines the
 * node's source, tsup.config.ts `noExternal`), the node, DA committee and
 * watcher entry points the supervisor (re)starts, and the workspace libraries
 * those entry points import at run time from their own dist.
 */
export const runtimeDistTargets = (layout: Layout): DistTarget[] => {
  const demo = join(layout.repoRoot, "demo");
  const pkg = (
    name: string,
    dist: string,
    sources: readonly string[] = [join(demo, name, "src")],
  ) => ({
    packageName: name,
    dist: join(demo, name, dist),
    sources,
  });
  return [
    pkg("midgard-node-tools", "dist/devnet-stack.js", [
      join(layout.toolsRoot, "src"),
      join(layout.nodeRoot, "src"),
    ]),
    pkg("midgard-node", "dist/index.js"),
    pkg("da-committee-node", "dist/index.js"),
    pkg("midgard-watcher", "dist/cli.js"),
    pkg("midgard-fault-proofs", "dist/index.js"),
    pkg("midgard-sdk", "dist/index.js"),
    pkg("midgard-core", "dist/index.js"),
    pkg("midgard-validation", "dist/index.js"),
    pkg("lucid-midgard", "dist/index.js"),
  ];
};

/**
 * The native binaries the build writes. Services never run these copies:
 * ensureArtifacts snapshots them into the run once. Only a build and a fresh
 * run's first snapshot read them.
 */
export const nativeBuildTargets = (layout: Layout): DistTarget[] => {
  const owner = join(layout.nodeRoot, "native/mpf-event-flat-wasm");
  return [
    {
      packageName: "midgard-node native MPF owner",
      dist: join(owner, "target/release/architecture-g-owner"),
      sources: [
        join(owner, "src"),
        join(owner, "Cargo.toml"),
        join(owner, "Cargo.lock"),
      ],
    },
    {
      packageName: "midgard-watcher native chain sync",
      dist: join(layout.watcherRoot, "dist/native/midgard-chain-sync"),
      sources: [join(layout.watcherRoot, "native-chain-sync")],
    },
  ];
};

/** One line per stale dist, with paths relative to the repository. */
export const describeStale = (repoRoot: string, entry: StaleDist): string =>
  entry.newestSource === undefined
    ? `${entry.packageName}: ${relative(repoRoot, entry.dist)} is missing`
    : `${entry.packageName}: ${relative(repoRoot, entry.dist)} is older than ${relative(repoRoot, entry.newestSource)}`;

/**
 * Never loaded by a service: source maps, type declarations, and the dist's
 * native build, which services run from the run's own snapshot.
 */
const unexecuted = (relativePath: string) =>
  relativePath === "native" || /\.(?:map|d\.[cm]?ts)$/u.test(relativePath);

/**
 * A sha256 over every file a service may load from each target's dist
 * directory, framed by path and length. A content digest, not an mtime: a
 * rebuild of an unchanged tree writes the same bytes and keeps the stamp,
 * and any change to the code a service would load changes it, however the
 * clocks or the build order fall. Hashing the ~40 MB this covers takes well
 * under a second.
 */
export const codeStamp = (targets: readonly DistTarget[]): string => {
  const hash = createHash("sha256");
  const walk = (root: string, current: string) => {
    const entries = readdirSync(current, { withFileTypes: true })
      .filter((entry) => !entry.name.startsWith("."))
      .sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
    for (const entry of entries) {
      const path = join(current, entry.name);
      const name = relative(root, path);
      if (unexecuted(name)) continue;
      if (entry.isDirectory()) walk(root, path);
      else if (entry.isFile()) {
        const contents = readFileSync(path);
        hash.update(`${name}\0${String(contents.length)}\0`);
        hash.update(contents);
      }
    }
  };
  for (const root of [
    ...new Set(targets.map((target) => dirname(target.dist))),
  ]) {
    hash.update(`${root}\0${existsSync(root) ? "present" : "missing"}\0`);
    if (existsSync(root)) walk(root, root);
  }
  return hash.digest("hex");
};

/** Refuses to run a command on a dist older than its sources. */
export const requireFreshDists = (layout: Layout, command: string): void => {
  const stale = staleDists(runtimeDistTargets(layout));
  if (stale.length === 0) return;
  const lines = stale.map((entry) => describeStale(layout.repoRoot, entry));
  throw new Error(
    `${command} refuses to run stale builds; rebuild them (pnpm --dir demo run build, or up without --no-build):\n  ${lines.join("\n  ")}`,
  );
};
