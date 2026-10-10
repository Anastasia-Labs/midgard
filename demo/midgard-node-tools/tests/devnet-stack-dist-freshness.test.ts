import {
  mkdirSync,
  mkdtempSync,
  rmSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  type DistTarget,
  runtimeDistTargets,
  staleDists,
} from "../src/devnet-stack/dist-freshness.js";
import { makeLayout } from "../src/devnet-stack/layout.js";

const dirs: string[] = [];
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const T0 = new Date("2026-09-30T10:00:00Z");
const at = (minutes: number) => new Date(T0.getTime() + minutes * 60_000);

/** A package tree with each file's mtime set to `minutes` past T0. */
const tree = (files: Record<string, number>) => {
  const root = mkdtempSync(join(tmpdir(), "devnet-stack-dist-"));
  dirs.push(root);
  for (const [path, minutes] of Object.entries(files)) {
    const file = join(root, path);
    mkdirSync(dirname(file), { recursive: true });
    writeFileSync(file, path);
    utimesSync(file, at(minutes), at(minutes));
  }
  return root;
};

const target = (root: string, sources = ["src"]): DistTarget => ({
  packageName: "pkg",
  dist: join(root, "dist/index.js"),
  sources: sources.map((source) => join(root, source)),
});

describe("staleDists", () => {
  it("passes a dist built after every source", () => {
    const root = tree({
      "src/a.ts": 0,
      "src/deep/b.ts": 5,
      "dist/index.js": 10,
    });
    expect(staleDists([target(root)])).toEqual([]);
  });

  it("names the package and its newest source newer than the dist", () => {
    const root = tree({
      "src/a.ts": 12,
      "src/deep/b.ts": 15,
      "src/c.ts": 0,
      "dist/index.js": 10,
    });
    expect(staleDists([target(root)])).toEqual([
      {
        packageName: "pkg",
        dist: join(root, "dist/index.js"),
        newestSource: join(root, "src/deep/b.ts"),
      },
    ]);
  });

  it("checks every source directory, as the controller bundle inlines another package", () => {
    const root = tree({
      "src/a.ts": 0,
      "other/src/b.ts": 20,
      "dist/index.js": 10,
    });
    expect(staleDists([target(root, ["src", "other/src"])])).toEqual([
      expect.objectContaining({ newestSource: join(root, "other/src/b.ts") }),
    ]);
  });

  it("ignores hidden files such as editor swap files", () => {
    const root = tree({
      "src/a.ts": 0,
      "src/.a.ts.swp": 20,
      "dist/index.js": 10,
    });
    expect(staleDists([target(root)])).toEqual([]);
  });

  it("reports a missing dist", () => {
    const root = tree({ "src/a.ts": 0 });
    expect(staleDists([target(root)])).toEqual([
      { packageName: "pkg", dist: join(root, "dist/index.js") },
    ]);
  });
});

describe("runtimeDistTargets", () => {
  it("covers every dist the stack executes and the node source the controller bundles", () => {
    const layout = makeLayout(join(tmpdir(), "devnet-stack-dist-run"));
    const targets = runtimeDistTargets(layout);
    expect(targets.map((t) => t.packageName)).toEqual([
      "midgard-node-tools",
      "midgard-node",
      "da-committee-node",
      "midgard-watcher",
      "midgard-fault-proofs",
      "midgard-sdk",
      "midgard-core",
      "l1-node-transport",
      "midgard-l1-follower",
      "midgard-validation",
      "lucid-midgard",
    ]);
    expect(targets[0]!.sources).toEqual([
      join(layout.toolsRoot, "src"),
      join(layout.nodeRoot, "src"),
    ]);
    expect(targets[0]!.dist).toBe(
      join(layout.toolsRoot, "dist/devnet-stack.js"),
    );
  });
});
