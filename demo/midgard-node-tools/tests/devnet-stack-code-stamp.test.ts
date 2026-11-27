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
  buildDatedToStart,
  type BuildInputs,
  DEPLOYMENT_PROFILE,
  pendingBuild,
} from "../src/devnet-stack/build.js";
import {
  codeStamp,
  type DistTarget,
  nativeBuildTargets,
  staleDists,
} from "../src/devnet-stack/dist-freshness.js";
import { makeLayout } from "../src/devnet-stack/layout.js";
import { specsDigest } from "../src/devnet-stack/services.js";
import { ensureSupervisor, supervisorRuns } from "../src/devnet-stack/stack.js";
import type { ServiceSpec } from "../src/devnet-stack/supervisor.js";

const dirs: string[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-code-stamp-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const T0 = new Date("2026-09-30T10:00:00Z").getTime();

/** Writes `files` under `root`; a number is the file's mtime in minutes past T0. */
const write = (
  root: string,
  files: Record<string, string | [string, number]>,
) => {
  for (const [path, value] of Object.entries(files)) {
    const [content, minutes] = typeof value === "string" ? [value, 0] : value;
    const file = join(root, path);
    mkdirSync(dirname(file), { recursive: true });
    writeFileSync(file, content);
    const at = new Date(T0 + minutes * 60_000);
    utimesSync(file, at, at);
  }
};

/** Two packages with built dists, as the stack runs them. */
const builtTree = () => {
  const root = scratch();
  write(root, {
    "a/dist/index.js": "a();",
    "a/dist/workers/w.js": "w();",
    "a/dist/index.js.map": "{}",
    "a/dist/index.d.ts": "export {};",
    "a/dist/native/midgard-l1-node-transport": "ELF",
    "b/dist/cli.js": "b();",
  });
  const targets: DistTarget[] = [
    { packageName: "a", dist: join(root, "a/dist/index.js"), sources: [] },
    { packageName: "b", dist: join(root, "b/dist/cli.js"), sources: [] },
  ];
  return { root, targets };
};

describe("codeStamp", () => {
  it("changes when any file a service loads changes, is added or vanishes", () => {
    const { root, targets } = builtTree();
    const before = codeStamp(targets);
    write(root, { "a/dist/workers/w.js": "w(2);" });
    const changed = codeStamp(targets);
    expect(changed).not.toBe(before);
    write(root, { "b/dist/extra.js": "x();" });
    expect(codeStamp(targets)).not.toBe(changed);
    rmSync(join(root, "b/dist"), { recursive: true });
    expect(codeStamp(targets)).not.toBe(changed);
  });

  it("hashes contents: a change that keeps every name and length changes it", () => {
    const { root, targets } = builtTree();
    const before = codeStamp(targets);
    write(root, { "b/dist/cli.js": "c();" });
    expect(codeStamp(targets)).not.toBe(before);
  });

  it("keeps the stamp for a rebuild that writes the same bytes, and for files no service loads", () => {
    const { root, targets } = builtTree();
    const before = codeStamp(targets);
    write(root, {
      "a/dist/index.js": ["a();", 90],
      "a/dist/index.js.map": '{"rebuilt":true}',
      "a/dist/index.d.ts": "export type X = 1;",
      "a/dist/native/midgard-l1-node-transport": "ELF2",
    });
    expect(codeStamp(targets)).toBe(before);
  });
});

const spec: ServiceSpec = {
  name: "node",
  command: "/usr/bin/node",
  args: ["dist/index.js", "listen"],
  cwd: "/repo/demo/midgard-node",
  env: { A: "1" },
};

describe("the supervisor against rebuilt dists", () => {
  it("counts a supervisor started before a rebuild as running other code, and one on unchanged dists as running this", () => {
    const { root, targets } = builtTree();
    const layout = makeLayout(scratch());
    mkdirSync(layout.state, { recursive: true });
    // What `supervise` records when it starts its services.
    writeFileSync(
      layout.supervisorSpecs,
      specsDigest([spec], codeStamp(targets)),
    );
    write(root, { "a/dist/index.js": ["a();", 30] });
    expect(supervisorRuns(layout, [spec], codeStamp(targets))).toBe(true);
    write(root, { "a/dist/index.js": "a(fixed);" });
    expect(supervisorRuns(layout, [spec], codeStamp(targets))).toBe(false);
  });

  const control = (runs: boolean, running: number | undefined) => {
    const calls: string[] = [];
    return {
      calls,
      control: {
        running: () => running,
        runs: () => runs,
        stop: async () => {
          calls.push("stop");
        },
        start: async () => {
          calls.push("start");
          return 777;
        },
      },
    };
  };

  it("restarts a supervisor running older code onto the new code", async () => {
    const { calls, control: c } = control(false, 4242);
    expect(await ensureSupervisor(c)).toBe(777);
    expect(calls).toEqual(["stop", "start"]);
  });

  it("leaves a supervisor running this code alone", async () => {
    const { calls, control: c } = control(true, 4242);
    expect(await ensureSupervisor(c)).toBe(4242);
    expect(calls).toEqual([]);
  });

  it("starts one when none runs", async () => {
    const { calls, control: c } = control(false, undefined);
    expect(await ensureSupervisor(c)).toBe(777);
    expect(calls).toEqual(["start"]);
  });
});

describe("native build targets", () => {
  it("covers both native binaries, judged against source files as well as directories", () => {
    const layout = makeLayout(join(tmpdir(), "devnet-code-stamp-run"));
    expect(nativeBuildTargets(layout).map((target) => target.dist)).toEqual([
      join(
        layout.nodeRoot,
        "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
      ),
      join(layout.transportRoot, "dist/native/midgard-l1-node-transport"),
    ]);
    const root = scratch();
    write(root, {
      "src/main.rs": ["fn main() {}", 0],
      "Cargo.lock": ["", 0],
      bin: ["ELF", 10],
    });
    const target = {
      packageName: "owner",
      dist: join(root, "bin"),
      sources: [join(root, "src"), join(root, "Cargo.lock")],
    };
    expect(staleDists([target])).toEqual([]);
    write(root, { "Cargo.lock": ["changed", 20] });
    expect(staleDists([target])).toEqual([
      {
        packageName: "owner",
        dist: join(root, "bin"),
        newestSource: join(root, "Cargo.lock"),
      },
    ]);
  });
});

describe("a native build step", () => {
  /** A binary built a minute after its sources, then a test-only edit. */
  const crate = () => {
    const root = scratch();
    const now = Date.now();
    const at = (offsetMs: number) => new Date(now + offsetMs);
    const file = (path: string, content: string, time: Date) => {
      mkdirSync(dirname(join(root, path)), { recursive: true });
      writeFileSync(join(root, path), content);
      utimesSync(join(root, path), time, time);
    };
    file("src/main.rs", "fn main() {}", at(-180_000));
    file("bin", "ELF", at(-120_000));
    // Behind #[cfg(test)]: cargo does not rebuild the binary for it.
    file("src/tests.rs", "#[test] fn t() {}", at(-60_000));
    const target: DistTarget = {
      packageName: "owner",
      dist: join(root, "bin"),
      sources: [join(root, "src")],
    };
    expect(staleDists([target])).toHaveLength(1);
    return { root, target, file, at };
  };

  it("reads as built after the sources it saw, even when it left its output untouched", async () => {
    const { target } = crate();
    await buildDatedToStart(target, async () => {});
    expect(staleDists([target])).toEqual([]);
  });

  it("still reads as stale for a source changed during or after the step", async () => {
    const during = crate();
    await buildDatedToStart(during.target, async () => {
      during.file("src/main.rs", "fn main() { 1; }", during.at(1_000));
    });
    expect(staleDists([during.target])).toHaveLength(1);
    const after = crate();
    await buildDatedToStart(after.target, async () => {});
    after.file("src/main.rs", "fn main() { 2; }", after.at(5_000));
    expect(staleDists([after.target])).toHaveLength(1);
  });

  it("leaves a failed step's output stale", async () => {
    const { target } = crate();
    await expect(
      buildDatedToStart(target, async () => {
        throw new Error("cargo failed");
      }),
    ).rejects.toThrow("cargo failed");
    expect(staleDists([target])).toHaveLength(1);
  });
});

describe("pendingBuild", () => {
  /** A tree on which every build output is current, and the checks it ran. */
  const current = (
    overrides: Partial<BuildInputs> = {},
    failing: string[] = [],
  ) => {
    const root = scratch();
    write(root, {
      "pnpm-lock.yaml": "lock",
      "node_modules/.pnpm/lock.yaml": "lock",
      "pkg/src/a.ts": ["a", 0],
      "pkg/dist/index.js": ["a", 10],
      "plutus.json.deployment.json": JSON.stringify({
        profile: { name: DEPLOYMENT_PROFILE },
      }),
    });
    const checks: string[] = [];
    const inputs: BuildInputs = {
      repoRoot: root,
      lockfile: join(root, "pnpm-lock.yaml"),
      installedLockfile: join(root, "node_modules/.pnpm/lock.yaml"),
      targets: [
        {
          packageName: "pkg",
          dist: join(root, "pkg/dist/index.js"),
          sources: [join(root, "pkg/src")],
        },
      ],
      blueprintRecord: join(root, "plutus.json.deployment.json"),
      check: async (args, label) => {
        checks.push(`${label}: ${args.join(" ")}`);
        return failing.includes(label) ? 1 : 0;
      },
      ...overrides,
    };
    return { root, inputs, checks };
  };

  it("finds nothing to build on a current tree, after checking the profile and the blueprint", async () => {
    const { inputs, checks } = current();
    expect(await pendingBuild(inputs)).toEqual([]);
    expect(checks).toEqual([
      `profile-check: scripts/deployment-profiles.mjs check ${DEPLOYMENT_PROFILE}`,
      "blueprint-check: scripts/lib/blueprint-stamp.mjs",
    ]);
  });

  it("builds when the install does not match the lockfile, or a dist is stale, without running the checks", async () => {
    const lockfile = current();
    write(lockfile.root, { "pnpm-lock.yaml": "lock v2" });
    expect(await pendingBuild(lockfile.inputs)).toEqual([
      "the installed dependencies do not match pnpm-lock.yaml",
    ]);
    expect(lockfile.checks).toEqual([]);
    const stale = current();
    write(stale.root, { "pkg/src/a.ts": ["a2", 20] });
    expect(await pendingBuild(stale.inputs)).toEqual([
      "pkg: pkg/dist/index.js is older than pkg/src/a.ts",
    ]);
  });

  it("builds when the generated profile, the blueprint or its profile is not current", async () => {
    expect(await pendingBuild(current({}, ["profile-check"]).inputs)).toEqual([
      `the generated deployment files are not those of ${DEPLOYMENT_PROFILE}`,
    ]);
    expect(await pendingBuild(current({}, ["blueprint-check"]).inputs)).toEqual(
      ["the blueprint does not match its sources and the pinned compiler"],
    );
    const other = current();
    write(other.root, {
      "plutus.json.deployment.json": JSON.stringify({
        profile: { name: "preprod-testing" },
      }),
    });
    expect(await pendingBuild(other.inputs)).toEqual([
      `the blueprint was not built for ${DEPLOYMENT_PROFILE}`,
    ]);
  });
});
