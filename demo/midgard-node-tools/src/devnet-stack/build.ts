import { existsSync, readFileSync, utimesSync } from "node:fs";
import { join } from "node:path";

import {
  describeStale,
  type DistTarget,
  nativeBuildTargets,
  runtimeDistTargets,
  staleDists,
} from "./dist-freshness.js";
import { execLogged, requireSuccess } from "./exec.js";
import type { Layout } from "./layout.js";

export const DEPLOYMENT_PROFILE = "local-devnet-testing";

const TOOLS = [
  ["docker", ["compose", "version"]],
  ["pnpm", ["--version"]],
  ["aiken", ["--version"]],
  ["cargo", ["--version"]],
  ["go", ["version"]],
] as const;

/**
 * Refuses to start on a host that cannot run the stack, before anything is
 * generated or spent. The services run on this same Node binary.
 */
export const validateEnvironment = async (logDir: string): Promise<void> => {
  const [major] = process.versions.node.split(".").map(Number);
  if (major !== 22)
    throw new Error(
      `Node ${process.versions.node} is not supported; run the controller with Node 22 (demo/.nvmrc)`,
    );
  const missing: string[] = [];
  for (const [command, args] of TOOLS) {
    try {
      const result = await execLogged(command, args, {
        logDir,
        label: `tool-${command}`,
        timeoutMs: 30_000,
      });
      if (result.code !== 0) missing.push(command);
    } catch {
      missing.push(command);
    }
  }
  if (missing.length > 0)
    throw new Error(`required tools are unavailable: ${missing.join(", ")}`);
  const daemon = await execLogged(
    "docker",
    ["info", "--format", "{{.ServerVersion}}"],
    {
      logDir,
      label: "docker-daemon",
      timeoutMs: 30_000,
    },
  );
  if (daemon.code !== 0) throw new Error("the Docker daemon is not reachable");
};

/**
 * Pinned dependencies, the profile's contracts and every runtime the stack
 * executes: lockfile install, profile build (Aiken blueprint + generated
 * profile), workspace dist, native MPF owner and chain-sync helper. Its logs
 * go to `logDir`, outside the run directory: on a fresh run the directory is
 * still to be generated, and generation moves any earlier content aside.
 */
export const buildEverything = async (
  layout: Layout,
  logDir: string,
): Promise<void> => {
  const demo = join(layout.repoRoot, "demo");
  const run = async (
    command: string,
    args: readonly string[],
    label: string,
    cwd = demo,
  ) =>
    requireSuccess(
      await execLogged(command, args, {
        cwd,
        env: { CI: "true" },
        logDir,
        label,
        timeoutMs: 3_600_000,
      }),
      label,
    );
  console.log("build: installing the locked workspace dependencies");
  await run("pnpm", ["install", "--frozen-lockfile"], "pnpm-install");
  console.log(`build: contracts and profile ${DEPLOYMENT_PROFILE}`);
  await run(
    "pnpm",
    ["run", "deployment:build", DEPLOYMENT_PROFILE],
    "deployment-build",
  );
  console.log("build: workspace packages");
  await run("pnpm", ["run", "build"], "workspace-build");
  console.log("build: native MPF owner and chain-sync helper");
  const [owner, chainSync] = nativeBuildTargets(layout);
  await buildDatedToStart(owner!, async () => {
    await run(
      "pnpm",
      ["--dir", "midgard-node", "run", "native:mpf-owner:build"],
      "native-owner",
    );
  });
  await buildDatedToStart(chainSync!, async () => {
    await run(
      "pnpm",
      ["--dir", "midgard-watcher", "run", "native:build"],
      "native-chain-sync",
    );
  });
};

/**
 * Runs the step that builds `target`, then dates its output to the moment
 * the step started. Cargo leaves its binary untouched when no input it reads
 * changed, as after an edit to a `#[cfg(test)]` module or a touch of the
 * manifest, and the freshness rule judges the binary against all of those;
 * left alone it would read as stale after every build. Dated to the start,
 * not the end, a source edited while the step ran still reads as newer.
 */
export const buildDatedToStart = async (
  target: DistTarget,
  step: () => Promise<void>,
): Promise<void> => {
  const started = new Date();
  await step();
  utimesSync(target.dist, started, started);
};

/** What decides whether buildEverything would change anything. */
export type BuildInputs = {
  readonly repoRoot: string;
  readonly lockfile: string;
  /** pnpm's copy of the lockfile it last installed from. */
  readonly installedLockfile: string;
  /** Every dist and native binary the build writes, with its sources. */
  readonly targets: readonly DistTarget[];
  /** The deployment profile the blueprint's build record names. */
  readonly blueprintRecord: string;
  /** Runs a check script under demo/ and resolves to its exit status. */
  readonly check: (args: readonly string[], label: string) => Promise<number>;
};

export const buildInputs = (layout: Layout, logDir: string): BuildInputs => {
  const demo = join(layout.repoRoot, "demo");
  return {
    repoRoot: layout.repoRoot,
    lockfile: join(demo, "pnpm-lock.yaml"),
    installedLockfile: join(demo, "node_modules/.pnpm/lock.yaml"),
    targets: [...runtimeDistTargets(layout), ...nativeBuildTargets(layout)],
    blueprintRecord: join(
      layout.repoRoot,
      "onchain/aiken/plutus.json.deployment.json",
    ),
    check: async (args, label) =>
      (
        await execLogged(process.execPath, args, {
          cwd: demo,
          logDir,
          label,
          timeoutMs: 300_000,
        })
      ).code ?? 1,
  };
};

const recordedProfile = (record: string): string | undefined => {
  try {
    return (
      JSON.parse(readFileSync(record, "utf8")) as {
        profile?: { name?: string };
      }
    ).profile?.name;
  } catch {
    return undefined;
  }
};

/**
 * Why buildEverything is needed, one line per reason; empty when every
 * output it writes is current for the tree. Each step's own freshness rule:
 * the install matches pnpm-lock.yaml byte for byte, no dist or native
 * binary is older than a source (the rule `--no-build` enforces), the
 * generated deployment files are this profile's, and the blueprint's build
 * record matches its sources, the pinned compiler and this profile.
 */
export const pendingBuild = async (inputs: BuildInputs): Promise<string[]> => {
  const reasons: string[] = [];
  if (
    !existsSync(inputs.installedLockfile) ||
    !readFileSync(inputs.installedLockfile).equals(
      readFileSync(inputs.lockfile),
    )
  )
    reasons.push("the installed dependencies do not match pnpm-lock.yaml");
  for (const stale of staleDists(inputs.targets))
    reasons.push(describeStale(inputs.repoRoot, stale));
  // The checks below run on the installed tree; a build is due anyway.
  if (reasons.length > 0) return reasons;
  const profile = await inputs.check(
    ["scripts/deployment-profiles.mjs", "check", DEPLOYMENT_PROFILE],
    "profile-check",
  );
  if (profile !== 0)
    reasons.push(
      `the generated deployment files are not those of ${DEPLOYMENT_PROFILE}`,
    );
  const blueprint = await inputs.check(
    ["scripts/lib/blueprint-stamp.mjs"],
    "blueprint-check",
  );
  if (blueprint !== 0)
    reasons.push(
      "the blueprint does not match its sources and the pinned compiler",
    );
  else if (recordedProfile(inputs.blueprintRecord) !== DEPLOYMENT_PROFILE)
    reasons.push(`the blueprint was not built for ${DEPLOYMENT_PROFILE}`);
  return reasons;
};
