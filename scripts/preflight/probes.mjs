// Capability probes shared by preflight and doctor.
//
// Every probe answers one question about the environment and returns
//   { name, status, detail, fix, env? }
// where status is
//   "available" — the capability is there;
//   "missing"   — it was looked for and is not there (fix says how to get it);
//   "unknown"   — it could not be looked for (a probe module is absent, a
//                 tool could not run). Unknown is never treated as available.
// `env`, when present, is what a check that needs the capability must run
// with (the per-worktree test database prefix).

import { spawnSync } from "node:child_process";
import { existsSync, readdirSync, statSync } from "node:fs";
import { connect } from "node:net";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { assertPinnedAiken } from "../../onchain/aiken/scripts/pinned-compiler.mjs";

// The local test server. The live end-to-end stack runs its own Postgres on
// 55433; nothing here may ever point a test suite at it.
export const TEST_POSTGRES_PORT = 5433;
export const FORBIDDEN_POSTGRES_PORTS = [55433];
export const START_TEST_POSTGRES = "bash scripts/start-test-postgres.sh";
const CORE_DIST_CHECK = "demo/scripts/assert-midgard-core-dist-current.mjs";
const BLUEPRINT_STAMP_MODULE = "demo/scripts/lib/blueprint-stamp.mjs";
const WORKTREE_IDENTITY_MODULE = "scripts/lib/worktree-identity.mjs";

const result = (name, status, detail, fix, extra = {}) => ({
  name,
  status,
  detail,
  fix,
  ...extra,
});

const importOptional = async (root, path) => {
  const absolute = resolve(root, path);
  if (!existsSync(absolute)) {
    return undefined;
  }
  return import(pathToFileURL(absolute).href);
};

export const probePostgres = ({
  host = "127.0.0.1",
  port = TEST_POSTGRES_PORT,
  timeoutMs = 1000,
  env = process.env,
} = {}) => {
  if (FORBIDDEN_POSTGRES_PORTS.includes(Number(port))) {
    throw new Error(
      `refusing to probe Postgres on ${String(port)}: that is the live stack, not the test server`,
    );
  }
  // The suites read POSTGRES_PORT; an ambient value pointing elsewhere would
  // send them past the server this probe looked at.
  const ambient = env.POSTGRES_PORT;
  if (ambient !== undefined && ambient !== "" && Number(ambient) !== port) {
    return Promise.resolve(
      result(
        "postgres",
        "missing",
        `POSTGRES_PORT=${ambient} in the environment points the test suites away from the test server on ${String(port)}`,
        "unset POSTGRES_PORT (the suites default to the test server on 5433)",
      ),
    );
  }
  return new Promise((done) => {
    const socket = connect({ host, port });
    const finish = (status, detail) => {
      socket.destroy();
      done(
        result(
          "postgres",
          status,
          detail,
          status === "available" ? undefined : START_TEST_POSTGRES,
        ),
      );
    };
    socket.setTimeout(timeoutMs, () =>
      finish(
        "missing",
        `no answer from ${host}:${String(port)} within ${String(timeoutMs)} ms`,
      ),
    );
    socket.once("connect", () =>
      finish("available", `test Postgres answers on ${host}:${String(port)}`),
    );
    socket.once("error", (error) =>
      finish(
        "missing",
        `test Postgres on ${host}:${String(port)} is not reachable (${error.code ?? error.message})`,
      ),
    );
  });
};

export const probePinnedCompiler = ({ binary } = {}) => {
  try {
    const version = assertPinnedAiken(binary);
    return result("aiken", "available", version);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return result(
      "aiken",
      // An unreadable pin means nothing could be compared; any other refusal
      // means the compiler this run would use is not the pinned one.
      /must declare|pin different/u.test(message) ? "unknown" : "missing",
      message.split("\n")[0],
      existsSync(resolve(import.meta.dirname, "../ci/build-aiken-fork.sh"))
        ? "bash scripts/ci/build-aiken-fork.sh, then export MIDGARD_AIKEN_BIN=<the fork binary it reports>"
        : "export MIDGARD_AIKEN_BIN=<path to the pinned fork>, or build it as .github/workflows/aiken-ci.yml does",
    );
  }
};

export const probeBlueprintStamp = async ({ root }) => {
  const module = await importOptional(root, BLUEPRINT_STAMP_MODULE).catch(
    (error) => ({ loadError: error }),
  );
  if (
    module === undefined ||
    typeof module.checkBlueprintStamp !== "function"
  ) {
    return result(
      "blueprint",
      "unknown",
      module?.loadError === undefined
        ? `could not check: ${BLUEPRINT_STAMP_MODULE} absent`
        : `could not check: ${BLUEPRINT_STAMP_MODULE} failed to load (${module.loadError.message})`,
      undefined,
    );
  }
  let verdict;
  try {
    verdict = await module.checkBlueprintStamp({ root });
  } catch (error) {
    return result(
      "blueprint",
      "unknown",
      `could not check: ${error.message}`,
      undefined,
    );
  }
  const status =
    verdict?.status === "fresh"
      ? "available"
      : verdict?.status === "stale" || verdict?.status === "missing"
        ? "missing"
        : "unknown";
  return result(
    "blueprint",
    status,
    `blueprint stamp ${String(verdict?.status)}${verdict?.detail ? `: ${verdict.detail}` : ""}`,
    status === "available" ? undefined : verdict?.fix,
  );
};

export const probeCoreDist = ({ root }) => {
  if (!existsSync(resolve(root, "demo/midgard-core/dist"))) {
    return result(
      "core-dist",
      "missing",
      "demo/midgard-core/dist is absent",
      "pnpm --dir demo --filter @al-ft/midgard-core run build",
    );
  }
  const run = spawnSync(process.execPath, [CORE_DIST_CHECK], {
    cwd: root,
    encoding: "utf8",
    timeout: 60_000,
  });
  const output = `${run.stdout ?? ""}${run.stderr ?? ""}`.trim();
  if (run.error !== undefined || run.status === null) {
    return result(
      "core-dist",
      "unknown",
      `could not check: ${run.error?.message ?? "the freshness check did not finish"}`,
      undefined,
    );
  }
  return run.status === 0
    ? result("core-dist", "available", output.split("\n")[0])
    : result(
        "core-dist",
        "missing",
        output.split("\n")[0] ||
          `the freshness check exited ${String(run.status)}`,
        "pnpm --dir demo --filter @al-ft/midgard-core run build",
      );
};

// Packages without a source-digest stamp can only be judged by timestamps: a
// source file newer than every emitted file proves the dist is stale.
export const probePackageDist = ({ root, directory, name }) => {
  const dist = resolve(root, directory, "dist");
  const probeName = `dist:${name}`;
  const build = `pnpm --dir demo --filter ${name} run build`;
  if (!existsSync(dist)) {
    return result(probeName, "missing", `${directory}/dist is absent`, build);
  }
  const newest = (path) => {
    let latest = 0;
    for (const entry of readdirSync(path, { recursive: true })) {
      const stats = statSync(join(path, String(entry)));
      if (stats.isFile() && stats.mtimeMs > latest) {
        latest = stats.mtimeMs;
      }
    }
    return latest;
  };
  const source = resolve(root, directory, "src");
  if (!existsSync(source)) {
    return result(
      probeName,
      "unknown",
      `could not check: ${directory}/src is absent`,
      undefined,
    );
  }
  if (newest(source) > newest(dist)) {
    return result(
      probeName,
      "missing",
      `${directory}/src has files newer than its dist`,
      build,
    );
  }
  // A timestamp check: it catches an edit after the last build, not a
  // checkout that moved sources backwards in time. Only midgard-core stamps
  // its dist with a source digest.
  return result(
    probeName,
    "available",
    `no file in ${directory}/src is newer than its dist (timestamp check)`,
  );
};

export const probeNodeModules = ({ root }) =>
  existsSync(resolve(root, "demo/node_modules/.modules.yaml"))
    ? result("node-modules", "available", "demo/node_modules is installed")
    : result(
        "node-modules",
        "missing",
        "demo/node_modules is not installed",
        "pnpm --dir demo install --frozen-lockfile",
      );

// Suites derive their database names from MIDGARD_TEST_DATABASE_PREFIX; two
// worktrees on the default prefix drop each other's shards.
export const probeDbPrefix = async ({ root, env = process.env }) => {
  const explicit = env.MIDGARD_TEST_DATABASE_PREFIX;
  if (explicit !== undefined && explicit !== "") {
    return result(
      "db-prefix",
      "available",
      `MIDGARD_TEST_DATABASE_PREFIX=${explicit}`,
    );
  }
  let identity;
  try {
    const module = await importOptional(root, WORKTREE_IDENTITY_MODULE);
    identity = module?.worktreeIdentity?.(root);
  } catch (error) {
    return result(
      "db-prefix",
      "unknown",
      `could not check: ${WORKTREE_IDENTITY_MODULE} failed (${error.message})`,
      "export MIDGARD_TEST_DATABASE_PREFIX=<unique>_",
    );
  }
  if (identity === undefined) {
    return result(
      "db-prefix",
      "unknown",
      `could not check: MIDGARD_TEST_DATABASE_PREFIX is unset and ${WORKTREE_IDENTITY_MODULE} is absent, so this checkout's databases cannot be told apart from another worktree's`,
      "export MIDGARD_TEST_DATABASE_PREFIX=<unique name for this worktree>",
    );
  }
  if (identity.isMainCheckout) {
    return result(
      "db-prefix",
      "available",
      "main checkout: the suites' default prefix",
    );
  }
  const prefix = `midgard_test_${identity.hash}`;
  return result(
    "db-prefix",
    "available",
    `worktree ${identity.slug}: MIDGARD_TEST_DATABASE_PREFIX=${prefix}`,
    undefined,
    { env: { MIDGARD_TEST_DATABASE_PREFIX: prefix } },
  );
};

export const probeMergeTree = ({ root }) => {
  const run = spawnSync("git", ["merge-tree", "-h"], {
    cwd: root,
    encoding: "utf8",
  });
  const usage = `${run.stdout ?? ""}${run.stderr ?? ""}`;
  return usage.includes("--write-tree")
    ? result(
        "git-merge-tree",
        "available",
        "git merge-tree supports --write-tree",
      )
    : result(
        "git-merge-tree",
        "missing",
        "git merge-tree --write-tree needs git 2.38 or later",
        "upgrade git",
      );
};

// The probe set preflight uses: each probe runs once, on first need, except
// that a check which rebuilds something (demo-build) invalidates the probes
// that looked at the old build.
export const createProbeSet = ({ root, env = process.env, overrides = {} }) => {
  const probes = {
    aiken: () => probePinnedCompiler({}),
    postgres: () => probePostgres({ env }),
    blueprint: () => probeBlueprintStamp({ root }),
    "core-dist": () => probeCoreDist({ root }),
    "node-modules": () => probeNodeModules({ root }),
    "db-prefix": () => probeDbPrefix({ root, env }),
    "git-merge-tree": () => probeMergeTree({ root }),
    ...overrides,
  };
  const cache = new Map();
  return {
    names: Object.keys(probes),
    get(name) {
      if (probes[name] === undefined) {
        return Promise.resolve(
          result(name, "unknown", `no probe named '${name}'`, undefined),
        );
      }
      if (!cache.has(name)) {
        cache.set(
          name,
          Promise.resolve().then(() => probes[name]()),
        );
      }
      return cache.get(name);
    },
    invalidate(names = []) {
      for (const name of names) {
        cache.delete(name);
      }
    },
  };
};
