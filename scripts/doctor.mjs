#!/usr/bin/env node

// Environment diagnosis for this checkout: is everything the checks and suites
// need present, and if not, the exact command that fixes it. Read-only: it
// starts nothing, installs nothing and writes no configuration.
//
// usage: node scripts/doctor.mjs [--report-only] [--json]
// Exit 0 when nothing failed (warnings do not count), 1 when something failed,
// 3 when nothing failed but something could not be checked. --report-only
// prints a compact summary and always exits 0 (the session-start hook).

import { spawnSync } from "node:child_process";
import {
  accessSync,
  constants,
  existsSync,
  lstatSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  realpathSync,
  statSync,
} from "node:fs";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  probeBlueprintStamp,
  probeCoreDist,
  probeDbPrefix,
  probeNodeModules,
  probePackageDist,
  probePinnedCompiler,
  probePostgres,
} from "./preflight/probes.mjs";

export const EXIT = { ok: 0, failed: 1, unknown: 3 };
const SPAWN_TIMEOUT_MS = 10_000;

// The hook names git itself invokes (githooks(5)). Anything else in the hooks
// directory is never run by git.
export const GIT_HOOK_NAMES = [
  "applypatch-msg",
  "pre-applypatch",
  "post-applypatch",
  "pre-commit",
  "pre-merge-commit",
  "prepare-commit-msg",
  "commit-msg",
  "post-commit",
  "pre-rebase",
  "post-checkout",
  "post-merge",
  "pre-push",
  "pre-receive",
  "update",
  "proc-receive",
  "post-receive",
  "post-update",
  "reference-transaction",
  "push-to-checkout",
  "pre-auto-gc",
  "post-rewrite",
  "sendemail-validate",
  "fsmonitor-watchman",
  "p4-changelist",
  "p4-prepare-changelist",
  "p4-post-changelist",
  "p4-pre-submit",
  "post-index-change",
];

// A doctor row: status "ok" | "warn" | "failed" | "unknown".
const row = (name, status, detail, fix) => ({ name, status, detail, fix });

const fromProbe = (probe, label = probe.name) =>
  row(
    label,
    probe.status === "available"
      ? "ok"
      : probe.status === "missing"
        ? "failed"
        : "unknown",
    probe.detail,
    probe.fix,
  );

const git = (root, args) =>
  spawnSync("git", args, {
    cwd: root,
    encoding: "utf8",
    timeout: SPAWN_TIMEOUT_MS,
  });

const isExecutable = (path) => {
  try {
    accessSync(path, constants.X_OK);
    return (statSync(path).mode & 0o111) !== 0;
  } catch {
    return false;
  }
};

// Every hook git would run for this checkout, read without changing anything:
// the hooks directory is `git rev-parse --git-path hooks`, which already
// honours core.hooksPath and the worktree's common directory. Each hook is
// followed through its symlinks. The incident this guards: a hook that lost
// its executable bit (hidden by core.fileMode=false) is skipped by git without
// an error, so a commit or push runs none of its checks.
export const checkHooks = ({ root }) => {
  const rows = [];
  const hooksRun = git(root, [
    "rev-parse",
    "--path-format=absolute",
    "--git-path",
    "hooks",
  ]);
  const dirsRun = git(root, [
    "rev-parse",
    "--path-format=absolute",
    "--git-dir",
    "--git-common-dir",
  ]);
  if (hooksRun.status !== 0 || dirsRun.status !== 0) {
    return [
      row(
        "hooks",
        "unknown",
        `could not check: git rev-parse failed (${(hooksRun.stderr || dirsRun.stderr || hooksRun.error?.message || "").trim()})`,
        undefined,
      ),
    ];
  }
  const hooksPath = hooksRun.stdout.trim();
  const [gitDir, commonDir] = dirsRun.stdout.trim().split("\n");
  const isWorktree = gitDir !== commonDir;
  const install = isWorktree
    ? "bash .githooks/install, run from the main checkout (run from a linked worktree it would repoint every checkout's hooks at this one)"
    : "bash .githooks/install";
  const ownHooks = resolve(root, ".githooks");
  const ownReal = existsSync(ownHooks) ? realpathSync(ownHooks) : ownHooks;

  // The repository's versioned hooks must be executable in the index too:
  // with core.fileMode=false a lost bit never shows in `git status`, and the
  // next clone checks the hook out non-executable.
  const tracked = git(root, ["ls-files", "-s", "--", ".githooks"]);
  const versioned = [];
  if (tracked.status === 0) {
    for (const line of tracked.stdout.split("\n").filter(Boolean)) {
      const [mode, , , ...pathParts] = line.split(/\s+/u);
      const path = pathParts.join(" ");
      const name = path.slice(".githooks/".length);
      if (!GIT_HOOK_NAMES.includes(name)) {
        continue;
      }
      versioned.push(name);
      if (mode !== "100755") {
        rows.push(
          row(
            `hooks:${name}`,
            "failed",
            `${path} is committed without its executable bit (mode ${mode})`,
            `git update-index --chmod=+x ${path}`,
          ),
        );
      }
    }
  }

  const present = existsSync(hooksPath)
    ? readdirSync(hooksPath).filter(
        (name) =>
          GIT_HOOK_NAMES.includes(name) ||
          // The versioned pre-commit chains to <hook>.local when it is
          // executable, and skips it silently when it is not.
          (name.endsWith(".local") &&
            GIT_HOOK_NAMES.includes(name.slice(0, -".local".length))),
      )
    : [];
  for (const name of present) {
    const path = join(hooksPath, name);
    const shown = relative(root, path).startsWith("..")
      ? path
      : relative(root, path);
    let target;
    try {
      target = realpathSync(path);
    } catch {
      const link = lstatSync(path).isSymbolicLink()
        ? ` (symlink to ${readlinkSafe(path)})`
        : "";
      rows.push(
        row(
          `hooks:${name}`,
          "failed",
          `${shown} points at nothing${link}; git skips it`,
          install,
        ),
      );
      continue;
    }
    if (!isExecutable(target)) {
      rows.push(
        row(
          `hooks:${name}`,
          "failed",
          `${target === path ? shown : `${shown} -> ${target}`} is not executable, so git skips it without an error`,
          `chmod +x ${target}`,
        ),
      );
      continue;
    }
    if (
      versioned.includes(name) &&
      dirname(target) !== ownReal &&
      target.endsWith(`/.githooks/${name}`)
    ) {
      rows.push(
        row(
          `hooks:${name}`,
          "warn",
          `${shown} runs ${target}, another checkout's copy, not this checkout's .githooks/${name}; a hook changed on this branch is not the one that runs`,
          isWorktree
            ? `diff ${target} .githooks/${name} (no output: the copies agree and nothing is lost)`
            : install,
        ),
      );
    }
  }
  for (const name of versioned) {
    if (!present.includes(name)) {
      rows.push(
        row(
          `hooks:${name}`,
          "failed",
          `.githooks/${name} is not installed in ${hooksPath}, so git never runs it`,
          install,
        ),
      );
    }
  }
  if (rows.length === 0) {
    rows.push(
      row(
        "hooks",
        "ok",
        `${String(present.length)} hook(s) in ${hooksPath}, all executable`,
      ),
    );
  }
  return rows;
};

const readlinkSafe = (path) => {
  try {
    return readlinkSync(path);
  } catch {
    return "?";
  }
};

const parseVersion = (text) => {
  const match = /(\d+)\.(\d+)\.(\d+)/u.exec(text ?? "");
  return match === null ? undefined : match.slice(1, 4).map(Number);
};

const compareVersions = (left, right) => {
  for (let index = 0; index < 3; index += 1) {
    if (left[index] !== right[index]) {
      return left[index] - right[index];
    }
  }
  return 0;
};

// Node: below the workspace's engines floor fails; anything but the version
// CI pins in demo/.nvmrc warns.
export const checkNode = ({ root, version = process.version }) => {
  let pinned;
  let floor;
  try {
    pinned = parseVersion(readFileSync(resolve(root, "demo/.nvmrc"), "utf8"));
    floor = parseVersion(
      JSON.parse(readFileSync(resolve(root, "demo/package.json"), "utf8"))
        .engines?.node,
    );
  } catch (error) {
    return row("node", "unknown", `could not check: ${error.message}`);
  }
  const current = parseVersion(version);
  if (pinned === undefined || current === undefined) {
    return row(
      "node",
      "unknown",
      "could not check: demo/.nvmrc or the node version is unreadable",
    );
  }
  const fix = `nvm install ${pinned.join(".")} && nvm use ${pinned.join(".")}`;
  if (floor !== undefined && compareVersions(current, floor) < 0) {
    return row(
      "node",
      "failed",
      `node ${current.join(".")} is below the workspace floor ${floor.join(".")}`,
      fix,
    );
  }
  if (compareVersions(current, pinned) !== 0) {
    return row(
      "node",
      "warn",
      `node ${current.join(".")}; CI uses ${pinned.join(".")} (demo/.nvmrc)`,
      fix,
    );
  }
  return row("node", "ok", `node ${current.join(".")} matches demo/.nvmrc`);
};

// pnpm as the workspace resolves it (run inside demo/, where packageManager
// applies). A different pnpm can rewrite the lockfile.
export const checkPnpm = ({ root, run = defaultPnpmVersion }) => {
  let pinned;
  try {
    const manager = JSON.parse(
      readFileSync(resolve(root, "demo/package.json"), "utf8"),
    ).packageManager;
    pinned = /^pnpm@(\d+\.\d+\.\d+)/u.exec(manager ?? "")?.[1];
  } catch (error) {
    return row("pnpm", "unknown", `could not check: ${error.message}`);
  }
  if (pinned === undefined) {
    return row(
      "pnpm",
      "unknown",
      "could not check: demo/package.json declares no pnpm packageManager",
    );
  }
  const fix = `corepack enable && corepack prepare pnpm@${pinned} --activate`;
  const reported = run(resolve(root, "demo"));
  if (reported.error !== undefined) {
    return row(
      "pnpm",
      reported.missing ? "failed" : "unknown",
      reported.missing
        ? "pnpm is not on PATH"
        : `could not check: ${reported.error}`,
      fix,
    );
  }
  const version = parseVersion(reported.version)?.join(".");
  if (version === undefined) {
    return row(
      "pnpm",
      "unknown",
      `could not check: pnpm --version printed '${reported.version}'`,
      fix,
    );
  }
  return version === pinned
    ? row("pnpm", "ok", `pnpm ${version} in demo/ matches packageManager`)
    : row(
        "pnpm",
        "failed",
        `pnpm ${version} in demo/; packageManager pins ${pinned}`,
        fix,
      );
};

const defaultPnpmVersion = (cwd) => {
  const run = spawnSync("pnpm", ["--version"], {
    cwd,
    encoding: "utf8",
    timeout: SPAWN_TIMEOUT_MS,
  });
  if (run.error !== undefined) {
    return {
      error: run.error.message,
      missing: run.error.code === "ENOENT",
    };
  }
  if (run.status !== 0) {
    return { error: `pnpm --version exited ${String(run.status)}` };
  }
  // pnpm may print a version-switch notice before the version.
  return { version: run.stdout.trim().split("\n").at(-1) };
};

export const defaultChecks = {
  aiken: () => fromProbe(probePinnedCompiler({})),
  postgres: ({ env }) =>
    probePostgres({ env }).then((probe) => fromProbe(probe)),
  "db-prefix": ({ root, env }) =>
    probeDbPrefix({ root, env }).then((probe) => fromProbe(probe)),
  "node-modules": ({ root }) => fromProbe(probeNodeModules({ root })),
  blueprint: ({ root }) =>
    probeBlueprintStamp({ root }).then((probe) => fromProbe(probe)),
  "dist:midgard-core": ({ root }) =>
    fromProbe(probeCoreDist({ root }), "dist:midgard-core"),
  "dist:midgard-sdk": ({ root }) =>
    fromProbe(
      probePackageDist({
        root,
        directory: "demo/midgard-sdk",
        name: "@al-ft/midgard-sdk",
      }),
      "dist:midgard-sdk",
    ),
  "dist:midgard-validation": ({ root }) =>
    fromProbe(
      probePackageDist({
        root,
        directory: "demo/midgard-validation",
        name: "@al-ft/midgard-validation",
      }),
      "dist:midgard-validation",
    ),
  hooks: ({ root }) => checkHooks({ root }),
  node: ({ root }) => checkNode({ root }),
  pnpm: ({ root }) => checkPnpm({ root }),
};

export const runDoctor = async ({
  root,
  env = process.env,
  checks = defaultChecks,
}) => {
  const rows = [];
  for (const [name, check] of Object.entries(checks)) {
    try {
      const outcome = await check({ root, env });
      rows.push(...(Array.isArray(outcome) ? outcome : [outcome]));
    } catch (error) {
      rows.push(row(name, "unknown", `could not check: ${error.message}`));
    }
  }
  const exitCode = rows.some((r) => r.status === "failed")
    ? EXIT.failed
    : rows.some((r) => r.status === "unknown")
      ? EXIT.unknown
      : EXIT.ok;
  return { rows, exitCode };
};

const LABEL = { ok: "ok", warn: "WARN", failed: "FAIL", unknown: "SKIP" };

export const formatReport = ({ rows, exitCode }, { compact }) => {
  const problems = rows.filter((r) => r.status !== "ok");
  const lines = [];
  if (compact) {
    const fine = rows.filter((r) => r.status === "ok").map((r) => r.name);
    lines.push(
      `midgard doctor: ${problems.length === 0 ? "environment ready" : `${String(problems.length)} item(s) need attention`}${fine.length > 0 ? ` (ok: ${fine.join(", ")})` : ""}`,
    );
  } else {
    for (const r of rows) {
      lines.push(`${LABEL[r.status].padEnd(4)} ${r.name}: ${r.detail}`);
      if (r.status !== "ok" && r.fix !== undefined) {
        lines.push(`     fix: ${r.fix}`);
      }
    }
  }
  if (compact) {
    for (const r of problems) {
      lines.push(`- ${LABEL[r.status]} ${r.name}: ${r.detail}`);
      if (r.fix !== undefined) {
        lines.push(`  fix: ${r.fix}`);
      }
    }
    if (problems.length > 0) {
      lines.push("Full report: node scripts/doctor.mjs");
    }
  } else {
    lines.push(
      exitCode === EXIT.ok
        ? "doctor: nothing failed"
        : exitCode === EXIT.failed
          ? "doctor: something failed (fix commands above)"
          : "doctor: nothing failed, but some items could not be checked (exit 3)",
    );
  }
  return `${lines.join("\n")}\n`;
};

export const main = async (
  argv,
  {
    root = resolve(dirname(fileURLToPath(import.meta.url)), ".."),
    env = process.env,
    stdout = (text) => process.stdout.write(text),
    stderr = (text) => process.stderr.write(text),
    checks,
  } = {},
) => {
  const reportOnly = argv.includes("--report-only");
  const json = argv.includes("--json");
  const unknownArgs = argv.filter(
    (arg) => arg !== "--report-only" && arg !== "--json",
  );
  if (unknownArgs.length > 0) {
    stderr(
      `unknown argument '${unknownArgs[0]}'\nusage: node scripts/doctor.mjs [--report-only] [--json]\n`,
    );
    return reportOnly ? EXIT.ok : 2;
  }
  let report;
  try {
    report = await runDoctor({
      root: isAbsolute(root) ? root : resolve(root),
      env,
      ...(checks === undefined ? {} : { checks }),
    });
  } catch (error) {
    // The session-start hook must never fail a session.
    stderr(`midgard doctor could not run: ${error.message}\n`);
    return reportOnly ? EXIT.ok : EXIT.unknown;
  }
  stdout(
    json
      ? `${JSON.stringify(report, null, 2)}\n`
      : formatReport(report, { compact: reportOnly }),
  );
  return reportOnly ? EXIT.ok : report.exitCode;
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  process.exitCode = await main(process.argv.slice(2));
}
