import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  checkHooks,
  checkNode,
  defaultPnpmVersion,
  EXIT,
  fromProbe,
  parseVersion,
  row,
} from "./doctor.check-hooks.mjs";
import {
  probeBlueprintStamp,
  probeCoreDist,
  probeDbPrefix,
  probeNodeModules,
  probePackageDist,
  probePinnedCompiler,
  probePostgres,
} from "./preflight/probes.mjs";

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

export const LABEL = {
  ok: "ok",
  warn: "WARN",
  failed: "FAIL",
  unknown: "SKIP",
};

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
