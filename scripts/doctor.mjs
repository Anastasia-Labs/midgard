#!/usr/bin/env node

// Environment diagnosis for this checkout: is everything the checks and suites
// need present, and if not, the exact command that fixes it. Read-only: it
// starts nothing, installs nothing and writes no configuration.
//
// usage: node scripts/doctor.mjs [--report-only] [--json]
// Exit 0 when nothing failed (warnings do not count), 1 when something failed,
// 3 when nothing failed but something could not be checked. --report-only
// prints a compact summary and always exits 0 (the session-start hook).

import "node:child_process";
import "node:fs";
import "node:path";
import "node:url";
import "./preflight/probes.mjs";
import "./doctor.check-hooks.mjs";
import "./doctor.check-pnpm.mjs";

import { dirname, isAbsolute, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { EXIT } from "./doctor.check-hooks.mjs";
import { formatReport, runDoctor } from "./doctor.check-pnpm.mjs";

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
export {
  checkHooks,
  checkNode,
  EXIT,
  GIT_HOOK_NAMES,
} from "./doctor.check-hooks.mjs";
export {
  checkPnpm,
  defaultChecks,
  formatReport,
  runDoctor,
} from "./doctor.check-pnpm.mjs";
