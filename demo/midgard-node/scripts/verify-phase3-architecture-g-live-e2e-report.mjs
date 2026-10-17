#!/usr/bin/env node

import "./verify-phase3-architecture-g-live-e2e-report.validate-step-evidence-shape.mjs";
import "node:fs";
import "node:path";
import "node:url";
import "./phase3-architecture-g-closure-lib.mjs";
import "./verify-phase3-architecture-g-live-e2e-report.validate-step-evidence-shape.mjs";
import "./verify-phase3-architecture-g-live-e2e-report.validate-step-evidence.mjs";
import "./verify-phase3-architecture-g-live-e2e-report.evaluate-phase3-live-e2-ereport.mjs";

import fs from "node:fs";
import { fileURLToPath } from "node:url";

import { absoluteArg } from "./phase3-architecture-g-closure-lib.mjs";
import { evaluatePhase3LiveE2EReport } from "./verify-phase3-architecture-g-live-e2e-report.evaluate-phase3-live-e2-ereport.mjs";

const isMain = process.argv[1] === fileURLToPath(import.meta.url);

if (isMain) {
  try {
    const reportPath = absoluteArg(process.argv.slice(2), "--report");
    const result = evaluatePhase3LiveE2EReport(
      JSON.parse(fs.readFileSync(reportPath, "utf8")),
    );
    process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    if (!result.passed) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 1;
  }
}
export { evaluatePhase3LiveE2EReport } from "./verify-phase3-architecture-g-live-e2e-report.evaluate-phase3-live-e2-ereport.mjs";
export {
  PHASE3_LIVE_COMMAND_SCHEMA,
  PHASE3_LIVE_E2E_AUTHORIZATION,
  PHASE3_LIVE_E2E_SCENARIO,
  PHASE3_LIVE_E2E_SCHEMA,
  PHASE3_LIVE_STEP_IDS,
  PHASE3_LIVE_STEP_SCHEMA,
} from "./verify-phase3-architecture-g-live-e2e-report.validate-step-evidence-shape.mjs";
