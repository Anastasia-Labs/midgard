#!/usr/bin/env node

import "./verify-phase3-architecture-g-soak-report.validate-sample-shape.mjs";
import "node:crypto";
import "node:fs";
import "node:path";
import "node:url";
import "./phase3-architecture-g-closure-lib.mjs";
import "./phase3-architecture-g-soak-preflight.mjs";
import "./throughput-valid-stress-corpus.mjs";
import "./phase3-architecture-g-load-generator-isolation.mjs";
import "./verify-phase3-architecture-g-soak-report.validate-sample-shape.mjs";
import "./verify-phase3-architecture-g-soak-report.validate-workload-summary-shape.mjs";
import "./verify-phase3-architecture-g-soak-report.validate-soak-report-shape.mjs";
import "./verify-phase3-architecture-g-soak-report.evaluate-phase3-architecture-gsoak-report.mjs";
import "./verify-phase3-architecture-g-soak-report.verify-phase3-architecture-gsoak-report-file.mjs";

import { fileURLToPath } from "node:url";

import { verifyPhase3ArchitectureGSoakReportFile } from "./verify-phase3-architecture-g-soak-report.verify-phase3-architecture-gsoak-report-file.mjs";

const isMain = process.argv[1] === fileURLToPath(import.meta.url);

if (isMain) {
  const reportPath = process.argv[2];
  if (reportPath === undefined) {
    console.error(
      "usage: verify-phase3-architecture-g-soak-report.mjs <report.json>",
    );
    process.exitCode = 2;
  } else {
    verifyPhase3ArchitectureGSoakReportFile(reportPath)
      .then((verification) => {
        console.log(JSON.stringify(verification, null, 2));
        if (!verification.passed) process.exitCode = 1;
      })
      .catch((error) => {
        console.error(error instanceof Error ? error.message : String(error));
        process.exitCode = 1;
      });
  }
}
export { evaluatePhase3ArchitectureGSoakReport } from "./verify-phase3-architecture-g-soak-report.evaluate-phase3-architecture-gsoak-report.mjs";
export {
  PHASE3_ACCEPTED_RATE_MIN_RATIO,
  PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS,
  PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS,
  PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC,
  PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
  PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
  PHASE3_ARCHITECTURE_G_TARGET_TPS,
  PHASE3_DRAIN_TIMEOUT_SEC,
  PHASE3_GENERATED_MAX_BYTES,
  PHASE3_GENERATED_MAX_NODES,
  PHASE3_MAX_AUDIT_AGE_MS,
  PHASE3_NODE_SATURATION_MIN_RATIO,
  PHASE3_OFFERED_RATE_MIN_RATIO,
  PHASE3_OWNER_MAX_RESIDENT_BYTES,
  PHASE3_OWNER_MAX_RESIDENT_NODES,
  PHASE3_PROCESS_MAX_DAILY_GROWTH_RATIO,
  PHASE3_WORKLOAD_LIFECYCLE_GRACE_MS,
} from "./verify-phase3-architecture-g-soak-report.validate-sample-shape.mjs";
export { verifyPhase3ArchitectureGSoakReportFile } from "./verify-phase3-architecture-g-soak-report.verify-phase3-architecture-gsoak-report-file.mjs";
