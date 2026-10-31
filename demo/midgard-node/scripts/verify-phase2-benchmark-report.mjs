import "./verify-phase2-benchmark-report.verify-chunk-ab-identity-and-order.mjs";
import "node:fs/promises";
import "node:url";
import "./verify-phase2-benchmark-report.verify-stage-breport.mjs";
import "./verify-phase2-benchmark-report.verify-chunk-ab-identity-and-order.mjs";
import "./verify-phase2-benchmark-report.verify-script-heavy-report.mjs";
import "./verify-phase2-benchmark-report.verify-phase2-benchmark-reports.mjs";
import "./verify-phase2-benchmark-report.main.mjs";

import { pathToFileURL } from "node:url";

import { main } from "./verify-phase2-benchmark-report.main.mjs";

if (
  process.argv[1] !== undefined &&
  pathToFileURL(process.argv[1]).href === import.meta.url
) {
  await main();
}
export { verifyPhase2BenchmarkReports } from "./verify-phase2-benchmark-report.verify-phase2-benchmark-reports.mjs";
export { verifyStageBReport } from "./verify-phase2-benchmark-report.verify-stage-breport.mjs";
