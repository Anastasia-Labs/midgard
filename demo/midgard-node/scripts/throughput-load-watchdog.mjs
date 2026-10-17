#!/usr/bin/env node

import "./throughput-load-watchdog.docker-runtime.mjs";
import "node:fs";
import "node:path";
import "node:child_process";
import "node:url";
import "./throughput-load-watchdog.canonical-watchdog-evidence-record-v1.mjs";
import "./throughput-load-watchdog.docker-runtime.mjs";
import "./throughput-load-watchdog.run-throughput-load-watchdog.mjs";

import { pathToFileURL } from "node:url";

import { main } from "./throughput-load-watchdog.run-throughput-load-watchdog.mjs";

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((error) => {
    process.stderr.write(
      `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
    );
    process.exitCode = 1;
  });
}
export {
  DEFAULT_REQUIRED_LABEL,
  parseThroughputWatchdogEvidenceLineV1,
  parseWatchdogArgs,
  WATCHDOG_SCHEMA_VERSION,
} from "./throughput-load-watchdog.canonical-watchdog-evidence-record-v1.mjs";
export { createEvidenceWriter } from "./throughput-load-watchdog.docker-runtime.mjs";
export { runThroughputLoadWatchdog } from "./throughput-load-watchdog.run-throughput-load-watchdog.mjs";
