#!/usr/bin/env node

import { spawn } from "node:child_process";
import { access, readFile } from "node:fs/promises";

import {
  PHASE5_DA_EXPECTED_NODE_VERSION,
  verifyPhase5DaDistributionReportWithEvidence,
} from "./verify-phase5-da-50k-distribution-report.mjs";

const required = [
  "MIDGARD_DA_PHASE5_FIXTURE_SUITE",
  "MIDGARD_DA_PHASE5_DISTRIBUTION_REPORT",
  "MIDGARD_DA_PHASE5_EXPECTED_IMAGE_REFERENCE",
  "MIDGARD_DA_PHASE5_EXPECTED_IMAGE_ID",
];
for (const name of required) {
  if (process.env[name] === undefined || process.env[name].length === 0) {
    throw new Error(`${name} is required for the formal Phase 5 DA gate`);
  }
}
if (process.version !== PHASE5_DA_EXPECTED_NODE_VERSION) {
  throw new Error(
    `formal Phase 5 DA gate requires ${PHASE5_DA_EXPECTED_NODE_VERSION}; got ${process.version}`,
  );
}
await access(process.env.MIDGARD_DA_PHASE5_FIXTURE_SUITE);

const child = spawn(
  "pnpm",
  [
    "exec",
    "vitest",
    "run",
    "tests/da-multi-process-50k-integration.test.ts",
    "--reporter=basic",
    "--disable-console-intercept",
  ],
  {
    env: {
      ...process.env,
      NODE_ENV: "emulator",
      MIDGARD_DA_PHASE5_DISTRIBUTION: "1",
      MIDGARD_DA_PUBLISH_CONCURRENCY: "8",
      MIDGARD_DA_ZSTD_LEVEL: "3",
    },
    stdio: "inherit",
  },
);
const code = await new Promise((resolve, reject) => {
  child.once("error", reject);
  child.once("exit", (exitCode, signal) => {
    if (signal !== null) {
      reject(new Error(`Phase 5 DA gate terminated by ${signal}`));
      return;
    }
    resolve(exitCode ?? 1);
  });
});
if (code !== 0) process.exit(code);

const report = JSON.parse(
  await readFile(process.env.MIDGARD_DA_PHASE5_DISTRIBUTION_REPORT, "utf8"),
);
const verdict = await verifyPhase5DaDistributionReportWithEvidence(
  report,
  process.env.MIDGARD_DA_PHASE5_FIXTURE_SUITE,
);
process.stdout.write(`${JSON.stringify(verdict)}\n`);
if (!verdict.passed) process.exitCode = 1;
