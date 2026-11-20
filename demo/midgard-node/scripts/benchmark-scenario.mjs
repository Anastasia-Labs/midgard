#!/usr/bin/env node
import { spawn } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  loadPhase1FormalBindingSync,
  PHASE1_FORMAL_SCENARIO,
  sha256FileSync,
  validatePhase1BindingEnvironment,
} from "./phase1-formal-identity.mjs";

const scenarioPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scenarioPath);
const packageRoot = path.resolve(scriptDir, "..");
const enginePath = path.join(
  packageRoot,
  "scripts/throughput-valid-stress.mjs",
);
export const phase1FormalHarnessIds = {
  scenarioId: sha256FileSync(scenarioPath),
  engineId: sha256FileSync(enginePath),
};
const defaultSha =
  process.env.GITHUB_SHA ?? process.env.MIDGARD_BUILD_GIT_SHA ?? "local";
const defaultResultsRoot = path.join(
  packageRoot,
  "benchmark-results",
  defaultSha,
);

const scenarioDefinitions = {
  "find-max-ramp": {
    scenarioClass: "A",
    description: "Discover the current admission/validation ceiling.",
    env: {
      STRESS_MODE: "find-max",
      STRESS_RAMP_START_TPS: "100",
      STRESS_RAMP_STEP_TPS: "100",
      STRESS_RAMP_MAX_TPS: "5000",
      STRESS_RAMP_STAGE_SEC: "15",
      STRESS_RAMP_MIN_ACCEPTED_RATIO: "0.99",
      STRESS_FIND_MAX_BINARY_ITERATIONS: "6",
      STRESS_FIND_MAX_REPEATS: "2",
      STRESS_FIND_MAX_MAX_CANDIDATES: "32",
      STRESS_CLIENT_SELF_CHECK_REQUIRED: "true",
    },
  },
  "accept-2500-tps-gate": {
    scenarioClass: "A",
    description: "Target gate for 2,500 accepted TPS.",
    env: {
      STRESS_MODE: "open",
      STRESS_TARGET_ACCEPTED_TPS: "2500",
      STRESS_OPEN_LOOP_RATE_TPS: "2500",
      STRESS_MEASURED_SEC: "300",
      STRESS_OFFERED_RATE_MIN_RATIO: "0.98",
      STRESS_ACCEPTED_RATE_MIN_RATIO: "0.99",
      STRESS_CLIENT_SELF_CHECK_REQUIRED: "true",
      STRESS_CLIENT_SELF_CHECK_MULTIPLIER: "2",
      STRESS_SCHEDULE_LAG_P95_MAX_MS: "100",
      STRESS_SCHEDULE_LAG_P99_MAX_MS: "250",
      STRESS_SUBMIT_LATENCY_P99_MAX_MS: "1000",
      STRESS_MISSED_START_MAX_RATIO: "0.001",
      STRESS_BACKLOG_SLOPE_MAX_PER_SEC: "0.1",
    },
  },
  "phase1-admission-5000-5min": {
    scenarioClass: "A",
    description:
      "Phase 1 formal five-minute 5,000 durable-admit/accepted TPS gate.",
    env: {
      STRESS_MODE: "open",
      STRESS_TARGET_ACCEPTED_TPS: "5000",
      STRESS_OPEN_LOOP_RATE_TPS: "5000",
      STRESS_MEASURED_SEC: "300",
      STRESS_OFFERED_RATE_MIN_RATIO: "0.98",
      STRESS_ACCEPTED_RATE_MIN_RATIO: "0.99",
      STRESS_CLIENT_SELF_CHECK_REQUIRED: "true",
      STRESS_CLIENT_SELF_CHECK_MULTIPLIER: "2",
      STRESS_SCHEDULE_LAG_P95_MAX_MS: "100",
      STRESS_SCHEDULE_LAG_P99_MAX_MS: "250",
      STRESS_SUBMIT_LATENCY_P99_MAX_MS: "1000",
      STRESS_MISSED_START_MAX_RATIO: "0.001",
      STRESS_BACKLOG_SLOPE_MAX_PER_SEC: "0.1",
      STRESS_FORMAL_BENCHMARK: "true",
    },
  },
  "soak-10min-at-max": {
    scenarioClass: "A",
    description:
      "Sustained 10-minute run at the discovered or explicitly supplied ceiling.",
    env: {
      STRESS_MODE: "open",
      STRESS_TARGET_ACCEPTED_TPS: "${STRESS_SOAK_TARGET_TPS:-1000}",
      STRESS_OPEN_LOOP_RATE_TPS: "${STRESS_SOAK_TARGET_TPS:-1000}",
      STRESS_MEASURED_SEC: "600",
      STRESS_WARMUP_SEC: "60",
      STRESS_COOLDOWN_SEC: "60",
      STRESS_DRAIN_TIMEOUT_SEC: "180",
      STRESS_ACCEPTED_RATE_MIN_RATIO: "0.99",
      STRESS_WAIT_FOR_COMMIT: "${STRESS_SOAK_WAIT_FOR_COMMIT:-false}",
    },
  },
  "phase1-starvation-2x-soak": {
    scenarioClass: "B",
    description:
      "Phase 1 single-stream gate: first five minutes prove 5,000 Stage-A TPS and the full ten-minute commit-enabled run proves starvation freedom at twice the 2,500 TPS program target.",
    env: {
      STRESS_MODE: "open",
      STRESS_CORPUS_SHAPE: "chain",
      STRESS_TARGET_ACCEPTED_TPS: "5000",
      STRESS_OPEN_LOOP_RATE_TPS: "5000",
      STRESS_MEASURED_SEC: "600",
      STRESS_WARMUP_TXS: "0",
      STRESS_WARMUP_SEC: "0",
      STRESS_COOLDOWN_SEC: "15",
      STRESS_DRAIN_TIMEOUT_SEC: "180",
      STRESS_ACCEPTED_RATE_MIN_RATIO: "0.99",
      STRESS_OFFERED_RATE_MIN_RATIO: "0.98",
      STRESS_SCHEDULE_LAG_P95_MAX_MS: "100",
      STRESS_SCHEDULE_LAG_P99_MAX_MS: "250",
      STRESS_SUBMIT_LATENCY_P99_MAX_MS: "1000",
      STRESS_MISSED_START_MAX_RATIO: "0.001",
      STRESS_CLIENT_SELF_CHECK: "true",
      STRESS_CLIENT_SELF_CHECK_REQUIRED: "true",
      STRESS_CLIENT_SELF_CHECK_MULTIPLIER: "2",
      STRESS_REQUIRE_METRIC_PRESENCE: "true",
      STRESS_MAX_CHAINS: "auto",
      STRESS_WAIT_FOR_COMMIT: "true",
      STRESS_PHASE1_STAGE_A_WINDOW_GATE: "true",
      STRESS_PHASE1_STAGE_A_WINDOW_SEC: "300",
      STRESS_PHASE1_STAGE_A_CHECKPOINT_MAX_JITTER_MS: "1000",
      STRESS_PHASE1_STARVATION_GATE: "true",
      STRESS_PHASE1_STARVATION_BASELINE_TPS: "2500",
      STRESS_PHASE1_STARVATION_MIN_OVERLOAD_RATIO: "2",
      STRESS_PHASE1_STARVATION_MIN_DURATION_SEC: "600",
      STRESS_PHASE1_STARVATION_MAX_AGE_MULTIPLIER: "3",
      STRESS_NODE_SATURATION_MIN_RATIO: "1",
      STRESS_FORMAL_BENCHMARK: "true",
    },
  },
  "burst-2x-target": {
    scenarioClass: "A",
    description: "Short 5,000 TPS burst to prove graceful overload behavior.",
    env: {
      STRESS_MODE: "open",
      STRESS_TARGET_ACCEPTED_TPS: "5000",
      STRESS_OPEN_LOOP_RATE_TPS: "5000",
      STRESS_MEASURED_SEC: "45",
      STRESS_WARMUP_SEC: "15",
      STRESS_COOLDOWN_SEC: "15",
      STRESS_REQUIRE_IDLE_NODE: "true",
      STRESS_IDLE_PROBE_SEC: "10",
    },
  },
  "mixed-workload-multi-io": {
    scenarioClass: "A",
    blockedUnless: "STRESS_MIXED_MULTI_IO_READY",
    blockedReport: {
      status: "blocked",
      reason:
        "mixed-multi-io corpus template is Phase 1 follow-up scope and is not yet available",
      rescheduleDate: "2026-07-16",
      trackingDoc: "docs/benchmark-scenarios/mixed-workload-multi-io.md",
    },
    description:
      "Ramp a realistic mixed multi-input/multi-output corpus once the template exists.",
    env: {
      STRESS_MODE: "ramp",
      STRESS_CORPUS_SHAPE: "mixed-multi-io",
      STRESS_RAMP_START_TPS: "100",
      STRESS_RAMP_STEP_TPS: "100",
      STRESS_RAMP_MAX_TPS: "${STRESS_MIXED_RAMP_MAX_TPS:-5000}",
      STRESS_RAMP_STAGE_SEC: "60",
      STRESS_MEASURED_SEC: "600",
      STRESS_ACCEPTED_RATE_MIN_RATIO: "0.99",
    },
  },
};

const usage = () => {
  const names = Object.keys(scenarioDefinitions).join("|");
  return `usage: benchmark-scenario.mjs <${names}> [--dry-run]\n`;
};

const expandValue = (value, env) =>
  value.replace(/\$\{([A-Z0-9_]+):-([^}]+)\}/g, (_match, name, fallback) =>
    env[name] === undefined || env[name] === "" ? fallback : env[name],
  );

export const buildScenarioEnvironment = ({
  scenarioName,
  baseEnv = process.env,
  resultsRoot = defaultResultsRoot,
}) => {
  const scenario = scenarioDefinitions[scenarioName];
  if (!scenario) {
    throw new Error(`unknown scenario '${scenarioName}'`);
  }
  const env = { ...baseEnv };
  env.STRESS_SCENARIO_NAME = scenarioName;
  env.STRESS_SCENARIO_CLASS = scenario.scenarioClass;
  env.STRESS_REPORT_PATH ??= path.join(resultsRoot, `${scenarioName}.json`);
  env.STRESS_ENV_FILE ??= "scripts/stress.benchmark.env";
  const formalScenario = scenario.env.STRESS_FORMAL_BENCHMARK === "true";
  for (const [name, value] of Object.entries(scenario.env)) {
    const expectedValue = expandValue(value, env);
    if (
      formalScenario &&
      env[name] !== undefined &&
      env[name] !== expectedValue
    ) {
      throw new Error(
        `formal scenario '${scenarioName}' requires ${name}=${expectedValue}; received ${env[name]}`,
      );
    }
    env[name] ??= expectedValue;
  }
  if (scenarioName === PHASE1_FORMAL_SCENARIO) {
    for (const [name, expectedValue] of Object.entries({
      STRESS_PHASE1_SCENARIO_HARNESS_ID: phase1FormalHarnessIds.scenarioId,
      STRESS_PHASE1_ENGINE_HARNESS_ID: phase1FormalHarnessIds.engineId,
    })) {
      if (env[name] !== undefined && env[name] !== expectedValue) {
        throw new Error(
          `formal scenario '${scenarioName}' requires ${name}=${expectedValue}; received ${env[name]}`,
        );
      }
      env[name] = expectedValue;
    }
    const binding = loadPhase1FormalBindingSync(env.STRESS_PHASE1_BINDING_PATH);
    validatePhase1BindingEnvironment({
      binding,
      env,
      scenarioId: phase1FormalHarnessIds.scenarioId,
      engineId: phase1FormalHarnessIds.engineId,
    });
  }
  return { scenario, env };
};

const writeBlockedReport = ({ scenarioName, scenario, env }) => {
  const reportPath = env.STRESS_REPORT_PATH;
  fs.mkdirSync(path.dirname(reportPath), { recursive: true });
  fs.writeFileSync(
    reportPath,
    `${JSON.stringify(
      {
        benchmark: "midgard-l2-throughput",
        scenario: scenarioName,
        scenarioClass: scenario.scenarioClass,
        generatedAtIso: new Date().toISOString(),
        ...scenario.blockedReport,
      },
      null,
      2,
    )}\n`,
  );
};

const main = async () => {
  const args = process.argv.slice(2);
  const dryRun = args.includes("--dry-run");
  const scenarioName = args.find((arg) => !arg.startsWith("--"));
  if (!scenarioName || scenarioName === "help") {
    process.stdout.write(usage());
    process.exit(scenarioName ? 0 : 2);
  }

  const { scenario, env } = buildScenarioEnvironment({ scenarioName });
  if (scenario.blockedUnless && env[scenario.blockedUnless] !== "1") {
    writeBlockedReport({ scenarioName, scenario, env });
    process.stderr.write(
      `${scenarioName} blocked: ${scenario.blockedReport.reason}; reschedule ${scenario.blockedReport.rescheduleDate}\n`,
    );
    process.exit(78);
  }

  if (dryRun) {
    process.stdout.write(
      `${JSON.stringify(
        {
          scenario: scenarioName,
          description: scenario.description,
          reportPath: env.STRESS_REPORT_PATH,
          scenarioClass: scenario.scenarioClass,
          env: Object.fromEntries(
            Object.entries(env)
              .filter(([name]) => name.startsWith("STRESS_"))
              .sort(([a], [b]) => a.localeCompare(b)),
          ),
        },
        null,
        2,
      )}\n`,
    );
    return;
  }

  fs.mkdirSync(path.dirname(env.STRESS_REPORT_PATH), { recursive: true });
  const child = spawn(process.execPath, [enginePath], {
    cwd: packageRoot,
    env,
    stdio: "inherit",
  });
  child.on("exit", (code, signal) => {
    if (signal) {
      process.kill(process.pid, signal);
      return;
    }
    process.exit(code ?? 1);
  });
};

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    process.stderr.write(`${error.stack ?? error.message}\n`);
    process.exit(1);
  });
}
