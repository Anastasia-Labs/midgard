import { join, resolve } from "node:path";

import { type JournalKillNodeProcessSpec } from "../e2e/journal-kill-process-harness.js";
import {
  isolatedChildEnv,
  ownershipForProcess,
  requiredIsolatedChildEnv,
} from "./e2e-journal-kill-recovery-acceptance.decode-phase4-reset-attestation.js";
import { activeProcessIsolation } from "./e2e-journal-kill-recovery-acceptance.load-phase4-process-isolation.js";
import { toolsCli } from "./e2e-journal-kill-recovery-acceptance.validate-phase4-phas-registration-transaction-body.js";
import { requiredEnv } from "./e2e-journal-kill-recovery-acceptance.validate-phase4-process-isolation-values.js";
import {
  assertExactGenesisPreflightOutput,
  assertPositivePreflightOutput,
  positiveIntegerEnv,
  runRequiredProcess,
  validatePhase4ResetAttestation,
} from "./e2e-journal-kill-recovery-acceptance.validate-phase4-reset-attestation.js";
import {
  PHASE4_GENESIS_BOOTSTRAP_ENV,
  PHASE4_GENESIS_BOOTSTRAP_TOKEN,
  PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE,
} from "./phase4-genesis-ledger.js";

const scenarioProcessEnv = ({
  port,
  metricsPort,
  commitIntervalMs = 250,
  confirmationIntervalMs = 250,
}: {
  readonly port: number;
  readonly metricsPort: number;
  readonly commitIntervalMs?: number;
  readonly confirmationIntervalMs?: number;
}): Readonly<Record<string, string | undefined>> => ({
  ...isolatedChildEnv(),
  POSTGRES_HOST: requiredIsolatedChildEnv("POSTGRES_HOST"),
  POSTGRES_PORT: requiredIsolatedChildEnv("POSTGRES_PORT"),
  PORT: port.toString(),
  PROM_METRICS_PORT: metricsPort.toString(),
  WAIT_BETWEEN_BLOCK_COMMITMENT: commitIntervalMs.toString(),
  WAIT_BETWEEN_BLOCK_CONFIRMATION: confirmationIntervalMs.toString(),
  BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS: "2000",
  BLOCK_CONFIRMATION_AWAIT_RETRIES: "1",
  MIDGARD_DA_PAYLOAD_ENVELOPE: "off",
  MIDGARD_DEPLOYMENT_MANIFEST_PATH: requiredIsolatedChildEnv(
    "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  ),
  MIDGARD_DA_DEPLOYMENT_MANIFEST_PATH: undefined,
  RUN_GENESIS_ON_STARTUP: "false",
});

export const makeNodeSpec = ({
  cwd,
  runDir,
  label,
  nodeId,
  port,
  metricsPort,
  timeoutMs,
  commitIntervalMs,
  confirmationIntervalMs,
}: {
  readonly cwd: string;
  readonly runDir: string;
  readonly label: string;
  readonly nodeId: string;
  readonly port: number;
  readonly metricsPort: number;
  readonly timeoutMs: number;
  readonly commitIntervalMs?: number;
  readonly confirmationIntervalMs?: number;
}): JournalKillNodeProcessSpec => {
  const ttlMs = positiveIntegerEnv(
    "MIDGARD_PHASE4_STATE_QUEUE_LEASE_TTL_MS",
    5_000,
  );
  return {
    nodeId,
    postgresIdentity: [
      requiredIsolatedChildEnv("POSTGRES_HOST"),
      requiredIsolatedChildEnv("POSTGRES_PORT"),
      requiredIsolatedChildEnv("POSTGRES_DB"),
    ].join(":"),
    ledgerMpfDbPath: join(runDir, label, nodeId, "ledger-mpf"),
    transactionsMpfDbPath: join(runDir, label, nodeId, "transactions-mpf"),
    stateQueueMutationLeaseTtlMs: ttlMs,
    process: {
      service: "midgard-node-listen",
      command: process.execPath,
      args: [resolve(cwd, "dist/index.js"), "listen"],
      cwd,
      envInheritance: "none",
      env: scenarioProcessEnv({
        port,
        metricsPort,
        commitIntervalMs,
        confirmationIntervalMs,
      }),
      rawLogPath: join(runDir, label, `${nodeId}.log`),
      timeoutMs,
      ownership: ownershipForProcess(label, nodeId),
    },
  };
};

export const submitL2Transfer = async ({
  cwd,
  port,
  walletSeedEnv,
  destination,
}: {
  readonly cwd: string;
  readonly port: number;
  readonly walletSeedEnv: string;
  readonly destination: string;
}): Promise<string> =>
  runRequiredProcess(
    {
      command: process.execPath,
      args: [
        resolve(cwd, "dist/index.js"),
        "submit-l2-transfer",
        "--wallet-seed-phrase-env",
        walletSeedEnv,
        "--endpoint",
        `http://127.0.0.1:${port.toString()}`,
        "--l2-address",
        destination,
        "--lovelace",
        positiveIntegerEnv(
          "MIDGARD_PHASE4_TRANSFER_LOVELACE",
          Number(PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE),
        ).toString(),
      ],
      cwd,
      env: isolatedChildEnv(),
    },
    `submit L2 transfer from ${walletSeedEnv}`,
  );

export const resetAndPreflight = async ({
  cwd,
  label,
}: {
  readonly cwd: string;
  readonly label: string;
}): Promise<void> => {
  const resetCommand = requiredEnv("MIDGARD_PHASE4_MATCHED_RESET_COMMAND");
  if (activeProcessIsolation === undefined) {
    throw new Error("Phase 4 process isolation identity is not initialized");
  }
  const resetOutput = await runRequiredProcess(
    {
      command: "/bin/bash",
      args: ["-lc", resetCommand],
      cwd,
      env: {
        ...isolatedChildEnv(),
        MIDGARD_PHASE4_SCENARIO_LABEL: label,
      },
    },
    `matched devnet+Postgres reset for ${label}`,
  );
  validatePhase4ResetAttestation({
    output: resetOutput,
    scenarioLabel: label,
    isolation: activeProcessIsolation,
  });
  const dist = resolve(cwd, "dist/index.js");
  const env = isolatedChildEnv();
  const genesisOutput = await runRequiredProcess(
    {
      command: process.execPath,
      // The genesis gate is tooling, so it runs through this package's own
      // binary; the node under test still runs from the operator dist.
      args: [toolsCli(), "phase4-genesis-ledger", "--verify-only"],
      cwd,
      env: {
        ...env,
        [PHASE4_GENESIS_BOOTSTRAP_ENV]: PHASE4_GENESIS_BOOTSTRAP_TOKEN,
        MIDGARD_PHASE4_PROCESS_TARGET: "local-devnet",
      },
    },
    `${label}: A/B genesis ledger`,
  );
  assertExactGenesisPreflightOutput(
    `${label}: A/B genesis ledger`,
    genesisOutput,
  );
  const preflights: readonly [string, readonly string[]][] = [
    ["deployment status", [dist, "deployment-status"]],
    [
      "reference-script wallet",
      [dist, "reference-script-wallet-status", "--json"],
    ],
    ["local Kupmios", [dist, "l1-provider-preflight", "--json"]],
    [
      "node-runtime reference scripts",
      [
        dist,
        "reconcile",
        "reference-scripts-complete",
        "--scope",
        "node-runtime",
        "--json",
      ],
    ],
  ];
  for (const [preflightLabel, args] of preflights) {
    const output = await runRequiredProcess(
      { command: process.execPath, args, cwd, env },
      `${label}: ${preflightLabel}`,
    );
    assertPositivePreflightOutput(`${label}: ${preflightLabel}`, output);
  }
};
