import { mkdir, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import { type Network, walletFromSeed } from "@lucid-evolution/lucid";

import {
  runPipelinedCommitLeaseContention,
  runPipelinedCommitNormalLeaseContention,
} from "../e2e/pipelined-commit-process-harness.js";
import {
  type CrashAcceptanceEvidence,
  requiredIsolatedChildEnv,
} from "./e2e-pipelined-commit-process-acceptance.decode-phase4-reset-attestation.js";
import {
  assertAcceptancePreconditions,
  initializeProcessOwnership,
} from "./e2e-pipelined-commit-process-acceptance.load-phase4-process-isolation.js";
import {
  makeNodeSpec,
  resetAndPreflight,
} from "./e2e-pipelined-commit-process-acceptance.reset-and-preflight.js";
import {
  runCrashCase,
  seedBaseAndPayload,
} from "./e2e-pipelined-commit-process-acceptance.run-crash-case.js";
import {
  decodeProcessSummaryBeforePersistence,
  runT1RecoveryCase,
} from "./e2e-pipelined-commit-process-acceptance.run-t1-recovery-case.js";
import { CRASH_MATRIX_CHECKPOINTS } from "./e2e-pipelined-commit-process-acceptance.validate-phase4-process-isolation-values.js";
import {
  captureDatabaseState,
  positiveIntegerEnv,
  processAcceptanceTimeoutMs,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-reset-attestation.js";

/**
 * `cwd` is the midgard-node package root: the operator `dist/index.js` under
 * test, its `logs/`, and the isolation env all resolve from there. This
 * tooling package's own assets resolve from `toolsPackageRoot()` instead.
 */
export const runPipelinedCommitProcessAcceptance = async ({
  cwd = process.cwd(),
}: {
  readonly cwd?: string;
} = {}): Promise<Readonly<Record<string, unknown>>> => {
  const isolation = await assertAcceptancePreconditions(cwd);
  const runDir = resolve(
    cwd,
    process.env.MIDGARD_PHASE4_PROCESS_RUN_DIR ??
      `logs/phase4-process-${new Date().toISOString().replaceAll(/[:.]/g, "-")}`,
  );
  await mkdir(runDir, { recursive: true });
  await initializeProcessOwnership(runDir);
  const nodeAPort = positiveIntegerEnv("MIDGARD_PHASE4_NODE_A_PORT", 3101);
  const nodeBPort = positiveIntegerEnv("MIDGARD_PHASE4_NODE_B_PORT", 3102);
  const metricsAPort = positiveIntegerEnv(
    "MIDGARD_PHASE4_NODE_A_METRICS_PORT",
    4101,
  );
  const metricsBPort = positiveIntegerEnv(
    "MIDGARD_PHASE4_NODE_B_METRICS_PORT",
    4102,
  );
  const network = requiredIsolatedChildEnv("NETWORK") as Network;
  const addressA = walletFromSeed(
    requiredIsolatedChildEnv("TESTNET_GENESIS_WALLET_SEED_PHRASE_A"),
    { network },
  ).address;
  const addressB = walletFromSeed(
    requiredIsolatedChildEnv("TESTNET_GENESIS_WALLET_SEED_PHRASE_B"),
    { network },
  ).address;
  if (addressA === addressB) {
    throw new Error(
      "Phase 4 process acceptance requires two distinct funded L2 wallets",
    );
  }
  if (new Set([nodeAPort, nodeBPort, metricsAPort, metricsBPort]).size !== 4) {
    throw new Error(
      "Phase 4 process acceptance node and metrics ports must all be distinct",
    );
  }

  const crashes: CrashAcceptanceEvidence[] = [];
  for (const checkpoint of CRASH_MATRIX_CHECKPOINTS) {
    crashes.push(
      await runCrashCase({
        cwd,
        runDir,
        checkpoint,
        port: nodeAPort,
        metricsPort: metricsAPort,
        addressA,
        addressB,
      }),
    );
  }

  const t1Recovery = await runT1RecoveryCase({
    cwd,
    runDir,
    port: nodeAPort,
    metricsPort: metricsAPort,
    addressA,
    addressB,
  });

  await resetAndPreflight({ cwd, label: "normal-lease-contention" });
  await seedBaseAndPayload({
    cwd,
    runDir,
    label: "normal-lease-contention",
    port: nodeAPort,
    metricsPort: metricsAPort,
    addressA,
    addressB,
  });
  const normalContention = await runPipelinedCommitNormalLeaseContention({
    left: makeNodeSpec({
      cwd,
      runDir,
      label: "normal-contention",
      nodeId: "node-a",
      port: nodeAPort,
      metricsPort: metricsAPort,
      timeoutMs: processAcceptanceTimeoutMs(),
    }),
    right: makeNodeSpec({
      cwd,
      runDir,
      label: "normal-contention",
      nodeId: "node-b",
      port: nodeBPort,
      metricsPort: metricsBPort,
      timeoutMs: processAcceptanceTimeoutMs(),
    }),
  });
  const normalContentionState = await captureDatabaseState();
  if (normalContentionState.activeJournalCount !== 1) {
    throw new Error(
      `Normal contention must leave exactly one active journal; observed ${normalContentionState.activeJournalCount.toString()}`,
    );
  }

  await resetAndPreflight({ cwd, label: "journal-kill-contention" });
  await seedBaseAndPayload({
    cwd,
    runDir,
    label: "journal-kill-contention",
    port: nodeAPort,
    metricsPort: metricsAPort,
    addressA,
    addressB,
  });
  const journalKillContention = await runPipelinedCommitLeaseContention({
    left: makeNodeSpec({
      cwd,
      runDir,
      label: "journal-kill-contention",
      nodeId: "node-a",
      port: nodeAPort,
      metricsPort: metricsAPort,
      timeoutMs: processAcceptanceTimeoutMs(),
    }),
    right: makeNodeSpec({
      cwd,
      runDir,
      label: "journal-kill-contention",
      nodeId: "node-b",
      port: nodeBPort,
      metricsPort: metricsBPort,
      timeoutMs: processAcceptanceTimeoutMs(),
    }),
    armFile: join(runDir, "journal-kill-contention", "journal.arm"),
  });
  const journalKillContentionState = await captureDatabaseState();
  if (journalKillContentionState.activeJournalCount !== 1) {
    throw new Error(
      `Journal-kill recovery must leave exactly one survivor journal; observed ${journalKillContentionState.activeJournalCount.toString()}`,
    );
  }
  if (
    !journalKillContentionState.recentLeases.some(
      (lease) =>
        lease.status === "failed" &&
        lease.lastError === "lease expired before release",
    )
  ) {
    throw new Error(
      "Journal-kill recovery did not retain evidence that the killed winner lease expired",
    );
  }

  const evidence = {
    schemaVersion: "midgard-phase4-pipelined-commit-process-acceptance-v1",
    mode: "attach-resume-matched-local-devnet-snapshot",
    runDir,
    checkpoints: CRASH_MATRIX_CHECKPOINTS,
    isolation,
    crashes,
    t1Recovery,
    normalContention,
    normalContentionState,
    journalKillContention,
    journalKillContentionState,
  } as const;
  const evidencePath = join(runDir, "summary.json");
  await decodeProcessSummaryBeforePersistence(evidence);
  await writeFile(
    evidencePath,
    `${JSON.stringify(evidence, null, 2)}\n`,
    "utf8",
  );
  return { ...evidence, evidencePath };
};
