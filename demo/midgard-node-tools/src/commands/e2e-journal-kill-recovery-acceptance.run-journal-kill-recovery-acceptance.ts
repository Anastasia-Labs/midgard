import { mkdir, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { type Network, walletFromSeed } from "@lucid-evolution/lucid";

import { runJournalKillContention } from "../e2e/journal-kill-process-harness.js";
import { requiredIsolatedChildEnv } from "./e2e-journal-kill-recovery-acceptance.decode-phase4-reset-attestation.js";
import {
  assertAcceptancePreconditions,
  initializeProcessOwnership,
} from "./e2e-journal-kill-recovery-acceptance.load-phase4-process-isolation.js";
import {
  makeNodeSpec,
  resetAndPreflight,
} from "./e2e-journal-kill-recovery-acceptance.reset-and-preflight.js";
import { seedBaseAndPayload } from "./e2e-journal-kill-recovery-acceptance.seed-base-and-payload.js";
import { toolsPackageRoot } from "./e2e-journal-kill-recovery-acceptance.validate-phase4-phas-registration-transaction-body.js";
import {
  captureDatabaseState,
  positiveIntegerEnv,
  processAcceptanceTimeoutMs,
} from "./e2e-journal-kill-recovery-acceptance.validate-phase4-reset-attestation.js";

/**
 * Runs the offline summary verifier on the evidence before it is written, so
 * a run never persists a summary its own verifier would reject.
 */
const decodeSummaryBeforePersistence = async (
  value: unknown,
): Promise<void> => {
  const moduleUrl = pathToFileURL(
    resolve(
      toolsPackageRoot(),
      "scripts/verify-phase4-journal-kill-recovery-summary.mjs",
    ),
  );
  const verifier = (await import(moduleUrl.href)) as {
    readonly decodePhase4JournalKillRecoverySummaryV1: (
      summary: unknown,
    ) => unknown;
  };
  verifier.decodePhase4JournalKillRecoverySummaryV1(value);
};

/**
 * `cwd` is the midgard-node package root: the operator `dist/index.js` under
 * test, its `logs/`, and the isolation env all resolve from there. This
 * tooling package's own assets resolve from `toolsPackageRoot()` instead.
 */
export const runJournalKillRecoveryAcceptance = async ({
  cwd = process.cwd(),
}: {
  readonly cwd?: string;
} = {}): Promise<Readonly<Record<string, unknown>>> => {
  const isolation = await assertAcceptancePreconditions(cwd);
  const runDir = resolve(
    cwd,
    process.env.MIDGARD_PHASE4_PROCESS_RUN_DIR ??
      `logs/phase4-journal-kill-${new Date().toISOString().replaceAll(/[:.]/g, "-")}`,
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
      "Phase 4 journal-kill acceptance requires two distinct funded L2 wallets",
    );
  }
  if (new Set([nodeAPort, nodeBPort, metricsAPort, metricsBPort]).size !== 4) {
    throw new Error(
      "Phase 4 journal-kill acceptance node and metrics ports must all be distinct",
    );
  }

  const label = "journal-kill-contention";
  await resetAndPreflight({ cwd, label });
  await seedBaseAndPayload({
    cwd,
    runDir,
    label,
    port: nodeAPort,
    metricsPort: metricsAPort,
    addressA,
    addressB,
  });
  const journalKillContention = await runJournalKillContention({
    left: makeNodeSpec({
      cwd,
      runDir,
      label,
      nodeId: "node-a",
      port: nodeAPort,
      metricsPort: metricsAPort,
      timeoutMs: processAcceptanceTimeoutMs(),
    }),
    right: makeNodeSpec({
      cwd,
      runDir,
      label,
      nodeId: "node-b",
      port: nodeBPort,
      metricsPort: metricsBPort,
      timeoutMs: processAcceptanceTimeoutMs(),
    }),
    armFile: join(runDir, label, "journal.arm"),
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
    schemaVersion: "midgard-phase4-journal-kill-recovery-acceptance-v1",
    mode: "attach-resume-matched-local-devnet-snapshot",
    runDir,
    isolation,
    journalKillContention,
    journalKillContentionState,
  } as const;
  const evidencePath = join(runDir, "summary.json");
  await decodeSummaryBeforePersistence(evidence);
  await writeFile(
    evidencePath,
    `${JSON.stringify(evidence, null, 2)}\n`,
    "utf8",
  );
  return { ...evidence, evidencePath };
};
