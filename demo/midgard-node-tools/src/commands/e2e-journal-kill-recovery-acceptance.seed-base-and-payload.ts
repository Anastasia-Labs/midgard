import { mkdir, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import { BLOCK_SUBMITTED_MARKER } from "../e2e/journal-kill-process-harness.js";
import { superviseHostProcess } from "../e2e/service-supervisor.js";
import {
  makeNodeSpec,
  submitL2Transfer,
} from "./e2e-journal-kill-recovery-acceptance.reset-and-preflight.js";
import {
  captureDatabaseState,
  processAcceptanceTimeoutMs,
  waitForLogMarker,
  waitForNewPayloadRetention,
  waitForReady,
} from "./e2e-journal-kill-recovery-acceptance.validate-phase4-reset-attestation.js";

/**
 * Commits base block N from wallet A on one seed node, then submits a wallet-B
 * transfer that stays retained for N+1, and stops the seed node. The
 * journal-kill contention then races two nodes for that N+1 payload.
 */
export const seedBaseAndPayload = async ({
  cwd,
  runDir,
  label,
  port,
  metricsPort,
  addressA,
  addressB,
}: {
  readonly cwd: string;
  readonly runDir: string;
  readonly label: string;
  readonly port: number;
  readonly metricsPort: number;
  readonly addressA: string;
  readonly addressB: string;
}): Promise<string> => {
  const seedSpec = makeNodeSpec({
    cwd,
    runDir,
    label: `${label}-seed-base`,
    nodeId: "seed-node",
    port,
    metricsPort,
    timeoutMs: processAcceptanceTimeoutMs(),
    confirmationIntervalMs: 600_000,
  });
  const stopFile = join(runDir, label, "seed.stop");
  const basePromise = superviseHostProcess({
    ...seedSpec.process,
    env: {
      ...seedSpec.process.env,
      LEDGER_MPF_DB_PATH: seedSpec.ledgerMpfDbPath,
      TRANSACTIONS_MPF_DB_PATH: seedSpec.transactionsMpfDbPath,
      STATE_QUEUE_MUTATION_LEASE_TTL_MS:
        seedSpec.stateQueueMutationLeaseTtlMs.toString(),
      STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Math.max(
        1,
        Math.floor(seedSpec.stateQueueMutationLeaseTtlMs / 3),
      ).toString(),
    },
    maxRestarts: 0,
    terminateOnFile: { path: stopFile, signal: "SIGTERM" },
  });
  let baseHeaderHash: string | undefined;
  let scenarioFailure: unknown;
  try {
    await waitForReady(port, 60_000);
    await submitL2Transfer({
      cwd,
      port,
      walletSeedEnv: "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
      destination: addressB,
    });
    await waitForLogMarker(
      seedSpec.process.rawLogPath,
      BLOCK_SUBMITTED_MARKER,
      120_000,
    );
    const baseState = await captureDatabaseState();
    if (baseState.activeJournal?.submittedTxHash === null) {
      throw new Error(
        `${label}: submitted base journal is missing its tx hash`,
      );
    }
    baseHeaderHash = baseState.activeJournal?.headerHash;
    if (baseHeaderHash === undefined) {
      throw new Error(
        `${label}: base block N did not create an active journal`,
      );
    }
    const existingTxIds = new Set(
      [...baseState.mempool, ...baseState.processed].map((entry) => entry.txId),
    );
    await submitL2Transfer({
      cwd,
      port,
      walletSeedEnv: "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
      destination: addressA,
    });
    await waitForNewPayloadRetention(existingTxIds, 30_000);
  } catch (error) {
    scenarioFailure = error;
  } finally {
    await mkdir(dirname(stopFile), { recursive: true });
    await writeFile(stopFile, "stop\n", { encoding: "utf8", flag: "wx" }).catch(
      (error: unknown) => {
        if (
          typeof error !== "object" ||
          error === null ||
          !("code" in error) ||
          error.code !== "EEXIST"
        ) {
          throw error;
        }
      },
    );
  }
  const baseSummary = await basePromise;
  if (scenarioFailure !== undefined) {
    throw scenarioFailure instanceof Error
      ? scenarioFailure
      : new Error("Journal-kill seed scenario failed", {
          cause: scenarioFailure,
        });
  }
  if (baseSummary.attempts[0]?.fileTermination?.path !== stopFile) {
    throw new Error(
      `${label}: seed node was not stopped by the external stop-file supervisor`,
    );
  }
  if (baseHeaderHash === undefined) {
    throw new Error(`${label}: base header evidence was not captured`);
  }
  return baseHeaderHash;
};
