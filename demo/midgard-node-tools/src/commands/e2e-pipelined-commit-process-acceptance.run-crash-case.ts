import { mkdir, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import type { PipelinedCommitCrashCheckpoint } from "midgard-node/e2e/pipelined-commit-crash-checkpoint";

import {
  assertNoJournalBeyondBase,
  restartPipelinedCommitNodeUntilFreshCandidate,
  restartPipelinedCommitNodeUntilSubmission,
  runPipelinedCommitCheckpointCrash,
  runPipelinedCommitFlagOffControl,
} from "../e2e/pipelined-commit-process-harness.js";
import { superviseHostProcess } from "../e2e/service-supervisor.js";
import { type CrashAcceptanceEvidence } from "./e2e-pipelined-commit-process-acceptance.decode-phase4-reset-attestation.js";
import {
  assertSameLogicalDatabaseState,
  makeNodeSpec,
  resetAndPreflight,
  submitL2Transfer,
} from "./e2e-pipelined-commit-process-acceptance.reset-and-preflight.js";
import { BLOCK_SUBMITTED_MARKER } from "./e2e-pipelined-commit-process-acceptance.validate-phase4-process-isolation-values.js";
import {
  captureDatabaseState,
  processAcceptanceTimeoutMs,
  waitForLogMarker,
  waitForNewPayloadRetention,
  waitForReady,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-reset-attestation.js";

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
      SPECULATIVE_COMMIT_BUILD: "false",
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
      : new Error("Pipelined commit scenario failed", {
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

export const runCrashCase = async ({
  cwd,
  runDir,
  checkpoint,
  port,
  metricsPort,
  addressA,
  addressB,
}: {
  readonly cwd: string;
  readonly runDir: string;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly port: number;
  readonly metricsPort: number;
  readonly addressA: string;
  readonly addressB: string;
}): Promise<CrashAcceptanceEvidence> => {
  const label = `crash-${checkpoint}`;
  await resetAndPreflight({ cwd, label: `${label}-flag-on` });
  const baseHeaderHash = await seedBaseAndPayload({
    cwd,
    runDir,
    label: `${label}-flag-on`,
    port,
    metricsPort,
    addressA,
    addressB,
  });
  const armFile = join(runDir, label, `${checkpoint}.arm`);
  const durableNodeIdentity = join(label, "node-a");
  const crashSpec = makeNodeSpec({
    cwd,
    runDir,
    label: `${label}-crash`,
    durableStoreLabel: durableNodeIdentity,
    nodeId: "node-a",
    port,
    metricsPort,
    timeoutMs: processAcceptanceTimeoutMs(),
  });
  const crash = await runPipelinedCommitCheckpointCrash({
    spec: crashSpec,
    checkpoint,
    armFile,
  });
  const afterCrash = await captureDatabaseState();
  assertNoJournalBeyondBase(afterCrash, baseHeaderHash);

  const restartReady = await restartPipelinedCommitNodeUntilFreshCandidate({
    // Keep the exact same node identity and durable MPF stores.  A restart
    // label is only a log namespace; deriving a new store here would turn
    // crash recovery into a fresh node and miss corruption/data-loss bugs.
    spec: {
      ...crashSpec,
      process: {
        ...crashSpec.process,
        rawLogPath: join(runDir, `${label}-restart-ready`, "node-a.log"),
      },
    },
    checkpoint,
    consumedArmFile: armFile,
  });
  const afterRestartReady = await captureDatabaseState();
  assertNoJournalBeyondBase(afterRestartReady, baseHeaderHash);

  const restartSubmitted = await restartPipelinedCommitNodeUntilSubmission({
    spec: {
      ...crashSpec,
      process: {
        ...crashSpec.process,
        rawLogPath: join(runDir, `${label}-restart-submit`, "node-a.log"),
      },
    },
    checkpoint,
    consumedArmFile: armFile,
  });
  const flagOnSubmitted = await captureDatabaseState();

  await resetAndPreflight({ cwd, label: `${label}-flag-off-control` });
  await seedBaseAndPayload({
    cwd,
    runDir,
    label: `${label}-flag-off-control`,
    port,
    metricsPort,
    addressA,
    addressB,
  });
  await runPipelinedCommitFlagOffControl({
    spec: makeNodeSpec({
      cwd,
      runDir,
      label: `${label}-flag-off-control-submit`,
      nodeId: "control-node",
      port,
      metricsPort,
      timeoutMs: processAcceptanceTimeoutMs(),
    }),
    stopMarker: BLOCK_SUBMITTED_MARKER,
  });
  const flagOffControl = await captureDatabaseState();
  assertSameLogicalDatabaseState({
    checkpoint,
    flagOn: flagOnSubmitted,
    flagOff: flagOffControl,
  });
  return {
    checkpoint,
    baseHeaderHash,
    crash,
    restartReady,
    restartSubmitted,
    afterCrash,
    afterRestartReady,
    flagOnSubmitted,
    flagOffControl,
  };
};
