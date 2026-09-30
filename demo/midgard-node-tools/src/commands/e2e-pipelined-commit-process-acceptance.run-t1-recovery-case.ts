import { realpath } from "node:fs/promises";
import { isAbsolute, join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { runPipelinedCommitNodeUntilMarker } from "../e2e/pipelined-commit-process-harness.js";
import {
  activeJournalIdentity,
  isolatedChildEnv,
  journalHeaderEndTimeMs,
  journalTransactionSourceIds,
  logByteLength,
  readLogAttempt,
  retainedPayloadIdentity,
  retainedPayloadTxIds,
  type T1RecoveryAcceptanceEvidence,
} from "./e2e-pipelined-commit-process-acceptance.decode-phase4-reset-attestation.js";
import { activeProcessIsolation } from "./e2e-pipelined-commit-process-acceptance.load-phase4-process-isolation.js";
import {
  makeNodeSpec,
  resetAndPreflight,
} from "./e2e-pipelined-commit-process-acceptance.reset-and-preflight.js";
import { seedBaseAndPayload } from "./e2e-pipelined-commit-process-acceptance.run-crash-case.js";
import { toolsPackageRoot } from "./e2e-pipelined-commit-process-acceptance.validate-phase4-phas-registration-transaction-body.js";
import {
  ACCEPTANCE_ENABLE_VALUE,
  requiredEnv,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-process-isolation-values.js";
import {
  captureDatabaseState,
  processAcceptanceTimeoutMs,
  runRequiredProcess,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-reset-attestation.js";
import {
  assertPhase4T1ReplacementAttemptOrdering,
  parseAndValidatePhase4T1RecoveryAttestation,
  PHASE4_T1_ACCEPTANCE_TOKEN,
  requireCardanoHash,
  requireL2HeaderHash,
  requirePhase4T1CandidateLine,
} from "./phase4-t1-recovery.js";

/** Executes the real matched-snapshot T1 rollback/advance/rebuild gate. */
export const runT1RecoveryCase = async ({
  cwd,
  runDir,
  port,
  metricsPort,
  addressA,
  addressB,
}: {
  readonly cwd: string;
  readonly runDir: string;
  readonly port: number;
  readonly metricsPort: number;
  readonly addressA: string;
  readonly addressB: string;
}): Promise<T1RecoveryAcceptanceEvidence> => {
  const label = "t1-recovered-tip";
  await resetAndPreflight({ cwd, label });
  const abandonedHeaderHash = requireL2HeaderHash(
    await seedBaseAndPayload({
      cwd,
      runDir,
      label,
      port,
      metricsPort,
      addressA,
      addressB,
    }),
    "submitted L2 header N",
  );
  const afterSeedState = await captureDatabaseState();
  if (
    afterSeedState.activeJournal === null ||
    afterSeedState.activeJournal.headerHash !== abandonedHeaderHash ||
    afterSeedState.activeJournal.submittedTxHash === null ||
    afterSeedState.activeJournal.baseTailHeaderHash === null
  ) {
    throw new Error(
      "T1 seed did not retain one submitted N journal and its base B",
    );
  }
  const abandonedSubmittedTxHash = requireCardanoHash(
    afterSeedState.activeJournal.submittedTxHash,
    "submitted Cardano transaction for N",
  );
  const originalBaseHeaderHash = requireL2HeaderHash(
    afterSeedState.activeJournal.baseTailHeaderHash,
    "original canonical L2 base B",
  );
  const abandonedHeaderEndTimeMs = journalHeaderEndTimeMs(afterSeedState);

  // Confirmation is deliberately held during the pre-recovery speculative
  // attempt. Otherwise a fast local chain can finalize N before its exact
  // candidate evidence is captured, turning T1 into an ordinary confirmation.
  const durableStoreLabel = `${label}-durable`;
  const preRecoverySpec = makeNodeSpec({
    cwd,
    runDir,
    label: `${label}-pre-recovery-attempt`,
    durableStoreLabel,
    nodeId: "node-a",
    port,
    metricsPort,
    timeoutMs: processAcceptanceTimeoutMs(),
    confirmationIntervalMs: 600_000,
  });
  const preRecoveryLogOffset = await logByteLength(
    preRecoverySpec.process.rawLogPath,
  );
  const ready = await runPipelinedCommitNodeUntilMarker({
    spec: preRecoverySpec,
    marker: "pipeline_trace phase=candidate_ready",
    suffix: "candidate-ready-before-t1",
  });
  if (
    ready.attempts[0]?.outputTermination?.marker !==
    "pipeline_trace phase=candidate_ready"
  ) {
    throw new Error("T1 gate did not stop at candidate-ready before recovery");
  }
  const preRecoveryAttemptLog = await readLogAttempt(
    preRecoverySpec.process.rawLogPath,
    preRecoveryLogOffset,
  );
  const preRecoveryCandidateLine = requirePhase4T1CandidateLine({
    attemptLog: preRecoveryAttemptLog,
    baseHeaderHash: abandonedHeaderHash,
    label: `candidate built on submitted N ${abandonedHeaderHash}`,
  });
  const preRecoveryState = await captureDatabaseState();
  if (
    preRecoveryState.activeJournal === null ||
    preRecoveryState.activeJournal.headerHash !== abandonedHeaderHash ||
    preRecoveryState.activeJournal.submittedTxHash !== abandonedSubmittedTxHash
  ) {
    throw new Error(
      "T1 pre-recovery confirmation hold failed: N was finalized or its active journal changed",
    );
  }
  const journalBeforeRestore = activeJournalIdentity(preRecoveryState);
  const payloadIdentityBeforeRestore =
    retainedPayloadIdentity(preRecoveryState);
  const retainedPayloadBeforeRestore = retainedPayloadTxIds(preRecoveryState);
  const abandonedPayloadTxIds = journalTransactionSourceIds(preRecoveryState);
  if (retainedPayloadBeforeRestore.length < 2) {
    throw new Error(
      "T1 gate requires both N's payload and the retained replacement payload",
    );
  }
  if (abandonedPayloadTxIds.length === 0) {
    throw new Error(
      "T1 submitted N journal has no transaction payload identity",
    );
  }

  const attemptId = `t1-${Date.now().toString()}-${process.pid.toString()}`;
  const recoveryEvidenceDir = join(runDir, label, attemptId, "chain-recovery");
  const requestedRecoveryPath = requiredEnv(
    "MIDGARD_PHASE4_T1_RECOVERY_COMMAND",
  );
  if (!isAbsolute(requestedRecoveryPath)) {
    throw new Error(
      "MIDGARD_PHASE4_T1_RECOVERY_COMMAND must be an absolute path",
    );
  }
  const [recoveryPath, repositoryRecoveryPath] = await Promise.all([
    realpath(requestedRecoveryPath),
    realpath(
      resolve(
        toolsPackageRoot(),
        "devnet/phase4-process/scripts/t1-recover.sh",
      ),
    ),
  ]);
  if (recoveryPath !== repositoryRecoveryPath) {
    throw new Error(
      "T1 recovery command must be the reviewed run-scoped t1-recover.sh",
    );
  }
  if (activeProcessIsolation === undefined) {
    throw new Error("T1 recovery has no active matched-snapshot identity");
  }
  const recoveryOutput = await runRequiredProcess(
    {
      command: recoveryPath,
      args: [],
      cwd,
      env: {
        ...isolatedChildEnv(),
        MIDGARD_PHASE4_PROCESS_ACCEPTANCE: ACCEPTANCE_ENABLE_VALUE,
        MIDGARD_PHASE4_PROCESS_TARGET: "local-devnet",
        MIDGARD_PHASE4_SCENARIO_LABEL: label,
        MIDGARD_PHASE4_T1_ACCEPTANCE_TOKEN: PHASE4_T1_ACCEPTANCE_TOKEN,
        MIDGARD_PHASE4_T1_ATTEMPT_ID: attemptId,
        MIDGARD_PHASE4_T1_ABANDONED_HEADER_HASH: abandonedHeaderHash,
        MIDGARD_PHASE4_T1_ABANDONED_SUBMITTED_TX_HASH: abandonedSubmittedTxHash,
        MIDGARD_PHASE4_T1_BASE_HEADER_HASH: originalBaseHeaderHash,
        MIDGARD_PHASE4_T1_MINIMUM_END_TIME_MS:
          abandonedHeaderEndTimeMs.toString(),
        MIDGARD_PHASE4_T1_SNAPSHOT_IDENTITY_SHA256:
          activeProcessIsolation.snapshotIdentitySha256,
        MIDGARD_PHASE4_T1_EVIDENCE_DIR: recoveryEvidenceDir,
      },
    },
    "matched-snapshot T1 chain-only recovery and canonical advance",
  );
  const recovery = parseAndValidatePhase4T1RecoveryAttestation({
    output: recoveryOutput,
    expected: {
      scenarioLabel: label,
      attemptId,
      composeProject: activeProcessIsolation.composeProject,
      networkMagic: activeProcessIsolation.networkMagic,
      snapshotIdentitySha256: activeProcessIsolation.snapshotIdentitySha256,
      abandonedHeaderHash,
      abandonedSubmittedTxHash,
      baseHeaderHash: originalBaseHeaderHash,
    },
  });
  const recoveredTipHeaderHash = recovery.recoveredTipHeaderHash;

  // t1-recover.sh compares deterministic SQL dumps. This controller also
  // requires its typed journal and retained transaction CBOR to be identical
  // before any restarted Midgard process is allowed to touch Postgres.
  const afterChainRestoreState = await captureDatabaseState();
  if (
    activeJournalIdentity(afterChainRestoreState) !== journalBeforeRestore ||
    retainedPayloadIdentity(afterChainRestoreState) !==
      payloadIdentityBeforeRestore
  ) {
    throw new Error(
      "T1 chain-only recovery changed the pending journal or retained payload CBOR",
    );
  }

  const postRecoverySpec = makeNodeSpec({
    cwd,
    runDir,
    label: `${label}-post-recovery-attempt`,
    durableStoreLabel,
    nodeId: "node-a",
    port,
    metricsPort,
    timeoutMs: processAcceptanceTimeoutMs(),
  });
  const postRecoveryLogOffset = await logByteLength(
    postRecoverySpec.process.rawLogPath,
  );
  const restart = await runPipelinedCommitNodeUntilMarker({
    spec: postRecoverySpec,
    marker: "pipeline_trace phase=candidate_submitted",
    suffix: "t1-recovered-tip-submit",
  });
  const recoveryAttemptLog = await readLogAttempt(
    postRecoverySpec.process.rawLogPath,
    postRecoveryLogOffset,
  );
  const { replacementCandidateLine } = assertPhase4T1ReplacementAttemptOrdering(
    {
      attemptLog: recoveryAttemptLog,
      recoveredTipHeaderHash,
    },
  );
  const state = await captureDatabaseState();
  if (
    state.activeJournal === null ||
    state.activeJournal.headerHash === abandonedHeaderHash ||
    state.activeJournal.baseTailHeaderHash !== recoveredTipHeaderHash ||
    state.activeJournal.submittedTxHash === null
  ) {
    throw new Error(
      "T1 replacement N' is missing, still equals N, is not based on F, or was not submitted",
    );
  }
  const replacementHeaderHash = requireL2HeaderHash(
    state.activeJournal.headerHash,
    "replacement L2 header N'",
  );
  const replacementSubmittedTxHash = requireCardanoHash(
    state.activeJournal.submittedTxHash,
    "replacement Cardano submission for N'",
  );
  const replacementPayloadTxIds = journalTransactionSourceIds(state);
  const missingPayload = abandonedPayloadTxIds.filter(
    (txId) => !replacementPayloadTxIds.includes(txId),
  );
  if (missingPayload.length > 0) {
    throw new Error(
      `T1 replacement N' lost retained transaction payloads: ${missingPayload.join(",")}`,
    );
  }
  if (retainedPayloadIdentity(state) !== payloadIdentityBeforeRestore) {
    throw new Error(
      "T1 replacement submission changed retained transaction IDs or canonical CBOR",
    );
  }
  return {
    abandonedHeaderHash,
    abandonedSubmittedTxHash,
    abandonedHeaderEndTimeMs,
    originalBaseHeaderHash,
    recoveredTipHeaderHash,
    candidateBaseHeaderHash: recoveredTipHeaderHash,
    recovery,
    preRecoveryCandidateLine,
    replacementCandidateLine,
    journalByteIdenticalAcrossChainRestore: true,
    abandonedPayloadTxIds,
    retainedPayloadTxIds: retainedPayloadBeforeRestore,
    replacementHeaderHash,
    replacementSubmittedTxHash,
    replacementPayloadTxIds,
    continuedSpeculation: true,
    restart,
    state,
  };
};

export const decodeProcessSummaryBeforePersistence = async (
  value: unknown,
): Promise<void> => {
  const moduleUrl = pathToFileURL(
    resolve(
      toolsPackageRoot(),
      "scripts/verify-phase4-pipelined-process-summary.mjs",
    ),
  );
  const verifier = (await import(moduleUrl.href)) as {
    readonly decodePhase4PipelinedProcessSummaryV1: (
      summary: unknown,
    ) => unknown;
  };
  verifier.decodePhase4PipelinedProcessSummaryV1(value);
};
