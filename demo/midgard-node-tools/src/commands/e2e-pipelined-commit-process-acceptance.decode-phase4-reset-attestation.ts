import { readFile, stat } from "node:fs/promises";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import type { PipelinedCommitCrashCheckpoint } from "midgard-node/e2e/pipelined-commit-crash-checkpoint";
import { exactObjectKeys } from "midgard-node/exact-object-keys";

import { type PipelinedCommitDatabaseState } from "../e2e/pipelined-commit-process-harness.js";
import { type OwnedProcessGroupSpec } from "../e2e/process-ownership.js";
import { type ServiceSupervisorSummary } from "../e2e/service-supervisor.js";
import {
  activeIsolatedChildEnv,
  activeProcessOwnership,
} from "./e2e-pipelined-commit-process-acceptance.load-phase4-process-isolation.js";
import { validatePhase4PhasRegistrationProof } from "./e2e-pipelined-commit-process-acceptance.validate-phase4-phas-registration-proof.js";
import {
  ISOLATED_COMPOSE_PREFIX,
  ISOLATED_DATABASE_PREFIX,
  type Phase4ResetAttestation,
  RESET_ATTESTATION_SCHEMA,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-process-isolation-values.js";
import {
  type Phase4T1RecoveryAttestation,
  requireL2TransactionId,
} from "./phase4-t1-recovery.js";

export const isolatedChildEnv = (): Readonly<NodeJS.ProcessEnv> => {
  if (activeIsolatedChildEnv === undefined) {
    throw new Error("Phase 4 immutable child environment is not initialized");
  }
  return activeIsolatedChildEnv;
};

export const requiredIsolatedChildEnv = (name: string): string => {
  const value = isolatedChildEnv()[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`Phase 4 immutable child environment is missing ${name}`);
  }
  return value;
};

export const ownershipForProcess = (
  label: string,
  nodeId: string,
): OwnedProcessGroupSpec => {
  if (activeProcessOwnership === undefined) {
    throw new Error("Phase 4 process ownership identity is not initialized");
  }
  const safeName = `${label}-${nodeId}`.replaceAll(/[^a-zA-Z0-9_.-]/gu, "_");
  return {
    recordPath: join(activeProcessOwnership.recordsDir, `${safeName}.json`),
    runToken: activeProcessOwnership.runToken,
  };
};

export type ProcessResult = {
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly output: string;
  readonly timedOut: boolean;
};

export type CrashAcceptanceEvidence = {
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly baseHeaderHash: string;
  readonly crash: ServiceSupervisorSummary;
  readonly restartReady: ServiceSupervisorSummary;
  readonly restartSubmitted: ServiceSupervisorSummary;
  readonly afterCrash: PipelinedCommitDatabaseState;
  readonly afterRestartReady: PipelinedCommitDatabaseState;
  readonly flagOnSubmitted: PipelinedCommitDatabaseState;
  readonly flagOffControl: PipelinedCommitDatabaseState;
};

export type T1RecoveryAcceptanceEvidence = {
  readonly abandonedHeaderHash: string;
  readonly abandonedSubmittedTxHash: string;
  readonly abandonedHeaderEndTimeMs: number;
  readonly originalBaseHeaderHash: string;
  readonly recoveredTipHeaderHash: string;
  readonly candidateBaseHeaderHash: string;
  readonly recovery: Phase4T1RecoveryAttestation;
  readonly preRecoveryCandidateLine: string;
  readonly replacementCandidateLine: string;
  readonly journalByteIdenticalAcrossChainRestore: true;
  readonly abandonedPayloadTxIds: readonly string[];
  readonly retainedPayloadTxIds: readonly string[];
  readonly replacementHeaderHash: string;
  readonly replacementSubmittedTxHash: string;
  readonly replacementPayloadTxIds: readonly string[];
  readonly continuedSpeculation: true;
  readonly restart: ServiceSupervisorSummary;
  readonly state: PipelinedCommitDatabaseState;
};

const stableJson = (value: unknown): string => JSON.stringify(value);

export const activeJournalIdentity = (
  state: PipelinedCommitDatabaseState,
): string => {
  if (state.activeJournal === null) {
    throw new Error(
      "T1 recovery requires one active pending-finalization journal",
    );
  }
  return stableJson(state.activeJournal);
};

export const journalHeaderEndTimeMs = (
  state: PipelinedCommitDatabaseState,
): number => {
  if (state.activeJournal === null) {
    throw new Error("T1 recovery cannot decode a missing active journal");
  }
  const header = Data.from(state.activeJournal.headerCbor, SDK.Header);
  const endTimeMs = Number(header.endTime);
  if (!Number.isSafeInteger(endTimeMs) || endTimeMs <= 0) {
    throw new Error("T1 pending journal header has an invalid end time");
  }
  return endTimeMs;
};

export const retainedPayloadTxIds = (
  state: PipelinedCommitDatabaseState,
): readonly string[] =>
  [
    ...new Set(
      [...state.mempool, ...state.processed].map((entry) => entry.txId),
    ),
  ]
    .map((txId) => requireL2TransactionId(txId, "retained L2 transaction id"))
    .sort((left, right) => left.localeCompare(right));

export const retainedPayloadIdentity = (
  state: PipelinedCommitDatabaseState,
): string =>
  stableJson(
    [...state.mempool, ...state.processed]
      .map((entry) => ({ txId: entry.txId, tx: entry.tx }))
      .sort((left, right) => left.txId.localeCompare(right.txId)),
  );

export const journalTransactionSourceIds = (
  state: PipelinedCommitDatabaseState,
): readonly string[] => {
  if (state.activeJournal === null) return [];
  return state.activeJournal.journalPayloadIdentity.transactions
    .flatMap((entry) => {
      if (
        typeof entry !== "object" ||
        entry === null ||
        !("sourceId" in entry) ||
        typeof entry.sourceId !== "string"
      ) {
        return [];
      }
      return [
        requireL2TransactionId(
          entry.sourceId,
          "replacement journal transaction id",
        ),
      ];
    })
    .sort((left, right) => left.localeCompare(right));
};

export const logByteLength = async (path: string): Promise<number> =>
  stat(path).then(
    (entry) => entry.size,
    (error: unknown) => {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return 0;
      }
      throw error;
    },
  );

export const readLogAttempt = async (
  path: string,
  startByte: number,
): Promise<string> => {
  const bytes = await readFile(path);
  if (startByte < 0 || startByte > bytes.length) {
    throw new Error("T1 log attempt boundary is outside the append-only log");
  }
  return bytes.subarray(startByte).toString("utf8");
};

export const decodePhase4ResetAttestation = (
  value: unknown,
): Phase4ResetAttestation => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "scenarioLabel",
      "composeProject",
      "networkMagic",
      "postgresDatabase",
      "deploymentManifestSha256",
      "snapshotSetSha256",
      "snapshotIdentitySha256",
      "phasRegistrationProofSha256",
      "phasRegistration",
      "cardanoTip",
      "kupoCheckpoint",
    ]) ||
    !exactObjectKeys(value.cardanoTip, ["slot", "hash"])
  ) {
    throw new Error(
      "Phase 4 reset attestation fields do not match the exact V1 schema",
    );
  }
  if (
    value.schemaVersion !== RESET_ATTESTATION_SCHEMA ||
    typeof value.scenarioLabel !== "string" ||
    value.scenarioLabel.length === 0 ||
    typeof value.composeProject !== "string" ||
    !value.composeProject.startsWith(ISOLATED_COMPOSE_PREFIX) ||
    !/^[a-z0-9_-]+$/u.test(value.composeProject) ||
    !Number.isSafeInteger(value.networkMagic) ||
    (value.networkMagic as number) <= 0 ||
    typeof value.postgresDatabase !== "string" ||
    !value.postgresDatabase.startsWith(ISOLATED_DATABASE_PREFIX) ||
    typeof value.deploymentManifestSha256 !== "string"
  ) {
    throw new Error(
      "Phase 4 reset attestation contains a noncanonical V1 value",
    );
  }
  for (const key of [
    "deploymentManifestSha256",
    "snapshotSetSha256",
    "snapshotIdentitySha256",
    "phasRegistrationProofSha256",
  ] as const) {
    if (typeof value[key] !== "string" || !/^[a-f0-9]{64}$/u.test(value[key])) {
      throw new Error(`Phase 4 reset attestation has invalid ${key}`);
    }
  }
  if (
    !Number.isSafeInteger(value.cardanoTip.slot) ||
    (value.cardanoTip.slot as number) < 0 ||
    typeof value.cardanoTip.hash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.cardanoTip.hash)
  ) {
    throw new Error("Phase 4 reset attestation has invalid Cardano tip");
  }
  if (value.kupoCheckpoint !== value.cardanoTip.slot) {
    throw new Error(
      "Phase 4 reset attestation Kupo checkpoint must equal the Cardano tip slot",
    );
  }
  const phasRegistration = validatePhase4PhasRegistrationProof(
    value.phasRegistration,
    "Phase 4 reset PHAS registration proof",
  );
  if (
    phasRegistration.networkMagic !== value.networkMagic ||
    phasRegistration.observedAtTip.slot !== value.cardanoTip.slot ||
    phasRegistration.observedAtTip.hash !== value.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 reset PHAS registration proof is not bound to its attestation",
    );
  }
  return value as Phase4ResetAttestation;
};

export const parseResetAttestation = (
  output: string,
): Phase4ResetAttestation => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(output.trim());
  } catch (cause) {
    throw new Error(
      `Phase 4 reset command must emit exactly one JSON attestation: ${String(cause)}`,
    );
  }
  return decodePhase4ResetAttestation(parsed);
};
