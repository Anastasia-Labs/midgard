import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import {
  access,
  mkdir,
  readdir,
  readFile,
  realpath,
  stat,
  writeFile,
} from "node:fs/promises";
import { dirname, isAbsolute, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  getAddressDetails,
  type Network,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { positiveSafeInteger } from "midgard-node/artifact-schema";
import { loadDotenvFile } from "midgard-node/e2e/env";
import type { PipelinedCommitCrashCheckpoint } from "midgard-node/e2e/pipelined-commit-crash-checkpoint";
import { exactObjectKeys } from "midgard-node/exact-object-keys";
import { loadPhasMembershipWithdrawalScript } from "midgard-node/phas-membership";
import { Database } from "midgard-node/services/database";

import {
  assertNoJournalBeyondBase,
  capturePipelinedCommitDatabaseState,
  type PipelinedCommitDatabaseState,
  type PipelinedCommitNodeProcessSpec,
  restartPipelinedCommitNodeUntilFreshCandidate,
  restartPipelinedCommitNodeUntilSubmission,
  runPipelinedCommitCheckpointCrash,
  runPipelinedCommitFlagOffControl,
  runPipelinedCommitLeaseContention,
  runPipelinedCommitNodeUntilMarker,
  runPipelinedCommitNormalLeaseContention,
} from "../e2e/pipelined-commit-process-harness.js";
import {
  cleanupOwnedProcessGroupAndRecord,
  generateOwnedProcessRunToken,
  type OwnedProcessGroupSpec,
} from "../e2e/process-ownership.js";
import {
  type ServiceSupervisorSummary,
  superviseHostProcess,
} from "../e2e/service-supervisor.js";
import {
  decodePhase4GenesisLedgerReport,
  PHASE4_GENESIS_BOOTSTRAP_ENV,
  PHASE4_GENESIS_BOOTSTRAP_TOKEN,
  PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE,
} from "./phase4-genesis-ledger.js";
import {
  assertPhase4T1ReplacementAttemptOrdering,
  parseAndValidatePhase4T1RecoveryAttestation,
  PHASE4_T1_ACCEPTANCE_TOKEN,
  type Phase4T1RecoveryAttestation,
  requireCardanoHash,
  requireL2HeaderHash,
  requireL2TransactionId,
  requirePhase4T1CandidateLine,
} from "./phase4-t1-recovery.js";

const ACCEPTANCE_ENABLE_VALUE = "pipelined-commit-live-v1";
const BLOCK_SUBMITTED_MARKER =
  "Block submitted; local finalization is intentionally deferred until L1 confirmation.";
const CRASH_MATRIX_CHECKPOINTS = [
  "speculative_mid_build",
  "candidate_ready_unconfirmed",
  "confirmation_wake_before_journal",
] as const satisfies readonly PipelinedCommitCrashCheckpoint[];

const RESET_ATTESTATION_SCHEMA =
  "midgard-phase4-local-devnet-reset-attestation-v1";
const ISOLATED_DATABASE_PREFIX = "midgard_phase4_process_";
const ISOLATED_COMPOSE_PREFIX = "midgard_phase4_process_";

export type Phase4PhasRegistrationProof = {
  readonly schemaVersion: "midgard-phase4-phas-registration-proof-v1";
  readonly source: "cardano-cli-local-state-query";
  readonly readOnly: true;
  readonly registered: true;
  readonly cardanoImage: { readonly ref: string; readonly id: string };
  readonly networkMagic: number;
  readonly manifestId: string;
  readonly registrationTxHash: string;
  readonly rewardAddress: string;
  readonly rewardAddressBase16: string;
  readonly scriptHash: string;
  readonly transactionBody: {
    readonly schemaVersion: "midgard-phas-registration-transaction-body-v1";
    readonly artifactSha256: string;
    readonly cborSha256: string;
    readonly cborSizeBytes: number;
    readonly cardanoCliTxHash: string;
    readonly certificate: {
      readonly kind: "stake_registration";
      readonly index: 0;
      readonly count: 1;
      readonly credentialType: "script";
      readonly scriptHash: string;
    };
  };
  readonly registrationDepositLovelace: number;
  readonly confirmation: {
    readonly slot: number;
    readonly blockHeaderHash: string;
  };
  readonly observedAtTip: { readonly slot: number; readonly hash: string };
};

export type Phase4ProcessIsolationIdentity = {
  readonly envFile: string;
  readonly deploymentManifestPath: string;
  readonly deploymentManifestSha256: string;
  readonly snapshotIdentityPath: string;
  readonly snapshotIdentitySha256: string;
  readonly snapshotCardanoTip: { readonly slot: number; readonly hash: string };
  readonly snapshotKupoCheckpoint: number;
  readonly snapshotBlueprintSha256: string;
  readonly snapshotPhasRegistrationProofSha256: string;
  readonly snapshotPhasRegistration: Phase4PhasRegistrationProof;
  readonly snapshotPhasRegistrationTransactionBody: {
    readonly type: "Unwitnessed Tx ConwayEra";
    readonly description: string;
    readonly cborHex: string;
  };
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly postgresDatabase: string;
  readonly postgresPort: number;
  readonly ogmiosPort: number;
  readonly kupoPort: number;
};

export type Phase4ResetAttestation = {
  readonly schemaVersion: typeof RESET_ATTESTATION_SCHEMA;
  readonly scenarioLabel: string;
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly postgresDatabase: string;
  readonly deploymentManifestSha256: string;
  readonly snapshotSetSha256: string;
  readonly snapshotIdentitySha256: string;
  readonly phasRegistrationProofSha256: string;
  readonly phasRegistration: Phase4PhasRegistrationProof;
  readonly cardanoTip: { readonly slot: number; readonly hash: string };
  readonly kupoCheckpoint: number;
};

export type Phase4MatchedSnapshotIdentity = {
  readonly schemaVersion: "midgard-phase4-matched-snapshot-identity-v1";
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly postgresDatabase: string;
  readonly deploymentManifestSha256: string;
  readonly blueprintSha256: string;
  readonly images: Readonly<
    Record<
      "cardanoNode" | "ogmios" | "kupo" | "postgres",
      { readonly ref: string; readonly id: string }
    >
  >;
  readonly artifacts: Readonly<
    Record<
      | "sourceSha256"
      | "distSha256"
      | "toolsSourceSha256"
      | "toolsDistSha256"
      | "genesisSha256"
      | "configSha256"
      | "acceptanceEnvSha256"
      | "composeSha256"
      | "phase4AssetsSha256"
      | "phasRegistrationProofSha256",
      string
    >
  >;
  readonly phasRegistration: Phase4PhasRegistrationProof;
  readonly cardanoTip: { readonly slot: number; readonly hash: string };
  readonly kupoCheckpoint: number;
};

let activeProcessIsolation: Phase4ProcessIsolationIdentity | undefined;
let activeIsolatedChildEnv: Readonly<NodeJS.ProcessEnv> | undefined;
let activeProcessOwnership:
  | { readonly runToken: string; readonly recordsDir: string }
  | undefined;

const PHASE4_CHILD_OS_ENV_KEYS = [
  "HOME",
  "LANG",
  "LC_ALL",
  "PATH",
  "SHELL",
  "TMPDIR",
  "TZ",
] as const;

export const buildPhase4IsolatedChildEnv = ({
  values,
  deploymentManifestPath,
  baseEnv = process.env,
}: {
  readonly values: Readonly<Record<string, string>>;
  readonly deploymentManifestPath: string;
  readonly baseEnv?: Readonly<NodeJS.ProcessEnv>;
}): Readonly<NodeJS.ProcessEnv> => {
  const osEnv = Object.fromEntries(
    PHASE4_CHILD_OS_ENV_KEYS.flatMap((key) => {
      const value = baseEnv[key];
      return value === undefined ? [] : [[key, value] as const];
    }),
  );
  return Object.freeze({
    ...osEnv,
    ...values,
    MIDGARD_DEPLOYMENT_MANIFEST_PATH: deploymentManifestPath,
  });
};

const isolatedChildEnv = (): Readonly<NodeJS.ProcessEnv> => {
  if (activeIsolatedChildEnv === undefined) {
    throw new Error("Phase 4 immutable child environment is not initialized");
  }
  return activeIsolatedChildEnv;
};

const requiredIsolatedChildEnv = (name: string): string => {
  const value = isolatedChildEnv()[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`Phase 4 immutable child environment is missing ${name}`);
  }
  return value;
};

const ownershipForProcess = (
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

const initializeProcessOwnership = async (runDir: string): Promise<void> => {
  const ownershipDir = join(runDir, "owned-process-groups");
  const tokenPath = join(ownershipDir, "run-token");
  await mkdir(ownershipDir, { recursive: true, mode: 0o700 });
  let runToken: string;
  try {
    runToken = (await readFile(tokenPath, "utf8")).trim();
  } catch (error) {
    if (
      typeof error !== "object" ||
      error === null ||
      !("code" in error) ||
      error.code !== "ENOENT"
    ) {
      throw error;
    }
    runToken = generateOwnedProcessRunToken();
    await writeFile(tokenPath, `${runToken}\n`, {
      encoding: "utf8",
      flag: "wx",
      mode: 0o600,
    });
  }
  if (!/^[a-f0-9]{64}$/u.test(runToken)) {
    throw new Error("Phase 4 owned process-group run token is invalid");
  }
  const orphanRecords = (await readdir(ownershipDir, { withFileTypes: true }))
    .filter((entry) => entry.isFile() && entry.name.endsWith(".json"))
    .map((entry) => join(ownershipDir, entry.name))
    .sort((left, right) => left.localeCompare(right));
  for (const recordPath of orphanRecords) {
    const cleanup = await cleanupOwnedProcessGroupAndRecord({
      spec: { recordPath, runToken },
    });
    if (!cleanup.success) {
      throw new Error(
        `Phase 4 refuses to start while owned process-group cleanup is unresolved for ${recordPath}: ${cleanup.error ?? cleanup.ownershipValidation.reason}`,
      );
    }
  }
  activeProcessOwnership = Object.freeze({
    runToken,
    recordsDir: ownershipDir,
  });
};

type ProcessResult = {
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly output: string;
  readonly timedOut: boolean;
};

type CrashAcceptanceEvidence = {
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

type T1RecoveryAcceptanceEvidence = {
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

const requiredEnv = (name: string): string => {
  const value = process.env[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`Missing required ${name}`);
  }
  return value;
};

const sha256File = async (path: string): Promise<string> =>
  createHash("sha256")
    .update(await readFile(path))
    .digest("hex");

const stableJson = (value: unknown): string => JSON.stringify(value);

const activeJournalIdentity = (state: PipelinedCommitDatabaseState): string => {
  if (state.activeJournal === null) {
    throw new Error(
      "T1 recovery requires one active pending-finalization journal",
    );
  }
  return stableJson(state.activeJournal);
};

const journalHeaderEndTimeMs = (
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

const retainedPayloadTxIds = (
  state: PipelinedCommitDatabaseState,
): readonly string[] =>
  [
    ...new Set(
      [...state.mempool, ...state.processed].map((entry) => entry.txId),
    ),
  ]
    .map((txId) => requireL2TransactionId(txId, "retained L2 transaction id"))
    .sort((left, right) => left.localeCompare(right));

const retainedPayloadIdentity = (state: PipelinedCommitDatabaseState): string =>
  stableJson(
    [...state.mempool, ...state.processed]
      .map((entry) => ({ txId: entry.txId, tx: entry.tx }))
      .sort((left, right) => left.txId.localeCompare(right.txId)),
  );

const journalTransactionSourceIds = (
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

const logByteLength = async (path: string): Promise<number> =>
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

const readLogAttempt = async (
  path: string,
  startByte: number,
): Promise<string> => {
  const bytes = await readFile(path);
  if (startByte < 0 || startByte > bytes.length) {
    throw new Error("T1 log attempt boundary is outside the append-only log");
  }
  return bytes.subarray(startByte).toString("utf8");
};

const requiredValue = (
  values: Readonly<Record<string, string>>,
  name: string,
): string => {
  const value = values[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`Phase 4 process env file is missing ${name}`);
  }
  return value;
};

const positiveInteger = (raw: string, label: string): number =>
  positiveSafeInteger(Number(raw), label);

const isolatedEndpointPort = (
  raw: string,
  label: string,
  protectedPorts: ReadonlySet<number>,
): number => {
  const endpoint = new URL(raw);
  if (endpoint.hostname !== "127.0.0.1" && endpoint.hostname !== "localhost") {
    throw new Error(`${label} must use a loopback host`);
  }
  if (endpoint.port.length === 0) {
    throw new Error(`${label} must declare an explicit isolated port`);
  }
  const port = positiveInteger(endpoint.port, `${label} port`);
  if (protectedPorts.has(port)) {
    throw new Error(`${label} may not use protected live port ${port}`);
  }
  return port;
};

export const validatePhase4ProcessIsolationValues = (
  values: Readonly<Record<string, string>>,
): Omit<
  Phase4ProcessIsolationIdentity,
  | "envFile"
  | "deploymentManifestPath"
  | "deploymentManifestSha256"
  | "snapshotIdentityPath"
  | "snapshotIdentitySha256"
  | "snapshotCardanoTip"
  | "snapshotKupoCheckpoint"
  | "snapshotBlueprintSha256"
  | "snapshotPhasRegistrationProofSha256"
  | "snapshotPhasRegistration"
  | "snapshotPhasRegistrationTransactionBody"
> => {
  const postgresHost = requiredValue(values, "POSTGRES_HOST");
  if (postgresHost !== "127.0.0.1") {
    throw new Error("Phase 4 isolated POSTGRES_HOST must be 127.0.0.1");
  }
  const postgresPort = positiveInteger(
    requiredValue(values, "POSTGRES_PORT"),
    "POSTGRES_PORT",
  );
  if (postgresPort === 5432 || postgresPort === 5433) {
    throw new Error(
      `Phase 4 isolated Postgres may not use protected live port ${postgresPort}`,
    );
  }
  const postgresDatabase = requiredValue(values, "POSTGRES_DB");
  if (!postgresDatabase.startsWith(ISOLATED_DATABASE_PREFIX)) {
    throw new Error(
      `Phase 4 POSTGRES_DB must start with ${ISOLATED_DATABASE_PREFIX}`,
    );
  }
  if (requiredValue(values, "L1_PROVIDER") !== "Kupmios") {
    throw new Error("Phase 4 process acceptance requires L1_PROVIDER=Kupmios");
  }
  if (
    requiredValue(values, "MIN_FEE_A") !== "0" ||
    requiredValue(values, "MIN_FEE_B") !== "0"
  ) {
    throw new Error(
      "Phase 4 process acceptance requires pinned MIN_FEE_A=0 and MIN_FEE_B=0",
    );
  }
  if (requiredValue(values, "RUN_GENESIS_ON_STARTUP") !== "false") {
    throw new Error("Phase 4 isolated nodes must use attach/resume");
  }
  if (requiredValue(values, "MIDGARD_DOTENV_MODE") !== "disabled") {
    throw new Error(
      "Phase 4 isolated nodes must disable checkout dotenv loading",
    );
  }
  if (requiredValue(values, "NETWORK") !== "Custom") {
    throw new Error("Phase 4 process acceptance requires NETWORK=Custom");
  }
  for (const seed of [
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_C",
  ]) {
    requiredValue(values, seed);
  }
  const composeProject = requiredValue(
    values,
    "MIDGARD_PHASE4_COMPOSE_PROJECT",
  );
  if (
    !composeProject.startsWith(ISOLATED_COMPOSE_PREFIX) ||
    !/^[a-z0-9_-]+$/u.test(composeProject)
  ) {
    throw new Error(
      `MIDGARD_PHASE4_COMPOSE_PROJECT must use ${ISOLATED_COMPOSE_PREFIX} and safe lowercase characters`,
    );
  }
  const networkMagic = positiveInteger(
    requiredValue(values, "MIDGARD_PHASE4_NETWORK_MAGIC"),
    "MIDGARD_PHASE4_NETWORK_MAGIC",
  );
  const ogmiosPort = isolatedEndpointPort(
    requiredValue(values, "L1_OGMIOS_KEY"),
    "L1_OGMIOS_KEY",
    new Set([1337]),
  );
  const kupoPort = isolatedEndpointPort(
    requiredValue(values, "L1_KUPO_KEY"),
    "L1_KUPO_KEY",
    new Set([1442]),
  );
  if (new Set([postgresPort, ogmiosPort, kupoPort]).size !== 3) {
    throw new Error("Phase 4 isolated service ports must be distinct");
  }
  return {
    composeProject,
    networkMagic,
    postgresDatabase,
    postgresPort,
    ogmiosPort,
    kupoPort,
  };
};

export const validatePhase4PhasRegistrationProof = (
  value: unknown,
  label: string,
): Phase4PhasRegistrationProof => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "source",
      "readOnly",
      "registered",
      "cardanoImage",
      "networkMagic",
      "manifestId",
      "registrationTxHash",
      "rewardAddress",
      "rewardAddressBase16",
      "scriptHash",
      "transactionBody",
      "registrationDepositLovelace",
      "confirmation",
      "observedAtTip",
    ])
  ) {
    throw new Error(`${label} fields do not match the exact V1 schema`);
  }
  const proof = value as Record<string, unknown> &
    Partial<Phase4PhasRegistrationProof>;
  if (
    !exactObjectKeys(proof.cardanoImage, ["ref", "id"]) ||
    !exactObjectKeys(proof.transactionBody, [
      "schemaVersion",
      "artifactSha256",
      "cborSha256",
      "cborSizeBytes",
      "cardanoCliTxHash",
      "certificate",
    ]) ||
    !exactObjectKeys(proof.transactionBody.certificate, [
      "kind",
      "index",
      "count",
      "credentialType",
      "scriptHash",
    ]) ||
    !exactObjectKeys(proof.confirmation, ["slot", "blockHeaderHash"]) ||
    !exactObjectKeys(proof.observedAtTip, ["slot", "hash"])
  ) {
    throw new Error(`${label} nested fields do not match the exact V1 schema`);
  }
  if (
    proof.schemaVersion !== "midgard-phase4-phas-registration-proof-v1" ||
    proof.source !== "cardano-cli-local-state-query" ||
    proof.readOnly !== true ||
    proof.registered !== true
  ) {
    throw new Error(`${label} is not an exact read-only registration proof`);
  }
  if (
    typeof proof.cardanoImage?.ref !== "string" ||
    !/@sha256:[a-f0-9]{64}$/u.test(proof.cardanoImage.ref) ||
    typeof proof.cardanoImage.id !== "string" ||
    !/^sha256:[a-f0-9]{64}$/u.test(proof.cardanoImage.id)
  ) {
    throw new Error(`${label} has invalid pinned Cardano image identity`);
  }
  if (
    !Number.isSafeInteger(proof.networkMagic) ||
    (proof.networkMagic ?? 0) <= 0 ||
    !/^[a-f0-9]{64}$/u.test(proof.manifestId ?? "") ||
    !/^[a-f0-9]{64}$/u.test(proof.registrationTxHash ?? "") ||
    !/^stake_test1[0-9a-z]+$/u.test(proof.rewardAddress ?? "") ||
    proof.rewardAddressBase16 !== `f0${proof.scriptHash ?? ""}` ||
    !/^[a-f0-9]{56}$/u.test(proof.scriptHash ?? "") ||
    !Number.isSafeInteger(proof.registrationDepositLovelace) ||
    (proof.registrationDepositLovelace ?? 0) <= 0
  ) {
    throw new Error(`${label} has invalid PHAS registration identity`);
  }
  const canonicalIdentity = SDK.phasMembershipIdentity(
    "Custom",
    loadPhasMembershipWithdrawalScript(),
  );
  if (
    proof.rewardAddress !== canonicalIdentity.rewardAddress ||
    proof.scriptHash !== canonicalIdentity.scriptHash
  ) {
    throw new Error(`${label} is not the canonical deployed PHAS identity`);
  }
  let addressDetails: ReturnType<typeof getAddressDetails>;
  try {
    addressDetails = getAddressDetails(proof.rewardAddress!);
  } catch (cause) {
    throw new Error(`${label} reward address is not valid canonical bech32`, {
      cause,
    });
  }
  if (
    addressDetails.type !== "Reward" ||
    addressDetails.networkId !== 0 ||
    addressDetails.address.hex !== proof.rewardAddressBase16 ||
    addressDetails.stakeCredential?.type !== "Script" ||
    addressDetails.stakeCredential.hash !== proof.scriptHash
  ) {
    throw new Error(
      `${label} reward account is not the exact testnet PHAS script credential`,
    );
  }
  const transactionBody = proof.transactionBody;
  if (
    transactionBody?.schemaVersion !==
      "midgard-phas-registration-transaction-body-v1" ||
    !/^[a-f0-9]{64}$/u.test(transactionBody.artifactSha256 ?? "") ||
    !/^[a-f0-9]{64}$/u.test(transactionBody.cborSha256 ?? "") ||
    !Number.isSafeInteger(transactionBody.cborSizeBytes) ||
    (transactionBody.cborSizeBytes ?? 0) <= 0 ||
    transactionBody.cardanoCliTxHash !== proof.registrationTxHash ||
    transactionBody.certificate?.kind !== "stake_registration" ||
    transactionBody.certificate.index !== 0 ||
    transactionBody.certificate.count !== 1 ||
    transactionBody.certificate.credentialType !== "script" ||
    transactionBody.certificate.scriptHash !== proof.scriptHash
  ) {
    throw new Error(
      `${label} does not bind the exact cardano-cli-inspected PHAS registration transaction body`,
    );
  }
  if (
    !Number.isSafeInteger(proof.confirmation?.slot) ||
    (proof.confirmation?.slot ?? -1) < 0 ||
    !/^[a-f0-9]{64}$/u.test(proof.confirmation?.blockHeaderHash ?? "") ||
    !Number.isSafeInteger(proof.observedAtTip?.slot) ||
    (proof.observedAtTip?.slot ?? -1) < 0 ||
    !/^[a-f0-9]{64}$/u.test(proof.observedAtTip?.hash ?? "") ||
    (proof.confirmation?.slot ?? Number.MAX_SAFE_INTEGER) >
      (proof.observedAtTip?.slot ?? -1)
  ) {
    throw new Error(`${label} has invalid confirmation-point evidence`);
  }
  return proof as Phase4PhasRegistrationProof;
};

export const decodePhase4MatchedSnapshotIdentity = (
  value: unknown,
): Phase4MatchedSnapshotIdentity => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "composeProject",
      "networkMagic",
      "postgresDatabase",
      "deploymentManifestSha256",
      "blueprintSha256",
      "images",
      "artifacts",
      "phasRegistration",
      "cardanoTip",
      "kupoCheckpoint",
    ])
  ) {
    throw new Error(
      "Phase 4 matched snapshot identity fields do not match the exact V1 schema",
    );
  }
  if (
    !exactObjectKeys(value.images, [
      "cardanoNode",
      "ogmios",
      "kupo",
      "postgres",
    ]) ||
    !exactObjectKeys(value.artifacts, [
      "sourceSha256",
      "distSha256",
      "toolsSourceSha256",
      "toolsDistSha256",
      "genesisSha256",
      "configSha256",
      "acceptanceEnvSha256",
      "composeSha256",
      "phase4AssetsSha256",
      "phasRegistrationProofSha256",
    ]) ||
    !exactObjectKeys(value.cardanoTip, ["slot", "hash"])
  ) {
    throw new Error(
      "Phase 4 matched snapshot identity nested fields do not match the exact V1 schema",
    );
  }
  for (const imageName of [
    "cardanoNode",
    "ogmios",
    "kupo",
    "postgres",
  ] as const) {
    const image = value.images[imageName];
    if (
      !exactObjectKeys(image, ["ref", "id"]) ||
      typeof image.ref !== "string" ||
      !/@sha256:[a-f0-9]{64}$/u.test(image.ref) ||
      typeof image.id !== "string" ||
      !/^sha256:[a-f0-9]{64}$/u.test(image.id)
    ) {
      throw new Error(
        `Phase 4 matched snapshot ${imageName} image identity is noncanonical`,
      );
    }
  }
  if (
    value.schemaVersion !== "midgard-phase4-matched-snapshot-identity-v1" ||
    typeof value.composeProject !== "string" ||
    !value.composeProject.startsWith(ISOLATED_COMPOSE_PREFIX) ||
    !/^[a-z0-9_-]+$/u.test(value.composeProject) ||
    !Number.isSafeInteger(value.networkMagic) ||
    (value.networkMagic as number) <= 0 ||
    typeof value.postgresDatabase !== "string" ||
    !value.postgresDatabase.startsWith(ISOLATED_DATABASE_PREFIX) ||
    typeof value.deploymentManifestSha256 !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.deploymentManifestSha256) ||
    typeof value.blueprintSha256 !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.blueprintSha256) ||
    !Number.isSafeInteger(value.cardanoTip.slot) ||
    (value.cardanoTip.slot as number) < 0 ||
    typeof value.cardanoTip.hash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.cardanoTip.hash) ||
    value.kupoCheckpoint !== value.cardanoTip.slot
  ) {
    throw new Error(
      "Phase 4 matched snapshot identity contains a noncanonical V1 value",
    );
  }
  for (const digest of Object.values(value.artifacts)) {
    if (typeof digest !== "string" || !/^[a-f0-9]{64}$/u.test(digest)) {
      throw new Error(
        "Phase 4 matched snapshot identity contains a noncanonical artifact digest",
      );
    }
  }
  const phasRegistration = validatePhase4PhasRegistrationProof(
    value.phasRegistration,
    "Phase 4 matched snapshot PHAS registration proof",
  );
  const cardanoNodeImage = value.images.cardanoNode as {
    readonly ref: string;
    readonly id: string;
  };
  if (
    phasRegistration.networkMagic !== value.networkMagic ||
    phasRegistration.cardanoImage.ref !== cardanoNodeImage.ref ||
    phasRegistration.cardanoImage.id !== cardanoNodeImage.id ||
    phasRegistration.observedAtTip.slot !== value.cardanoTip.slot ||
    phasRegistration.observedAtTip.hash !== value.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 matched snapshot PHAS proof is not bound to the snapshot identity",
    );
  }
  return value as Phase4MatchedSnapshotIdentity;
};

export const validatePhase4PhasRegistrationTransactionBody = (
  value: unknown,
  proof: Phase4PhasRegistrationProof,
): Phase4ProcessIsolationIdentity["snapshotPhasRegistrationTransactionBody"] => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("Phase 4 PHAS transaction-body envelope must be an object");
  }
  const envelope = value as {
    readonly type?: unknown;
    readonly description?: unknown;
    readonly cborHex?: unknown;
  };
  if (
    Object.keys(envelope)
      .sort((left, right) => left.localeCompare(right))
      .join(",") !== "cborHex,description,type"
  ) {
    throw new Error(
      "Phase 4 PHAS transaction-body envelope fields do not match the exact schema",
    );
  }
  if (
    envelope.type !== "Unwitnessed Tx ConwayEra" ||
    typeof envelope.description !== "string" ||
    envelope.description.length === 0 ||
    typeof envelope.cborHex !== "string" ||
    envelope.cborHex.length === 0 ||
    envelope.cborHex.length % 2 !== 0 ||
    !/^[a-f0-9]+$/u.test(envelope.cborHex)
  ) {
    throw new Error(
      "Phase 4 PHAS transaction-body envelope is not exact canonical unsigned CBOR",
    );
  }
  const cborBytes = Buffer.from(envelope.cborHex, "hex");
  const transaction = CML.Transaction.from_cbor_hex(envelope.cborHex);
  const body = transaction.body();
  const certificates = body.certs();
  const certificate = certificates?.len() === 1 ? certificates.get(0) : null;
  const credential = certificate?.as_stake_registration()?.stake_credential();
  if (
    createHash("sha256").update(cborBytes).digest("hex") !==
      proof.transactionBody.cborSha256 ||
    cborBytes.length !== proof.transactionBody.cborSizeBytes ||
    transaction.to_canonical_cbor_hex() !== envelope.cborHex ||
    transaction.witness_set().to_cbor_hex() !== "a0" ||
    CML.hash_transaction(body).to_hex() !== proof.registrationTxHash ||
    certificate?.kind() !== CML.CertificateKind.StakeRegistration ||
    credential?.kind() !== CML.CredentialKind.Script ||
    credential.as_script()?.to_hex() !== proof.scriptHash
  ) {
    throw new Error(
      "Phase 4 PHAS unsigned transaction body does not contain the exact submitted script registration certificate",
    );
  }
  return envelope as Phase4ProcessIsolationIdentity["snapshotPhasRegistrationTransactionBody"];
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

const parseResetAttestation = (output: string): Phase4ResetAttestation => {
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

export const validatePhase4ResetAttestation = ({
  output,
  scenarioLabel,
  isolation,
}: {
  readonly output: string;
  readonly scenarioLabel: string;
  readonly isolation: Phase4ProcessIsolationIdentity;
}): Phase4ResetAttestation => {
  const attestation = parseResetAttestation(output);
  const expected = {
    schemaVersion: RESET_ATTESTATION_SCHEMA,
    scenarioLabel,
    composeProject: isolation.composeProject,
    networkMagic: isolation.networkMagic,
    postgresDatabase: isolation.postgresDatabase,
    deploymentManifestSha256: isolation.deploymentManifestSha256,
  } as const;
  for (const [key, value] of Object.entries(expected)) {
    if (attestation[key as keyof typeof attestation] !== value) {
      throw new Error(
        `Phase 4 reset attestation ${key} mismatch: expected=${JSON.stringify(value) ?? "undefined"},actual=${JSON.stringify(attestation[key as keyof typeof attestation]) ?? "undefined"}`,
      );
    }
  }
  if (!/^[a-f0-9]{64}$/u.test(attestation.snapshotSetSha256)) {
    throw new Error("Phase 4 reset attestation has invalid snapshotSetSha256");
  }
  if (!/^[a-f0-9]{64}$/u.test(attestation.snapshotIdentitySha256)) {
    throw new Error(
      "Phase 4 reset attestation has invalid snapshotIdentitySha256",
    );
  }
  if (!/^[a-f0-9]{64}$/u.test(attestation.phasRegistrationProofSha256)) {
    throw new Error(
      "Phase 4 reset attestation has invalid phasRegistrationProofSha256",
    );
  }
  const phasRegistration = validatePhase4PhasRegistrationProof(
    attestation.phasRegistration,
    "Phase 4 reset PHAS registration proof",
  );
  if (
    attestation.phasRegistrationProofSha256 !==
      isolation.snapshotPhasRegistrationProofSha256 ||
    JSON.stringify(phasRegistration) !==
      JSON.stringify(isolation.snapshotPhasRegistration)
  ) {
    throw new Error(
      "Phase 4 reset PHAS registration proof does not match the frozen snapshot proof",
    );
  }
  if (
    !Number.isSafeInteger(attestation.cardanoTip?.slot) ||
    attestation.cardanoTip.slot < 0 ||
    !/^[a-f0-9]{64}$/u.test(attestation.cardanoTip.hash)
  ) {
    throw new Error("Phase 4 reset attestation has invalid Cardano tip");
  }
  if (
    phasRegistration.networkMagic !== isolation.networkMagic ||
    phasRegistration.observedAtTip.slot !== attestation.cardanoTip.slot ||
    phasRegistration.observedAtTip.hash !== attestation.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 reset PHAS registration proof is not bound to the frozen Cardano tip and network",
    );
  }
  if (
    !Number.isSafeInteger(attestation.kupoCheckpoint) ||
    attestation.kupoCheckpoint !== attestation.cardanoTip.slot
  ) {
    throw new Error(
      "Phase 4 reset attestation Kupo checkpoint must equal the Cardano tip slot",
    );
  }
  if (attestation.snapshotIdentitySha256 !== isolation.snapshotIdentitySha256) {
    throw new Error(
      `Phase 4 reset attestation snapshot identity mismatch: expected=${isolation.snapshotIdentitySha256},actual=${attestation.snapshotIdentitySha256}`,
    );
  }
  if (
    attestation.cardanoTip.slot !== isolation.snapshotCardanoTip.slot ||
    attestation.cardanoTip.hash !== isolation.snapshotCardanoTip.hash
  ) {
    throw new Error(
      `Phase 4 reset attestation frozen Cardano tip mismatch: expected=${isolation.snapshotCardanoTip.slot.toString()}:${isolation.snapshotCardanoTip.hash},actual=${attestation.cardanoTip.slot.toString()}:${attestation.cardanoTip.hash}`,
    );
  }
  if (attestation.kupoCheckpoint !== isolation.snapshotKupoCheckpoint) {
    throw new Error(
      `Phase 4 reset attestation frozen Kupo checkpoint mismatch: expected=${isolation.snapshotKupoCheckpoint.toString()},actual=${attestation.kupoCheckpoint.toString()}`,
    );
  }
  return attestation;
};

const loadPhase4ProcessIsolation = async (
  cwd: string,
): Promise<Phase4ProcessIsolationIdentity> => {
  const requestedEnvFile = requiredEnv("MIDGARD_PHASE4_PROCESS_ENV_FILE");
  const requestedManifest = requiredEnv(
    "MIDGARD_PHASE4_PROCESS_DEPLOYMENT_MANIFEST_PATH",
  );
  const requestedSnapshotIdentity = requiredValue(
    await loadDotenvFile(requestedEnvFile),
    "MIDGARD_PHASE4_SNAPSHOT_IDENTITY_PATH",
  );
  const requestedBlueprint = requiredValue(
    await loadDotenvFile(requestedEnvFile),
    "MIDGARD_REAL_BLUEPRINT_PATH",
  );
  if (!isAbsolute(requestedEnvFile) || !isAbsolute(requestedManifest)) {
    throw new Error(
      "Phase 4 process env and deployment manifest paths must be absolute",
    );
  }
  if (
    !isAbsolute(requestedSnapshotIdentity) ||
    !isAbsolute(requestedBlueprint)
  ) {
    throw new Error(
      "Phase 4 snapshot identity and Aiken blueprint paths must be absolute",
    );
  }
  const [
    envFile,
    deploymentManifestPath,
    snapshotIdentityPath,
    blueprintPath,
    defaultEnv,
    defaultManifest,
  ] = await Promise.all([
    realpath(requestedEnvFile),
    realpath(requestedManifest),
    realpath(requestedSnapshotIdentity),
    realpath(requestedBlueprint),
    realpath(resolve(cwd, ".env")),
    realpath(resolve(cwd, "deploymentInfo/contract-deployment-info.json")),
  ]);
  if (envFile === defaultEnv || deploymentManifestPath === defaultManifest) {
    throw new Error(
      "Phase 4 process acceptance refuses the checkout live env or deployment manifest",
    );
  }
  const values = await loadDotenvFile(envFile);
  if (
    Object.keys(values).some(
      (key) =>
        key.startsWith("MIDGARD_PHASE4_PROCESS_") ||
        key.startsWith("MIDGARD_PHASE4_T1_"),
    )
  ) {
    throw new Error(
      "Phase 4 process/T1 authorization controls may not be supplied by the child env file",
    );
  }
  const validated = validatePhase4ProcessIsolationValues(values);
  const deploymentManifestSha256 = await sha256File(deploymentManifestPath);
  const snapshotIdentitySha256 = await sha256File(snapshotIdentityPath);
  const snapshotIdentity = decodePhase4MatchedSnapshotIdentity(
    JSON.parse(await readFile(snapshotIdentityPath, "utf8")) as unknown,
  );
  if (
    snapshotIdentity.schemaVersion !==
      "midgard-phase4-matched-snapshot-identity-v1" ||
    snapshotIdentity.composeProject !== validated.composeProject ||
    snapshotIdentity.networkMagic !== validated.networkMagic ||
    snapshotIdentity.postgresDatabase !== validated.postgresDatabase ||
    snapshotIdentity.deploymentManifestSha256 !== deploymentManifestSha256 ||
    snapshotIdentity.blueprintSha256 !== (await sha256File(blueprintPath)) ||
    !Number.isSafeInteger(snapshotIdentity.cardanoTip?.slot) ||
    typeof snapshotIdentity.cardanoTip?.hash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(snapshotIdentity.cardanoTip.hash) ||
    !Number.isSafeInteger(snapshotIdentity.kupoCheckpoint) ||
    snapshotIdentity.kupoCheckpoint !== snapshotIdentity.cardanoTip.slot
  ) {
    throw new Error(
      "Phase 4 snapshot identity does not match the isolated run identity",
    );
  }
  const imageEntries = [
    snapshotIdentity.images?.cardanoNode,
    snapshotIdentity.images?.ogmios,
    snapshotIdentity.images?.kupo,
    snapshotIdentity.images?.postgres,
  ];
  if (
    imageEntries.some(
      (image) =>
        typeof image?.ref !== "string" ||
        !/@sha256:[a-f0-9]{64}$/u.test(image.ref) ||
        typeof image.id !== "string" ||
        image.id.trim().length === 0,
    )
  ) {
    throw new Error(
      "Phase 4 snapshot identity must bind immutable image refs and effective image IDs",
    );
  }
  for (const name of [
    "sourceSha256",
    "distSha256",
    "toolsSourceSha256",
    "toolsDistSha256",
    "genesisSha256",
    "configSha256",
    "acceptanceEnvSha256",
    "composeSha256",
    "phase4AssetsSha256",
    "phasRegistrationProofSha256",
  ] as const) {
    if (
      !/^[a-f0-9]{64}$/u.test(String(snapshotIdentity.artifacts?.[name] ?? ""))
    ) {
      throw new Error(
        `Phase 4 snapshot identity is missing artifact hash ${name}`,
      );
    }
  }
  const snapshotPhasRegistration = validatePhase4PhasRegistrationProof(
    snapshotIdentity.phasRegistration,
    "Phase 4 snapshot PHAS registration proof",
  );
  const snapshotTransactionBodyPath = await realpath(
    resolve(
      dirname(snapshotIdentityPath),
      "phas-registration-transaction-body.json",
    ),
  );
  const snapshotTransactionBodyBytes = await readFile(
    snapshotTransactionBodyPath,
  );
  if (
    createHash("sha256").update(snapshotTransactionBodyBytes).digest("hex") !==
    snapshotPhasRegistration.transactionBody.artifactSha256
  ) {
    throw new Error(
      "Phase 4 snapshot PHAS transaction-body artifact does not match its proof digest",
    );
  }
  const snapshotPhasRegistrationTransactionBody =
    validatePhase4PhasRegistrationTransactionBody(
      JSON.parse(snapshotTransactionBodyBytes.toString("utf8")) as unknown,
      snapshotPhasRegistration,
    );
  if (
    snapshotPhasRegistration.networkMagic !== validated.networkMagic ||
    snapshotPhasRegistration.cardanoImage.ref !==
      snapshotIdentity.images?.cardanoNode?.ref ||
    snapshotPhasRegistration.cardanoImage.id !==
      snapshotIdentity.images?.cardanoNode?.id ||
    snapshotPhasRegistration.observedAtTip.slot !==
      snapshotIdentity.cardanoTip.slot ||
    snapshotPhasRegistration.observedAtTip.hash !==
      snapshotIdentity.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 snapshot PHAS registration proof is not bound to the isolated ledger and pinned Cardano image",
    );
  }
  activeIsolatedChildEnv = buildPhase4IsolatedChildEnv({
    values,
    deploymentManifestPath,
  });
  // Database and Lucid layers execute in this controller process. Child
  // processes never inherit this mutable object; they use the frozen snapshot.
  Object.assign(process.env, values, {
    MIDGARD_DEPLOYMENT_MANIFEST_PATH: deploymentManifestPath,
  });
  return {
    envFile,
    deploymentManifestPath,
    deploymentManifestSha256,
    snapshotIdentityPath,
    snapshotIdentitySha256,
    snapshotCardanoTip: {
      slot: snapshotIdentity.cardanoTip.slot,
      hash: snapshotIdentity.cardanoTip.hash,
    },
    snapshotKupoCheckpoint: snapshotIdentity.kupoCheckpoint,
    snapshotBlueprintSha256: snapshotIdentity.blueprintSha256,
    snapshotPhasRegistrationProofSha256:
      snapshotIdentity.artifacts.phasRegistrationProofSha256,
    snapshotPhasRegistration,
    snapshotPhasRegistrationTransactionBody,
    ...validated,
  };
};

const positiveIntegerEnv = (name: string, fallback: number): number => {
  const raw = process.env[name];
  if (raw === undefined || raw.trim().length === 0) return fallback;
  return positiveSafeInteger(Number(raw), name);
};

const processAcceptanceTimeoutMs = (): number =>
  positiveIntegerEnv("MIDGARD_PHASE4_PROCESS_TIMEOUT_MS", 600_000);

const runProcess = ({
  command,
  args,
  cwd,
  env = isolatedChildEnv(),
  timeoutMs = processAcceptanceTimeoutMs(),
}: {
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly env?: NodeJS.ProcessEnv;
  readonly timeoutMs?: number;
}): Promise<ProcessResult> =>
  new Promise((resolvePromise, reject) => {
    const child = spawn(command, [...args], {
      cwd,
      env,
      shell: false,
      stdio: ["ignore", "pipe", "pipe"],
      detached: process.platform !== "win32",
    });
    let output = "";
    let timedOut = false;
    let forceKillTimer: NodeJS.Timeout | undefined;
    const terminate = (signal: NodeJS.Signals): void => {
      if (child.pid === undefined) return;
      try {
        if (process.platform === "win32") child.kill(signal);
        else process.kill(-child.pid, signal);
      } catch (error) {
        if (
          typeof error !== "object" ||
          error === null ||
          !("code" in error) ||
          error.code !== "ESRCH"
        ) {
          throw error;
        }
      }
    };
    const timeout = setTimeout(() => {
      timedOut = true;
      terminate("SIGTERM");
      forceKillTimer = setTimeout(() => terminate("SIGKILL"), 2_000);
    }, timeoutMs);
    child.stdout.on("data", (chunk: Buffer) => {
      output += chunk.toString("utf8");
    });
    child.stderr.on("data", (chunk: Buffer) => {
      output += chunk.toString("utf8");
    });
    child.on("error", (error) => {
      clearTimeout(timeout);
      if (forceKillTimer !== undefined) clearTimeout(forceKillTimer);
      reject(error);
    });
    child.on("close", (exitCode, signal) => {
      clearTimeout(timeout);
      if (forceKillTimer !== undefined) clearTimeout(forceKillTimer);
      resolvePromise({ exitCode, signal, output, timedOut });
    });
  });

const runRequiredProcess = async (
  input: Parameters<typeof runProcess>[0],
  label: string,
): Promise<string> => {
  const result = await runProcess(input);
  if (result.timedOut || result.exitCode !== 0 || result.signal !== null) {
    throw new Error(
      `${label} failed (timed_out=${String(result.timedOut)},exit=${result.exitCode?.toString() ?? "null"},signal=${result.signal ?? "none"}):\n${result.output}`,
    );
  }
  return result.output;
};

const assertPositivePreflightOutput = (label: string, output: string): void => {
  const negativeBoolean =
    /"(?:satisfied|ready|healthy|complete|configured)"\s*:\s*false/i;
  const negativeStatus =
    /"(?:status|verdict)"\s*:\s*"(?:failed|failure|missing|unsatisfied|blocked|unhealthy|not_ready)"/i;
  if (negativeBoolean.test(output) || negativeStatus.test(output)) {
    throw new Error(`${label} reported an unsatisfied preflight:\n${output}`);
  }
};

const assertExactGenesisPreflightOutput = (
  label: string,
  output: string,
): void => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(output.trim()) as unknown;
  } catch (cause) {
    throw new Error(`${label} must emit exactly one JSON V1 report`, { cause });
  }
  const report = decodePhase4GenesisLedgerReport(parsed);
  if (report.mode !== "verify" || report.status !== "already_present") {
    throw new Error(`${label} did not verify the exact existing genesis state`);
  }
};

const waitFor = async ({
  label,
  timeoutMs,
  probe,
}: {
  readonly label: string;
  readonly timeoutMs: number;
  readonly probe: () => Promise<boolean>;
}): Promise<void> => {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (await probe()) return;
    await new Promise((resolvePromise) => setTimeout(resolvePromise, 250));
  }
  throw new Error(
    `${label} did not become true within ${timeoutMs.toString()}ms`,
  );
};

const waitForReady = (port: number, timeoutMs: number): Promise<void> =>
  waitFor({
    label: `node ready on port ${port.toString()}`,
    timeoutMs,
    probe: async () => {
      const response = await fetch(
        `http://127.0.0.1:${port.toString()}/readyz`,
      ).catch(() => null);
      return response?.ok === true;
    },
  });

const waitForNewPayloadRetention = (
  existingTxIds: ReadonlySet<string>,
  timeoutMs: number,
): Promise<void> =>
  waitFor({
    label: "second L2 payload retained in mempool or processed_mempool",
    timeoutMs,
    probe: async () => {
      const state = await captureDatabaseState();
      return [...state.mempool, ...state.processed].some(
        (entry) => !existingTxIds.has(entry.txId),
      );
    },
  });

const waitForLogMarker = (
  path: string,
  marker: string,
  timeoutMs: number,
): Promise<void> =>
  waitFor({
    label: `log marker ${marker}`,
    timeoutMs,
    probe: async () =>
      readFile(path, "utf8").then(
        (text) => text.includes(marker),
        () => false,
      ),
  });

const captureDatabaseState = (): Promise<PipelinedCommitDatabaseState> =>
  Effect.runPromise(
    capturePipelinedCommitDatabaseState.pipe(Effect.provide(Database.layer)),
  );

const logicalDatabaseState = (state: PipelinedCommitDatabaseState) => ({
  activeJournalCount: state.activeJournalCount,
  activeJournal:
    state.activeJournal === null
      ? null
      : {
          headerHash: state.activeJournal.headerHash,
          headerCbor: state.activeJournal.headerCbor,
          journalPayloadIdentity: state.activeJournal.journalPayloadIdentity,
          baseTailHeaderHash: state.activeJournal.baseTailHeaderHash,
          baseTailOutRef: state.activeJournal.baseTailOutRef,
          baseTailDatumCbor: state.activeJournal.baseTailDatumCbor,
          baseRoots: state.activeJournal.baseRoots,
          expectedRoots: state.activeJournal.expectedRoots,
          mpfReplay: state.activeJournal.mpfReplay,
          leaseTokenPresent: state.activeJournal.leaseToken.length > 0,
          submittedTxHash: state.activeJournal.submittedTxHash,
          submitted: state.activeJournal.submittedTxHash !== null,
          status: state.activeJournal.status,
          depositCount: state.activeJournal.depositCount,
          mempoolTxCount: state.activeJournal.mempoolTxCount,
        },
  activeLease:
    state.activeLease === null
      ? null
      : {
          holder: state.activeLease.holder,
          status: state.activeLease.status,
          tokenPresent: state.activeLease.token.length > 0,
        },
  deposits: state.deposits.map((deposit) => ({
    status: deposit.status,
    projected: deposit.projectedHeaderHash !== null,
  })),
  mempool: state.mempool,
  processed: state.processed,
});

const assertSameLogicalDatabaseState = ({
  checkpoint,
  flagOn,
  flagOff,
}: {
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly flagOn: PipelinedCommitDatabaseState;
  readonly flagOff: PipelinedCommitDatabaseState;
}): void => {
  const on = JSON.stringify(logicalDatabaseState(flagOn));
  const off = JSON.stringify(logicalDatabaseState(flagOff));
  if (on !== off) {
    throw new Error(
      `Flag-on/flag-off logical DB state diverged for ${checkpoint}:\nflag_on=${on}\nflag_off=${off}`,
    );
  }
};

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
  USER_EVENT_BARRIER_REFRESH_INTERVAL_MS: "250",
  MIDGARD_DA_PAYLOAD_ENVELOPE: "off",
  MIDGARD_DEPLOYMENT_MANIFEST_PATH: requiredIsolatedChildEnv(
    "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  ),
  MIDGARD_DA_DEPLOYMENT_MANIFEST_PATH: undefined,
  RUN_GENESIS_ON_STARTUP: "false",
});

const makeNodeSpec = ({
  cwd,
  runDir,
  label,
  durableStoreLabel = label,
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
  /** Durable identity must remain stable across crash/restart labels. */
  readonly durableStoreLabel?: string;
  readonly nodeId: string;
  readonly port: number;
  readonly metricsPort: number;
  readonly timeoutMs: number;
  readonly commitIntervalMs?: number;
  readonly confirmationIntervalMs?: number;
}): PipelinedCommitNodeProcessSpec => {
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
    ledgerMpfDbPath: join(runDir, durableStoreLabel, nodeId, "ledger-mpf"),
    transactionsMpfDbPath: join(
      runDir,
      durableStoreLabel,
      nodeId,
      "transactions-mpf",
    ),
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

const submitL2Transfer = async ({
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

const resetAndPreflight = async ({
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

const seedBaseAndPayload = async ({
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

const runCrashCase = async ({
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

/** Executes the real matched-snapshot T1 rollback/advance/rebuild gate. */
const runT1RecoveryCase = async ({
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

const assertAcceptancePreconditions = async (
  cwd: string,
): Promise<Phase4ProcessIsolationIdentity> => {
  if (process.env.MIDGARD_DOTENV_MODE !== "disabled") {
    throw new Error(
      "Set MIDGARD_DOTENV_MODE=disabled before starting the Phase 4 acceptance controller",
    );
  }
  if (
    process.env.MIDGARD_PHASE4_PROCESS_ACCEPTANCE !== ACCEPTANCE_ENABLE_VALUE
  ) {
    throw new Error(
      `Set MIDGARD_PHASE4_PROCESS_ACCEPTANCE=${ACCEPTANCE_ENABLE_VALUE} to authorize the destructive matched-snapshot devnet acceptance run`,
    );
  }
  if (process.env.MIDGARD_PHASE4_PROCESS_TARGET !== "local-devnet") {
    throw new Error(
      "MIDGARD_PHASE4_PROCESS_TARGET must be local-devnet; this acceptance command refuses public-network mutation",
    );
  }
  requiredEnv("MIDGARD_PHASE4_MATCHED_RESET_COMMAND");
  requiredEnv("MIDGARD_PHASE4_T1_RECOVERY_COMMAND");
  const isolation = await loadPhase4ProcessIsolation(cwd);
  await Promise.all([
    access(isolation.envFile),
    access(resolve(cwd, "dist/index.js")),
    access(toolsCli()),
    access(isolation.deploymentManifestPath),
  ]);
  activeProcessIsolation = isolation;
  return isolation;
};

/**
 * This package's own root, located from the running module so the same code
 * resolves it from the built bundle (`dist/index.js`) and from source under
 * vitest. The devnet assets and the summary verifier live here, not in the
 * node root the acceptance run points at.
 */
const toolsPackageRoot = (): string => {
  let directory = dirname(fileURLToPath(import.meta.url));
  for (;;) {
    const candidate = join(directory, "package.json");
    if (existsSync(candidate)) {
      const manifest = JSON.parse(readFileSync(candidate, "utf8")) as {
        readonly name?: unknown;
      };
      if (manifest.name === "midgard-node-tools") {
        return directory;
      }
    }
    const parent = dirname(directory);
    if (parent === directory) {
      throw new Error(
        "midgard-node-tools package root not found above the acceptance module",
      );
    }
    directory = parent;
  }
};

/** The built tooling CLI, used to re-enter this package for gated steps. */
const toolsCli = (): string => resolve(toolsPackageRoot(), "dist/index.js");

const decodeProcessSummaryBeforePersistence = async (
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
