import { createHash } from "node:crypto";
import { readdir, readFile } from "node:fs/promises";
import { dirname, isAbsolute, join, resolve } from "node:path";
import { isDeepStrictEqual } from "node:util";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "@al-ft/midgard-fault-proofs";
import {
  type DeploymentManifestValue,
  parseDeploymentManifestValue,
} from "midgard-node/deployment-manifest-v1";

import type {
  DbEvidence,
  RawEvidenceRef,
  TransactionEvidence,
} from "../e2e/summary.js";
import type { E2EStateCorrectionAcceptance } from "./e2e-state-correction-acceptance.js";
import { REQUIRED_STATE_CORRECTION_GATE_LABELS } from "./e2e-state-correction-acceptance.js";

export const E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION =
  "midgard-e2e-authenticated-l1-tx-observation-v1" as const;
export const E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION =
  "midgard-e2e-state-correction-recovery-observation-v1" as const;
export const E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION =
  "midgard-e2e-state-correction-final-snapshot-v1" as const;

export type StateCorrectionIndependentSourcePaths = {
  readonly deploymentManifestPath: string;
  readonly blueprintPath: string;
  readonly cataloguePath: string;
  readonly parametersPath: string;
  readonly releaseEvidencePath: string;
  readonly workflowJournalDirectories: readonly string[];
  readonly l1ObservationPaths: readonly string[];
  readonly recoveryObservationPaths: readonly string[];
  readonly finalSnapshotPath: string;
};

export type StateCorrectionIndependentEvidence = {
  readonly db: readonly DbEvidence[];
  readonly transactions: readonly TransactionEvidence[];
  readonly rawEvidence: readonly RawEvidenceRef[];
  readonly notes: readonly string[];
};

/**
 * Non-artifact authority. Production callers must re-read the configured local
 * Cardano sources and database; returning success from data loaded out of the
 * evidence directory is not an implementation of this port.
 */
export interface StateCorrectionIndependentAuthority {
  authenticateTransaction(input: {
    readonly txHash: string;
    readonly kupoOutputIndex: number;
    readonly includedAt: ChainPoint;
    readonly observedAtTip: ChainPoint & {
      readonly confirmationDepth: number;
    };
    readonly rawSourceDigests: {
      readonly kupoResponseSha256: string;
      readonly ogmiosBlockResponseSha256: string;
      readonly ogmiosTipResponseSha256: string;
    };
  }): Promise<void>;
  authenticateFinalState(input: {
    readonly manifestId: string;
    readonly observedAt: ChainPoint & { readonly confirmationDepth: number };
    readonly stateQueueDepth: number;
    readonly unfinishedMutationJobs: number;
    readonly pendingFinalizations: number;
    readonly retainedProofTokens: readonly {
      readonly unit: string;
      readonly outRef: string;
    }[];
    readonly economics: readonly {
      readonly familyId: string;
      readonly removalTxHash: string;
      readonly kupoOutputIndex: number;
      readonly includedAt: ChainPoint;
      readonly referencedProofTokenOutRef: string;
      readonly operatorCredential: string;
      readonly proverCredential: string;
      readonly operatorBondInputOutRef: string | null;
      readonly operatorBondInputLovelace: string;
      readonly proverRewardOutputOutRef: string | null;
      readonly removalFeeLovelace: string;
      readonly slashedLovelace: string;
      readonly proverRewardLovelace: string;
    }[];
    readonly withdrawalReservePayout: {
      readonly payoutConcludeTxHash: string;
      readonly kupoOutputIndex: number;
      readonly includedAt: ChainPoint;
      readonly destination: string;
      readonly payoutValueSha256: string;
      readonly reserveValueSha256: string;
    };
    readonly snapshotDigest: string;
    readonly rawSourceDigests: {
      readonly kupoStateQueueResponseSha256: string;
      readonly kupoProofTokenResponseSha256s: readonly string[];
      readonly ogmiosTipResponseSha256: string;
      readonly nodeDatabaseExportSha256: string;
    };
  }): Promise<void>;
}

type JsonValue =
  | null
  | boolean
  | number
  | string
  | readonly JsonValue[]
  | { readonly [key: string]: JsonValue };

type ChainPoint = {
  readonly slot: string;
  readonly blockHash: string;
};

type AuthenticatedL1TxObservation = {
  readonly schemaVersion: typeof E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION;
  readonly runId: string;
  readonly network: "Preprod";
  readonly manifestId: string;
  readonly txHash: string;
  readonly includedAt: ChainPoint;
  readonly observedAtTip: ChainPoint & { readonly confirmationDepth: number };
  readonly authentication: {
    readonly source: "local-kupmios-ogmios";
    readonly kupoResponsePath: string;
    readonly kupoResponseSha256: string;
    readonly ogmiosBlockResponsePath: string;
    readonly ogmiosBlockResponseSha256: string;
    readonly ogmiosTipResponsePath: string;
    readonly ogmiosTipResponseSha256: string;
  };
};

type RecoveryObservation = {
  readonly schemaVersion: typeof E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION;
  readonly runId: string;
  readonly manifestId: string;
  readonly id: string;
  readonly beforeJournalSha256: string;
  readonly afterJournalSha256: string;
  readonly duplicateSubmissionCount: number;
  readonly lostEvidenceCount: number;
  readonly verifiedBeforeReconciliationCount: number;
  readonly unrecoverableWorkflowCount: number;
  readonly manualRepairCount: number;
  readonly terminalState: "recovered";
  readonly watcherState: "ready_after_reconciliation";
};

type FinalSnapshot = {
  readonly schemaVersion: typeof E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION;
  readonly runId: string;
  readonly network: "Preprod";
  readonly manifestId: string;
  readonly observedAt: ChainPoint & { readonly confirmationDepth: number };
  readonly authentication: {
    readonly source: "local-kupmios-ogmios-and-node-db";
    readonly kupoStateQueueResponsePath: string;
    readonly kupoStateQueueResponseSha256: string;
    readonly kupoProofTokenResponses: readonly {
      readonly unit: string;
      readonly outRef: string;
      readonly responsePath: string;
      readonly responseSha256: string;
    }[];
    readonly ogmiosTipResponsePath: string;
    readonly ogmiosTipResponseSha256: string;
    readonly nodeDatabaseExportPath: string;
    readonly nodeDatabaseExportSha256: string;
  };
  readonly stateQueue: {
    readonly depth: number;
    readonly fraudulentHeaderHashes: readonly string[];
  };
  readonly jobs: {
    readonly unfinishedMutationJobs: number;
    readonly pendingFinalizations: number;
  };
  readonly watcher: {
    readonly readiness: "ready";
    readonly verification: "resumed_after_reconciliation";
  };
  readonly economics: readonly {
    readonly familyId: string;
    readonly removalTxHash: string;
    readonly proofTokenUnit: string;
    readonly proofTokenOutRef: string;
    readonly removalReferencedProofTokenOutRef: string;
    readonly proofTokenFinalState: "retained";
    readonly operatorCredential: string;
    readonly proverCredential: string;
    readonly operatorBondInputOutRef: string | null;
    readonly operatorBondInputLovelace: string;
    readonly proverRewardOutputOutRef: string | null;
    readonly removalFeeLovelace: string;
    readonly slashedLovelace: string;
    readonly proverRewardLovelace: string;
    readonly duplicateRewardCount: number;
  }[];
  readonly withdrawalReservePayout: {
    readonly withdrawalOrderTxHash: string;
    readonly reserveTxHash: string;
    readonly payoutInitTxHash: string;
    readonly payoutAddTxHashes: readonly string[];
    readonly payoutConcludeTxHash: string;
    readonly destination: string;
    readonly payoutValueSha256: string;
    readonly reserveValueSha256: string;
    readonly status: "paid";
  };
  readonly forcedClassifications: readonly {
    readonly direction: "valid-marked-invalid" | "invalid-marked-valid";
    readonly evidenceTxHash: string;
    readonly correctionTxHash: string;
    readonly canonicalClassification: "valid" | "invalid";
    readonly finalClassification: "valid" | "invalid";
  }[];
};

type NodeDatabaseExport = Pick<
  FinalSnapshot,
  | "runId"
  | "manifestId"
  | "stateQueue"
  | "jobs"
  | "watcher"
  | "economics"
  | "withdrawalReservePayout"
  | "forcedClassifications"
> & {
  readonly schemaVersion: "midgard-e2e-state-correction-node-db-export-v1";
};

type LoadedWorkflow = {
  readonly directory: string;
  readonly digest: string;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly terminal: FraudProofWorkflowTerminal;
  readonly confirmedTxHashes: ReadonlySet<string>;
  readonly entryPaths: readonly string[];
};

const record = (
  value: unknown,
  field: string,
): Readonly<Record<string, unknown>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  keys: readonly string[],
  field: string,
): void => {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `${field} must contain exactly: ${expected.join(", ")}; found: ${actual.join(", ")}`,
    );
  }
};

const canonicalString = (value: unknown, field: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${field} must be a non-empty canonical string`);
  }
  return value;
};

const exactString = <T extends string>(
  value: unknown,
  expected: T,
  field: string,
): T => {
  if (value !== expected) {
    throw new Error(`${field} must be ${expected}`);
  }
  return expected;
};

const nonNegativeInteger = (value: unknown, field: string): number => {
  if (!Number.isSafeInteger(value) || Number(value) < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return Number(value);
};

const positiveInteger = (value: unknown, field: string): number => {
  const parsed = nonNegativeInteger(value, field);
  if (parsed < 1) throw new Error(`${field} must be positive`);
  return parsed;
};

const lowerHex = (value: unknown, bytes: number, field: string): string => {
  const parsed = canonicalString(value, field);
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(parsed)) {
    throw new Error(`${field} must be ${bytes.toString()}-byte lowercase hex`);
  }
  return parsed;
};

const sha256Hex = (value: unknown, field: string): string =>
  lowerHex(value, 32, field);

const canonicalAssetUnit = (value: unknown, field: string): string => {
  const parsed = canonicalString(value, field);
  if (!/^[0-9a-f]{56,120}$/u.test(parsed)) {
    throw new Error(`${field} must be a canonical Cardano asset unit`);
  }
  return parsed;
};

const canonicalOutRef = (value: unknown, field: string): string => {
  const parsed = canonicalString(value, field);
  if (!/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(parsed)) {
    throw new Error(`${field} must be a canonical Cardano output reference`);
  }
  return parsed;
};

const canonicalLovelace = (value: unknown, field: string): string => {
  const parsed = canonicalString(value, field);
  if (!/^(?:0|[1-9][0-9]*)$/u.test(parsed)) {
    throw new Error(`${field} must be canonical non-negative lovelace`);
  }
  return parsed;
};

const stringArray = (
  value: unknown,
  field: string,
  parse: (entry: unknown, field: string) => string = canonicalString,
): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${field} must be an array`);
  return value.map((entry, index) =>
    parse(entry, `${field}[${index.toString()}]`),
  );
};

const parseChainPoint = (value: unknown, field: string): ChainPoint => {
  const candidate = record(value, field);
  exactKeys(candidate, ["slot", "blockHash"], field);
  const slot = canonicalString(candidate.slot, `${field}.slot`);
  if (!/^(?:0|[1-9][0-9]*)$/u.test(slot)) {
    throw new Error(`${field}.slot must be a canonical non-negative integer`);
  }
  return {
    slot,
    blockHash: sha256Hex(candidate.blockHash, `${field}.blockHash`),
  };
};

const parseObservedChainPoint = (
  value: unknown,
  field: string,
): ChainPoint & { readonly confirmationDepth: number } => {
  const candidate = record(value, field);
  exactKeys(candidate, ["slot", "blockHash", "confirmationDepth"], field);
  return {
    ...parseChainPoint(
      { slot: candidate.slot, blockHash: candidate.blockHash },
      field,
    ),
    confirmationDepth: positiveInteger(
      candidate.confirmationDepth,
      `${field}.confirmationDepth`,
    ),
  };
};

const normalizeJson = (value: unknown, field: string): JsonValue => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value) || !Number.isSafeInteger(value)) {
      throw new Error(`${field} numbers must be finite safe integers`);
    }
    return value;
  }
  if (Array.isArray(value)) {
    return value.map((entry, index) =>
      normalizeJson(entry, `${field}[${index.toString()}]`),
    );
  }
  const candidate = record(value, field);
  return Object.fromEntries(
    Object.entries(candidate).map(([key, entry]) => [
      key,
      normalizeJson(entry, `${field}.${key}`),
    ]),
  );
};

const stableJson = (value: JsonValue): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  return `{${Object.entries(value)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([key, child]) => `${JSON.stringify(key)}:${stableJson(child)}`)
    .join(",")}}`;
};

const sha256 = (value: string | Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const jsonDigest = (value: unknown): string =>
  sha256(stableJson(normalizeJson(value, "digest input")));

const readJson = async (path: string): Promise<unknown> =>
  JSON.parse(await readFile(path, "utf8")) as unknown;

const referencedPath = (parentPath: string, childPath: string): string =>
  isAbsolute(childPath) ? childPath : resolve(dirname(parentPath), childPath);

const readDigestCheckedJson = async ({
  parentPath,
  childPath,
  expectedSha256,
  field,
}: {
  readonly parentPath: string;
  readonly childPath: string;
  readonly expectedSha256: string;
  readonly field: string;
}): Promise<{ readonly path: string; readonly value: unknown }> => {
  const path = referencedPath(parentPath, childPath);
  const bytes = await readFile(path);
  assertEqual(sha256(bytes), expectedSha256, `${field} digest`);
  try {
    return { path, value: JSON.parse(bytes.toString("utf8")) as unknown };
  } catch (cause) {
    throw new Error(`${field} must contain JSON`, { cause });
  }
};

type ParsedKupoMatch = {
  readonly transactionId: string;
  readonly outputIndex: number;
  readonly createdAt: ChainPoint;
  readonly spentAt: unknown;
  readonly assets: Readonly<Record<string, unknown>>;
};

const parseKupoMatches = (
  value: unknown,
  field: string,
): readonly ParsedKupoMatch[] => {
  if (!Array.isArray(value)) throw new Error(`${field} must be an array`);
  return value.map((entry, index) => {
    const itemField = `${field}[${index.toString()}]`;
    const item = record(entry, itemField);
    const createdAt = record(item.created_at, `${itemField}.created_at`);
    const rawSlot = nonNegativeInteger(
      createdAt.slot_no,
      `${itemField}.created_at.slot_no`,
    );
    const valueRecord = record(item.value, `${itemField}.value`);
    const assets = record(valueRecord.assets, `${itemField}.value.assets`);
    return {
      transactionId: sha256Hex(
        item.transaction_id,
        `${itemField}.transaction_id`,
      ),
      outputIndex: nonNegativeInteger(
        item.output_index,
        `${itemField}.output_index`,
      ),
      createdAt: {
        slot: rawSlot.toString(),
        blockHash: sha256Hex(
          createdAt.header_hash,
          `${itemField}.created_at.header_hash`,
        ),
      },
      spentAt: item.spent_at,
      assets,
    };
  });
};

const unwrapOgmiosResult = (value: unknown, field: string): unknown => {
  const candidate = record(value, field);
  return Object.hasOwn(candidate, "result") ? candidate.result : candidate;
};

type ParsedOgmiosBlock = {
  readonly point: ChainPoint;
  readonly height: number;
  readonly transactionIds: ReadonlySet<string>;
};

const parseOgmiosBlock = (value: unknown, field: string): ParsedOgmiosBlock => {
  const result = record(unwrapOgmiosResult(value, field), `${field}.result`);
  const block = record(
    Object.hasOwn(result, "block") ? result.block : result,
    `${field}.block`,
  );
  if (!Array.isArray(block.transactions)) {
    throw new Error(`${field}.block.transactions must be an array`);
  }
  const transactionIds = new Set(
    block.transactions.map((transaction, index) =>
      sha256Hex(
        record(transaction, `${field}.block.transactions[${index.toString()}]`)
          .id,
        `${field}.block.transactions[${index.toString()}].id`,
      ),
    ),
  );
  return {
    point: {
      slot: nonNegativeInteger(block.slot, `${field}.block.slot`).toString(),
      blockHash: sha256Hex(block.id, `${field}.block.id`),
    },
    height: nonNegativeInteger(block.height, `${field}.block.height`),
    transactionIds,
  };
};

type ParsedOgmiosTip = ChainPoint & { readonly height: number };

const parseOgmiosTip = (value: unknown, field: string): ParsedOgmiosTip => {
  const result = record(unwrapOgmiosResult(value, field), `${field}.result`);
  const tip = record(
    Object.hasOwn(result, "tip") ? result.tip : result,
    `${field}.tip`,
  );
  return {
    slot: nonNegativeInteger(tip.slot, `${field}.tip.slot`).toString(),
    blockHash: sha256Hex(tip.id, `${field}.tip.id`),
    height: nonNegativeInteger(tip.height, `${field}.tip.height`),
  };
};

type DerivedL1Observation = {
  readonly observation: AuthenticatedL1TxObservation;
  readonly kupoOutputIndex: number;
  readonly inclusionHeight: number;
  readonly rawPaths: readonly string[];
};

const deriveAuthenticatedL1Observation = async ({
  observation,
  observationPath,
  authority,
}: {
  readonly observation: AuthenticatedL1TxObservation;
  readonly observationPath: string;
  readonly authority: StateCorrectionIndependentAuthority;
}): Promise<DerivedL1Observation> => {
  const [kupoRaw, ogmiosBlockRaw, ogmiosTipRaw] = await Promise.all([
    readDigestCheckedJson({
      parentPath: observationPath,
      childPath: observation.authentication.kupoResponsePath,
      expectedSha256: observation.authentication.kupoResponseSha256,
      field: `${observation.txHash} raw Kupo response`,
    }),
    readDigestCheckedJson({
      parentPath: observationPath,
      childPath: observation.authentication.ogmiosBlockResponsePath,
      expectedSha256: observation.authentication.ogmiosBlockResponseSha256,
      field: `${observation.txHash} raw Ogmios block response`,
    }),
    readDigestCheckedJson({
      parentPath: observationPath,
      childPath: observation.authentication.ogmiosTipResponsePath,
      expectedSha256: observation.authentication.ogmiosTipResponseSha256,
      field: `${observation.txHash} raw Ogmios tip response`,
    }),
  ]);
  const kupoMatches = parseKupoMatches(
    kupoRaw.value,
    `${observation.txHash} raw Kupo response`,
  );
  if (kupoMatches.length === 0) {
    throw new Error(`${observation.txHash} raw Kupo response has no matches`);
  }
  const matchingKupo = kupoMatches.filter(
    (match) => match.transactionId === observation.txHash,
  );
  if (matchingKupo.length !== kupoMatches.length) {
    throw new Error(
      `${observation.txHash} raw Kupo response contains another transaction`,
    );
  }
  const kupoPoint = matchingKupo[0]!.createdAt;
  for (const match of matchingKupo) {
    assertEqual(
      match.createdAt,
      kupoPoint,
      `${observation.txHash} Kupo creation point agreement`,
    );
  }
  const ogmiosBlock = parseOgmiosBlock(
    ogmiosBlockRaw.value,
    `${observation.txHash} raw Ogmios block response`,
  );
  assertEqual(
    ogmiosBlock.point,
    kupoPoint,
    `${observation.txHash} Kupo/Ogmios inclusion point`,
  );
  if (!ogmiosBlock.transactionIds.has(observation.txHash)) {
    throw new Error(
      `${observation.txHash} is absent from its raw Ogmios block response`,
    );
  }
  const ogmiosTip = parseOgmiosTip(
    ogmiosTipRaw.value,
    `${observation.txHash} raw Ogmios tip response`,
  );
  if (ogmiosTip.height < ogmiosBlock.height) {
    throw new Error(`${observation.txHash} Ogmios tip precedes inclusion`);
  }
  const includedAt = kupoPoint;
  const observedAtTip = {
    slot: ogmiosTip.slot,
    blockHash: ogmiosTip.blockHash,
    confirmationDepth: ogmiosTip.height - ogmiosBlock.height + 1,
  };
  assertEqual(
    observation.includedAt,
    includedAt,
    `${observation.txHash} claimed/raw inclusion point`,
  );
  assertEqual(
    observation.observedAtTip,
    observedAtTip,
    `${observation.txHash} claimed/raw tip observation`,
  );
  const rawSourceDigests = {
    kupoResponseSha256: observation.authentication.kupoResponseSha256,
    ogmiosBlockResponseSha256:
      observation.authentication.ogmiosBlockResponseSha256,
    ogmiosTipResponseSha256: observation.authentication.ogmiosTipResponseSha256,
  };
  await authority.authenticateTransaction({
    txHash: observation.txHash,
    kupoOutputIndex: matchingKupo[0]!.outputIndex,
    includedAt,
    observedAtTip,
    rawSourceDigests,
  });
  return {
    observation: { ...observation, includedAt, observedAtTip },
    kupoOutputIndex: matchingKupo[0]!.outputIndex,
    inclusionHeight: ogmiosBlock.height,
    rawPaths: [kupoRaw.path, ogmiosBlockRaw.path, ogmiosTipRaw.path],
  };
};

const parseAuthenticatedL1Observation = (
  value: unknown,
  field: string,
): AuthenticatedL1TxObservation => {
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "network",
      "manifestId",
      "txHash",
      "includedAt",
      "observedAtTip",
      "authentication",
    ],
    field,
  );
  const authentication = record(
    candidate.authentication,
    `${field}.authentication`,
  );
  exactKeys(
    authentication,
    [
      "source",
      "kupoResponsePath",
      "kupoResponseSha256",
      "ogmiosBlockResponsePath",
      "ogmiosBlockResponseSha256",
      "ogmiosTipResponsePath",
      "ogmiosTipResponseSha256",
    ],
    `${field}.authentication`,
  );
  return {
    schemaVersion: exactString(
      candidate.schemaVersion,
      E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION,
      `${field}.schemaVersion`,
    ),
    runId: canonicalString(candidate.runId, `${field}.runId`),
    network: exactString(candidate.network, "Preprod", `${field}.network`),
    manifestId: sha256Hex(candidate.manifestId, `${field}.manifestId`),
    txHash: sha256Hex(candidate.txHash, `${field}.txHash`),
    includedAt: parseChainPoint(candidate.includedAt, `${field}.includedAt`),
    observedAtTip: parseObservedChainPoint(
      candidate.observedAtTip,
      `${field}.observedAtTip`,
    ),
    authentication: {
      source: exactString(
        authentication.source,
        "local-kupmios-ogmios",
        `${field}.authentication.source`,
      ),
      kupoResponsePath: canonicalString(
        authentication.kupoResponsePath,
        `${field}.authentication.kupoResponsePath`,
      ),
      kupoResponseSha256: sha256Hex(
        authentication.kupoResponseSha256,
        `${field}.authentication.kupoResponseSha256`,
      ),
      ogmiosBlockResponsePath: canonicalString(
        authentication.ogmiosBlockResponsePath,
        `${field}.authentication.ogmiosBlockResponsePath`,
      ),
      ogmiosBlockResponseSha256: sha256Hex(
        authentication.ogmiosBlockResponseSha256,
        `${field}.authentication.ogmiosBlockResponseSha256`,
      ),
      ogmiosTipResponsePath: canonicalString(
        authentication.ogmiosTipResponsePath,
        `${field}.authentication.ogmiosTipResponsePath`,
      ),
      ogmiosTipResponseSha256: sha256Hex(
        authentication.ogmiosTipResponseSha256,
        `${field}.authentication.ogmiosTipResponseSha256`,
      ),
    },
  };
};

const parseRecoveryObservation = (
  value: unknown,
  field: string,
): RecoveryObservation => {
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "manifestId",
      "id",
      "beforeJournalSha256",
      "afterJournalSha256",
      "duplicateSubmissionCount",
      "lostEvidenceCount",
      "verifiedBeforeReconciliationCount",
      "unrecoverableWorkflowCount",
      "manualRepairCount",
      "terminalState",
      "watcherState",
    ],
    field,
  );
  return {
    schemaVersion: exactString(
      candidate.schemaVersion,
      E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION,
      `${field}.schemaVersion`,
    ),
    runId: canonicalString(candidate.runId, `${field}.runId`),
    manifestId: sha256Hex(candidate.manifestId, `${field}.manifestId`),
    id: canonicalString(candidate.id, `${field}.id`),
    beforeJournalSha256: sha256Hex(
      candidate.beforeJournalSha256,
      `${field}.beforeJournalSha256`,
    ),
    afterJournalSha256: sha256Hex(
      candidate.afterJournalSha256,
      `${field}.afterJournalSha256`,
    ),
    duplicateSubmissionCount: nonNegativeInteger(
      candidate.duplicateSubmissionCount,
      `${field}.duplicateSubmissionCount`,
    ),
    lostEvidenceCount: nonNegativeInteger(
      candidate.lostEvidenceCount,
      `${field}.lostEvidenceCount`,
    ),
    verifiedBeforeReconciliationCount: nonNegativeInteger(
      candidate.verifiedBeforeReconciliationCount,
      `${field}.verifiedBeforeReconciliationCount`,
    ),
    unrecoverableWorkflowCount: nonNegativeInteger(
      candidate.unrecoverableWorkflowCount,
      `${field}.unrecoverableWorkflowCount`,
    ),
    manualRepairCount: nonNegativeInteger(
      candidate.manualRepairCount,
      `${field}.manualRepairCount`,
    ),
    terminalState: exactString(
      candidate.terminalState,
      "recovered",
      `${field}.terminalState`,
    ),
    watcherState: exactString(
      candidate.watcherState,
      "ready_after_reconciliation",
      `${field}.watcherState`,
    ),
  };
};

const parseFinalSnapshot = (value: unknown): FinalSnapshot => {
  const field = "final snapshot";
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "network",
      "manifestId",
      "observedAt",
      "authentication",
      "stateQueue",
      "jobs",
      "watcher",
      "economics",
      "withdrawalReservePayout",
      "forcedClassifications",
    ],
    field,
  );
  const stateQueue = record(candidate.stateQueue, `${field}.stateQueue`);
  exactKeys(
    stateQueue,
    ["depth", "fraudulentHeaderHashes"],
    `${field}.stateQueue`,
  );
  const jobs = record(candidate.jobs, `${field}.jobs`);
  exactKeys(
    jobs,
    ["unfinishedMutationJobs", "pendingFinalizations"],
    `${field}.jobs`,
  );
  const watcher = record(candidate.watcher, `${field}.watcher`);
  exactKeys(watcher, ["readiness", "verification"], `${field}.watcher`);
  const authentication = record(
    candidate.authentication,
    `${field}.authentication`,
  );
  exactKeys(
    authentication,
    [
      "source",
      "kupoStateQueueResponsePath",
      "kupoStateQueueResponseSha256",
      "kupoProofTokenResponses",
      "ogmiosTipResponsePath",
      "ogmiosTipResponseSha256",
      "nodeDatabaseExportPath",
      "nodeDatabaseExportSha256",
    ],
    `${field}.authentication`,
  );
  if (!Array.isArray(authentication.kupoProofTokenResponses)) {
    throw new Error(
      `${field}.authentication.kupoProofTokenResponses must be an array`,
    );
  }
  const kupoProofTokenResponses = authentication.kupoProofTokenResponses.map(
    (value, index) => {
      const itemField = `${field}.authentication.kupoProofTokenResponses[${index.toString()}]`;
      const item = record(value, itemField);
      exactKeys(
        item,
        ["unit", "outRef", "responsePath", "responseSha256"],
        itemField,
      );
      return {
        unit: canonicalAssetUnit(item.unit, `${itemField}.unit`),
        outRef: canonicalOutRef(item.outRef, `${itemField}.outRef`),
        responsePath: canonicalString(
          item.responsePath,
          `${itemField}.responsePath`,
        ),
        responseSha256: sha256Hex(
          item.responseSha256,
          `${itemField}.responseSha256`,
        ),
      };
    },
  );
  if (!Array.isArray(candidate.economics)) {
    throw new Error(`${field}.economics must be an array`);
  }
  const economics = candidate.economics.map((value, index) => {
    const itemField = `${field}.economics[${index.toString()}]`;
    const item = record(value, itemField);
    exactKeys(
      item,
      [
        "familyId",
        "removalTxHash",
        "proofTokenUnit",
        "proofTokenOutRef",
        "removalReferencedProofTokenOutRef",
        "proofTokenFinalState",
        "operatorCredential",
        "proverCredential",
        "operatorBondInputOutRef",
        "operatorBondInputLovelace",
        "proverRewardOutputOutRef",
        "removalFeeLovelace",
        "slashedLovelace",
        "proverRewardLovelace",
        "duplicateRewardCount",
      ],
      itemField,
    );
    return {
      familyId: canonicalString(item.familyId, `${itemField}.familyId`),
      removalTxHash: sha256Hex(
        item.removalTxHash,
        `${itemField}.removalTxHash`,
      ),
      proofTokenUnit: canonicalAssetUnit(
        item.proofTokenUnit,
        `${itemField}.proofTokenUnit`,
      ),
      proofTokenOutRef: canonicalOutRef(
        item.proofTokenOutRef,
        `${itemField}.proofTokenOutRef`,
      ),
      removalReferencedProofTokenOutRef: canonicalOutRef(
        item.removalReferencedProofTokenOutRef,
        `${itemField}.removalReferencedProofTokenOutRef`,
      ),
      proofTokenFinalState: exactString(
        item.proofTokenFinalState,
        "retained",
        `${itemField}.proofTokenFinalState`,
      ),
      operatorCredential: canonicalString(
        item.operatorCredential,
        `${itemField}.operatorCredential`,
      ),
      proverCredential: canonicalString(
        item.proverCredential,
        `${itemField}.proverCredential`,
      ),
      operatorBondInputOutRef:
        item.operatorBondInputOutRef === null
          ? null
          : canonicalOutRef(
              item.operatorBondInputOutRef,
              `${itemField}.operatorBondInputOutRef`,
            ),
      operatorBondInputLovelace: canonicalLovelace(
        item.operatorBondInputLovelace,
        `${itemField}.operatorBondInputLovelace`,
      ),
      proverRewardOutputOutRef:
        item.proverRewardOutputOutRef === null
          ? null
          : canonicalOutRef(
              item.proverRewardOutputOutRef,
              `${itemField}.proverRewardOutputOutRef`,
            ),
      removalFeeLovelace: canonicalLovelace(
        item.removalFeeLovelace,
        `${itemField}.removalFeeLovelace`,
      ),
      slashedLovelace: canonicalLovelace(
        item.slashedLovelace,
        `${itemField}.slashedLovelace`,
      ),
      proverRewardLovelace: canonicalLovelace(
        item.proverRewardLovelace,
        `${itemField}.proverRewardLovelace`,
      ),
      duplicateRewardCount: nonNegativeInteger(
        item.duplicateRewardCount,
        `${itemField}.duplicateRewardCount`,
      ),
    };
  });
  const withdrawal = record(
    candidate.withdrawalReservePayout,
    `${field}.withdrawalReservePayout`,
  );
  exactKeys(
    withdrawal,
    [
      "withdrawalOrderTxHash",
      "reserveTxHash",
      "payoutInitTxHash",
      "payoutAddTxHashes",
      "payoutConcludeTxHash",
      "destination",
      "payoutValueSha256",
      "reserveValueSha256",
      "status",
    ],
    `${field}.withdrawalReservePayout`,
  );
  if (!Array.isArray(candidate.forcedClassifications)) {
    throw new Error(`${field}.forcedClassifications must be an array`);
  }
  const forcedClassifications = candidate.forcedClassifications.map(
    (value, index) => {
      const itemField = `${field}.forcedClassifications[${index.toString()}]`;
      const item = record(value, itemField);
      exactKeys(
        item,
        [
          "direction",
          "evidenceTxHash",
          "correctionTxHash",
          "canonicalClassification",
          "finalClassification",
        ],
        itemField,
      );
      const rawDirection = canonicalString(
        item.direction,
        `${itemField}.direction`,
      );
      if (
        rawDirection !== "valid-marked-invalid" &&
        rawDirection !== "invalid-marked-valid"
      ) {
        throw new Error(`${itemField}.direction is unsupported`);
      }
      const direction: "valid-marked-invalid" | "invalid-marked-valid" =
        rawDirection;
      const rawCanonicalClassification = canonicalString(
        item.canonicalClassification,
        `${itemField}.canonicalClassification`,
      );
      const rawFinalClassification = canonicalString(
        item.finalClassification,
        `${itemField}.finalClassification`,
      );
      if (
        (rawCanonicalClassification !== "valid" &&
          rawCanonicalClassification !== "invalid") ||
        (rawFinalClassification !== "valid" &&
          rawFinalClassification !== "invalid")
      ) {
        throw new Error(
          `${itemField} classifications must be valid or invalid`,
        );
      }
      const canonicalClassification: "valid" | "invalid" =
        rawCanonicalClassification;
      const finalClassification: "valid" | "invalid" = rawFinalClassification;
      return {
        direction,
        evidenceTxHash: sha256Hex(
          item.evidenceTxHash,
          `${itemField}.evidenceTxHash`,
        ),
        correctionTxHash: sha256Hex(
          item.correctionTxHash,
          `${itemField}.correctionTxHash`,
        ),
        canonicalClassification,
        finalClassification,
      };
    },
  );
  return {
    schemaVersion: exactString(
      candidate.schemaVersion,
      E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
      `${field}.schemaVersion`,
    ),
    runId: canonicalString(candidate.runId, `${field}.runId`),
    network: exactString(candidate.network, "Preprod", `${field}.network`),
    manifestId: sha256Hex(candidate.manifestId, `${field}.manifestId`),
    observedAt: parseObservedChainPoint(
      candidate.observedAt,
      `${field}.observedAt`,
    ),
    authentication: {
      source: exactString(
        authentication.source,
        "local-kupmios-ogmios-and-node-db",
        `${field}.authentication.source`,
      ),
      kupoStateQueueResponsePath: canonicalString(
        authentication.kupoStateQueueResponsePath,
        `${field}.authentication.kupoStateQueueResponsePath`,
      ),
      kupoStateQueueResponseSha256: sha256Hex(
        authentication.kupoStateQueueResponseSha256,
        `${field}.authentication.kupoStateQueueResponseSha256`,
      ),
      kupoProofTokenResponses,
      ogmiosTipResponsePath: canonicalString(
        authentication.ogmiosTipResponsePath,
        `${field}.authentication.ogmiosTipResponsePath`,
      ),
      ogmiosTipResponseSha256: sha256Hex(
        authentication.ogmiosTipResponseSha256,
        `${field}.authentication.ogmiosTipResponseSha256`,
      ),
      nodeDatabaseExportPath: canonicalString(
        authentication.nodeDatabaseExportPath,
        `${field}.authentication.nodeDatabaseExportPath`,
      ),
      nodeDatabaseExportSha256: sha256Hex(
        authentication.nodeDatabaseExportSha256,
        `${field}.authentication.nodeDatabaseExportSha256`,
      ),
    },
    stateQueue: {
      depth: nonNegativeInteger(stateQueue.depth, `${field}.stateQueue.depth`),
      fraudulentHeaderHashes: stringArray(
        stateQueue.fraudulentHeaderHashes,
        `${field}.stateQueue.fraudulentHeaderHashes`,
        (entry, entryField) => lowerHex(entry, 28, entryField),
      ),
    },
    jobs: {
      unfinishedMutationJobs: nonNegativeInteger(
        jobs.unfinishedMutationJobs,
        `${field}.jobs.unfinishedMutationJobs`,
      ),
      pendingFinalizations: nonNegativeInteger(
        jobs.pendingFinalizations,
        `${field}.jobs.pendingFinalizations`,
      ),
    },
    watcher: {
      readiness: exactString(
        watcher.readiness,
        "ready",
        `${field}.watcher.readiness`,
      ),
      verification: exactString(
        watcher.verification,
        "resumed_after_reconciliation",
        `${field}.watcher.verification`,
      ),
    },
    economics,
    withdrawalReservePayout: {
      withdrawalOrderTxHash: sha256Hex(
        withdrawal.withdrawalOrderTxHash,
        `${field}.withdrawalReservePayout.withdrawalOrderTxHash`,
      ),
      reserveTxHash: sha256Hex(
        withdrawal.reserveTxHash,
        `${field}.withdrawalReservePayout.reserveTxHash`,
      ),
      payoutInitTxHash: sha256Hex(
        withdrawal.payoutInitTxHash,
        `${field}.withdrawalReservePayout.payoutInitTxHash`,
      ),
      payoutAddTxHashes: stringArray(
        withdrawal.payoutAddTxHashes,
        `${field}.withdrawalReservePayout.payoutAddTxHashes`,
        sha256Hex,
      ),
      payoutConcludeTxHash: sha256Hex(
        withdrawal.payoutConcludeTxHash,
        `${field}.withdrawalReservePayout.payoutConcludeTxHash`,
      ),
      destination: canonicalString(
        withdrawal.destination,
        `${field}.withdrawalReservePayout.destination`,
      ),
      payoutValueSha256: sha256Hex(
        withdrawal.payoutValueSha256,
        `${field}.withdrawalReservePayout.payoutValueSha256`,
      ),
      reserveValueSha256: sha256Hex(
        withdrawal.reserveValueSha256,
        `${field}.withdrawalReservePayout.reserveValueSha256`,
      ),
      status: exactString(
        withdrawal.status,
        "paid",
        `${field}.withdrawalReservePayout.status`,
      ),
    },
    forcedClassifications,
  };
};

const parseNodeDatabaseExport = (value: unknown): NodeDatabaseExport => {
  const field = "raw node database export";
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "manifestId",
      "stateQueue",
      "jobs",
      "watcher",
      "economics",
      "withdrawalReservePayout",
      "forcedClassifications",
    ],
    field,
  );
  exactString(
    candidate.schemaVersion,
    "midgard-e2e-state-correction-node-db-export-v1",
    `${field}.schemaVersion`,
  );
  const parsed = parseFinalSnapshot({
    schemaVersion: E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
    runId: candidate.runId,
    network: "Preprod",
    manifestId: candidate.manifestId,
    observedAt: {
      slot: "0",
      blockHash: "00".repeat(32),
      confirmationDepth: 1,
    },
    authentication: {
      source: "local-kupmios-ogmios-and-node-db",
      kupoStateQueueResponsePath: "not-used",
      kupoStateQueueResponseSha256: "00".repeat(32),
      kupoProofTokenResponses: [],
      ogmiosTipResponsePath: "not-used",
      ogmiosTipResponseSha256: "00".repeat(32),
      nodeDatabaseExportPath: "not-used",
      nodeDatabaseExportSha256: "00".repeat(32),
    },
    stateQueue: candidate.stateQueue,
    jobs: candidate.jobs,
    watcher: candidate.watcher,
    economics: candidate.economics,
    withdrawalReservePayout: candidate.withdrawalReservePayout,
    forcedClassifications: candidate.forcedClassifications,
  });
  return {
    schemaVersion: "midgard-e2e-state-correction-node-db-export-v1",
    runId: parsed.runId,
    manifestId: parsed.manifestId,
    stateQueue: parsed.stateQueue,
    jobs: parsed.jobs,
    watcher: parsed.watcher,
    economics: parsed.economics,
    withdrawalReservePayout: parsed.withdrawalReservePayout,
    forcedClassifications: parsed.forcedClassifications,
  };
};

const workflowDigest = (
  names: readonly string[],
  contents: readonly string[],
): string =>
  sha256(
    names
      .map((name, index) => `${name}:${sha256(contents[index] ?? "")}`)
      .join("\n"),
  );

const loadWorkflow = async (directory: string): Promise<LoadedWorkflow> => {
  const names = (await readdir(directory))
    .filter((name) => /^\d{8}\.json$/u.test(name))
    .sort();
  if (names.length === 0) {
    throw new Error(`workflow journal ${directory} has no immutable entries`);
  }
  names.forEach((name, index) => {
    const expected = `${index.toString().padStart(8, "0")}.json`;
    if (name !== expected) {
      throw new Error(
        `workflow journal ${directory} entry gap: expected ${expected}, found ${name}`,
      );
    }
  });
  const entryPaths = names.map((name) => join(directory, name));
  const contents = await Promise.all(
    entryPaths.map((path) => readFile(path, "utf8")),
  );
  const entries = contents.map(
    (content) => JSON.parse(content) as FraudProofWorkflowJournalEntry,
  );
  const workflowId = entries[0]?.workflowId;
  if (workflowId === undefined)
    throw new Error(`workflow journal ${directory} is empty`);
  validateFraudProofWorkflowJournal({ workflowId, entries });
  const completed = entries.filter((entry) => entry.event.kind === "completed");
  const last = entries.at(-1);
  if (
    completed.length !== 1 ||
    last?.event.kind !== "completed" ||
    completed[0]?.event.kind !== "completed"
  ) {
    throw new Error(
      `workflow journal ${directory} must have exactly one terminal completed event and it must be last`,
    );
  }
  const terminalEvent = completed[0].event;
  const terminalDigest = journalJsonDigest(
    normalizeJournalJson(
      terminalEvent.terminal,
      "acceptance workflow terminal",
    ),
  );
  if (terminalDigest !== terminalEvent.terminalDigest) {
    throw new Error(`workflow journal ${directory} terminal digest mismatch`);
  }
  for (const entry of entries) {
    if (
      entry.event.kind === "prepared" &&
      journalJsonDigest(entry.event.artifact) !== entry.event.artifactDigest
    ) {
      throw new Error(`workflow journal ${directory} prepared digest mismatch`);
    }
  }
  const preflightTxByAction = new Map<string, string>();
  const intentTxByAction = new Map<string, string>();
  const observedTxByAction = new Map<string, string>();
  const confirmedActions = new Set<string>();
  for (const entry of entries) {
    const event = entry.event;
    if (event.kind === "preflight_passed") {
      preflightTxByAction.set(event.actionId, event.txHash);
    } else if (event.kind === "submission_intent") {
      if (preflightTxByAction.get(event.actionId) !== event.txHash) {
        throw new Error(
          `workflow journal ${directory} intent ${event.actionId} is not bound to its passed preflight body`,
        );
      }
      intentTxByAction.set(event.actionId, event.txHash);
    } else if (event.kind === "submitted") {
      if (intentTxByAction.get(event.actionId) !== event.txHash) {
        throw new Error(
          `workflow journal ${directory} submitted ${event.actionId} without a matching durable intent`,
        );
      }
      observedTxByAction.set(event.actionId, event.txHash);
    } else if (
      event.kind === "reconciled" &&
      event.outcome === "confirmed" &&
      event.txHash !== undefined
    ) {
      if (intentTxByAction.get(event.actionId) !== event.txHash) {
        throw new Error(
          `workflow journal ${directory} reconciled ${event.actionId} without a matching durable intent`,
        );
      }
      observedTxByAction.set(event.actionId, event.txHash);
    } else if (event.kind === "confirmed") {
      if (
        confirmedActions.has(event.actionId) ||
        observedTxByAction.get(event.actionId) !== event.txHash
      ) {
        throw new Error(
          `workflow journal ${directory} confirmed ${event.actionId} without one matching submitted/reconciled transaction`,
        );
      }
      confirmedActions.add(event.actionId);
    }
  }
  const confirmedTxHashes = new Set(
    entries.flatMap((entry) =>
      entry.event.kind === "confirmed" ? [entry.event.txHash] : [],
    ),
  );
  return {
    directory,
    digest: workflowDigest(names, contents),
    entries,
    terminal: terminalEvent.terminal,
    confirmedTxHashes,
    entryPaths,
  };
};

const assertEqual = (
  actual: unknown,
  expected: unknown,
  field: string,
): void => {
  if (!isDeepStrictEqual(actual, expected)) {
    throw new Error(
      `${field} mismatch: expected=${JSON.stringify(expected)} actual=${JSON.stringify(actual)}`,
    );
  }
};

const manifestCatalogue = (
  manifest: DeploymentManifestValue,
): NonNullable<
  DeploymentManifestValue["contracts"][string]["fraudProofCatalogue"]
> => {
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error("deployment manifest has no fraud-proof catalogue");
  }
  return catalogue;
};

type RequiredTransaction = {
  readonly label: string;
  readonly txHash: string;
};

const requiredTransactions = (
  claim: E2EStateCorrectionAcceptance,
): readonly RequiredTransaction[] => [
  ...claim.families.flatMap((family) => [
    { label: `fault-proof:${family.familyId}:init`, txHash: family.initTxHash },
    ...family.proofStepTxHashes.map((txHash, index) => ({
      label: `fault-proof:${family.familyId}:step-${(index + 1).toString()}`,
      txHash,
    })),
    {
      label: `fault-proof:${family.familyId}:proof-token`,
      txHash: family.proofTokenTxHash,
    },
    {
      label: `fault-proof:${family.familyId}:removal`,
      txHash: family.removalTxHash,
    },
    {
      label: `fault-proof:${family.familyId}:correction`,
      txHash: family.correctionTxHash,
    },
  ]),
  {
    label: "withdrawal-order",
    txHash: claim.withdrawalReservePayout.withdrawalOrderTxHash,
  },
  {
    label: "withdrawal-reserve",
    txHash: claim.withdrawalReservePayout.reserveTxHash,
  },
  {
    label: "payout-init",
    txHash: claim.withdrawalReservePayout.payoutInitTxHash,
  },
  ...claim.withdrawalReservePayout.payoutAddTxHashes.map((txHash, index) => ({
    label: `payout-add-${(index + 1).toString()}`,
    txHash,
  })),
  {
    label: "payout-conclude",
    txHash: claim.withdrawalReservePayout.payoutConcludeTxHash,
  },
  ...claim.forcedClassifications.flatMap((drill) => [
    {
      label: `forced-classification:${drill.direction}:evidence`,
      txHash: drill.evidenceTxHash,
    },
    {
      label: `forced-classification:${drill.direction}:correction`,
      txHash: drill.correctionTxHash,
    },
  ]),
];

export const reconcileStateCorrectionIndependentEvidence = async ({
  expectedRunId,
  claim,
  paths,
  authority,
}: {
  readonly expectedRunId: string;
  readonly claim: E2EStateCorrectionAcceptance;
  readonly paths: StateCorrectionIndependentSourcePaths;
  readonly authority: StateCorrectionIndependentAuthority;
}): Promise<StateCorrectionIndependentEvidence> => {
  assertEqual(claim.runId, expectedRunId, "state-correction acceptance run");
  const [
    manifestValue,
    blueprintBytes,
    catalogueValue,
    parametersValue,
    releaseEvidenceBytes,
    workflows,
    l1Values,
    recoveryValues,
    finalSnapshotValue,
  ] = await Promise.all([
    readJson(paths.deploymentManifestPath),
    readFile(paths.blueprintPath),
    readJson(paths.cataloguePath),
    readJson(paths.parametersPath),
    readFile(paths.releaseEvidencePath),
    Promise.all(paths.workflowJournalDirectories.map(loadWorkflow)),
    Promise.all(paths.l1ObservationPaths.map(readJson)),
    Promise.all(paths.recoveryObservationPaths.map(readJson)),
    readJson(paths.finalSnapshotPath),
  ]);

  const manifest = parseDeploymentManifestValue(manifestValue);
  if (manifest.network !== "Preprod") {
    throw new Error(
      `deployment manifest network must be Preprod, found ${manifest.network}`,
    );
  }
  const catalogueCandidate = record(catalogueValue, "catalogue source");
  exactKeys(catalogueCandidate, ["root", "categories"], "catalogue source");
  const catalogue = manifestCatalogue(manifest);
  assertEqual(catalogueValue, catalogue, "independent catalogue source");
  const blueprintSha256 = sha256(blueprintBytes);
  const parametersSha256 = computeDeploymentManifestJsonDigest(parametersValue);
  const releaseEvidenceSha256 = sha256(releaseEvidenceBytes);
  assertEqual(
    manifest.manifestId,
    claim.deployment.manifestId,
    "manifest identity",
  );
  assertEqual(
    manifest.proofEvidence.blueprintHash,
    blueprintSha256,
    "manifest/blueprint identity",
  );
  assertEqual(
    claim.deployment.blueprintSha256,
    blueprintSha256,
    "claim blueprint identity",
  );
  assertEqual(catalogue.root, claim.deployment.catalogueRoot, "catalogue root");
  assertEqual(
    manifest.cardanoProtocolParameters.snapshot,
    parametersValue,
    "protocol parameter snapshot",
  );
  assertEqual(
    manifest.cardanoProtocolParameters.digest,
    parametersSha256,
    "manifest parameter digest",
  );
  assertEqual(
    claim.deployment.parametersSha256,
    parametersSha256,
    "claim parameter digest",
  );
  assertEqual(
    manifest.proofEvidence.digest,
    releaseEvidenceSha256,
    "manifest release evidence identity",
  );
  assertEqual(
    claim.deployment.releaseEvidenceSha256,
    releaseEvidenceSha256,
    "claim release evidence identity",
  );

  if (workflows.length !== claim.families.length) {
    throw new Error(
      `workflow journal count mismatch: expected ${claim.families.length.toString()}, found ${workflows.length.toString()}`,
    );
  }
  const workflowByCategory = new Map(
    workflows.map((workflow) => [
      workflow.entries[0]!.identity.category,
      workflow,
    ]),
  );
  if (workflowByCategory.size !== workflows.length) {
    throw new Error("workflow journals contain duplicate family categories");
  }
  const terminals = new Map<string, FraudProofWorkflowTerminal>();
  for (const family of claim.families) {
    const workflow = workflowByCategory.get(
      family.familyId as Parameters<typeof workflowByCategory.get>[0],
    );
    if (workflow === undefined) {
      throw new Error(`missing workflow journal for ${family.familyId}`);
    }
    const identity = workflow.entries[0]!.identity;
    const terminal = workflow.terminal;
    assertEqual(
      identity.deploymentFingerprint,
      manifest.manifestId,
      `${family.familyId} workflow deployment`,
    );
    assertEqual(
      identity.target,
      { kind: "state_queue_header", headerHash: family.headerHash },
      `${family.familyId} workflow target`,
    );
    assertEqual(
      terminal.category,
      family.familyId,
      `${family.familyId} terminal category`,
    );
    assertEqual(
      terminal.headerHash,
      family.headerHash,
      `${family.familyId} terminal header`,
    );
    assertEqual(
      terminal.proofToken.createdByTxHash,
      family.proofTokenTxHash,
      `${family.familyId} proof-token creation`,
    );
    assertEqual(
      terminal.proofToken.retainedAtFinalState,
      true,
      `${family.familyId} permanent proof-token retention`,
    );
    assertEqual(
      terminal.correction.removalTxHash,
      family.removalTxHash,
      `${family.familyId} removal`,
    );
    assertEqual(
      terminal.correction.referencedProofTokenOutRef,
      terminal.proofToken.outRef,
      `${family.familyId} removal proof-token reference`,
    );
    assertEqual(
      family.correctionTxHash,
      family.removalTxHash,
      `${family.familyId} correction/removal identity`,
    );
    assertEqual(
      terminal.economics.slashedLovelace,
      family.expectedSlashLovelace,
      `${family.familyId} exact slash`,
    );
    assertEqual(
      terminal.economics.proverRewardLovelace,
      family.expectedProverRewardLovelace,
      `${family.familyId} exact prover reward`,
    );
    assertEqual(
      {
        slot: terminal.observedAt.slot,
        blockHash: terminal.observedAt.blockHash,
      },
      family.chainPoint,
      `${family.familyId} final chain point`,
    );
    const familyTxHashes = new Set([
      family.initTxHash,
      ...family.proofStepTxHashes,
      family.proofTokenTxHash,
      family.removalTxHash,
      family.correctionTxHash,
    ]);
    for (const txHash of familyTxHashes) {
      if (!workflow.confirmedTxHashes.has(txHash)) {
        throw new Error(
          `${family.familyId} required transaction ${txHash} is not confirmed in its journal`,
        );
      }
    }
    if (workflow.confirmedTxHashes.size !== familyTxHashes.size) {
      throw new Error(
        `${family.familyId} workflow has confirmed transactions omitted from the aggregate claim`,
      );
    }
    terminals.set(family.familyId, terminal);
  }

  const claimedL1Observations = l1Values.map((value, index) =>
    parseAuthenticatedL1Observation(
      value,
      `L1 observation ${paths.l1ObservationPaths[index] ?? index.toString()}`,
    ),
  );
  const derivedL1Observations = await Promise.all(
    claimedL1Observations.map((observation, index) =>
      deriveAuthenticatedL1Observation({
        observation,
        observationPath: paths.l1ObservationPaths[index]!,
        authority,
      }),
    ),
  );
  const l1Observations = derivedL1Observations.map(
    (derived) => derived.observation,
  );
  const observationByTxHash = new Map(
    l1Observations.map((observation, index) => [
      observation.txHash,
      { observation, path: paths.l1ObservationPaths[index]! },
    ]),
  );
  if (observationByTxHash.size !== l1Observations.length) {
    throw new Error(
      "authenticated L1 observations contain duplicate transaction hashes",
    );
  }
  const required = requiredTransactions(claim);
  const requiredHashes = new Set(required.map((entry) => entry.txHash));
  for (const requiredTx of required) {
    const observed = observationByTxHash.get(requiredTx.txHash)?.observation;
    if (observed === undefined) {
      throw new Error(
        `required transaction ${requiredTx.label}:${requiredTx.txHash} has no authenticated L1 observation`,
      );
    }
    assertEqual(
      observed.runId,
      claim.runId,
      `${requiredTx.label} observation run`,
    );
    assertEqual(
      observed.manifestId,
      manifest.manifestId,
      `${requiredTx.label} observation deployment`,
    );
  }
  if (observationByTxHash.size !== requiredHashes.size) {
    throw new Error(
      "authenticated L1 observations contain transactions outside the required Q57 set",
    );
  }
  for (const family of claim.families) {
    const terminal = terminals.get(family.familyId)!;
    const removalObservation = observationByTxHash.get(
      family.removalTxHash,
    )!.observation;
    assertEqual(
      removalObservation.observedAtTip,
      terminal.observedAt,
      `${family.familyId} authenticated terminal chain observation`,
    );
  }
  const terminalPoint = (txHash: string): ChainPoint => {
    const observed = observationByTxHash.get(txHash)?.observation.observedAtTip;
    if (observed === undefined) {
      throw new Error(
        `missing authenticated terminal observation for ${txHash}`,
      );
    }
    return { slot: observed.slot, blockHash: observed.blockHash };
  };
  assertEqual(
    terminalPoint(claim.withdrawalReservePayout.payoutConcludeTxHash),
    claim.withdrawalReservePayout.chainPoint,
    "withdrawal payout authenticated terminal chain point",
  );
  for (const drill of claim.forcedClassifications) {
    assertEqual(
      terminalPoint(drill.correctionTxHash),
      drill.chainPoint,
      `${drill.direction} authenticated terminal chain point`,
    );
  }

  if (recoveryValues.length !== claim.recoveryDrills.length) {
    throw new Error(
      `recovery observation count mismatch: expected ${claim.recoveryDrills.length.toString()}, found ${recoveryValues.length.toString()}`,
    );
  }
  const recoveryObservations = recoveryValues.map((value, index) =>
    parseRecoveryObservation(
      value,
      `recovery observation ${paths.recoveryObservationPaths[index] ?? index.toString()}`,
    ),
  );
  for (const [index, observed] of recoveryObservations.entries()) {
    const claimed = claim.recoveryDrills[index]!;
    assertEqual(
      observed.id,
      claimed.id,
      `recovery observation ${index.toString()} id`,
    );
    assertEqual(observed.runId, claim.runId, `${observed.id} run`);
    assertEqual(
      observed.manifestId,
      manifest.manifestId,
      `${observed.id} deployment`,
    );
    for (const [field, count] of Object.entries({
      duplicateSubmissionCount: observed.duplicateSubmissionCount,
      lostEvidenceCount: observed.lostEvidenceCount,
      verifiedBeforeReconciliationCount:
        observed.verifiedBeforeReconciliationCount,
      unrecoverableWorkflowCount: observed.unrecoverableWorkflowCount,
      manualRepairCount: observed.manualRepairCount,
    })) {
      if (count !== 0) throw new Error(`${observed.id} ${field} must be zero`);
    }
    const path = paths.recoveryObservationPaths[index]!;
    assertEqual(
      claimed.evidenceSha256,
      sha256(await readFile(path)),
      `${observed.id} raw evidence digest`,
    );
  }

  const finalSnapshot = parseFinalSnapshot(finalSnapshotValue);
  assertEqual(finalSnapshot.runId, claim.runId, "final snapshot run");
  assertEqual(
    finalSnapshot.manifestId,
    manifest.manifestId,
    "final snapshot deployment",
  );
  const finalAuthentication = finalSnapshot.authentication;
  const [
    rawStateQueue,
    rawFinalOgmiosTip,
    rawNodeDatabaseExport,
    rawProofTokens,
  ] = await Promise.all([
    readDigestCheckedJson({
      parentPath: paths.finalSnapshotPath,
      childPath: finalAuthentication.kupoStateQueueResponsePath,
      expectedSha256: finalAuthentication.kupoStateQueueResponseSha256,
      field: "final raw Kupo state-queue response",
    }),
    readDigestCheckedJson({
      parentPath: paths.finalSnapshotPath,
      childPath: finalAuthentication.ogmiosTipResponsePath,
      expectedSha256: finalAuthentication.ogmiosTipResponseSha256,
      field: "final raw Ogmios tip response",
    }),
    readDigestCheckedJson({
      parentPath: paths.finalSnapshotPath,
      childPath: finalAuthentication.nodeDatabaseExportPath,
      expectedSha256: finalAuthentication.nodeDatabaseExportSha256,
      field: "final raw node database export",
    }),
    Promise.all(
      finalAuthentication.kupoProofTokenResponses.map((response, index) =>
        readDigestCheckedJson({
          parentPath: paths.finalSnapshotPath,
          childPath: response.responsePath,
          expectedSha256: response.responseSha256,
          field: `final raw Kupo proof-token response ${index.toString()}`,
        }),
      ),
    ),
  ]);
  const rawStateQueueMatches = parseKupoMatches(
    rawStateQueue.value,
    "final raw Kupo state-queue response",
  );
  if (rawStateQueueMatches.length !== 0) {
    throw new Error("final raw Kupo state-queue response is not drained");
  }
  const expectedRetainedProofTokens = claim.families.map((family) => {
    const token = terminals.get(family.familyId)!.proofToken;
    return { unit: token.unit, outRef: token.outRef };
  });
  assertEqual(
    finalAuthentication.kupoProofTokenResponses.map(({ unit, outRef }) => ({
      unit,
      outRef,
    })),
    expectedRetainedProofTokens,
    "final Kupo retained proof-token query set",
  );
  const proofTokenKeys = new Set<string>();
  for (const [index, rawProofToken] of rawProofTokens.entries()) {
    const declared = finalAuthentication.kupoProofTokenResponses[index]!;
    const key = `${declared.unit}:${declared.outRef}`;
    if (proofTokenKeys.has(key)) {
      throw new Error(`duplicate final Kupo proof-token query ${key}`);
    }
    proofTokenKeys.add(key);
    const matches = parseKupoMatches(
      rawProofToken.value,
      `final raw Kupo proof-token response ${index.toString()}`,
    );
    if (matches.length !== 1) {
      throw new Error(
        `final raw Kupo proof-token response ${index.toString()} must contain exactly one match`,
      );
    }
    const match = matches[0]!;
    const separator = declared.outRef.lastIndexOf("#");
    const expectedTxHash = declared.outRef.slice(0, separator);
    const expectedOutputIndex = Number(declared.outRef.slice(separator + 1));
    assertEqual(
      { txHash: match.transactionId, outputIndex: match.outputIndex },
      { txHash: expectedTxHash, outputIndex: expectedOutputIndex },
      `final retained proof-token ${key} output reference`,
    );
    if (match.spentAt !== null) {
      throw new Error(`final permanent proof token ${key} is spent`);
    }
    const matchingAssetEntries = Object.entries(match.assets).filter(
      ([assetKey]) => assetKey.replaceAll(".", "") === declared.unit,
    );
    if (
      matchingAssetEntries.length !== 1 ||
      canonicalLovelace(
        matchingAssetEntries[0]?.[1],
        `final retained proof-token ${key} quantity`,
      ) !== "1"
    ) {
      throw new Error(
        `final permanent proof token ${key} is missing or not retained at quantity one`,
      );
    }
  }
  const finalOgmiosTip = parseOgmiosTip(
    rawFinalOgmiosTip.value,
    "final raw Ogmios tip response",
  );
  const newestRequiredInclusionHeight = Math.max(
    ...derivedL1Observations.map((derived) => derived.inclusionHeight),
  );
  if (finalOgmiosTip.height < newestRequiredInclusionHeight) {
    throw new Error(
      "final raw Ogmios tip precedes a required Q57 transaction inclusion",
    );
  }
  const derivedFinalObservedAt = {
    slot: finalOgmiosTip.slot,
    blockHash: finalOgmiosTip.blockHash,
    confirmationDepth:
      finalOgmiosTip.height - newestRequiredInclusionHeight + 1,
  };
  assertEqual(
    finalSnapshot.observedAt,
    derivedFinalObservedAt,
    "final snapshot/raw Ogmios observation",
  );
  const nodeDatabaseExport = parseNodeDatabaseExport(
    rawNodeDatabaseExport.value,
  );
  assertEqual(
    nodeDatabaseExport.runId,
    claim.runId,
    "node database export run",
  );
  assertEqual(
    nodeDatabaseExport.manifestId,
    manifest.manifestId,
    "node database export deployment",
  );
  assertEqual(
    nodeDatabaseExport.stateQueue,
    { depth: rawStateQueueMatches.length, fraudulentHeaderHashes: [] },
    "Kupo/node database final state queue",
  );
  assertEqual(
    {
      stateQueue: finalSnapshot.stateQueue,
      jobs: finalSnapshot.jobs,
      watcher: finalSnapshot.watcher,
      economics: finalSnapshot.economics,
      withdrawalReservePayout: finalSnapshot.withdrawalReservePayout,
      forcedClassifications: finalSnapshot.forcedClassifications,
    },
    {
      stateQueue: nodeDatabaseExport.stateQueue,
      jobs: nodeDatabaseExport.jobs,
      watcher: nodeDatabaseExport.watcher,
      economics: nodeDatabaseExport.economics,
      withdrawalReservePayout: nodeDatabaseExport.withdrawalReservePayout,
      forcedClassifications: nodeDatabaseExport.forcedClassifications,
    },
    "final snapshot/raw node database state",
  );
  const finalSnapshotDigest = jsonDigest(finalSnapshotValue);
  const transactionAuthorityInput = (txHash: string) => {
    const derived = derivedL1Observations.find(
      (entry) => entry.observation.txHash === txHash,
    );
    if (derived === undefined) {
      throw new Error(`missing derived L1 authority input for ${txHash}`);
    }
    return {
      kupoOutputIndex: derived.kupoOutputIndex,
      includedAt: derived.observation.includedAt,
    };
  };
  await authority.authenticateFinalState({
    manifestId: manifest.manifestId,
    observedAt: derivedFinalObservedAt,
    stateQueueDepth: nodeDatabaseExport.stateQueue.depth,
    unfinishedMutationJobs: nodeDatabaseExport.jobs.unfinishedMutationJobs,
    pendingFinalizations: nodeDatabaseExport.jobs.pendingFinalizations,
    retainedProofTokens: expectedRetainedProofTokens,
    economics: finalSnapshot.economics.map((entry) => ({
      familyId: entry.familyId,
      removalTxHash: entry.removalTxHash,
      ...transactionAuthorityInput(entry.removalTxHash),
      referencedProofTokenOutRef: entry.removalReferencedProofTokenOutRef,
      operatorCredential: entry.operatorCredential,
      proverCredential: entry.proverCredential,
      operatorBondInputOutRef: entry.operatorBondInputOutRef,
      operatorBondInputLovelace: entry.operatorBondInputLovelace,
      proverRewardOutputOutRef: entry.proverRewardOutputOutRef,
      removalFeeLovelace: entry.removalFeeLovelace,
      slashedLovelace: entry.slashedLovelace,
      proverRewardLovelace: entry.proverRewardLovelace,
    })),
    withdrawalReservePayout: {
      payoutConcludeTxHash:
        finalSnapshot.withdrawalReservePayout.payoutConcludeTxHash,
      ...transactionAuthorityInput(
        finalSnapshot.withdrawalReservePayout.payoutConcludeTxHash,
      ),
      destination: finalSnapshot.withdrawalReservePayout.destination,
      payoutValueSha256:
        finalSnapshot.withdrawalReservePayout.payoutValueSha256,
      reserveValueSha256:
        finalSnapshot.withdrawalReservePayout.reserveValueSha256,
    },
    snapshotDigest: finalSnapshotDigest,
    rawSourceDigests: {
      kupoStateQueueResponseSha256:
        finalAuthentication.kupoStateQueueResponseSha256,
      kupoProofTokenResponseSha256s:
        finalAuthentication.kupoProofTokenResponses.map(
          (response) => response.responseSha256,
        ),
      ogmiosTipResponseSha256: finalAuthentication.ogmiosTipResponseSha256,
      nodeDatabaseExportSha256: finalAuthentication.nodeDatabaseExportSha256,
    },
  });
  if (
    finalSnapshot.stateQueue.depth !== 0 ||
    finalSnapshot.stateQueue.fraudulentHeaderHashes.length !== 0 ||
    finalSnapshot.jobs.unfinishedMutationJobs !== 0 ||
    finalSnapshot.jobs.pendingFinalizations !== 0
  ) {
    throw new Error("final chain/queue state is not drained and corrected");
  }
  assertEqual(
    finalSnapshot.economics.map((entry) => entry.familyId),
    claim.families.map((family) => family.familyId),
    "final economics family order",
  );
  for (const [index, observed] of finalSnapshot.economics.entries()) {
    const family = claim.families[index]!;
    const terminal = terminals.get(family.familyId)!;
    assertEqual(
      observed.removalTxHash,
      family.removalTxHash,
      `${family.familyId} economic removal`,
    );
    assertEqual(
      observed.proofTokenUnit,
      terminal.proofToken.unit,
      `${family.familyId} retained proof-token unit`,
    );
    assertEqual(
      observed.proofTokenOutRef,
      terminal.proofToken.outRef,
      `${family.familyId} retained proof-token outref`,
    );
    assertEqual(
      observed.removalReferencedProofTokenOutRef,
      terminal.correction.referencedProofTokenOutRef,
      `${family.familyId} snapshot removal proof-token reference`,
    );
    assertEqual(
      observed.proofTokenFinalState,
      "retained",
      `${family.familyId} snapshot proof-token final state`,
    );
    assertEqual(
      observed.operatorCredential,
      terminal.economics.operatorCredential,
      `${family.familyId} operator credential`,
    );
    assertEqual(
      observed.proverCredential,
      terminal.economics.proverCredential,
      `${family.familyId} prover credential`,
    );
    assertEqual(
      observed.operatorBondInputOutRef,
      terminal.economics.operatorBondInputOutRef,
      `${family.familyId} operator bond input outref`,
    );
    assertEqual(
      observed.operatorBondInputLovelace,
      terminal.economics.operatorBondInputLovelace,
      `${family.familyId} operator bond input lovelace`,
    );
    assertEqual(
      observed.proverRewardOutputOutRef,
      terminal.economics.proverRewardOutputOutRef,
      `${family.familyId} prover reward output outref`,
    );
    assertEqual(
      observed.removalFeeLovelace,
      terminal.economics.removalFeeLovelace,
      `${family.familyId} removal fee`,
    );
    assertEqual(
      observed.slashedLovelace,
      family.expectedSlashLovelace,
      `${family.familyId} snapshot slash`,
    );
    assertEqual(
      observed.proverRewardLovelace,
      family.expectedProverRewardLovelace,
      `${family.familyId} snapshot reward`,
    );
    if (observed.duplicateRewardCount !== 0) {
      throw new Error(
        `${family.familyId} final snapshot observed a duplicate reward`,
      );
    }
  }
  const withdrawalClaim = claim.withdrawalReservePayout;
  assertEqual(
    finalSnapshot.withdrawalReservePayout,
    {
      withdrawalOrderTxHash: withdrawalClaim.withdrawalOrderTxHash,
      reserveTxHash: withdrawalClaim.reserveTxHash,
      payoutInitTxHash: withdrawalClaim.payoutInitTxHash,
      payoutAddTxHashes: withdrawalClaim.payoutAddTxHashes,
      payoutConcludeTxHash: withdrawalClaim.payoutConcludeTxHash,
      destination: withdrawalClaim.expectedDestination,
      payoutValueSha256: withdrawalClaim.expectedPayoutValueSha256,
      reserveValueSha256: withdrawalClaim.expectedReserveValueSha256,
      status: "paid",
    },
    "final withdrawal/reserve/payout state",
  );
  assertEqual(
    finalSnapshot.forcedClassifications,
    claim.forcedClassifications.map((drill) => ({
      direction: drill.direction,
      evidenceTxHash: drill.evidenceTxHash,
      correctionTxHash: drill.correctionTxHash,
      canonicalClassification: drill.canonicalClassification,
      finalClassification: drill.finalClassification,
    })),
    "forced classification final state",
  );
  assertEqual(
    claim.finalState.finalStateSha256,
    finalSnapshotDigest,
    "final snapshot digest",
  );

  const identityDetails = {
    runId: claim.runId,
    manifestId: manifest.manifestId,
    blueprintSha256,
    catalogueRoot: catalogue.root,
    parametersSha256,
    releaseEvidenceSha256,
  };
  const satisfied = (
    label: string,
    details: Readonly<Record<string, string>>,
  ): DbEvidence => ({
    label,
    status: "satisfied",
    source: "independent-state-correction-reconciliation-v1",
    details: { ...identityDetails, ...details },
  });
  const transactions = required.map((entry) => ({
    label: entry.label,
    txHash: entry.txHash,
    status: "confirmed" as const,
    source: `authenticated-l1:${observationByTxHash.get(entry.txHash)!.path}`,
  }));
  const rawEvidence: RawEvidenceRef[] = [
    {
      label: "state-correction-deployment-manifest",
      path: paths.deploymentManifestPath,
    },
    { label: "state-correction-blueprint", path: paths.blueprintPath },
    { label: "state-correction-catalogue", path: paths.cataloguePath },
    { label: "state-correction-parameters", path: paths.parametersPath },
    {
      label: "state-correction-release-evidence",
      path: paths.releaseEvidencePath,
    },
    ...workflows.flatMap((workflow) => [
      {
        label: `workflow-journal:${workflow.entries[0]!.identity.category}`,
        path: workflow.directory,
      },
      ...workflow.entryPaths.map((path, index) => ({
        label: `workflow-journal-entry:${workflow.entries[0]!.identity.category}:${index.toString()}`,
        path,
      })),
    ]),
    ...paths.l1ObservationPaths.map((path, index) => ({
      label: `authenticated-l1-observation:${index.toString()}`,
      path,
    })),
    ...derivedL1Observations.flatMap((derived, observationIndex) =>
      derived.rawPaths.map((path, rawIndex) => ({
        label: `authenticated-l1-raw:${observationIndex.toString()}:${rawIndex.toString()}`,
        path,
      })),
    ),
    ...paths.recoveryObservationPaths.map((path, index) => ({
      label: `state-correction-recovery:${claim.recoveryDrills[index]!.id}`,
      path,
    })),
    { label: "state-correction-final-snapshot", path: paths.finalSnapshotPath },
    {
      label: "state-correction-final-kupo-state-queue-raw",
      path: rawStateQueue.path,
    },
    ...rawProofTokens.map((raw, index) => ({
      label: `state-correction-final-kupo-proof-token-raw:${index.toString()}`,
      path: raw.path,
    })),
    {
      label: "state-correction-final-ogmios-tip-raw",
      path: rawFinalOgmiosTip.path,
    },
    {
      label: "state-correction-final-node-database-export-raw",
      path: rawNodeDatabaseExport.path,
    },
  ];
  return {
    db: [
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[0], {
        familyCount: claim.families.length.toString(),
        workflowJournalDigests: workflows
          .map((workflow) => workflow.digest)
          .join(","),
        authenticatedL1TransactionCount: requiredHashes.size.toString(),
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[1], {
        reconciledFamilies: claim.families.length.toString(),
        duplicateRewards: "0",
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[2], {
        destination: finalSnapshot.withdrawalReservePayout.destination,
        payoutValueSha256:
          finalSnapshot.withdrawalReservePayout.payoutValueSha256,
        reserveValueSha256:
          finalSnapshot.withdrawalReservePayout.reserveValueSha256,
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[3], {
        directions: finalSnapshot.forcedClassifications
          .map((drill) => drill.direction)
          .join(","),
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[4], {
        recoveredCases: recoveryObservations.map((drill) => drill.id).join(","),
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[5], {
        observedAtSlot: finalSnapshot.observedAt.slot,
        observedAtBlockHash: finalSnapshot.observedAt.blockHash,
        confirmationDepth:
          finalSnapshot.observedAt.confirmationDepth.toString(),
        finalSnapshotSha256: finalSnapshotDigest,
      }),
    ],
    transactions,
    rawEvidence,
    notes: [
      `State-correction acceptance independently reconciled ${claim.families.length.toString()} family journals, ${requiredHashes.size.toString()} authenticated L1 transactions, and ${recoveryObservations.length.toString()} recovery observations at ${finalSnapshot.observedAt.slot}:${finalSnapshot.observedAt.blockHash}.`,
    ],
  };
};
