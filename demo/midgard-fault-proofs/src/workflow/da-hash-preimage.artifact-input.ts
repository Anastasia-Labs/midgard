import { DA_HASH_PREIMAGE_VIOLATION_ID } from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  DA_HASH_PREIMAGE_EVIDENCE_SCHEMA_VERSION,
  prepareDaHashPreimageFromCommittedLeaves,
  type PreparedDaHashPreimageOutput,
} from "../prepare-da-hash-preimage.js";
import {
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitDaHashPreimageStep01 } from "../submit-da-hash-preimage-step-01.js";
import { submitDaHashPreimageStep02 } from "../submit-da-hash-preimage-step-02.js";
import { submitInit } from "../submit-init.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import { type LinearFamilyReferenceScripts } from "./family-definition.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";

export const DA_HASH_PREIMAGE_ARTIFACT =
  "midgard-production-da-hash-preimage-artifact-v1" as const;

type DaHashPreimageArtifactEntry = readonly [string, string];

export type DaHashPreimageArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof DA_HASH_PREIMAGE_ARTIFACT;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: number;
  readonly committedTxId: string;
  readonly entries: readonly DaHashPreimageArtifactEntry[];
};

const HEX_32 = /^[0-9a-f]{64}$/u;

const HEX_28 = /^[0-9a-f]{56}$/u;

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  expected: readonly string[],
  label: string,
): void => {
  const actual = Object.keys(value).sort();
  const canonical = [...expected].sort();
  if (
    actual.length !== canonical.length ||
    actual.some((key, index) => key !== canonical[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
};

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const artifactFields = [
  "schemaVersion",
  "headerHash",
  "committedTransactionsRoot",
  "l2TransactionCount",
  "committedTxId",
  "entries",
] as const;

const artifactInput = (
  value: unknown,
): {
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly committedTxId: string;
  readonly entries: readonly DaHashPreimageArtifactEntry[];
} => {
  const candidate = record(value, "da-hash-preimage workflow artifact");
  exactKeys(candidate, artifactFields, "da-hash-preimage workflow artifact");
  if (candidate.schemaVersion !== DA_HASH_PREIMAGE_ARTIFACT) {
    throw new Error("da-hash-preimage workflow artifact version changed");
  }
  if (
    !Number.isSafeInteger(candidate.l2TransactionCount) ||
    (candidate.l2TransactionCount as number) < 0
  ) {
    throw new Error(
      "da-hash-preimage workflow artifact count is not a non-negative safe integer",
    );
  }
  if (!Array.isArray(candidate.entries) || candidate.entries.length === 0) {
    throw new Error(
      "da-hash-preimage workflow artifact has no committed leaves",
    );
  }
  const entries = candidate.entries.map((entry, index) => {
    if (!Array.isArray(entry) || entry.length !== 2) {
      throw new Error(
        `da-hash-preimage workflow leaf ${index.toString()} is malformed`,
      );
    }
    return Object.freeze([
      canonicalHex(entry[0], HEX_32, `committed leaf ${index.toString()} key`),
      canonicalHex(
        entry[1],
        /^(?:[0-9a-f]{2})+$/u,
        `committed leaf ${index.toString()} value`,
      ),
    ] as const);
  });
  return {
    headerHash: canonicalHex(
      candidate.headerHash,
      HEX_28,
      "da-hash-preimage artifact header",
    ),
    committedTransactionsRoot: canonicalHex(
      candidate.committedTransactionsRoot,
      HEX_32,
      "da-hash-preimage committed transactions root",
    ),
    l2TransactionCount: BigInt(candidate.l2TransactionCount as number),
    committedTxId: canonicalHex(
      candidate.committedTxId,
      HEX_32,
      "da-hash-preimage violating committed key",
    ),
    entries: Object.freeze(entries),
  };
};

/**
 * Reopens the counted transactions root and re-runs Q44 from the journaled raw
 * leaves. No verdict, proof, or decoded transaction claim is trusted from the
 * durable artifact.
 */
export const admitDaHashPreimageArtifact = async (
  value: unknown,
): Promise<PreparedDaHashPreimageOutput> => {
  const admitted = artifactInput(value);
  return await prepareDaHashPreimageFromCommittedLeaves({
    headerHash: admitted.headerHash,
    committedTransactionsRoot: admitted.committedTransactionsRoot,
    l2TransactionCount: admitted.l2TransactionCount,
    entries: admitted.entries,
    committedTxId: admitted.committedTxId,
  });
};

const sameJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

/** Creates the minimal raw-leaf artifact from the independently routed plan. */
export const daHashPreimageArtifact = async (
  plan: PreparedDaHashPreimageOutput,
): Promise<DaHashPreimageArtifact> => {
  if (
    plan.schemaVersion !== DA_HASH_PREIMAGE_EVIDENCE_SCHEMA_VERSION ||
    plan.violationId !== DA_HASH_PREIMAGE_VIOLATION_ID ||
    plan.files !== undefined
  ) {
    throw new Error(
      "da-hash-preimage production plan is not an in-memory authenticated raw-leaf plan",
    );
  }
  const artifact = normalizeJournalJson({
    schemaVersion: DA_HASH_PREIMAGE_ARTIFACT,
    headerHash: plan.headerHash,
    committedTransactionsRoot: plan.committedTransactionsRoot,
    l2TransactionCount: plan.l2TransactionCount,
    committedTxId: plan.violation.committedTxId,
    entries: plan.leaves.map(
      (leaf) => [leaf.committedTxId, leaf.committedLeafValueCbor] as const,
    ),
  }) as DaHashPreimageArtifact;
  const rederived = await admitDaHashPreimageArtifact(artifact);
  if (
    rederived.violationId !== plan.violationId ||
    rederived.headerHash !== plan.headerHash ||
    rederived.committedTransactionsRoot !== plan.committedTransactionsRoot ||
    rederived.l2TransactionCount !== plan.l2TransactionCount ||
    !sameJson(rederived.txInclusion, plan.txInclusion) ||
    !sameJson(rederived.step02State, plan.step02State)
  ) {
    throw new Error(
      "da-hash-preimage production plan differs from raw-leaf re-derivation",
    );
  }
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

export type DaHashPreimageWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "daHashPreimage",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type BoundDaHashPreimageTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"daHashPreimage">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: DaHashPreimageWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

export type DaHashPreimageBuilderSet = Readonly<{
  init: typeof submitInit;
  step01: typeof submitDaHashPreimageStep01;
  step02: typeof submitDaHashPreimageStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

export const productionBuilders: DaHashPreimageBuilderSet = Object.freeze({
  init: submitInit,
  step01: submitDaHashPreimageStep01,
  step02: submitDaHashPreimageStep02,
  remove: submitRemoveFraudulentBlock,
});

export const requiredAction = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "da-hash-preimage workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "daHashPreimage" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("da-hash-preimage workflow action changed identity");
  }
  return input;
};

export const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`da-hash-preimage workflow action omitted ${name}`);
  }
  return value;
};
