import { createHash } from "node:crypto";

import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { type MintingPolicy, type UTxO } from "@lucid-evolution/lucid";

import { type FaultProofFieldOpeningPlan } from "../field-opening.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
} from "./journal.js";
import { journalJsonDigest, normalizeJournalJson } from "./journal.js";
import {
  type FraudProofWorkflowAction,
  type FraudProofWorkflowReconcileResult,
} from "./orchestrator.js";
import {
  rawDatumPreimagePublicationPlan,
  type RawDatumPreimageRequirement,
} from "./raw-datum-preimage.js";
import { type SignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";
import { type LocallyEvaluatedTransaction } from "./transaction-boundary.js";

export const FIELD_CARRIAGE_PREREQUISITE =
  "midgard-production-field-carriage-prerequisite-v1" as const;

export const RAW_DATUM_PREIMAGE_PREREQUISITE =
  "midgard-raw-datum-preimage-prerequisite-v1" as const;

export const FIELD_CARRIAGE_RECOVERY =
  "midgard-production-field-carriage-recovery-v1" as const;

export const TX_HASH = /^[0-9a-f]{64}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type RawCommittedFieldCarriagePlan = Readonly<{
  kind: "raw_committed_preimage_v1";
  sourceKind: 0n | 1n;
  fieldIndex: number;
  nativeTxId: string;
  preimage: Buffer;
  commitment: string;
  plan: MidgardFieldCarriagePlan;
}>;

/** Plans raw committed bytes without pretending they decoded into §5.1 items. */
export const createRawCommittedFieldCarriagePlan = ({
  sourceKind,
  owner,
  nativeTxId,
  fieldIndex,
  preimage,
}: {
  readonly owner: string;
  readonly nativeTxId: string;
  readonly sourceKind: 0n | 1n;
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
}): RawCommittedFieldCarriagePlan => {
  if (!/^[0-9a-f]{56}$/u.test(owner) || !/^[0-9a-f]{64}$/u.test(nativeTxId)) {
    throw new Error("raw committed field carriage identity is malformed");
  }
  const bytes = Buffer.from(preimage);
  const plan = planMidgardFieldCarriage({
    owner: Buffer.from(owner, "hex"),
    txId: Buffer.from(nativeTxId, "hex"),
    fieldIndex,
    preimage: bytes,
  });
  return Object.freeze({
    kind: "raw_committed_preimage_v1",
    sourceKind,
    fieldIndex: plan.fieldIndex,
    nativeTxId: plan.txId.toString("hex"),
    preimage: bytes,
    commitment: plan.commitment.toString("hex"),
    plan,
  });
};

export type FieldCarriageRequirement = Readonly<{
  planned: FaultProofFieldOpeningPlan | RawCommittedFieldCarriagePlan;
  /** Exact compact bytes the tier-3 certificate policy welds. */
  compactCbor: string;
  witnessSetCompactCbor?: string;
  certificate: Readonly<{
    policyId: string;
    mintingScript: MintingPolicy;
    referenceScriptUtxo: UTxO;
  }>;
}>;

type RawRequirement = RawDatumPreimageRequirement &
  Readonly<{ planned: ReturnType<typeof rawDatumPreimagePublicationPlan> }>;

export type PreimageCarriageRequirement =
  | FieldCarriageRequirement
  | RawDatumPreimageRequirement;

export type Requirement = (FieldCarriageRequirement | RawRequirement) &
  Readonly<{
    identitySha256: string;
    publicationDatums: readonly string[];
    publicationDigests: readonly string[];
    certificateDatumCbor: string | null;
    certificateUnit: string | null;
  }>;

export type Recovery = Readonly<{
  schemaVersion: typeof FIELD_CARRIAGE_RECOVERY;
  kind: "publication" | "certificate";
  requirementSha256: string;
  outRef: string;
  datumCbor: string;
  unit: string | null;
}>;

export interface FieldCarriagePrerequisitePort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof FIELD_CARRIAGE_PREREQUISITE;
  readonly category: Category;
  resolveAuthenticated(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<
    Readonly<{
      publications: readonly UTxO[];
      certificate?: UTxO;
      requirement: FieldCarriageRequirement | RawRequirement | null;
    }>
  >;
  inspect(input: {
    readonly headerHash: string;
    readonly baseAction: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
  }): Promise<
    | { readonly kind: "not_required" | "satisfied" }
    | { readonly kind: "pending"; readonly reason: string }
    | { readonly kind: "required"; readonly action: FraudProofWorkflowAction }
  >;
  capture(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<{
    readonly transaction: LocallyEvaluatedTransaction;
    readonly durableRecovery: JournalJsonObject;
  }>;
  reconcile(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly txHash?: string;
    readonly durableRecovery?: JournalJsonObject;
    readonly signedTransactionCborHex?: string;
    readonly authorizeResubmission?: (
      input: SignedWorkflowTransaction,
    ) => Promise<void>;
  }): Promise<FraudProofWorkflowReconcileResult>;
}

export const sha256 = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

export const sameJson = (left: unknown, right: unknown): boolean =>
  left === undefined || right === undefined
    ? left === right
    : journalJsonDigest(normalizeJournalJson(left)) ===
      journalJsonDigest(normalizeJournalJson(right));

export const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

export const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
