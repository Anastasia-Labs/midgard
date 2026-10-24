import { createHash } from "node:crypto";

import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { Proof } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import {
  type PublishedProofChunk,
  resolvePublishedProofChunks,
  splitProofIntoChunkDatums,
} from "../publish-proof-chunks.js";
import {
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator.js";
import { type SignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";
import { type LocallyEvaluatedTransaction } from "./transaction-boundary.js";

export const PROOF_CHUNK_PREREQUISITE =
  "midgard-production-proof-chunk-prerequisite-v1" as const;

export const PROOF_CHUNK_PUBLICATION_RECOVERY =
  "midgard-production-proof-chunk-publication-recovery-v1" as const;

export const PROOF_CARRIAGE_RECOVERY =
  "midgard-production-proof-carriage-recovery-v1" as const;

export const TX_HASH = /^[0-9a-f]{64}$/u;

export const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;

export type ProofChunkRequirement = Readonly<{
  proofCbor: string;
  proofCborSha256: string;
  chunkDatums: readonly string[];
  chunkDatumSha256s: readonly string[];
}>;

export type ProofChunkPublicationRecovery = Readonly<{
  schemaVersion: typeof PROOF_CHUNK_PUBLICATION_RECOVERY;
  proofCborSha256: string;
  outputs: readonly Readonly<{ outRef: string; datumCbor: string }>[];
}>;

export type DirectCapacityFailure = Readonly<{
  kind: "max_tx_size";
  maximumTransactionBytes: number;
  actualTransactionBytes: number;
  errorSha256: string;
}>;

export type ProofCarriageRecovery = Readonly<{
  schemaVersion: typeof PROOF_CARRIAGE_RECOVERY;
  route: "direct" | "publication";
  baseAction: FraudProofWorkflowAction;
  proofCborSha256: string;
  directCapacityFailure?: DirectCapacityFailure;
  baseDurableRecovery?: JournalJsonObject;
  publicationDurableRecovery?: JournalJsonObject;
}>;

export interface ProofChunkPrerequisitePort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof PROOF_CHUNK_PREREQUISITE;
  readonly category: Category;
  classifyDirectCapacityFailure(cause: unknown): DirectCapacityFailure;
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

type DirectFirstProofCarriageRoute = "direct" | "publication";

const directFirstProofCarriageRouteByAction = new WeakMap<
  object,
  DirectFirstProofCarriageRoute
>();

export const withDirectFirstProofCarriageRoute = async <Result>({
  action,
  route,
  run,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly route: DirectFirstProofCarriageRoute;
  readonly run: () => Promise<Result>;
}): Promise<Result> => {
  if (directFirstProofCarriageRouteByAction.has(action)) {
    throw new Error("proof-carriage action already has an active route");
  }
  directFirstProofCarriageRouteByAction.set(action, route);
  try {
    return await run();
  } finally {
    directFirstProofCarriageRouteByAction.delete(action);
  }
};

/**
 * Resolves an already-authorized published route without making publication a
 * prerequisite for the direct fit attempt. `undefined` is deliberately the
 * direct route: the outer adapter will only revisit this after an exact
 * release-bound capacity refusal and a journal-confirmed publication.
 */
export const resolveDirectFirstProofChunks = async ({
  action,
  lucid,
  address,
  proofCbor,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly proofCbor: string;
}): Promise<readonly PublishedProofChunk[]> => {
  const route = directFirstProofCarriageRouteByAction.get(action);
  if (route === undefined) {
    throw new Error(
      "proof chunks were requested outside an admitted direct-first route",
    );
  }
  if (route === "direct") return [];
  const chunks = await resolvePublishedProofChunks({
    lucid,
    address,
    proofCbor,
  });
  if (chunks === undefined) {
    throw new Error(
      "journal-authorized proof publication has no exact complete output set",
    );
  }
  return chunks;
};

// The orchestrator hands back the journal-normalized (key-sorted) action, so
// equality must be structural, not byte order.
export const sameJson = (left: unknown, right: unknown): boolean =>
  journalJsonDigest(normalizeJournalJson(left)) ===
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

// Artifacts carry the proof as the MPF library encodes it (an indefinite-length
// CBOR list), so the chunk identity is bound to the canonical re-encoding and
// any encoding of the same proof steps names the same publication.
export const requirementFor = ({
  proofCbor: encodedProofCbor,
  label,
}: {
  readonly proofCbor: string;
  readonly label: string;
}): ProofChunkRequirement => {
  if (typeof encodedProofCbor !== "string" || encodedProofCbor.length === 0) {
    throw new Error(`${label} omitted its MPF proof`);
  }
  let proofCbor: string;
  try {
    proofCbor = canonicalPlutusDataCbor(
      Data.to(Data.from(encodedProofCbor, Proof), Proof),
    );
  } catch {
    throw new Error(`${label} MPF proof is not a PlutusData MPF proof`);
  }
  const chunkDatums = Object.freeze([...splitProofIntoChunkDatums(proofCbor)]);
  return Object.freeze({
    proofCbor,
    proofCborSha256: sha256(proofCbor),
    chunkDatums,
    chunkDatumSha256s: Object.freeze(chunkDatums.map(sha256)),
  });
};

export const publicationAction = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  baseAction,
  requirement,
}: {
  readonly category: Category;
  readonly baseAction: FraudProofWorkflowAction;
  readonly requirement: ProofChunkRequirement;
}): FraudProofWorkflowAction => {
  const frozenBaseAction = Object.freeze({
    actionId: baseAction.actionId,
    input: Object.freeze({ ...baseAction.input }),
  });
  return Object.freeze({
    actionId: `publish-proof-chunks:${baseAction.actionId}:${requirement.proofCborSha256}`,
    input: Object.freeze({
      schemaVersion: PROOF_CHUNK_PREREQUISITE,
      category,
      stage: "direct_or_publish_proof",
      forAction: frozenBaseAction,
      proofCborSha256: requirement.proofCborSha256,
      chunkDatumSha256s: requirement.chunkDatumSha256s,
    }),
  });
};

export const isPublicationAction = (
  action: FraudProofWorkflowAction,
): boolean =>
  action.input.schemaVersion === PROOF_CHUNK_PREREQUISITE &&
  action.input.stage === "direct_or_publish_proof";

export const routeActionIdentity = (
  action: FraudProofWorkflowAction,
): Readonly<{
  baseAction: FraudProofWorkflowAction;
  requirement: ProofChunkRequirement;
}> => {
  const input = exact(
    action.input,
    [
      "schemaVersion",
      "category",
      "stage",
      "forAction",
      "proofCborSha256",
      "chunkDatumSha256s",
    ],
    "proof-carriage route action",
  );
  if (
    input.schemaVersion !== PROOF_CHUNK_PREREQUISITE ||
    input.stage !== "direct_or_publish_proof" ||
    typeof input.proofCborSha256 !== "string" ||
    !TX_HASH.test(input.proofCborSha256) ||
    !Array.isArray(input.chunkDatumSha256s) ||
    input.chunkDatumSha256s.some(
      (digest) => typeof digest !== "string" || !TX_HASH.test(digest),
    )
  ) {
    throw new Error("proof-carriage route action changed identity");
  }
  const rawAction = exact(
    input.forAction,
    ["actionId", "input"],
    "proof-carriage route base action",
  );
  if (typeof rawAction.actionId !== "string") {
    throw new Error("proof-carriage route base action is invalid");
  }
  return Object.freeze({
    baseAction: Object.freeze({
      actionId: rawAction.actionId,
      input: record(
        rawAction.input,
        "proof-carriage route base action input",
      ) as JournalJsonObject,
    }),
    requirement: Object.freeze({
      proofCbor: "",
      proofCborSha256: input.proofCborSha256,
      chunkDatums: Object.freeze([]),
      chunkDatumSha256s: Object.freeze(
        input.chunkDatumSha256s as readonly string[],
      ),
    }),
  });
};
