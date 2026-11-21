/**
 * What the prepare-side producers share: the proof binding every witness
 * opens with, and the ScriptSources witness each of its stages pushes.
 */
import {
  commitMidgardValidationMerkleFrontier,
  encodeCbor,
  encodeMidgardBlake2b224TraceControl,
  encodeMidgardLedgerOutputProofControl,
  type MidgardBlake2b224TraceControl,
  type MidgardLedgerOutputProofControl,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";

import {
  encodeScriptDiscoveryControlCbor,
  encodeValidationFrontierPeaks as peaks,
  type ScriptDiscoveryTraceControl,
} from "./control-encoding.js";
import { emptyMidgardInputResolutionSchedule } from "./input-resolution.js";
import type { PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import type { MintFoldTraceControl } from "./trace-builder-types.js";

type Frontier = MidgardValidationMerkleFrontier;

/** The proof binding every prepare-side witness opens with. */
export const proofBinding = ({
  proofSource,
  contextCbor,
}: Pick<PreparedValidationTransaction, "proofSource" | "contextCbor">) => [
  proofSource.compactCbor,
  proofSource.witnessSetCompactCbor,
  proofSource.fieldPreimageLengthsCbor,
  contextCbor,
];

/** Whether a stage's authenticated fold commits its canonical frontier. */
export const sameFold = (folded: Frontier, canonical: Frontier): boolean =>
  commitMidgardValidationMerkleFrontier(folded).equals(
    commitMidgardValidationMerkleFrontier(canonical),
  );

export const emptyMintFoldControl = (
  assetFrontier: Frontier,
): MintFoldTraceControl => ({
  policyCount: -1,
  policyCursor: 0,
  previousPolicy: Buffer.alloc(0),
  activePolicy: Buffer.alloc(0),
  itemLength: 0,
  itemCommitment: Buffer.alloc(0),
  itemCursor: 0,
  assetsRemaining: 0,
  policyAssetCursor: 0,
  previousAsset: Buffer.alloc(0),
  assetFrontier,
});

export const emptyScriptDiscoveryControl = (
  executionFrontier: Frontier,
): ScriptDiscoveryTraceControl => ({
  purposeCursor: 0,
  sourceCursor: 0,
  redeemerCursor: 0,
  currentPurposeKind: -1,
  currentPurposeIndex: -1n,
  currentScriptHash: Buffer.alloc(0),
  currentSubject: Buffer.alloc(0),
  matchedSourceIndex: -1,
  matchedLanguageTag: -1,
  matchedSourceLeaf: Buffer.alloc(0),
  usedInlineBitmap: 0n,
  usedRedeemerBitmap: 0n,
  executionFrontier,
  redeemerItemControlHash: Buffer.alloc(0),
});

/** What every ScriptSources stage reads of the prepared transaction. */
export type ScriptSourcesPrepared = Pick<
  PreparedValidationTransaction,
  | "proofSource"
  | "contextCbor"
  | "resolutionScheduleNodes"
  | "resolutionScheduleHash"
  | "signerFrontier"
  | "signerFrontierCommitment"
  | "inlineScriptSourceFrontier"
  | "emptyValidationFrontier"
>;

/** An inline source whose hash trace is in flight at stage 0. */
export type PendingInlineSource = {
  readonly sourceIndex: number;
  readonly inlineSourceCount: number;
  readonly languageTag: 0 | 3 | 128;
  readonly payloadOffset: number;
  readonly payloadLength: number;
  readonly itemLength: number;
  readonly itemCommitment: Buffer;
  readonly hashControl: MidgardBlake2b224TraceControl;
};

/** The receive-purpose scan a stage carries: empty until stage 5. */
export type ReceiveScan = {
  readonly sourceFrontier: Frontier;
  readonly receiveCount: number;
  readonly previousHash: Buffer;
  readonly candidateHash: Buffer;
  readonly descriptorFrontier: Frontier;
};

/** The folds stage 3 leaves, which every later stage carries. */
export type ResolvedInputReplay = {
  readonly sourceFrontier: Frontier;
  readonly redeemerFrontier: Frontier;
  readonly replayCursor: number;
  readonly replayRemainingScheduleHash: Buffer;
  readonly spendIndex: number;
  readonly purposeFrontier: Frontier;
  readonly resolvedItemFrontier: Frontier;
};

export type ScriptSourcesWitness = {
  readonly stage: number;
  readonly sourceFrontier: Frontier;
  readonly redeemerFrontier: Frontier;
  /** Stage 3's resolved-item fold; empty before it. */
  readonly resolvedItemFrontier?: Frontier;
  /** Stage 6's mint fold; unopened before it. */
  readonly mintFoldControl?: MintFoldTraceControl;
  readonly replayCursor?: number;
  readonly replayRemainingScheduleHash?: Buffer;
  readonly spendIndex?: number;
  readonly purposeFrontier?: Frontier;
  readonly outputCursor?: number;
  readonly outputFrontier?: Frontier;
  readonly outputTotalCount?: number;
  readonly receiveScan?: ReceiveScan;
  readonly inlineSourceCount?: number;
  readonly redeemerTotalCount?: number;
  readonly observerScan?: {
    readonly totalCount: number;
    readonly seen: number;
    readonly previousHash: Buffer;
  };
  readonly outputProof?: MidgardLedgerOutputProofControl | null;
  readonly discovery?: ScriptDiscoveryTraceControl;
  readonly pendingSource?: PendingInlineSource | null;
  readonly redeemerItemControlHash?: Buffer;
};

/** The ScriptSources work witness at `input`'s stage and folds. */
export const scriptSourcesWitnessCbor = (
  prepared: ScriptSourcesPrepared,
  input: ScriptSourcesWitness,
): Buffer => {
  const empty = prepared.emptyValidationFrontier;
  const observerScan = input.observerScan ?? {
    totalCount: 0,
    seen: 0,
    previousHash: Buffer.alloc(0),
  };
  const receiveScan = input.receiveScan ?? {
    sourceFrontier: empty,
    receiveCount: 0,
    previousHash: Buffer.alloc(0),
    candidateHash: Buffer.alloc(0),
    descriptorFrontier: empty,
  };
  const mint = input.mintFoldControl ?? emptyMintFoldControl(empty);
  const fields: unknown[] = [
    ...proofBinding(prepared),
    BigInt(prepared.resolutionScheduleNodes.length),
    BigInt(prepared.signerFrontier.count),
    prepared.signerFrontierCommitment,
    peaks(input.resolvedItemFrontier ?? empty),
    BigInt(input.stage),
    BigInt(input.sourceFrontier.count),
    peaks(input.sourceFrontier),
    BigInt(input.redeemerFrontier.count),
    peaks(input.redeemerFrontier),
    BigInt(input.replayCursor ?? 0),
    input.replayRemainingScheduleHash ?? emptyMidgardInputResolutionSchedule(),
    BigInt(input.spendIndex ?? 0),
    BigInt(input.purposeFrontier?.count ?? 0),
    peaks(input.purposeFrontier ?? empty),
    BigInt(input.outputCursor ?? 0),
    BigInt(input.outputFrontier?.count ?? 0),
    peaks(input.outputFrontier ?? empty),
    BigInt(input.outputTotalCount ?? input.outputFrontier?.count ?? 0),
    [
      BigInt(receiveScan.sourceFrontier.count),
      peaks(receiveScan.sourceFrontier),
      BigInt(receiveScan.receiveCount),
      receiveScan.previousHash,
      receiveScan.candidateHash,
      peaks(receiveScan.descriptorFrontier),
    ],
    BigInt(
      input.inlineSourceCount ?? prepared.inlineScriptSourceFrontier.count,
    ),
    BigInt(input.redeemerTotalCount ?? input.redeemerFrontier.count),
    [
      BigInt(observerScan.totalCount),
      observerScan.previousHash,
      BigInt(observerScan.seen),
    ],
    [
      BigInt(mint.policyCount),
      BigInt(mint.policyCursor),
      mint.previousPolicy,
      mint.activePolicy,
      BigInt(mint.itemLength),
      mint.itemCommitment,
      BigInt(mint.itemCursor),
      BigInt(mint.assetsRemaining),
      BigInt(mint.policyAssetCursor),
      mint.previousAsset,
      BigInt(mint.assetFrontier.count),
      peaks(mint.assetFrontier),
    ],
    prepared.resolutionScheduleHash,
  ];
  const pending = input.pendingSource ?? null;
  if (input.stage === 0 && pending !== null) {
    fields.push(
      encodeCbor([
        1n,
        BigInt(pending.sourceIndex),
        BigInt(pending.inlineSourceCount),
        BigInt(pending.languageTag),
        BigInt(pending.payloadOffset),
        BigInt(pending.payloadLength),
        BigInt(pending.itemLength),
        pending.itemCommitment,
        encodeMidgardBlake2b224TraceControl(pending.hashControl),
      ]),
    );
  } else if (
    input.stage === 1 &&
    input.redeemerItemControlHash !== undefined &&
    input.redeemerItemControlHash.length > 0
  ) {
    fields.push(input.redeemerItemControlHash);
  } else if (
    input.stage === 5 &&
    input.outputProof !== undefined &&
    input.outputProof !== null
  ) {
    fields.push(encodeMidgardLedgerOutputProofControl(input.outputProof));
  } else if (input.stage >= 8) {
    fields.push(
      encodeScriptDiscoveryControlCbor(
        input.discovery ?? emptyScriptDiscoveryControl(empty),
      ),
    );
  }
  return encodeCbor(fields);
};
