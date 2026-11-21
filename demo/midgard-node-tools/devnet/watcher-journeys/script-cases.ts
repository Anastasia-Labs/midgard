import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardVersionedScript,
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import type * as SDK from "@al-ft/midgard-sdk/core";

import {
  buildJourneyOwnerMintTransaction,
  type JourneyOwnerTransactionInput,
  prepareJourneyForcedTransaction,
} from "./forced-transaction.js";
import type { JourneyLedgerPredecessor } from "./journey-ledger.js";
import type { OwnedOutput } from "./owner-output-selection.js";

/** `(lam ctx [(lam _ ()) ctx])`: a Plutus V3 script must return Unit. */
const unitScriptBytes = encodeCbor(Buffer.from("0101002324980041", "hex"));
/** `(lam ctx (error))`: a Plutus V3 script that fails whatever it is given. */
const failingScriptBytes = encodeCbor(Buffer.from("0101002601", "hex"));

/**
 * The owner's signed mint under a Plutus V3 policy, spending `spent`, for
 * normal L2 submission only: a forced transaction cannot carry it.
 */
export const buildJourneyScriptTransaction = (
  input: JourneyOwnerTransactionInput & {
    spent: OwnedOutput;
    variant?: "plutusMint" | "failingPlutusMint";
    /** Paid from the spent output; zero unless the caller prices it. */
    fee?: bigint;
  },
) => {
  const mintRedeemer = encodeMidgardRedeemerWitnessItem({
    purpose: "Mint",
    index: 0n,
    redeemerCbor: Buffer.from("00", "hex"),
    executionUnits: { memory: 5_000_000n, steps: 1_000_000_000n },
  });
  return buildJourneyOwnerMintTransaction({
    predecessor: input.predecessor,
    ledgerOwnerSeedPhrase: input.ledgerOwnerSeedPhrase,
    spent: input.spent,
    policyScript: {
      language: "PlutusV3",
      scriptBytes:
        input.variant === "failingPlutusMint"
          ? failingScriptBytes
          : unitScriptBytes,
    },
    fee: input.fee ?? 0n,
    outputReferenceScript: decodeMidgardVersionedScript(
      Buffer.from("820043820180", "hex"),
    ),
    redeemerWitnesses: [mintRedeemer],
    scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
      midgardFieldCommitment(encodeMidgardFieldPreimage([mintRedeemer])),
      ["PlutusV3"],
    ),
  });
};

/** These claims accuse a valid transaction at a concrete forced-order coordinate. */
export const JOURNEY_SCRIPT_FORCED_REASONS = {
  nativeScriptDecoding: {
    ResolvedReferenceScriptMalformed: { source_kind: 0n, input_index: 0n },
  },
  nativeScriptInvalid: { WitnessNativeScriptFalse: { script_index: 0n } },
  witnessScriptDecoding: { WitnessNativeScriptMalformed: { script_index: 0n } },
  scriptIntegrityHashMissing: "ScriptIntegrityHashMissing",
  outputReferenceScriptDecoding: {
    OutputReferenceScriptMalformed: { output_index: 0n },
  },
  redeemerCanonicity: { RedeemerMalformed: { redeemer_index: 0n } },
  missingRedeemer: {
    RedeemerMissing: { purpose_kind: 2n, purpose_index: 0n },
  },
  missingScriptSource: {
    ScriptSourceMissing: { purpose_kind: 1n, purpose_index: 0n },
  },
  unusedRedeemer: { UnusedRedeemer: { redeemer_index: 0n } },
  receivePurposeLanguage: {
    ReceivePurposePlutusV3Forbidden: { purpose_kind: 3n, purpose_index: 0n },
  },
  scriptIntegrityHashMismatch: "ScriptIntegrityHashMismatch",
  executionSourceScriptDecoding: {
    ExecutionNativeScriptMalformed: { purpose_kind: 1n, purpose_index: 0n },
  },
  executionNativeScriptInvalid: {
    ExecutionNativeScriptFalse: { purpose_kind: 1n, purpose_index: 0n },
  },
  distinctAssetAccumulationLimit: {
    InputAssetAccumulationLimit: { input_index: 0n, asset_index: 0n },
  },
  unusedScriptWitness: { UnusedScriptWitness: { script_index: 0n } },
} satisfies Partial<
  Record<SDK.FraudProofCatalogueCategoryName, SDK.RejectionReason>
>;

export type JourneyScriptForcedCategory =
  keyof typeof JOURNEY_SCRIPT_FORCED_REASONS;
export const JOURNEY_SCRIPT_FORCED_CATEGORIES = Object.keys(
  JOURNEY_SCRIPT_FORCED_REASONS,
) as JourneyScriptForcedCategory[];

export type JourneyScriptFixtureInput = {
  category: JourneyScriptForcedCategory;
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  /** Must be returned by the staged L1 forced-order action in live journeys. */
  orderKey: SDK.OutputReference;
};

export const buildJourneyScriptForcedFault = async (
  input: JourneyScriptFixtureInput,
) => {
  const material = prepareJourneyForcedTransaction(input);
  const replayInput = {
    canonicalTransactionCbor: material.transaction.canonicalCbor,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.ledger.entries,
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
  };
  const control = await retainedTransactionFixture(replayInput);
  if (control.replay.trace.verdict !== "accepted")
    throw new Error(
      `Script journey ${input.category} valid control failed: ${control.replay.trace.rejectionCode}`,
    );
  const fault = await retainedTransactionFixture({
    ...replayInput,
    source: {
      kind: "forced",
      orderKey: input.orderKey,
      verdict: {
        ForcedTxInvalid: {
          reason: JOURNEY_SCRIPT_FORCED_REASONS[input.category],
        },
      },
    },
  });
  return { ...fault, control, material };
};

/** Replays the same on-chain order honestly after the malicious block is removed. */
export const buildJourneyScriptForcedSuccessor = async (
  input: JourneyScriptFixtureInput,
) => {
  const material = prepareJourneyForcedTransaction(input);
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: material.transaction.canonicalCbor,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.ledger.entries,
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    source: {
      kind: "forced",
      orderKey: input.orderKey,
      verdict: "ForcedTxValid",
    },
  });
  if (block.replay.trace.verdict !== "accepted")
    throw new Error(
      `Honest forced successor failed: ${block.replay.trace.rejectionCode}`,
    );
  return block;
};
