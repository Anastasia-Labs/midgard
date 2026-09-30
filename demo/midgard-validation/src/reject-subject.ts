import type { MidgardLedgerTx } from "./ledger-tx/types.js";
import { midgardOutRefToCborHex } from "./validation-candidate.js";

/** One item of a transaction field, by field index and item ordinal. */
export type RejectFieldItem = {
  readonly fieldIndex: bigint;
  readonly itemIndex: bigint;
};

/** `InputNotFound.source_kind` for a spend input (field 0). */
export const REJECT_SOURCE_KIND_SPEND = 0n;

/** `InputNotFound.source_kind` for a reference input (field 1). */
export const REJECT_SOURCE_KIND_REFERENCE = 1n;

/**
 * The subject a rejection names: the `RejectionReasonV1` arm the failed rule
 * belongs to, plus the ordinals that arm carries.
 *
 * A forced verdict must cite the exact arm and subject the fault proofs
 * reopen. The rejection code cannot supply either: one code covers several
 * arms, and a family that reopens a named witness, signer, input or output
 * convicts the operator when that subject turns out to be sound. So the rule
 * that fails records what it found.
 *
 * Ordinals follow the arm's coordinate convention: positions in the
 * transaction's own field order (spend inputs, reference inputs, outputs,
 * address witnesses, script witnesses, redeemers, required signers, required
 * observers), and for purposes the redeemer-pointer namespace (purpose kind
 * 0 spend, 1 mint, 2 observe, 3 receive).
 */
export type RejectSubject =
  | {
      readonly arm: "DuplicateInput";
      readonly first: RejectFieldItem;
      readonly second: RejectFieldItem;
    }
  | { readonly arm: "AddressWitnessSignatureInvalid"; readonly index: bigint }
  | { readonly arm: "RequiredSignerUnsigned"; readonly index: bigint }
  | {
      readonly arm: "WitnessNativeScriptFalse" | "WitnessNativeScriptMalformed";
      readonly index: bigint;
    }
  | { readonly arm: "ObserverOrderInvalid"; readonly index: bigint }
  | { readonly arm: "ScriptIntegrityHashMissing" }
  | { readonly arm: "ObserversForbiddenOnUntaggedNetwork" }
  | {
      readonly arm: "InputNotFound" | "InputSpentOutputNonCanonical";
      readonly sourceKind: bigint;
      readonly index: bigint;
    }
  | { readonly arm: "SpendInputSignerMissing"; readonly index: bigint }
  | { readonly arm: "ProtectedOutputSignerMissing"; readonly index: bigint }
  | {
      readonly arm: "ScriptSourceMissing" | "RedeemerMissing";
      readonly purposeKind: bigint;
      readonly purposeIndex: bigint;
    }
  | { readonly arm: "UnusedRedeemer"; readonly index: bigint }
  | { readonly arm: "UnusedScriptWitness"; readonly index: bigint }
  | { readonly arm: "ScriptIntegrityHashMismatch" }
  | {
      readonly arm:
        | "ExecutionNativeScriptFalse"
        | "ReceivePurposePlutusV3Forbidden"
        | "PlutusExecutionFailed";
      readonly index: bigint;
    }
  | {
      readonly arm: "OutputNonCanonical" | "OutputBelowMinAda";
      readonly index: bigint;
    };

/** The arm tags a {@link RejectSubject} can carry. */
export type RejectSubjectArm = RejectSubject["arm"];

/**
 * Position of an out-ref (as its canonical CBOR hex) in the transaction's
 * spend or reference input field. Validation reaches inputs through their
 * out-ref keys, while the verdict cites the field ordinal.
 */
export const inputOrdinalOf = (
  ledgerTx: MidgardLedgerTx,
  sourceKind: bigint,
  outRefHex: string,
): bigint => {
  const inputs =
    sourceKind === REJECT_SOURCE_KIND_SPEND
      ? ledgerTx.spendInputs
      : ledgerTx.referenceInputs;
  const index = inputs.findIndex(
    (outRef) => midgardOutRefToCborHex(outRef) === outRefHex,
  );
  if (index < 0) {
    throw new Error(
      `rejected input ${outRefHex} is not in the transaction's ${sourceKind === REJECT_SOURCE_KIND_SPEND ? "spend" : "reference"} inputs`,
    );
  }
  return BigInt(index);
};
