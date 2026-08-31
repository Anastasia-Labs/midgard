/**
 * `da-hash-preimage` fault-proof family (Goal task `Q44`).
 *
 * A `transactions_root` leaf is `(key, Data(L2TransactionSourceV1))`. This
 * module is the total TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/da-hash-preimage/rule.ak`: malformed
 * source envelopes are evidence, not decoder preconditions.
 */
import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxProofFieldLengthsV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxProofFieldLengthsV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { L2TransactionSourceV1 } from "../ledger-state.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const DA_HASH_PREIMAGE_VIOLATION_ID_V1 = "da-hash-preimage" as const;

/** Exact constructor order of Aiken `VerdictV1`. */
export const DaHashPreimageVerdictV1Schema = Data.Enum([
  Data.Literal("MalformedSource"),
  Data.Literal("KeyMismatch"),
  Data.Literal("MalformedProofSource"),
  Data.Literal("DerivedIdMismatch"),
  Data.Literal("NoViolation"),
]);
export type DaHashPreimageVerdictV1 = Data.Static<
  typeof DaHashPreimageVerdictV1Schema
>;
export const DaHashPreimageVerdictV1 =
  DaHashPreimageVerdictV1Schema as unknown as DaHashPreimageVerdictV1;

export type DaHashPreimageAdjudicationV1 = {
  readonly verdict: DaHashPreimageVerdictV1;
  readonly embeddedTxId: string | null;
  readonly derivedTxId: string | null;
};

const exactBytes = (
  encoded: Uint8Array,
  decode: (bytes: Uint8Array) => unknown,
  encode: (value: never) => Uint8Array,
): unknown => {
  const decoded = decode(encoded);
  if (!Buffer.from(encode(decoded as never)).equals(Buffer.from(encoded))) {
    throw new Error("non-canonical proof-source component");
  }
  return decoded;
};

/**
 * Totally adjudicates one authenticated raw transactions-root value.
 *
 * The ordering is consensus-significant and matches Aiken: malformed outer
 * source, key mismatch, malformed/internal proof-source mismatch, body-id
 * mismatch, then the valid negative. No malformed accusation escapes as an
 * exception.
 */
export const adjudicateCommittedSourceLeafV1 = ({
  committedTxId,
  committedLeafValue,
}: {
  readonly committedTxId: string;
  readonly committedLeafValue: Uint8Array;
}): DaHashPreimageAdjudicationV1 => {
  const sourceCbor = Buffer.from(committedLeafValue).toString("hex");
  let source: L2TransactionSourceV1;
  try {
    source = Data.from(sourceCbor, L2TransactionSourceV1);
    if (Data.to(source, L2TransactionSourceV1) !== sourceCbor) {
      return {
        verdict: "MalformedSource",
        embeddedTxId: null,
        derivedTxId: null,
      };
    }
  } catch {
    return {
      verdict: "MalformedSource",
      embeddedTxId: null,
      derivedTxId: null,
    };
  }

  const embeddedTxId = source.tx_id.toLowerCase();
  if (embeddedTxId !== committedTxId.toLowerCase()) {
    return { verdict: "KeyMismatch", embeddedTxId, derivedTxId: null };
  }

  let compact: ReturnType<typeof decodeMidgardNativeTxCompactV1>;
  try {
    const compactCbor = Buffer.from(source.source.compact_cbor, "hex");
    compact = exactBytes(
      compactCbor,
      decodeMidgardNativeTxCompactV1,
      encodeMidgardNativeTxCompactV1,
    ) as ReturnType<typeof decodeMidgardNativeTxCompactV1>;

    const witnessCbor = Buffer.from(
      source.source.witness_set_compact_cbor,
      "hex",
    );
    exactBytes(
      witnessCbor,
      decodeMidgardNativeTxWitnessSetCompactV1,
      encodeMidgardNativeTxWitnessSetCompactV1,
    );
    if (!computeHash32(witnessCbor).equals(compact.transactionWitnessSetHash)) {
      throw new Error("witness-set hash mismatch");
    }

    const lengthsCbor = Buffer.from(
      source.source.field_preimage_lengths_cbor,
      "hex",
    );
    exactBytes(
      lengthsCbor,
      decodeMidgardNativeTxProofFieldLengthsV1,
      encodeMidgardNativeTxProofFieldLengthsV1,
    );
  } catch {
    return {
      verdict: "MalformedProofSource",
      embeddedTxId,
      derivedTxId: null,
    };
  }

  const derivedTxId = computeMidgardNativeTxIdV1(compact).toString("hex");
  return derivedTxId === embeddedTxId
    ? { verdict: "NoViolation", embeddedTxId, derivedTxId }
    : { verdict: "DerivedIdMismatch", embeddedTxId, derivedTxId };
};

export const isDaHashPreimageViolationV1 = (
  verdict: DaHashPreimageVerdictV1,
): boolean => verdict !== "NoViolation";

/** Evidence record derived from the authenticated raw committed leaf. */
export type DaHashPreimageEvidenceV1 = {
  readonly violationId: typeof DA_HASH_PREIMAGE_VIOLATION_ID_V1;
  readonly committedTxId: string;
  readonly committedLeafValueCbor: string;
  readonly verdict: DaHashPreimageVerdictV1;
  readonly embeddedTxId: string | null;
  readonly derivedTxId: string | null;
  readonly isViolation: boolean;
};

export const daHashPreimageEvidenceFromCommittedLeafV1 = ({
  committedTxId,
  committedLeafValue,
}: {
  readonly committedTxId: string;
  readonly committedLeafValue: Uint8Array;
}): DaHashPreimageEvidenceV1 => {
  const normalizedCommittedTxId = committedTxId.toLowerCase();
  const adjudication = adjudicateCommittedSourceLeafV1({
    committedTxId: normalizedCommittedTxId,
    committedLeafValue,
  });
  return Object.freeze({
    violationId: DA_HASH_PREIMAGE_VIOLATION_ID_V1,
    committedTxId: normalizedCommittedTxId,
    committedLeafValueCbor: Buffer.from(committedLeafValue).toString("hex"),
    ...adjudication,
    isViolation: isDaHashPreimageViolationV1(adjudication.verdict),
  });
};

// ## On-chain schemas (positional agreement with the Aiken step modules)

export const DaHashPreimageStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type DaHashPreimageStep01Datum = Data.Static<
  typeof DaHashPreimageStep01DatumSchema
>;
export const DaHashPreimageStep01Datum =
  DaHashPreimageStep01DatumSchema as unknown as DaHashPreimageStep01Datum;

export const DaHashPreimageStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionArgsSchema);
export type DaHashPreimageStep01SpendRedeemer = Data.Static<
  typeof DaHashPreimageStep01SpendRedeemerSchema
>;
export const DaHashPreimageStep01SpendRedeemer =
  DaHashPreimageStep01SpendRedeemerSchema as unknown as DaHashPreimageStep01SpendRedeemer;

/** Mirrors `midgard/fraud_proofs/da_hash_preimage/step_02.State`. */
export const DaHashPreimageStep02StateSchema = Data.Object({
  verdict: DaHashPreimageVerdictV1Schema,
});
export type DaHashPreimageStep02State = Data.Static<
  typeof DaHashPreimageStep02StateSchema
>;
export const DaHashPreimageStep02State =
  DaHashPreimageStep02StateSchema as unknown as DaHashPreimageStep02State;

export const DaHashPreimageStep02DatumSchema = faultProofStepDatumSchema(
  DaHashPreimageStep02StateSchema,
);
export type DaHashPreimageStep02Datum = Data.Static<
  typeof DaHashPreimageStep02DatumSchema
>;
export const DaHashPreimageStep02Datum =
  DaHashPreimageStep02DatumSchema as unknown as DaHashPreimageStep02Datum;

export const DaHashPreimageStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type DaHashPreimageStep02Args = Data.Static<
  typeof DaHashPreimageStep02ArgsSchema
>;
export const DaHashPreimageStep02Args =
  DaHashPreimageStep02ArgsSchema as unknown as DaHashPreimageStep02Args;

export const DaHashPreimageStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(DaHashPreimageStep02ArgsSchema);
export type DaHashPreimageStep02SpendRedeemer = Data.Static<
  typeof DaHashPreimageStep02SpendRedeemerSchema
>;
export const DaHashPreimageStep02SpendRedeemer =
  DaHashPreimageStep02SpendRedeemerSchema as unknown as DaHashPreimageStep02SpendRedeemer;

/** Step-01 args are the shared source-leaf inclusion args. */
export const DaHashPreimageTxInclusionArgsSchema = NativeTxInclusionArgsSchema;
export type DaHashPreimageTxInclusionArgs = NativeTxInclusionArgs;
export const DaHashPreimageTxInclusionArgs = NativeTxInclusionArgs;

export const DaHashPreimageStepCancelSchema = FaultProofStepCancelSchema;
export type DaHashPreimageStepCancel = FaultProofStepCancel;
export const DaHashPreimageStepCancel = FaultProofStepCancel;

export const daHashPreimageStep02StateFromEvidenceV1 = (
  evidence: DaHashPreimageEvidenceV1,
): DaHashPreimageStep02State => ({ verdict: evidence.verdict });
