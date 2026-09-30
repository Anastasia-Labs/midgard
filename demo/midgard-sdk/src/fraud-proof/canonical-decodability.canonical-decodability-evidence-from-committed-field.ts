import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import { FieldCarriageSchema } from "../native-tx-field-access.js";
import {
  CANONICAL_DECODABILITY_VIOLATION_ID,
  type CanonicalDecodabilityEvidence,
  isCanonicalDecodabilityViolation,
  MIDGARD_ENVELOPE_VERDICT_NAMES,
  midgardEnvelopeVerdict,
} from "./canonical-decodability.walk-midgard-envelope-items.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  NativeTxInclusionCarriageSchema,
  NativeTxWitnessSetCompactSchema,
} from "./native.js";

/**
 * Builds the evidence record for one committed field.
 *
 * The caller must have authenticated `committedPreimage` against the field's
 * §4 commitment, positionally extracted from the compact structures the block's
 * `transactions_root` committed — on chain that check is the door's, and it is
 * what makes these bytes evidence rather than a claim. This function performs no
 * I/O and never throws.
 */
export const canonicalDecodabilityEvidenceFromCommittedField = ({
  badTxId,
  fieldIndex,
  committedPreimage,
}: {
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
}): CanonicalDecodabilityEvidence => {
  const verdict = midgardEnvelopeVerdict(committedPreimage);
  return Object.freeze({
    violationId: CANONICAL_DECODABILITY_VIOLATION_ID,
    badTxId: badTxId.toLowerCase(),
    fieldIndex,
    committedPreimage: Buffer.from(committedPreimage).toString("hex"),
    committedPreimageByteCount: committedPreimage.length,
    verdict,
    verdictName: MIDGARD_ENVELOPE_VERDICT_NAMES[verdict] ?? "unknown",
    isViolation: isCanonicalDecodabilityViolation({ fieldIndex, verdict }),
  });
};

// ## On-chain schemas (positional agreement with the Aiken step modules)

/**
 * §2.5 fields 0–5. No witness set is carried, because the door consults none
 * for a body field — a claim cannot carry one the door would ignore.
 */
export const BodyFieldClaimSchema = Data.Object({
  field_index: Data.Integer(),
  carriage: FieldCarriageSchema,
});

export type BodyFieldClaimV1 = Data.Static<typeof BodyFieldClaimSchema>;

export const BodyFieldClaimV1 =
  asDataType<BodyFieldClaimV1>(BodyFieldClaimSchema);

/**
 * §2.5 fields 6–8. `witness_set` is unauthenticated on arrival and is checked
 * against the block-committed `witness_set_hash` inside the door.
 */
export const WitnessFieldClaimSchema = Data.Object({
  field_index: Data.Integer(),
  witness_set: NativeTxWitnessSetCompactSchema,
  carriage: FieldCarriageSchema,
});

export type WitnessFieldClaimV1 = Data.Static<typeof WitnessFieldClaimSchema>;

export const WitnessFieldClaimV1 = asDataType<WitnessFieldClaimV1>(
  WitnessFieldClaimSchema,
);

/**
 * `midgard/fraud_proofs/canonical_decodability/rule.CommittedFieldClaim`.
 *
 * **Constructor order is wire format.** `Data.Enum` pins the Constr index
 * positionally: `BodyFieldClaim` 0, `WitnessFieldClaim` 1, exactly as the Aiken
 * sum declares them.
 */
export const CommittedFieldClaimSchema = Data.Enum([
  Data.Object({ BodyFieldClaim: BodyFieldClaimSchema }),
  Data.Object({ WitnessFieldClaim: WitnessFieldClaimSchema }),
]);

export type CommittedFieldClaim = Data.Static<typeof CommittedFieldClaimSchema>;

export const CommittedFieldClaim = asDataType<CommittedFieldClaim>(
  CommittedFieldClaimSchema,
);

export const CanonicalDecodabilityStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);

export type CanonicalDecodabilityStep01Datum = Data.Static<
  typeof CanonicalDecodabilityStep01DatumSchema
>;

export const CanonicalDecodabilityStep01Datum =
  asDataType<CanonicalDecodabilityStep01Datum>(
    CanonicalDecodabilityStep01DatumSchema,
  );

/** Mirrors `midgard/fraud_proofs/canonical_decodability/step_01.Args`. */
export const CanonicalDecodabilityStep01ArgsSchema = Data.Object({
  inclusion: NativeTxInclusionCarriageSchema,
  claim: CommittedFieldClaimSchema,
});

export type CanonicalDecodabilityStep01Args = Data.Static<
  typeof CanonicalDecodabilityStep01ArgsSchema
>;

export const CanonicalDecodabilityStep01Args =
  asDataType<CanonicalDecodabilityStep01Args>(
    CanonicalDecodabilityStep01ArgsSchema,
  );

export const CanonicalDecodabilityStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CanonicalDecodabilityStep01ArgsSchema);

export type CanonicalDecodabilityStep01SpendRedeemer = Data.Static<
  typeof CanonicalDecodabilityStep01SpendRedeemerSchema
>;

export const CanonicalDecodabilityStep01SpendRedeemer =
  asDataType<CanonicalDecodabilityStep01SpendRedeemer>(
    CanonicalDecodabilityStep01SpendRedeemerSchema,
  );

/**
 * Mirrors `midgard/fraud_proofs/canonical_decodability/step_02.State`.
 *
 * `(bad_tx_id, field_index)` is §12.1's fault address; `verdict` is the proof.
 * No preimage bytes travel — the bytes were authenticated in the transaction
 * that read them.
 */
export const CanonicalDecodabilityStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  field_index: Data.Integer(),
  verdict: Data.Integer(),
});

export type CanonicalDecodabilityStep02State = Data.Static<
  typeof CanonicalDecodabilityStep02StateSchema
>;

export const CanonicalDecodabilityStep02State =
  asDataType<CanonicalDecodabilityStep02State>(
    CanonicalDecodabilityStep02StateSchema,
  );

export const CanonicalDecodabilityStep02DatumSchema = faultProofStepDatumSchema(
  CanonicalDecodabilityStep02StateSchema,
);

export type CanonicalDecodabilityStep02Datum = Data.Static<
  typeof CanonicalDecodabilityStep02DatumSchema
>;

export const CanonicalDecodabilityStep02Datum =
  asDataType<CanonicalDecodabilityStep02Datum>(
    CanonicalDecodabilityStep02DatumSchema,
  );

export const CanonicalDecodabilityStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});

export type CanonicalDecodabilityStep02Args = Data.Static<
  typeof CanonicalDecodabilityStep02ArgsSchema
>;

export const CanonicalDecodabilityStep02Args =
  asDataType<CanonicalDecodabilityStep02Args>(
    CanonicalDecodabilityStep02ArgsSchema,
  );

export const CanonicalDecodabilityStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CanonicalDecodabilityStep02ArgsSchema);

export type CanonicalDecodabilityStep02SpendRedeemer = Data.Static<
  typeof CanonicalDecodabilityStep02SpendRedeemerSchema
>;

export const CanonicalDecodabilityStep02SpendRedeemer =
  asDataType<CanonicalDecodabilityStep02SpendRedeemer>(
    CanonicalDecodabilityStep02SpendRedeemerSchema,
  );

export const CanonicalDecodabilityTxInclusionArgsSchema =
  NativeTxInclusionArgsSchema;

export type CanonicalDecodabilityTxInclusionArgs = NativeTxInclusionArgs;

export const CanonicalDecodabilityTxInclusionArgs = NativeTxInclusionArgs;

export const CanonicalDecodabilityStepCancelSchema = FaultProofStepCancelSchema;

export type CanonicalDecodabilityStepCancel = FaultProofStepCancel;

export const CanonicalDecodabilityStepCancel = FaultProofStepCancel;

/**
 * Builds the step-02 state exactly as the on-chain step-01 validator derives it,
 * so an off-chain builder and the L1 verifier cannot drift.
 */
export const canonicalDecodabilityStep02StateFromEvidence = (
  evidence: CanonicalDecodabilityEvidence,
): CanonicalDecodabilityStep02State => ({
  bad_tx_id: evidence.badTxId,
  field_index: BigInt(evidence.fieldIndex),
  verdict: BigInt(evidence.verdict),
});
