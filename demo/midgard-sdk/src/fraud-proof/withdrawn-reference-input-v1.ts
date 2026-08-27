/**
 * `withdrawn-reference-input` family (Q19) — off-chain schema twins.
 *
 * Proves a committed transaction references an input that the same block's
 * counted withdrawals root commits a **valid** L2 withdrawal for (spec
 * §5.1.16, reference side). Three steps: native-tx binding, §8.8 field-1
 * opening, then a counted-root withdrawal membership check against the
 * header's own `withdrawals_root`.
 *
 * Violation: `withdrawn-reference-input`.
 * Catalogue category: **not registered yet**. `00000010` is reserved only for
 * emulator wiring; production registration will allocate the real id. Until
 * then this module is reached by direct import rather than through
 * `fraud-proof/catalogue.ts`, and the asset-name helper is parameterized on
 * the category id instead of pinning one.
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/withdrawn-reference-input/
 * step-0{1,2,3}.ak` field for field and constructor index for constructor
 * index.
 */
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";
import { WithdrawalSourceMembershipProofSchema } from "@/transition-trace.js";

import { FieldOpeningV1Schema } from "./field-opening-v1.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MidgardTxInputSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

/** Normative violation identifier. */
export const WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1 =
  "withdrawn-reference-input" as const;

// ## Thread NFT asset name

/**
 * A withdrawn-reference-input computation-thread token's asset name: the
 * family's category id (4 bytes; test-only or allocated at registration)
 * followed by the challenged header hash.
 */
export const withdrawnReferenceInputThreadTokenAssetNameV1 = (
  categoryId: string,
  challengedHeaderHash: string,
): string => {
  if (!/^[0-9a-f]{8}$/u.test(categoryId)) {
    throw new Error(
      "withdrawn-reference-input category id must be 4 bytes of lowercase hex",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error("challenged header hash must be 28 bytes of lowercase hex");
  }
  return `${categoryId}${challengedHeaderHash}`;
};

export const WithdrawnReferenceInputStepCancelSchema =
  FaultProofStepCancelSchema;
export type WithdrawnReferenceInputStepCancel = FaultProofStepCancel;
export const WithdrawnReferenceInputStepCancel =
  FaultProofStepCancel as unknown as WithdrawnReferenceInputStepCancel;

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so
// it is read with the generic computation-thread step datum. The Continue arm
// carries the bare `NativeTxInclusionArgs` (this family predates the #545
// carriage sum); the produced UTxO carries step-02's state plus the header's
// counted withdrawals commitment.

export const WithdrawnReferenceInputTxInclusionArgsSchema =
  NativeTxInclusionArgsSchema;
export type WithdrawnReferenceInputTxInclusionArgs = NativeTxInclusionArgs;
export const WithdrawnReferenceInputTxInclusionArgs =
  NativeTxInclusionArgs as unknown as WithdrawnReferenceInputTxInclusionArgs;

export const WithdrawnReferenceInputStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawnReferenceInputTxInclusionArgsSchema);
export type WithdrawnReferenceInputStep01SpendRedeemer = Data.Static<
  typeof WithdrawnReferenceInputStep01SpendRedeemerSchema
>;
export const WithdrawnReferenceInputStep01SpendRedeemer =
  WithdrawnReferenceInputStep01SpendRedeemerSchema as unknown as WithdrawnReferenceInputStep01SpendRedeemer;

export const WithdrawnReferenceInputStep01DatumSchema =
  faultProofStepDatumSchema(Data.Any());
export type WithdrawnReferenceInputStep01Datum = Data.Static<
  typeof WithdrawnReferenceInputStep01DatumSchema
>;
export const WithdrawnReferenceInputStep01Datum =
  WithdrawnReferenceInputStep01DatumSchema as unknown as WithdrawnReferenceInputStep01Datum;

// ## Step 02 — open the reference-inputs preimage and select the bad input

/**
 * Mirrors `midgard/fraud_proofs/withdrawn_reference_input/step_02.State`:
 * the §2.5 anchor plus the header's counted withdrawals commitment (root and
 * count travel together so step-03 can unwrap the counted root).
 */
export const WithdrawnReferenceInputStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  blocks_withdrawals_root: H32Schema,
  blocks_withdrawal_count: Data.Integer(),
});
export type WithdrawnReferenceInputStep02State = Data.Static<
  typeof WithdrawnReferenceInputStep02StateSchema
>;
export const WithdrawnReferenceInputStep02State =
  WithdrawnReferenceInputStep02StateSchema as unknown as WithdrawnReferenceInputStep02State;

export const WithdrawnReferenceInputStep02DatumSchema =
  faultProofStepDatumSchema(WithdrawnReferenceInputStep02StateSchema);
export type WithdrawnReferenceInputStep02Datum = Data.Static<
  typeof WithdrawnReferenceInputStep02DatumSchema
>;
export const WithdrawnReferenceInputStep02Datum =
  WithdrawnReferenceInputStep02DatumSchema as unknown as WithdrawnReferenceInputStep02Datum;

/**
 * Mirrors `midgard/fraud_proofs/withdrawn_reference_input/step_02.Args`.
 * Field 1, not field 0 — the index is a literal at the on-chain call site,
 * and the same fixed-stride reader serves both input fields.
 */
export const WithdrawnReferenceInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  reference_inputs_opening: FieldOpeningV1Schema,
  bad_reference_input_index: Data.Integer(),
});
export type WithdrawnReferenceInputStep02Args = Data.Static<
  typeof WithdrawnReferenceInputStep02ArgsSchema
>;
export const WithdrawnReferenceInputStep02Args =
  WithdrawnReferenceInputStep02ArgsSchema as unknown as WithdrawnReferenceInputStep02Args;

export const WithdrawnReferenceInputStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawnReferenceInputStep02ArgsSchema);
export type WithdrawnReferenceInputStep02SpendRedeemer = Data.Static<
  typeof WithdrawnReferenceInputStep02SpendRedeemerSchema
>;
export const WithdrawnReferenceInputStep02SpendRedeemer =
  WithdrawnReferenceInputStep02SpendRedeemerSchema as unknown as WithdrawnReferenceInputStep02SpendRedeemer;

// ## Step 03 — prove a valid committed withdrawal spent the referenced input
//
// The redeemer carries a full `RootMembershipProof<WithdrawalId,
// WithdrawalInfo>` (the SDK's `WithdrawalSourceMembershipProofSchema`). The
// validator hashes `cbor.serialise(key)`/`cbor.serialise(value)`, so any MPF
// leaf offchain MUST be built with `committedWithdrawalKeyBytesV1` /
// `committedWithdrawalValueBytesV1` (aiken-canonical bytes) — node-canonical
// CBOR of the same Plutus data builds roots the validator can never verify.

export const WithdrawnReferenceInputStep03StateSchema = Data.Object({
  missing_reference_input: MidgardTxInputSchema,
  blocks_withdrawals_root: H32Schema,
  blocks_withdrawal_count: Data.Integer(),
});
export type WithdrawnReferenceInputStep03State = Data.Static<
  typeof WithdrawnReferenceInputStep03StateSchema
>;
export const WithdrawnReferenceInputStep03State =
  WithdrawnReferenceInputStep03StateSchema as unknown as WithdrawnReferenceInputStep03State;

export const WithdrawnReferenceInputStep03DatumSchema =
  faultProofStepDatumSchema(WithdrawnReferenceInputStep03StateSchema);
export type WithdrawnReferenceInputStep03Datum = Data.Static<
  typeof WithdrawnReferenceInputStep03DatumSchema
>;
export const WithdrawnReferenceInputStep03Datum =
  WithdrawnReferenceInputStep03DatumSchema as unknown as WithdrawnReferenceInputStep03Datum;

export const WithdrawnReferenceInputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  withdrawal_membership: WithdrawalSourceMembershipProofSchema,
});
export type WithdrawnReferenceInputStep03Args = Data.Static<
  typeof WithdrawnReferenceInputStep03ArgsSchema
>;
export const WithdrawnReferenceInputStep03Args =
  WithdrawnReferenceInputStep03ArgsSchema as unknown as WithdrawnReferenceInputStep03Args;

export const WithdrawnReferenceInputStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawnReferenceInputStep03ArgsSchema);
export type WithdrawnReferenceInputStep03SpendRedeemer = Data.Static<
  typeof WithdrawnReferenceInputStep03SpendRedeemerSchema
>;
export const WithdrawnReferenceInputStep03SpendRedeemer =
  WithdrawnReferenceInputStep03SpendRedeemerSchema as unknown as WithdrawnReferenceInputStep03SpendRedeemer;

// ## Step resolver

export const WITHDRAWN_REFERENCE_INPUT_STEP_NAMES_V1 = [
  "step_01",
  "step_02",
  "step_03",
] as const;
export type WithdrawnReferenceInputStepNameV1 =
  (typeof WITHDRAWN_REFERENCE_INPUT_STEP_NAMES_V1)[number];

/** Exhaustive datum-schema resolver for the three deployed steps. */
export const withdrawnReferenceInputStepDatumSchemaV1 = (
  step: WithdrawnReferenceInputStepNameV1,
) => {
  switch (step) {
    case "step_01":
      return WithdrawnReferenceInputStep01DatumSchema;
    case "step_02":
      return WithdrawnReferenceInputStep02DatumSchema;
    case "step_03":
      return WithdrawnReferenceInputStep03DatumSchema;
  }
};
