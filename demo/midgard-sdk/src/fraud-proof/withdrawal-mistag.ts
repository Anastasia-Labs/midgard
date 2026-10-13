/**
 * `withdrawal-mistag` standalone proof family (D-S8/Q41).
 *
 * Production catalogue category `withdrawalMistag` (`00000014`). The schemas
 * mirror `onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/step-0*.ak`.
 */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  H32Schema,
  hashHexWithBlake2b,
  type HashingError,
  MerkleRootSchema,
  OutputReferenceSchema,
  ProofSchema,
} from "../common.js";
import {
  EventKeySchema,
  EventToStepValueSchema,
  HeaderHashSchema,
  TransitionStepSchema,
  WithdrawalBody,
  WithdrawalBodySchema,
  WithdrawalInfo,
  WithdrawalInfoSchema,
} from "../ledger-state.js";
import {
  EventToStepMembershipProofSchema,
  type RootMembershipProof,
  TransitionTraceMembershipProofSchema,
  WithdrawalSourceMembershipProofSchema,
} from "../transition-trace.js";
import { WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID } from "./catalogue.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const WITHDRAWAL_MISTAG_VIOLATION_ID = "withdrawal-mistag" as const;
export const WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT = 100;
export const withdrawalMistagThreadTokenAssetName = (
  challengedHeaderHash: string,
): string => {
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error(
      "withdrawal-mistag header hash must be 28-byte lowercase hex",
    );
  }
  return `${WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID}${challengedHeaderHash}`;
};

export const WithdrawalMistagStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type WithdrawalMistagStep01Datum = Data.Static<
  typeof WithdrawalMistagStep01DatumSchema
>;
export const WithdrawalMistagStep01Datum =
  asDataType<WithdrawalMistagStep01Datum>(WithdrawalMistagStep01DatumSchema);
export const withdrawalMistagCarriedEvidenceSchema = <
  T extends ReturnType<typeof Data.Object>,
>(
  schema: T,
) =>
  Data.Enum([
    Data.Object({ InlineEvidence: Data.Object({ value: schema }) }),
    Data.Object({ StructuredEvidence: Data.Object({ tree: Data.Any() }) }),
  ]);

export const WithdrawalMistagStep01PayloadSchema = Data.Object({
  committed_withdrawal: WithdrawalSourceMembershipProofSchema,
});
export const WithdrawalMistagStep01ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  payload: withdrawalMistagCarriedEvidenceSchema(
    WithdrawalMistagStep01PayloadSchema,
  ),
});
export type WithdrawalMistagStep01Args = Data.Static<
  typeof WithdrawalMistagStep01ArgsSchema
>;
export const WithdrawalMistagStep01Args =
  asDataType<WithdrawalMistagStep01Args>(WithdrawalMistagStep01ArgsSchema);
export const WithdrawalMistagStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep01ArgsSchema);
export type WithdrawalMistagStep01SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep01SpendRedeemerSchema
>;
export const WithdrawalMistagStep01SpendRedeemer =
  asDataType<WithdrawalMistagStep01SpendRedeemer>(
    WithdrawalMistagStep01SpendRedeemerSchema,
  );

export const WithdrawalMistagStep02StateSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  withdrawal_id: OutputReferenceSchema,
  withdrawal_info_hash: H32Schema,
  claimed_valid: Data.Boolean(),
  event_to_step_root: MerkleRootSchema,
  total_event_count: Data.Integer(),
  transition_trace_root: MerkleRootSchema,
  transition_step_count: Data.Integer(),
});
export type WithdrawalMistagStep02State = Data.Static<
  typeof WithdrawalMistagStep02StateSchema
>;
export const WithdrawalMistagStep02State =
  asDataType<WithdrawalMistagStep02State>(WithdrawalMistagStep02StateSchema);
export const WithdrawalMistagStep02DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep02StateSchema,
);
export type WithdrawalMistagStep02Datum = Data.Static<
  typeof WithdrawalMistagStep02DatumSchema
>;
export const WithdrawalMistagStep02Datum =
  asDataType<WithdrawalMistagStep02Datum>(WithdrawalMistagStep02DatumSchema);
export const WithdrawalMistagStep02PayloadSchema = Data.Object({
  withdrawal_info: WithdrawalInfoSchema,
  event_to_step: EventToStepMembershipProofSchema,
  transition_step: TransitionTraceMembershipProofSchema,
});
export const WithdrawalMistagStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  payload: withdrawalMistagCarriedEvidenceSchema(
    WithdrawalMistagStep02PayloadSchema,
  ),
});
export type WithdrawalMistagStep02Args = Data.Static<
  typeof WithdrawalMistagStep02ArgsSchema
>;
export const WithdrawalMistagStep02Args =
  asDataType<WithdrawalMistagStep02Args>(WithdrawalMistagStep02ArgsSchema);
export const WithdrawalMistagStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep02ArgsSchema);
export type WithdrawalMistagStep02SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep02SpendRedeemerSchema
>;
export const WithdrawalMistagStep02SpendRedeemer =
  asDataType<WithdrawalMistagStep02SpendRedeemer>(
    WithdrawalMistagStep02SpendRedeemerSchema,
  );

export const WithdrawalMistagStep03StateSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  withdrawal_id: OutputReferenceSchema,
  withdrawal_info_hash: H32Schema,
  claimed_valid: Data.Boolean(),
  pre_utxos_root: MerkleRootSchema,
});
export type WithdrawalMistagStep03State = Data.Static<
  typeof WithdrawalMistagStep03StateSchema
>;
export const WithdrawalMistagStep03State =
  asDataType<WithdrawalMistagStep03State>(WithdrawalMistagStep03StateSchema);
export const WithdrawalMistagStep03DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep03StateSchema,
);
export type WithdrawalMistagStep03Datum = Data.Static<
  typeof WithdrawalMistagStep03DatumSchema
>;
export const WithdrawalMistagStep03Datum =
  asDataType<WithdrawalMistagStep03Datum>(WithdrawalMistagStep03DatumSchema);
export const WithdrawalMistagLedgerEvidenceSchema = Data.Enum([
  Data.Object({
    PresentLedgerOutput: Data.Object({
      descriptor_cbor: Data.Bytes(),
      membership_proof: ProofSchema,
    }),
  }),
  Data.Object({
    AbsentLedgerOutput: Data.Object({
      non_membership_proof: ProofSchema,
    }),
  }),
]);
export type WithdrawalMistagLedgerEvidence = Data.Static<
  typeof WithdrawalMistagLedgerEvidenceSchema
>;
export const WithdrawalMistagLedgerEvidence =
  asDataType<WithdrawalMistagLedgerEvidence>(
    WithdrawalMistagLedgerEvidenceSchema,
  );
export const WithdrawalMistagStep03PayloadSchema = Data.Object({
  withdrawal_info: WithdrawalInfoSchema,
  evidence: WithdrawalMistagLedgerEvidenceSchema,
});
export const WithdrawalMistagStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  payload: withdrawalMistagCarriedEvidenceSchema(
    WithdrawalMistagStep03PayloadSchema,
  ),
});
export type WithdrawalMistagStep03Args = Data.Static<
  typeof WithdrawalMistagStep03ArgsSchema
>;
export const WithdrawalMistagStep03Args =
  asDataType<WithdrawalMistagStep03Args>(WithdrawalMistagStep03ArgsSchema);
export const WithdrawalMistagStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep03ArgsSchema);
export type WithdrawalMistagStep03SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep03SpendRedeemerSchema
>;
export const WithdrawalMistagStep03SpendRedeemer =
  asDataType<WithdrawalMistagStep03SpendRedeemer>(
    WithdrawalMistagStep03SpendRedeemerSchema,
  );

export const WithdrawalMistagStep04StateSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  withdrawal_id: OutputReferenceSchema,
  withdrawal_body_hash: H32Schema,
  claimed_valid: Data.Boolean(),
  output_present: Data.Boolean(),
  owner_signature_valid: Data.Boolean(),
  cardano_value_size: Data.Integer(),
  output_lovelace: Data.Integer(),
  output_asset_count: Data.Integer(),
  output_asset_frontier_commitment: Data.Bytes(),
});
export type WithdrawalMistagStep04State = Data.Static<
  typeof WithdrawalMistagStep04StateSchema
>;
export const WithdrawalMistagStep04State =
  asDataType<WithdrawalMistagStep04State>(WithdrawalMistagStep04StateSchema);
export const WithdrawalMistagStep04DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep04StateSchema,
);
export type WithdrawalMistagStep04Datum = Data.Static<
  typeof WithdrawalMistagStep04DatumSchema
>;
export const WithdrawalMistagStep04Datum =
  asDataType<WithdrawalMistagStep04Datum>(WithdrawalMistagStep04DatumSchema);
export const WithdrawalMistagStep04PayloadSchema = Data.Object({
  withdrawal_body: WithdrawalBodySchema,
});
export const WithdrawalMistagStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  payload: withdrawalMistagCarriedEvidenceSchema(
    WithdrawalMistagStep04PayloadSchema,
  ),
});
export type WithdrawalMistagStep04Args = Data.Static<
  typeof WithdrawalMistagStep04ArgsSchema
>;
export const WithdrawalMistagStep04Args =
  asDataType<WithdrawalMistagStep04Args>(WithdrawalMistagStep04ArgsSchema);
export const WithdrawalMistagStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep04ArgsSchema);
export type WithdrawalMistagStep04SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep04SpendRedeemerSchema
>;
export const WithdrawalMistagStep04SpendRedeemer =
  asDataType<WithdrawalMistagStep04SpendRedeemer>(
    WithdrawalMistagStep04SpendRedeemerSchema,
  );

export const WithdrawalMistagStep05StateSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  withdrawal_id: OutputReferenceSchema,
  claimed_valid: Data.Boolean(),
  actual_valid: Data.Boolean(),
  exact_output_bytes: Data.Integer(),
  required_lovelace: Data.Integer(),
});
export type WithdrawalMistagStep05State = Data.Static<
  typeof WithdrawalMistagStep05StateSchema
>;
export const WithdrawalMistagStep05State =
  asDataType<WithdrawalMistagStep05State>(WithdrawalMistagStep05StateSchema);
export const WithdrawalMistagStep05DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep05StateSchema,
);
export type WithdrawalMistagStep05Datum = Data.Static<
  typeof WithdrawalMistagStep05DatumSchema
>;
export const WithdrawalMistagStep05Datum =
  asDataType<WithdrawalMistagStep05Datum>(WithdrawalMistagStep05DatumSchema);
export const WithdrawalMistagStep05ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type WithdrawalMistagStep05Args = Data.Static<
  typeof WithdrawalMistagStep05ArgsSchema
>;
export const WithdrawalMistagStep05Args =
  asDataType<WithdrawalMistagStep05Args>(WithdrawalMistagStep05ArgsSchema);
export const WithdrawalMistagStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep05ArgsSchema);
export type WithdrawalMistagStep05SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep05SpendRedeemerSchema
>;
export const WithdrawalMistagStep05SpendRedeemer =
  asDataType<WithdrawalMistagStep05SpendRedeemer>(
    WithdrawalMistagStep05SpendRedeemerSchema,
  );

export type WithdrawalMistagDirection =
  | "valid-marked-invalid"
  | "invalid-marked-valid";

export type WithdrawalMistagPreparedEvidence = {
  readonly version: 1;
  readonly challengedHeaderHash: string;
  readonly committedWithdrawal: RootMembershipProof<
    Data.Static<typeof OutputReferenceSchema>,
    WithdrawalInfo
  >;
  readonly eventToStep: RootMembershipProof<
    Data.Static<typeof EventKeySchema>,
    Data.Static<typeof EventToStepValueSchema>
  >;
  readonly transitionStep: RootMembershipProof<
    bigint,
    Data.Static<typeof TransitionStepSchema>
  >;
  readonly ledgerEvidence: WithdrawalMistagLedgerEvidence;
  readonly withdrawalInfoHash: string;
  readonly withdrawalBodyHash: string;
  /** Exact canonical Cardano value encoding length authenticated by the descriptor. */
  readonly cardanoValueSize: bigint;
  readonly outputPresent: boolean;
  readonly coreValid: boolean;
  readonly ownerSignatureValid: boolean;
  readonly outputLovelace: bigint;
  readonly outputAssetCount: bigint;
  readonly outputAssetFrontierCommitment: string;
  readonly actualValid: boolean;
  readonly payable: boolean;
  readonly exactOutputBytes: bigint;
  readonly requiredLovelace: bigint;
  readonly direction: WithdrawalMistagDirection;
};

export const withdrawalInfoBytes = (info: WithdrawalInfo): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(info, WithdrawalInfo),
  );

export const withdrawalBodyBytes = (body: WithdrawalBody): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(body, WithdrawalBody),
  );

export const withdrawalMistagInfoCommitment = (
  info: WithdrawalInfo,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(withdrawalInfoBytes(info), 32);

export const withdrawalMistagBodyCommitment = (
  body: WithdrawalBody,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(withdrawalBodyBytes(body), 32);

export const withdrawalMistagLovelace = (body: WithdrawalBody): bigint =>
  body.l2_value.get("")?.get("") ?? 0n;

export const withdrawalClaimsValid = (info: WithdrawalInfo): boolean =>
  info.validity === "WithdrawalIsValid";

export const withdrawalMistagDirection = ({
  claimedValid,
  actualValid,
}: {
  readonly claimedValid: boolean;
  readonly actualValid: boolean;
}): WithdrawalMistagDirection => {
  if (claimedValid === actualValid) {
    throw new Error("withdrawal-mistag evidence is honestly tagged");
  }
  return claimedValid ? "invalid-marked-valid" : "valid-marked-invalid";
};
