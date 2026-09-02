/**
 * `withdrawal-mistag` standalone proof family (D-S8/Q41).
 *
 * Production catalogue category `withdrawalMistag` (`00000014`). The schemas
 * mirror `onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/step-0*.ak`.
 */
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type AddressData,
  H32Schema,
  hashHexWithBlake2b,
  type HashingError,
  MerkleRootSchema,
  OutputReferenceSchema,
  ProofSchema,
} from "@/common.js";
import {
  type CardanoDatum,
  EventKeySchema,
  EventToStepValueSchema,
  HeaderHashSchema,
  TransitionStepSchema,
  WithdrawalBody,
  WithdrawalBodySchema,
  WithdrawalInfo,
  WithdrawalInfoSchema,
} from "@/ledger-state.js";
import {
  EventToStepMembershipProofSchema,
  type RootMembershipProof,
  TransitionTraceMembershipProofSchema,
  WithdrawalSourceMembershipProofSchema,
} from "@/transition-trace.js";

import { WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID_V1 } from "./catalogue.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const WITHDRAWAL_MISTAG_VIOLATION_ID_V1 = "withdrawal-mistag" as const;
export const WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT_V1 = 100;
export const WITHDRAWAL_MISTAG_MAXIMUM_VALUE_CBOR_BYTES_V1 = 5_000;
export const WITHDRAWAL_MISTAG_COINS_PER_UTXO_BYTE_V1 = 4_310n;

export const withdrawalMistagThreadTokenAssetNameV1 = (
  challengedHeaderHash: string,
): string => {
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error(
      "withdrawal-mistag header hash must be 28-byte lowercase hex",
    );
  }
  return `${WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID_V1}${challengedHeaderHash}`;
};

export const WithdrawalMistagStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type WithdrawalMistagStep01Datum = Data.Static<
  typeof WithdrawalMistagStep01DatumSchema
>;
export const WithdrawalMistagStep01Datum =
  WithdrawalMistagStep01DatumSchema as unknown as WithdrawalMistagStep01Datum;
export const WithdrawalMistagStep01ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  committed_withdrawal: WithdrawalSourceMembershipProofSchema,
});
export type WithdrawalMistagStep01Args = Data.Static<
  typeof WithdrawalMistagStep01ArgsSchema
>;
export const WithdrawalMistagStep01Args =
  WithdrawalMistagStep01ArgsSchema as unknown as WithdrawalMistagStep01Args;
export const WithdrawalMistagStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep01ArgsSchema);
export type WithdrawalMistagStep01SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep01SpendRedeemerSchema
>;
export const WithdrawalMistagStep01SpendRedeemer =
  WithdrawalMistagStep01SpendRedeemerSchema as unknown as WithdrawalMistagStep01SpendRedeemer;

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
  WithdrawalMistagStep02StateSchema as unknown as WithdrawalMistagStep02State;
export const WithdrawalMistagStep02DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep02StateSchema,
);
export type WithdrawalMistagStep02Datum = Data.Static<
  typeof WithdrawalMistagStep02DatumSchema
>;
export const WithdrawalMistagStep02Datum =
  WithdrawalMistagStep02DatumSchema as unknown as WithdrawalMistagStep02Datum;
export const WithdrawalMistagStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  withdrawal_info: WithdrawalInfoSchema,
  event_to_step: EventToStepMembershipProofSchema,
  transition_step: TransitionTraceMembershipProofSchema,
});
export type WithdrawalMistagStep02Args = Data.Static<
  typeof WithdrawalMistagStep02ArgsSchema
>;
export const WithdrawalMistagStep02Args =
  WithdrawalMistagStep02ArgsSchema as unknown as WithdrawalMistagStep02Args;
export const WithdrawalMistagStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep02ArgsSchema);
export type WithdrawalMistagStep02SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep02SpendRedeemerSchema
>;
export const WithdrawalMistagStep02SpendRedeemer =
  WithdrawalMistagStep02SpendRedeemerSchema as unknown as WithdrawalMistagStep02SpendRedeemer;

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
  WithdrawalMistagStep03StateSchema as unknown as WithdrawalMistagStep03State;
export const WithdrawalMistagStep03DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep03StateSchema,
);
export type WithdrawalMistagStep03Datum = Data.Static<
  typeof WithdrawalMistagStep03DatumSchema
>;
export const WithdrawalMistagStep03Datum =
  WithdrawalMistagStep03DatumSchema as unknown as WithdrawalMistagStep03Datum;
export const WithdrawalMistagLedgerEvidenceV1Schema = Data.Enum([
  Data.Object({
    PresentLedgerOutput: Data.Object({
      output_cbor: Data.Bytes(),
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
export type WithdrawalMistagLedgerEvidenceV1 = Data.Static<
  typeof WithdrawalMistagLedgerEvidenceV1Schema
>;
export const WithdrawalMistagLedgerEvidenceV1 =
  WithdrawalMistagLedgerEvidenceV1Schema as unknown as WithdrawalMistagLedgerEvidenceV1;
export const WithdrawalMistagStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  withdrawal_info: WithdrawalInfoSchema,
  evidence: WithdrawalMistagLedgerEvidenceV1Schema,
});
export type WithdrawalMistagStep03Args = Data.Static<
  typeof WithdrawalMistagStep03ArgsSchema
>;
export const WithdrawalMistagStep03Args =
  WithdrawalMistagStep03ArgsSchema as unknown as WithdrawalMistagStep03Args;
export const WithdrawalMistagStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep03ArgsSchema);
export type WithdrawalMistagStep03SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep03SpendRedeemerSchema
>;
export const WithdrawalMistagStep03SpendRedeemer =
  WithdrawalMistagStep03SpendRedeemerSchema as unknown as WithdrawalMistagStep03SpendRedeemer;

export const WithdrawalMistagStep04StateSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  withdrawal_id: OutputReferenceSchema,
  withdrawal_body_hash: H32Schema,
  claimed_valid: Data.Boolean(),
  output_present: Data.Boolean(),
  core_valid: Data.Boolean(),
  cardano_value_size: Data.Integer(),
});
export type WithdrawalMistagStep04State = Data.Static<
  typeof WithdrawalMistagStep04StateSchema
>;
export const WithdrawalMistagStep04State =
  WithdrawalMistagStep04StateSchema as unknown as WithdrawalMistagStep04State;
export const WithdrawalMistagStep04DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep04StateSchema,
);
export type WithdrawalMistagStep04Datum = Data.Static<
  typeof WithdrawalMistagStep04DatumSchema
>;
export const WithdrawalMistagStep04Datum =
  WithdrawalMistagStep04DatumSchema as unknown as WithdrawalMistagStep04Datum;
export const WithdrawalMistagStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  withdrawal_body: WithdrawalBodySchema,
});
export type WithdrawalMistagStep04Args = Data.Static<
  typeof WithdrawalMistagStep04ArgsSchema
>;
export const WithdrawalMistagStep04Args =
  WithdrawalMistagStep04ArgsSchema as unknown as WithdrawalMistagStep04Args;
export const WithdrawalMistagStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep04ArgsSchema);
export type WithdrawalMistagStep04SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep04SpendRedeemerSchema
>;
export const WithdrawalMistagStep04SpendRedeemer =
  WithdrawalMistagStep04SpendRedeemerSchema as unknown as WithdrawalMistagStep04SpendRedeemer;

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
  WithdrawalMistagStep05StateSchema as unknown as WithdrawalMistagStep05State;
export const WithdrawalMistagStep05DatumSchema = faultProofStepDatumSchema(
  WithdrawalMistagStep05StateSchema,
);
export type WithdrawalMistagStep05Datum = Data.Static<
  typeof WithdrawalMistagStep05DatumSchema
>;
export const WithdrawalMistagStep05Datum =
  WithdrawalMistagStep05DatumSchema as unknown as WithdrawalMistagStep05Datum;
export const WithdrawalMistagStep05ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type WithdrawalMistagStep05Args = Data.Static<
  typeof WithdrawalMistagStep05ArgsSchema
>;
export const WithdrawalMistagStep05Args =
  WithdrawalMistagStep05ArgsSchema as unknown as WithdrawalMistagStep05Args;
export const WithdrawalMistagStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawalMistagStep05ArgsSchema);
export type WithdrawalMistagStep05SpendRedeemer = Data.Static<
  typeof WithdrawalMistagStep05SpendRedeemerSchema
>;
export const WithdrawalMistagStep05SpendRedeemer =
  WithdrawalMistagStep05SpendRedeemerSchema as unknown as WithdrawalMistagStep05SpendRedeemer;

export type WithdrawalMistagDirectionV1 =
  | "valid-marked-invalid"
  | "invalid-marked-valid";

export type WithdrawalMistagPreparedEvidenceV1 = {
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
  readonly ledgerEvidence: WithdrawalMistagLedgerEvidenceV1;
  readonly withdrawalInfoHash: string;
  readonly withdrawalBodyHash: string;
  /** Exact canonical Cardano value encoding length authenticated by the descriptor. */
  readonly cardanoValueSize: bigint;
  readonly outputPresent: boolean;
  readonly coreValid: boolean;
  readonly actualValid: boolean;
  readonly payable: boolean;
  readonly exactOutputBytes: bigint;
  readonly requiredLovelace: bigint;
  readonly direction: WithdrawalMistagDirectionV1;
};

export const withdrawalInfoBytesV1 = (info: WithdrawalInfo): string =>
  aikenSerialisedPlutusDataCbor(Data.to(info, WithdrawalInfo));

export const withdrawalBodyBytesV1 = (body: WithdrawalBody): string =>
  aikenSerialisedPlutusDataCbor(Data.to(body, WithdrawalBody));

export const withdrawalMistagInfoCommitmentV1 = (
  info: WithdrawalInfo,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(withdrawalInfoBytesV1(info), 32);

export const withdrawalMistagBodyCommitmentV1 = (
  body: WithdrawalBody,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(withdrawalBodyBytesV1(body), 32);

const cborHeadLength = (length: bigint): bigint => {
  if (length < 24n) return 1n;
  if (length <= 255n) return 2n;
  if (length <= 65_535n) return 3n;
  if (length <= 4_294_967_295n) return 5n;
  return 9n;
};

const pointerWordLength = (value: bigint): bigint => {
  if (value < 0n)
    throw new Error("withdrawal pointer component must be non-negative");
  return value < 128n ? 1n : 1n + pointerWordLength(value / 128n);
};

export const withdrawalMistagAddressPayloadBytesV1 = (
  address: AddressData,
): bigint => {
  const stake = address.stakeCredential;
  if (stake === null) return 29n;
  if ("Inline" in stake) return 57n;
  const pointer = stake.Pointer[0];
  return (
    29n +
    pointerWordLength(pointer.slotNumber) +
    pointerWordLength(pointer.transactionIndex) +
    pointerWordLength(pointer.certificateIndex)
  );
};

const datumOptionBytes = (datum: CardanoDatum): bigint => {
  if (datum === "NoDatum") return 0n;
  if ("DatumHash" in datum) {
    if (!/^[0-9a-fA-F]{64}$/u.test(datum.DatumHash.hash)) {
      throw new Error("withdrawal datum hash must be 32 bytes");
    }
    return 37n;
  }
  // Aiken's `cbor.serialise(Data)` uses canonical definite maps. Lucid may
  // emit an equivalent indefinite map, whose byte length is different.
  const encoded = aikenSerialisedPlutusDataCbor(
    Data.to(datum.InlineDatum.data),
  );
  const dataLength = BigInt(encoded.length / 2);
  return 5n + cborHeadLength(dataLength) + dataLength;
};

/** Exact twin of `step_04.exact_payout_output_bytes_v1`. */
export const withdrawalMistagExactPayoutOutputBytesV1 = ({
  body,
  cardanoValueSize,
}: {
  readonly body: WithdrawalBody;
  readonly cardanoValueSize: bigint;
}): bigint => {
  if (cardanoValueSize < 0n)
    throw new Error("Cardano value size must be non-negative");
  const addressLength = withdrawalMistagAddressPayloadBytesV1(body.l1_address);
  return (
    3n +
    cborHeadLength(addressLength) +
    addressLength +
    cardanoValueSize +
    datumOptionBytes(body.l1_datum)
  );
};

export const withdrawalMistagMinimumLovelaceV1 = ({
  body,
  cardanoValueSize,
  coinsPerUtxoByte = WITHDRAWAL_MISTAG_COINS_PER_UTXO_BYTE_V1,
}: {
  readonly body: WithdrawalBody;
  readonly cardanoValueSize: bigint;
  readonly coinsPerUtxoByte?: bigint;
}): bigint =>
  coinsPerUtxoByte *
  (160n + withdrawalMistagExactPayoutOutputBytesV1({ body, cardanoValueSize }));

export const withdrawalMistagLovelaceV1 = (body: WithdrawalBody): bigint =>
  body.l2_value.get("")?.get("") ?? 0n;

export const withdrawalMistagPayableV1 = ({
  body,
  cardanoValueSize,
  coinsPerUtxoByte = WITHDRAWAL_MISTAG_COINS_PER_UTXO_BYTE_V1,
}: {
  readonly body: WithdrawalBody;
  readonly cardanoValueSize: bigint;
  readonly coinsPerUtxoByte?: bigint;
}): boolean =>
  cardanoValueSize > 0n &&
  cardanoValueSize <= BigInt(WITHDRAWAL_MISTAG_MAXIMUM_VALUE_CBOR_BYTES_V1) &&
  withdrawalMistagLovelaceV1(body) >=
    withdrawalMistagMinimumLovelaceV1({
      body,
      cardanoValueSize,
      coinsPerUtxoByte,
    });

export const withdrawalClaimsValidV1 = (info: WithdrawalInfo): boolean =>
  info.validity === "WithdrawalIsValid";

export const withdrawalMistagDirectionV1 = ({
  claimedValid,
  actualValid,
}: {
  readonly claimedValid: boolean;
  readonly actualValid: boolean;
}): WithdrawalMistagDirectionV1 => {
  if (claimedValid === actualValid) {
    throw new Error("withdrawal-mistag evidence is honestly tagged");
  }
  return claimedValid ? "invalid-marked-valid" : "valid-marked-invalid";
};
