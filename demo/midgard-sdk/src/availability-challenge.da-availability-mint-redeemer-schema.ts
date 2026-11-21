import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, fromHex, toHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  CANONICAL_CBOR_HEX,
  CHALLENGE_ASSET_NAME,
  DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX,
  DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES,
  DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS,
  DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY,
  DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES,
  DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS,
  DA_AVAILABILITY_TERMINAL_ACCUMULATOR_ASSET_NAME_PREFIX,
  DA_AVAILABILITY_TRANCHE_ASSET_NAME_PREFIX,
  HASH_28,
  HASH_32,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";
import { type OutputReference, OutputReferenceSchema } from "./common.js";
import { FrontierPeakSchema } from "./fraud-proof/validation-auxiliary-witness.js";
import { HeaderHashSchema } from "./ledger-state.js";

export const requireHash = (
  value: string,
  width: 28 | 32,
  field: string,
): void => {
  const pattern = width === 28 ? HASH_28 : HASH_32;
  if (!pattern.test(value)) {
    throw new DaAvailabilityCommitmentError(
      `${field} must be exactly ${width.toString()} lowercase hex bytes`,
    );
  }
};

export const requireSafePositiveInteger = (
  value: number,
  field: string,
): void => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new DaAvailabilityCommitmentError(
      `${field} must be a positive safe integer`,
    );
  }
};

export const hashDomainAndData = (
  domain: Uint8Array,
  valueCborHex: string,
): string =>
  toHex(
    blake2b(
      Buffer.concat([Buffer.from(domain), Buffer.from(fromHex(valueCborHex))]),
      { dkLen: 32 },
    ),
  );

export const parseCanonicalDataCbor = <Schema, Value>(input: {
  readonly cborHex: string;
  readonly schema: Schema;
  readonly name: string;
}): Value => {
  if (!CANONICAL_CBOR_HEX.test(input.cborHex)) {
    throw new DaAvailabilityCommitmentError(
      `${input.name} must be non-empty lowercase CBOR hex`,
    );
  }
  let decoded: Value;
  try {
    decoded = Data.from(input.cborHex, input.schema as never) as Value;
  } catch (error) {
    throw new DaAvailabilityCommitmentError(
      `${input.name} is not valid V1 Plutus Data: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  if (Data.to(decoded as never, input.schema as never) !== input.cborHex) {
    throw new DaAvailabilityCommitmentError(
      `${input.name} must use the canonical V1 Plutus Data encoding`,
    );
  }
  return decoded;
};

const outRefIdentity28 = (outRef: OutputReference): string =>
  toHex(
    blake2b(fromHex(Data.to(outRef as never, OutputReferenceSchema as never)), {
      dkLen: 28,
    }),
  );

/**
 * The challenge identity: "DACH" ++ blake2b_224(serialise(outref)) of the
 * challenger's funding input consumed by `OpenChallenge` (the twin of Aiken
 * `challenge_asset_name_v1`). A spent outref can never be consumed again, so
 * the identity is unique per challenge.
 */
export const daAvailabilityChallengeAssetName = (
  challengerFundingOutRef: OutputReference,
): string =>
  `${DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX}${outRefIdentity28(challengerFundingOutRef)}`;

export const daAvailabilityTrancheAssetName = (input: {
  readonly challengeAssetName: string;
  readonly trancheIndex: number;
}): string => {
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentError(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  if (
    !Number.isSafeInteger(input.trancheIndex) ||
    input.trancheIndex < 0 ||
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY
  ) {
    throw new DaAvailabilityCommitmentError(
      "trancheIndex is outside the structural transaction safety bound",
    );
  }
  const challengeSuffix = input.challengeAssetName.slice(
    DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX.length,
  );
  const encodedIndex = Buffer.alloc(2);
  // Aiken's integer_to_bytearray(False, 2, index) is little endian.
  encodedIndex.writeUInt16LE(input.trancheIndex);
  return `${DA_AVAILABILITY_TRANCHE_ASSET_NAME_PREFIX}${challengeSuffix}${encodedIndex.toString("hex")}`;
};

export const daAvailabilityTerminalAccumulatorAssetName = (
  challengeAssetName: string,
): string => {
  if (!CHALLENGE_ASSET_NAME.test(challengeAssetName)) {
    throw new DaAvailabilityCommitmentError(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  return `${DA_AVAILABILITY_TERMINAL_ACCUMULATOR_ASSET_NAME_PREFIX}${challengeAssetName.slice(
    DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX.length,
  )}`;
};

export const DaAvailabilityTerminalAccumulatorDatumSchema = Data.Object({
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
  next_tranche_index: Data.Integer(),
  folded_terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  has_timed_out_tranche: Data.Boolean(),
  response_deadline: Data.Integer(),
  challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
  remaining_challenger_lovelace: Data.Integer(),
});

export type DaAvailabilityTerminalAccumulatorDatum = Data.Static<
  typeof DaAvailabilityTerminalAccumulatorDatumSchema
>;

export const DaAvailabilityTerminalAccumulatorDatum =
  asDataType<DaAvailabilityTerminalAccumulatorDatum>(
    DaAvailabilityTerminalAccumulatorDatumSchema,
  );

export const DaAvailabilityPublicationDatumSchema = Data.Object({
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
  tranche_index: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  chunk_frontier: Data.Array(FrontierPeakSchema),
  chunk_siblings: Data.Array(Data.Bytes({ minLength: 32, maxLength: 32 })),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  next_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  chunk: Data.Bytes(),
});

export type DaAvailabilityPublicationDatum = Data.Static<
  typeof DaAvailabilityPublicationDatumSchema
>;

export const DaAvailabilityPublicationDatum =
  asDataType<DaAvailabilityPublicationDatum>(
    DaAvailabilityPublicationDatumSchema,
  );

/**
 * Exact minting-policy ABI for the challenge lifecycle (Aiken
 * `MintRedeemerV1`, constructor order 0 Open, 1 Settle, 2 Close, 3 Timeout).
 * `TimeoutChallenge` names the pooled committee bond input and its continuing
 * output; `challenger_refund_output_index` is the one challenger output that
 * carries both the refund and any slash payout.
 */
export const DaAvailabilityMintRedeemerSchema = Data.Enum([
  Data.Object({
    OpenChallenge: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      record_output_index: Data.Integer(),
      challenger_input_index: Data.Integer(),
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      first_tranche_output_index: Data.Integer(),
      terminal_accumulator_output_index: Data.Integer(),
      challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
    }),
  }),
  Data.Object({
    SettleTranche: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      record_ref_input_index: Data.Integer(),
      terminal_accumulator_input_index: Data.Integer(),
      terminal_accumulator_output_index: Data.Integer(),
      tranche_input_index: Data.Integer(),
      carrier_input_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    CloseChallenge: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      record_input_index: Data.Integer(),
      terminal_accumulator_input_index: Data.Integer(),
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      challenger_refund_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    TimeoutChallenge: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      record_input_index: Data.Integer(),
      terminal_accumulator_input_index: Data.Integer(),
      state_queue_mint_redeemer_index: Data.Integer(),
      pool_input_index: Data.Integer(),
      pool_output_index: Data.Integer(),
      challenger_refund_output_index: Data.Integer(),
    }),
  }),
]);

export type DaAvailabilityMintRedeemer = Data.Static<
  typeof DaAvailabilityMintRedeemerSchema
>;

export const DaAvailabilityMintRedeemer =
  asDataType<DaAvailabilityMintRedeemer>(DaAvailabilityMintRedeemerSchema);

/**
 * Exact spending-validator ABI (Aiken `SpendRedeemerV1`) for the challenge
 * record, tranche and carrier UTxOs.
 */
export const DaAvailabilitySpendRedeemerSchema = Data.Enum([
  Data.Object({
    AdvanceTranche: Data.Object({
      thread_output_index: Data.Integer(),
      carrier_output_index: Data.Integer(),
      m_previous_carrier_input_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    ConsumeCarrier: Data.Object({
      thread_input_index: Data.Integer(),
      thread_spend_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Coordinate: Data.Object({ mint_redeemer_index: Data.Integer() }),
  }),
]);

export type DaAvailabilitySpendRedeemer = Data.Static<
  typeof DaAvailabilitySpendRedeemerSchema
>;

export const DaAvailabilitySpendRedeemer =
  asDataType<DaAvailabilitySpendRedeemer>(DaAvailabilitySpendRedeemerSchema);

export class DaAvailabilityCommitmentError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "DaAvailabilityCommitmentV1Error";
  }
}

export const daAvailabilityResponseWindowMs = (
  payloadByteLength: number,
): number => {
  requireSafePositiveInteger(payloadByteLength, "payloadByteLength");
  if (payloadByteLength > DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES) {
    throw new DaAvailabilityCommitmentError(
      "payloadByteLength exceeds the canonical 64 MiB V1 DA limit",
    );
  }
  return payloadByteLength <= DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES
    ? DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS
    : DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS;
};

export const daAvailabilityResponseDeadline = (input: {
  readonly payloadByteLength: number;
  readonly openedAt: bigint;
}): bigint => {
  if (input.openedAt < 0n) {
    throw new DaAvailabilityCommitmentError(
      "availability challenge openedAt must be non-negative",
    );
  }
  return (
    input.openedAt +
    BigInt(daAvailabilityResponseWindowMs(input.payloadByteLength))
  );
};

export type DaAvailabilityTrancheLayout = Readonly<{
  trancheIndex: number;
  startOffset: number;
  byteLength: number;
}>;
