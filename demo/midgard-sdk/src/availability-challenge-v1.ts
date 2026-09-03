import {
  buildMidgardValidationMerkleMembershipIndex,
  commitMidgardValidationMerkleFrontier,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES,
  MIDGARD_MAX_DA_PAYLOAD_BYTES,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import { Data, fromHex, toHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { type OutputReference, OutputReferenceSchema } from "./common.js";
import { FrontierPeakSchema } from "./fraud-proof/validation-auxiliary-witness-v1.js";
import { HeaderHashSchema } from "./ledger-state.js";
export {
  DaAvailabilityStateQueueStatus,
  daAvailabilityStateQueueStatusPermitsMerge,
  DaAvailabilityStateQueueStatusSchema,
} from "./da-availability-state-v1.js";

export const DA_AVAILABILITY_COMMITMENT_VERSION = 1n;

export const DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES = 64 * 1024;
export const DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES =
  MIDGARD_MAX_DA_PAYLOAD_BYTES;

export const DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS = 60 * 60 * 1_000;
export const DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS = 48 * 60 * 60 * 1_000;

export const DA_AVAILABILITY_BOND_LOVELACE_MEASUREMENT_CANDIDATE =
  10_000_000_000n;
export const DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE =
  10_000_000_000n;

/**
 * The first response-publication measurement candidate. It is deliberately
 * named as a candidate: Q58 may promote it to the compiled response chunk
 * bound only after a signed, reference-script-backed testnet-profile
 * transaction retains the protocol's 512-byte reliability reserve.
 */
export const DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE =
  Object.freeze({
    // Exact signed reference-script transaction frontier: 15,872 bytes,
    // retaining the required 512-byte maxTxSize reserve. 14,021 serializes to
    // 15,873 and is therefore rejected by the adjacent measurement.
    chunkByteLength: 14_020,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  });

/** Absolute safety ceilings, not an activated response geometry. */
export const DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES =
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES;
export const DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY =
  MIDGARD_CONSENSUS_LIMITS.maxOutputCount;

const HASH_28 = /^[0-9a-f]{56}$/u;
const HASH_32 = /^[0-9a-f]{64}$/u;
const CANONICAL_CBOR_HEX = /^(?:[0-9a-f]{2})+$/u;

const requireHash = (value: string, width: 28 | 32, field: string): void => {
  const pattern = width === 28 ? HASH_28 : HASH_32;
  if (!pattern.test(value)) {
    throw new DaAvailabilityCommitmentError(
      `${field} must be exactly ${width.toString()} lowercase hex bytes`,
    );
  }
};

const requireSafePositiveInteger = (value: number, field: string): void => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new DaAvailabilityCommitmentError(
      `${field} must be a positive safe integer`,
    );
  }
};

const hashDomainAndData = (domain: Uint8Array, valueCborHex: string): string =>
  toHex(
    blake2b(
      Buffer.concat([Buffer.from(domain), Buffer.from(fromHex(valueCborHex))]),
      { dkLen: 32 },
    ),
  );

const parseCanonicalDataCbor = <Schema, Value>(input: {
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

const ATTESTATION_COMMITMENT_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityAttestationV1",
  "ascii",
);
const TRANCHE_START_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTrancheStartV1",
  "ascii",
);
const TRANCHE_STEP_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTrancheStepV1",
  "ascii",
);
const PUBLISHED_TERMINAL_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityPublishedV1",
  "ascii",
);
const CHUNK_LEAF_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityChunkLeafV1",
  "ascii",
);
const TERMINAL_ACCUMULATOR_START_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTerminalStartV1",
  "ascii",
);
const TERMINAL_ACCUMULATOR_STEP_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTerminalStepV1",
  "ascii",
);

export const DA_AVAILABILITY_BOND_ASSET_NAME_PREFIX = Buffer.from(
  "DABN",
  "ascii",
).toString("hex");
export const DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX = Buffer.from(
  "DACH",
  "ascii",
).toString("hex");
export const DA_AVAILABILITY_TRANCHE_ASSET_NAME_PREFIX = Buffer.from(
  "DT",
  "ascii",
).toString("hex");
export const DA_AVAILABILITY_TERMINAL_ACCUMULATOR_ASSET_NAME_PREFIX =
  Buffer.from("DACT", "ascii").toString("hex");
const CHALLENGE_ASSET_NAME = new RegExp(
  `^${DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX}[0-9a-f]{56}$`,
  "u",
);
const BOND_ASSET_NAME = new RegExp(
  `^${DA_AVAILABILITY_BOND_ASSET_NAME_PREFIX}[0-9a-f]{56}$`,
  "u",
);

const outRefIdentity28 = (outRef: OutputReference): string =>
  toHex(
    blake2b(fromHex(Data.to(outRef as never, OutputReferenceSchema as never)), {
      dkLen: 28,
    }),
  );

export const daAvailabilityBondAssetName = (
  attestationInputOutRef: OutputReference,
): string =>
  `${DA_AVAILABILITY_BOND_ASSET_NAME_PREFIX}${outRefIdentity28(attestationInputOutRef)}`;

export const daAvailabilityChallengeAssetName = (
  bondInputOutRef: OutputReference,
): string =>
  `${DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX}${outRefIdentity28(bondInputOutRef)}`;

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
  return `${DA_AVAILABILITY_TRANCHE_ASSET_NAME_PREFIX}${challengeSuffix}${input.trancheIndex
    .toString(16)
    .padStart(4, "0")}`;
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

export const DaAvailabilityTrancheDescriptorSchema = Data.Object({
  tranche_index: Data.Integer(),
  start_offset: Data.Integer(),
  byte_length: Data.Integer(),
  chunk_count: Data.Integer(),
  chunk_commitment: Data.Bytes({ minLength: 32, maxLength: 32 }),
  terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export type DaAvailabilityTrancheDescriptor = Data.Static<
  typeof DaAvailabilityTrancheDescriptorSchema
>;
export const DaAvailabilityTrancheDescriptor =
  DaAvailabilityTrancheDescriptorSchema as unknown as DaAvailabilityTrancheDescriptor;

/**
 * Release-bound response geometry. The applied response-transaction
 * measurement selects these values in authenticated deployment/DA parameters;
 * the wire schema does not turn the first 4 MiB/4 KiB sizing probe into
 * protocol law.
 */
export const DaAvailabilityResponseGeometrySchema = Data.Object({
  chunk_byte_length: Data.Integer(),
  tranche_byte_length: Data.Integer(),
  max_tranche_count: Data.Integer(),
});
export type DaAvailabilityResponseGeometry = Data.Static<
  typeof DaAvailabilityResponseGeometrySchema
>;
export const DaAvailabilityResponseGeometry =
  DaAvailabilityResponseGeometrySchema as unknown as DaAvailabilityResponseGeometry;

/**
 * Authenticated release/DA parameters selected after applied response-cost
 * measurement. The two bonds remain matching, but their activated lovelace
 * amount is deployment data rather than a wire-level constant.
 */
export const DaAvailabilityParametersSchema = Data.Object({
  response_geometry: DaAvailabilityResponseGeometrySchema,
  da_bond_lovelace: Data.Integer(),
  challenger_bond_lovelace: Data.Integer(),
  max_open_fee_lovelace: Data.Integer(),
  max_publication_fee_lovelace: Data.Integer(),
  max_settlement_fee_lovelace: Data.Integer(),
  max_close_fee_lovelace: Data.Integer(),
  max_timeout_fee_lovelace: Data.Integer(),
});
export type DaAvailabilityParameters = Data.Static<
  typeof DaAvailabilityParametersSchema
>;
export const DaAvailabilityParameters =
  DaAvailabilityParametersSchema as unknown as DaAvailabilityParameters;

export const DaAvailabilityCommitmentSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  payload_byte_length: Data.Integer(),
  response_geometry: DaAvailabilityResponseGeometrySchema,
  tranche_descriptors: Data.Array(DaAvailabilityTrancheDescriptorSchema),
  bond_owner: Data.Bytes({ minLength: 28, maxLength: 28 }),
});
export type DaAvailabilityCommitment = Data.Static<
  typeof DaAvailabilityCommitmentSchema
>;
export const DaAvailabilityCommitment =
  DaAvailabilityCommitmentSchema as unknown as DaAvailabilityCommitment;

const DaAvailabilityTrancheStartSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  tranche_index: Data.Integer(),
  start_offset: Data.Integer(),
  byte_length: Data.Integer(),
});

const DaAvailabilityTrancheStepSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  tranche_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

const DaAvailabilityTerminalAccumulatorStartSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

const DaAvailabilityChunkLeafSchema = Data.Object({
  version: Data.Integer(),
  tranche_index: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

export const DaAvailabilityBondDatumSchema = Data.Enum([
  Data.Object({
    Available: Data.Object({
      commitment: DaAvailabilityCommitmentSchema,
      da_bond_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
      attested_signers: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Object({
    ChallengedBond: Data.Object({
      commitment: DaAvailabilityCommitmentSchema,
      da_bond_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
      attested_signers: Data.Bytes({ minLength: 32, maxLength: 32 }),
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
      opened_at: Data.Integer(),
      response_deadline: Data.Integer(),
    }),
  }),
]);
export type DaAvailabilityBondDatum = Data.Static<
  typeof DaAvailabilityBondDatumSchema
>;
export const DaAvailabilityBondDatum =
  DaAvailabilityBondDatumSchema as unknown as DaAvailabilityBondDatum;

export const DaAvailabilityTrancheDatumSchema = Data.Enum([
  Data.Object({
    Active: Data.Object({
      deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
      header_hash: HeaderHashSchema,
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      descriptor: DaAvailabilityTrancheDescriptorSchema,
      next_offset: Data.Integer(),
      accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
      latest_carrier_output_index: Data.Nullable(Data.Integer()),
      response_deadline: Data.Integer(),
      challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
    }),
  }),
  Data.Object({
    Receipt: Data.Object({
      deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
      header_hash: HeaderHashSchema,
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      descriptor: DaAvailabilityTrancheDescriptorSchema,
      terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
      terminal_carrier_output_index: Data.Integer(),
      challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
    }),
  }),
]);
export type DaAvailabilityTrancheDatum = Data.Static<
  typeof DaAvailabilityTrancheDatumSchema
>;
export const DaAvailabilityTrancheDatum =
  DaAvailabilityTrancheDatumSchema as unknown as DaAvailabilityTrancheDatum;

export const DaAvailabilityTrancheTerminalStatusSchema = Data.Enum([
  Data.Object({
    PublishedTranche: Data.Object({
      terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Object({
    TimedOutTranche: Data.Object({
      next_offset: Data.Integer(),
      partial_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
]);
export type DaAvailabilityTrancheTerminalStatus = Data.Static<
  typeof DaAvailabilityTrancheTerminalStatusSchema
>;
export const DaAvailabilityTrancheTerminalStatus =
  DaAvailabilityTrancheTerminalStatusSchema as unknown as DaAvailabilityTrancheTerminalStatus;

const DaAvailabilityTerminalAccumulatorStepSchema = Data.Object({
  version: Data.Integer(),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  tranche_index: Data.Integer(),
  status: DaAvailabilityTrancheTerminalStatusSchema,
});

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
  DaAvailabilityTerminalAccumulatorDatumSchema as unknown as DaAvailabilityTerminalAccumulatorDatum;

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
  DaAvailabilityPublicationDatumSchema as unknown as DaAvailabilityPublicationDatum;

/** Exact minting-policy ABI for the retained DA bond/challenge lifecycle. */
export const DaAvailabilityMintRedeemerSchema = Data.Enum([
  Data.Object({
    MintBondFromAttestation: Data.Object({
      hub_oracle_ref_input_index: Data.Integer(),
      da_attestation_input_index: Data.Integer(),
      da_attestation_mint_redeemer_index: Data.Integer(),
      bond_output_index: Data.Integer(),
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    OpenChallenge: Data.Object({
      hub_oracle_ref_input_index: Data.Integer(),
      bond_input_index: Data.Integer(),
      bond_output_index: Data.Integer(),
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
      bond_ref_input_index: Data.Integer(),
      terminal_accumulator_input_index: Data.Integer(),
      terminal_accumulator_output_index: Data.Integer(),
      tranche_input_index: Data.Integer(),
      carrier_input_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    CloseChallenge: Data.Object({
      hub_oracle_ref_input_index: Data.Integer(),
      bond_input_index: Data.Integer(),
      terminal_accumulator_input_index: Data.Integer(),
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      da_refund_output_index: Data.Integer(),
      challenger_refund_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    TimeoutChallenge: Data.Object({
      hub_oracle_ref_input_index: Data.Integer(),
      bond_input_index: Data.Integer(),
      terminal_accumulator_input_index: Data.Integer(),
      state_queue_mint_redeemer_index: Data.Integer(),
      da_slash_output_index: Data.Integer(),
      challenger_refund_output_index: Data.Integer(),
    }),
  }),
]);
export type DaAvailabilityMintRedeemer = Data.Static<
  typeof DaAvailabilityMintRedeemerSchema
>;
export const DaAvailabilityMintRedeemer =
  DaAvailabilityMintRedeemerSchema as unknown as DaAvailabilityMintRedeemer;

/** Exact spending-validator ABI for bond, tranche and carrier UTxOs. */
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
  DaAvailabilitySpendRedeemerSchema as unknown as DaAvailabilitySpendRedeemer;

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

export const assertCanonicalDaAvailabilityResponseGeometry = (
  geometry: DaAvailabilityResponseGeometry,
): void => {
  const chunkByteLength = Number(geometry.chunk_byte_length);
  const trancheByteLength = Number(geometry.tranche_byte_length);
  const maxTrancheCount = Number(geometry.max_tranche_count);
  requireSafePositiveInteger(
    chunkByteLength,
    "response_geometry.chunk_byte_length",
  );
  requireSafePositiveInteger(
    trancheByteLength,
    "response_geometry.tranche_byte_length",
  );
  requireSafePositiveInteger(
    maxTrancheCount,
    "response_geometry.max_tranche_count",
  );
  if (
    BigInt(chunkByteLength) !== geometry.chunk_byte_length ||
    BigInt(trancheByteLength) !== geometry.tranche_byte_length ||
    BigInt(maxTrancheCount) !== geometry.max_tranche_count
  ) {
    throw new DaAvailabilityCommitmentError(
      "response geometry must fit canonical safe integers",
    );
  }
  if (chunkByteLength > DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES) {
    throw new DaAvailabilityCommitmentError(
      "response chunk exceeds the L1 reliable-publication safety ceiling",
    );
  }
  if (
    trancheByteLength < DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES ||
    trancheByteLength > DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES
  ) {
    throw new DaAvailabilityCommitmentError(
      "response tranche must cover the complete small class and stay within the 64 MiB payload ceiling",
    );
  }
  if (
    maxTrancheCount > DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY ||
    Math.ceil(DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES / trancheByteLength) >
      maxTrancheCount
  ) {
    throw new DaAvailabilityCommitmentError(
      "response geometry cannot cover the 64 MiB class within its authenticated tranche-count bound",
    );
  }
};

export const availabilityResponseGeometry = (input: {
  readonly chunkByteLength: number;
  readonly trancheByteLength: number;
  readonly maxTrancheCount: number;
}): DaAvailabilityResponseGeometry => {
  const geometry = {
    chunk_byte_length: BigInt(input.chunkByteLength),
    tranche_byte_length: BigInt(input.trancheByteLength),
    max_tranche_count: BigInt(input.maxTrancheCount),
  };
  assertCanonicalDaAvailabilityResponseGeometry(geometry);
  return geometry;
};

export const assertCanonicalDaAvailabilityParameters = (
  parameters: DaAvailabilityParameters,
): void => {
  assertCanonicalDaAvailabilityResponseGeometry(parameters.response_geometry);
  if (
    parameters.da_bond_lovelace <= 0n ||
    parameters.challenger_bond_lovelace !== parameters.da_bond_lovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "availability release parameters require positive, exactly matching DA and challenger bonds",
    );
  }
  if (
    parameters.max_open_fee_lovelace <= 0n ||
    parameters.max_publication_fee_lovelace <= 0n ||
    parameters.max_settlement_fee_lovelace <= 0n ||
    parameters.max_close_fee_lovelace <= 0n ||
    parameters.max_timeout_fee_lovelace <= 0n
  ) {
    throw new DaAvailabilityCommitmentError(
      "availability release fee ceilings must be positive measured values",
    );
  }
  const maximumPublicationCount = BigInt(
    maximumDaAvailabilityPublicationCount(parameters.response_geometry),
  );
  const terminalFeeCeiling =
    parameters.max_close_fee_lovelace > parameters.max_timeout_fee_lovelace
      ? parameters.max_close_fee_lovelace
      : parameters.max_timeout_fee_lovelace;
  if (
    maximumPublicationCount * parameters.max_publication_fee_lovelace +
      parameters.response_geometry.max_tranche_count *
        parameters.max_settlement_fee_lovelace +
      terminalFeeCeiling >=
    parameters.challenger_bond_lovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "challenger bond must cover every maximum-size publication fee plus the larger terminal fee ceiling",
    );
  }
};

export const daAvailabilityParameters = (input: {
  readonly responseGeometry: DaAvailabilityResponseGeometry;
  readonly daBondLovelace: bigint;
  readonly challengerBondLovelace: bigint;
  readonly maxOpenFeeLovelace: bigint;
  readonly maxPublicationFeeLovelace: bigint;
  readonly maxSettlementFeeLovelace: bigint;
  readonly maxCloseFeeLovelace: bigint;
  readonly maxTimeoutFeeLovelace: bigint;
}): DaAvailabilityParameters => {
  const parameters = {
    response_geometry: input.responseGeometry,
    da_bond_lovelace: input.daBondLovelace,
    challenger_bond_lovelace: input.challengerBondLovelace,
    max_open_fee_lovelace: input.maxOpenFeeLovelace,
    max_publication_fee_lovelace: input.maxPublicationFeeLovelace,
    max_settlement_fee_lovelace: input.maxSettlementFeeLovelace,
    max_close_fee_lovelace: input.maxCloseFeeLovelace,
    max_timeout_fee_lovelace: input.maxTimeoutFeeLovelace,
  };
  assertCanonicalDaAvailabilityParameters(parameters);
  return parameters;
};

export const encodeDaAvailabilityParameters = (
  parameters: DaAvailabilityParameters,
): string => {
  assertCanonicalDaAvailabilityParameters(parameters);
  return Data.to(parameters as never, DaAvailabilityParametersSchema as never);
};

/**
 * Strict durable/configuration codec. Shape-compatible or non-canonical CBOR
 * is never accepted as authenticated release parameters.
 */
export const parseDaAvailabilityParametersCbor = (
  cborHex: string,
): DaAvailabilityParameters => {
  const parameters = parseCanonicalDataCbor<
    typeof DaAvailabilityParametersSchema,
    DaAvailabilityParameters
  >({
    cborHex,
    schema: DaAvailabilityParametersSchema,
    name: "availability parameters",
  });
  assertCanonicalDaAvailabilityParameters(parameters);
  return parameters;
};

/**
 * Deterministic minimal tranche partition under the authenticated measured
 * geometry. Its tranche width/count are release data, while the exact 64 KiB
 * and 64 MiB response classes stay protocol-fixed.
 */
export const deriveDaAvailabilityTrancheLayout = (
  payloadByteLength: number,
  responseGeometry: DaAvailabilityResponseGeometry,
): readonly DaAvailabilityTrancheLayout[] => {
  daAvailabilityResponseWindowMs(payloadByteLength);
  assertCanonicalDaAvailabilityResponseGeometry(responseGeometry);
  const trancheByteLength = Number(responseGeometry.tranche_byte_length);
  const trancheCount = Math.ceil(payloadByteLength / trancheByteLength);
  if (trancheCount > Number(responseGeometry.max_tranche_count)) {
    throw new DaAvailabilityCommitmentError(
      "payload requires more than the authenticated response geometry's tranche bound",
    );
  }
  const result: DaAvailabilityTrancheLayout[] = [];
  let startOffset = 0;
  for (let trancheIndex = 0; trancheIndex < trancheCount; trancheIndex += 1) {
    const byteLength = Math.min(
      trancheByteLength,
      payloadByteLength - startOffset,
    );
    result.push({ trancheIndex, startOffset, byteLength });
    startOffset += byteLength;
  }
  return result;
};

export const maximumDaAvailabilityPublicationCount = (
  responseGeometry: DaAvailabilityResponseGeometry,
): number => {
  assertCanonicalDaAvailabilityResponseGeometry(responseGeometry);
  const chunkByteLength = Number(responseGeometry.chunk_byte_length);
  return deriveDaAvailabilityTrancheLayout(
    DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES,
    responseGeometry,
  ).reduce(
    (total, tranche) => total + Math.ceil(tranche.byteLength / chunkByteLength),
    0,
  );
};

export const daAvailabilityTrancheStartAccumulator = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly trancheIndex: number;
  readonly startOffset: number;
  readonly byteLength: number;
}): string => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  requireSafePositiveInteger(input.byteLength, "byteLength");
  if (
    !Number.isSafeInteger(input.trancheIndex) ||
    input.trancheIndex < 0 ||
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY ||
    !Number.isSafeInteger(input.startOffset) ||
    input.startOffset < 0
  ) {
    throw new DaAvailabilityCommitmentError(
      "tranche index and start offset must be canonical non-negative integers",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      tranche_index: BigInt(input.trancheIndex),
      start_offset: BigInt(input.startOffset),
      byte_length: BigInt(input.byteLength),
    } as never,
    DaAvailabilityTrancheStartSchema as never,
  );
  return hashDomainAndData(TRANCHE_START_DOMAIN, cbor);
};

export const daAvailabilityTerminalAccumulatorStart = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly challengeAssetName: string;
}): string => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentError(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      challenge_asset_name: input.challengeAssetName,
    } as never,
    DaAvailabilityTerminalAccumulatorStartSchema as never,
  );
  return hashDomainAndData(TERMINAL_ACCUMULATOR_START_DOMAIN, cbor);
};

/** Cross-language twin of the bounded per-tranche terminal fold. */
export const foldDaAvailabilityTerminalAccumulator = (input: {
  readonly previousAccumulator: string;
  readonly trancheIndex: number;
  readonly status: DaAvailabilityTrancheTerminalStatus;
}): string => {
  requireHash(input.previousAccumulator, 32, "previousAccumulator");
  if (
    !Number.isSafeInteger(input.trancheIndex) ||
    input.trancheIndex < 0 ||
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY
  ) {
    throw new DaAvailabilityCommitmentError(
      "terminal accumulator tranche index must be a canonical bounded integer",
    );
  }
  if ("PublishedTranche" in input.status) {
    requireHash(
      input.status.PublishedTranche.terminal_accumulator,
      32,
      "status.PublishedTranche.terminal_accumulator",
    );
  } else {
    const timedOut = input.status.TimedOutTranche;
    if (timedOut.next_offset < 0n) {
      throw new DaAvailabilityCommitmentError(
        "timed-out tranche next offset must be non-negative",
      );
    }
    requireHash(
      timedOut.partial_accumulator,
      32,
      "status.TimedOutTranche.partial_accumulator",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_VERSION,
      previous_accumulator: input.previousAccumulator,
      tranche_index: BigInt(input.trancheIndex),
      status: input.status,
    } as never,
    DaAvailabilityTerminalAccumulatorStepSchema as never,
  );
  return hashDomainAndData(TERMINAL_ACCUMULATOR_STEP_DOMAIN, cbor);
};

export const daAvailabilityTrancheStepAccumulator = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly trancheIndex: number;
  readonly chunkOffset: number;
  readonly chunk: Uint8Array;
  readonly previousAccumulator: string;
}): string => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  requireHash(input.previousAccumulator, 32, "previousAccumulator");
  requireSafePositiveInteger(input.chunk.length, "chunk.length");
  if (
    !Number.isSafeInteger(input.trancheIndex) ||
    input.trancheIndex < 0 ||
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY ||
    !Number.isSafeInteger(input.chunkOffset) ||
    input.chunkOffset < 0
  ) {
    throw new DaAvailabilityCommitmentError(
      "tranche index and chunk offset must be canonical non-negative integers",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      tranche_index: BigInt(input.trancheIndex),
      chunk_offset: BigInt(input.chunkOffset),
      chunk_byte_length: BigInt(input.chunk.length),
      chunk_hash: toHex(blake2b(input.chunk, { dkLen: 32 })),
      previous_accumulator: input.previousAccumulator,
    } as never,
    DaAvailabilityTrancheStepSchema as never,
  );
  return hashDomainAndData(TRANCHE_STEP_DOMAIN, cbor);
};

export const daAvailabilityChunkLeafHash = (input: {
  readonly trancheIndex: number;
  readonly chunkIndex: number;
  readonly chunkOffset: number;
  readonly chunkByteLength: number;
  readonly chunkHash: string;
}): string => {
  for (const [field, value] of [
    ["trancheIndex", input.trancheIndex],
    ["chunkIndex", input.chunkIndex],
    ["chunkOffset", input.chunkOffset],
  ] as const) {
    if (!Number.isSafeInteger(value) || value < 0) {
      throw new DaAvailabilityCommitmentError(
        `${field} must be a canonical non-negative safe integer`,
      );
    }
  }
  requireSafePositiveInteger(input.chunkByteLength, "chunkByteLength");
  requireHash(input.chunkHash, 32, "chunkHash");
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_VERSION,
      tranche_index: BigInt(input.trancheIndex),
      chunk_index: BigInt(input.chunkIndex),
      chunk_offset: BigInt(input.chunkOffset),
      chunk_byte_length: BigInt(input.chunkByteLength),
      chunk_hash: input.chunkHash,
    } as never,
    DaAvailabilityChunkLeafSchema as never,
  );
  return hashDomainAndData(CHUNK_LEAF_DOMAIN, cbor);
};

const trancheChunkLeafHashes = (input: {
  readonly layout: DaAvailabilityTrancheLayout;
  readonly payload: Uint8Array;
  readonly chunkByteLength: number;
}): readonly Buffer[] => {
  const leaves: Buffer[] = [];
  const endOffset = input.layout.startOffset + input.layout.byteLength;
  let chunkIndex = 0;
  for (
    let chunkOffset = input.layout.startOffset;
    chunkOffset < endOffset;
    chunkOffset += input.chunkByteLength
  ) {
    const chunkEnd = Math.min(chunkOffset + input.chunkByteLength, endOffset);
    const chunk = input.payload.subarray(chunkOffset, chunkEnd);
    leaves.push(
      Buffer.from(
        daAvailabilityChunkLeafHash({
          trancheIndex: input.layout.trancheIndex,
          chunkIndex,
          chunkOffset,
          chunkByteLength: chunk.length,
          chunkHash: toHex(blake2b(chunk, { dkLen: 32 })),
        }),
        "hex",
      ),
    );
    chunkIndex += 1;
  }
  return leaves;
};

const terminalAccumulator = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly layout: DaAvailabilityTrancheLayout;
  readonly payload: Uint8Array;
  readonly chunkByteLength: number;
}): string => {
  let accumulator = daAvailabilityTrancheStartAccumulator({
    deploymentIdentity: input.deploymentIdentity,
    headerHash: input.headerHash,
    trancheIndex: input.layout.trancheIndex,
    startOffset: input.layout.startOffset,
    byteLength: input.layout.byteLength,
  });
  const endOffset = input.layout.startOffset + input.layout.byteLength;
  for (
    let chunkOffset = input.layout.startOffset;
    chunkOffset < endOffset;
    chunkOffset += input.chunkByteLength
  ) {
    const chunkEnd = Math.min(chunkOffset + input.chunkByteLength, endOffset);
    accumulator = daAvailabilityTrancheStepAccumulator({
      deploymentIdentity: input.deploymentIdentity,
      headerHash: input.headerHash,
      trancheIndex: input.layout.trancheIndex,
      chunkOffset,
      chunk: input.payload.subarray(chunkOffset, chunkEnd),
      previousAccumulator: accumulator,
    });
  }
  return accumulator;
};

export const buildDaAvailabilityCommitment = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly payload: Uint8Array;
  readonly bondOwner: string;
  readonly responseGeometry: DaAvailabilityResponseGeometry;
}): DaAvailabilityCommitment => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  requireHash(input.bondOwner, 28, "bondOwner");
  assertCanonicalDaAvailabilityResponseGeometry(input.responseGeometry);
  const chunkByteLength = Number(input.responseGeometry.chunk_byte_length);
  const layout = deriveDaAvailabilityTrancheLayout(
    input.payload.length,
    input.responseGeometry,
  );
  const trancheDescriptors = layout.map(
    (entry): DaAvailabilityTrancheDescriptor => {
      const leaves = trancheChunkLeafHashes({
        layout: entry,
        payload: input.payload,
        chunkByteLength,
      });
      const membership = buildMidgardValidationMerkleMembershipIndex(leaves);
      return {
        tranche_index: BigInt(entry.trancheIndex),
        start_offset: BigInt(entry.startOffset),
        byte_length: BigInt(entry.byteLength),
        chunk_count: BigInt(leaves.length),
        chunk_commitment: toHex(
          commitMidgardValidationMerkleFrontier(membership.frontier),
        ),
        terminal_accumulator: terminalAccumulator({
          deploymentIdentity: input.deploymentIdentity,
          headerHash: input.headerHash,
          layout: entry,
          payload: input.payload,
          chunkByteLength,
        }),
      };
    },
  );
  return {
    version: DA_AVAILABILITY_COMMITMENT_VERSION,
    deployment_identity: input.deploymentIdentity,
    header_hash: input.headerHash,
    payload_byte_length: BigInt(input.payload.length),
    response_geometry: input.responseGeometry,
    tranche_descriptors: trancheDescriptors,
    bond_owner: input.bondOwner,
  };
};

export const assertCanonicalDaAvailabilityCommitment = (
  commitment: DaAvailabilityCommitment,
  expectedResponseGeometry?: DaAvailabilityResponseGeometry,
): void => {
  if (commitment.version !== DA_AVAILABILITY_COMMITMENT_VERSION) {
    throw new DaAvailabilityCommitmentError(
      "availability commitment version must be exactly V1",
    );
  }
  requireHash(commitment.deployment_identity, 28, "deployment_identity");
  requireHash(commitment.header_hash, 28, "header_hash");
  requireHash(commitment.bond_owner, 28, "bond_owner");
  const payloadByteLength = Number(commitment.payload_byte_length);
  requireSafePositiveInteger(payloadByteLength, "payload_byte_length");
  if (BigInt(payloadByteLength) !== commitment.payload_byte_length) {
    throw new DaAvailabilityCommitmentError(
      "payload length must fit a canonical safe integer",
    );
  }
  assertCanonicalDaAvailabilityResponseGeometry(commitment.response_geometry);
  if (
    expectedResponseGeometry !== undefined &&
    Data.to(
      commitment.response_geometry as never,
      DaAvailabilityResponseGeometrySchema as never,
    ) !==
      Data.to(
        expectedResponseGeometry as never,
        DaAvailabilityResponseGeometrySchema as never,
      )
  ) {
    throw new DaAvailabilityCommitmentError(
      "response geometry does not equal the authenticated deployment/DA parameters",
    );
  }
  const layout = deriveDaAvailabilityTrancheLayout(
    payloadByteLength,
    commitment.response_geometry,
  );
  if (commitment.tranche_descriptors.length !== layout.length) {
    throw new DaAvailabilityCommitmentError(
      "tranche descriptor count is not the deterministic minimal partition",
    );
  }
  for (const [index, descriptor] of commitment.tranche_descriptors.entries()) {
    const expected = layout[index];
    if (
      expected === undefined ||
      descriptor.tranche_index !== BigInt(expected.trancheIndex) ||
      descriptor.start_offset !== BigInt(expected.startOffset) ||
      descriptor.byte_length !== BigInt(expected.byteLength) ||
      descriptor.chunk_count !==
        BigInt(
          Math.ceil(
            expected.byteLength /
              Number(commitment.response_geometry.chunk_byte_length),
          ),
        )
    ) {
      throw new DaAvailabilityCommitmentError(
        "tranche descriptors must be contiguous, ordered, and minimally partitioned",
      );
    }
    requireHash(
      descriptor.chunk_commitment,
      32,
      `tranche_descriptors[${index.toString()}].chunk_commitment`,
    );
    requireHash(
      descriptor.terminal_accumulator,
      32,
      `tranche_descriptors[${index.toString()}].terminal_accumulator`,
    );
  }
};

export const encodeDaAvailabilityCommitment = (
  commitment: DaAvailabilityCommitment,
): string => {
  assertCanonicalDaAvailabilityCommitment(commitment);
  return Data.to(commitment as never, DaAvailabilityCommitmentSchema as never);
};

/** Strict signed-commitment codec for restart and cross-service handoff. */
export const parseDaAvailabilityCommitmentCbor = (
  cborHex: string,
  expectedResponseGeometry?: DaAvailabilityResponseGeometry,
): DaAvailabilityCommitment => {
  const commitment = parseCanonicalDataCbor<
    typeof DaAvailabilityCommitmentSchema,
    DaAvailabilityCommitment
  >({
    cborHex,
    schema: DaAvailabilityCommitmentSchema,
    name: "availability commitment",
  });
  assertCanonicalDaAvailabilityCommitment(commitment, expectedResponseGeometry);
  return commitment;
};

const assertCanonicalDaAvailabilityTrancheDescriptor = (
  descriptor: DaAvailabilityTrancheDescriptor,
): void => {
  const trancheIndex = Number(descriptor.tranche_index);
  const startOffset = Number(descriptor.start_offset);
  const byteLength = Number(descriptor.byte_length);
  if (
    !Number.isSafeInteger(trancheIndex) ||
    trancheIndex < 0 ||
    trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY ||
    !Number.isSafeInteger(startOffset) ||
    startOffset < 0 ||
    BigInt(trancheIndex) !== descriptor.tranche_index ||
    BigInt(startOffset) !== descriptor.start_offset
  ) {
    throw new DaAvailabilityCommitmentError(
      "tranche descriptor index and offset must be canonical bounded integers",
    );
  }
  requireSafePositiveInteger(byteLength, "tranche descriptor byte_length");
  if (
    BigInt(byteLength) !== descriptor.byte_length ||
    startOffset + byteLength > DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES
  ) {
    throw new DaAvailabilityCommitmentError(
      "tranche descriptor must fit within the canonical 64 MiB payload range",
    );
  }
  requireHash(
    descriptor.terminal_accumulator,
    32,
    "tranche descriptor terminal_accumulator",
  );
};

export const assertCanonicalDaAvailabilityBondDatum = (
  datum: DaAvailabilityBondDatum,
  expectedParameters?: DaAvailabilityParameters,
): void => {
  if (expectedParameters !== undefined) {
    assertCanonicalDaAvailabilityParameters(expectedParameters);
  }
  const fields = "Available" in datum ? datum.Available : datum.ChallengedBond;
  assertCanonicalDaAvailabilityCommitment(
    fields.commitment,
    expectedParameters?.response_geometry,
  );
  if (!BOND_ASSET_NAME.test(fields.da_bond_asset_name)) {
    throw new DaAvailabilityCommitmentError(
      "da_bond_asset_name must be the canonical 32-byte DABN identity",
    );
  }
  requireHash(fields.committee_signers_hash, 32, "committee_signers_hash");
  requireHash(fields.attested_signers, 32, "attested_signers");
  if ("ChallengedBond" in datum) {
    const challenged = datum.ChallengedBond;
    if (!CHALLENGE_ASSET_NAME.test(challenged.challenge_asset_name)) {
      throw new DaAvailabilityCommitmentError(
        "challenge_asset_name must be the canonical 32-byte DACH identity",
      );
    }
    requireHash(challenged.challenger, 28, "challenger");
    if (
      challenged.opened_at < 0n ||
      challenged.response_deadline !==
        daAvailabilityResponseDeadline({
          payloadByteLength: Number(challenged.commitment.payload_byte_length),
          openedAt: challenged.opened_at,
        })
    ) {
      throw new DaAvailabilityCommitmentError(
        "challenged bond must carry the exact canonical response deadline",
      );
    }
  }
};

export const encodeDaAvailabilityBondDatum = (
  datum: DaAvailabilityBondDatum,
  expectedParameters?: DaAvailabilityParameters,
): string => {
  assertCanonicalDaAvailabilityBondDatum(datum, expectedParameters);
  return Data.to(datum as never, DaAvailabilityBondDatumSchema as never);
};

export const parseDaAvailabilityBondDatumCbor = (
  cborHex: string,
  expectedParameters?: DaAvailabilityParameters,
): DaAvailabilityBondDatum => {
  const datum = parseCanonicalDataCbor<
    typeof DaAvailabilityBondDatumSchema,
    DaAvailabilityBondDatum
  >({
    cborHex,
    schema: DaAvailabilityBondDatumSchema,
    name: "availability bond datum",
  });
  assertCanonicalDaAvailabilityBondDatum(datum, expectedParameters);
  return datum;
};

export const assertCanonicalDaAvailabilityTrancheDatum = (
  datum: DaAvailabilityTrancheDatum,
): void => {
  const fields = "Active" in datum ? datum.Active : datum.Receipt;
  requireHash(fields.deployment_identity, 28, "deployment_identity");
  requireHash(fields.header_hash, 28, "header_hash");
  if (!CHALLENGE_ASSET_NAME.test(fields.challenge_asset_name)) {
    throw new DaAvailabilityCommitmentError(
      "challenge_asset_name must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(fields.challenger, 28, "challenger");
  assertCanonicalDaAvailabilityTrancheDescriptor(fields.descriptor);
  if ("Active" in datum) {
    const active = datum.Active;
    const endOffset =
      active.descriptor.start_offset + active.descriptor.byte_length;
    if (
      active.next_offset < active.descriptor.start_offset ||
      active.next_offset >= endOffset ||
      active.response_deadline < 0n
    ) {
      throw new DaAvailabilityCommitmentError(
        "active tranche cursor/deadline is outside its canonical range",
      );
    }
    requireHash(active.accumulator, 32, "accumulator");
  } else if (
    datum.Receipt.terminal_accumulator !==
    datum.Receipt.descriptor.terminal_accumulator
  ) {
    throw new DaAvailabilityCommitmentError(
      "receipt terminal accumulator must equal its signed descriptor",
    );
  }
};

export const encodeDaAvailabilityTrancheDatum = (
  datum: DaAvailabilityTrancheDatum,
): string => {
  assertCanonicalDaAvailabilityTrancheDatum(datum);
  return Data.to(datum as never, DaAvailabilityTrancheDatumSchema as never);
};

export const parseDaAvailabilityTrancheDatumCbor = (
  cborHex: string,
): DaAvailabilityTrancheDatum => {
  const datum = parseCanonicalDataCbor<
    typeof DaAvailabilityTrancheDatumSchema,
    DaAvailabilityTrancheDatum
  >({
    cborHex,
    schema: DaAvailabilityTrancheDatumSchema,
    name: "availability tranche datum",
  });
  assertCanonicalDaAvailabilityTrancheDatum(datum);
  return datum;
};

export const assertCanonicalDaAvailabilityTerminalAccumulatorDatum = (
  datum: DaAvailabilityTerminalAccumulatorDatum,
): void => {
  requireHash(datum.deployment_identity, 28, "deployment_identity");
  requireHash(datum.header_hash, 28, "header_hash");
  if (!CHALLENGE_ASSET_NAME.test(datum.challenge_asset_name)) {
    throw new DaAvailabilityCommitmentError(
      "challenge_asset_name must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(
    datum.folded_terminal_accumulator,
    32,
    "folded_terminal_accumulator",
  );
  requireHash(datum.challenger, 28, "challenger");
  if (
    datum.next_tranche_index < 0n ||
    datum.next_tranche_index >
      BigInt(DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY) ||
    datum.response_deadline < 0n ||
    datum.remaining_challenger_lovelace <= 0n
  ) {
    throw new DaAvailabilityCommitmentError(
      "terminal accumulator cursor, deadline, and remaining challenger value must be canonical",
    );
  }
};

export const encodeDaAvailabilityTerminalAccumulatorDatum = (
  datum: DaAvailabilityTerminalAccumulatorDatum,
): string => {
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(datum);
  return Data.to(
    datum as never,
    DaAvailabilityTerminalAccumulatorDatumSchema as never,
  );
};

export const parseDaAvailabilityTerminalAccumulatorDatumCbor = (
  cborHex: string,
): DaAvailabilityTerminalAccumulatorDatum => {
  const datum = parseCanonicalDataCbor<
    typeof DaAvailabilityTerminalAccumulatorDatumSchema,
    DaAvailabilityTerminalAccumulatorDatum
  >({
    cborHex,
    schema: DaAvailabilityTerminalAccumulatorDatumSchema,
    name: "availability terminal accumulator datum",
  });
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(datum);
  return datum;
};

export type DaAvailabilityChallengeDatumPlan = Readonly<{
  challengeAssetName: string;
  responseDeadline: bigint;
  challengedBond: DaAvailabilityBondDatum;
  trancheThreads: readonly DaAvailabilityTrancheDatum[];
  trancheFunding: readonly DaAvailabilityTrancheFunding[];
  terminalAccumulator: DaAvailabilityTerminalAccumulatorDatum;
  terminalAccumulatorFundingLovelace: bigint;
}>;

export type DaAvailabilityTrancheFunding = Readonly<{
  trancheIndex: number;
  initialLovelace: bigint;
  maximumPublicationFeeReserveLovelace: bigint;
  maximumSettlementFeeReserveLovelace: bigint;
}>;

/**
 * Deterministic exact split of the approved challenger fee bond. Each tranche
 * first receives enough value for every publication and its one settlement at
 * their authenticated fee ceilings; the terminal accumulator separately holds
 * the larger close/timeout ceiling. All remaining working/refund value is
 * split with a one-lovelace remainder assigned to the earliest descriptors.
 */
export const planDaAvailabilityTrancheFunding = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly parameters: DaAvailabilityParameters;
}): readonly DaAvailabilityTrancheFunding[] => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  assertCanonicalDaAvailabilityCommitment(
    input.commitment,
    input.parameters.response_geometry,
  );
  const trancheCount = input.commitment.tranche_descriptors.length;
  requireSafePositiveInteger(trancheCount, "trancheCount");
  const chunkByteLength = Number(
    input.parameters.response_geometry.chunk_byte_length,
  );
  const publicationFeeReserves = input.commitment.tranche_descriptors.map(
    (descriptor) =>
      BigInt(Math.ceil(Number(descriptor.byte_length) / chunkByteLength)) *
      input.parameters.max_publication_fee_lovelace,
  );
  const totalPublicationFeeReserve = publicationFeeReserves.reduce(
    (total, reserve) => total + reserve,
    0n,
  );
  const totalSettlementFeeReserve =
    BigInt(trancheCount) * input.parameters.max_settlement_fee_lovelace;
  const terminalFeeCeiling =
    input.parameters.max_close_fee_lovelace >
    input.parameters.max_timeout_fee_lovelace
      ? input.parameters.max_close_fee_lovelace
      : input.parameters.max_timeout_fee_lovelace;
  const distributable =
    input.parameters.challenger_bond_lovelace -
    totalPublicationFeeReserve -
    totalSettlementFeeReserve -
    terminalFeeCeiling;
  if (distributable <= 0n) {
    throw new DaAvailabilityCommitmentError(
      "challenger bond does not leave working/refund value after publication, settlement, and terminal fee reserves",
    );
  }
  const count = BigInt(trancheCount);
  const base = distributable / count;
  const remainder = distributable % count;
  return publicationFeeReserves.map(
    (maximumPublicationFeeReserveLovelace, trancheIndex) => ({
      trancheIndex,
      maximumPublicationFeeReserveLovelace,
      maximumSettlementFeeReserveLovelace:
        input.parameters.max_settlement_fee_lovelace,
      initialLovelace:
        maximumPublicationFeeReserveLovelace +
        input.parameters.max_settlement_fee_lovelace +
        base +
        (BigInt(trancheIndex) < remainder ? 1n : 0n),
    }),
  );
};

/**
 * Datum/value-topology plan for the approved split challenger fee bond. It
 * fixes identity, deadline and the initial per-tranche shares, while the
 * measured fee ceiling and each exact transaction fee remain separate.
 */
export const buildDaAvailabilityChallengeDatumPlan = (input: {
  readonly availableBond: DaAvailabilityBondDatum;
  readonly bondInputOutRef: OutputReference;
  readonly challenger: string;
  readonly openedAt: bigint;
  readonly parameters: DaAvailabilityParameters;
}): DaAvailabilityChallengeDatumPlan => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  assertCanonicalDaAvailabilityBondDatum(input.availableBond, input.parameters);
  if (!("Available" in input.availableBond)) {
    throw new DaAvailabilityCommitmentError(
      "only an available retained DA bond may open a challenge",
    );
  }
  requireHash(input.challenger, 28, "challenger");
  const available = input.availableBond.Available;
  const challengeAssetName = daAvailabilityChallengeAssetName(
    input.bondInputOutRef,
  );
  const responseDeadline = daAvailabilityResponseDeadline({
    payloadByteLength: Number(available.commitment.payload_byte_length),
    openedAt: input.openedAt,
  });
  const challengedBond: DaAvailabilityBondDatum = {
    ChallengedBond: {
      ...available,
      challenge_asset_name: challengeAssetName,
      challenger: input.challenger,
      opened_at: input.openedAt,
      response_deadline: responseDeadline,
    },
  };
  const trancheThreads = available.commitment.tranche_descriptors.map(
    (descriptor): DaAvailabilityTrancheDatum => ({
      Active: {
        deployment_identity: available.commitment.deployment_identity,
        header_hash: available.commitment.header_hash,
        challenge_asset_name: challengeAssetName,
        descriptor,
        next_offset: descriptor.start_offset,
        accumulator: daAvailabilityTrancheStartAccumulator({
          deploymentIdentity: available.commitment.deployment_identity,
          headerHash: available.commitment.header_hash,
          trancheIndex: Number(descriptor.tranche_index),
          startOffset: Number(descriptor.start_offset),
          byteLength: Number(descriptor.byte_length),
        }),
        latest_carrier_output_index: null,
        response_deadline: responseDeadline,
        challenger: input.challenger,
      },
    }),
  );
  assertCanonicalDaAvailabilityBondDatum(challengedBond, input.parameters);
  trancheThreads.forEach(assertCanonicalDaAvailabilityTrancheDatum);
  const trancheFunding = planDaAvailabilityTrancheFunding({
    commitment: available.commitment,
    parameters: input.parameters,
  });
  const terminalAccumulatorFundingLovelace =
    input.parameters.max_close_fee_lovelace >
    input.parameters.max_timeout_fee_lovelace
      ? input.parameters.max_close_fee_lovelace
      : input.parameters.max_timeout_fee_lovelace;
  const terminalAccumulator: DaAvailabilityTerminalAccumulatorDatum = {
    deployment_identity: available.commitment.deployment_identity,
    header_hash: available.commitment.header_hash,
    challenge_asset_name: challengeAssetName,
    next_tranche_index: 0n,
    folded_terminal_accumulator: daAvailabilityTerminalAccumulatorStart({
      deploymentIdentity: available.commitment.deployment_identity,
      headerHash: available.commitment.header_hash,
      challengeAssetName,
    }),
    has_timed_out_tranche: false,
    response_deadline: responseDeadline,
    challenger: input.challenger,
    remaining_challenger_lovelace: terminalAccumulatorFundingLovelace,
  };
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(terminalAccumulator);
  return {
    challengeAssetName,
    responseDeadline,
    challengedBond,
    trancheThreads,
    trancheFunding,
    terminalAccumulator,
    terminalAccumulatorFundingLovelace,
  };
};

export const planDaAvailabilityPublicationValueTransition = (input: {
  readonly threadInputLovelace: bigint;
  readonly previousCarrierInputLovelace: bigint;
  readonly nextCarrierOutputLovelace: bigint;
  readonly transactionFeeLovelace: bigint;
  readonly minimumThreadOutputLovelace: bigint;
  readonly isFirstPublication: boolean;
  readonly parameters: DaAvailabilityParameters;
}): bigint => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  if (
    input.threadInputLovelace <= 0n ||
    input.previousCarrierInputLovelace < 0n ||
    input.nextCarrierOutputLovelace <= 0n ||
    input.minimumThreadOutputLovelace <= 0n ||
    input.transactionFeeLovelace <= 0n ||
    input.transactionFeeLovelace >
      input.parameters.max_publication_fee_lovelace ||
    (input.isFirstPublication
      ? input.previousCarrierInputLovelace !== 0n
      : input.previousCarrierInputLovelace <= 0n)
  ) {
    throw new DaAvailabilityCommitmentError(
      "publication value transition has a noncanonical carrier, thread floor, or fee above its authenticated ceiling",
    );
  }
  const threadOutputLovelace =
    input.threadInputLovelace +
    input.previousCarrierInputLovelace -
    input.nextCarrierOutputLovelace -
    input.transactionFeeLovelace;
  if (threadOutputLovelace < input.minimumThreadOutputLovelace) {
    throw new DaAvailabilityCommitmentError(
      "publication fee/carrier would consume the protected tranche working floor",
    );
  }
  return threadOutputLovelace;
};

export type DaAvailabilitySettlementPlan = Readonly<{
  status: DaAvailabilityTrancheTerminalStatus;
  nextTerminalAccumulator: DaAvailabilityTerminalAccumulatorDatum;
  nextTerminalLovelace: bigint;
}>;

/**
 * Pure mirror of one bounded `SettleTranche` transition. Production builders
 * feed it decoded, script-authenticated UTxO data and then emit the exact datum
 * and value it returns.
 */
export const planDaAvailabilitySettlement = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly terminalAccumulator: DaAvailabilityTerminalAccumulatorDatum;
  readonly tranche: DaAvailabilityTrancheDatum;
  readonly threadLovelace: bigint;
  readonly carrierLovelace: bigint;
  readonly transactionFeeLovelace: bigint;
  readonly inclusiveValidityLower: bigint;
  readonly parameters: DaAvailabilityParameters;
}): DaAvailabilitySettlementPlan => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  assertCanonicalDaAvailabilityCommitment(
    input.commitment,
    input.parameters.response_geometry,
  );
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(
    input.terminalAccumulator,
  );
  assertCanonicalDaAvailabilityTrancheDatum(input.tranche);
  const terminal = input.terminalAccumulator;
  const descriptorIndex = Number(terminal.next_tranche_index);
  if (
    !Number.isSafeInteger(descriptorIndex) ||
    descriptorIndex < 0 ||
    input.commitment.tranche_descriptors[descriptorIndex] === undefined ||
    input.threadLovelace <= 0n ||
    input.carrierLovelace < 0n ||
    input.transactionFeeLovelace <= 0n ||
    input.transactionFeeLovelace > input.parameters.max_settlement_fee_lovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "settlement indices, values, or fee are not canonical",
    );
  }
  const descriptor = input.commitment.tranche_descriptors[descriptorIndex]!;
  let status: DaAvailabilityTrancheTerminalStatus;
  let trancheIdentity: {
    readonly deployment_identity: string;
    readonly header_hash: string;
    readonly challenge_asset_name: string;
    readonly descriptor: DaAvailabilityTrancheDescriptor;
    readonly challenger: string;
  };
  if ("Receipt" in input.tranche) {
    trancheIdentity = input.tranche.Receipt;
    if (
      input.tranche.Receipt.terminal_accumulator !==
      descriptor.terminal_accumulator
    ) {
      throw new DaAvailabilityCommitmentError(
        "published settlement receipt does not equal its signed terminal accumulator",
      );
    }
    status = {
      PublishedTranche: {
        terminal_accumulator: descriptor.terminal_accumulator,
      },
    };
  } else {
    trancheIdentity = input.tranche.Active;
    if (input.inclusiveValidityLower < terminal.response_deadline) {
      throw new DaAvailabilityCommitmentError(
        "an active tranche may settle only at or after the authenticated deadline",
      );
    }
    status = {
      TimedOutTranche: {
        next_offset: input.tranche.Active.next_offset,
        partial_accumulator: input.tranche.Active.accumulator,
      },
    };
  }
  if (
    trancheIdentity.deployment_identity !==
      input.commitment.deployment_identity ||
    trancheIdentity.header_hash !== input.commitment.header_hash ||
    trancheIdentity.challenge_asset_name !== terminal.challenge_asset_name ||
    Data.to(
      trancheIdentity.descriptor as never,
      DaAvailabilityTrancheDescriptorSchema as never,
    ) !==
      Data.to(
        descriptor as never,
        DaAvailabilityTrancheDescriptorSchema as never,
      ) ||
    trancheIdentity.challenger !== terminal.challenger ||
    terminal.deployment_identity !== input.commitment.deployment_identity ||
    terminal.header_hash !== input.commitment.header_hash ||
    terminal.next_tranche_index !== descriptor.tranche_index
  ) {
    throw new DaAvailabilityCommitmentError(
      "settlement tranche, terminal accumulator, and signed commitment identities differ",
    );
  }
  const nextTerminalLovelace =
    terminal.remaining_challenger_lovelace +
    input.threadLovelace +
    input.carrierLovelace -
    input.transactionFeeLovelace;
  if (nextTerminalLovelace <= 0n) {
    throw new DaAvailabilityCommitmentError(
      "settlement consumes the protected challenger value",
    );
  }
  const nextTerminalAccumulator: DaAvailabilityTerminalAccumulatorDatum = {
    ...terminal,
    next_tranche_index: terminal.next_tranche_index + 1n,
    folded_terminal_accumulator: foldDaAvailabilityTerminalAccumulator({
      previousAccumulator: terminal.folded_terminal_accumulator,
      trancheIndex: descriptorIndex,
      status,
    }),
    has_timed_out_tranche:
      terminal.has_timed_out_tranche || "TimedOutTranche" in status,
    remaining_challenger_lovelace: nextTerminalLovelace,
  };
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(
    nextTerminalAccumulator,
  );
  return { status, nextTerminalAccumulator, nextTerminalLovelace };
};

export const assertDaAvailabilityChallengerBondConservation = (input: {
  readonly initialChallengerBondLovelace: bigint;
  readonly currentThreadLovelace: readonly bigint[];
  readonly currentCarrierLovelace: readonly bigint[];
  readonly paidTransactionFeesLovelace: readonly bigint[];
}): void => {
  const allValues = [
    ...input.currentThreadLovelace,
    ...input.currentCarrierLovelace,
  ];
  const allFees = input.paidTransactionFeesLovelace;
  if (
    input.initialChallengerBondLovelace <= 0n ||
    allValues.some((value) => value <= 0n) ||
    allFees.some((fee) => fee <= 0n) ||
    allValues.reduce((total, value) => total + value, 0n) +
      allFees.reduce((total, value) => total + value, 0n) !==
      input.initialChallengerBondLovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "challenger bond is not isolated and exactly conserved by live threads, carriers, and paid fees",
    );
  }
};

export type DaAvailabilityTrancheProtectedValue = Readonly<{
  trancheIndex: number;
  threadLovelace: bigint;
  carrierLovelace: bigint;
}>;

export type DaAvailabilityTrancheRefund = Readonly<{
  trancheIndex: number;
  refundLovelace: bigint;
  attributedTransactionFeeLovelace: bigint;
}>;

export const planDaAvailabilityTerminalRefund = (input: {
  readonly kind: "close" | "timeout";
  readonly tranches: readonly DaAvailabilityTrancheProtectedValue[];
  readonly transactionFeeLovelace: bigint;
  readonly parameters: DaAvailabilityParameters;
}): readonly DaAvailabilityTrancheRefund[] => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  const feeCeiling =
    input.kind === "close"
      ? input.parameters.max_close_fee_lovelace
      : input.parameters.max_timeout_fee_lovelace;
  if (
    input.tranches.length === 0 ||
    input.tranches.some(
      (value, index) =>
        value.trancheIndex !== index ||
        value.threadLovelace <= 0n ||
        value.carrierLovelace < 0n,
    ) ||
    input.transactionFeeLovelace <= 0n ||
    input.transactionFeeLovelace > feeCeiling
  ) {
    throw new DaAvailabilityCommitmentError(
      "terminal availability transition has a noncanonical protected value or fee above its authenticated ceiling",
    );
  }
  const refunds = input.tranches.map(
    (value, index): DaAvailabilityTrancheRefund => {
      const attributedTransactionFeeLovelace =
        index === 0 ? input.transactionFeeLovelace : 0n;
      return {
        trancheIndex: value.trancheIndex,
        attributedTransactionFeeLovelace,
        refundLovelace:
          value.threadLovelace +
          value.carrierLovelace -
          attributedTransactionFeeLovelace,
      };
    },
  );
  if (refunds.some((refund) => refund.refundLovelace <= 0n)) {
    throw new DaAvailabilityCommitmentError(
      "terminal availability transition leaves no challenger refund",
    );
  }
  return refunds;
};

export const assertCanonicalDaAvailabilityPublicationDatum = (
  publication: DaAvailabilityPublicationDatum,
  expectedResponseGeometry: DaAvailabilityResponseGeometry,
  expectedDescriptor: DaAvailabilityTrancheDescriptor,
): void => {
  requireHash(publication.deployment_identity, 28, "deployment_identity");
  requireHash(publication.header_hash, 28, "header_hash");
  if (!CHALLENGE_ASSET_NAME.test(publication.challenge_asset_name)) {
    throw new DaAvailabilityCommitmentError(
      "challenge_asset_name must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(publication.chunk_hash, 32, "chunk_hash");
  requireHash(publication.previous_accumulator, 32, "previous_accumulator");
  requireHash(publication.next_accumulator, 32, "next_accumulator");
  if (!CANONICAL_CBOR_HEX.test(publication.chunk)) {
    throw new DaAvailabilityCommitmentError(
      "availability publication chunk must be non-empty lowercase hex bytes",
    );
  }
  const trancheIndex = Number(publication.tranche_index);
  const chunkIndex = Number(publication.chunk_index);
  const chunkOffset = Number(publication.chunk_offset);
  const chunkByteLength = Number(publication.chunk_byte_length);
  if (
    !Number.isSafeInteger(trancheIndex) ||
    trancheIndex < 0 ||
    trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY ||
    !Number.isSafeInteger(chunkIndex) ||
    chunkIndex < 0 ||
    !Number.isSafeInteger(chunkOffset) ||
    chunkOffset < 0 ||
    BigInt(trancheIndex) !== publication.tranche_index ||
    BigInt(chunkIndex) !== publication.chunk_index ||
    BigInt(chunkOffset) !== publication.chunk_offset
  ) {
    throw new DaAvailabilityCommitmentError(
      "publication tranche/chunk indices and chunk offset must be canonical bounded integers",
    );
  }
  requireSafePositiveInteger(chunkByteLength, "chunk_byte_length");
  if (BigInt(chunkByteLength) !== publication.chunk_byte_length) {
    throw new DaAvailabilityCommitmentError(
      "publication chunk length must fit a canonical safe integer",
    );
  }
  const chunk = fromHex(publication.chunk);
  if (
    chunk.length !== chunkByteLength ||
    chunkByteLength > DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES
  ) {
    throw new DaAvailabilityCommitmentError(
      "publication chunk bytes do not equal its bounded declared length",
    );
  }
  assertCanonicalDaAvailabilityResponseGeometry(expectedResponseGeometry);
  if (
    publication.chunk_byte_length > expectedResponseGeometry.chunk_byte_length
  ) {
    throw new DaAvailabilityCommitmentError(
      "publication chunk exceeds the authenticated response geometry",
    );
  }
  if (publication.chunk_hash !== toHex(blake2b(chunk, { dkLen: 32 }))) {
    throw new DaAvailabilityCommitmentError(
      "publication chunk hash does not equal its inline bytes",
    );
  }
  const frontier = {
    count: publication.chunk_frontier.reduce((count, peak, index) => {
      const height = Number(peak.height);
      if (
        !Number.isSafeInteger(height) ||
        height < 0 ||
        BigInt(height) !== peak.height
      ) {
        throw new DaAvailabilityCommitmentError(
          `chunk_frontier[${index.toString()}].height is not canonical`,
        );
      }
      requireHash(peak.hash, 32, `chunk_frontier[${index.toString()}].hash`);
      return count + 2 ** height;
    }, 0),
    peaks: publication.chunk_frontier.map((peak) => ({
      height: Number(peak.height),
      hash: Buffer.from(peak.hash, "hex"),
    })),
  };
  for (const [index, sibling] of publication.chunk_siblings.entries()) {
    requireHash(sibling, 32, `chunk_siblings[${index.toString()}]`);
  }
  const leafHash = Buffer.from(
    daAvailabilityChunkLeafHash({
      trancheIndex,
      chunkIndex,
      chunkOffset,
      chunkByteLength,
      chunkHash: publication.chunk_hash,
    }),
    "hex",
  );
  if (
    !verifyMidgardValidationMerkleMembership({
      frontier,
      leafIndex: chunkIndex,
      leafHash,
      siblings: publication.chunk_siblings.map((sibling) =>
        Buffer.from(sibling, "hex"),
      ),
    })
  ) {
    throw new DaAvailabilityCommitmentError(
      "publication chunk is not an index-bound member of its signed frontier",
    );
  }
  if (
    expectedDescriptor.chunk_count !== BigInt(frontier.count) ||
    expectedDescriptor.chunk_commitment !==
      toHex(commitMidgardValidationMerkleFrontier(frontier))
  ) {
    throw new DaAvailabilityCommitmentError(
      "publication frontier does not equal the signed tranche descriptor",
    );
  }
  const expectedNextAccumulator = daAvailabilityTrancheStepAccumulator({
    deploymentIdentity: publication.deployment_identity,
    headerHash: publication.header_hash,
    trancheIndex,
    chunkOffset,
    chunk,
    previousAccumulator: publication.previous_accumulator,
  });
  if (publication.next_accumulator !== expectedNextAccumulator) {
    throw new DaAvailabilityCommitmentError(
      "publication next accumulator does not equal its canonical step",
    );
  }
};

export const encodeDaAvailabilityPublicationDatum = (
  publication: DaAvailabilityPublicationDatum,
  expectedResponseGeometry: DaAvailabilityResponseGeometry,
  expectedDescriptor: DaAvailabilityTrancheDescriptor,
): string => {
  assertCanonicalDaAvailabilityPublicationDatum(
    publication,
    expectedResponseGeometry,
    expectedDescriptor,
  );
  return Data.to(
    publication as never,
    DaAvailabilityPublicationDatumSchema as never,
  );
};

/** Strict inline-publication codec; L1 provenance remains service-owned. */
export const parseDaAvailabilityPublicationDatumCbor = (
  cborHex: string,
  expectedResponseGeometry: DaAvailabilityResponseGeometry,
  expectedDescriptor: DaAvailabilityTrancheDescriptor,
): DaAvailabilityPublicationDatum => {
  const publication = parseCanonicalDataCbor<
    typeof DaAvailabilityPublicationDatumSchema,
    DaAvailabilityPublicationDatum
  >({
    cborHex,
    schema: DaAvailabilityPublicationDatumSchema,
    name: "availability publication",
  });
  assertCanonicalDaAvailabilityPublicationDatum(
    publication,
    expectedResponseGeometry,
    expectedDescriptor,
  );
  return publication;
};

export const daAvailabilityAttestationMessage = (
  commitment: DaAvailabilityCommitment,
): Uint8Array => {
  assertCanonicalDaAvailabilityCommitment(commitment);
  return fromHex(
    hashDomainAndData(
      ATTESTATION_COMMITMENT_DOMAIN,
      Data.to(commitment as never, DaAvailabilityCommitmentSchema as never),
    ),
  );
};

/** Compact state-queue marker admitted only after every ordered receipt. */
export const daAvailabilityPublishedTerminalCommitment = (
  commitment: DaAvailabilityCommitment,
): string => {
  assertCanonicalDaAvailabilityCommitment(commitment);
  return hashDomainAndData(
    PUBLISHED_TERMINAL_DOMAIN,
    Data.to(commitment as never, DaAvailabilityCommitmentSchema as never),
  );
};

export const verifyDaAvailabilityPayloadCommitment = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly payload: Uint8Array;
}): boolean => {
  assertCanonicalDaAvailabilityCommitment(input.commitment);
  if (BigInt(input.payload.length) !== input.commitment.payload_byte_length) {
    return false;
  }
  const rebuilt = buildDaAvailabilityCommitment({
    deploymentIdentity: input.commitment.deployment_identity,
    headerHash: input.commitment.header_hash,
    payload: input.payload,
    bondOwner: input.commitment.bond_owner,
    responseGeometry: input.commitment.response_geometry,
  });
  return (
    Data.to(rebuilt as never, DaAvailabilityCommitmentSchema as never) ===
    Data.to(input.commitment as never, DaAvailabilityCommitmentSchema as never)
  );
};

export type DaAvailabilityTranchePublicationPlan = Readonly<{
  descriptor: DaAvailabilityTrancheDescriptor;
  initialAccumulator: string;
  publications: readonly DaAvailabilityPublicationDatum[];
}>;

export type DaAvailabilityPublicationTier =
  | "complete_item_inline"
  | "ordered_chunks"
  | "parallel_tranches";

/**
 * Chooses the least fragmented response tier permitted by the authenticated
 * applied-transaction measurement. A complete item is never split when it
 * fits the signed inline-publication byte limit.
 */
export const daAvailabilityPublicationTier = (input: {
  readonly payloadByteLength: number;
  readonly responseGeometry: DaAvailabilityResponseGeometry;
}): DaAvailabilityPublicationTier => {
  daAvailabilityResponseWindowMs(input.payloadByteLength);
  assertCanonicalDaAvailabilityResponseGeometry(input.responseGeometry);
  if (
    input.payloadByteLength <= Number(input.responseGeometry.chunk_byte_length)
  ) {
    return "complete_item_inline";
  }
  return input.payloadByteLength <=
    Number(input.responseGeometry.tranche_byte_length)
    ? "ordered_chunks"
    : "parallel_tranches";
};

/**
 * Reconstructs the exact ordered public response. Each publication carries one
 * inline-datum chunk, while the continued tranche UTxO can remain compact.
 */
export const planDaAvailabilityPublications = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly payload: Uint8Array;
  readonly challengeAssetName: string;
}): readonly DaAvailabilityTranchePublicationPlan[] => {
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentError(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  if (
    !verifyDaAvailabilityPayloadCommitment({
      commitment: input.commitment,
      payload: input.payload,
    })
  ) {
    throw new DaAvailabilityCommitmentError(
      "payload does not equal the signed DA availability commitment",
    );
  }
  const chunkByteLength = Number(
    input.commitment.response_geometry.chunk_byte_length,
  );
  const tier = daAvailabilityPublicationTier({
    payloadByteLength: input.payload.length,
    responseGeometry: input.commitment.response_geometry,
  });
  return input.commitment.tranche_descriptors.map((descriptor) => {
    const trancheIndex = Number(descriptor.tranche_index);
    const startOffset = Number(descriptor.start_offset);
    const endOffset = startOffset + Number(descriptor.byte_length);
    const layout = {
      trancheIndex,
      startOffset,
      byteLength: Number(descriptor.byte_length),
    } satisfies DaAvailabilityTrancheLayout;
    const chunkLeaves = trancheChunkLeafHashes({
      layout,
      payload: input.payload,
      chunkByteLength,
    });
    const membershipIndex =
      buildMidgardValidationMerkleMembershipIndex(chunkLeaves);
    if (
      descriptor.chunk_count !== BigInt(chunkLeaves.length) ||
      descriptor.chunk_commitment !==
        toHex(commitMidgardValidationMerkleFrontier(membershipIndex.frontier))
    ) {
      throw new DaAvailabilityCommitmentError(
        `tranche ${trancheIndex.toString()} chunk commitment does not equal the signed payload`,
      );
    }
    const initialAccumulator = daAvailabilityTrancheStartAccumulator({
      deploymentIdentity: input.commitment.deployment_identity,
      headerHash: input.commitment.header_hash,
      trancheIndex,
      startOffset,
      byteLength: Number(descriptor.byte_length),
    });
    let previousAccumulator = initialAccumulator;
    const publications: DaAvailabilityPublicationDatum[] = [];
    let chunkIndex = 0;
    for (
      let chunkOffset = startOffset;
      chunkOffset < endOffset;
      chunkOffset += chunkByteLength
    ) {
      const chunkEnd = Math.min(chunkOffset + chunkByteLength, endOffset);
      const chunk = input.payload.subarray(chunkOffset, chunkEnd);
      const chunkHash = toHex(blake2b(chunk, { dkLen: 32 }));
      const nextAccumulator = daAvailabilityTrancheStepAccumulator({
        deploymentIdentity: input.commitment.deployment_identity,
        headerHash: input.commitment.header_hash,
        trancheIndex,
        chunkOffset,
        chunk,
        previousAccumulator,
      });
      const membership = membershipIndex.membershipAt(chunkIndex);
      publications.push({
        deployment_identity: input.commitment.deployment_identity,
        header_hash: input.commitment.header_hash,
        challenge_asset_name: input.challengeAssetName,
        tranche_index: BigInt(trancheIndex),
        chunk_index: BigInt(chunkIndex),
        chunk_offset: BigInt(chunkOffset),
        chunk_byte_length: BigInt(chunk.length),
        chunk_hash: chunkHash,
        chunk_frontier: membership.frontier.peaks.map((peak) => ({
          height: BigInt(peak.height),
          hash: toHex(peak.hash),
        })),
        chunk_siblings: membership.siblings.map((sibling) => toHex(sibling)),
        previous_accumulator: previousAccumulator,
        next_accumulator: nextAccumulator,
        chunk: toHex(chunk),
      });
      previousAccumulator = nextAccumulator;
      chunkIndex += 1;
    }
    if (previousAccumulator !== descriptor.terminal_accumulator) {
      throw new DaAvailabilityCommitmentError(
        `tranche ${trancheIndex.toString()} does not reach its signed terminal accumulator`,
      );
    }
    if (
      tier === "complete_item_inline" &&
      (input.commitment.tranche_descriptors.length !== 1 ||
        publications.length !== 1 ||
        publications[0]!.chunk_byte_length !==
          input.commitment.payload_byte_length)
    ) {
      throw new DaAvailabilityCommitmentError(
        "a complete fitting availability item must use exactly one inline publication",
      );
    }
    return { descriptor, initialAccumulator, publications };
  });
};

/** Off-chain twin of the deadline-bound in-tranche validator transition. */
export const advanceDaAvailabilityTranche = (input: {
  readonly active: DaAvailabilityTrancheDatum;
  readonly publication: DaAvailabilityPublicationDatum;
  readonly responseGeometry: DaAvailabilityResponseGeometry;
  readonly inclusiveValidityUpper: bigint;
  readonly carrierOutputIndex: bigint;
}): DaAvailabilityTrancheDatum => {
  assertCanonicalDaAvailabilityResponseGeometry(input.responseGeometry);
  if (typeof input.active !== "object" || !("Active" in input.active)) {
    throw new DaAvailabilityCommitmentError(
      "a terminal receipt cannot accept another publication",
    );
  }
  const active = input.active.Active;
  if (input.carrierOutputIndex < 0n) {
    throw new DaAvailabilityCommitmentError(
      "publication carrier output index must be non-negative",
    );
  }
  if (input.inclusiveValidityUpper > active.response_deadline) {
    throw new DaAvailabilityCommitmentError(
      "availability publication validity upper exceeds the response deadline",
    );
  }
  const descriptor = active.descriptor;
  assertCanonicalDaAvailabilityPublicationDatum(
    input.publication,
    input.responseGeometry,
    descriptor,
  );
  const endOffset = descriptor.start_offset + descriptor.byte_length;
  const remaining = endOffset - active.next_offset;
  const expectedChunkLength =
    remaining < input.responseGeometry.chunk_byte_length
      ? remaining
      : input.responseGeometry.chunk_byte_length;
  const chunk = fromHex(input.publication.chunk);
  const nextAccumulator = daAvailabilityTrancheStepAccumulator({
    deploymentIdentity: active.deployment_identity,
    headerHash: active.header_hash,
    trancheIndex: Number(descriptor.tranche_index),
    chunkOffset: Number(active.next_offset),
    chunk,
    previousAccumulator: active.accumulator,
  });
  if (
    expectedChunkLength <= 0n ||
    input.publication.deployment_identity !== active.deployment_identity ||
    input.publication.header_hash !== active.header_hash ||
    input.publication.challenge_asset_name !== active.challenge_asset_name ||
    input.publication.tranche_index !== descriptor.tranche_index ||
    input.publication.chunk_index !==
      (active.next_offset - descriptor.start_offset) /
        input.responseGeometry.chunk_byte_length ||
    input.publication.chunk_offset !== active.next_offset ||
    input.publication.chunk_byte_length !== expectedChunkLength ||
    BigInt(chunk.length) !== expectedChunkLength ||
    input.publication.chunk_hash !== toHex(blake2b(chunk, { dkLen: 32 })) ||
    input.publication.previous_accumulator !== active.accumulator ||
    input.publication.next_accumulator !== nextAccumulator
  ) {
    throw new DaAvailabilityCommitmentError(
      "availability publication does not exactly advance the authenticated tranche",
    );
  }
  const nextOffset = active.next_offset + expectedChunkLength;
  if (nextOffset === endOffset) {
    if (nextAccumulator !== descriptor.terminal_accumulator) {
      throw new DaAvailabilityCommitmentError(
        "terminal publication does not equal the signed tranche accumulator",
      );
    }
    return {
      Receipt: {
        deployment_identity: active.deployment_identity,
        header_hash: active.header_hash,
        challenge_asset_name: active.challenge_asset_name,
        descriptor,
        terminal_accumulator: nextAccumulator,
        terminal_carrier_output_index: input.carrierOutputIndex,
        challenger: active.challenger,
      },
    };
  }
  return {
    Active: {
      ...active,
      next_offset: nextOffset,
      accumulator: nextAccumulator,
      latest_carrier_output_index: input.carrierOutputIndex,
    },
  };
};

/**
 * Exact close gate: one compact receipt per signed descriptor, in descriptor
 * order, with no duplicate, foreign, or merely shape-compatible receipt.
 */
export const assertDaAvailabilityTerminalReceipts = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly challengeAssetName: string;
  readonly challenger: string;
  readonly receipts: readonly DaAvailabilityTrancheDatum[];
}): string => {
  assertCanonicalDaAvailabilityCommitment(input.commitment);
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentError(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(input.challenger, 28, "challenger");
  if (input.receipts.length !== input.commitment.tranche_descriptors.length) {
    throw new DaAvailabilityCommitmentError(
      "terminal receipt count must equal the signed descriptor count",
    );
  }
  for (const [
    index,
    descriptor,
  ] of input.commitment.tranche_descriptors.entries()) {
    const datum = input.receipts[index];
    if (
      datum === undefined ||
      typeof datum !== "object" ||
      !("Receipt" in datum)
    ) {
      throw new DaAvailabilityCommitmentError(
        `terminal receipt ${index.toString()} is missing or still active`,
      );
    }
    const receipt = datum.Receipt;
    if (
      receipt.deployment_identity !== input.commitment.deployment_identity ||
      receipt.header_hash !== input.commitment.header_hash ||
      receipt.challenge_asset_name !== input.challengeAssetName ||
      receipt.challenger !== input.challenger ||
      Data.to(
        receipt.descriptor as never,
        DaAvailabilityTrancheDescriptorSchema as never,
      ) !==
        Data.to(
          descriptor as never,
          DaAvailabilityTrancheDescriptorSchema as never,
        ) ||
      receipt.terminal_accumulator !== descriptor.terminal_accumulator
    ) {
      throw new DaAvailabilityCommitmentError(
        `terminal receipt ${index.toString()} does not equal its signed descriptor`,
      );
    }
  }
  return daAvailabilityPublishedTerminalCommitment(input.commitment);
};

export type DaAvailabilityPublicationObservation = Readonly<{
  publication: DaAvailabilityPublicationDatum;
  inclusiveValidityUpper: bigint;
  /** Exact output index of this publication's carrier in its admitted L1 tx. */
  carrierOutputIndex: bigint;
}>;

export type DaAvailabilityTrancheEvidence = Readonly<{
  descriptor: DaAvailabilityTrancheDescriptor;
  publications: readonly DaAvailabilityPublicationObservation[];
}>;

export type DaAvailabilityChallengedBondEvidence = Readonly<{
  /** Exact inline datum read from the challenged retained-bond output. */
  datumCborHex: string;
  /** Available-bond input consumed by the challenge transaction; derives DACH. */
  bondInputOutRef: OutputReference;
  /** Challenged-bond output carrying the datum and retained DABN identity. */
  challengedBondOutputOutRef: OutputReference;
}>;

type DaAvailabilityChallengedBondFields = Extract<
  DaAvailabilityBondDatum,
  { ChallengedBond: unknown }
>["ChallengedBond"];

const assertCanonicalDaAvailabilityEvidenceOutRef = (
  outRef: OutputReference,
  field: string,
): void => {
  if (
    typeof outRef !== "object" ||
    outRef === null ||
    Object.getPrototypeOf(outRef) !== Object.prototype ||
    Reflect.ownKeys(outRef).length !== 2 ||
    !Reflect.has(outRef, "transactionId") ||
    !Reflect.has(outRef, "outputIndex") ||
    !HASH_32.test(outRef.transactionId) ||
    outRef.outputIndex < 0n ||
    outRef.outputIndex > 65_535n
  ) {
    throw new DaAvailabilityCommitmentError(
      `${field} must be a canonical bounded Cardano output reference`,
    );
  }
};

const challengedBondFieldsFromEvidence = (
  evidence: DaAvailabilityChallengedBondEvidence,
  parameters: DaAvailabilityParameters,
): DaAvailabilityChallengedBondFields => {
  assertCanonicalDaAvailabilityParameters(parameters);
  if (
    typeof evidence !== "object" ||
    evidence === null ||
    Object.getPrototypeOf(evidence) !== Object.prototype ||
    Reflect.ownKeys(evidence).length !== 3 ||
    !Reflect.has(evidence, "datumCborHex") ||
    !Reflect.has(evidence, "bondInputOutRef") ||
    !Reflect.has(evidence, "challengedBondOutputOutRef")
  ) {
    throw new DaAvailabilityCommitmentError(
      "challengedBond evidence must contain exactly datum and input/output identities",
    );
  }
  assertCanonicalDaAvailabilityEvidenceOutRef(
    evidence.bondInputOutRef,
    "challengedBond.bondInputOutRef",
  );
  assertCanonicalDaAvailabilityEvidenceOutRef(
    evidence.challengedBondOutputOutRef,
    "challengedBond.challengedBondOutputOutRef",
  );
  if (
    evidence.bondInputOutRef.transactionId ===
      evidence.challengedBondOutputOutRef.transactionId &&
    evidence.bondInputOutRef.outputIndex ===
      evidence.challengedBondOutputOutRef.outputIndex
  ) {
    throw new DaAvailabilityCommitmentError(
      "challenged bond output cannot equal its consumed available-bond input",
    );
  }
  const bondDatum = parseDaAvailabilityBondDatumCbor(
    evidence.datumCborHex,
    parameters,
  );
  if (!("ChallengedBond" in bondDatum)) {
    throw new DaAvailabilityCommitmentError(
      "public evidence reconstruction requires the authenticated challenged-bond datum",
    );
  }
  const challenged = bondDatum.ChallengedBond;
  const expectedChallengeAssetName = daAvailabilityChallengeAssetName(
    evidence.bondInputOutRef,
  );
  if (challenged.challenge_asset_name !== expectedChallengeAssetName) {
    throw new DaAvailabilityCommitmentError(
      "challenged-bond datum does not carry the DACH identity derived from its consumed bond input",
    );
  }
  return challenged;
};

/**
 * Response planner whose identity, commitment and deadline originate in the
 * exact challenged-bond datum. Production callers must obtain the evidence
 * from the admitted raw-L1 challenge transaction.
 */
export const planDaAvailabilityPublicationsFromChallengedBond = (input: {
  readonly challengedBond: DaAvailabilityChallengedBondEvidence;
  readonly parameters: DaAvailabilityParameters;
  readonly payload: Uint8Array;
}): readonly DaAvailabilityTranchePublicationPlan[] => {
  const challenged = challengedBondFieldsFromEvidence(
    input.challengedBond,
    input.parameters,
  );
  return planDaAvailabilityPublications({
    commitment: challenged.commitment,
    payload: input.payload,
    challengeAssetName: challenged.challenge_asset_name,
  });
};

/**
 * Public-evidence reconstruction from authenticated L1 publication history.
 * The caller supplies chain-ordered observations; this verifier never sorts or
 * repairs them, so a missing/reordered/replayed chunk fails closed.
 */
export const reconstructDaAvailabilityPayload = (input: {
  readonly challengedBond: DaAvailabilityChallengedBondEvidence;
  readonly parameters: DaAvailabilityParameters;
  readonly tranches: readonly DaAvailabilityTrancheEvidence[];
}): Uint8Array => {
  const challenged = challengedBondFieldsFromEvidence(
    input.challengedBond,
    input.parameters,
  );
  const commitment = challenged.commitment;
  if (input.tranches.length !== commitment.tranche_descriptors.length) {
    throw new DaAvailabilityCommitmentError(
      "public evidence tranche count does not equal the signed descriptor count",
    );
  }
  const payloadParts: Uint8Array[] = [];
  for (const [index, descriptor] of commitment.tranche_descriptors.entries()) {
    const evidence = input.tranches[index];
    if (
      evidence === undefined ||
      Data.to(
        evidence.descriptor as never,
        DaAvailabilityTrancheDescriptorSchema as never,
      ) !==
        Data.to(
          descriptor as never,
          DaAvailabilityTrancheDescriptorSchema as never,
        )
    ) {
      throw new DaAvailabilityCommitmentError(
        `public evidence tranche ${index.toString()} is missing or reordered`,
      );
    }
    let state: DaAvailabilityTrancheDatum = {
      Active: {
        deployment_identity: commitment.deployment_identity,
        header_hash: commitment.header_hash,
        challenge_asset_name: challenged.challenge_asset_name,
        descriptor,
        next_offset: descriptor.start_offset,
        accumulator: daAvailabilityTrancheStartAccumulator({
          deploymentIdentity: commitment.deployment_identity,
          headerHash: commitment.header_hash,
          trancheIndex: Number(descriptor.tranche_index),
          startOffset: Number(descriptor.start_offset),
          byteLength: Number(descriptor.byte_length),
        }),
        latest_carrier_output_index: null,
        response_deadline: challenged.response_deadline,
        challenger: challenged.challenger,
      },
    };
    for (const observation of evidence.publications) {
      state = advanceDaAvailabilityTranche({
        active: state,
        publication: observation.publication,
        responseGeometry: commitment.response_geometry,
        inclusiveValidityUpper: observation.inclusiveValidityUpper,
        carrierOutputIndex: observation.carrierOutputIndex,
      });
      payloadParts.push(fromHex(observation.publication.chunk));
    }
    if (typeof state !== "object" || !("Receipt" in state)) {
      throw new DaAvailabilityCommitmentError(
        `public evidence tranche ${index.toString()} is incomplete`,
      );
    }
  }
  const payload = Uint8Array.from(
    Buffer.concat(payloadParts.map((part) => Buffer.from(part))),
  );
  if (
    !verifyDaAvailabilityPayloadCommitment({
      commitment,
      payload,
    })
  ) {
    throw new DaAvailabilityCommitmentError(
      "reconstructed public evidence does not equal the signed payload commitment",
    );
  }
  return payload;
};
