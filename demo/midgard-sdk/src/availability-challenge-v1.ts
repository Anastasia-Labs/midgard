import {
  buildMidgardValidationMerkleMembershipIndexV1,
  commitMidgardValidationMerkleFrontierV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES_V1,
  MIDGARD_MAX_DA_PAYLOAD_BYTES_V1,
  verifyMidgardValidationMerkleMembershipV1,
} from "@al-ft/midgard-core";
import { Data, fromHex, toHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { type OutputReference, OutputReferenceSchema } from "./common.js";
import { FrontierPeakV1Schema } from "./fraud-proof/validation-auxiliary-witness-v1.js";
import { HeaderHashSchema } from "./ledger-state.js";
export {
  daAvailabilityStateQueueStatusPermitsMergeV1,
  DaAvailabilityStateQueueStatusV1,
  DaAvailabilityStateQueueStatusV1Schema,
} from "./da-availability-state-v1.js";

export const DA_AVAILABILITY_COMMITMENT_V1_VERSION = 1n;

export const DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES_V1 = 64 * 1024;
export const DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES_V1 =
  MIDGARD_MAX_DA_PAYLOAD_BYTES_V1;

export const DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS_V1 = 60 * 60 * 1_000;
export const DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS_V1 = 48 * 60 * 60 * 1_000;

export const DA_AVAILABILITY_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1 =
  10_000_000_000n;
export const DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1 =
  10_000_000_000n;

/**
 * The first response-publication measurement candidate. It is deliberately
 * named as a candidate: Q58 may promote it to the compiled response chunk
 * bound only after a signed, reference-script-backed testnet-profile
 * transaction retains the protocol's 512-byte reliability reserve.
 */
export const DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE_V1 =
  Object.freeze({
    // Exact signed reference-script transaction frontier: 15,872 bytes,
    // retaining the required 512-byte maxTxSize reserve. 14,021 serializes to
    // 15,873 and is therefore rejected by the adjacent measurement.
    chunkByteLength: 14_020,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  });

/** Absolute safety ceilings, not an activated response geometry. */
export const DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES_V1 =
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES_V1;
export const DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1 =
  MIDGARD_CONSENSUS_LIMITS_V1.maxOutputCount;

const HASH_28 = /^[0-9a-f]{56}$/u;
const HASH_32 = /^[0-9a-f]{64}$/u;
const CANONICAL_CBOR_HEX = /^(?:[0-9a-f]{2})+$/u;

const requireHash = (value: string, width: 28 | 32, field: string): void => {
  const pattern = width === 28 ? HASH_28 : HASH_32;
  if (!pattern.test(value)) {
    throw new DaAvailabilityCommitmentV1Error(
      `${field} must be exactly ${width.toString()} lowercase hex bytes`,
    );
  }
};

const requireSafePositiveInteger = (value: number, field: string): void => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new DaAvailabilityCommitmentV1Error(
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
    throw new DaAvailabilityCommitmentV1Error(
      `${input.name} must be non-empty lowercase CBOR hex`,
    );
  }
  let decoded: Value;
  try {
    decoded = Data.from(input.cborHex, input.schema as never) as Value;
  } catch (error) {
    throw new DaAvailabilityCommitmentV1Error(
      `${input.name} is not valid V1 Plutus Data: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  if (Data.to(decoded as never, input.schema as never) !== input.cborHex) {
    throw new DaAvailabilityCommitmentV1Error(
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

export const daAvailabilityBondAssetNameV1 = (
  attestationInputOutRef: OutputReference,
): string =>
  `${DA_AVAILABILITY_BOND_ASSET_NAME_PREFIX}${outRefIdentity28(attestationInputOutRef)}`;

export const daAvailabilityChallengeAssetNameV1 = (
  bondInputOutRef: OutputReference,
): string =>
  `${DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX}${outRefIdentity28(bondInputOutRef)}`;

export const daAvailabilityTrancheAssetNameV1 = (input: {
  readonly challengeAssetName: string;
  readonly trancheIndex: number;
}): string => {
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentV1Error(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  if (
    !Number.isSafeInteger(input.trancheIndex) ||
    input.trancheIndex < 0 ||
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1
  ) {
    throw new DaAvailabilityCommitmentV1Error(
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

export const daAvailabilityTerminalAccumulatorAssetNameV1 = (
  challengeAssetName: string,
): string => {
  if (!CHALLENGE_ASSET_NAME.test(challengeAssetName)) {
    throw new DaAvailabilityCommitmentV1Error(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  return `${DA_AVAILABILITY_TERMINAL_ACCUMULATOR_ASSET_NAME_PREFIX}${challengeAssetName.slice(
    DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX.length,
  )}`;
};

export const DaAvailabilityTrancheDescriptorV1Schema = Data.Object({
  tranche_index: Data.Integer(),
  start_offset: Data.Integer(),
  byte_length: Data.Integer(),
  chunk_count: Data.Integer(),
  chunk_commitment: Data.Bytes({ minLength: 32, maxLength: 32 }),
  terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export type DaAvailabilityTrancheDescriptorV1 = Data.Static<
  typeof DaAvailabilityTrancheDescriptorV1Schema
>;
export const DaAvailabilityTrancheDescriptorV1 =
  DaAvailabilityTrancheDescriptorV1Schema as unknown as DaAvailabilityTrancheDescriptorV1;

/**
 * Release-bound response geometry. The applied response-transaction
 * measurement selects these values in authenticated deployment/DA parameters;
 * the wire schema does not turn the first 4 MiB/4 KiB sizing probe into
 * protocol law.
 */
export const DaAvailabilityResponseGeometryV1Schema = Data.Object({
  chunk_byte_length: Data.Integer(),
  tranche_byte_length: Data.Integer(),
  max_tranche_count: Data.Integer(),
});
export type DaAvailabilityResponseGeometryV1 = Data.Static<
  typeof DaAvailabilityResponseGeometryV1Schema
>;
export const DaAvailabilityResponseGeometryV1 =
  DaAvailabilityResponseGeometryV1Schema as unknown as DaAvailabilityResponseGeometryV1;

/**
 * Authenticated release/DA parameters selected after applied response-cost
 * measurement. The two bonds remain matching, but their activated lovelace
 * amount is deployment data rather than a wire-level constant.
 */
export const DaAvailabilityParametersV1Schema = Data.Object({
  response_geometry: DaAvailabilityResponseGeometryV1Schema,
  da_bond_lovelace: Data.Integer(),
  challenger_bond_lovelace: Data.Integer(),
  max_open_fee_lovelace: Data.Integer(),
  max_publication_fee_lovelace: Data.Integer(),
  max_settlement_fee_lovelace: Data.Integer(),
  max_close_fee_lovelace: Data.Integer(),
  max_timeout_fee_lovelace: Data.Integer(),
});
export type DaAvailabilityParametersV1 = Data.Static<
  typeof DaAvailabilityParametersV1Schema
>;
export const DaAvailabilityParametersV1 =
  DaAvailabilityParametersV1Schema as unknown as DaAvailabilityParametersV1;

export const DaAvailabilityCommitmentV1Schema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  payload_byte_length: Data.Integer(),
  response_geometry: DaAvailabilityResponseGeometryV1Schema,
  tranche_descriptors: Data.Array(DaAvailabilityTrancheDescriptorV1Schema),
  bond_owner: Data.Bytes({ minLength: 28, maxLength: 28 }),
});
export type DaAvailabilityCommitmentV1 = Data.Static<
  typeof DaAvailabilityCommitmentV1Schema
>;
export const DaAvailabilityCommitmentV1 =
  DaAvailabilityCommitmentV1Schema as unknown as DaAvailabilityCommitmentV1;

const DaAvailabilityTrancheStartV1Schema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  tranche_index: Data.Integer(),
  start_offset: Data.Integer(),
  byte_length: Data.Integer(),
});

const DaAvailabilityTrancheStepV1Schema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  tranche_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

const DaAvailabilityTerminalAccumulatorStartV1Schema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

const DaAvailabilityChunkLeafV1Schema = Data.Object({
  version: Data.Integer(),
  tranche_index: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

export const DaAvailabilityBondDatumV1Schema = Data.Enum([
  Data.Object({
    Available: Data.Object({
      commitment: DaAvailabilityCommitmentV1Schema,
      da_bond_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
      attested_signers: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Object({
    ChallengedBond: Data.Object({
      commitment: DaAvailabilityCommitmentV1Schema,
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
export type DaAvailabilityBondDatumV1 = Data.Static<
  typeof DaAvailabilityBondDatumV1Schema
>;
export const DaAvailabilityBondDatumV1 =
  DaAvailabilityBondDatumV1Schema as unknown as DaAvailabilityBondDatumV1;

export const DaAvailabilityTrancheDatumV1Schema = Data.Enum([
  Data.Object({
    Active: Data.Object({
      deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
      header_hash: HeaderHashSchema,
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      descriptor: DaAvailabilityTrancheDescriptorV1Schema,
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
      descriptor: DaAvailabilityTrancheDescriptorV1Schema,
      terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
      terminal_carrier_output_index: Data.Integer(),
      challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
    }),
  }),
]);
export type DaAvailabilityTrancheDatumV1 = Data.Static<
  typeof DaAvailabilityTrancheDatumV1Schema
>;
export const DaAvailabilityTrancheDatumV1 =
  DaAvailabilityTrancheDatumV1Schema as unknown as DaAvailabilityTrancheDatumV1;

export const DaAvailabilityTrancheTerminalStatusV1Schema = Data.Enum([
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
export type DaAvailabilityTrancheTerminalStatusV1 = Data.Static<
  typeof DaAvailabilityTrancheTerminalStatusV1Schema
>;
export const DaAvailabilityTrancheTerminalStatusV1 =
  DaAvailabilityTrancheTerminalStatusV1Schema as unknown as DaAvailabilityTrancheTerminalStatusV1;

const DaAvailabilityTerminalAccumulatorStepV1Schema = Data.Object({
  version: Data.Integer(),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  tranche_index: Data.Integer(),
  status: DaAvailabilityTrancheTerminalStatusV1Schema,
});

export const DaAvailabilityTerminalAccumulatorDatumV1Schema = Data.Object({
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
export type DaAvailabilityTerminalAccumulatorDatumV1 = Data.Static<
  typeof DaAvailabilityTerminalAccumulatorDatumV1Schema
>;
export const DaAvailabilityTerminalAccumulatorDatumV1 =
  DaAvailabilityTerminalAccumulatorDatumV1Schema as unknown as DaAvailabilityTerminalAccumulatorDatumV1;

export const DaAvailabilityPublicationDatumV1Schema = Data.Object({
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
  tranche_index: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  chunk_frontier: Data.Array(FrontierPeakV1Schema),
  chunk_siblings: Data.Array(Data.Bytes({ minLength: 32, maxLength: 32 })),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  next_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  chunk: Data.Bytes(),
});
export type DaAvailabilityPublicationDatumV1 = Data.Static<
  typeof DaAvailabilityPublicationDatumV1Schema
>;
export const DaAvailabilityPublicationDatumV1 =
  DaAvailabilityPublicationDatumV1Schema as unknown as DaAvailabilityPublicationDatumV1;

/** Exact minting-policy ABI for the retained DA bond/challenge lifecycle. */
export const DaAvailabilityMintRedeemerV1Schema = Data.Enum([
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
export type DaAvailabilityMintRedeemerV1 = Data.Static<
  typeof DaAvailabilityMintRedeemerV1Schema
>;
export const DaAvailabilityMintRedeemerV1 =
  DaAvailabilityMintRedeemerV1Schema as unknown as DaAvailabilityMintRedeemerV1;

/** Exact spending-validator ABI for bond, tranche and carrier UTxOs. */
export const DaAvailabilitySpendRedeemerV1Schema = Data.Enum([
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
export type DaAvailabilitySpendRedeemerV1 = Data.Static<
  typeof DaAvailabilitySpendRedeemerV1Schema
>;
export const DaAvailabilitySpendRedeemerV1 =
  DaAvailabilitySpendRedeemerV1Schema as unknown as DaAvailabilitySpendRedeemerV1;

export class DaAvailabilityCommitmentV1Error extends Error {
  constructor(message: string) {
    super(message);
    this.name = "DaAvailabilityCommitmentV1Error";
  }
}

export const daAvailabilityResponseWindowMsV1 = (
  payloadByteLength: number,
): number => {
  requireSafePositiveInteger(payloadByteLength, "payloadByteLength");
  if (payloadByteLength > DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES_V1) {
    throw new DaAvailabilityCommitmentV1Error(
      "payloadByteLength exceeds the canonical 64 MiB V1 DA limit",
    );
  }
  return payloadByteLength <= DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES_V1
    ? DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS_V1
    : DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS_V1;
};

export const daAvailabilityResponseDeadlineV1 = (input: {
  readonly payloadByteLength: number;
  readonly openedAt: bigint;
}): bigint => {
  if (input.openedAt < 0n) {
    throw new DaAvailabilityCommitmentV1Error(
      "availability challenge openedAt must be non-negative",
    );
  }
  return (
    input.openedAt +
    BigInt(daAvailabilityResponseWindowMsV1(input.payloadByteLength))
  );
};

export type DaAvailabilityTrancheLayoutV1 = Readonly<{
  trancheIndex: number;
  startOffset: number;
  byteLength: number;
}>;

export const assertCanonicalDaAvailabilityResponseGeometryV1 = (
  geometry: DaAvailabilityResponseGeometryV1,
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
    throw new DaAvailabilityCommitmentV1Error(
      "response geometry must fit canonical safe integers",
    );
  }
  if (chunkByteLength > DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES_V1) {
    throw new DaAvailabilityCommitmentV1Error(
      "response chunk exceeds the L1 reliable-publication safety ceiling",
    );
  }
  if (
    trancheByteLength < DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES_V1 ||
    trancheByteLength > DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES_V1
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "response tranche must cover the complete small class and stay within the 64 MiB payload ceiling",
    );
  }
  if (
    maxTrancheCount > DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1 ||
    Math.ceil(DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES_V1 / trancheByteLength) >
      maxTrancheCount
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "response geometry cannot cover the 64 MiB class within its authenticated tranche-count bound",
    );
  }
};

export const availabilityResponseGeometryV1 = (input: {
  readonly chunkByteLength: number;
  readonly trancheByteLength: number;
  readonly maxTrancheCount: number;
}): DaAvailabilityResponseGeometryV1 => {
  const geometry = {
    chunk_byte_length: BigInt(input.chunkByteLength),
    tranche_byte_length: BigInt(input.trancheByteLength),
    max_tranche_count: BigInt(input.maxTrancheCount),
  };
  assertCanonicalDaAvailabilityResponseGeometryV1(geometry);
  return geometry;
};

export const assertCanonicalDaAvailabilityParametersV1 = (
  parameters: DaAvailabilityParametersV1,
): void => {
  assertCanonicalDaAvailabilityResponseGeometryV1(parameters.response_geometry);
  if (
    parameters.da_bond_lovelace <= 0n ||
    parameters.challenger_bond_lovelace !== parameters.da_bond_lovelace
  ) {
    throw new DaAvailabilityCommitmentV1Error(
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
    throw new DaAvailabilityCommitmentV1Error(
      "availability release fee ceilings must be positive measured values",
    );
  }
  const maximumPublicationCount = BigInt(
    maximumDaAvailabilityPublicationCountV1(parameters.response_geometry),
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
    throw new DaAvailabilityCommitmentV1Error(
      "challenger bond must cover every maximum-size publication fee plus the larger terminal fee ceiling",
    );
  }
};

export const daAvailabilityParametersV1 = (input: {
  readonly responseGeometry: DaAvailabilityResponseGeometryV1;
  readonly daBondLovelace: bigint;
  readonly challengerBondLovelace: bigint;
  readonly maxOpenFeeLovelace: bigint;
  readonly maxPublicationFeeLovelace: bigint;
  readonly maxSettlementFeeLovelace: bigint;
  readonly maxCloseFeeLovelace: bigint;
  readonly maxTimeoutFeeLovelace: bigint;
}): DaAvailabilityParametersV1 => {
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
  assertCanonicalDaAvailabilityParametersV1(parameters);
  return parameters;
};

export const encodeDaAvailabilityParametersV1 = (
  parameters: DaAvailabilityParametersV1,
): string => {
  assertCanonicalDaAvailabilityParametersV1(parameters);
  return Data.to(
    parameters as never,
    DaAvailabilityParametersV1Schema as never,
  );
};

/**
 * Strict durable/configuration codec. Shape-compatible or non-canonical CBOR
 * is never accepted as authenticated release parameters.
 */
export const parseDaAvailabilityParametersV1Cbor = (
  cborHex: string,
): DaAvailabilityParametersV1 => {
  const parameters = parseCanonicalDataCbor<
    typeof DaAvailabilityParametersV1Schema,
    DaAvailabilityParametersV1
  >({
    cborHex,
    schema: DaAvailabilityParametersV1Schema,
    name: "availability parameters",
  });
  assertCanonicalDaAvailabilityParametersV1(parameters);
  return parameters;
};

/**
 * Deterministic minimal tranche partition under the authenticated measured
 * geometry. Its tranche width/count are release data, while the exact 64 KiB
 * and 64 MiB response classes stay protocol-fixed.
 */
export const deriveDaAvailabilityTrancheLayoutV1 = (
  payloadByteLength: number,
  responseGeometry: DaAvailabilityResponseGeometryV1,
): readonly DaAvailabilityTrancheLayoutV1[] => {
  daAvailabilityResponseWindowMsV1(payloadByteLength);
  assertCanonicalDaAvailabilityResponseGeometryV1(responseGeometry);
  const trancheByteLength = Number(responseGeometry.tranche_byte_length);
  const trancheCount = Math.ceil(payloadByteLength / trancheByteLength);
  if (trancheCount > Number(responseGeometry.max_tranche_count)) {
    throw new DaAvailabilityCommitmentV1Error(
      "payload requires more than the authenticated response geometry's tranche bound",
    );
  }
  const result: DaAvailabilityTrancheLayoutV1[] = [];
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

export const maximumDaAvailabilityPublicationCountV1 = (
  responseGeometry: DaAvailabilityResponseGeometryV1,
): number => {
  assertCanonicalDaAvailabilityResponseGeometryV1(responseGeometry);
  const chunkByteLength = Number(responseGeometry.chunk_byte_length);
  return deriveDaAvailabilityTrancheLayoutV1(
    DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES_V1,
    responseGeometry,
  ).reduce(
    (total, tranche) => total + Math.ceil(tranche.byteLength / chunkByteLength),
    0,
  );
};

export const daAvailabilityTrancheStartAccumulatorV1 = (input: {
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
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1 ||
    !Number.isSafeInteger(input.startOffset) ||
    input.startOffset < 0
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "tranche index and start offset must be canonical non-negative integers",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_V1_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      tranche_index: BigInt(input.trancheIndex),
      start_offset: BigInt(input.startOffset),
      byte_length: BigInt(input.byteLength),
    } as never,
    DaAvailabilityTrancheStartV1Schema as never,
  );
  return hashDomainAndData(TRANCHE_START_DOMAIN, cbor);
};

export const daAvailabilityTerminalAccumulatorStartV1 = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly challengeAssetName: string;
}): string => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentV1Error(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_V1_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      challenge_asset_name: input.challengeAssetName,
    } as never,
    DaAvailabilityTerminalAccumulatorStartV1Schema as never,
  );
  return hashDomainAndData(TERMINAL_ACCUMULATOR_START_DOMAIN, cbor);
};

/** Cross-language twin of the bounded per-tranche terminal fold. */
export const foldDaAvailabilityTerminalAccumulatorV1 = (input: {
  readonly previousAccumulator: string;
  readonly trancheIndex: number;
  readonly status: DaAvailabilityTrancheTerminalStatusV1;
}): string => {
  requireHash(input.previousAccumulator, 32, "previousAccumulator");
  if (
    !Number.isSafeInteger(input.trancheIndex) ||
    input.trancheIndex < 0 ||
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1
  ) {
    throw new DaAvailabilityCommitmentV1Error(
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
      throw new DaAvailabilityCommitmentV1Error(
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
      version: DA_AVAILABILITY_COMMITMENT_V1_VERSION,
      previous_accumulator: input.previousAccumulator,
      tranche_index: BigInt(input.trancheIndex),
      status: input.status,
    } as never,
    DaAvailabilityTerminalAccumulatorStepV1Schema as never,
  );
  return hashDomainAndData(TERMINAL_ACCUMULATOR_STEP_DOMAIN, cbor);
};

export const daAvailabilityTrancheStepAccumulatorV1 = (input: {
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
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1 ||
    !Number.isSafeInteger(input.chunkOffset) ||
    input.chunkOffset < 0
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "tranche index and chunk offset must be canonical non-negative integers",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_V1_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      tranche_index: BigInt(input.trancheIndex),
      chunk_offset: BigInt(input.chunkOffset),
      chunk_byte_length: BigInt(input.chunk.length),
      chunk_hash: toHex(blake2b(input.chunk, { dkLen: 32 })),
      previous_accumulator: input.previousAccumulator,
    } as never,
    DaAvailabilityTrancheStepV1Schema as never,
  );
  return hashDomainAndData(TRANCHE_STEP_DOMAIN, cbor);
};

export const daAvailabilityChunkLeafHashV1 = (input: {
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
      throw new DaAvailabilityCommitmentV1Error(
        `${field} must be a canonical non-negative safe integer`,
      );
    }
  }
  requireSafePositiveInteger(input.chunkByteLength, "chunkByteLength");
  requireHash(input.chunkHash, 32, "chunkHash");
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_V1_VERSION,
      tranche_index: BigInt(input.trancheIndex),
      chunk_index: BigInt(input.chunkIndex),
      chunk_offset: BigInt(input.chunkOffset),
      chunk_byte_length: BigInt(input.chunkByteLength),
      chunk_hash: input.chunkHash,
    } as never,
    DaAvailabilityChunkLeafV1Schema as never,
  );
  return hashDomainAndData(CHUNK_LEAF_DOMAIN, cbor);
};

const trancheChunkLeafHashes = (input: {
  readonly layout: DaAvailabilityTrancheLayoutV1;
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
        daAvailabilityChunkLeafHashV1({
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
  readonly layout: DaAvailabilityTrancheLayoutV1;
  readonly payload: Uint8Array;
  readonly chunkByteLength: number;
}): string => {
  let accumulator = daAvailabilityTrancheStartAccumulatorV1({
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
    accumulator = daAvailabilityTrancheStepAccumulatorV1({
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

export const buildDaAvailabilityCommitmentV1 = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly payload: Uint8Array;
  readonly bondOwner: string;
  readonly responseGeometry: DaAvailabilityResponseGeometryV1;
}): DaAvailabilityCommitmentV1 => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  requireHash(input.bondOwner, 28, "bondOwner");
  assertCanonicalDaAvailabilityResponseGeometryV1(input.responseGeometry);
  const chunkByteLength = Number(input.responseGeometry.chunk_byte_length);
  const layout = deriveDaAvailabilityTrancheLayoutV1(
    input.payload.length,
    input.responseGeometry,
  );
  const trancheDescriptors = layout.map(
    (entry): DaAvailabilityTrancheDescriptorV1 => {
      const leaves = trancheChunkLeafHashes({
        layout: entry,
        payload: input.payload,
        chunkByteLength,
      });
      const membership = buildMidgardValidationMerkleMembershipIndexV1(leaves);
      return {
        tranche_index: BigInt(entry.trancheIndex),
        start_offset: BigInt(entry.startOffset),
        byte_length: BigInt(entry.byteLength),
        chunk_count: BigInt(leaves.length),
        chunk_commitment: toHex(
          commitMidgardValidationMerkleFrontierV1(membership.frontier),
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
    version: DA_AVAILABILITY_COMMITMENT_V1_VERSION,
    deployment_identity: input.deploymentIdentity,
    header_hash: input.headerHash,
    payload_byte_length: BigInt(input.payload.length),
    response_geometry: input.responseGeometry,
    tranche_descriptors: trancheDescriptors,
    bond_owner: input.bondOwner,
  };
};

export const assertCanonicalDaAvailabilityCommitmentV1 = (
  commitment: DaAvailabilityCommitmentV1,
  expectedResponseGeometry?: DaAvailabilityResponseGeometryV1,
): void => {
  if (commitment.version !== DA_AVAILABILITY_COMMITMENT_V1_VERSION) {
    throw new DaAvailabilityCommitmentV1Error(
      "availability commitment version must be exactly V1",
    );
  }
  requireHash(commitment.deployment_identity, 28, "deployment_identity");
  requireHash(commitment.header_hash, 28, "header_hash");
  requireHash(commitment.bond_owner, 28, "bond_owner");
  const payloadByteLength = Number(commitment.payload_byte_length);
  requireSafePositiveInteger(payloadByteLength, "payload_byte_length");
  if (BigInt(payloadByteLength) !== commitment.payload_byte_length) {
    throw new DaAvailabilityCommitmentV1Error(
      "payload length must fit a canonical safe integer",
    );
  }
  assertCanonicalDaAvailabilityResponseGeometryV1(commitment.response_geometry);
  if (
    expectedResponseGeometry !== undefined &&
    Data.to(
      commitment.response_geometry as never,
      DaAvailabilityResponseGeometryV1Schema as never,
    ) !==
      Data.to(
        expectedResponseGeometry as never,
        DaAvailabilityResponseGeometryV1Schema as never,
      )
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "response geometry does not equal the authenticated deployment/DA parameters",
    );
  }
  const layout = deriveDaAvailabilityTrancheLayoutV1(
    payloadByteLength,
    commitment.response_geometry,
  );
  if (commitment.tranche_descriptors.length !== layout.length) {
    throw new DaAvailabilityCommitmentV1Error(
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
      throw new DaAvailabilityCommitmentV1Error(
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

export const encodeDaAvailabilityCommitmentV1 = (
  commitment: DaAvailabilityCommitmentV1,
): string => {
  assertCanonicalDaAvailabilityCommitmentV1(commitment);
  return Data.to(
    commitment as never,
    DaAvailabilityCommitmentV1Schema as never,
  );
};

/** Strict signed-commitment codec for restart and cross-service handoff. */
export const parseDaAvailabilityCommitmentV1Cbor = (
  cborHex: string,
  expectedResponseGeometry?: DaAvailabilityResponseGeometryV1,
): DaAvailabilityCommitmentV1 => {
  const commitment = parseCanonicalDataCbor<
    typeof DaAvailabilityCommitmentV1Schema,
    DaAvailabilityCommitmentV1
  >({
    cborHex,
    schema: DaAvailabilityCommitmentV1Schema,
    name: "availability commitment",
  });
  assertCanonicalDaAvailabilityCommitmentV1(
    commitment,
    expectedResponseGeometry,
  );
  return commitment;
};

const assertCanonicalDaAvailabilityTrancheDescriptorV1 = (
  descriptor: DaAvailabilityTrancheDescriptorV1,
): void => {
  const trancheIndex = Number(descriptor.tranche_index);
  const startOffset = Number(descriptor.start_offset);
  const byteLength = Number(descriptor.byte_length);
  if (
    !Number.isSafeInteger(trancheIndex) ||
    trancheIndex < 0 ||
    trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1 ||
    !Number.isSafeInteger(startOffset) ||
    startOffset < 0 ||
    BigInt(trancheIndex) !== descriptor.tranche_index ||
    BigInt(startOffset) !== descriptor.start_offset
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "tranche descriptor index and offset must be canonical bounded integers",
    );
  }
  requireSafePositiveInteger(byteLength, "tranche descriptor byte_length");
  if (
    BigInt(byteLength) !== descriptor.byte_length ||
    startOffset + byteLength > DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES_V1
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "tranche descriptor must fit within the canonical 64 MiB payload range",
    );
  }
  requireHash(
    descriptor.terminal_accumulator,
    32,
    "tranche descriptor terminal_accumulator",
  );
};

export const assertCanonicalDaAvailabilityBondDatumV1 = (
  datum: DaAvailabilityBondDatumV1,
  expectedParameters?: DaAvailabilityParametersV1,
): void => {
  if (expectedParameters !== undefined) {
    assertCanonicalDaAvailabilityParametersV1(expectedParameters);
  }
  const fields = "Available" in datum ? datum.Available : datum.ChallengedBond;
  assertCanonicalDaAvailabilityCommitmentV1(
    fields.commitment,
    expectedParameters?.response_geometry,
  );
  if (!BOND_ASSET_NAME.test(fields.da_bond_asset_name)) {
    throw new DaAvailabilityCommitmentV1Error(
      "da_bond_asset_name must be the canonical 32-byte DABN identity",
    );
  }
  requireHash(fields.committee_signers_hash, 32, "committee_signers_hash");
  requireHash(fields.attested_signers, 32, "attested_signers");
  if ("ChallengedBond" in datum) {
    const challenged = datum.ChallengedBond;
    if (!CHALLENGE_ASSET_NAME.test(challenged.challenge_asset_name)) {
      throw new DaAvailabilityCommitmentV1Error(
        "challenge_asset_name must be the canonical 32-byte DACH identity",
      );
    }
    requireHash(challenged.challenger, 28, "challenger");
    if (
      challenged.opened_at < 0n ||
      challenged.response_deadline !==
        daAvailabilityResponseDeadlineV1({
          payloadByteLength: Number(challenged.commitment.payload_byte_length),
          openedAt: challenged.opened_at,
        })
    ) {
      throw new DaAvailabilityCommitmentV1Error(
        "challenged bond must carry the exact canonical response deadline",
      );
    }
  }
};

export const encodeDaAvailabilityBondDatumV1 = (
  datum: DaAvailabilityBondDatumV1,
  expectedParameters?: DaAvailabilityParametersV1,
): string => {
  assertCanonicalDaAvailabilityBondDatumV1(datum, expectedParameters);
  return Data.to(datum as never, DaAvailabilityBondDatumV1Schema as never);
};

export const parseDaAvailabilityBondDatumV1Cbor = (
  cborHex: string,
  expectedParameters?: DaAvailabilityParametersV1,
): DaAvailabilityBondDatumV1 => {
  const datum = parseCanonicalDataCbor<
    typeof DaAvailabilityBondDatumV1Schema,
    DaAvailabilityBondDatumV1
  >({
    cborHex,
    schema: DaAvailabilityBondDatumV1Schema,
    name: "availability bond datum",
  });
  assertCanonicalDaAvailabilityBondDatumV1(datum, expectedParameters);
  return datum;
};

export const assertCanonicalDaAvailabilityTrancheDatumV1 = (
  datum: DaAvailabilityTrancheDatumV1,
): void => {
  const fields = "Active" in datum ? datum.Active : datum.Receipt;
  requireHash(fields.deployment_identity, 28, "deployment_identity");
  requireHash(fields.header_hash, 28, "header_hash");
  if (!CHALLENGE_ASSET_NAME.test(fields.challenge_asset_name)) {
    throw new DaAvailabilityCommitmentV1Error(
      "challenge_asset_name must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(fields.challenger, 28, "challenger");
  assertCanonicalDaAvailabilityTrancheDescriptorV1(fields.descriptor);
  if ("Active" in datum) {
    const active = datum.Active;
    const endOffset =
      active.descriptor.start_offset + active.descriptor.byte_length;
    if (
      active.next_offset < active.descriptor.start_offset ||
      active.next_offset >= endOffset ||
      active.response_deadline < 0n
    ) {
      throw new DaAvailabilityCommitmentV1Error(
        "active tranche cursor/deadline is outside its canonical range",
      );
    }
    requireHash(active.accumulator, 32, "accumulator");
  } else if (
    datum.Receipt.terminal_accumulator !==
    datum.Receipt.descriptor.terminal_accumulator
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "receipt terminal accumulator must equal its signed descriptor",
    );
  }
};

export const encodeDaAvailabilityTrancheDatumV1 = (
  datum: DaAvailabilityTrancheDatumV1,
): string => {
  assertCanonicalDaAvailabilityTrancheDatumV1(datum);
  return Data.to(datum as never, DaAvailabilityTrancheDatumV1Schema as never);
};

export const parseDaAvailabilityTrancheDatumV1Cbor = (
  cborHex: string,
): DaAvailabilityTrancheDatumV1 => {
  const datum = parseCanonicalDataCbor<
    typeof DaAvailabilityTrancheDatumV1Schema,
    DaAvailabilityTrancheDatumV1
  >({
    cborHex,
    schema: DaAvailabilityTrancheDatumV1Schema,
    name: "availability tranche datum",
  });
  assertCanonicalDaAvailabilityTrancheDatumV1(datum);
  return datum;
};

export const assertCanonicalDaAvailabilityTerminalAccumulatorDatumV1 = (
  datum: DaAvailabilityTerminalAccumulatorDatumV1,
): void => {
  requireHash(datum.deployment_identity, 28, "deployment_identity");
  requireHash(datum.header_hash, 28, "header_hash");
  if (!CHALLENGE_ASSET_NAME.test(datum.challenge_asset_name)) {
    throw new DaAvailabilityCommitmentV1Error(
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
      BigInt(DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1) ||
    datum.response_deadline < 0n ||
    datum.remaining_challenger_lovelace <= 0n
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "terminal accumulator cursor, deadline, and remaining challenger value must be canonical",
    );
  }
};

export const encodeDaAvailabilityTerminalAccumulatorDatumV1 = (
  datum: DaAvailabilityTerminalAccumulatorDatumV1,
): string => {
  assertCanonicalDaAvailabilityTerminalAccumulatorDatumV1(datum);
  return Data.to(
    datum as never,
    DaAvailabilityTerminalAccumulatorDatumV1Schema as never,
  );
};

export const parseDaAvailabilityTerminalAccumulatorDatumV1Cbor = (
  cborHex: string,
): DaAvailabilityTerminalAccumulatorDatumV1 => {
  const datum = parseCanonicalDataCbor<
    typeof DaAvailabilityTerminalAccumulatorDatumV1Schema,
    DaAvailabilityTerminalAccumulatorDatumV1
  >({
    cborHex,
    schema: DaAvailabilityTerminalAccumulatorDatumV1Schema,
    name: "availability terminal accumulator datum",
  });
  assertCanonicalDaAvailabilityTerminalAccumulatorDatumV1(datum);
  return datum;
};

export type DaAvailabilityChallengeDatumPlanV1 = Readonly<{
  challengeAssetName: string;
  responseDeadline: bigint;
  challengedBond: DaAvailabilityBondDatumV1;
  trancheThreads: readonly DaAvailabilityTrancheDatumV1[];
  trancheFunding: readonly DaAvailabilityTrancheFundingV1[];
  terminalAccumulator: DaAvailabilityTerminalAccumulatorDatumV1;
  terminalAccumulatorFundingLovelace: bigint;
}>;

export type DaAvailabilityTrancheFundingV1 = Readonly<{
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
export const planDaAvailabilityTrancheFundingV1 = (input: {
  readonly commitment: DaAvailabilityCommitmentV1;
  readonly parameters: DaAvailabilityParametersV1;
}): readonly DaAvailabilityTrancheFundingV1[] => {
  assertCanonicalDaAvailabilityParametersV1(input.parameters);
  assertCanonicalDaAvailabilityCommitmentV1(
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
    throw new DaAvailabilityCommitmentV1Error(
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
export const buildDaAvailabilityChallengeDatumPlanV1 = (input: {
  readonly availableBond: DaAvailabilityBondDatumV1;
  readonly bondInputOutRef: OutputReference;
  readonly challenger: string;
  readonly openedAt: bigint;
  readonly parameters: DaAvailabilityParametersV1;
}): DaAvailabilityChallengeDatumPlanV1 => {
  assertCanonicalDaAvailabilityParametersV1(input.parameters);
  assertCanonicalDaAvailabilityBondDatumV1(
    input.availableBond,
    input.parameters,
  );
  if (!("Available" in input.availableBond)) {
    throw new DaAvailabilityCommitmentV1Error(
      "only an available retained DA bond may open a challenge",
    );
  }
  requireHash(input.challenger, 28, "challenger");
  const available = input.availableBond.Available;
  const challengeAssetName = daAvailabilityChallengeAssetNameV1(
    input.bondInputOutRef,
  );
  const responseDeadline = daAvailabilityResponseDeadlineV1({
    payloadByteLength: Number(available.commitment.payload_byte_length),
    openedAt: input.openedAt,
  });
  const challengedBond: DaAvailabilityBondDatumV1 = {
    ChallengedBond: {
      ...available,
      challenge_asset_name: challengeAssetName,
      challenger: input.challenger,
      opened_at: input.openedAt,
      response_deadline: responseDeadline,
    },
  };
  const trancheThreads = available.commitment.tranche_descriptors.map(
    (descriptor): DaAvailabilityTrancheDatumV1 => ({
      Active: {
        deployment_identity: available.commitment.deployment_identity,
        header_hash: available.commitment.header_hash,
        challenge_asset_name: challengeAssetName,
        descriptor,
        next_offset: descriptor.start_offset,
        accumulator: daAvailabilityTrancheStartAccumulatorV1({
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
  assertCanonicalDaAvailabilityBondDatumV1(challengedBond, input.parameters);
  trancheThreads.forEach(assertCanonicalDaAvailabilityTrancheDatumV1);
  const trancheFunding = planDaAvailabilityTrancheFundingV1({
    commitment: available.commitment,
    parameters: input.parameters,
  });
  const terminalAccumulatorFundingLovelace =
    input.parameters.max_close_fee_lovelace >
    input.parameters.max_timeout_fee_lovelace
      ? input.parameters.max_close_fee_lovelace
      : input.parameters.max_timeout_fee_lovelace;
  const terminalAccumulator: DaAvailabilityTerminalAccumulatorDatumV1 = {
    deployment_identity: available.commitment.deployment_identity,
    header_hash: available.commitment.header_hash,
    challenge_asset_name: challengeAssetName,
    next_tranche_index: 0n,
    folded_terminal_accumulator: daAvailabilityTerminalAccumulatorStartV1({
      deploymentIdentity: available.commitment.deployment_identity,
      headerHash: available.commitment.header_hash,
      challengeAssetName,
    }),
    has_timed_out_tranche: false,
    response_deadline: responseDeadline,
    challenger: input.challenger,
    remaining_challenger_lovelace: terminalAccumulatorFundingLovelace,
  };
  assertCanonicalDaAvailabilityTerminalAccumulatorDatumV1(terminalAccumulator);
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

export const planDaAvailabilityPublicationValueTransitionV1 = (input: {
  readonly threadInputLovelace: bigint;
  readonly previousCarrierInputLovelace: bigint;
  readonly nextCarrierOutputLovelace: bigint;
  readonly transactionFeeLovelace: bigint;
  readonly minimumThreadOutputLovelace: bigint;
  readonly isFirstPublication: boolean;
  readonly parameters: DaAvailabilityParametersV1;
}): bigint => {
  assertCanonicalDaAvailabilityParametersV1(input.parameters);
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
    throw new DaAvailabilityCommitmentV1Error(
      "publication value transition has a noncanonical carrier, thread floor, or fee above its authenticated ceiling",
    );
  }
  const threadOutputLovelace =
    input.threadInputLovelace +
    input.previousCarrierInputLovelace -
    input.nextCarrierOutputLovelace -
    input.transactionFeeLovelace;
  if (threadOutputLovelace < input.minimumThreadOutputLovelace) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication fee/carrier would consume the protected tranche working floor",
    );
  }
  return threadOutputLovelace;
};

export type DaAvailabilitySettlementPlanV1 = Readonly<{
  status: DaAvailabilityTrancheTerminalStatusV1;
  nextTerminalAccumulator: DaAvailabilityTerminalAccumulatorDatumV1;
  nextTerminalLovelace: bigint;
}>;

/**
 * Pure mirror of one bounded `SettleTranche` transition. Production builders
 * feed it decoded, script-authenticated UTxO data and then emit the exact datum
 * and value it returns.
 */
export const planDaAvailabilitySettlementV1 = (input: {
  readonly commitment: DaAvailabilityCommitmentV1;
  readonly terminalAccumulator: DaAvailabilityTerminalAccumulatorDatumV1;
  readonly tranche: DaAvailabilityTrancheDatumV1;
  readonly threadLovelace: bigint;
  readonly carrierLovelace: bigint;
  readonly transactionFeeLovelace: bigint;
  readonly inclusiveValidityLower: bigint;
  readonly parameters: DaAvailabilityParametersV1;
}): DaAvailabilitySettlementPlanV1 => {
  assertCanonicalDaAvailabilityParametersV1(input.parameters);
  assertCanonicalDaAvailabilityCommitmentV1(
    input.commitment,
    input.parameters.response_geometry,
  );
  assertCanonicalDaAvailabilityTerminalAccumulatorDatumV1(
    input.terminalAccumulator,
  );
  assertCanonicalDaAvailabilityTrancheDatumV1(input.tranche);
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
    throw new DaAvailabilityCommitmentV1Error(
      "settlement indices, values, or fee are not canonical",
    );
  }
  const descriptor = input.commitment.tranche_descriptors[descriptorIndex]!;
  let status: DaAvailabilityTrancheTerminalStatusV1;
  let trancheIdentity: {
    readonly deployment_identity: string;
    readonly header_hash: string;
    readonly challenge_asset_name: string;
    readonly descriptor: DaAvailabilityTrancheDescriptorV1;
    readonly challenger: string;
  };
  if ("Receipt" in input.tranche) {
    trancheIdentity = input.tranche.Receipt;
    if (
      input.tranche.Receipt.terminal_accumulator !==
      descriptor.terminal_accumulator
    ) {
      throw new DaAvailabilityCommitmentV1Error(
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
      throw new DaAvailabilityCommitmentV1Error(
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
      DaAvailabilityTrancheDescriptorV1Schema as never,
    ) !==
      Data.to(
        descriptor as never,
        DaAvailabilityTrancheDescriptorV1Schema as never,
      ) ||
    trancheIdentity.challenger !== terminal.challenger ||
    terminal.deployment_identity !== input.commitment.deployment_identity ||
    terminal.header_hash !== input.commitment.header_hash ||
    terminal.next_tranche_index !== descriptor.tranche_index
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "settlement tranche, terminal accumulator, and signed commitment identities differ",
    );
  }
  const nextTerminalLovelace =
    terminal.remaining_challenger_lovelace +
    input.threadLovelace +
    input.carrierLovelace -
    input.transactionFeeLovelace;
  if (nextTerminalLovelace <= 0n) {
    throw new DaAvailabilityCommitmentV1Error(
      "settlement consumes the protected challenger value",
    );
  }
  const nextTerminalAccumulator: DaAvailabilityTerminalAccumulatorDatumV1 = {
    ...terminal,
    next_tranche_index: terminal.next_tranche_index + 1n,
    folded_terminal_accumulator: foldDaAvailabilityTerminalAccumulatorV1({
      previousAccumulator: terminal.folded_terminal_accumulator,
      trancheIndex: descriptorIndex,
      status,
    }),
    has_timed_out_tranche:
      terminal.has_timed_out_tranche || "TimedOutTranche" in status,
    remaining_challenger_lovelace: nextTerminalLovelace,
  };
  assertCanonicalDaAvailabilityTerminalAccumulatorDatumV1(
    nextTerminalAccumulator,
  );
  return { status, nextTerminalAccumulator, nextTerminalLovelace };
};

export const assertDaAvailabilityChallengerBondConservationV1 = (input: {
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
    throw new DaAvailabilityCommitmentV1Error(
      "challenger bond is not isolated and exactly conserved by live threads, carriers, and paid fees",
    );
  }
};

export type DaAvailabilityTrancheProtectedValueV1 = Readonly<{
  trancheIndex: number;
  threadLovelace: bigint;
  carrierLovelace: bigint;
}>;

export type DaAvailabilityTrancheRefundV1 = Readonly<{
  trancheIndex: number;
  refundLovelace: bigint;
  attributedTransactionFeeLovelace: bigint;
}>;

export const planDaAvailabilityTerminalRefundV1 = (input: {
  readonly kind: "close" | "timeout";
  readonly tranches: readonly DaAvailabilityTrancheProtectedValueV1[];
  readonly transactionFeeLovelace: bigint;
  readonly parameters: DaAvailabilityParametersV1;
}): readonly DaAvailabilityTrancheRefundV1[] => {
  assertCanonicalDaAvailabilityParametersV1(input.parameters);
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
    throw new DaAvailabilityCommitmentV1Error(
      "terminal availability transition has a noncanonical protected value or fee above its authenticated ceiling",
    );
  }
  const refunds = input.tranches.map(
    (value, index): DaAvailabilityTrancheRefundV1 => {
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
    throw new DaAvailabilityCommitmentV1Error(
      "terminal availability transition leaves no challenger refund",
    );
  }
  return refunds;
};

export const assertCanonicalDaAvailabilityPublicationDatumV1 = (
  publication: DaAvailabilityPublicationDatumV1,
  expectedResponseGeometry: DaAvailabilityResponseGeometryV1,
  expectedDescriptor: DaAvailabilityTrancheDescriptorV1,
): void => {
  requireHash(publication.deployment_identity, 28, "deployment_identity");
  requireHash(publication.header_hash, 28, "header_hash");
  if (!CHALLENGE_ASSET_NAME.test(publication.challenge_asset_name)) {
    throw new DaAvailabilityCommitmentV1Error(
      "challenge_asset_name must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(publication.chunk_hash, 32, "chunk_hash");
  requireHash(publication.previous_accumulator, 32, "previous_accumulator");
  requireHash(publication.next_accumulator, 32, "next_accumulator");
  if (!CANONICAL_CBOR_HEX.test(publication.chunk)) {
    throw new DaAvailabilityCommitmentV1Error(
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
    trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY_V1 ||
    !Number.isSafeInteger(chunkIndex) ||
    chunkIndex < 0 ||
    !Number.isSafeInteger(chunkOffset) ||
    chunkOffset < 0 ||
    BigInt(trancheIndex) !== publication.tranche_index ||
    BigInt(chunkIndex) !== publication.chunk_index ||
    BigInt(chunkOffset) !== publication.chunk_offset
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication tranche/chunk indices and chunk offset must be canonical bounded integers",
    );
  }
  requireSafePositiveInteger(chunkByteLength, "chunk_byte_length");
  if (BigInt(chunkByteLength) !== publication.chunk_byte_length) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication chunk length must fit a canonical safe integer",
    );
  }
  const chunk = fromHex(publication.chunk);
  if (
    chunk.length !== chunkByteLength ||
    chunkByteLength > DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES_V1
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication chunk bytes do not equal its bounded declared length",
    );
  }
  assertCanonicalDaAvailabilityResponseGeometryV1(expectedResponseGeometry);
  if (
    publication.chunk_byte_length > expectedResponseGeometry.chunk_byte_length
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication chunk exceeds the authenticated response geometry",
    );
  }
  if (publication.chunk_hash !== toHex(blake2b(chunk, { dkLen: 32 }))) {
    throw new DaAvailabilityCommitmentV1Error(
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
        throw new DaAvailabilityCommitmentV1Error(
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
    daAvailabilityChunkLeafHashV1({
      trancheIndex,
      chunkIndex,
      chunkOffset,
      chunkByteLength,
      chunkHash: publication.chunk_hash,
    }),
    "hex",
  );
  if (
    !verifyMidgardValidationMerkleMembershipV1({
      frontier,
      leafIndex: chunkIndex,
      leafHash,
      siblings: publication.chunk_siblings.map((sibling) =>
        Buffer.from(sibling, "hex"),
      ),
    })
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication chunk is not an index-bound member of its signed frontier",
    );
  }
  if (
    expectedDescriptor.chunk_count !== BigInt(frontier.count) ||
    expectedDescriptor.chunk_commitment !==
      toHex(commitMidgardValidationMerkleFrontierV1(frontier))
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication frontier does not equal the signed tranche descriptor",
    );
  }
  const expectedNextAccumulator = daAvailabilityTrancheStepAccumulatorV1({
    deploymentIdentity: publication.deployment_identity,
    headerHash: publication.header_hash,
    trancheIndex,
    chunkOffset,
    chunk,
    previousAccumulator: publication.previous_accumulator,
  });
  if (publication.next_accumulator !== expectedNextAccumulator) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication next accumulator does not equal its canonical step",
    );
  }
};

export const encodeDaAvailabilityPublicationDatumV1 = (
  publication: DaAvailabilityPublicationDatumV1,
  expectedResponseGeometry: DaAvailabilityResponseGeometryV1,
  expectedDescriptor: DaAvailabilityTrancheDescriptorV1,
): string => {
  assertCanonicalDaAvailabilityPublicationDatumV1(
    publication,
    expectedResponseGeometry,
    expectedDescriptor,
  );
  return Data.to(
    publication as never,
    DaAvailabilityPublicationDatumV1Schema as never,
  );
};

/** Strict inline-publication codec; L1 provenance remains service-owned. */
export const parseDaAvailabilityPublicationDatumV1Cbor = (
  cborHex: string,
  expectedResponseGeometry: DaAvailabilityResponseGeometryV1,
  expectedDescriptor: DaAvailabilityTrancheDescriptorV1,
): DaAvailabilityPublicationDatumV1 => {
  const publication = parseCanonicalDataCbor<
    typeof DaAvailabilityPublicationDatumV1Schema,
    DaAvailabilityPublicationDatumV1
  >({
    cborHex,
    schema: DaAvailabilityPublicationDatumV1Schema,
    name: "availability publication",
  });
  assertCanonicalDaAvailabilityPublicationDatumV1(
    publication,
    expectedResponseGeometry,
    expectedDescriptor,
  );
  return publication;
};

export const daAvailabilityAttestationMessageV1 = (
  commitment: DaAvailabilityCommitmentV1,
): Uint8Array => {
  assertCanonicalDaAvailabilityCommitmentV1(commitment);
  return fromHex(
    hashDomainAndData(
      ATTESTATION_COMMITMENT_DOMAIN,
      Data.to(commitment as never, DaAvailabilityCommitmentV1Schema as never),
    ),
  );
};

/** Compact state-queue marker admitted only after every ordered receipt. */
export const daAvailabilityPublishedTerminalCommitmentV1 = (
  commitment: DaAvailabilityCommitmentV1,
): string => {
  assertCanonicalDaAvailabilityCommitmentV1(commitment);
  return hashDomainAndData(
    PUBLISHED_TERMINAL_DOMAIN,
    Data.to(commitment as never, DaAvailabilityCommitmentV1Schema as never),
  );
};

export const verifyDaAvailabilityPayloadCommitmentV1 = (input: {
  readonly commitment: DaAvailabilityCommitmentV1;
  readonly payload: Uint8Array;
}): boolean => {
  assertCanonicalDaAvailabilityCommitmentV1(input.commitment);
  if (BigInt(input.payload.length) !== input.commitment.payload_byte_length) {
    return false;
  }
  const rebuilt = buildDaAvailabilityCommitmentV1({
    deploymentIdentity: input.commitment.deployment_identity,
    headerHash: input.commitment.header_hash,
    payload: input.payload,
    bondOwner: input.commitment.bond_owner,
    responseGeometry: input.commitment.response_geometry,
  });
  return (
    Data.to(rebuilt as never, DaAvailabilityCommitmentV1Schema as never) ===
    Data.to(
      input.commitment as never,
      DaAvailabilityCommitmentV1Schema as never,
    )
  );
};

export type DaAvailabilityTranchePublicationPlanV1 = Readonly<{
  descriptor: DaAvailabilityTrancheDescriptorV1;
  initialAccumulator: string;
  publications: readonly DaAvailabilityPublicationDatumV1[];
}>;

export type DaAvailabilityPublicationTierV1 =
  | "complete_item_inline"
  | "ordered_chunks"
  | "parallel_tranches";

/**
 * Chooses the least fragmented response tier permitted by the authenticated
 * applied-transaction measurement. A complete item is never split when it
 * fits the signed inline-publication byte limit.
 */
export const daAvailabilityPublicationTierV1 = (input: {
  readonly payloadByteLength: number;
  readonly responseGeometry: DaAvailabilityResponseGeometryV1;
}): DaAvailabilityPublicationTierV1 => {
  daAvailabilityResponseWindowMsV1(input.payloadByteLength);
  assertCanonicalDaAvailabilityResponseGeometryV1(input.responseGeometry);
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
export const planDaAvailabilityPublicationsV1 = (input: {
  readonly commitment: DaAvailabilityCommitmentV1;
  readonly payload: Uint8Array;
  readonly challengeAssetName: string;
}): readonly DaAvailabilityTranchePublicationPlanV1[] => {
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentV1Error(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  if (
    !verifyDaAvailabilityPayloadCommitmentV1({
      commitment: input.commitment,
      payload: input.payload,
    })
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "payload does not equal the signed DA availability commitment",
    );
  }
  const chunkByteLength = Number(
    input.commitment.response_geometry.chunk_byte_length,
  );
  const tier = daAvailabilityPublicationTierV1({
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
    } satisfies DaAvailabilityTrancheLayoutV1;
    const chunkLeaves = trancheChunkLeafHashes({
      layout,
      payload: input.payload,
      chunkByteLength,
    });
    const membershipIndex =
      buildMidgardValidationMerkleMembershipIndexV1(chunkLeaves);
    if (
      descriptor.chunk_count !== BigInt(chunkLeaves.length) ||
      descriptor.chunk_commitment !==
        toHex(commitMidgardValidationMerkleFrontierV1(membershipIndex.frontier))
    ) {
      throw new DaAvailabilityCommitmentV1Error(
        `tranche ${trancheIndex.toString()} chunk commitment does not equal the signed payload`,
      );
    }
    const initialAccumulator = daAvailabilityTrancheStartAccumulatorV1({
      deploymentIdentity: input.commitment.deployment_identity,
      headerHash: input.commitment.header_hash,
      trancheIndex,
      startOffset,
      byteLength: Number(descriptor.byte_length),
    });
    let previousAccumulator = initialAccumulator;
    const publications: DaAvailabilityPublicationDatumV1[] = [];
    let chunkIndex = 0;
    for (
      let chunkOffset = startOffset;
      chunkOffset < endOffset;
      chunkOffset += chunkByteLength
    ) {
      const chunkEnd = Math.min(chunkOffset + chunkByteLength, endOffset);
      const chunk = input.payload.subarray(chunkOffset, chunkEnd);
      const chunkHash = toHex(blake2b(chunk, { dkLen: 32 }));
      const nextAccumulator = daAvailabilityTrancheStepAccumulatorV1({
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
      throw new DaAvailabilityCommitmentV1Error(
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
      throw new DaAvailabilityCommitmentV1Error(
        "a complete fitting availability item must use exactly one inline publication",
      );
    }
    return { descriptor, initialAccumulator, publications };
  });
};

/** Off-chain twin of the deadline-bound in-tranche validator transition. */
export const advanceDaAvailabilityTrancheV1 = (input: {
  readonly active: DaAvailabilityTrancheDatumV1;
  readonly publication: DaAvailabilityPublicationDatumV1;
  readonly responseGeometry: DaAvailabilityResponseGeometryV1;
  readonly inclusiveValidityUpper: bigint;
  readonly carrierOutputIndex: bigint;
}): DaAvailabilityTrancheDatumV1 => {
  assertCanonicalDaAvailabilityResponseGeometryV1(input.responseGeometry);
  if (typeof input.active !== "object" || !("Active" in input.active)) {
    throw new DaAvailabilityCommitmentV1Error(
      "a terminal receipt cannot accept another publication",
    );
  }
  const active = input.active.Active;
  if (input.carrierOutputIndex < 0n) {
    throw new DaAvailabilityCommitmentV1Error(
      "publication carrier output index must be non-negative",
    );
  }
  if (input.inclusiveValidityUpper > active.response_deadline) {
    throw new DaAvailabilityCommitmentV1Error(
      "availability publication validity upper exceeds the response deadline",
    );
  }
  const descriptor = active.descriptor;
  assertCanonicalDaAvailabilityPublicationDatumV1(
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
  const nextAccumulator = daAvailabilityTrancheStepAccumulatorV1({
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
    throw new DaAvailabilityCommitmentV1Error(
      "availability publication does not exactly advance the authenticated tranche",
    );
  }
  const nextOffset = active.next_offset + expectedChunkLength;
  if (nextOffset === endOffset) {
    if (nextAccumulator !== descriptor.terminal_accumulator) {
      throw new DaAvailabilityCommitmentV1Error(
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
export const assertDaAvailabilityTerminalReceiptsV1 = (input: {
  readonly commitment: DaAvailabilityCommitmentV1;
  readonly challengeAssetName: string;
  readonly challenger: string;
  readonly receipts: readonly DaAvailabilityTrancheDatumV1[];
}): string => {
  assertCanonicalDaAvailabilityCommitmentV1(input.commitment);
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentV1Error(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(input.challenger, 28, "challenger");
  if (input.receipts.length !== input.commitment.tranche_descriptors.length) {
    throw new DaAvailabilityCommitmentV1Error(
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
      throw new DaAvailabilityCommitmentV1Error(
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
        DaAvailabilityTrancheDescriptorV1Schema as never,
      ) !==
        Data.to(
          descriptor as never,
          DaAvailabilityTrancheDescriptorV1Schema as never,
        ) ||
      receipt.terminal_accumulator !== descriptor.terminal_accumulator
    ) {
      throw new DaAvailabilityCommitmentV1Error(
        `terminal receipt ${index.toString()} does not equal its signed descriptor`,
      );
    }
  }
  return daAvailabilityPublishedTerminalCommitmentV1(input.commitment);
};

export type DaAvailabilityPublicationObservationV1 = Readonly<{
  publication: DaAvailabilityPublicationDatumV1;
  inclusiveValidityUpper: bigint;
  /** Exact output index of this publication's carrier in its admitted L1 tx. */
  carrierOutputIndex: bigint;
}>;

export type DaAvailabilityTrancheEvidenceV1 = Readonly<{
  descriptor: DaAvailabilityTrancheDescriptorV1;
  publications: readonly DaAvailabilityPublicationObservationV1[];
}>;

export type DaAvailabilityChallengedBondEvidenceV1 = Readonly<{
  /** Exact inline datum read from the challenged retained-bond output. */
  datumCborHex: string;
  /** Available-bond input consumed by the challenge transaction; derives DACH. */
  bondInputOutRef: OutputReference;
  /** Challenged-bond output carrying the datum and retained DABN identity. */
  challengedBondOutputOutRef: OutputReference;
}>;

type DaAvailabilityChallengedBondFieldsV1 = Extract<
  DaAvailabilityBondDatumV1,
  { ChallengedBond: unknown }
>["ChallengedBond"];

const assertCanonicalDaAvailabilityEvidenceOutRefV1 = (
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
    throw new DaAvailabilityCommitmentV1Error(
      `${field} must be a canonical bounded Cardano output reference`,
    );
  }
};

const challengedBondFieldsFromEvidenceV1 = (
  evidence: DaAvailabilityChallengedBondEvidenceV1,
  parameters: DaAvailabilityParametersV1,
): DaAvailabilityChallengedBondFieldsV1 => {
  assertCanonicalDaAvailabilityParametersV1(parameters);
  if (
    typeof evidence !== "object" ||
    evidence === null ||
    Object.getPrototypeOf(evidence) !== Object.prototype ||
    Reflect.ownKeys(evidence).length !== 3 ||
    !Reflect.has(evidence, "datumCborHex") ||
    !Reflect.has(evidence, "bondInputOutRef") ||
    !Reflect.has(evidence, "challengedBondOutputOutRef")
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "challengedBond evidence must contain exactly datum and input/output identities",
    );
  }
  assertCanonicalDaAvailabilityEvidenceOutRefV1(
    evidence.bondInputOutRef,
    "challengedBond.bondInputOutRef",
  );
  assertCanonicalDaAvailabilityEvidenceOutRefV1(
    evidence.challengedBondOutputOutRef,
    "challengedBond.challengedBondOutputOutRef",
  );
  if (
    evidence.bondInputOutRef.transactionId ===
      evidence.challengedBondOutputOutRef.transactionId &&
    evidence.bondInputOutRef.outputIndex ===
      evidence.challengedBondOutputOutRef.outputIndex
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "challenged bond output cannot equal its consumed available-bond input",
    );
  }
  const bondDatum = parseDaAvailabilityBondDatumV1Cbor(
    evidence.datumCborHex,
    parameters,
  );
  if (!("ChallengedBond" in bondDatum)) {
    throw new DaAvailabilityCommitmentV1Error(
      "public evidence reconstruction requires the authenticated challenged-bond datum",
    );
  }
  const challenged = bondDatum.ChallengedBond;
  const expectedChallengeAssetName = daAvailabilityChallengeAssetNameV1(
    evidence.bondInputOutRef,
  );
  if (challenged.challenge_asset_name !== expectedChallengeAssetName) {
    throw new DaAvailabilityCommitmentV1Error(
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
export const planDaAvailabilityPublicationsFromChallengedBondV1 = (input: {
  readonly challengedBond: DaAvailabilityChallengedBondEvidenceV1;
  readonly parameters: DaAvailabilityParametersV1;
  readonly payload: Uint8Array;
}): readonly DaAvailabilityTranchePublicationPlanV1[] => {
  const challenged = challengedBondFieldsFromEvidenceV1(
    input.challengedBond,
    input.parameters,
  );
  return planDaAvailabilityPublicationsV1({
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
export const reconstructDaAvailabilityPayloadV1 = (input: {
  readonly challengedBond: DaAvailabilityChallengedBondEvidenceV1;
  readonly parameters: DaAvailabilityParametersV1;
  readonly tranches: readonly DaAvailabilityTrancheEvidenceV1[];
}): Uint8Array => {
  const challenged = challengedBondFieldsFromEvidenceV1(
    input.challengedBond,
    input.parameters,
  );
  const commitment = challenged.commitment;
  if (input.tranches.length !== commitment.tranche_descriptors.length) {
    throw new DaAvailabilityCommitmentV1Error(
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
        DaAvailabilityTrancheDescriptorV1Schema as never,
      ) !==
        Data.to(
          descriptor as never,
          DaAvailabilityTrancheDescriptorV1Schema as never,
        )
    ) {
      throw new DaAvailabilityCommitmentV1Error(
        `public evidence tranche ${index.toString()} is missing or reordered`,
      );
    }
    let state: DaAvailabilityTrancheDatumV1 = {
      Active: {
        deployment_identity: commitment.deployment_identity,
        header_hash: commitment.header_hash,
        challenge_asset_name: challenged.challenge_asset_name,
        descriptor,
        next_offset: descriptor.start_offset,
        accumulator: daAvailabilityTrancheStartAccumulatorV1({
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
      state = advanceDaAvailabilityTrancheV1({
        active: state,
        publication: observation.publication,
        responseGeometry: commitment.response_geometry,
        inclusiveValidityUpper: observation.inclusiveValidityUpper,
        carrierOutputIndex: observation.carrierOutputIndex,
      });
      payloadParts.push(fromHex(observation.publication.chunk));
    }
    if (typeof state !== "object" || !("Receipt" in state)) {
      throw new DaAvailabilityCommitmentV1Error(
        `public evidence tranche ${index.toString()} is incomplete`,
      );
    }
  }
  const payload = Uint8Array.from(
    Buffer.concat(payloadParts.map((part) => Buffer.from(part))),
  );
  if (
    !verifyDaAvailabilityPayloadCommitmentV1({
      commitment,
      payload,
    })
  ) {
    throw new DaAvailabilityCommitmentV1Error(
      "reconstructed public evidence does not equal the signed payload commitment",
    );
  }
  return payload;
};
