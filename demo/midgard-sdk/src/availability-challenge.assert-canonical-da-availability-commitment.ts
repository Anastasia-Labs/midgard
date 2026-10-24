import { Data, fromHex, toHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  assertCanonicalDaAvailabilityParameters,
  assertCanonicalDaAvailabilityResponseGeometry,
  deriveDaAvailabilityTrancheLayout,
} from "./availability-challenge.assert-canonical-da-availability-parameters.js";
import {
  DaAvailabilityCommitmentError,
  daAvailabilityResponseDeadline,
  DaAvailabilityTerminalAccumulatorDatum,
  parseCanonicalDataCbor,
  requireHash,
  requireSafePositiveInteger,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  CHALLENGE_ASSET_NAME,
  DA_AVAILABILITY_COMMITMENT_VERSION,
  DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES,
  DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY,
  DaAvailabilityChallengeRecord,
  DaAvailabilityChallengeRecordSchema,
  DaAvailabilityCommitment,
  DaAvailabilityCommitmentSchema,
  DaAvailabilityParameters,
  DaAvailabilityResponseGeometry,
  DaAvailabilityResponseGeometrySchema,
  DaAvailabilityTrancheDatum,
  DaAvailabilityTrancheDatumSchema,
  DaAvailabilityTrancheDescriptor,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";

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

/**
 * The `commitment_hash` a state-queue node carries once attested: the untagged
 * blake2b-256 of the commitment's Plutus Data serialisation (the twin of Aiken
 * `commitment_hash_v1`, which is `serialise_and_hash_32` with no domain). The
 * commitment must be canonical; hashing a malformed one would yield an
 * identity no on-chain status can ever carry.
 */
export const daAvailabilityCommitmentHash = (
  commitment: DaAvailabilityCommitment,
): string =>
  toHex(
    blake2b(fromHex(encodeDaAvailabilityCommitment(commitment)), {
      dkLen: 32,
    }),
  );

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

/**
 * A challenge record is canonical when its commitment is canonical (under the
 * authenticated response geometry, when given), its identities have their
 * exact widths and prefix, and its deadline is exactly the one the payload
 * size class derives from `opened_at`, as `OpenChallenge` enforces on-chain.
 */
export const assertCanonicalDaAvailabilityChallengeRecord = (
  record: DaAvailabilityChallengeRecord,
  expectedParameters?: DaAvailabilityParameters,
): void => {
  if (expectedParameters !== undefined) {
    assertCanonicalDaAvailabilityParameters(expectedParameters);
  }
  assertCanonicalDaAvailabilityCommitment(
    record.commitment,
    expectedParameters?.response_geometry,
  );
  if (!CHALLENGE_ASSET_NAME.test(record.challenge_asset_name)) {
    throw new DaAvailabilityCommitmentError(
      "challenge_asset_name must be the canonical 32-byte DACH identity",
    );
  }
  requireHash(record.challenger, 28, "challenger");
  if (
    record.opened_at < 0n ||
    record.response_deadline !==
      daAvailabilityResponseDeadline({
        payloadByteLength: Number(record.commitment.payload_byte_length),
        openedAt: record.opened_at,
      })
  ) {
    throw new DaAvailabilityCommitmentError(
      "challenge record must carry the exact canonical response deadline",
    );
  }
};

export const encodeDaAvailabilityChallengeRecord = (
  record: DaAvailabilityChallengeRecord,
  expectedParameters?: DaAvailabilityParameters,
): string => {
  assertCanonicalDaAvailabilityChallengeRecord(record, expectedParameters);
  return Data.to(record as never, DaAvailabilityChallengeRecordSchema as never);
};

/** Strict challenge-record codec: canonical CBOR and a canonical record. */
export const parseDaAvailabilityChallengeRecordCbor = (
  cborHex: string,
  expectedParameters?: DaAvailabilityParameters,
): DaAvailabilityChallengeRecord => {
  const record = parseCanonicalDataCbor<
    typeof DaAvailabilityChallengeRecordSchema,
    DaAvailabilityChallengeRecord
  >({
    cborHex,
    schema: DaAvailabilityChallengeRecordSchema,
    name: "availability challenge record",
  });
  assertCanonicalDaAvailabilityChallengeRecord(record, expectedParameters);
  return record;
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
