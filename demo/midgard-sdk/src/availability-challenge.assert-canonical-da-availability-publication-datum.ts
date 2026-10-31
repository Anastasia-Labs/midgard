import {
  commitMidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import { Data, fromHex, toHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { assertCanonicalDaAvailabilityCommitment } from "./availability-challenge.assert-canonical-da-availability-commitment.js";
import { assertCanonicalDaAvailabilityResponseGeometry } from "./availability-challenge.assert-canonical-da-availability-parameters.js";
import {
  buildDaAvailabilityCommitment,
  daAvailabilityChunkLeafHash,
  daAvailabilityTrancheStepAccumulator,
} from "./availability-challenge.build-da-availability-commitment.js";
import {
  DaAvailabilityCommitmentError,
  DaAvailabilityPublicationDatum,
  DaAvailabilityPublicationDatumSchema,
  daAvailabilityResponseWindowMs,
  hashDomainAndData,
  parseCanonicalDataCbor,
  requireHash,
  requireSafePositiveInteger,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  ATTESTATION_COMMITMENT_DOMAIN,
  CANONICAL_CBOR_HEX,
  CHALLENGE_ASSET_NAME,
  DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES,
  DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY,
  DaAvailabilityCommitment,
  DaAvailabilityCommitmentSchema,
  DaAvailabilityResponseGeometry,
  DaAvailabilityTrancheDescriptor,
  PUBLISHED_TERMINAL_DOMAIN,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";

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
