import {
  buildMidgardValidationMerkleMembershipIndex,
  commitMidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import { Data, toHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  assertCanonicalDaAvailabilityResponseGeometry,
  daAvailabilityTrancheStartAccumulator,
  deriveDaAvailabilityTrancheLayout,
} from "./availability-challenge.assert-canonical-da-availability-parameters.js";
import {
  DaAvailabilityCommitmentError,
  type DaAvailabilityTrancheLayout,
  hashDomainAndData,
  requireHash,
  requireSafePositiveInteger,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  CHUNK_LEAF_DOMAIN,
  DA_AVAILABILITY_COMMITMENT_VERSION,
  DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY,
  DaAvailabilityChunkLeafSchema,
  DaAvailabilityCommitment,
  DaAvailabilityResponseGeometry,
  DaAvailabilityTerminalAccumulatorStepSchema,
  DaAvailabilityTrancheDescriptor,
  DaAvailabilityTrancheStepSchema,
  DaAvailabilityTrancheTerminalStatus,
  TERMINAL_ACCUMULATOR_STEP_DOMAIN,
  TRANCHE_STEP_DOMAIN,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";

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

export const trancheChunkLeafHashes = (input: {
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
  readonly responseGeometry: DaAvailabilityResponseGeometry;
}): DaAvailabilityCommitment => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
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
  };
};
