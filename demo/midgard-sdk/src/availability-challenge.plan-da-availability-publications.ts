import {
  buildMidgardValidationMerkleMembershipIndex,
  commitMidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import { Data, fromHex, toHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { assertCanonicalDaAvailabilityCommitment } from "./availability-challenge.assert-canonical-da-availability-commitment.js";
import {
  assertCanonicalDaAvailabilityResponseGeometry,
  daAvailabilityTrancheStartAccumulator,
} from "./availability-challenge.assert-canonical-da-availability-parameters.js";
import {
  assertCanonicalDaAvailabilityPublicationDatum,
  daAvailabilityPublicationTier,
  daAvailabilityPublishedTerminalCommitment,
  type DaAvailabilityTranchePublicationPlan,
  verifyDaAvailabilityPayloadCommitment,
} from "./availability-challenge.assert-canonical-da-availability-publication-datum.js";
import {
  daAvailabilityTrancheStepAccumulator,
  trancheChunkLeafHashes,
} from "./availability-challenge.build-da-availability-commitment.js";
import {
  DaAvailabilityCommitmentError,
  DaAvailabilityPublicationDatum,
  type DaAvailabilityTrancheLayout,
  requireHash,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  CHALLENGE_ASSET_NAME,
  DaAvailabilityCommitment,
  DaAvailabilityResponseGeometry,
  DaAvailabilityTrancheDatum,
  DaAvailabilityTrancheDescriptor,
  DaAvailabilityTrancheDescriptorSchema,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";

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
