/**
 * The CARRIAGE half of published-chunk MPF proof carriage (issue #545): what a
 * step builder needs in order to take the chunked route.
 *
 * `publish-proof-chunks.ts` is the publication half, and it selects a fee input
 * through the double-spend step-01 module. Keeping the carriage helpers here
 * lets the four foundational families' step builders reach them without
 * importing that publication module back through themselves.
 */

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  ABSENT_VALUE_HASH,
  ChunkedProofClaim,
  MAXIMUM_CHUNK_PROOF_STEP_COUNT,
  MAXIMUM_PROOF_CHUNK_COUNT,
  MPF_CHUNKED_VERIFY_WITHDRAW_TITLE,
  Proof,
  ProofChunkDatum,
  requireReferenceInputIndex,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type RedeemerContext,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import { getCompiledScript } from "./runtime.js";

/** One published proof chunk, as the step builder needs to see it. */
export type PublishedProofChunk = {
  readonly utxo: UTxO;
  readonly outRef: string;
  readonly datumCbor: string;
  readonly stepCount: number;
};

/**
 * Splits a proof's steps into publication chunks of at most
 * `MAXIMUM_CHUNK_PROOF_STEP_COUNT` steps, in proof order.
 *
 * Returns the chunks' inline-datum CBOR. The concatenation of the chunks in
 * this order is the original proof, which is the only property the on-chain
 * verification depends on.
 */
export const splitProofIntoChunkDatums = (
  proofCbor: string,
): readonly string[] => {
  const steps = Data.from(proofCbor, Proof) as unknown as readonly unknown[];
  if (steps.length === 0) {
    return [];
  }
  const datums: string[] = [];
  for (
    let offset = 0;
    offset < steps.length;
    offset += MAXIMUM_CHUNK_PROOF_STEP_COUNT
  ) {
    const slice = steps.slice(offset, offset + MAXIMUM_CHUNK_PROOF_STEP_COUNT);
    datums.push(
      canonicalPlutusDataCbor(
        Data.to({ proof_steps: slice } as never, ProofChunkDatum),
      ),
    );
  }
  if (datums.length > MAXIMUM_PROOF_CHUNK_COUNT) {
    throw new Error(
      `A proof of ${String(steps.length)} steps needs ${String(datums.length)} publication chunks, above the on-chain maximum of ${String(MAXIMUM_PROOF_CHUNK_COUNT)}.`,
    );
  }
  return datums;
};

/**
 * Wallet inputs with the published chunks removed, so a step transaction can
 * never spend the very UTxOs it is referencing (nor offer them as collateral).
 */
export const walletInputsExcludingChunks = ({
  walletUtxos,
  chunks,
}: {
  readonly walletUtxos: readonly UTxO[];
  readonly chunks: readonly PublishedProofChunk[];
}): UTxO[] =>
  walletUtxos.filter(
    (utxo) =>
      !chunks.some(
        (chunk) =>
          chunk.utxo.txHash === utxo.txHash &&
          chunk.utxo.outputIndex === utxo.outputIndex,
      ),
  );

/**
 * The chunks' positions in the ledger-canonical reference-input order, which is
 * the order the on-chain `tx.reference_inputs` presents and therefore the order
 * the step's redeemer must name.
 *
 * Derived from the builder's own reference-input list before the transaction is
 * balanced; `requireBuiltChunkReferenceIndices` re-derives it from the final
 * builder context to prove the two agree.
 */
export const derivedChunkReferenceIndices = ({
  referenceInputs,
  chunks,
  label,
}: {
  readonly referenceInputs: readonly UTxO[];
  readonly chunks: readonly PublishedProofChunk[];
  readonly label: string;
}): bigint[] => {
  const canonical = [...referenceInputs].sort(compareOutRefs);
  return chunks.map((chunk) => {
    const index = canonical.findIndex(
      (utxo) =>
        utxo.txHash === chunk.utxo.txHash &&
        utxo.outputIndex === chunk.utxo.outputIndex,
    );
    if (index < 0) {
      throw new Error(
        `Published proof chunk ${chunk.outRef} is not among the ${label} reference inputs.`,
      );
    }
    return BigInt(index);
  });
};

/** Re-derives the chunk order from the built transaction and requires a match. */
export const requireBuiltChunkReferenceIndices = ({
  ctx,
  chunks,
  derived,
  label,
}: {
  readonly ctx: RedeemerContext;
  readonly chunks: readonly PublishedProofChunk[];
  readonly derived: readonly bigint[];
  readonly label: string;
}): void => {
  for (const [position, chunk] of chunks.entries()) {
    const builtIndex = requireReferenceInputIndex(
      ctx,
      chunk.utxo,
      `${label} published proof chunk`,
    );
    if (builtIndex !== derived[position]) {
      throw new Error(
        `Published proof chunk ${chunk.outRef} landed at reference-input index ${builtIndex.toString()}, not the derived ${String(derived[position])}.`,
      );
    }
  }
};

/**
 * The merkelized verifier's claim redeemer for one membership opening.
 *
 * A step on the chunked route attaches this in place of the `phas` membership
 * withdrawal: the proof never enters the transaction, only the claim and the
 * chunks' reference inputs do.
 */
export const chunkedMembershipClaimRedeemer = ({
  merkleRoot,
  keyBytes,
  valueBytes,
  orderedChunkReferenceInputIndices,
}: {
  readonly merkleRoot: string;
  readonly keyBytes: string;
  readonly valueBytes: string;
  readonly orderedChunkReferenceInputIndices: readonly bigint[];
}): string =>
  Data.to(
    {
      mode: "Membership",
      merkle_root: merkleRoot,
      key_bytes: keyBytes,
      value_hash: computeHash32(Buffer.from(valueBytes, "hex")).toString("hex"),
      ordered_chunk_reference_input_indices: [
        ...orderedChunkReferenceInputIndices,
      ],
    },
    ChunkedProofClaim,
  );

/**
 * The same claim for one ABSENCE opening, which the merkelized verifier
 * dispatches through its non-membership terminal. An absence has no value, so
 * the claim carries the pinned absent-value digest rather than a hash of one.
 */
export const chunkedNonMembershipClaimRedeemer = ({
  merkleRoot,
  keyBytes,
  orderedChunkReferenceInputIndices,
}: {
  readonly merkleRoot: string;
  readonly keyBytes: string;
  readonly orderedChunkReferenceInputIndices: readonly bigint[];
}): string =>
  Data.to(
    {
      mode: "NonMembership",
      merkle_root: merkleRoot,
      key_bytes: keyBytes,
      value_hash: ABSENT_VALUE_HASH,
      ordered_chunk_reference_input_indices: [
        ...orderedChunkReferenceInputIndices,
      ],
    },
    ChunkedProofClaim,
  );

export const chunkedVerifyWithdrawalScript = (blueprint: unknown): Script => ({
  type: "PlutusV3",
  script: getCompiledScript(blueprint, MPF_CHUNKED_VERIFY_WITHDRAW_TITLE),
});
