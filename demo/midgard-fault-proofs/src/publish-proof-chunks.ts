/**
 * Published-chunk carriage of an MPF proof (issue #545).
 *
 * A fault-proof step that carries its membership proof in the step
 * transaction's redeemers pays ~276 complete-signed-transaction bytes per
 * forced branch level, which exhausts the 16,384-byte L1 envelope at branch
 * level 21..23 (finding `Q1X-F5`). The on-chain remediation
 * (`midgard/mpf_chunked_proof_v1`, wired into the four foundational families by
 * `midgard/fraud_proofs/chunked_inclusion_v1`) takes the proof out of the step
 * transaction entirely: the very same canonical `ProofStep` list is published
 * across bounded, inert UTxOs whose inline datum carries nothing but steps, and
 * the step transaction references them and names their order.
 *
 * This module is the off-chain half. It splits a proof, publishes the chunks,
 * and hands back UTxOs a step builder can `readFrom`. Publication needs no
 * validator, no token and no ordering claim: a chunk is data, the step trusts
 * its content and never its provenance, and a chunk whose steps do not
 * reconstruct the challenged root simply fails verification.
 */

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  ChunkedProofClaim,
  MAXIMUM_CHUNK_PROOF_STEP_COUNT,
  MAXIMUM_PROOF_CHUNK_COUNT,
  MPF_CHUNKED_VERIFY_WITHDRAW_TITLE,
  Proof,
  ProofChunkDatum,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  getCompiledScript,
  outRefLabel,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
} from "./spend-input-witness.js";
import { selectFeeInput } from "./submit-step-01.js";

export { MAXIMUM_CHUNK_PROOF_STEP_COUNT, MAXIMUM_PROOF_CHUNK_COUNT };

/** One published proof chunk, as the step builder needs to see it. */
export type PublishedProofChunkV1 = {
  readonly utxo: UTxO;
  readonly outRef: string;
  readonly datumCbor: string;
  readonly stepCount: number;
};

export type PublishedProofV1 = {
  readonly txHash: string;
  readonly chunks: readonly PublishedProofChunkV1[];
  readonly proofStepCount: number;
  readonly spentFeeInput: UTxO;
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
 * Publishes a proof as chunk UTxOs at the prover's own key address.
 *
 * The UTxOs hold Ada only, carry no reference script, and are never spent by
 * the step that references them — a step builder must keep them out of both its
 * inputs and its collateral.
 */
export const publishProofChunksV1 = async ({
  lucid,
  network,
  signer,
  proofCbor,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly proofCbor: string;
  readonly awaitConfirmation?: boolean;
}): Promise<PublishedProofV1> => {
  const datums = splitProofIntoChunkDatums(proofCbor);
  if (datums.length === 0) {
    throw new Error(
      "A zero-step proof has nothing to publish; carry it in the step redeemer instead.",
    );
  }
  const address = credentialToAddress(network, {
    type: "Key",
    hash: signer.paymentKeyHash,
  });
  const { coinsPerUtxoByte } = await resolveProtocolParameters(lucid);

  signer.selectWallet(lucid);
  const spentFeeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let tx = lucid.newTx().collectFrom([spentFeeInput]);
  for (const datum of datums) {
    tx = tx.pay.ToAddressWithData(
      address,
      { kind: "inline", value: datum },
      {
        lovelace: minimumLovelaceForInlineDatumOutput({
          address,
          datum,
          coinsPerUtxoByte,
        }),
      },
    );
  }
  const unsigned = await tx
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });

  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  const claimed = new Set<number>();
  const chunks: PublishedProofChunkV1[] = [];
  const candidates = await lucid.utxosAt(address);
  for (const datum of datums) {
    const match = candidates.find(
      (utxo) =>
        !claimed.has(utxo.outputIndex) &&
        utxo.txHash === txHash &&
        typeof utxo.datum === "string" &&
        canonicalPlutusDataCbor(utxo.datum) === datum,
    );
    if (match === undefined) {
      throw new Error(
        `Published proof chunk was not found among the outputs of ${txHash}.`,
      );
    }
    claimed.add(match.outputIndex);
    const confirmed = await fetchUtxoByOutRef({
      lucid,
      outRef: { txHash: match.txHash, outputIndex: match.outputIndex },
      label: "published proof chunk",
    });
    if (confirmed.datumHash != null || typeof confirmed.datum !== "string") {
      throw new Error(
        `Published proof chunk ${outRefLabel(confirmed)} does not carry an inline datum.`,
      );
    }
    if (confirmed.scriptRef != null) {
      throw new Error(
        `Published proof chunk ${outRefLabel(confirmed)} carries a reference script.`,
      );
    }
    const decoded = Data.from(datum, ProofChunkDatum) as unknown as {
      readonly proof_steps: readonly unknown[];
    };
    chunks.push({
      utxo: confirmed,
      outRef: outRefLabel(confirmed),
      datumCbor: datum,
      stepCount: decoded.proof_steps.length,
    });
  }

  return {
    txHash,
    chunks,
    proofStepCount: chunks.reduce((total, chunk) => total + chunk.stepCount, 0),
    spentFeeInput,
  };
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
  readonly chunks: readonly PublishedProofChunkV1[];
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
 * The merkelized verifier's script, reward address and claim redeemer for one
 * membership opening.
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

export const chunkedVerifyWithdrawalScript = (blueprint: unknown): Script => ({
  type: "PlutusV3",
  script: getCompiledScript(blueprint, MPF_CHUNKED_VERIFY_WITHDRAW_TITLE),
});
