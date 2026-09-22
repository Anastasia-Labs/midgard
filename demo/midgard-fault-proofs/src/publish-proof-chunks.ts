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
 * This module is the off-chain publication half. It splits a proof, publishes
 * the chunks, and hands back UTxOs a step builder can `readFrom`. Publication
 * needs no validator, no token and no ordering claim: a chunk is data, the step
 * trusts its content and never its provenance, and a chunk whose steps do not
 * reconstruct the challenged root simply fails verification. The carriage half
 * the step builders reach for lives in `proof-chunk-carriage.ts` and is
 * re-exported here.
 */

import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  MAXIMUM_CHUNK_PROOF_STEP_COUNT,
  MAXIMUM_PROOF_CHUNK_COUNT,
  ProofChunkDatum,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type PublishedProofChunk,
  splitProofIntoChunkDatums,
} from "./proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
} from "./spend-input-witness.js";
import { selectFeeInput } from "./submit-step-01.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
} from "./workflow/transaction-boundary.js";

export * from "./proof-chunk-carriage.js";
export { MAXIMUM_CHUNK_PROOF_STEP_COUNT, MAXIMUM_PROOF_CHUNK_COUNT };

export type PublishedProof = {
  readonly txHash: string;
  readonly chunks: readonly PublishedProofChunk[];
  readonly proofStepCount: number;
  readonly spentFeeInput: UTxO;
};

export const resolvePublishedProofChunks = async ({
  lucid,
  address,
  proofCbor,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly proofCbor: string;
}): Promise<readonly PublishedProofChunk[] | undefined> => {
  const datums = splitProofIntoChunkDatums(proofCbor);
  if (datums.length === 0) return [];
  const candidates = await lucid.utxosAt(address);
  const claimed = new Set<string>();
  const chunks: PublishedProofChunk[] = [];
  for (const datum of datums) {
    const match = candidates.find((utxo) => {
      const label = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
      return (
        !claimed.has(label) &&
        typeof utxo.datum === "string" &&
        utxo.datumHash == null &&
        utxo.scriptRef == null &&
        canonicalPlutusDataCbor(utxo.datum) === datum
      );
    });
    if (match === undefined) return undefined;
    const label = `${match.txHash}#${match.outputIndex.toString()}`;
    claimed.add(label);
    const decoded = Data.from(datum, ProofChunkDatum) as unknown as {
      readonly proof_steps: readonly unknown[];
    };
    chunks.push({
      utxo: match,
      outRef: label,
      datumCbor: datum,
      stepCount: decoded.proof_steps.length,
    });
  }
  return chunks;
};

/**
 * Publishes a proof as chunk UTxOs at the prover's own key address.
 *
 * The UTxOs hold Ada only, carry no reference script, and are never spent by
 * the step that references them — a step builder must keep them out of both its
 * inputs and its collateral.
 */
export const publishProofChunks = async ({
  lucid,
  network,
  signer,
  proofCbor,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly proofCbor: string;
  /** Production workflow seam for the chunk publication transaction. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<PublishedProof> => {
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
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  const claimed = new Set<number>();
  const chunks: PublishedProofChunk[] = [];
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
