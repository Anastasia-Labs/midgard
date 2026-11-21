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
} from "@al-ft/midgard-sdk/core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { isAdaOnlyInlineDatumOutput } from "./ada-only-carriage-output.js";
import {
  type PublishedProofChunk,
  splitProofIntoChunkDatums,
} from "./proof-chunk-carriage.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
} from "./spend-input-witness.js";
import { completeStep, finishStep, fundStep } from "./step-completion.js";
import type { LocallyEvaluatedTransaction } from "./workflow/transaction-boundary.js";
export * from "./proof-chunk-carriage.js";
import {
  type ProofMaterialsHolder,
  type ProofMaterialsView,
  proofMaterialsViewFor,
} from "./workflow/proof-materials-view.js";
export { MAXIMUM_CHUNK_PROOF_STEP_COUNT, MAXIMUM_PROOF_CHUNK_COUNT };

export type PublishedProof = {
  readonly txHash: string;
  readonly chunks: readonly PublishedProofChunk[];
  readonly proofStepCount: number;
};

const publishedChunk = (utxo: UTxO, datum: string): PublishedProofChunk => {
  const decoded = Data.from(datum, ProofChunkDatum) as unknown as {
    readonly proof_steps: readonly unknown[];
  };
  return {
    utxo,
    outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
    datumCbor: datum,
    stepCount: decoded.proof_steps.length,
  };
};

/**
 * The chunks of `proofCbor` live at the proof-materials `address`, found by
 * content through the process's view: for each chunk datum, a plain output
 * with exactly those bytes, the same copy every workflow picks; undefined
 * unless every chunk has one. Whoever paid a copy, only the prover can spend
 * it, and the step checks content alone.
 */
export const resolvePublishedProofChunks = async ({
  lucid,
  address,
  proofCbor,
  view,
  holder,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly proofCbor: string;
  /** The process's view; the address read through `lucid` without one. */
  readonly view?: ProofMaterialsView;
  /** The workflow whose step needs the chunks, until it is released. */
  readonly holder: ProofMaterialsHolder;
}): Promise<readonly PublishedProofChunk[] | undefined> => {
  const datums = splitProofIntoChunkDatums(proofCbor);
  if (datums.length === 0) return [];
  const found = await proofMaterialsViewFor({ view, lucid, address }).find(
    datums,
    holder,
  );
  const chunks: PublishedProofChunk[] = [];
  for (const [index, datum] of datums.entries()) {
    const utxo = found[index];
    if (utxo === undefined) return undefined;
    chunks.push(publishedChunk(utxo, datum));
  }
  return chunks;
};

/**
 * The chunks of `proofCbor` among the outputs of the one publication
 * `txHash`: what reading that transaction back checks it produced.
 */
export const resolvePublicationProofChunks = async ({
  lucid,
  address,
  proofCbor,
  txHash,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly proofCbor: string;
  readonly txHash: string;
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
        utxo.txHash === txHash &&
        !claimed.has(label) &&
        isAdaOnlyInlineDatumOutput(utxo) &&
        canonicalPlutusDataCbor(utxo.datum) === datum
      );
    });
    if (match === undefined) return undefined;
    claimed.add(`${match.txHash}#${match.outputIndex.toString()}`);
    chunks.push(publishedChunk(match, datum));
  }
  return chunks;
};

/**
 * Builds and signs the transaction that publishes a proof as chunk UTxOs at
 * the prover's proof-materials address.
 *
 * The UTxOs hold Ada only, carry no reference script, and are never spent by
 * the step that references them — a step builder must keep them out of both its
 * inputs and its collateral.
 */
export const buildProofChunkPublication = async ({
  lucid,
  signer,
  proofCbor,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly proofCbor: string;
}): Promise<{ readonly transaction: LocallyEvaluatedTransaction }> => {
  const datums = splitProofIntoChunkDatums(proofCbor);
  if (datums.length === 0) {
    throw new Error(
      "A zero-step proof has nothing to publish; carry it in the step redeemer instead.",
    );
  }
  const address = signer.proofMaterialsAddress;
  const { coinsPerUtxoByte } = await resolveProtocolParameters(lucid);

  const funding = await fundStep(lucid, { signer });
  let tx = funding.tx;
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
  const signed = await completeStep(tx, funding);
  return { transaction: finishStep(signed, { referenceScripts: [] }) };
};

/**
 * Reads back the chunks of `proofCbor` that the publication `txHash` paid to
 * the prover's proof-materials address, each once and in proof order, refusing a
 * chunk without an inline datum or with a reference script.
 */
export const readPublishedProofChunks = async ({
  lucid,
  signer,
  proofCbor,
  txHash,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly proofCbor: string;
  readonly txHash: string;
}): Promise<PublishedProof> => {
  const datums = splitProofIntoChunkDatums(proofCbor);
  const address = signer.proofMaterialsAddress;
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
  };
};
