import { compactPlutusDataCarriageCbor } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { PrerequisiteTurn } from "../fault-proof-turn.js";
import { finishStep } from "../step-completion.js";
import { findCarriage } from "../workflow/own-outputs.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import {
  readTransitionProof,
  transitionProofCbor,
  type TransitionProofInput,
} from "./proof-material.js";

export const transitionTraceProofChunks = (proof: TransitionProofInput) => {
  const bytes = Buffer.from(
    compactPlutusDataCarriageCbor(transitionProofCbor(proof)),
    "hex",
  );
  return {
    hash: computeHash32(bytes).toString("hex"),
    chunks: Array.from({ length: Math.ceil(bytes.length / 4096) }, (_, i) =>
      bytes.subarray(i * 4096, (i + 1) * 4096).toString("hex"),
    ),
  };
};
/** Avoid duplicating large timing proofs in the route redeemer and output. */
export const transitionTraceTimedProofNeedsChunks = (
  proof: TransitionProofInput,
): boolean =>
  ("OmittedDueL1Event" in readTransitionProof(proof).fault ||
    "OutOfWindowSourceEvent" in readTransitionProof(proof).fault) &&
  transitionTraceProofChunks(proof).chunks.length > 1;

export const resolveTransitionTraceProofCarriage = async ({
  lucid,
  address,
  proof,
  materials,
  publish = false,
}: {
  lucid: LucidEvolution;
  /** The prover's proof-materials address. */
  address: string;
  proof: TransitionProofInput;
  /** The workflow's view of its proof-materials address, on a workflow. */
  materials?: ProofMaterialsAccess;
  publish?: boolean;
}): Promise<readonly UTxO[] | PrerequisiteTurn> => {
  return resolveTransitionTraceByteCarriage({
    lucid,
    address,
    chunks: transitionTraceProofChunks(proof).chunks,
    materials,
    publish,
  });
};
export const transitionTraceByteChunks = (bytes: string) =>
  Array.from({ length: Math.ceil(bytes.length / 8192) }, (_, i) =>
    bytes.slice(i * 8192, (i + 1) * 8192),
  );
export const resolveTransitionTraceByteCarriage = async ({
  lucid,
  address,
  chunks,
  materials,
  publish = false,
}: {
  lucid: LucidEvolution;
  /** The prover's proof-materials address. */
  address: string;
  chunks: readonly string[];
  /** The workflow's view of its proof-materials address, on a workflow. */
  materials?: ProofMaterialsAccess;
  publish?: boolean;
}): Promise<readonly UTxO[] | PrerequisiteTurn> => {
  return resolveTransitionTraceDataCarriage({
    lucid,
    address,
    datums: chunks.map((chunk) => Data.to(chunk)),
    materials,
    publish,
  });
};
/**
 * The carriage at `address`, the prover's proof-materials address, for each
 * of `datums`, in order, found by content (`findCarriage`): the k-th plain
 * output holding datum `i`'s bytes for the k-th occurrence, whoever paid it.
 * With `publish`, the first missing chunk is returned as a publication turn
 * that pays it to that address; without it, a missing chunk is an error.
 */
export const resolveTransitionTraceDataCarriage = async ({
  lucid,
  address,
  datums,
  materials,
  publish = false,
}: {
  lucid: LucidEvolution;
  address: string;
  datums: readonly string[];
  materials?: ProofMaterialsAccess;
  publish?: boolean;
}): Promise<readonly UTxO[] | PrerequisiteTurn> => {
  const found = await findCarriage({ lucid, address, datums, materials });
  const missing = found.findIndex((utxo) => utxo === undefined);
  if (missing < 0) return found as readonly UTxO[];
  if (!publish)
    throw new Error(
      "transition-trace proof carriage requires a published chunk",
    );
  const signed = await (
    await lucid
      .newTx()
      .pay.ToAddressWithData(
        address,
        { kind: "inline", value: datums[missing]! },
        { lovelace: 0n },
      )
      .complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  return {
    kind: "publication",
    transaction: finishStep(signed, { referenceScripts: [] }),
    label: "transition-trace proof chunk publication",
    plan: {
      key: `transition-trace-data:${computeHash32(
        Buffer.from(datums.join(""), "hex"),
      ).toString("hex")}`,
      transactions: datums.length,
    },
  };
};
