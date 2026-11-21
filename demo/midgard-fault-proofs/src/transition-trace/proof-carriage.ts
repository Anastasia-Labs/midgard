import { compactPlutusDataCarriageCbor } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { PrerequisiteTurn } from "../fault-proof-turn.js";
import {
  fieldPublicationClaimKey,
  lucidOwnOutputReader,
} from "../field-opening.own-field-carriage.js";
import { finishStep } from "../step-completion.js";
import {
  claimOwnOutputs,
  inlineDatumCarriageSlot,
  type OwnOutputJournal,
  resolveOwnOutputs,
} from "../workflow/own-outputs.js";
import { withContentFoundCarriage } from "../workflow/proof-materials-view.carriage-claims.js";
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
  ownOutputs,
  publish = false,
}: {
  lucid: LucidEvolution;
  /** The prover's proof-materials address. */
  address: string;
  proof: TransitionProofInput;
  /** Our claims on the chunks. */
  ownOutputs: OwnOutputJournal;
  publish?: boolean;
}): Promise<readonly UTxO[] | PrerequisiteTurn> => {
  return resolveTransitionTraceByteCarriage({
    lucid,
    address,
    chunks: transitionTraceProofChunks(proof).chunks,
    ownOutputs,
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
  ownOutputs,
  publish = false,
}: {
  lucid: LucidEvolution;
  /** The prover's proof-materials address. */
  address: string;
  chunks: readonly string[];
  /** Our claims on the chunks. */
  ownOutputs: OwnOutputJournal;
  publish?: boolean;
}): Promise<readonly UTxO[] | PrerequisiteTurn> => {
  return resolveTransitionTraceDataCarriage({
    lucid,
    address,
    datums: chunks.map((chunk) => Data.to(chunk)),
    ownOutputs,
    publish,
  });
};
/**
 * The carriage at `address`, the prover's proof-materials address, for each
 * of `datums`, in order: the output with datum `i`'s bytes that the
 * workflow's proof-materials view finds (the k-th copy for the k-th
 * occurrence, whoever paid it, since only the prover can spend it), or else
 * the live output `ownOutputs` claims for datum `i` under its publication
 * claim key (the key a workflow claims its own chunk publications under).
 * Only an ada-only output with the datum inline serves: an output that also
 * carries a reference script or other assets holds the same bytes but makes
 * the step that reads it pay for more than the chunk. With `publish`, the
 * first chunk we hold no live output for
 * is claimed and returned as a publication turn that pays it to that address;
 * without it, a missing chunk is an error.
 */
export const resolveTransitionTraceDataCarriage = async ({
  lucid,
  address,
  datums,
  ownOutputs,
  publish = false,
}: {
  lucid: LucidEvolution;
  address: string;
  datums: readonly string[];
  ownOutputs: OwnOutputJournal;
  publish?: boolean;
}): Promise<readonly UTxO[] | PrerequisiteTurn> => {
  const slots = datums.map((datum, index) => ({
    key: fieldPublicationClaimKey(index, datum),
    slot: inlineDatumCarriageSlot(address, datum),
  }));
  const claimed = await withContentFoundCarriage(
    ownOutputs,
    slots.map(({ key }, index) => ({
      key,
      address,
      datumCbor: datums[index]!,
    })),
  );
  const resolved = await resolveOwnOutputs({
    slots: slots.map(({ key, slot }) => ({
      slot,
      claims: claimed.claims(key),
    })),
    read: lucidOwnOutputReader(lucid),
  });
  if (resolved.kind === "ready") return resolved.utxos;
  if (!publish)
    throw new Error(
      "transition-trace proof carriage requires a claimed chunk publication",
    );
  const { key, slot } = slots[resolved.ordinal]!;
  const signed = await (
    await lucid
      .newTx()
      .pay.ToAddressWithData(
        address,
        { kind: "inline", value: datums[resolved.ordinal]! },
        { lovelace: 0n },
      )
      .complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  const [claim] = claimOwnOutputs({ signed, slots: [slot] });
  ownOutputs.claim(key, claim!.outRef);
  const label = "transition-trace proof chunk publication";
  return {
    kind: "publication",
    transaction: finishStep(signed, { referenceScripts: [] }),
    label,
    output: { outRef: claim!.outRef, label },
    plan: {
      key: `transition-trace-data:${computeHash32(
        Buffer.from(datums.join(""), "hex"),
      ).toString("hex")}`,
      transactions: datums.length,
    },
  };
};
