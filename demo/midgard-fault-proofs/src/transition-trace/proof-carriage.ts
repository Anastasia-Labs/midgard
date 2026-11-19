import { compactPlutusDataCarriageCbor } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  fieldPublicationClaimKey,
  lucidOwnOutputReader,
} from "../field-opening.own-field-carriage.js";
import {
  claimOwnOutputs,
  inlineDatumCarriageSlot,
  type OwnOutputJournal,
  resolveOwnOutputs,
} from "../workflow/own-outputs.js";
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
}): Promise<readonly UTxO[]> => {
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
}): Promise<readonly UTxO[]> => {
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
 * of `datums`, in order: the live output of ours `ownOutputs` claims for
 * datum `i` under its publication claim key (the key a workflow journals its
 * own chunk publications under). Only an ada-only output with the datum
 * inline serves: an output that also carries a reference script or other
 * assets holds the same bytes but makes the step that reads it pay for more
 * than the chunk. Another workflow's output with the same bytes is never
 * taken for ours. With `publish`, a chunk we hold no live output for is paid
 * to that address first and claimed.
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
}): Promise<readonly UTxO[]> => {
  const slots = datums.map((datum, index) => ({
    key: fieldPublicationClaimKey(index, datum),
    slot: inlineDatumCarriageSlot(address, datum),
  }));
  const published = new Set<number>();
  for (;;) {
    const resolved = await resolveOwnOutputs({
      slots: slots.map(({ key, slot }) => ({
        slot,
        claims: ownOutputs.claims(key),
      })),
      read: lucidOwnOutputReader(lucid),
    });
    if (resolved.kind === "ready") return resolved.utxos;
    if (!publish)
      throw new Error(
        "transition-trace proof carriage requires a journaled chunk publication",
      );
    if (published.has(resolved.ordinal))
      throw new Error("submitted transition-trace proof chunk disappeared");
    published.add(resolved.ordinal);
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
    await lucid.awaitTx(await signed.submit());
    ownOutputs.claim(key, claim!.outRef);
  }
};
