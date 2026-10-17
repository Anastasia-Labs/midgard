import { compactPlutusDataCarriageCbor } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

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
  proof,
  publish = false,
}: {
  lucid: LucidEvolution;
  proof: TransitionProofInput;
  publish?: boolean;
}): Promise<readonly UTxO[]> => {
  return resolveTransitionTraceByteCarriage({
    lucid,
    chunks: transitionTraceProofChunks(proof).chunks,
    publish,
  });
};
export const transitionTraceByteChunks = (bytes: string) =>
  Array.from({ length: Math.ceil(bytes.length / 8192) }, (_, i) =>
    bytes.slice(i * 8192, (i + 1) * 8192),
  );
export const resolveTransitionTraceByteCarriage = async ({
  lucid,
  chunks,
  publish = false,
}: {
  lucid: LucidEvolution;
  chunks: readonly string[];
  publish?: boolean;
}): Promise<readonly UTxO[]> => {
  return resolveTransitionTraceDataCarriage({
    lucid,
    datums: chunks.map((chunk) => Data.to(chunk)),
    publish,
  });
};
export const resolveTransitionTraceDataCarriage = async ({
  lucid,
  datums,
  publish = false,
}: {
  lucid: LucidEvolution;
  datums: readonly string[];
  publish?: boolean;
}): Promise<readonly UTxO[]> => {
  const address = await lucid.wallet().address();
  let candidates = await lucid.utxosAt(address);
  const result: UTxO[] = [];
  for (const datum of datums) {
    let candidate = candidates.find((utxo) => utxo.datum === datum);
    if (candidate === undefined) {
      if (!publish)
        throw new Error(
          "transition-trace proof carriage requires a journaled chunk publication",
        );
      const signed = await (
        await lucid
          .newTx()
          .pay.ToAddressWithData(
            address,
            { kind: "inline", value: datum },
            { lovelace: 0n },
          )
          .complete({ localUPLCEval: true })
      ).sign
        .withWallet()
        .complete();
      await lucid.awaitTx(await signed.submit());
      candidates = await lucid.utxosAt(address);
      candidate = candidates.find((utxo) => utxo.datum === datum);
      if (candidate === undefined)
        throw new Error("submitted transition-trace proof chunk disappeared");
    }
    result.push(candidate);
  }
  return result;
};
