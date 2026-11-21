/**
 * Where a script's staged chunks stand for the script-decode port: each
 * chunk is the plain output with its bytes that the workflow's
 * proof-materials view finds at the staging address (the k-th copy for the
 * k-th chunk holding them, whoever paid it, since only the prover spends
 * there), read back by its out-ref with the strict carriage shape.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import { scriptDecodeStagingDatums } from "../script-decode/submit.js";
import { inlineDatumCarriageSlot, resolveOwnOutputs } from "./own-outputs.js";
import type { ProofMaterialsAccess } from "./proof-materials-view.js";

/**
 * Ready with the chunks in order; else the first chunk to publish. A chunk
 * of ours that landed and is not yet visible stays in the process's
 * in-flight set, which holds the workflow until the read catches up.
 */
export const scriptDecodeStagedChunks = async ({
  lucid,
  address,
  scriptBytes,
  materials,
}: {
  readonly lucid: LucidEvolution;
  /** The staging address: the prover's own. */
  readonly address: string;
  readonly scriptBytes: Uint8Array;
  /** The workflow's view of the staging address and the workflow itself. */
  readonly materials: ProofMaterialsAccess;
}) => {
  const datums = scriptDecodeStagingDatums(scriptBytes);
  const found =
    address === materials.view.address
      ? await materials.view.find(datums, materials.holder)
      : [];
  return await resolveOwnOutputs({
    slots: datums.map((datum, chunkIndex) => {
      const own = found[chunkIndex];
      return {
        slot: inlineDatumCarriageSlot(address, datum),
        claims:
          own === undefined
            ? []
            : [{ outRef: `${own.txHash}#${own.outputIndex.toString()}` }],
      };
    }),
    read: (outRefs) => lucid.utxosByOutRef([...outRefs]),
  });
};
