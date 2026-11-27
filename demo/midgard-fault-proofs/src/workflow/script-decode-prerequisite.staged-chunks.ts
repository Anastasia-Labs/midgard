/**
 * Where a script's staged chunks stand for the script-decode port: each
 * chunk is the plain output with its bytes found at the staging address
 * (the k-th copy for the k-th chunk holding them, whoever paid it, since
 * only the prover spends there), through the workflow's proof-materials view.
 */
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { scriptDecodeStagingDatums } from "../script-decode/submit.js";
import { findCarriage } from "./own-outputs.js";
import type { ProofMaterialsAccess } from "./proof-materials-view.js";

/**
 * Ready with the chunks in order; else the first chunk to publish. A chunk
 * of ours in our node's mempool is read as published.
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
}): Promise<
  | Readonly<{ kind: "ready"; utxos: readonly UTxO[] }>
  | Readonly<{ kind: "publish"; ordinal: number }>
> => {
  const found = await findCarriage({
    lucid,
    address,
    datums: scriptDecodeStagingDatums(scriptBytes),
    materials,
  });
  const ordinal = found.findIndex((utxo) => utxo === undefined);
  return ordinal < 0
    ? { kind: "ready", utxos: found as readonly UTxO[] }
    : { kind: "publish", ordinal };
};
