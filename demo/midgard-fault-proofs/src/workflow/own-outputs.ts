/**
 * Carriage found by content. The data a step references (field
 * publications, transition-trace data, staged script-decode bytes) is a
 * plain output at the prover's proof-materials address holding exact inline
 * datum bytes. Only the prover spends there and the chain checks content
 * alone, so any such output serves, whoever paid it.
 *
 * Nothing about our own sends is kept: every call reads the address again,
 * through the workflow's proof-materials view when there is one (which
 * registers the datums as the workflow's needs, so no sweep takes what a
 * step found) and through the provider otherwise (the CLI and direct step
 * paths). A provider that applies the node's mempool finds an output of a
 * transaction we sent before it is in a block.
 */
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
  type ProofMaterialsAccess,
} from "./proof-materials-view.js";

/**
 * For each of `datums`, in order, a live plain output at `address` holding
 * exactly those bytes, or undefined: the k-th copy by out-ref for the k-th
 * occurrence of the same bytes.
 */
export const findCarriage = async ({
  lucid,
  address,
  datums,
  materials,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly address: string;
  readonly datums: readonly string[];
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}): Promise<readonly (UTxO | undefined)[]> =>
  materials !== undefined && materials.view.address === address
    ? await materials.view.find(datums, materials.holder)
    : await createProofMaterialsView({
        address,
        reader: lucidProofMaterialsReader(lucid, address),
      }).find(datums);
