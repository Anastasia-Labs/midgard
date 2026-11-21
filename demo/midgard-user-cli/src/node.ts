/**
 * What a withdrawal reads from L2, all from a node's public HTTP API through
 * lucid-midgard's provider: the deployment the node serves, the selected
 * output, and the status of the transaction that produced it.
 */
import { MidgardNodeProvider } from "@al-ft/lucid-midgard";
import { type Assets, type OutRef } from "@lucid-evolution/lucid";

export type UserNodeUtxo = {
  readonly address: string;
  readonly assets: Assets;
};

export type UserNode = {
  /** The manifest id the node reports, when it reports one. */
  readonly deploymentManifestId: () => Promise<string | undefined>;
  readonly utxoByOutRef: (outRef: OutRef) => Promise<UserNodeUtxo | undefined>;
  /** The exact status kind (`committed`, `not_found`, `accepted`, …). */
  readonly txStatus: (txHash: string) => Promise<string>;
};

export const connectMidgardNode = async (
  endpoint: string,
  fetchImpl: typeof fetch,
): Promise<UserNode> => {
  const provider = await MidgardNodeProvider.create({
    endpoint,
    fetch: fetchImpl,
  });
  return {
    deploymentManifestId: async () =>
      (await provider.getProtocolInfo()).deploymentMarker?.manifestId,
    utxoByOutRef: async (outRef) => {
      const utxo = await provider.getUtxoByOutRef(outRef);
      return utxo === undefined
        ? undefined
        : { address: utxo.output.address, assets: utxo.output.assets };
    },
    txStatus: async (txHash) => (await provider.getTxStatus(txHash)).kind,
  };
};
