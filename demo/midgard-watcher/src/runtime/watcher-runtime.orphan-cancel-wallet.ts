import { resolveProverSigner } from "@al-ft/midgard-fault-proofs";
import { Lucid, type Provider } from "@lucid-evolution/lucid";

import type { WatcherOrphanCancelInput } from "../funding/prover-funding-recovery.orphan-cancel.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  loadWatcherSecretText,
  type WatcherProcessConfig,
} from "./process-config.js";

/**
 * The prover wallet a cancel of orphaned funding signs with, and whether a
 * target can still run. The secret is resolved again for each cancel and
 * never held. A target is gone once the finalized queue no longer holds its
 * header; before the first observation, none is.
 */
export const watcherOrphanCancelInput = (
  input: Readonly<{
    config: WatcherProcessConfig;
    address: string;
    provider: Provider;
    inclusion: () => WatcherAuthenticatedStateQueueObservation | null;
  }>,
): WatcherOrphanCancelInput => ({
  wallet: {
    address: input.address,
    open: async () => {
      const {
        targetNetwork: network,
        customNetwork,
        proverWallet,
      } = input.config.watcherConfig;
      const lucid = await Lucid(
        input.provider,
        network,
        customNetwork?.slotConfig === undefined
          ? {}
          : { slotConfig: customNetwork.slotConfig },
      );
      const secret = await loadWatcherSecretText(proverWallet.keySource);
      const signer = resolveProverSigner(
        secret.startsWith("ed25519_sk")
          ? { network, walletPrivateKey: secret }
          : { network, walletSeedPhrase: secret },
        Object.freeze({}),
      );
      if (signer.address !== input.address)
        throw new Error("the prover wallet secret no longer names its address");
      signer.selectWallet(lucid);
      return {
        slotOf: (unixTimeMs) => lucid.unixTimeToSlot(unixTimeMs),
        timeOf: (slot) => lucid.slotToUnixTime(slot),
        sign: async (transaction) => await lucid.wallet().signTx(transaction),
      };
    },
    submit: async (signedTransactionCborHex) => {
      await input.provider.submitTx(signedTransactionCborHex);
    },
  },
  targetGone: ({ headerHash }) => {
    const observation = input.inclusion();
    return (
      observation !== null &&
      !observation.finalizedHeaders.some(
        (header) => header.headerHash === headerHash,
      )
    );
  },
});
