import {
  type ReadNativeLedgerFile,
  readNativeLedgerGenesis,
} from "@al-ft/midgard-l1-follower/native-reward-account";

import type { WatcherConfig } from "../runtime/config.js";

export type WatcherNativeNodeConfig = Pick<
  WatcherConfig,
  "targetNetwork" | "customNetwork"
> & {
  readonly l1: Pick<WatcherConfig["l1"], "source">;
};

/**
 * The node identity this watcher admits: the Shelley genesis the node config
 * declares, read by the shared {@link readNativeLedgerGenesis}, must carry the
 * configured Custom network magic and epoch-zero slot clock, and its digest
 * must be the configured genesis identity.
 */
export const deriveWatcherNativeGenesisIdentity = async (input: {
  readonly watcherConfig: WatcherNativeNodeConfig;
  readonly unsafeReadIdentityFileForTest?: ReadNativeLedgerFile;
}): Promise<{ genesisIdentitySha256: string; networkMagic: number }> => {
  const { chainSync } = input.watcherConfig.l1.source;
  const { genesis, genesisIdentitySha256, networkMagic, nodeConfig } =
    await readNativeLedgerGenesis({
      network: input.watcherConfig.targetNetwork,
      nodeConfigPath: chainSync.nodeConfigPath,
      ...(input.unsafeReadIdentityFileForTest === undefined
        ? {}
        : { unsafeReadFileForTest: input.unsafeReadIdentityFileForTest }),
    });
  if (input.watcherConfig.targetNetwork === "Custom") {
    const custom = input.watcherConfig.customNetwork;
    if (custom === undefined || networkMagic !== custom.networkMagic)
      throw new Error(
        "node genesis network magic differs from watcher network",
      );
    const clock = custom.slotConfig;
    if (
      typeof genesis.systemStart !== "string" ||
      Date.parse(genesis.systemStart) !== clock.zeroTime ||
      typeof genesis.slotLength !== "number" ||
      genesis.slotLength * 1000 !== clock.slotLength ||
      clock.zeroSlot !== 0 ||
      genesis.networkId !== "Testnet" ||
      nodeConfig.TestShelleyHardForkAtEpoch !== 0 ||
      nodeConfig.TestConwayHardForkAtEpoch !== 0
    )
      throw new Error(
        "Custom network slot clock differs from epoch-zero testnet genesis",
      );
  }
  if (genesisIdentitySha256 !== chainSync.genesisIdentitySha256) {
    throw new Error("node genesis identity differs from watcher configuration");
  }
  return { genesisIdentitySha256, networkMagic };
};
