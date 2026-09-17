import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import type { WatcherNativeRewardAccountQuery } from "midgard-watcher";

export const journeyNativeNodeQuery = async (
  runDirectory: string,
): Promise<WatcherNativeRewardAccountQuery> => {
  const bytes = await readFile(
    join(runDirectory, "genesis/shelley-genesis.json"),
  );
  const genesis = JSON.parse(bytes.toString("utf8"));
  if (genesis.networkMagic !== 424242 || genesis.networkId !== "Testnet")
    throw new Error(
      "Journey native query requires its isolated devnet genesis",
    );
  return {
    binaryPath: join(runDirectory, "work/midgard-chain-sync"),
    timeoutMs: 10_000,
    watcherConfig: {
      targetNetwork: "Custom",
      customNetwork: {
        networkMagic: genesis.networkMagic,
        slotConfig: {
          zeroTime: Date.parse(genesis.systemStart),
          zeroSlot: 0,
          slotLength: genesis.slotLength * 1000,
        },
      },
      l1: {
        source: {
          sourceMode: "local_node",
          authorityNodeId: "watcher-journey-node",
          chainSync: {
            kind: "cardano_node_socket",
            socketPath: join(runDirectory, "cardano/ipc/node.socket"),
            nodeConfigPath: join(runDirectory, "config/config.json"),
            genesisConfigPath: join(
              runDirectory,
              "genesis/shelley-genesis.json",
            ),
            genesisIdentitySha256: createHash("sha256")
              .update(bytes)
              .digest("hex"),
          },
          queryServices: [],
        },
      },
    },
  };
};
