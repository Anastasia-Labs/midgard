import {
  type NativeLedgerAuthority,
  NativeLedgerKupmios,
} from "@al-ft/midgard-core/native-reward-account";
import { type KupmiosOptions } from "@lucid-evolution/lucid";
import {
  deriveWatcherNativeGenesisIdentity,
  type WatcherNativeNodeConfig,
} from "midgard-watcher";

/** The journey devnet's local node, as the reward-account ledger query reads it. */
export type JourneyNativeNodeQuery = Readonly<{
  watcherConfig: WatcherNativeNodeConfig;
  binaryPath: string;
  timeoutMs: number;
}>;

const journeyNativeLedgerAuthority = async (
  input: JourneyNativeNodeQuery,
): Promise<NativeLedgerAuthority> => {
  const source = input.watcherConfig.l1.source;
  const identity = await deriveWatcherNativeGenesisIdentity(input);
  return {
    authorityNodeId: source.authorityNodeId,
    binaryPath: input.binaryPath,
    genesisIdentitySha256: identity.genesisIdentitySha256,
    network: input.watcherConfig.targetNetwork,
    networkMagic: identity.networkMagic,
    socketPath: source.chainSync.socketPath,
    timeoutMs: input.timeoutMs,
  };
};

/**
 * The journey devnet's Kupo/Ogmios transport, with reward-account state read
 * from the local ledger. Only the devnet journeys' own Lucid uses it; the
 * watcher reads L1 through its chain follower.
 */
export class JourneyLocalKupmios extends NativeLedgerKupmios {
  constructor(
    kupoUrl: string,
    ogmiosUrl: string,
    native: JourneyNativeNodeQuery,
    options?: KupmiosOptions,
  ) {
    super(
      kupoUrl,
      ogmiosUrl,
      () => journeyNativeLedgerAuthority(native),
      options,
    );
  }
}
