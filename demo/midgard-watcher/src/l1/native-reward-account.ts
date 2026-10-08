import {
  type NativeLedgerAuthority,
  NativeLedgerKupmios,
  queryNativeRewardAccount,
} from "@al-ft/midgard-core/native-reward-account";
import {
  type KupmiosOptions,
  type RewardAccountState,
} from "@lucid-evolution/lucid";

import {
  deriveWatcherNativeGenesisIdentity,
  type WatcherNativeNodeConfig,
} from "./native-chain-sync.derive-watcher-native-genesis-identity.js";

export type WatcherNativeRewardAccountQuery = Readonly<{
  watcherConfig: WatcherNativeNodeConfig;
  binaryPath: string;
  timeoutMs: number;
}>;

/** Query registration, rewards and pool delegation at one acquired node snapshot. */
export const queryWatcherNativeRewardAccount = async (
  input: WatcherNativeRewardAccountQuery,
  rewardAddress: string,
): Promise<RewardAccountState> =>
  await queryNativeRewardAccount(
    await watcherNativeLedgerAuthority(input),
    rewardAddress,
  );

const watcherNativeLedgerAuthority = async (
  input: WatcherNativeRewardAccountQuery,
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

/** Kupo/Ogmios transport with reward-account state obtained from the local ledger. */
export class WatcherLocalKupmios extends NativeLedgerKupmios {
  constructor(
    kupoUrl: string,
    ogmiosUrl: string,
    native: WatcherNativeRewardAccountQuery,
    options?: KupmiosOptions,
  ) {
    super(
      kupoUrl,
      ogmiosUrl,
      () => watcherNativeLedgerAuthority(native),
      options,
    );
  }
}
