/**
 * A Kupmios provider whose reward-account state comes from the local node's
 * ledger (tools only: the prover CLI, journeys, SDK users). Role processes
 * read L1 through the follower and never import this module (the role
 * boundary test pins it).
 */
import {
  Kupmios,
  type KupmiosOptions,
  type RewardAccountState,
} from "@lucid-evolution/lucid";

import {
  type NativeLedgerAuthority,
  queryNativeRewardAccount,
} from "./native-reward-account.js";

/**
 * Kupo/Ogmios transport whose reward-account state comes from the local
 * ledger. Without a configured authority it refuses reward-account reads:
 * Ogmios would report every undelegated registered account as absent.
 */
export class NativeLedgerKupmios extends Kupmios {
  readonly #authority: () => Promise<NativeLedgerAuthority | undefined>;

  constructor(
    kupoUrl: string,
    ogmiosUrl: string,
    authority: () => Promise<NativeLedgerAuthority | undefined>,
    options?: KupmiosOptions,
  ) {
    super(kupoUrl, ogmiosUrl, options);
    this.#authority = authority;
  }

  override async getRewardAccount(
    rewardAddress: string,
  ): Promise<RewardAccountState> {
    const authority = await this.#authority();
    if (authority === undefined)
      throw new Error(
        "Reward-account state requires a local node ledger: Ogmios omits registered accounts that have no stake-pool delegation. Configure the node socket, node config and node transport binary.",
      );
    return await queryNativeRewardAccount(authority, rewardAddress);
  }
}
