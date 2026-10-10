import {
  Lucid,
  type LucidEvolution,
  type LucidOptions,
  type Provider,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { registerL1TipSource } from "midgard-node/l1-heads";

import { readOgmiosTipSlot } from "./ledger-tip.js";

/** Where a journey Lucid runs: the devnet's Ogmios and slot mapping. */
export type JourneyLucidNetwork = {
  readonly ogmiosUrl: string;
  readonly customNetwork: {
    readonly slotConfig: NonNullable<LucidOptions["slotConfig"]>;
  };
};

/**
 * Registers the devnet's Ogmios tip as `lucid`'s heads tip source. Node code
 * a journey drives takes its L1 "now" from `l1SlotNow`, which a non-emulator
 * client answers only through a registered source; the harness has no
 * follower store, so the source is the chain tip itself.
 */
export const registerJourneyL1Tip = (
  lucid: LucidEvolution,
  { ogmiosUrl, customNetwork: { slotConfig } }: JourneyLucidNetwork,
): LucidEvolution => {
  registerL1TipSource(
    [lucid],
    () => Effect.tryPromise(() => readOgmiosTipSlot(ogmiosUrl)),
    { slotLengthMs: slotConfig.slotLength },
  );
  return lucid;
};

/** A live journey Lucid on the isolated devnet, with its tip source. */
export const journeyLucid = async (
  provider: Provider,
  network: JourneyLucidNetwork,
  evaluator?: LucidOptions["evaluator"],
): Promise<LucidEvolution> =>
  registerJourneyL1Tip(
    await Lucid(provider, "Custom", {
      slotConfig: network.customNetwork.slotConfig,
      ...(evaluator === undefined ? {} : { evaluator }),
    }),
    network,
  );
