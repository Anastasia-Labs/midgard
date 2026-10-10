import type {
  LucidEvolution,
  LucidOptions,
  Provider,
} from "@lucid-evolution/lucid";
import { type L1Access, l1AccessOfProvider } from "midgard-node/l1-access";
import { openKupmiosAccess } from "midgard-node/l1-external/kupmios-access";

/** Where a journey Lucid runs: the devnet's Kupo, Ogmios and slot mapping. */
export type JourneyLucidNetwork = {
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly customNetwork: {
    readonly slotConfig: NonNullable<LucidOptions["slotConfig"]>;
  };
};

/**
 * The harness's L1 access (option E): it is a tool, so it reads L1 through
 * the Kupmios adapter over its own provider, whose tip (the clock node code
 * a journey drives reads through `l1SlotNow`) is the devnet's Ogmios tip.
 * The adapter is opened once per provider; every Lucid built over that
 * provider afterwards, by the harness or by a capture test, has its clock.
 */
export const journeyL1Access = (
  provider: Provider,
  { kupoUrl, ogmiosUrl, customNetwork: { slotConfig } }: JourneyLucidNetwork,
): L1Access =>
  l1AccessOfProvider(provider) ??
  openKupmiosAccess({
    network: "Custom",
    kupoUrl,
    ogmiosUrl,
    provider,
    slotConfig,
  });

/** A live journey Lucid on the isolated devnet, over the harness access. */
export const journeyLucid = async (
  provider: Provider,
  network: JourneyLucidNetwork,
  evaluator?: LucidOptions["evaluator"],
): Promise<LucidEvolution> =>
  await journeyL1Access(provider, network).lucid(
    "Custom",
    evaluator === undefined ? {} : { evaluator },
  );
