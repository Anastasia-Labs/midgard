/**
 * The L1 view each live Lucid client of the node reads through (N1): its
 * submit-slot snapshot (the local node's ledger tip against wall time) and
 * its view point (the follower's cursor, where every tracked read is
 * answered). `services/lucid.ts` registers both for the clients it builds;
 * an emulator client without a registration answers from its own chain.
 */
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import {
  isEmulatorProvider,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { L1ViewPoint } from "./services/l1-provider.js";

export type L1ProviderView = Readonly<{
  submitSlotSnapshot: () => Effect.Effect<SubmitSlotSnapshot, Error>;
  viewPoint: () => Effect.Effect<L1ViewPoint, Error>;
}>;

// One view per Lucid client, so separate clients (and tests) never share one.
const views = new WeakMap<LucidEvolution, L1ProviderView>();

/** Registers the view the given clients read through. */
export const registerL1ProviderView = (
  apis: readonly LucidEvolution[],
  view: L1ProviderView,
): void => {
  for (const api of apis) views.set(api, view);
};

/** The client's registered submit-slot snapshot reader, if any. */
export const registeredSubmitSlotSnapshot = (
  api: LucidEvolution,
): L1ProviderView["submitSlotSnapshot"] | undefined =>
  views.get(api)?.submitSlotSnapshot;

/**
 * The point the client's tracked reads are answered at: the follower's
 * cursor. An emulator client without a registration answers with its own
 * chain slot.
 */
export const providerViewPoint = (
  api: LucidEvolution,
): Effect.Effect<L1ViewPoint, Error> => {
  const view = views.get(api);
  if (view !== undefined) return view.viewPoint();
  const provider = api.config().provider;
  if (provider !== undefined && isEmulatorProvider(provider)) {
    const slot = api.currentSlot();
    return Effect.succeed({ slot, id: `emulator:${slot.toString()}` });
  }
  return Effect.fail(
    new Error("This Lucid client has no registered L1 view point"),
  );
};

/** One line of submit-slot evidence for logs and readiness. */
export const submitSlotEvidence = (snapshot: SubmitSlotSnapshot): string =>
  [
    `submitSlot=${snapshot.currentSlot.toString()}`,
    `slotSource=${snapshot.source}`,
    `observedAtMs=${snapshot.observedAtMs.toString()}`,
    ...(snapshot.ledgerTipSlot === undefined
      ? []
      : [`ledgerTipSlot=${snapshot.ledgerTipSlot.toString()}`]),
  ].join(",");
