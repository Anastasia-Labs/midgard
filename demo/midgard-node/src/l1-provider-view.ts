/**
 * The L1 view each live Lucid client reads through: its submit-slot snapshot
 * and its view point, both from the L1 access the client was built over
 * (`l1-access.ts`). An emulator client answers from its own chain.
 */
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import {
  isEmulatorProvider,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { l1AccessOf, type L1ViewPoint } from "./l1-access.js";

const asError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause), { cause });

/** The client's access submit-slot snapshot reader, if it has an access. */
export const accessSubmitSlotSnapshot = (
  api: LucidEvolution,
): (() => Effect.Effect<SubmitSlotSnapshot, Error>) | undefined => {
  const access = l1AccessOf(api);
  return access === undefined
    ? undefined
    : () =>
        Effect.tryPromise({ try: access.submitSlotSnapshot, catch: asError });
};

/**
 * The point the client's reads are answered at, synchronized with its
 * access's tip (the follower's cursor once it reached the node's tip; a
 * tool's freshly acquired ledger point). An emulator client answers with its
 * own chain slot.
 */
export const providerViewPoint = (
  api: LucidEvolution,
): Effect.Effect<L1ViewPoint, Error> => {
  const access = l1AccessOf(api);
  if (access !== undefined)
    return Effect.tryPromise({
      try: access.synchronizedViewPoint,
      catch: asError,
    });
  const provider = api.config().provider;
  if (provider !== undefined && isEmulatorProvider(provider)) {
    const slot = api.currentSlot();
    return Effect.succeed({ slot, id: `emulator:${slot.toString()}` });
  }
  return Effect.fail(
    new Error("This Lucid client is not built over an L1 access adapter"),
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
