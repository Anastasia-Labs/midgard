import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { l1NowUnixTimeMs } from "../l1-heads.js";
import { canonicalSlotConfigForLucid } from "../lucid-time.js";
import {
  awaitLedgerTipAtLowerBound,
  observedLedgerTip,
} from "../transactions/ledger-tip-lower-bound.js";

/**
 * The validity interval of an availability command's transaction
 * (`SDK.daAvailabilityValidityWindow`). The command submits straight through
 * its provider, past the node's submit check, so the interval must be
 * admissible at the observed ledger tip when it is built. It asks for the
 * full lookahead of room past the L1 clock (`minimumRoomSlots`), so the
 * window never anchors on a lagging tip: an anchored interval ends no later
 * than the lookahead, so it never runs that far unless it is the lookahead's
 * own. While the tip has not reached the lower bound of the interval running
 * the lookahead past the clock (with one-second slots and neither limit, it
 * trails the L1 clock by more than 178 slots; or `lowerLimitMs` is past the
 * last block), the command waits for the next block, as the node's one-shot
 * commands do, rather than submit an interval that ends soon after the
 * clock, and takes the interval again from the clock read after the wait.
 * An interval the window leaves no room for, or one that ends before the L1
 * clock (no block can still include it), is refused.
 */
export const availabilityValidityWindow = (
  lucid: LucidEvolution,
  label: string,
  limits: SDK.DaAvailabilityValidityLimits = {},
): Promise<{ readonly validFrom: bigint; readonly validTo: bigint }> => {
  const atTip = <A>(read: Effect.Effect<A, unknown>) =>
    Effect.mapError(
      read,
      (cause) =>
        new Error(
          `${label} needs the observed L1 ledger tip: ${formatUnknownError(cause)}`,
          { cause },
        ),
    );
  return Effect.runPromise(
    Effect.gen(function* () {
      for (;;) {
        const tip = yield* atTip(observedLedgerTip(lucid));
        const nowMs = yield* atTip(l1NowUnixTimeMs(lucid));
        const window = SDK.daAvailabilityValidityWindow({
          lucid,
          tip,
          nowMs,
          limits,
          minimumRoomSlots: Math.ceil(
            SDK.DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS /
              canonicalSlotConfigForLucid(lucid).slotLength,
          ),
        });
        if (window.status === "refused")
          return yield* Effect.fail(new Error(`${label}: ${window.detail}`));
        if (window.status === "ready")
          return { validFrom: window.validFrom, validTo: window.validTo };
        yield* atTip(
          awaitLedgerTipAtLowerBound(lucid, window.validFrom, label),
        );
      }
    }),
  );
};
