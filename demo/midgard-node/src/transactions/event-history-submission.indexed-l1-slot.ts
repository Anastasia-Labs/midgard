import type * as SDK from "@al-ft/midgard-sdk";
import { CML, type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { providerViewPoint } from "../l1-provider-view.js";

/** The slot through which the provider answering `observe` has indexed L1:
 * the follower's cursor once it reached the local node's ledger tip (an
 * emulator client's own chain slot). Undefined, with a warning, while it is
 * not available. */
export const indexedL1Slot = (lucid: LucidEvolution) =>
  providerViewPoint(lucid).pipe(
    Effect.map((point): number | undefined => point.slot),
    Effect.catchAll((cause) =>
      Effect.as(
        Effect.logWarning(
          `History submission cannot read the indexed L1 slot, so it treats every pending history transaction as still landable: ${String(cause)}`,
        ),
        undefined,
      ),
    ),
  );

/** A body whose validity ended below the indexed slot cannot land on this
 * chain once an observation made after reading that slot has not seen it;
 * another submission may since have taken over and spent its inputs. The
 * workflow keeps it as abandoned, so a rollback that lands it is adopted.
 * Undefined while the body may still land. */
export const settleExpiredHistoryAttempt = (
  lucid: LucidEvolution,
  attempt: SDK.EventHistorySubmissionAttempt,
  observe: (
    attempt: SDK.EventHistorySubmissionAttempt,
  ) => Promise<SDK.EventHistorySubmissionOutcome>,
) =>
  Effect.gen(function* () {
    const ttl = CML.Transaction.from_cbor_hex(attempt.transactionCbor)
      .body()
      .ttl();
    const indexed = ttl === undefined ? undefined : yield* indexedL1Slot(lucid);
    if (ttl === undefined || indexed === undefined || ttl >= BigInt(indexed))
      return undefined;
    const outcome = yield* Effect.tryPromise(() => observe(attempt));
    return outcome.kind === "Confirmed"
      ? outcome
      : ({ kind: "InputConflict" } as const);
  });
