import type * as SDK from "@al-ft/midgard-sdk";
import { CML, type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { NodeConfig } from "../services/config.js";
import { resolvePreSubmitSlotSnapshot } from "./utils.submit-recovery-options.js";

/** The slot through which the provider answering `observe` has indexed L1:
 * the emulator's, or the configured Kupo's most recent checkpoint. Undefined,
 * with a warning, when neither is available. */
export const indexedL1Slot = (lucid: LucidEvolution) =>
  resolvePreSubmitSlotSnapshot(lucid).pipe(
    Effect.map((snapshot): number | undefined => snapshot.currentSlot),
    Effect.orElse(() =>
      Effect.flatMap(
        Effect.serviceOption(NodeConfig),
        Option.match({
          onNone: () => Effect.succeed(undefined),
          onSome: ({ L1_KUPO_KEY }) =>
            Effect.tryPromise(async () => {
              const response = await fetch(
                `${L1_KUPO_KEY.replace(/\/+$/u, "")}/health`,
                {
                  headers: { accept: "text/plain" },
                  signal: AbortSignal.timeout(10_000),
                },
              );
              const slot = Number(
                (await response.text()).match(
                  /^kupo_most_recent_checkpoint\s+([0-9]+(?:\.[0-9]+)?)/mu,
                )?.[1],
              );
              if (!response.ok || !Number.isSafeInteger(slot))
                throw new Error("Kupo reported no most recent checkpoint");
              return slot;
            }),
        }),
      ),
    ),
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
