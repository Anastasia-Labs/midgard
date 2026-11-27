/**
 * The node rows of the events a processed foreign landed block names
 * (`recordNamedEvents`), written in the transaction that records the block
 * from the follower facts its replay checked: the deposits and withdrawals
 * through follower ingestion's own insert (`ingestKind`), the forced orders
 * through the forced-order row insert, each screened as the forced-order
 * hook screens the rows it writes.
 */
import type { ProjectedEvent } from "@al-ft/midgard-l1-follower/events";
import { Effect } from "effect";

import type { ForcedAdmissionDivergences } from "../forced-orders/admission-divergence.js";
import { screenForcedAdmissions } from "../forced-orders/entry.js";
import type { UserEventEntry, UserEventReading } from "../l1-events/entries.js";
import { unrecordedNamedEvents } from "../landed-blocks/store.js";
import { ingestKind } from "./follower-events.js";
import * as ForcedTransactions from "./forcedTransactions.js";
import { sqlErrorToDatabaseError } from "./utils/common.js";

/** A deposit or withdrawal as the projection reads it at a view, decoded. */
export type ReadEvent = Readonly<{
  event: ProjectedEvent;
  entry: UserEventEntry;
}>;

/**
 * The events a replayed foreign block names, from the follower facts its
 * replay checked them against: the deposits and withdrawals as read at the
 * view, and the forced orders' rows (`forcedOrderEntry`).
 */
export type NamedEvents = Readonly<{
  events: readonly ReadEvent[];
  forced: readonly ForcedTransactions.Entry[];
}>;

/**
 * Writes the node rows of the events a processed landed block names, in
 * the caller's transaction, after the block's row: through ingestion's
 * inserts, so each row is the one ingestion writes from the same fact and
 * whichever write comes second leaves it as it is. A retired event is
 * written too, as the block that names it is processed: once its
 * retirement is pruned from the follower, nothing else could. An id that
 * already has a row keeps it, whatever admission wrote it, as ingestion
 * keeps it. A forced row written here goes through the admission screen
 * (`screenForcedAdmissions`, naming its divergence in `divergences`): the
 * forced-order hook then finds the row and never screens it.
 */
export const recordNamedEvents = (
  named: NamedEvents,
  divergences?: ForcedAdmissionDivergences,
) =>
  Effect.gen(function* () {
    const entries = new Map(
      named.events.map(({ event, entry }) => [event.idCbor, entry] as const),
    );
    const read = (event: ProjectedEvent): UserEventReading => ({
      kind: "readable",
      entry: entries.get(event.idCbor)!,
    });
    for (const kind of ["deposit", "withdrawal"] as const)
      yield* ingestKind(
        kind,
        named.events
          .filter(({ event }) => event.kind === kind)
          .map(({ event }) => event),
        read,
      );
    if (named.forced.length === 0) return;
    const lacking = yield* unrecordedNamedEvents("forced");
    const forced = named.forced.filter((entry) =>
      lacking.has(
        entry[ForcedTransactions.Columns.TX_ORDER_ID].toString("hex"),
      ),
    );
    yield* ForcedTransactions.insertEntries(forced);
    yield* screenForcedAdmissions(forced, divergences);
  }).pipe(
    sqlErrorToDatabaseError(
      "follower_event_ingestion",
      "Failed to record named events",
    ),
  );
