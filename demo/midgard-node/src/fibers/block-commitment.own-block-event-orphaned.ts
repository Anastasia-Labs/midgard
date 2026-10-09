import { Effect, Ref } from "effect";

import {
  describePoisonedOwnHeaders,
  readPoisonedOwnHeaders,
} from "../database/poisoned-own-headers.js";
import { Globals } from "../services/index.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";

/** The source `l1_own_block_event_orphaned` is raised under. */
export const OWN_BLOCK_EVENT_ORPHANED_SOURCE = "own_block_event_orphaned";

/** A landed own block includes a deposit or withdrawal whose L1 admission
 * left the chain (`database/poisoned-own-headers.ts`): no block is
 * committed, and that block and the landed blocks built on it are not
 * merged, until the header leaves the landed queue (a landed correction or
 * a rollback that removes it). Admissions, older merges, DA and the
 * follower continue. It clears on the first commitment tick after. */
export const L1_OWN_BLOCK_EVENT_ORPHANED = "l1_own_block_event_orphaned";

/**
 * Raises or clears `l1_own_block_event_orphaned` from the poisoned own
 * headers, and answers whether this commitment tick plans nothing. A pending
 * local finalization recovery still runs: it builds no block. A failed read
 * plans nothing this tick and leaves the reason as it is; the next tick
 * reads again. Never fails.
 */
export const refuseCommitForOrphanedOwnBlockEvent = Effect.gen(function* () {
  const globals = yield* Globals;
  const read = yield* Effect.either(readPoisonedOwnHeaders);
  if (read._tag === "Left") {
    yield* Effect.logWarning(
      "Could not read the own landed blocks whose events left the chain; this commitment tick plans nothing.",
      read.left,
    );
    return true;
  }
  const poisoned = read.right;
  if (poisoned.orphaned.length === 0) {
    yield* clearLivenessIncident(globals, OWN_BLOCK_EVENT_ORPHANED_SOURCE);
    return false;
  }
  yield* raiseLivenessIncident(
    globals,
    OWN_BLOCK_EVENT_ORPHANED_SOURCE,
    L1_OWN_BLOCK_EVENT_ORPHANED,
    `${describePoisonedOwnHeaders(poisoned)}; every commit holds until a landed correction or a rollback removes the header`,
  );
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const localFinalizationBlock = yield* Ref.get(
    globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
  );
  return !(localFinalizationPending && localFinalizationBlock !== "");
});
