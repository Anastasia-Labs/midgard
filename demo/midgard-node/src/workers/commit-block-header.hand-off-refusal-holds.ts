import { Effect } from "effect";

import { IntentJournal } from "../services/intent-journal.js";
import type { NotifyCommitWorkerParent } from "./commit-block-header.commit-explicit-block-header-program.js";

/**
 * Hands the refusal holds a commit worker run's intent journal could not
 * write to the parent (I1-H1). The worker's journal ends with its thread, so
 * a hold left unwritten there would be lost: one more write is tried first,
 * then whatever is still unwritten goes to the parent in a notice ahead of
 * the output, and the parent's journal takes it over (`adopt`): `/readyz`
 * names it, and the parent's refresh at every tip writes it until it lands.
 * Without a parent (in-process runs), the run's journal keeps them.
 */
export const handOffUnwrittenRefusalHolds = (
  notifyParent: NotifyCommitWorkerParent | undefined,
): Effect.Effect<void, never, IntentJournal> =>
  Effect.gen(function* () {
    const journal = yield* IntentJournal;
    const pending = journal.handOff();
    if (pending.length === 0) return;
    journal.adopt(pending);
    yield* journal.refresh();
    if (notifyParent === undefined) return;
    const holds = journal.handOff();
    if (holds.length > 0)
      yield* notifyParent({ type: "IntentRefusalHoldsNotice", holds });
  });
