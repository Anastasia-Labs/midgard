/**
 * Worker-thread entry for tests/intent-journal-worker-holds.test.ts, bundled
 * from source and run in a real worker thread. It records intents through
 * the journal stack the commit and settlement workers provide
 * (`IntentJournalLive` over `Database.workerLayer` and `NodeConfig.layer`)
 * and reports each refusal's reason. With `handOff`, it then ends as a
 * commit worker run does (`handOffUnwrittenRefusalHolds`), and reports the
 * notices the run posts to its parent.
 */
import { parentPort, workerData } from "node:worker_threads";

import { Effect, Either } from "effect";

import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
import {
  IntentJournal,
  IntentJournalLive,
  journaledIntent,
  type NodeIntentFamily,
} from "../../src/services/intent-journal.js";
import { handOffUnwrittenRefusalHolds } from "../../src/workers/commit-block-header.hand-off-refusal-holds.js";
import type { IntentRefusalHoldsNotice } from "../../src/workers/utils/commit-block-header.js";

export type IntentJournalWorkerProbeInput = Readonly<{
  records: readonly Readonly<{
    family: NodeIntentFamily;
    workflowKey: string;
    signedTxCbor: string;
    txHash: string;
  }>[];
  handOff?: boolean;
}>;

export type IntentJournalWorkerProbeResult = Readonly<{
  /** Each record's refusal reason, or null when it was recorded. */
  reasons: readonly (string | null)[];
  /** The notices the run posted to its parent (with `handOff`). */
  notices: readonly IntentRefusalHoldsNotice[];
}>;

if (parentPort !== null) {
  const port = parentPort;
  const input = workerData as IntentJournalWorkerProbeInput;
  const result = await Effect.runPromise(
    Effect.gen(function* () {
      const journal = yield* IntentJournal;
      const reasons: (string | null)[] = [];
      const notices: IntentRefusalHoldsNotice[] = [];
      for (const {
        family,
        workflowKey,
        signedTxCbor,
        txHash,
      } of input.records) {
        const plan = yield* journal.openPlan;
        const outcome = yield* Effect.either(
          journal.record(
            journaledIntent(family, workflowKey, plan),
            signedTxCbor,
            txHash,
            { kind: "record_only" },
          ),
        );
        reasons.push(Either.isLeft(outcome) ? outcome.left.reason : null);
      }
      if (input.handOff === true)
        yield* handOffUnwrittenRefusalHolds((notice) =>
          Effect.sync(() => {
            if (notice.type === "IntentRefusalHoldsNotice")
              notices.push(notice);
          }),
        );
      return { reasons, notices };
    }).pipe(
      Effect.provide(IntentJournalLive),
      Effect.provide(Database.workerLayer),
      Effect.provide(NodeConfig.layer),
    ),
  );
  port.postMessage(result satisfies IntentJournalWorkerProbeResult);
}
