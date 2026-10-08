/**
 * Worker-thread entry for tests/intent-journal-worker-holds.test.ts, bundled
 * from source and run in a real worker thread. It records intents through
 * the journal stack the commit and settlement workers provide
 * (`IntentJournalLive` over `Database.workerLayer` and `NodeConfig.layer`)
 * and reports each refusal's reason.
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

export type IntentJournalWorkerProbeInput = readonly Readonly<{
  family: NodeIntentFamily;
  workflowKey: string;
  signedTxCbor: string;
  txHash: string;
}>[];

/** Each record's refusal reason, or null when it was recorded. */
export type IntentJournalWorkerProbeResult = readonly (string | null)[];

if (parentPort !== null) {
  const port = parentPort;
  const input = workerData as IntentJournalWorkerProbeInput;
  const result = await Effect.runPromise(
    Effect.gen(function* () {
      const journal = yield* IntentJournal;
      const reasons: (string | null)[] = [];
      for (const { family, workflowKey, signedTxCbor, txHash } of input) {
        const outcome = yield* Effect.either(
          journal.record(
            journaledIntent(family, workflowKey),
            signedTxCbor,
            txHash,
          ),
        );
        reasons.push(Either.isLeft(outcome) ? outcome.left.reason : null);
      }
      return reasons;
    }).pipe(
      Effect.provide(IntentJournalLive),
      Effect.provide(Database.workerLayer),
      Effect.provide(NodeConfig.layer),
    ),
  );
  port.postMessage(result satisfies IntentJournalWorkerProbeResult);
}
