import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { retrieveCorrectionObserverJournalDependencies } from "../src/database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import { recordMergeJob } from "./history-retention-prune.fixtures.js";
import {
  header,
  isolatedDb,
  journalFixture,
  observedJournal,
} from "./local-mutation-job-abandonment.journal-fixture.js";

describe("correction observer journal dependencies", () => {
  it.effect(
    "keeps finalized recovery siblings and abandoned bases while releasing unrelated settled journals",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const settled = header("dependency:settled");
          yield* observedJournal(settled);
          yield* PendingBlockFinalizationsDB.markFinalized(settled);
          yield* recordMergeJob(settled, "completed");
          yield* sql`UPDATE pending_block_finalizations
            SET base_tail_header_hash = ${header("unrelated-base")},
                base_tail_out_ref = 'unrelated#0',
                submitted_tx_hash = ${Buffer.concat([settled, Buffer.alloc(4)])}
            WHERE header_hash = ${settled}`;
          const active = header("dependency:active");
          const abandoned = header("dependency:abandoned");
          const finalized = header("dependency:finalized");
          // One journal is active at a time, and finalizing one deletes the
          // unsubmitted abandoned siblings on its base: finalize first.
          yield* observedJournal(finalized);
          yield* PendingBlockFinalizationsDB.markFinalized(finalized);
          yield* recordMergeJob(finalized, "completed");
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            journalFixture(abandoned),
          );
          yield* PendingBlockFinalizationsDB.markAbandoned(abandoned);
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            journalFixture(active),
          );

          const dependencies =
            yield* retrieveCorrectionObserverJournalDependencies;
          const base = {
            baseTailHeaderHash: header("base-tail").toString("hex"),
            baseTailOutRef: "base#0",
          };
          expect(dependencies).toEqual(
            [
              { headerHash: active.toString("hex"), ...base, abandoned: false },
              {
                headerHash: finalized.toString("hex"),
                ...base,
                abandoned: false,
              },
              {
                headerHash: abandoned.toString("hex"),
                ...base,
                abandoned: true,
              },
            ].sort((left, right) =>
              // bytea order is the hex code-unit order.
              left.headerHash < right.headerHash ? -1 : 1,
            ),
          );
        }),
      ),
  );
});
