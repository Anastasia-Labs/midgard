import { createHash, randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as Authority from "../../src/database/eventHistoryAuthority.js";
import * as Journal from "../../src/database/eventHistoryJournal.js";
import type { EventHistorySourceBinding } from "../../src/l1-event-history-source.js";

/** Shared real SQL journal seed used by recovery and retention regressions. */
export const seedHistoryJournalFixture = async ({
  binding,
  initial,
  modelOriginReceipt,
  run,
}: {
  binding: EventHistorySourceBinding;
  initial: Journal.Checkpoint["capture"];
  modelOriginReceipt: string;
  run: <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) => Promise<A>;
}) => {
  const token = await run(
    Authority.acquire({
      deploymentIdentity: binding.manifestId,
      ownerToken: randomUUID(),
      leaseDurationMs: 60_000,
    }),
  );
  await run(
    Authority.withRecovery(
      token,
      Journal.seed({
        binding,
        capture: initial,
        height: 1,
        originReceipt: modelOriginReceipt,
        originReceiptDigest: createHash("sha256")
          .update(modelOriginReceipt)
          .digest("hex"),
        incarnations: [],
      }),
    ),
  );
  const checkpoint = await run(Journal.load(binding));
  if (checkpoint === null) throw new Error("Missing test checkpoint");
  return { token, checkpoint };
};
