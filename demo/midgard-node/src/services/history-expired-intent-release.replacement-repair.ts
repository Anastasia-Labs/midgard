import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as MutationJobsDB from "../database/mutationJobs.js";
import type * as Pending from "../database/pendingBlockFinalizations.js";
import * as StateQueueLeases from "../database/stateQueueMutationLeases.js";
import {
  reviveReplacedCanonicalJournal,
  signedIntentReplacementDigest,
} from "./canonical-journal-recovery.js";
import { HistoryRecoverySuperseded } from "./event-history-recovery.js";
import type { rederiveDecision } from "./history-expired-intent-release.rederive-decision.js";
import type { Decision } from "./history-expired-intent-release.signed-commit-node.js";
import { C, failure } from "./history-expired-intent-release.table.js";
import { reincludeStateQueueCorrectedBlocks } from "./state-queue-correction-recovery.js";

/** The SQL repair of a signed-intent replacement or revival, run in the
 * retained plan's recovery transaction after its native CAS. */
export const replacementRepair = (input: {
  readonly decision: Extract<Decision, { kind: "replace" | "revive" }>;
  readonly record: Pending.Record;
  readonly current: (
    expected: Decision["kind"],
  ) => ReturnType<typeof rederiveDecision>;
  readonly context: string;
}) =>
  Effect.gen(function* () {
    const { decision, record, context } = input;
    const headerHash = record[C.HEADER_HASH];
    const header = headerHash.toString("hex");
    const replacementDigest = signedIntentReplacementDigest(record)!;
    const targetRoot = record[C.BASE_UTXOS_ROOT];
    const revived = decision.kind === "revive" ? decision.revived : undefined;
    const now = yield* input.current(decision.kind);
    const { parentAggregate } = now;
    // The displaced blocks are the ones this transaction's own re-derivation
    // names, so a block finalized or abandoned since the plan was prepared is
    // never missed or reopened twice; the winner must be the same one.
    const displaced =
      now.decision.kind === "revive" ? now.decision.displaced : [];
    if (
      revived !== undefined &&
      (now.decision.kind !== "revive" ||
        !now.decision.revived[C.HEADER_HASH].equals(revived[C.HEADER_HASH]))
    )
      return yield* Effect.fail(
        new HistoryRecoverySuperseded({
          message: `The replaced block revived over ${header} changed since its decision`,
        }),
      );
    const sql = yield* SqlClient.SqlClient;
    // Abandons the journal under its replacement digest and reopens every
    // member (L2 transactions, deposits, withdrawals, forced
    // transactions), restoring the speculative ledger, in this
    // transaction. Its signed content is kept. Each displaced block (sibling
    // first, then its descendants) is reopened from local finalization the
    // same way and abandoned under its own replacement digest, so it stays
    // revivable should it ever land after all; only then is no sibling on
    // the winner's base left locally finalized, which the revival requires.
    const reopened = [
      {
        headerHash: header,
        transitionDigest: replacementDigest,
        kind: "unlanded" as const,
      },
      ...(yield* Effect.forEach(displaced, (block) => {
        const digest = signedIntentReplacementDigest(block);
        return digest === undefined
          ? Effect.fail(
              failure(
                `Displaced block ${block[C.HEADER_HASH].toString("hex")} has no signed intent to abandon it under`,
              ),
            )
          : Effect.succeed({
              headerHash: block[C.HEADER_HASH].toString("hex"),
              transitionDigest: digest,
              kind: "displaced" as const,
            });
      })),
    ];
    const results = yield* reincludeStateQueueCorrectedBlocks(reopened);
    const missed = reopened.find(
      (_, index) =>
        results[index]?.journalFound !== true ||
        results[index]?.abandonedFromStatus === undefined,
    );
    if (results.length !== reopened.length || missed !== undefined)
      return yield* Effect.fail(
        failure(
          `Signed-intent replacement did not abandon ${missed?.headerHash ?? header}`,
        ),
      );
    yield* MutationJobsDB.abandonLocalBlockFinalization(
      headerHash,
      `${context} can no longer land: ${decision.kind === "replace" ? decision.cause : `replaced block ${revived![C.HEADER_HASH].toString("hex")} holds its base's slot`}`,
    );
    // A replaced block can never be continued; retire only its lease.
    yield* StateQueueLeases.release(record[C.STATE_QUEUE_LEASE_TOKEN]);
    // The SQL marker follows the native root the plan's CAS proved, from
    // the replaced journal's candidate root (or already at its target
    // when a resumed plan re-runs this repair) and nothing else.
    const engine = yield* sql`UPDATE mpf_engine_state
      SET root_hex = ${targetRoot},
        utxo_payload_entry_count = ${parentAggregate?.entryCount ?? null},
        utxo_payload_encoded_tuple_bytes = ${parentAggregate?.encodedTupleBytes ?? null},
        updated_at = NOW()
      WHERE store_name = 'ledger'
        AND root_hex IN (${record[C.EXPECTED_UTXOS_ROOT]}, ${targetRoot})
      RETURNING store_name`;
    if (engine.length !== 1)
      return yield* Effect.fail(
        failure(
          "Native SQL marker changed before the signed-intent replacement",
        ),
      );
    // The winner takes its members back; its SQL marker moves to its
    // candidate root and native replay follows at local finalization.
    if (revived !== undefined)
      yield* reviveReplacedCanonicalJournal(revived[C.HEADER_HASH]);
  });
