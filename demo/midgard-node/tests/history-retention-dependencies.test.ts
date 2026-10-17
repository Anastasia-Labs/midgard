import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { pruneFinalizedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import {
  DAY_MS,
  DEPLOYMENT,
  journals,
  remainingLabels,
  run,
} from "./history-retention-prune.fixtures.js";
import { header } from "./local-mutation-job-abandonment.journal-fixture.js";

const prune = () =>
  pruneFinalizedBeyondChallengeability({
    challengeableCutoff: new Date(Date.now() - 15 * DAY_MS),
    view: {
      confirmedHeadHash: header("foreign-head"),
      liveQueueHeaderHashes: [],
    },
    deploymentIdentityDigest: DEPLOYMENT,
    batchLimit: 1,
  });

/** Distinct bases unless a test explicitly links them. */
const seed = (labels: readonly string[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* journals(
      labels.map((label, index) => ({
        label,
        status: "locally_applied" as const,
        endedAgoMs: (40 - index) * DAY_MS,
      })),
    );
    yield* sql`UPDATE pending_block_finalizations SET
      base_tail_out_ref = encode(header_hash, 'hex') || '#0',
      base_tail_header_hash = substring(sha256(header_hash) from 1 for 28)`;
  });

describe("journal retention dependencies", () => {
  it.each(["pending_submission", "abandoned"])(
    "keeps the base, alternate-incarnation sibling and descendants needed by a %s journal",
    async (status) => {
      const labels = [
        "base",
        "unfinished",
        "sibling",
        "child",
        "grandchild",
        "unrelated",
        "newest",
      ];
      const result = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* seed(labels);
          yield* sql`UPDATE pending_block_finalizations SET
          base_tail_header_hash = ${header("base")}, base_tail_out_ref = 'base#0'
          WHERE header_hash = ${header("unfinished")}`;
          yield* sql`UPDATE pending_block_finalizations SET status = ${status}
          WHERE header_hash = ${header("unfinished")}`;
          // Same non-root header/root, a different node incarnation/outref.
          yield* sql`UPDATE pending_block_finalizations SET
          base_tail_header_hash = ${header("base")}, base_tail_out_ref = 'base#1'
          WHERE header_hash = ${header("sibling")}`;
          for (const [child, parent] of [
            ["child", "sibling"],
            ["grandchild", "child"],
          ])
            yield* sql`UPDATE pending_block_finalizations SET base_tail_header_hash = ${header(parent!)}
            WHERE header_hash = ${header(child!)}`;
          const removed = yield* prune();
          return { removed, kept: yield* remainingLabels(labels) };
        }),
      );
      expect(result.removed).toBe(1);
      expect(result.kept).toEqual(
        labels.filter((label) => label !== "unrelated"),
      );
    },
  );
});
