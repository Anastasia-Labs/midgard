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

  it.each(["prepared", "applied"])(
    "keeps every member of a retained %s recovery plan",
    async (state) => {
      const labels = ["plan-primary", "plan-member", "unrelated", "newest"];
      const result = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* seed(labels);
          yield* sql`INSERT INTO event_history_recovery_plans
        (recovery_id, binding_digest, manifest_id, header_hash, intent, evidence_digest,
          checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
        VALUES (${Buffer.alloc(32, 1)}, ${Buffer.alloc(32, 2)}, ${DEPLOYMENT},
          ${header("plan-primary")}, ${JSON.stringify({ members: [{ headerHash: header("plan-member").toString("hex") }] })},
          ${Buffer.alloc(32, 3)}, 0, ${Buffer.alloc(32, 4)}, ${Buffer.alloc(32, 5)}, 0, ${state})`;
          const removed = yield* prune();
          return { removed, kept: yield* remainingLabels(labels) };
        }),
      );
      expect(result.removed).toBe(1);
      expect(result.kept).toEqual(["plan-primary", "plan-member", "newest"]);
    },
  );

  it.each(["prepared", "applied"])(
    "keeps displaced journals named by a retained %s revival plan under this deployment",
    async (state) => {
      const labels = [
        "replay-base",
        "plan-primary",
        "displaced",
        "displaced-child",
        "unlisted-sibling",
        "unlisted-descendant",
        "unrelated",
        "foreign-named",
        "newest",
      ];
      const result = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* seed(labels);
          // Every row is finalized: the plan must itself seed the closure,
          // including an aged replay base and members not named by its intent.
          yield* sql`UPDATE pending_block_finalizations SET
            base_tail_header_hash = ${header("replay-base")}, base_tail_out_ref = 'replay-base#0'
            WHERE header_hash IN (${header("plan-primary")}, ${header("displaced")}, ${header("unlisted-sibling")})`;
          yield* sql`UPDATE pending_block_finalizations SET
            base_tail_header_hash = ${header("displaced")}
            WHERE header_hash = ${header("displaced-child")}`;
          yield* sql`UPDATE pending_block_finalizations SET
            base_tail_header_hash = ${header("displaced-child")}
            WHERE header_hash = ${header("unlisted-descendant")}`;
          const intent = {
            domain: "midgard-history-displaced-block-revival-intent-v1",
            bindingDigest: "02".repeat(32),
            manifestId: DEPLOYMENT.toString("hex"),
            headerHash: header("plan-primary").toString("hex"),
            signedTransactionHash: "03".repeat(32),
            signedTransactionCborSha256: "04".repeat(32),
            targetRoot: "05".repeat(32),
            journalDigest: "06".repeat(32),
            expectedRoot: "07".repeat(32),
            displacedHeaderHashes: ["displaced", "displaced-child"].map(
              (label) => header(label).toString("hex"),
            ),
          };
          yield* sql`INSERT INTO event_history_recovery_plans
            (recovery_id, binding_digest, manifest_id, header_hash, intent, evidence_digest,
              checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
            VALUES (${Buffer.alloc(32, 1)}, ${Buffer.alloc(32, 2)}, ${DEPLOYMENT},
              ${header("plan-primary")}, ${JSON.stringify(intent)},
              ${Buffer.alloc(32, 3)}, 0, ${Buffer.alloc(32, 4)}, ${Buffer.alloc(32, 5)}, 0, ${state})`;
          // Foreign authority names a local journal, but does not hold it.
          yield* sql`INSERT INTO event_history_recovery_plans
            (recovery_id, binding_digest, manifest_id, header_hash, intent, evidence_digest,
              checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
            VALUES (${Buffer.alloc(32, 8)}, ${Buffer.alloc(32, 9)}, ${Buffer.alloc(32, 10)},
              ${header("foreign-primary")}, ${JSON.stringify({ ...intent, displacedHeaderHashes: [header("foreign-named").toString("hex")] })},
              ${Buffer.alloc(32, 11)}, 0, ${Buffer.alloc(32, 12)}, ${Buffer.alloc(32, 13)}, 0, 'applied')`;
          const removed = yield* prune();
          return { removed, kept: yield* remainingLabels(labels) };
        }),
      );
      const prunable = ["unrelated", "foreign-named"];
      // An applied receipt binds only its named journals and their replay
      // bases; later normal finalized descendants may age out.
      if (state === "applied")
        prunable.push("unlisted-sibling", "unlisted-descendant");
      expect(result.removed).toBe(prunable.length);
      expect(result.kept).toEqual(
        labels.filter((label) => !prunable.includes(label)),
      );
    },
  );

  it.each(["prepared", "applied"])(
    "keeps the original provenance and exact members of a %s compensation plan",
    async (state) => {
      const labels = [
        "replay-base",
        "original-winner",
        "original-displaced",
        "prefix",
        "suffix",
        "removed-member",
        "later-child",
        "unrelated",
        "foreign-named",
        "newest",
      ];
      const result = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* seed(labels);
          yield* sql`UPDATE pending_block_finalizations SET
            base_tail_header_hash = ${header("replay-base")}, base_tail_out_ref = 'compensation-base#0'
            WHERE header_hash IN (${header("original-winner")}, ${header("original-displaced")}, ${header("prefix")})`;
          yield* sql`UPDATE pending_block_finalizations SET
            base_tail_header_hash = ${header("prefix")}
            WHERE header_hash = ${header("later-child")}`;
          const intent = {
            domain: "midgard-history-displacement-compensation-intent-v1",
            headerHash: header("prefix").toString("hex"),
            originalRecoveryId: "11".repeat(32),
            originalIntent: {
              headerHash: header("original-winner").toString("hex"),
              displacedHeaderHashes: [
                header("original-displaced").toString("hex"),
              ],
            },
            prefixHeaderHashes: [header("prefix").toString("hex")],
            suffixHeaderHashes: [header("suffix").toString("hex")],
            suffixMembers: [
              {
                headerHash: header("removed-member").toString("hex"),
                transitionDigest: "22".repeat(32),
                kind: "removed",
              },
            ],
          };
          yield* sql`INSERT INTO event_history_recovery_plans
            (recovery_id, binding_digest, manifest_id, header_hash, intent, evidence_digest,
              checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
            VALUES (${Buffer.alloc(32, 1)}, ${Buffer.alloc(32, 2)}, ${DEPLOYMENT},
              ${header("prefix")}, ${JSON.stringify(intent)},
              ${Buffer.alloc(32, 3)}, 0, ${Buffer.alloc(32, 4)}, ${Buffer.alloc(32, 5)}, 0, ${state})`;
          yield* sql`INSERT INTO event_history_recovery_plans
            (recovery_id, binding_digest, manifest_id, header_hash, intent, evidence_digest,
              checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
            VALUES (${Buffer.alloc(32, 8)}, ${Buffer.alloc(32, 9)}, ${Buffer.alloc(32, 10)},
              ${header("foreign-primary")}, ${JSON.stringify({ ...intent, originalIntent: { headerHash: header("foreign-named").toString("hex") } })},
              ${Buffer.alloc(32, 11)}, 0, ${Buffer.alloc(32, 12)}, ${Buffer.alloc(32, 13)}, 0, 'prepared')`;
          const removed = yield* prune();
          return { removed, kept: yield* remainingLabels(labels) };
        }),
      );
      const prunable = ["unrelated", "foreign-named"];
      if (state === "applied") prunable.push("later-child");
      expect(result.removed).toBe(prunable.length);
      expect(result.kept).toEqual(
        labels.filter((label) => !prunable.includes(label)),
      );
    },
  );

  it("ignores malformed displaced hash names instead of poisoning the prune batch", async () => {
    const labels = ["plan-primary", "unrelated", "newest"];
    const result = await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* seed(labels);
        yield* sql`INSERT INTO event_history_recovery_plans
          (recovery_id, binding_digest, manifest_id, header_hash, intent, evidence_digest,
            checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
          VALUES (${Buffer.alloc(32, 1)}, ${Buffer.alloc(32, 2)}, ${DEPLOYMENT},
            ${header("plan-primary")}, ${JSON.stringify({ displacedHeaderHashes: ["not-hex", "ff".repeat(32), null] })},
            ${Buffer.alloc(32, 3)}, 0, ${Buffer.alloc(32, 4)}, ${Buffer.alloc(32, 5)}, 0, 'applied')`;
        const removed = yield* prune();
        return { removed, kept: yield* remainingLabels(labels) };
      }),
    );
    expect(result.removed).toBe(1);
    expect(result.kept).toEqual(["plan-primary", "newest"]);
  });
});
