/**
 * Prepared recovery plans of the retired kinds (`retired-plans.ts`): a node
 * that prepared a signed-header recovery, a signed-intent release, a
 * displaced-block revival, a displacement compensation or a correction
 * rewind before those services were deleted keeps it until something
 * removes it. The landed-block rebase the follower-change driver runs does:
 * a retained one makes the rebase due, the rebase moves the native MPF to
 * the landed target from wherever the plan's restore left it, and discards
 * the plan in its SQL transaction. An applied receipt of any kind, and a
 * prepared plan of a domain no version wrote, stay as they were.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  CORRECTION_REWIND_RECOVERY_DOMAIN,
  DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
  DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN,
  SIGNED_HEADER_RECOVERY_DOMAIN,
  SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
} from "../src/database/eventHistoryRecoveryPlans.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import type { Globals } from "../src/services/globals.js";
import {
  attempt,
  BLOCK,
  E1,
  expectRebased,
  freshNative,
  hex,
  processOf,
  R0,
  R1,
  root,
  run,
  seed,
  sqlRun,
} from "./landed-blocks-rebase.fixture.js";

const RETIRED = [
  ["signed_header", SIGNED_HEADER_RECOVERY_DOMAIN],
  ["signed_intent_release", SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN],
  ["displaced_block_revival", DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN],
  ["displacement_compensation", DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN],
  ["correction_rewind", CORRECTION_REWIND_RECOVERY_DOMAIN],
] as const;

/** A domain no node version wrote: its plan is not a retired kind. */
const UNKNOWN_DOMAIN = "midgard-history-unknown-intent-v1";

const DEPLOYMENT = "de".repeat(32);
const BINDING = Buffer.alloc(32, 0x61);
const OTHER_BINDING = Buffer.alloc(32, 0x62);
const CHECKPOINT = Buffer.alloc(32, 0x63);

type Plan = Readonly<{
  id: Buffer;
  binding: Buffer;
  domain: string;
  state: "prepared" | "applied";
}>;

/** The cursor of `binding`, at revision 0 on `CHECKPOINT`. */
const cursor = (globals: Globals, binding: Buffer) =>
  sqlRun(
    globals,
    (sql) => sql`INSERT INTO event_history_cursor (binding_digest, manifest_id,
        origin_receipt, origin_receipt_digest, anchor_hash, anchor_slot,
        anchor_height, anchor_snapshot_digest, head_hash, head_slot,
        head_height, head_application_revision, snapshot_digest, revision,
        addresses)
      VALUES (${binding}, ${Buffer.from(DEPLOYMENT, "hex")}, 'origin',
        ${CHECKPOINT}, ${CHECKPOINT}, 0, 0, ${CHECKPOINT}, ${CHECKPOINT}, 0, 0,
        NULL, ${CHECKPOINT}, 0, '[]'::jsonb)`,
  );

/** A recovery plan row of `domain` for the seeded block. */
const plan = (globals: Globals, row: Plan) =>
  sqlRun(
    globals,
    (sql) => sql`INSERT INTO event_history_recovery_plans (recovery_id,
        binding_digest, manifest_id, header_hash, intent, evidence_digest,
        checkpoint_revision, head_hash, snapshot_digest, owner_generation,
        state)
      VALUES (${row.id}, ${row.binding}, ${Buffer.from(DEPLOYMENT, "hex")},
        ${Buffer.from(BLOCK, "hex")},
        ${JSON.stringify({ domain: row.domain, headerHash: BLOCK, expectedRoot: R0 })},
        ${CHECKPOINT}, 0, ${CHECKPOINT}, ${CHECKPOINT}, 0, ${row.state})`,
  );

const plans = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ recovery_id: Buffer; state: string }>`
        SELECT recovery_id, state FROM event_history_recovery_plans
        ORDER BY recovery_id`,
    ),
  ).then((rows) => rows.map((row) => [hex(row.recovery_id), row.state]));

/** Whether the landed-block rebase is due (and can run). */
const due = (globals: Globals) =>
  run(globals, rebasePlan).then((plan) => plan.kind);

/** A node whose working ledger already follows the landed block. */
const settled = async () => {
  const native = freshNative();
  const globals = await processOf(native);
  await seed(globals);
  expectRebased(await attempt(globals));
  await cursor(globals, BINDING);
  await cursor(globals, OTHER_BINDING);
  return { native, globals };
};

describe(
  "prepared recovery plans of the retired kinds",
  { concurrent: false },
  () => {
    it.each(RETIRED)(
      "discards a prepared %s plan and leaves other plans",
      async (_kind, domain) => {
        const { native, globals } = await settled();
        const retired: Plan = {
          id: Buffer.alloc(32, 0x01),
          binding: BINDING,
          domain,
          state: "prepared",
        };
        const receipt: Plan = {
          id: Buffer.alloc(32, 0x02),
          binding: BINDING,
          domain,
          state: "applied",
        };
        const unknown: Plan = {
          id: Buffer.alloc(32, 0x03),
          binding: OTHER_BINDING,
          domain: UNKNOWN_DOMAIN,
          state: "prepared",
        };
        for (const row of [retired, receipt, unknown]) await plan(globals, row);
        // Its native restore ran before the process stopped.
        native.durableRoot = root(0x55);

        expect(await due(globals)).toBe("ready");

        const shown = await attempt(globals);
        expect(shown.hold).toBeUndefined();
        expect(shown.due).toBe("none");
        expect(shown.working).toEqual([hex(E1.outref)]);
        // The native MPF is back on the landed target.
        expect(native.durableRoot).toBe(R1);
        expect(await plans(globals)).toEqual([
          [hex(receipt.id), "applied"],
          [hex(unknown.id), "prepared"],
        ]);
      },
    );

    it("does not make the rebase due for an applied receipt or a prepared plan of an unknown domain", async () => {
      const { native, globals } = await settled();
      const rows: Plan[] = [
        {
          id: Buffer.alloc(32, 0x02),
          binding: BINDING,
          domain: SIGNED_HEADER_RECOVERY_DOMAIN,
          state: "applied",
        },
        {
          id: Buffer.alloc(32, 0x03),
          binding: OTHER_BINDING,
          domain: UNKNOWN_DOMAIN,
          state: "prepared",
        },
      ];
      for (const row of rows) await plan(globals, row);
      // Nothing makes the rebase due, so the native root stays where it is.
      native.durableRoot = root(0x55);

      expect(await due(globals)).toBe("none");
      const shown = await attempt(globals);
      expect(shown.hold).toBeUndefined();
      expect(native.durableRoot).toBe(root(0x55));
      expect(await plans(globals)).toEqual(
        rows.map((row) => [hex(row.id), row.state]),
      );
    });
  },
);
