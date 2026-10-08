/**
 * The node's commit predicate over the follower's projections (I1), each
 * condition in both polarities, on the node database:
 *
 * - its block journal row is live, the tail it spends is still the landed
 *   tail, and the scheduler names this operator at the lower validity bound;
 *   each failing condition is `false`;
 * - the queue's head allows the append (the Q61 fence,
 *   `state-queue.ak:137-158`): a Challenged head, a head with a completed
 *   fraud proof, or an Unattested head whose timeout boundary the inclusive
 *   upper validity bound reaches is `false`;
 * - every included event is canonical (`false` otherwise) and at least d
 *   blocks below the view: a shallower one, after a rewind, waits under
 *   `INTENT_EVENTS_NOT_DEEP` and is wanted again once the chain regrows; a
 *   forced order's admission height is held to the same horizon.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { INTENT_EVENTS_NOT_DEEP } from "../src/services/l1-follower.intent-predicates.js";
import { IntentPredicateWait } from "../src/services/l1-follower.intents.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  BLOCK,
  eventIdOf,
  forcedOrder,
  journalRow,
  listEvent,
  member,
  viewHeight,
} from "./helpers/intent-predicates-commit.js";
import {
  FOREIGN,
  openPredicateScenario,
  outRefText,
  OWN,
  type PredicateScenario,
} from "./helpers/intent-predicates-scenario.js";
import {
  loadOperatorSetChainFixture,
  type OperatorSetChainFixture,
} from "./helpers/operator-set-chain.js";
import {
  NODE_PREFIX,
  nodeDatum,
  queueOutput,
} from "./helpers/state-queue-sim.fixtures.js";

const opened: FactStore[] = [];
let fixture: OperatorSetChainFixture;
beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const open = () => openPredicateScenario(fixture, opened);

/**
 * The last upper validity slot the scenario's Unattested head accepts an
 * append through: POSIX time counts from the simulated origin at one second
 * per slot, the head's header ends at 2000 ms, and the inclusive upper bound
 * is the exclusive slot bound's time minus one.
 */
const LAST_APPEND_SLOT =
  SIM_ORIGIN.point.slot +
  Number((2_000n + SDK.DA_ATTESTATION_TIMEOUT_MS) / 1_000n);

/** This operator holds the shift and the block journal row is live. */
const ownShift = async (s: PredicateScenario): Promise<void> => {
  await s.land(s.lists.insert(s.live(), "active", OWN));
  await s.land(s.lists.shift(s.live(), OWN, 0n));
  await journalRow("pending_submission");
};

/** A commit appending to `tail`, valid through `invalidAfter` (none when null). */
const commitOn = (
  s: PredicateScenario,
  tail: { txHash: Buffer; index: number },
  invalidAfter: number | null = LAST_APPEND_SLOT,
) =>
  s.record(
    "commit",
    `commit:tail=${outRefText(tail)}`,
    s.spend([tail], invalidAfter === null ? {} : { invalidAfter }),
    BLOCK,
  );

/** Replaces the queue's only node with one carrying `status` (and `provenFraud`). */
const restatusHead = async (
  s: PredicateScenario,
  status: SDK.DaAvailabilityStateQueueStatus,
  provenFraud: string | null = null,
) => {
  const [txHash] = await s.driver.forward([
    {
      inputs: [s.head],
      outputs: [
        queueOutput(
          NODE_PREFIX + s.firstHash,
          nodeDatum(s.first, status, null, provenFraud),
        ),
      ],
      nonce: s.nonce(),
    },
  ]);
  return { txHash: txHash!, index: 0 };
};

const waits = async (s: PredicateScenario, hash: Buffer): Promise<void> => {
  const error = await s.verdict(hash).then(
    () => null,
    (cause: unknown) => cause,
  );
  expect(error).toBeInstanceOf(IntentPredicateWait);
  expect((error as IntentPredicateWait).reason).toBe(INTENT_EVENTS_NOT_DEEP);
};

describe("the node's commit predicate over the follower's projections", () => {
  it("wanted while its journal row, tail, shift and included events hold; each failing one is false", async () => {
    const s = await open();
    await s.land(s.lists.insert(s.live(), "active", OWN));
    await s.land(s.lists.shift(s.live(), OWN, 0n));
    const commit = await commitOn(s, s.head);

    // No journal row for the header: not wanted.
    expect(await s.verdict(commit)).toBe(false);
    await journalRow("pending_submission");
    expect(await s.verdict(commit)).toBe(true);

    // An included deposit, canonical and d below the view.
    const eventId = eventIdOf(0x31);
    const { key, origin } = await listEvent(s, "deposit", eventId, 1);
    await member("pending_block_finalization_deposits", eventId, {
      key,
      origin,
    });
    expect(await s.verdict(commit)).toBe(true);
    // Admitted within d of the view: a wait, not false.
    const height = await viewHeight(s);
    await s.sql("UPDATE node_l1_events SET admitted_height = ?", [height - 1]);
    await waits(s, commit);
    await s.sql("UPDATE node_l1_events SET admitted_height = ?", [height - 2]);
    expect(await s.verdict(commit)).toBe(true);
    // No longer canonical: the admission key is gone.
    await s.sql("DELETE FROM l1_event_keys");
    expect(await s.verdict(commit)).toBe(false);
    // Not canonical wins over shallow.
    await s.sql("UPDATE node_l1_events SET admitted_height = ?", [height - 1]);
    expect(await s.verdict(commit)).toBe(false);
    await s.sql("UPDATE node_l1_events SET admitted_height = ?", [height - 2]);
    await s.sql(
      "INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot) VALUES (?, ?, ?, ?)",
      ["deposit", key, origin, 0],
    );
    expect(await s.verdict(commit)).toBe(true);

    // A forced member with no forced row: false.
    await member(
      "pending_block_finalization_forced_transactions",
      Buffer.alloc(32, 0x44),
      null,
    );
    expect(await s.verdict(commit)).toBe(false);
    await s.sql("DELETE FROM pending_block_finalization_forced_transactions");
    expect(await s.verdict(commit)).toBe(true);

    // The scheduler hands the shift to another operator: false.
    await s.land(s.lists.insert(s.live(), "active", FOREIGN));
    await s.land(s.lists.shift(s.live(), FOREIGN, 0n));
    expect(await s.verdict(commit)).toBe(false);
    await s.land(s.lists.shift(s.live(), OWN, 0n));
    expect(await s.verdict(commit)).toBe(true);
    // Its lower validity bound before the shift's start (the appointment
    // the node submitted just before the commit): still wanted.
    expect(await s.verdict(commit, { slotToPosixMs: () => -1 })).toBe(true);
    // Its lower validity bound past the shift: false.
    expect(
      await s.verdict(commit, {
        slotToPosixMs: () => Number(SDK.SHIFT_DURATION_MS),
      }),
    ).toBe(false);

    // The journal row abandoned: false.
    await db(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) =>
          sql`UPDATE pending_block_finalizations SET status = 'abandoned'`,
      ),
    );
    expect(await s.verdict(commit)).toBe(false);
  });

  it("a tail that is no longer the landed tail is false", async () => {
    const s = await open();
    await ownShift(s);
    const stale = await commitOn(s, s.root);
    expect(await s.verdict(stale)).toBe(false);
    // The tail named but not spent: false.
    const unspent = await s.record(
      "commit",
      `commit:tail=${outRefText(s.head)}`,
      s.spend([s.spare(0)], { invalidAfter: LAST_APPEND_SLOT }),
      BLOCK,
    );
    expect(await s.verdict(unspent)).toBe(false);
  });

  it("an included event a rewind leaves shallow waits, and is wanted again once the chain regrows", async () => {
    const s = await open();
    await ownShift(s);
    await s.driver.forward([]);
    await s.driver.forward([]);
    const commit = await commitOn(s, s.head);
    const eventId = eventIdOf(0x32);
    const identity = await listEvent(
      s,
      "deposit",
      eventId,
      (await viewHeight(s)) - 2,
    );
    await member("pending_block_finalization_deposits", eventId, identity);
    expect(await s.verdict(commit)).toBe(true);
    // The rewind drops one block; the event stays admitted, now within d.
    await s.driver.backward(1);
    await waits(s, commit);
    // The chain regrows past the horizon.
    await s.driver.forward([]);
    expect(await s.verdict(commit)).toBe(true);
  });

  it("a forced order admitted within d of the view waits, and is wanted once it is d deep", async () => {
    const s = await open();
    await ownShift(s);
    const commit = await commitOn(s, s.head);
    const memberId = Buffer.alloc(32, 0x45);
    await member(
      "pending_block_finalization_forced_transactions",
      memberId,
      null,
    );
    const height = await viewHeight(s);
    await forcedOrder(s, memberId, Buffer.alloc(32, 0x46), height - 1);
    await waits(s, commit);
    await s.sql("UPDATE node_l1_forced_order_fields SET height = ?", [
      height - 2,
    ]);
    expect(await s.verdict(commit)).toBe(true);
  });

  it("an Unattested head allows the append only through an inclusive upper bound before its timeout boundary", async () => {
    const s = await open();
    await ownShift(s);
    expect(await s.verdict(await commitOn(s, s.head))).toBe(true);
    expect(
      await s.verdict(await commitOn(s, s.head, LAST_APPEND_SLOT + 1)),
    ).toBe(false);
    // An inclusive upper bound exactly at the boundary is refused; one
    // millisecond before it is allowed.
    const boundary = 2_000 + Number(SDK.DA_ATTESTATION_TIMEOUT_MS);
    const until = LAST_APPEND_SLOT + 2;
    const timeTo =
      (upperMs: number) =>
      (slot: number): number =>
        slot === until ? upperMs : (slot - SIM_ORIGIN.point.slot) * 1000;
    const exact = await commitOn(s, s.head, until);
    expect(
      await s.verdict(exact, { slotToPosixMs: timeTo(boundary + 1) }),
    ).toBe(false);
    expect(await s.verdict(exact, { slotToPosixMs: timeTo(boundary) })).toBe(
      true,
    );
    // No upper bound: the validator requires a closed range.
    expect(await s.verdict(await commitOn(s, s.head, null))).toBe(false);
  });

  const COMMITMENT = "c1".repeat(32);

  it("a Challenged head refuses the append", async () => {
    const s = await open();
    await ownShift(s);
    const challenged = await restatusHead(s, {
      Challenged: {
        commitment_hash: COMMITMENT,
        challenge_asset_name: "c2".repeat(32),
      },
    });
    expect(await s.verdict(await commitOn(s, challenged))).toBe(false);
  });

  it("an Attested head allows the append past the unattested timeout boundary", async () => {
    const s = await open();
    await ownShift(s);
    const attested = await restatusHead(s, {
      Attested: { commitment_hash: COMMITMENT },
    });
    expect(
      await s.verdict(await commitOn(s, attested, LAST_APPEND_SLOT + 1)),
    ).toBe(true);
  });

  it("a head with a completed fraud proof refuses the append", async () => {
    const s = await open();
    await ownShift(s);
    const fraudulent = await restatusHead(
      s,
      { Attested: { commitment_hash: COMMITMENT } },
      "f0".repeat(32),
    );
    expect(await s.verdict(await commitOn(s, fraudulent))).toBe(false);
  });
});
