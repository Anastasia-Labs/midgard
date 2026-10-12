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
 * - its commit anchor is on the follower's chain (`false` when it has none
 *   or a rewind removed it) and the view is at least d blocks above it: a
 *   shallower view, after a rewind, waits under
 *   `INTENT_COMMIT_ANCHOR_NOT_DEEP` and is wanted again once the chain
 *   regrows.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { INTENT_COMMIT_ANCHOR_NOT_DEEP } from "../src/services/l1-follower.intent-predicates.js";
import { IntentPredicateWait } from "../src/services/l1-follower.intents.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  anchorAt,
  BLOCK,
  journalRow,
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

/** The scenario's commit-event depth (`deps().commitEventDepth`). */
const D = 2;

/** The follower block d below the current view: a fresh commit's anchor. */
const freshAnchor = async (s: PredicateScenario) =>
  anchorAt(s, (await viewHeight(s)) - D);

/**
 * The last upper validity slot the scenario's Unattested head accepts an
 * append through: POSIX time counts from the simulated origin at one second
 * per slot, the head's header ends at 2000 ms, and the inclusive upper bound
 * is the exclusive slot bound's time minus one.
 */
const LAST_APPEND_SLOT =
  SIM_ORIGIN.point.slot +
  Number((2_000n + SDK.DA_ATTESTATION_TIMEOUT_MS) / 1_000n);

/**
 * This operator holds the shift and the block journal row is live, anchored
 * d below the view.
 */
const ownShift = async (s: PredicateScenario): Promise<void> => {
  await s.land(s.lists.insert(s.live(), "active", OWN));
  await s.land(s.lists.shift(s.live(), OWN, 0n));
  await journalRow("pending_submission", await freshAnchor(s));
};

const setAnchor = (anchor: { hash: Buffer; height: number; slot: number }) =>
  db(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`UPDATE pending_block_finalizations
        SET commit_anchor_hash = ${anchor.hash},
          commit_anchor_height = ${anchor.height},
          commit_anchor_slot = ${anchor.slot}`,
    ),
  );

/**
 * `ownShift`, then d + 1 empty blocks, with the journal re-anchored d below
 * the view: a rewind of up to d + 1 blocks removes no landed transaction.
 */
const anchoredAboveEmptyBlocks = async (s: PredicateScenario) => {
  await ownShift(s);
  for (let i = 0; i <= D; i += 1) await s.driver.forward([]);
  await setAnchor(await freshAnchor(s));
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
  expect((error as IntentPredicateWait).reason).toBe(
    INTENT_COMMIT_ANCHOR_NOT_DEEP,
  );
};

describe("the node's commit predicate over the follower's projections", () => {
  it("wanted while its journal row, tail, shift and commit anchor hold; each failing one is false", async () => {
    const s = await open();
    await s.land(s.lists.insert(s.live(), "active", OWN));
    await s.land(s.lists.shift(s.live(), OWN, 0n));
    const commit = await commitOn(s, s.head);

    // No journal row for the header: not wanted.
    expect(await s.verdict(commit)).toBe(false);
    // A journal row without a commit anchor: not wanted.
    await journalRow("pending_submission", null);
    expect(await s.verdict(commit)).toBe(false);
    // Anchored d below the view.
    const height = await viewHeight(s);
    await setAnchor(await anchorAt(s, height - D));
    expect(await s.verdict(commit)).toBe(true);
    // An anchor fewer than d blocks below the view: a wait, not false.
    await setAnchor(await anchorAt(s, height - D + 1));
    await waits(s, commit);
    // An anchor the follower does not hold at its height: false, and not
    // canonical wins over shallow.
    const other = { hash: Buffer.alloc(32, 0x5a), height: height - D, slot: 1 };
    await setAnchor(other);
    expect(await s.verdict(commit)).toBe(false);
    await setAnchor({ ...other, height: height - D + 1 });
    expect(await s.verdict(commit)).toBe(false);
    await setAnchor(await anchorAt(s, height - D));
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

  it("a commit whose anchor a rewind leaves fewer than d blocks deep waits, and is wanted again once the chain regrows", async () => {
    const s = await open();
    await anchoredAboveEmptyBlocks(s);
    const commit = await commitOn(s, s.head);
    expect(await s.verdict(commit)).toBe(true);
    // The rewind drops one block above the anchor; the anchor stays.
    await s.driver.backward(1);
    await waits(s, commit);
    // The chain regrows d blocks above the anchor.
    await s.driver.forward([]);
    expect(await s.verdict(commit)).toBe(true);
  });

  it("a commit whose anchor a rewind removes is false, also once the chain regrows", async () => {
    const s = await open();
    await anchoredAboveEmptyBlocks(s);
    const commit = await commitOn(s, s.head);
    expect(await s.verdict(commit)).toBe(true);
    // The rewind drops the anchor block and the d blocks above it.
    await s.driver.backward(D + 1);
    for (let i = 0; i <= D + 1; i += 1) await s.driver.forward([]);
    expect(await s.verdict(commit)).toBe(false);
    // The same commit anchored on the new chain is wanted.
    await setAnchor(await freshAnchor(s));
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
