/**
 * The node's §8.4 family predicates over the follower's projections (I1),
 * each in both polarities, on the node database:
 *
 * - merge: the header it names is the queue's head, and it spends the head
 *   and the root;
 * - attestation: the header it names is landed;
 * - correction: the removed header is landed and the target is still
 *   unattested and timed out at the lower validity bound;
 * - payout absorb/initialize: the event it names is listed and not
 *   retired; fund/conclude: every input is still a live fact.
 *
 * The commit predicate is in `l1-follower-intent-predicates-commit.test.ts`,
 * the operator-set transitions in
 * `l1-follower-intent-predicates-operators.test.ts`, and the reference-script
 * and stake-registration families in
 * `l1-follower-intent-predicates-wallet.test.ts`.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { eventIdOf, listEvent } from "./helpers/intent-predicates-commit.js";
import { openPredicateScenario } from "./helpers/intent-predicates-scenario.js";
import {
  loadOperatorSetChainFixture,
  type OperatorSetChainFixture,
} from "./helpers/operator-set-chain.js";

const opened: FactStore[] = [];
let fixture: OperatorSetChainFixture;
beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const open = () => openPredicateScenario(fixture, opened);

describe("the node's §8.4 predicates over the follower's projections", () => {
  it("merge: wanted while its header is the head and it spends the head and the root", async () => {
    const s = await open();
    const header = Buffer.from(s.firstHash, "hex");
    const merge = await s.record(
      "merge",
      `merge:head=${s.firstHash}`,
      s.spend([s.root, s.head]),
      header,
    );
    expect(await s.verdict(merge)).toBe(true);
    const other = await s.record(
      "merge",
      "merge:head=other",
      s.spend([s.root, s.head], { nonce: s.nonce() }),
      Buffer.alloc(28, 0xee),
    );
    expect(await s.verdict(other)).toBe(false);
    const headOnly = await s.record(
      "merge",
      `merge:head=${s.firstHash}:head-only`,
      s.spend([s.head]),
      header,
    );
    expect(await s.verdict(headOnly)).toBe(false);
  });

  it("attest: wanted while its header is landed", async () => {
    const s = await open();
    const landed = await s.record(
      "attest",
      `attest:${s.firstHash}:apply`,
      s.spend([s.spare(0)]),
      Buffer.from(s.firstHash, "hex"),
    );
    expect(await s.verdict(landed)).toBe(true);
    const absent = await s.record(
      "attest",
      "attest:absent:apply",
      s.spend([s.spare(1)]),
      Buffer.alloc(28, 0xee),
    );
    expect(await s.verdict(absent)).toBe(false);
  });

  it("correction: wanted while the removed header is landed and the target is unattested and timed out", async () => {
    const s = await open();
    const deadline = Number(s.first.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS);
    const correction = await s.record(
      "correction",
      `correction:${s.firstHash}:remove-last:${s.firstHash}`,
      s.spend([s.head, s.spare(0)]),
      Buffer.from(s.firstHash, "hex"),
    );
    expect(await s.verdict(correction, { slotToPosixMs: () => deadline })).toBe(
      true,
    );
    expect(
      await s.verdict(correction, { slotToPosixMs: () => deadline - 1 }),
    ).toBe(false);
    const gone = await s.record(
      "correction",
      `correction:${s.firstHash}:prune-descendant:${"ee".repeat(28)}`,
      s.spend([s.spare(1)]),
      Buffer.alloc(28, 0xee),
    );
    expect(await s.verdict(gone, { slotToPosixMs: () => deadline })).toBe(
      false,
    );
  });

  it("payout absorb/initialize: wanted while the event is listed and not retired", async () => {
    const s = await open();
    const deposit = eventIdOf(0x51);
    const withdrawal = eventIdOf(0x52);
    const absorb = await s.record(
      "settlement",
      `settlement:deposit:${deposit.toString("hex")}:absorb`,
      s.spend([s.spare(0)]),
      deposit,
    );
    const initialize = await s.record(
      "reserve_payout",
      `reserve_payout:${withdrawal.toString("hex")}:initialize`,
      s.spend([s.spare(1)]),
      withdrawal,
    );
    expect(await s.verdict(absorb)).toBe(false);
    expect(await s.verdict(initialize)).toBe(false);
    await listEvent(s, "deposit", deposit, 1);
    await listEvent(s, "withdrawal", withdrawal, 1);
    expect(await s.verdict(absorb)).toBe(true);
    expect(await s.verdict(initialize)).toBe(true);
    await s.sql("UPDATE node_l1_events SET retired_slot = 0");
    expect(await s.verdict(absorb)).toBe(false);
    expect(await s.verdict(initialize)).toBe(false);
  });

  it("payout fund/conclude: wanted while every input is live", async () => {
    const s = await open();
    const eventId = eventIdOf(0x53);
    const conclude = await s.record(
      "settlement",
      `settlement:withdrawal:${eventId.toString("hex")}:conclude`,
      s.spend([s.spare(0)]),
      eventId,
    );
    expect(await s.verdict(conclude)).toBe(true);
    // Another transaction spends the payout input.
    await s.driver.forward([s.spend([s.spare(0)], { nonce: s.nonce() })]);
    expect(await s.verdict(conclude)).toBe(false);
  });
});
