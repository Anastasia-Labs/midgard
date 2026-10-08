/**
 * The node follower's intent stage (plan §8.3 S6, §8.4, I1) over a
 * simulated chain on a SQLite and a Postgres follower store carrying the
 * landed state queue (P1) and the intent journal:
 *
 * - a live, wanted intent the mempool lacks gets its exact journaled bytes,
 *   at most once per tip; one in the mempool is left alone;
 * - an intent whose §8.4 predicate fails (a merge of a header P1 does not
 *   hold at its head) is abandoned; one whose predicate holds (an
 *   attestation of a landed header, a payout funding step over live
 *   inputs) is sent; each predicate's polarities are in
 *   `l1-follower-intent-predicates.test.ts`;
 * - a landed intent, and one a foreign transaction beat to an input, are
 *   never sent;
 * - an unhealthy queue, a failed mempool read, a failed pass and an owed
 *   wallet seed are named holds, and nothing is abandoned for them;
 * - a resend the ledger refuses at two tips in a row holds its family until
 *   the facts make the intent dead or a resend is accepted; refused at a
 *   third distinct tip at or past its lower validity bound, the intent is
 *   abandoned (`ledger_rejected`) and the hold clears, while refusals at one
 *   tip, or below the bound, keep it held and never abandon it.
 */
import {
  decodeTransaction,
  type FactStore,
  WALLET_SEED_PENDING,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  INTENT_RECONCILE_FAILED,
  INTENT_RECONCILE_TRANSIENT,
  INTENT_RESUBMIT_REJECTED,
  nodeIntentTrackedSet,
} from "../src/services/l1-follower.intents.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import {
  GENESIS,
  intentStageScenarios,
  PREFIX,
  txId,
} from "./helpers/l1-follower-intents-scenario.js";
import {
  nodeDatum,
  QUEUE_ADDRESS,
  queueOutput,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const openScenario = intentStageScenarios(databases, opened);

describe.each(["sqlite", "postgres"] as const)(
  "the node intent stage over a %s follower store",
  (dialect) => {
    it("resubmits the journaled bytes of live, wanted intents once per tip and abandons a header family P1 no longer holds", async () => {
      const s = await openScenario(dialect);
      // A payout funding step: wanted while its inputs are live facts.
      const funding = await s.record(
        "reserve_payout",
        s.spend(s.spare(0)),
        "01".repeat(36),
        "reserve_payout:test:add_funds",
      );
      const attest = await s.record("attest", s.spend(s.spare(1)), s.firstHash);
      const merge = await s.record(
        "merge",
        s.spend(s.spare(2)),
        "ee".repeat(28),
      );
      const commit = await s.record("commit", s.spend(s.spare(3)));
      s.transport.mempool.add(txId(commit));
      const stage = s.stage();

      expect(await stage.run()).toEqual([]);
      const sent = s.transport.sent.map((bytes) => bytes.toString("hex"));
      expect(sent.sort()).toEqual(
        [funding, attest].map((bytes) => bytes.toString("hex")).sort(),
      );
      const actions = new Map(
        stage
          .lastReport()!
          .intents.map((entry) => [entry.intent.family, entry.action]),
      );
      expect(Object.fromEntries(actions)).toEqual({
        reserve_payout: "resubmit",
        attest: "resubmit",
        merge: "abandon",
        commit: "wait_in_mempool",
      });
      expect(await s.events(merge)).toEqual(["signed", "abandoned"]);
      expect(s.logs.some((line) => line.startsWith("abandon merge"))).toBe(
        true,
      );

      // Same tip: nothing is sent again; the abandoned merge is dead.
      expect(await stage.run()).toEqual([]);
      expect(s.transport.sent).toHaveLength(2);
      expect(
        stage.lastReport()!.intents.find((e) => e.intent.family === "merge")!
          .action,
      ).toBe("dead");

      // A new tip: the still-live intents get the same bytes once more.
      await s.chain.forward([]);
      await stage.run();
      expect(
        s.transport.sent
          .slice(2)
          .map((b) => b.toString("hex"))
          .sort(),
      ).toEqual(sent.sort());
      stage.close();
    });

    it("never sends a landed intent or one a foreign transaction beat to its input", async () => {
      const s = await openScenario(dialect);
      const landedTx = s.spend(s.spare(0));
      await s.record("reserve_payout", landedTx);
      await s.record("retire", s.spend(s.spare(1)));
      // A foreign transaction (never journaled) spends the retire's input.
      await s.chain.forward([landedTx, s.spend(s.spare(1))]);
      const stage = s.stage();

      expect(await stage.run()).toEqual([]);
      expect(s.transport.sent).toEqual([]);
      const byFamily = Object.fromEntries(
        stage
          .lastReport()!
          .intents.map((entry) => [
            entry.intent.family,
            [entry.status.kind, entry.action],
          ]),
      );
      expect(byFamily).toEqual({
        reserve_payout: ["landed", "follow"],
        retire: ["conflicted", "dead"],
      });
      stage.close();
    });

    it("holds instead of abandoning when the queue is unhealthy or the mempool read fails", async () => {
      const s = await openScenario(dialect);
      const attest = await s.record(
        "correction",
        s.spend(s.spare(0)),
        s.firstHash,
      );
      const orphan = simHeader(2, GENESIS);
      await s.chain.forward([
        {
          inputs: [s.chain.chain.outsideInput()],
          outputs: [
            queueOutput(
              PREFIX + SDK.stateQueueHeaderHash(orphan),
              nodeDatum(orphan, "Unattested", null),
            ),
          ],
          nonce: s.nonce(),
        },
      ]);
      const stage = s.stage();
      const holds = await stage.run();
      expect(holds).toHaveLength(1);
      expect(holds[0]!.reason).toBe(INTENT_RECONCILE_TRANSIENT);
      expect(holds[0]!.detail).toContain("unhealthy (orphan_node)");
      expect(s.transport.sent).toEqual([]);
      expect(await s.events(attest)).toEqual(["signed"]);

      const register = await s.record("register", s.spend(s.spare(1)));
      s.transport.failMempool = true;
      const failed = await stage.run();
      expect(failed.map((hold) => hold.reason)).toEqual([
        INTENT_RECONCILE_TRANSIENT,
        INTENT_RECONCILE_TRANSIENT,
      ]);
      expect(
        failed.some((hold) => hold.detail.includes("mempool read failed")),
      ).toBe(true);
      expect(await s.events(register)).toEqual(["signed"]);
      expect(stage.holds()).toEqual(failed);
      stage.close();
    });

    it("holds a family whose resend the ledger refuses at two tips, until another landed tx spends its input", async () => {
      const s = await openScenario(dialect);
      const add = s.spend(s.spare(0));
      const funding = await s.record(
        "reserve_payout",
        add,
        "01".repeat(36),
        "reserve_payout:test:add_funds",
      );
      s.transport.refuse.add(txId(funding));
      const stage = s.stage();
      expect(await stage.run()).toEqual([]);
      // Same tip: not sent again, so not refused again.
      expect(await stage.run()).toEqual([]);
      await s.chain.forward([]);
      const held = await stage.run();
      expect(held.map((hold) => hold.reason)).toEqual([
        INTENT_RESUBMIT_REJECTED,
      ]);
      expect(held[0]!.detail).toContain("reserve_payout:");
      expect(held[0]!.detail).toContain(txId(funding));
      expect(await s.events(funding)).toEqual([
        "signed",
        "submit_attempt",
        "submit_rejected",
        "submit_attempt",
        "submit_rejected",
      ]);
      expect(await stage.run()).toEqual(held);
      // Another tx spends its input and lands: the intent is dead.
      await s.chain.forward([s.spend(s.spare(0))]);
      expect(await stage.run()).toEqual([]);
      stage.close();
    });

    it("abandons an intent the ledger refuses at a third distinct tip at or past its lower bound (ledger_rejected), and the hold clears", async () => {
      const s = await openScenario(dialect);
      const funding = await s.record(
        "reserve_payout",
        s.spend(s.spare(0)),
        "01".repeat(36),
        "reserve_payout:test:add_funds",
      );
      s.transport.refuse.add(txId(funding));
      const stage = s.stage();
      expect(await stage.run()).toEqual([]);
      await s.chain.forward([]);
      expect((await stage.run()).map((hold) => hold.reason)).toEqual([
        INTENT_RESUBMIT_REJECTED,
      ]);
      await s.chain.forward([]);
      expect(await stage.run()).toEqual([]);
      const entry = stage
        .lastReport()!
        .intents.find((e) => e.intent.family === "reserve_payout")!;
      expect([entry.action, entry.rejection]).toEqual([
        "abandon",
        Buffer.from("refused").toString("hex"),
      ]);
      const log = await s.eventLog(funding);
      expect(log.map((event) => event.kind)).toEqual([
        "signed",
        ...Array.from({ length: 3 }, () => [
          "submit_attempt",
          "submit_rejected",
        ]).flat(),
        "abandoned",
      ]);
      expect(log.at(-1)!.detail).toEqual({
        reason: "ledger_rejected",
        tips: 3,
      });
      // Dead from now on: never sent again, nothing held.
      const sent = s.transport.sent.length;
      await s.chain.forward([]);
      expect(await stage.run()).toEqual([]);
      expect(s.transport.sent).toHaveLength(sent);
      expect(
        stage.lastReport()!.entry(decodeTransaction(funding).hash)?.status.kind,
      ).toBe("abandoned");
      stage.close();
    });

    it("keeps holding, and never abandons, an intent the ledger refuses three times at one tip", async () => {
      const s = await openScenario(dialect);
      const funding = await s.record(
        "reserve_payout",
        s.spend(s.spare(0)),
        "01".repeat(36),
        "reserve_payout:test:add_funds",
      );
      s.transport.refuse.add(txId(funding));
      const stage = s.stage();
      expect(await stage.run()).toEqual([]);
      // A block on top, rolled back: the tip is the same block again, at a
      // new generation, so the bytes go out (and are refused) once more.
      for (let again = 0; again < 2; again += 1) {
        await s.chain.forward([]);
        await s.chain.backward(1);
        expect((await stage.run()).map((hold) => hold.reason)).toEqual([
          INTENT_RESUBMIT_REJECTED,
        ]);
      }
      expect(await s.events(funding)).toEqual([
        "signed",
        ...Array.from({ length: 3 }, () => [
          "submit_attempt",
          "submit_rejected",
        ]).flat(),
      ]);
      expect(
        stage.lastReport()!.entry(decodeTransaction(funding).hash)?.status.kind,
      ).toBe("live");
      stage.close();
    });

    it("clears a resend-refusal hold once a resend is accepted, and once the intent's validity has passed; refusals below its lower bound never abandon it", async () => {
      const s = await openScenario(dialect);
      const accepted = await s.record(
        "reserve_payout",
        s.spend(s.spare(0)),
        "01".repeat(36),
        "reserve_payout:a:add_funds",
      );
      const tip = s.chain.chain.tip.point.slot;
      const expiring = await s.record(
        "settlement",
        // Valid from a slot past every tip it is refused at below.
        {
          ...s.spend(s.spare(1)),
          invalidBefore: tip + 5,
          invalidAfter: tip + 6,
        },
        "02".repeat(36),
        "settlement:b:add_funds",
      );
      for (const tx of [accepted, expiring]) s.transport.refuse.add(txId(tx));
      const stage = s.stage();
      await stage.run();
      await s.chain.forward([]);
      expect(
        (await stage.run()).map((hold) => hold.detail.split(":")[0]),
      ).toEqual(["reserve_payout", "settlement"]);
      s.transport.refuse.delete(txId(accepted));
      await s.chain.forward([]);
      expect(
        (await stage.run()).map((hold) => hold.detail.split(":")[0]),
      ).toEqual(["settlement"]);
      // Three refusals at distinct tips, all below its lower bound: held, not abandoned.
      expect(s.chain.chain.tip.point.slot).toBeLessThan(tip + 5);
      expect(await s.events(expiring)).toEqual([
        "signed",
        ...Array.from({ length: 3 }, () => [
          "submit_attempt",
          "submit_rejected",
        ]).flat(),
      ]);
      while (s.chain.chain.tip.point.slot < tip + 6) await s.chain.forward([]);
      expect(await stage.run()).toEqual([]);
      stage.close();
    });

    it("names an owed wallet seed and a failed pass as holds", async () => {
      const s = await openScenario(dialect);
      const stage = s.stage([QUEUE_ADDRESS]);
      const holds = await stage.run();
      expect(holds.map((hold) => hold.reason)).toEqual([WALLET_SEED_PENDING]);
      expect(holds[0]!.detail).toContain("ledger_unavailable");
      stage.close();
      opened.splice(opened.indexOf(s.store), 1);
      await s.store.close();
      const after = await s.stage().run();
      expect(after.map((hold) => hold.reason)).toEqual([
        INTENT_RECONCILE_FAILED,
      ]);
    });
  },
);

describe("the node intent tracked set", () => {
  it("is the protocol payment credentials and the hub-oracle policy, without the seeded wallets", () => {
    expect(
      nodeIntentTrackedSet({
        protocolPaymentCredentials: ["ab".repeat(28)],
        hubOraclePolicyId: "cd".repeat(28),
      }),
    ).toEqual({
      addresses: new Set(),
      paymentCredentials: new Set(["ab".repeat(28)]),
      policies: new Set(["cd".repeat(28)]),
    });
  });
});
