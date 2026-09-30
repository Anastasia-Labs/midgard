import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import { admitObservedMerges } from "./expired-signed-intent-release-evidence-emulator.admit-observed-merges.js";
import {
  appendThenMerge,
  discardSignedHeaderPlan,
  mergeLeavingRoot,
  replacedRootBuiltPair,
  retainSignedHeaderPlan,
  snapshotRevival,
} from "./expired-signed-intent-release-evidence-emulator.append-then-merge.js";
import {
  extendTransitions,
  observeMerges,
  observerContext,
  recordTransitions,
} from "./expired-signed-intent-release-evidence-emulator.merge-checkpoint.js";
import {
  availableBlockAssetName,
  C,
  finalizeRecordedBlock,
  mergedIntoRootView,
} from "./expired-signed-intent-release-evidence-emulator.signed-intent-release-evidence.js";
import {
  closeLifecycle,
  openCorrectionRewindScenario,
  read,
  readJournal,
  readSqlLedgerRoot,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  expectReplaced,
  makeRewritableQueueTransport,
  moveToExactSlot,
  nativeRoot,
  nextPoint,
  readEmulatorQueue,
  resetSharedRows,
  signedTtl,
  snapshotUnreplaced,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";

describe.sequential(
  "signed-intent release of a root-built commit after merges",
  () => {
    it("replaces a root-built commit once the observer records the transition after the root was left empty, naming a foreign block, though merges hid the slot from the confirmed state; never before", async () => {
      const view = makeRewritableQueueTransport();
      const h = await openHistoryProductionOwnerLifecycle({
        transportFactory: view.transportFactory,
      });
      try {
        await resetSharedRows();
        await advanceEmulatorPastLatestBlockEndTime(h.fixture);
        const inclusion = await submitDeposit(h, 12_000_000n);
        const lost = await submitUnlandedBlock(h, inclusion);
        const header = lost.submittedHeaderHash;
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        expect(await readEmulatorQueue(h)).toHaveLength(1);
        const context = observerContext(h);
        const base = journal[C.BASE_TAIL_HEADER_HASH].toString("hex");
        // E was built on the root output B, which the merge of its base D
        // left with an empty queue; the observer recorded that merge. A
        // foreign F then took B, a foreign G followed, and both were merged:
        // the confirmed state is G over F, so nothing links it to D.
        const { previous, merge } = mergeLeavingRoot(
          context,
          base,
          journal[C.BASE_TAIL_OUT_REF],
        );
        const recorded = await recordTransitions(context, previous, [merge]);
        const foreign = "f4".repeat(28);
        const later = "f5".repeat(28);
        view.setRewrite(mergedIntoRootView(h, later, foreign));
        const untouched = await snapshotUnreplaced(header);
        moveToExactSlot(h, ttl - 1);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // Past E's TTL no later transition is recorded yet: which block took
        // B is unknown, so E defers. (Kills "drop the root-emptying arm":
        // the recorded merge of D reads as D merged while still the tail, and
        // E is replaced. Kills "replace while no later transition is
        // recorded".)
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // The observer then records F's and G's appends and merges: the
        // merge of F names F as the block that took B, so the next point
        // replaces E. (Kills "the root-emptying arm always defers".)
        await extendTransitions(
          context,
          recorded.queue,
          appendThenMerge(context, recorded.queue, [foreign, later], 11),
        );
        await nextPoint(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("revives this node's replaced root-built block that the observer records taking the emptied root's slot, abandons its unlanded replacement, and locally finalizes the winner once", async () => {
      const view = makeRewritableQueueTransport();
      const h = await openHistoryProductionOwnerLifecycle({
        transportFactory: view.transportFactory,
      });
      try {
        const { header, journal, replacement } = await replacedRootBuiltPair(h);
        const context = observerContext(h);
        // E landed on the root output B after all, which the merge of its
        // base D had left empty; G followed and E and G were merged, all
        // still pending. The confirmed state is G over E: neither it nor an
        // admitted transition shows E landed.
        const { previous, merge } = mergeLeavingRoot(
          context,
          journal[C.BASE_TAIL_HEADER_HASH].toString("hex"),
          journal[C.BASE_TAIL_OUT_REF],
        );
        const later = "f6".repeat(28);
        await recordTransitions(context, previous, [
          merge,
          ...appendThenMerge(context, merge.nextQueue, [header, later], 11, [
            journal[C.INTENDED_TX_HASH]!.toString("hex"),
          ]),
        ]);
        view.setRewrite(mergedIntoRootView(h, later, header));
        // Past N's TTL, the merge after B was left empty names E as the
        // block that took it: N is replaced and E revived. (Kills "drop the
        // root-emptying arm": the recorded merge of D reads as D merged
        // while still the tail, and N is replaced without reviving E.)
        moveToExactSlot(h, signedTtl(replacement[C.SIGNED_TX_CBOR]!));
        await synchronizeWithin(h);
        await expectReplaced(replacement, { globalsReset: false, handle: h });
        expect((await readJournal(header))[C.STATUS]).toBe(
          Pending.Status.ObservedWaitingStability,
        );
        expect(availableBlockAssetName(h)).toBe(
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
        );
        await finalizeRecordedBlock(h, header);
        expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("revives this node's replaced block on the same base output once an admitted merge folds it, with no transition recorded around its base; never on a pending one", async () => {
      const view = makeRewritableQueueTransport();
      const h = await openHistoryProductionOwnerLifecycle({
        transportFactory: view.transportFactory,
      });
      try {
        const { header, journal, replacement } = await replacedRootBuiltPair(h);
        const context = observerContext(h);
        // E landed on the root and G followed; the observer saw only the
        // queue [root, E, G] and then the merges of E and G, still pending.
        // Nothing it recorded names E's base or its root output.
        const eTx = journal[C.INTENDED_TX_HASH]!.toString("hex");
        const later = "f7".repeat(28);
        const observed = await observeMerges(
          context,
          [
            { headerHash: null, outRef: `${eTx}#0` },
            { headerHash: header, outRef: `${eTx}#1` },
            { headerHash: later, outRef: `${"c7".repeat(32)}#1` },
          ],
          2,
        );
        view.setRewrite(mergedIntoRootView(h, later, header));
        // Past N's TTL a pending merge of E may still be retracted: N defers.
        // (Kills "a pending merge shows a replaced sibling landed".)
        const untouched = await snapshotUnreplaced(
          replacement[C.HEADER_HASH].toString("hex"),
        );
        moveToExactSlot(h, signedTtl(replacement[C.SIGNED_TX_CBOR]!));
        await synchronizeWithin(h);
        expect(
          await snapshotUnreplaced(replacement[C.HEADER_HASH].toString("hex")),
        ).toEqual(untouched);
        // Once the merges are admitted, E's own landing decides: N is
        // replaced and E revived. (Kills "drop the replaced-sibling arm": N
        // defers forever.)
        await admitObservedMerges(context, observed);
        await nextPoint(h);
        await expectReplaced(replacement, { globalsReset: false, handle: h });
        expect((await readJournal(header))[C.STATUS]).toBe(
          Pending.Status.ObservedWaitingStability,
        );
        expect(availableBlockAssetName(h)).toBe(
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
        );
        await finalizeRecordedBlock(h, header);
        expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);
  },
);

describe.sequential("replaced-block revival evidence", () => {
  it("revives a replaced block while no journal is active only once an admitted merge saw it on the queue, never on a pending merge's hint or while a plan is retained, and hands it to local finalization", async () => {
    const scenario = await openCorrectionRewindScenario({
      blocks: 2,
      unlandedTail: true,
    });
    const { h } = scenario;
    try {
      const [, header] = scenario.headers as [string, string];
      const journal = await readJournal(header);
      const queue = await scenario.readQueue();
      moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
      await synchronizeWithin(h);
      await expectReplaced(journal, { handle: h });
      const replaced = await snapshotRevival(h);
      // The observer records a merge of D that saw E as D's successor, below
      // its release depth: a hint that makes E a revival candidate, bound to
      // no checkpoint and not final. No journal is active and nothing bound
      // to the checkpoint shows E landed, so nothing is revived. (Kills
      // "revive a candidate on the observer's hint alone" and "take a pending
      // merge as evidence".)
      const observed = await observeMerges(
        scenario,
        [
          ...queue,
          {
            headerHash: header,
            outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
          },
        ],
        1,
      );
      await nextPoint(h);
      expect(await snapshotRevival(h)).toEqual(replaced);
      // The merge becomes final while a signed-header recovery plan is
      // retained: the revival waits for it, and the gate stays closed.
      // (Kills "revive while a plan is retained".)
      await admitObservedMerges(scenario, observed);
      await retainSignedHeaderPlan();
      try {
        h.fixture.emulator.awaitBlock(1);
        vi.setSystemTime(new Date(h.fixture.emulator.now()));
        expect(await h.appendTipWhileGateClosed()).toBeDefined();
        expect(await snapshotRevival(h)).toEqual(replaced);
      } finally {
        await discardSignedHeaderPlan();
      }
      // With no plan retained, the admitted merge that saw E on the queue
      // revives it. (Kills "drop the admitted-merge evidence": E stays
      // abandoned.)
      await nextPoint(h);
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.ObservedWaitingStability,
      );
      expect((await readSqlLedgerRoot()).root_hex).toBe(
        journal[C.EXPECTED_UTXOS_ROOT],
      );
      expect(
        Effect.runSync(Ref.get(h.globals.LOCAL_FINALIZATION_PENDING)),
      ).toBe(true);
      expect(availableBlockAssetName(h)).toBe(
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
      );
      // The native root stays at the base: local finalization replays E.
      expect(await nativeRoot(h)).toBe(journal[C.BASE_UTXOS_ROOT]);
    } finally {
      await discardSignedHeaderPlan();
      await closeLifecycle(h);
    }
  }, 900_000);
});

const INJECTED_PLAN_FAILURE =
  "injected crash while marking the release applied";

export const INJECTED_REPLAY_INTERRUPT =
  "injected crash after the native replay";

/** A database fault at the plan's final state change, inside the release's
 * own transaction. */
export const refusePlanApplication = (refuse: boolean) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      if (!refuse) {
        yield* sql`DROP TRIGGER IF EXISTS midgard_test_refuse_plan_applied
          ON event_history_recovery_plans`;
        yield* sql`DROP FUNCTION IF EXISTS midgard_test_refuse_plan_applied()`;
        return;
      }
      yield* sql.unsafe(`CREATE OR REPLACE FUNCTION midgard_test_refuse_plan_applied()
        RETURNS trigger LANGUAGE plpgsql AS $$
        BEGIN RAISE EXCEPTION '${INJECTED_PLAN_FAILURE}'; END $$`);
      yield* sql.unsafe(`CREATE TRIGGER midgard_test_refuse_plan_applied
        BEFORE UPDATE ON event_history_recovery_plans FOR EACH ROW
        WHEN (NEW.state = 'applied' AND OLD.state = 'prepared')
        EXECUTE FUNCTION midgard_test_refuse_plan_applied()`);
    }),
  );
