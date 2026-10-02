import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { observeMerges } from "./expired-signed-intent-release-evidence-emulator.merge-checkpoint.js";
import {
  availableBlockAssetName,
  C,
  expectLandedAndFinalizedOnce,
  finalizeRecordedBlock,
  mergedIntoRootView,
} from "./expired-signed-intent-release-evidence-emulator.signed-intent-release-evidence.js";
import {
  closeLifecycle,
  openCorrectionRewindScenario,
  read,
  readJournal,
  readObserver,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import {
  advanceL1ToSlot,
  expectReplaced,
  landSignedCommitAsFork,
  makeRewritableQueueTransport,
  moveToExactSlot,
  nativeRoot,
  nextPoint,
  signedTtl,
  snapshotUnreplaced,
  synchronizeWithin,
  UNLANDED,
} from "./helpers/signed-intent-replacement.js";

describe.sequential(
  "signed-intent release after its base left the queue",
  () => {
    it("replaces a signed commit whose base was merged with a foreign successor only at its TTL while the history journals no spend of its base output (owner ruling 2026-09-26)", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        expect(UNLANDED).toContain(journal[C.STATUS]);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        expect(h.fixture.emulator.slot).toBeLessThan(ttl);
        const queue = await scenario.readQueue();
        expect(queue.map(({ headerHash }) => headerHash)).toEqual([null, base]);
        // A foreign block F took D's slot; D and then F were merged, so the
        // served queue is a root whose confirmed state is F. Only the observed
        // merge of D names D's successor. (Kills "defer whenever D is absent".)
        const foreign = "f2".repeat(28);
        await observeMerges(
          scenario,
          [...queue, { headerHash: foreign, outRef: `${"f3".repeat(32)}#1` }],
          2,
        );
        const untouched = await snapshotUnreplaced(header);
        view.setRewrite(mergedIntoRootView(h, foreign));
        moveToExactSlot(h, ttl - 1);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("decides again when the correction observer records its base's merge after a first decision found nothing recorded, and replaces the commit", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        // D and a foreign successor F were merged, but the observer (cursor
        // at [root, D]) has recorded neither yet: past E's TTL the first
        // decision defers, and so does the next point while its view is
        // unchanged.
        const foreign = "f8".repeat(28);
        view.setRewrite(mergedIntoRootView(h, foreign));
        const untouched = await snapshotUnreplaced(header);
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        await nextPoint(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // The observer then records the merge of D, naming F as D's
        // successor: the next point replaces E. (Kills "a nothing-recorded
        // deferral is sticky": E is never decided again.)
        await observeMerges(
          scenario,
          [...queue, { headerHash: foreign, outRef: `${"f9".repeat(32)}#1` }],
          2,
        );
        await nextPoint(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("decides again when the correction observer, blocked at the first decision, then records its base's merge naming the commit, and locally finalizes it once", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        // E landed on D, and D, E and G were merged. The observer has no view
        // at all (its row is gone), so the first decision past E's TTL
        // defers.
        await read(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM state_queue_terminal_observer_states`;
          }),
        );
        const later = "fa".repeat(28);
        view.setRewrite(mergedIntoRootView(h, later));
        const untouched = await snapshotUnreplaced(header);
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // The observer bootstraps and records the merge of D, which names E
        // as D's successor: the next point records E landed. (Kills "a
        // blocked-observer deferral is sticky".)
        await observeMerges(
          scenario,
          [
            ...queue,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: later, outRef: `${"fb".repeat(32)}#1` },
          ],
          1,
        );
        await nextPoint(h);
        await expectLandedAndFinalizedOnce(h, journal);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("never records a signed commit landed when a correction removed it after it landed, though its journaled canonical history holds it", async () => {
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const untouched = await snapshotUnreplaced(header);
        // E lands inside its window; the correction fiber's cursor then sees
        // it as the queue tail.
        advanceL1ToSlot(h, ttl);
        await landSignedCommitAsFork(h, journal[C.SIGNED_TX_CBOR]!);
        await read(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM state_queue_terminal_observer_states`;
          }),
        );
        expect((await scenario.tick(h.globals)).status).toBe("bootstrapped");
        // A correction removes E, and the observer records it below its
        // release depth before the history owner sees any of this.
        await scenario.removeTail(header, { observe: false });
        await scenario.tick(h.globals);
        // E is in the journaled canonical history, but the correction path
        // owns it: nothing is recorded or replaced. (Kills "drop the
        // correction-of-the-block deferral": E is recorded landed and
        // locally finalized although it was removed.)
        await scenario.nextSourceBlock();
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("replaces a signed commit whose base a correction removed when this node no longer journals that base", async () => {
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
      });
      const { h } = scenario;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const untouched = await snapshotUnreplaced(header);
        await scenario.removeTail(base);
        expect(h.fixture.emulator.slot).toBeGreaterThanOrEqual(ttl);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        await scenario.tick(h.globals);
        // D's journal is pruned (as for a base this node never journaled):
        // no correction path reconciles it with E, so the correction of D
        // decides for replacement. (Kills "always defer on a correction of
        // the base": E waits for a correction path that never comes.)
        await read(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const pruned = yield* sql`DELETE FROM pending_block_finalizations
              WHERE header_hash = ${Buffer.from(base, "hex")}
              RETURNING header_hash`;
            expect(pruned).toHaveLength(1);
          }),
        );
        await scenario.nextSourceBlock();
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("replaces a signed commit whose base was merged while it was still the queue tail", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        expect(queue.map(({ headerHash }) => headerHash)).toEqual([null, base]);
        // D was merged while it was still the tail, and a later foreign block
        // F was merged after it: the served queue shows neither E nor D, so
        // only the observed merge of D decides, and it names no successor.
        // (Kills "defer when the merged base had no successor".)
        await observeMerges(scenario, queue, 1);
        view.setRewrite(mergedIntoRootView(h, "fe".repeat(28)));
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("revives this node's replaced block named as its merged base's successor from its own signed commit, abandons the unlanded replacement, and locally finalizes the winner once", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectReplaced(journal, { handle: h });
        // N: E's members on the same base, handed to L1 and lost. The
        // scheduler alignment is skipped as in the revival tests; the view
        // below is synthetic anyway.
        const lost = await submitUnlandedBlock(
          h,
          h.fixture.emulator.now() - 1000,
          { alignScheduler: false },
        );
        const replacement = await readJournal(lost.submittedHeaderHash);
        expect(replacement[C.BASE_TAIL_HEADER_HASH].toString("hex")).toBe(base);
        // E landed on D after all; D, E and G were merged, and only the merge
        // of D is recorded, naming E as D's successor. E's node exists only
        // as its signed commit created it. (Kills "revive only a successor
        // whose node is on the queue": N waits forever.)
        const later = "fc".repeat(28);
        await observeMerges(
          scenario,
          [
            ...queue,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: later, outRef: `${"fd".repeat(32)}#1` },
          ],
          1,
        );
        view.setRewrite(mergedIntoRootView(h, later));
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

    it("confirms a signed commit an observed merge folded into the confirmed state, and locally finalizes it once", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const [root] = await scenario.readQueue();
        // E landed on D; D was merged before the observer's cursor, then E and
        // a later block G were merged. Only the observed merge of E shows E
        // landed: no merge of D is recorded. (Kills "drop the observed-merge
        // evidence".)
        await observeMerges(
          scenario,
          [
            root!,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: "f4".repeat(28), outRef: `${"f5".repeat(32)}#1` },
          ],
          2,
        );
        view.setRewrite(mergedIntoRootView(h, "f4".repeat(28)));
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectLandedAndFinalizedOnce(h, journal);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("confirms a signed commit named as its base's successor by the observed merge of its base, and locally finalizes it once", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        // E landed on D; D, E and a later block G were merged, but the
        // observer has recorded only the merge of D so far. That merge names E
        // as D's successor. (Kills "wait when the merged successor is E".)
        await observeMerges(
          scenario,
          [
            ...queue,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: "f6".repeat(28), outRef: `${"f7".repeat(32)}#1` },
          ],
          1,
        );
        view.setRewrite(mergedIntoRootView(h, "f6".repeat(28)));
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectLandedAndFinalizedOnce(h, journal);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("defers a signed commit whose base a correction removed to the correction path, which then abandons it under the correction", async () => {
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
      });
      const { h } = scenario;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const untouched = await snapshotUnreplaced(header);
        const removal = await scenario.removeTail(base);
        // The removal waited out D's attestation timeout, past E's TTL; the
        // observer has not recorded it yet, so nothing is decided.
        expect(h.fixture.emulator.slot).toBeGreaterThanOrEqual(ttl);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // Observed below its release depth: the correction names D, which this
        // node journals, so E defers to the correction path. (Kills "replace
        // when a correction removed D".)
        await scenario.tick(h.globals);
        await scenario.nextSourceBlock();
        await scenario.nextSourceBlock();
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // Admitted: the correction rewind abandons E under the correction.
        await scenario.awaitRemovalFinality();
        expect(
          (await scenario.tick(h.globals)).admittedTransactionHashes,
        ).toEqual([removal.accepted.transaction.txHash]);
        await scenario.nextSourceBlock();
        const digest = (await readObserver()).admitted[0]!.transitionDigest;
        const abandoned = await readJournal(header);
        expect(abandoned[C.STATUS]).toBe(Pending.Status.Abandoned);
        expect(abandoned[C.CORRECTION_TRANSITION_DIGEST]).toBe(digest);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);
  },
);
