import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  availableBlockAssetName,
  C,
  expectLandedAndFinalizedOnce,
  finalizeRecordedBlock,
  mergedIntoRootView,
} from "./expired-signed-intent-release-evidence-emulator.expect-landed-and-finalized-once.js";
import { observeMerges } from "./expired-signed-intent-release-evidence-emulator.merge-checkpoint.js";
import {
  closeLifecycle,
  openCorrectionRewindScenario,
  readJournal,
  readObserver,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import {
  expectReplaced,
  makeRewritableQueueTransport,
  moveToExactSlot,
  nativeRoot,
  signedTtl,
  snapshotUnreplaced,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";

describe(
  "signed-intent release after its base left the queue",
  { concurrent: false },
  () => {
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
