import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  availableBlockAssetName,
  C,
  expectLandedAndFinalizedOnce,
  finalizeRecordedBlock,
  mergedIntoRootView,
} from "./expired-signed-intent-release-evidence-emulator.expect-landed-and-finalized-once.js";
import {
  closeLifecycle,
  readDeposits,
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  admitTwoFundedTransfers,
  advanceL1ToSlot,
  expectReplaced,
  type Handle,
  landSignedCommitAsFork,
  makeRewritableQueueTransport,
  moveToExactSlot,
  nativeRoot,
  readEmulatorQueue,
  readImmutableCounts,
  resetSharedRows,
  seedCorrectionObserver,
  signedTtl,
  snapshotUnreplaced,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";

/**
 * Which authenticated evidence decides a signed commit E past its TTL when
 * the exact-point queue no longer shows its base D or E itself: E landed and
 * was merged (confirmed state, journaled canonical history, observed merge),
 * D was merged (its successor at that merge decides), or a correction removed
 * D (the correction path owns E). Actual deployed validators, the production
 * history owner and Architecture G; the served queue and the correction
 * observer's authenticated transitions are synthetic where the emulator cannot
 * produce the chain (a merge of a block this node did not merge).
 */

describe("signed-intent release evidence", { concurrent: false }, () => {
  it("confirms a signed commit whose header the confirmed state holds after a merge, and locally finalizes it once", async () => {
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
      expect(journal.depositEventIds).toHaveLength(1);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      // E landed and was merged: the root's confirmed state is E, and neither
      // D nor E is a node. (Kills "drop the confirmed-state evidence": D is
      // then absent with nothing recorded, and E stays unreconciled.)
      view.setRewrite(mergedIntoRootView(h, header));
      moveToExactSlot(h, ttl);
      await synchronizeWithin(h);
      await expectLandedAndFinalizedOnce(h, journal);
      expect(
        (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
      ).toEqual([header]);
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);

  it("re-derives a landed and merged block's node after a restart, and locally finalizes it once", async () => {
    const view = makeRewritableQueueTransport();
    const initial = await openHistoryProductionOwnerLifecycle({
      transportFactory: view.transportFactory,
    });
    let h: Handle & Pick<typeof initial, "close"> = initial;
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
      const inclusion = await submitDeposit(initial, 12_000_000n);
      const lost = await submitUnlandedBlock(initial, inclusion);
      const header = lost.submittedHeaderHash;
      const journal = await readJournal(header);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      view.setRewrite(mergedIntoRootView(initial, header));
      moveToExactSlot(initial, ttl);
      await synchronizeWithin(initial);
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.ObservedWaitingStability,
      );
      // The node restarts before local finalization. E's node is merged, so
      // no queue read yields it again; the startup hydration re-derives it
      // from E's retained signed commit. (Kills "hydrate an observed journal
      // without its node": local finalization then has no block and the node
      // is wedged.)
      const restarted = await initial.restartRuntime({ synchronize: false });
      h = restarted;
      await synchronizeWithin(restarted);
      await expectLandedAndFinalizedOnce(restarted, journal);
      expect(
        (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
      ).toEqual([header]);
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);

  it("confirms a signed commit its journaled canonical history holds though the queue shows neither it nor its base, and commits each transfer once", async () => {
    const view = makeRewritableQueueTransport();
    const h = await openHistoryProductionOwnerLifecycle({
      transportFactory: view.transportFactory,
    });
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(h.fixture);
      const { txIds } = await admitTwoFundedTransfers(h);
      const lost = await submitUnlandedBlock(
        h,
        h.fixture.emulator.now() - 1000,
      );
      const header = lost.submittedHeaderHash;
      const journal = await readJournal(header);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      // E is included (as on a fork where it landed in its window), and the
      // served queue is that chain after E and a later foreign block were
      // merged: only the history this node journaled shows E. (Kills "drop
      // the canonical-history evidence".)
      advanceL1ToSlot(h, ttl);
      await landSignedCommitAsFork(h, journal[C.SIGNED_TX_CBOR]!);
      view.setRewrite(mergedIntoRootView(h, "f1".repeat(28)));
      await synchronizeWithin(h);
      await expectLandedAndFinalizedOnce(h, journal, async (handle, hash) => {
        await finalizeRecordedBlock(handle, hash);
      });
      expect(await readImmutableCounts(txIds)).toEqual(
        Object.fromEntries(txIds.map((id) => [id, 1])),
      );
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);

  it("replaces a signed commit built on the root once the confirmed state shows a foreign block took the root's slot, only at its TTL while the history journals no spend of its base output (owner ruling 2026-09-26)", async () => {
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
      // E was built on the root: its base is the confirmed header, which no
      // merge or correction ever names. The observer's view is current and
      // records nothing.
      expect(await seedCorrectionObserver(h)).toHaveLength(1);
      // A foreign block F took the root's slot and was merged: the confirmed
      // state is F and links to E's base. (Kills "drop the confirmed-state
      // slot holder": E's base is absent with nothing recorded, and E defers
      // forever.)
      view.setRewrite(mergedIntoRootView(h, "f2".repeat(28)));
      const untouched = await snapshotUnreplaced(header);
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

  it("revives this node's replaced root-built block that the confirmed state holds from its own signed commit, abandons the unlanded replacement, and locally finalizes the winner once", async () => {
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
      expect(await readEmulatorQueue(h)).toHaveLength(1);
      moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
      await synchronizeWithin(h);
      await expectReplaced(journal, { handle: h });
      // N: E's members on the same base (the root), handed to L1 and lost.
      // The scheduler alignment is skipped as in the revival tests; the view
      // below is synthetic anyway.
      const next = await submitUnlandedBlock(
        h,
        h.fixture.emulator.now() - 1000,
        { alignScheduler: false },
      );
      const replacement = await readJournal(next.submittedHeaderHash);
      expect(replacement[C.BASE_TAIL_HEADER_HASH]).toEqual(
        journal[C.BASE_TAIL_HEADER_HASH],
      );
      // E landed on the root after all and was merged: the confirmed state is
      // E and links to the base. E's node exists only as its signed commit
      // created it; the root is not it. (Kills "drop the confirmed-state slot
      // holder": N defers forever. Kills "take the queue entry holding the
      // winner's header as its node": the root is revived as E's node.)
      await seedCorrectionObserver(h);
      view.setRewrite(mergedIntoRootView(h, header));
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
});
