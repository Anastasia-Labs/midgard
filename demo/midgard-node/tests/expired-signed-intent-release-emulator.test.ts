import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  commitNextBlock,
  finalizeLocally,
  outputOf,
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  admitTwoFundedTransfers,
  advanceL1ToSlot,
  awaitOwnerReady,
  expectReplaced,
  expectUnreplaced,
  type Handle,
  holdLeaseAsCrashed,
  makeRewritableQueueTransport,
  moveToExactSlot,
  nativeRoot,
  nextPoint,
  readDepositHeader,
  readImmutableCounts,
  readPlans,
  resetSharedRows,
  retireCrashedLease,
  signedTtl,
  snapshotUnreplaced,
  UNLANDED,
  updateJournal,
} from "./helpers/signed-intent-replacement.js";

/**
 * "Whichever lands wins" for a signed commit E that missed its validity
 * window: actual deployed validators, the production history owner and
 * Architecture G, and emulator transactions. E is a real signed commit the
 * production commit worker handed to L1, dropped from the emulator mempool
 * before any block included it (the live 3b61adb6 shape). Only chain-point
 * names and the history transport are synthetic. The history owner replaces
 * E exactly when a source point at or past E's TTL shows E's base still the
 * queue tail, or a block that is not E holding its slot; it confirms E when
 * E holds it.
 */

const C = Pending.Columns;

it("replaces a signed commit of two L2 transfers only at a source point reaching its TTL, not on wall clock across a restart, and the recommit lands and commits each transfer once", async () => {
  const lifecycle = await openHistoryProductionOwnerLifecycle();
  let h: Handle = lifecycle;
  let crashedLease: string | undefined;
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    // Two L2 transfers spending outputs funded through a merged deposit
    // block, admitted and not yet committed.
    const { first, second, txIds } = await admitTwoFundedTransfers(lifecycle);

    // E is signed and handed to L1, which never includes it.
    const lost = await submitUnlandedBlock(
      lifecycle,
      h.fixture.emulator.now() - 1000,
    );
    const header = lost.submittedHeaderHash;
    const journal = await readJournal(header);
    expect(UNLANDED).toContain(journal[C.STATUS]);
    expect(journal.depositEventIds).toHaveLength(0);
    expect(journal.mempoolTxIds.map((id) => id.toString("hex")).sort()).toEqual(
      txIds,
    );
    expect(journal[C.INTENDED_TX_HASH]!.toString("hex")).toBe(
      lost.submittedTxHash,
    );
    expect(journal[C.BASE_UTXOS_ROOT]).not.toBe(journal[C.EXPECTED_UTXOS_ROOT]);
    const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
    expect(h.batches.at(-1)!.observedSlot).toBeLessThan(ttl - 1);
    crashedLease = journal[C.STATE_QUEUE_LEASE_TOKEN];
    await holdLeaseAsCrashed(crashedLease);
    const untouched = await snapshotUnreplaced(header);

    // A source point at TTL - 1, the last slot E could still be included in:
    // nothing is replaced. (Kills the TTL off-by-one mutant `>= ttl - 1`.)
    moveToExactSlot(h, ttl - 1);
    await h.synchronize();
    expect(h.batches.at(-1)!.observedSlot).toBe(ttl - 1);
    await expectUnreplaced(header, untouched);

    // L1 and the wall clock reach the TTL, but no source point does: a
    // restart converges on its retained checkpoint and replaces nothing.
    moveToExactSlot(h, ttl);
    const restarted = await lifecycle.restartRuntime({ synchronize: false });
    h = restarted;
    await awaitOwnerReady(restarted);
    await expectUnreplaced(header, untouched);
    expect(
      Effect.runSync(
        Ref.get(restarted.globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
      ),
    ).toBe(lost.submittedTxHash);

    // The source point at exactly the TTL slot, D.next still empty: the same
    // append closes the gate, replaces E and reopens it. (Kills the mutants
    // `> ttl` and "drop the replacement".)
    await restarted.synchronize();
    expect(restarted.batches.at(-1)!.observedSlot).toBe(ttl);
    await expectReplaced(journal, { handle: restarted });

    // NEW_E recommits both reopened transfers on the restored base, lands and
    // is locally finalized; each transfer is committed exactly once.
    const next = await commitNextBlock(restarted);
    expect(next.submittedHeaderHash).not.toBe(header);
    const recommitted = await readJournal(next.submittedHeaderHash);
    expect(recommitted[C.BASE_UTXOS_ROOT]).toBe(journal[C.BASE_UTXOS_ROOT]);
    expect(
      recommitted.mempoolTxIds.map((id) => id.toString("hex")).sort(),
    ).toEqual(txIds);
    await restarted.synchronize();
    await finalizeLocally(restarted, next.submittedHeaderHash);
    expect(
      (await readJournal(next.submittedHeaderHash))[C.EXPECTED_UTXOS_ROOT],
    ).toBe(await nativeRoot(restarted));
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 1])),
    );
    await outputOf(restarted, first, 5_000_000n);
    await outputOf(restarted, second, 4_000_000n);
    // E stays replaced.
    expect((await readJournal(header))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
  } finally {
    if (crashedLease !== undefined) await retireCrashedLease(crashedLease);
    await closeLifecycle(h);
  }
}, 900_000);

it("confirms, and never replaces, a signed commit included in the last slot of its window", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const inclusion = await submitDeposit(h, 12_000_000n);
    const lost = await submitUnlandedBlock(h, inclusion);
    const header = lost.submittedHeaderHash;
    const journal = await readJournal(header);
    const signed = journal[C.SIGNED_TX_CBOR]!;
    const ttl = signedTtl(signed);
    // Withheld until TTL - 1, then included: the block that includes it is
    // already past the TTL, so the first source point showing it is one the
    // reconciliation acts at.
    advanceL1ToSlot(h, ttl - 1);
    expect(await h.fixture.emulator.submitTx(signed.toString("hex"))).toBe(
      lost.submittedTxHash,
    );
    expect(await h.fixture.operatorLucid.awaitTx(lost.submittedTxHash)).toBe(
      true,
    );
    expect(h.fixture.emulator.slot).toBeGreaterThanOrEqual(ttl);
    await h.synchronize();
    // E holds D's slot: its observation is recorded, it is never abandoned.
    const observed = await readJournal(header);
    expect(observed[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
    expect(observed[C.CORRECTION_TRANSITION_DIGEST]).toBeNull();
    expect(await readPlans()).toEqual([]);
    expect(await readDepositHeader(journal.depositEventIds[0]!)).toBe(header);
    const g = h.globals;
    expect(Effect.runSync(Ref.get(g.LOCAL_FINALIZATION_PENDING))).toBe(true);
    expect(
      Effect.runSync(Ref.get(g.AVAILABLE_LOCAL_FINALIZATION_BLOCK)),
    ).not.toBe("");
    expect(Effect.runSync(Ref.get(g.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH))).toBe(
      "",
    );
    await nextPoint(h);
    expect((await readJournal(header))[C.STATUS]).toBe(
      Pending.Status.ObservedWaitingStability,
    );
    // The normal path locally finalizes the included commit.
    await finalizeLocally(h, header);
    expect((await readJournal(header))[C.STATUS]).not.toBe(
      Pending.Status.Abandoned,
    );
    expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
    expect(await readPlans()).toEqual([]);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

it("fails closed on an undecodable or unsigned intent past its TTL, and replaces a deposit block once its signed bytes are intact", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const inclusion = await submitDeposit(h, 12_000_000n);
    const lost = await submitUnlandedBlock(h, inclusion);
    const header = lost.submittedHeaderHash;
    const journal = await readJournal(header);
    expect(journal.depositEventIds).toHaveLength(1);
    const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);

    // Undecodable signed bytes: no TTL can be read, so the intent is never
    // shown unable to land; the owner stays ready and replaces nothing.
    await updateJournal(header, {
      [C.SIGNED_TX_CBOR]: Buffer.from("deadbeef", "hex"),
    });
    const undecodable = await snapshotUnreplaced(header);
    advanceL1ToSlot(h, ttl + 1);
    await h.synchronize();
    await nextPoint(h);
    await expectUnreplaced(header, undecodable);

    // Unsigned: there is no signed intent to reconcile.
    await updateJournal(header, {
      [C.INTENDED_TX_HASH]: null,
      [C.SIGNED_TX_CBOR]: null,
    });
    const unsigned = await snapshotUnreplaced(header);
    await nextPoint(h);
    await expectUnreplaced(header, unsigned);

    // The same state with its genuine signed intent is replaced at the next
    // point: the refusals above were the bytes'.
    await updateJournal(header, {
      [C.INTENDED_TX_HASH]: journal[C.INTENDED_TX_HASH],
      [C.SIGNED_TX_CBOR]: journal[C.SIGNED_TX_CBOR],
    });
    await nextPoint(h);
    await expectReplaced(journal, { handle: h });
    // The reopened deposit recommits on the restored base.
    const next = await commitNextBlock(h);
    const recommitted = await readJournal(next.submittedHeaderHash);
    expect(recommitted[C.BASE_UTXOS_ROOT]).toBe(journal[C.BASE_UTXOS_ROOT]);
    expect(recommitted.depositEventIds.map((id) => id.toString("hex"))).toEqual(
      journal.depositEventIds.map((id) => id.toString("hex")),
    );
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

/** The queue view of a chain on which another operator's block F, not E,
 * took E's base slot D: D's successor is F, and F is a node keyed by its own
 * header (E's header with another end time). E never lands on it. */
const foreignSuccessorView = async (h: Handle, journal: Pending.Record) => {
  const { policyId } = h.fixture.contracts.stateQueue;
  const header = journal[C.HEADER_HASH].toString("hex");
  const tx = CML.Transaction.from_cbor_bytes(journal[C.SIGNED_TX_CBOR]!);
  const body = tx.body();
  const ownUnit = policyId + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header;
  const own = Array.from({ length: body.outputs().len() }, (_, index) =>
    coreToTxOutput(body.outputs().get(index)),
  ).find((output) => output.assets[ownUnit] === 1n);
  body.free();
  tx.free();
  if (own?.datum == null) throw new Error("E's signed commit has no node");
  const view = SDK.linkedListDatumToNodeView(
    Data.from(own.datum, SDK.LinkedListDatum),
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
  );
  const node = Data.castFrom(view.data, SDK.StateQueueNode);
  const foreignHeader = { ...node.header, endTime: node.header.endTime + 1n };
  const foreignHash = await Effect.runPromise(
    SDK.hashBlockHeader(foreignHeader),
  );
  expect(foreignHash).not.toBe(header);
  const foreignTx = "fe".repeat(32);
  const assets = Object.fromEntries(
    Object.entries(own.assets).filter(([unit]) => unit !== ownUnit),
  );
  const foreignNode: LedgerSnapshotOutput = {
    txHash: foreignTx,
    outputIndex: 1,
    address: own.address,
    assets: {
      ...assets,
      [policyId + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + foreignHash]: 1n,
    },
    datum: SDK.encodeLinkedListNodeView({
      key: { Key: { key: foreignHash } },
      next: "Empty",
      data: SDK.castStateQueueNodeToData({
        ...node,
        header: foreignHeader,
      }) as never,
    }),
    hasReferenceScript: false,
  };
  const base = journal[C.BASE_TAIL_HEADER_HASH].toString("hex");
  const baseUnits = [
    policyId + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + base,
    policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME,
  ];
  return {
    foreignHash,
    rewrite: (outputs: readonly LedgerSnapshotOutput[]) => {
      const tail =
        outputs.find((output) => output.assets[baseUnits[0]!] === 1n) ??
        outputs.find((output) => output.assets[baseUnits[1]!] === 1n);
      if (tail?.datum === undefined) throw new Error("D is not in the view");
      const datum = Data.from(tail.datum, SDK.LinkedListDatum);
      expect(datum.link).toBeNull();
      const successor: LedgerSnapshotOutput = {
        ...tail,
        txHash: foreignTx,
        outputIndex: 0,
        datum: Data.to({ ...datum, link: foreignHash }, SDK.LinkedListDatum),
      };
      return [
        ...outputs.filter((output) => output !== tail),
        successor,
        foreignNode,
      ];
    },
  };
};

it("replaces a signed commit whose base slot a foreign block holds, never before its TTL", async () => {
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
    const untouched = await snapshotUnreplaced(header);
    const foreign = await foreignSuccessorView(h, journal);
    view.setRewrite(foreign.rewrite);
    // F holds D's slot before E's TTL: nothing is decided before the TTL.
    moveToExactSlot(h, ttl - 1);
    await h.synchronize();
    await expectUnreplaced(header, untouched);
    // At the TTL: F, not E, holds the slot, and this node has no journal for
    // F. E is replaced. (Kills "treat a foreign successor as undecidable".)
    moveToExactSlot(h, ttl);
    await h.synchronize();
    await expectReplaced(journal, { handle: h });
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
