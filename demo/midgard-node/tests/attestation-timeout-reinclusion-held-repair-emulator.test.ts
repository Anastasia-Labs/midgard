// The held, retained and rolled-back repairs of a correction rewind, split
// from attestation-timeout-reinclusion-hardening-emulator.test.ts so each file
// stays within the per-file budget. Every test opens its own scenario.
import "node:crypto";
import "node:util";
import "@al-ft/midgard-core/canonical-json";
import "@effect/sql";
import "effect";
import "vitest";
import "../src/database/pendingBlockFinalizations.js";
import "../src/services/state-queue-correction-ledger-restore.js";
import "../src/services/state-queue-correction-observer.js";
import "../src/services/state-queue-correction-rewind.js";
import "./helpers/correction-rewind-scenario.js";
import "./attestation-timeout-reinclusion-hardening-emulator.inspect-with-foreign-submission.js";
import "./attestation-timeout-reinclusion-hardening-emulator.read-payload-surfaces.js";

import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  REWIND_REJECT_CODE_DEPENDENT_INPUT,
  REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
} from "../src/services/state-queue-correction-ledger-restore.js";
import { blockedReasons } from "../src/services/state-queue-correction-rewind.load-retained-chain.js";
import {
  C,
  captureStoredState,
  failureText,
  nativeRoot,
  updateJournal,
} from "./attestation-timeout-reinclusion-hardening-emulator.inspect-with-foreign-submission.js";
import {
  admitOnlyRemoval,
  captureState,
  foreignManifest,
  hex,
  INJECTED_PLAN_FAILURE,
  openNativeOwner,
  readPayloadSurfaces,
  refusePlanApplication,
} from "./attestation-timeout-reinclusion-hardening-emulator.read-payload-surfaces.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  closeLifecycle,
  commitAndLocallyFinalizeNextBlock,
  commitNextBlock,
  CONTENT_AMOUNTS,
  depositorL2Utxos,
  flushWriteBehind,
  type Lifecycle,
  openCorrectionRewindScenario,
  outputOf,
  readAcceptanceTraces,
  readDeposits,
  readJournal,
  readRecoveryPlans,
  readSqlLedgerRoot,
} from "./helpers/correction-rewind-scenario.js";

it("returns a removed block's L2 transfer, withdrawal, forced transaction and deposit to the pending sets, rejects exactly the dependents of its reopened deposit, and re-commits everything on the rewound base", async () => {
  const scenario = await openCorrectionRewindScenario({
    blocks: 1,
    content: true,
  });
  const { h } = scenario;
  try {
    const content = scenario.removedContent!;
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    const transferId = hex(content.transfer.txId);
    // The removed block carries all four payload kinds.
    expect(
      removed.txMembers.map((member) =>
        hex(member[Pending.MemberColumns.MEMBER_ID]),
      ),
    ).toEqual([transferId]);
    expect(removed.withdrawalEventIds).toHaveLength(1);
    expect(removed.forcedTransactionEventIds.map(hex)).toEqual([
      hex(content.forced.eventId),
    ]);
    expect(removed.depositEventIds).toHaveLength(1);

    // Pending transactions admitted after the removed block: one on a merged
    // output (independent of the reopened deposit), one on the removed
    // block's deposit, and one on that one's output. (A pending spend of the
    // removed transfer's own output would re-commit as an intra-block chain,
    // whose net ledger delta is owned by a separate fix.)
    const onTransfer = await buildDepositorTransfer(
      h,
      [content.independentInput],
      2_000_000n,
    );
    expect(await admitTransfer(h, onTransfer)).toBe("accepted");
    const reopenedDepositOutputs = (await depositorL2Utxos(h)).filter(
      (utxo) => utxo.assets.lovelace === CONTENT_AMOUNTS.reopenedDeposit,
    );
    expect(reopenedDepositOutputs).toHaveLength(1);
    const reopenedDepositOutput = reopenedDepositOutputs[0]!;
    const onDeposit = await buildDepositorTransfer(
      h,
      [reopenedDepositOutput],
      6_000_000n,
    );
    expect(await admitTransfer(h, onDeposit)).toBe("accepted");
    const onDependent = await buildDepositorTransfer(
      h,
      [await outputOf(h, onDeposit, 6_000_000n)],
      2_000_000n,
    );
    expect(await admitTransfer(h, onDependent)).toBe("accepted");
    // Each acceptance left its durable traces: the accepted admission, its
    // persisted address history, and its batch's acceptance receipt.
    await flushWriteBehind(h);
    const acceptanceTraces = () =>
      readAcceptanceTraces({
        txIds: [onDeposit.txId, onDependent.txId],
        depositEventIds: removed.depositEventIds,
      });
    const accepted = await acceptanceTraces();
    expect(accepted.admissions).toEqual({
      [hex(onDeposit.txId)]: { status: "accepted", code: null },
      [hex(onDependent.txId)]: { status: "accepted", code: null },
    });
    expect(accepted.addressHistory).toEqual(
      [hex(onDeposit.txId), hex(onDependent.txId)].sort(),
    );
    expect(accepted.receipts).toEqual([
      { txIds: [hex(onDeposit.txId)], reversed: false },
      { txIds: [hex(onDependent.txId)], reversed: false },
    ]);
    const dependentOutRefs = (await depositorL2Utxos(h))
      .filter(
        (utxo) =>
          utxo.txHash === hex(onDeposit.txId) ||
          utxo.txHash === hex(onDependent.txId),
      )
      .map((utxo) => utxo.outrefCbor);
    expect(dependentOutRefs.length).toBeGreaterThanOrEqual(2);

    const surfaces = () =>
      readPayloadSurfaces({
        headerHash: removedHeader,
        txIds: [
          content.transfer.txId,
          onTransfer.txId,
          onDeposit.txId,
          onDependent.txId,
        ],
        outRefs: [
          content.withdrawn.outrefCbor,
          reopenedDepositOutput.outrefCbor,
          ...dependentOutRefs,
        ],
        forcedEventId: content.forced.eventId,
        withdrawalEventId: removed.withdrawalEventIds[0]!,
      });
    const before = await surfaces();
    expect(before.blockTxs).toEqual([transferId]);
    expect(before.immutable).toEqual([transferId]);
    expect(before.ledger.has(hex(content.withdrawn.outrefCbor))).toBe(false);
    expect(before.forced).toEqual([
      { status: "finalized", header: removedHeader },
    ]);

    const removal = await scenario.removeTail(removedHeader);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    await scenario.nextSourceBlock();
    const target = removed[C.BASE_UTXOS_ROOT];
    expect(await nativeRoot(h)).toBe(target);
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );

    const after = await surfaces();
    // F5: the removed block's transaction left ImmutableDB and BlocksDB and
    // is pending again; the independent descendant stays pending.
    expect(after.blockTxs).toEqual([]);
    expect(after.immutable).toEqual([]);
    expect(
      after.mempool.has(transferId) || after.processed.has(transferId),
    ).toBe(true);
    expect(
      after.mempool.has(hex(onTransfer.txId)) ||
        after.processed.has(hex(onTransfer.txId)),
    ).toBe(true);
    // F6: exactly the dependents of the reopened deposit are rejected, at
    // the exact rule, and leave no pending row or ledger output behind.
    expect(after.rejections).toEqual({
      [hex(onDeposit.txId)]: REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
      [hex(onDependent.txId)]: REWIND_REJECT_CODE_DEPENDENT_INPUT,
    });
    for (const rejected of [onDeposit, onDependent]) {
      expect(after.mempool.has(hex(rejected.txId))).toBe(false);
      expect(after.processed.has(hex(rejected.txId))).toBe(false);
    }
    for (const outRef of dependentOutRefs)
      expect(after.ledger.has(hex(outRef))).toBe(false);
    // The rejection undoes each acceptance whole, in the same transaction:
    // the admissions are terminally rejected at the same rule, the address
    // history is gone, and both receipts are reversed, so neither ledger
    // repair wedge (a published dependency on the reopened deposit, or an
    // unreversed receipt it cannot invert) is left behind.
    expect(await acceptanceTraces()).toEqual({
      admissions: {
        [hex(onDeposit.txId)]: {
          status: "rejected",
          code: REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
        },
        [hex(onDependent.txId)]: {
          status: "rejected",
          code: REWIND_REJECT_CODE_DEPENDENT_INPUT,
        },
      },
      rejections: after.rejections,
      addressHistory: [],
      receipts: accepted.receipts.map((receipt) => ({
        ...receipt,
        reversed: true,
      })),
      incompleteReceipts: [],
      publishedDependencies: [],
    });
    // The withdrawn output is back; the reopened deposit's output is in the
    // ledger again but not spendable until a block carries the deposit.
    expect(after.ledger.has(hex(content.withdrawn.outrefCbor))).toBe(true);
    expect(after.ledger.has(hex(reopenedDepositOutput.outrefCbor))).toBe(true);
    expect(
      (await depositorL2Utxos(h)).some((utxo) =>
        utxo.outrefCbor.equals(reopenedDepositOutput.outrefCbor),
      ),
    ).toBe(false);
    expect(after.forced).toEqual([{ status: "projected", header: null }]);
    expect(after.withdrawal).toHaveLength(1);
    expect(after.withdrawal[0]!.header).toBeNull();

    // The next block carries every reopened payload on the rewound base.
    const nextHeader = await commitAndLocallyFinalizeNextBlock(h);
    const next = await readJournal(nextHeader);
    expect(next[C.BASE_UTXOS_ROOT]).toBe(target);
    expect(
      next.txMembers
        .map((member) => hex(member[Pending.MemberColumns.MEMBER_ID]))
        .sort(),
    ).toEqual([transferId, hex(onTransfer.txId)].sort());
    expect(next.withdrawalEventIds.map(hex)).toEqual(
      removed.withdrawalEventIds.map(hex),
    );
    expect(next.forcedTransactionEventIds.map(hex)).toEqual(
      removed.forcedTransactionEventIds.map(hex),
    );
    expect(next.depositEventIds.map(hex)).toEqual(
      removed.depositEventIds.map(hex),
    );
    // Once carried again, the re-queued deposit's output is spendable.
    const again = (await depositorL2Utxos(h)).filter((utxo) =>
      utxo.outrefCbor.equals(reopenedDepositOutput.outrefCbor),
    );
    expect(again).toHaveLength(1);
    const respend = await buildDepositorTransfer(h, again, 7_000_000n);
    expect(await admitTransfer(h, respend)).toBe("accepted");
  } finally {
    await closeLifecycle(h);
  }
}, 1_200_000);

it("holds the repair when the removed chain stops proving after the native root moved, and resumes the retained plan in-process once it proves again", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  try {
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    await admitOnlyRemoval(scenario);
    const sqlRoot = (await readSqlLedgerRoot()).root_hex;
    const deposits = await readDeposits();
    // Between the plan and the repair transaction, the journal stops being
    // this deployment's block. The repair re-proves the chain under its own
    // lock and holds the gate closed; nothing it would write commits.
    const owner = await openNativeOwner(scenario.h);
    const restore = owner.restoreCanonicalRoot.bind(owner);
    let deployment: Record<string, unknown> | undefined;
    owner.restoreCanonicalRoot = async (plan) => {
      await restore(plan);
      deployment ??= await updateJournal(removedHeader, {
        [C.DEPLOYMENT_MANIFEST_ID]: foreignManifest(removed),
      });
    };
    await scenario.nextSourceBlockWhileRefused();
    expect([...blockedReasons.values()]).toContain(
      `Retained correction rewind ${removedHeader} is no longer provable: removed block ${removedHeader} belongs to another deployment`,
    );
    expect(deployment).toBeDefined();
    expect(await nativeRoot(scenario.h)).toBe(removed[C.BASE_UTXOS_ROOT]);
    expect((await readSqlLedgerRoot()).root_hex).toBe(sqlRoot);
    expect(await readDeposits()).toEqual(deposits);
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Finalized,
    );
    expect((await readRecoveryPlans()).map(({ state }) => state)).toEqual([
      "prepared",
    ]);
    // Once the chain proves again, the same process resumes the retained plan
    // from the moved native root and completes it.
    owner.restoreCanonicalRoot = restore;
    await updateJournal(removedHeader, deployment!);
    await scenario.nextSourceBlock();
    expect(blockedReasons.size).toBe(0);
    expect(await nativeRoot(scenario.h)).toBe(removed[C.BASE_UTXOS_ROOT]);
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      removed[C.BASE_UTXOS_ROOT],
    );
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
    expect((await readRecoveryPlans()).map(({ state }) => state)).toEqual([
      "applied",
    ]);
    const next = await commitNextBlock(scenario.h);
    expect(
      (await readJournal(next.submittedHeaderHash)).depositEventIds.map(hex),
    ).toEqual(removed.depositEventIds.map(hex));
  } finally {
    await closeLifecycle(scenario.h);
  }
}, 900_000);

it("holds a retained plan whose unlanded member stopped proving, and resumes it in-process once the member proves again", async () => {
  const scenario = await openCorrectionRewindScenario({
    blocks: 2,
    unlandedTail: true,
  });
  try {
    const [removedHeader, childHeader] = scenario.headers as [string, string];
    const parent = await readJournal(removedHeader);
    const child = await readJournal(childHeader);
    const removal = await scenario.removeTail(removedHeader);
    await scenario.awaitRemovalFinality();
    expect(
      (await scenario.tick(scenario.h.globals)).admittedTransactionHashes,
    ).toEqual([removal.accepted.transaction.txHash]);
    const sqlRoot = (await readSqlLedgerRoot()).root_hex;
    const deposits = await readDeposits();
    // Between the plan and the repair transaction, the unlanded member's
    // journal records an L1 observation. The repair re-proves the retained
    // members under its own lock and holds; nothing it would write commits.
    const owner = await openNativeOwner(scenario.h);
    const restore = owner.restoreCanonicalRoot.bind(owner);
    let observed: Record<string, unknown> | undefined;
    owner.restoreCanonicalRoot = async (plan) => {
      await restore(plan);
      observed ??= await updateJournal(childHeader, {
        [C.STATUS]: Pending.Status.ObservedWaitingStability,
      });
    };
    await scenario.nextSourceBlockWhileRefused();
    expect([...blockedReasons.values()]).toContain(
      `Retained correction rewind member ${childHeader} is no longer provably unlanded: descendant ${childHeader} of removed block ${removedHeader} is not removed by an admitted correction yet (journal status ${Pending.Status.ObservedWaitingStability})`,
    );
    expect(observed).toBeDefined();
    expect(await nativeRoot(scenario.h)).toBe(parent[C.BASE_UTXOS_ROOT]);
    expect((await readSqlLedgerRoot()).root_hex).toBe(sqlRoot);
    expect(await readDeposits()).toEqual(deposits);
    for (const header of [removedHeader, childHeader])
      expect((await readJournal(header))[C.STATUS]).not.toBe(
        Pending.Status.Abandoned,
      );
    const retained = await readRecoveryPlans();
    expect(retained.map(({ state }) => state)).toEqual(["prepared"]);
    expect(retained[0]!.intent.members).toEqual([
      expect.objectContaining({ headerHash: removedHeader, kind: "removed" }),
      expect.objectContaining({ headerHash: childHeader, kind: "unlanded" }),
    ]);
    // Once the member proves again, the same process resumes the retained
    // plan and abandons both journals.
    owner.restoreCanonicalRoot = restore;
    await updateJournal(childHeader, observed!);
    await scenario.nextSourceBlock();
    expect(blockedReasons.size).toBe(0);
    expect(await nativeRoot(scenario.h)).toBe(parent[C.BASE_UTXOS_ROOT]);
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      parent[C.BASE_UTXOS_ROOT],
    );
    for (const header of [removedHeader, childHeader])
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.Abandoned,
      );
    expect((await readRecoveryPlans()).map(({ state }) => state)).toEqual([
      "applied",
    ]);
    const next = await commitNextBlock(scenario.h);
    expect(
      (await readJournal(next.submittedHeaderHash)).depositEventIds
        .map(hex)
        .sort(),
    ).toEqual(
      [...parent.depositEventIds, ...child.depositEventIds].map(hex).sort(),
    );
  } finally {
    await closeLifecycle(scenario.h);
  }
}, 900_000);

it("holds the rewind from a native root outside the removed chain without preparing a plan, and rewinds in-process once the root is the chain's", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  try {
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    await admitOnlyRemoval(scenario);
    const untouched = await captureState(scenario);
    expect(untouched.native).toBe(removed[C.EXPECTED_UTXOS_ROOT]);
    // The retained native store reports a root no block of the removed chain
    // moved from or to: no base this rewind can prove it restores from.
    const foreignRoot = "ab".repeat(32);
    const owner = await openNativeOwner(scenario.h);
    const diagnostics = owner.diagnostics.bind(owner);
    owner.diagnostics = async () => ({
      ...(await diagnostics()),
      durableRoot: foreignRoot,
    });
    await scenario.nextSourceBlockWhileRefused();
    await scenario.nextSourceBlockWhileRefused();
    expect([...blockedReasons.values()]).toEqual([
      `Native MPF durable root ${foreignRoot} is outside the removed chain ${removedHeader}; refusing to rewind`,
    ]);
    const { native, ...stored } = untouched;
    expect(await readRecoveryPlans()).toEqual([]);
    expect(await captureStoredState(scenario)).toEqual(stored);
    expect((await diagnostics()).durableRoot).toBe(native);
    owner.diagnostics = diagnostics;
    await scenario.nextSourceBlock();
    expect(blockedReasons.size).toBe(0);
    expect(await nativeRoot(scenario.h)).toBe(removed[C.BASE_UTXOS_ROOT]);
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
  } finally {
    await closeLifecycle(scenario.h);
  }
}, 900_000);

it("rolls back every repair write when the plan cannot be marked applied, and completes the retained plan after a restart", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    await admitOnlyRemoval(scenario);
    const sqlRoot = (await readSqlLedgerRoot()).root_hex;
    const deposits = await readDeposits();
    await refusePlanApplication(true);
    expect(await failureText(scenario.nextSourceBlock())).toContain(
      INJECTED_PLAN_FAILURE,
    );
    // The native root moved; the journal abandonment, reinclusion and SQL
    // marker commit together with the plan's application or not at all.
    expect(await nativeRoot(scenario.h)).toBe(removed[C.BASE_UTXOS_ROOT]);
    expect((await readSqlLedgerRoot()).root_hex).toBe(sqlRoot);
    expect(await readDeposits()).toEqual(deposits);
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Finalized,
    );
    expect((await readRecoveryPlans()).map(({ state }) => state)).toEqual([
      "prepared",
    ]);
    const restarted = await scenario.h.restartRuntime({
      afterStop: () => refusePlanApplication(false),
    });
    h = restarted;
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      removed[C.BASE_UTXOS_ROOT],
    );
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
    expect((await readRecoveryPlans()).map(({ state }) => state)).toEqual([
      "applied",
    ]);
    const next = await commitNextBlock(restarted);
    expect(
      (await readJournal(next.submittedHeaderHash)).depositEventIds.map(hex),
    ).toEqual(removed.depositEventIds.map(hex));
  } finally {
    await refusePlanApplication(false);
    await closeLifecycle(h);
  }
}, 900_000);
