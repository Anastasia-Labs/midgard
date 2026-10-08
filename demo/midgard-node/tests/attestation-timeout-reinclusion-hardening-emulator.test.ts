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

import { Effect, Ref } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  assertRewoundRemovalsStand,
  parseStateQueueCorrectionObserverState,
} from "../src/services/state-queue-correction-observer.js";
import {
  C,
  captureStoredState,
  failureText,
  holdLeaseAsCrashed,
  inspectObligation,
  inspectWithForeignSubmission,
  INTEGRITY_FAILURE,
  journalUpdate,
  nativeRoot,
  readDaTerminalOutcomes,
  readLeaseStatus,
  rebindObserverDeployment,
  retireCrashedLease,
  REWIND_DOMAIN,
  updateJournal,
  withCursorQueueNode,
  withoutConsumedOutRef,
  writeAndInspect,
  writeObserverState,
} from "./attestation-timeout-reinclusion-hardening-emulator.inspect-with-foreign-submission.js";
import {
  captureState,
  expectNoRewind,
  foreignManifest,
} from "./attestation-timeout-reinclusion-hardening-emulator.read-payload-surfaces.js";
import {
  closeLifecycle,
  commitNextBlock,
  type Lifecycle,
  observerRowRestore,
  openCorrectionRewindScenario,
  read,
  readJournal,
  readObserver,
  readObserverRow,
  readRecoveryPlans,
  readSqlLedgerRoot,
  restoreObserverRow,
} from "./helpers/correction-rewind-scenario.js";

it("abandons a removed block's unlanded descendant in the same repair and commits both reincluded deposits, and refuses a descendant it cannot prove never lands", async () => {
  const scenario = await openCorrectionRewindScenario({
    blocks: 2,
    unlandedTail: true,
  });
  const { h } = scenario;
  let crashedLease: string | undefined;
  try {
    const [removedHeader, childHeader] = scenario.headers as [string, string];
    const parent = await readJournal(removedHeader);
    const child = await readJournal(childHeader);
    expect(parent[C.STATUS]).toBe(Pending.Status.LocallyApplied);
    // Handed to L1 with its signed intent, never observed there.
    expect([
      Pending.Status.PendingSubmission,
      Pending.Status.SubmittedLocalFinalizationPending,
      Pending.Status.SubmittedUnconfirmed,
    ]).toContain(child[C.STATUS]);
    expect(child[C.SUBMITTED_TX_HASH]).not.toBeNull();
    expect(child[C.SIGNED_TX_CBOR]).not.toBeNull();
    expect(child[C.BASE_TAIL_HEADER_HASH].toString("hex")).toBe(removedHeader);
    expect(
      (await scenario.readQueue()).map(({ headerHash }) => headerHash),
    ).not.toContain(childHeader);

    // Before the removal is admitted, the child is not proven unlanded.
    const removal = await scenario.removeTail(removedHeader);
    expect(await inspectObligation(scenario)).toEqual({ kind: "none" });
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    const ready = {
      kind: "ready",
      members: [
        { headerHash: removedHeader, kind: "removed" },
        { headerHash: childHeader, kind: "unlanded" },
      ],
    };
    expect(await inspectObligation(scenario)).toEqual(ready);
    const untouched = await captureState(scenario);
    // The lost commit's process died holding its state-queue mutation lease.
    crashedLease = child[C.STATE_QUEUE_LEASE_TOKEN];
    await holdLeaseAsCrashed(crashedLease);
    expect(await readLeaseStatus(child[C.STATE_QUEUE_LEASE_TOKEN])).toBe(
      "active",
    );

    // Refused: a descendant handed to L1 whose signed bytes are gone cannot
    // be proven never to land. Nothing is rewound or abandoned, across
    // forward appends.
    const signed = await updateJournal(childHeader, {
      [C.INTENDED_TX_HASH]: null,
      [C.SIGNED_TX_CBOR]: null,
    });
    const unsigned = await inspectObligation(scenario);
    expect(unsigned.kind).toBe("blocked");
    expect("reason" in unsigned ? unsigned.reason : "").toContain(
      `descendant ${childHeader} of removed block ${removedHeader} is not removed by an admitted correction yet; it was handed to L1 without retained signed bytes, so it cannot be proven never to land`,
    );
    await scenario.nextSourceBlockWhileRefused();
    await scenario.nextSourceBlockWhileRefused();
    await expectNoRewind(scenario, untouched);

    // Refused: a descendant whose commit does not spend the queue node the
    // admitted correction consumed may still land. Its signed bytes come back
    // in the same write, so the obligation never proves in between (see
    // writeAndInspect).
    const baseTail = await updateJournal(childHeader, {
      ...signed,
      [C.BASE_TAIL_OUT_REF]: `${"ab".repeat(32)}#0`,
    });
    const elsewhere = await inspectObligation(scenario);
    expect(elsewhere.kind).toBe("blocked");
    expect("reason" in elsewhere ? elsewhere.reason : "").toContain(
      `its commit spends ${"ab".repeat(32)}#0, which the admitted correction of ${removedHeader} did not consume`,
    );
    await scenario.nextSourceBlockWhileRefused();
    await expectNoRewind(scenario, untouched);

    // The running node's commit fiber recorded the lost submission as in
    // flight and awaiting local finalization. Recorded before the restore:
    // the owner may rewind as soon as the obligation proves.
    Effect.runSync(
      Effect.all([
        Ref.set(h.globals.LOCAL_FINALIZATION_PENDING, true),
        Ref.set(
          h.globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          child[C.SUBMITTED_TX_HASH]!.toString("hex"),
        ),
      ]),
    );
    // Proven: the same repair rewinds to the removed block's base and
    // abandons both journals under the removal's admitted correction.
    expect(
      await writeAndInspect(
        scenario,
        journalUpdate(childHeader, {
          [C.BASE_TAIL_OUT_REF]: baseTail[C.BASE_TAIL_OUT_REF],
        }),
      ),
    ).toEqual(ready);
    await scenario.nextSourceBlock();
    const target = parent[C.BASE_UTXOS_ROOT];
    expect(await nativeRoot(h)).toBe(target);
    expect((await readSqlLedgerRoot()).root_hex).toBe(target);
    const digest = (await readObserver()).admitted[0]!.transitionDigest;
    for (const header of [removedHeader, childHeader]) {
      const journal = await readJournal(header);
      expect(journal[C.STATUS]).toBe(Pending.Status.Abandoned);
      expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBe(digest);
    }
    const plans = await readRecoveryPlans();
    expect(plans.map(({ state }) => state)).toEqual(["applied"]);
    expect(plans[0]!.intent.domain).toBe(REWIND_DOMAIN);
    expect(
      plans[0]!.intent.members?.map(({ headerHash }) => headerHash),
    ).toEqual([removedHeader, childHeader]);
    expect(plans[0]!.intent.targetRoot).toBe(target);
    expect(await inspectObligation(scenario)).toEqual({ kind: "none" });
    // Abandoned blocks can never be continued: their leases are retired with
    // them, and the local-finalization gate is reopened for the next block.
    for (const record of [parent, child])
      expect(await readLeaseStatus(record[C.STATE_QUEUE_LEASE_TOKEN])).not.toBe(
        "active",
      );
    expect(Effect.runSync(Ref.get(h.globals.LOCAL_FINALIZATION_PENDING))).toBe(
      false,
    );
    expect(
      Effect.runSync(Ref.get(h.globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH)),
    ).toBe("");
    // The next block lands on the rewound base with both deposits.
    const next = await commitNextBlock(h);
    const committed = await readJournal(next.submittedHeaderHash);
    expect(committed[C.BASE_UTXOS_ROOT]).toBe(target);
    expect(committed.depositEventIds.map((id) => id.toString("hex"))).toEqual(
      expect.arrayContaining(
        [...parent.depositEventIds, ...child.depositEventIds].map((id) =>
          id.toString("hex"),
        ),
      ),
    );
    expect(committed.depositEventIds).toHaveLength(2);
    expect((await scenario.readQueue()).at(-1)!.headerHash).toBe(
      next.submittedHeaderHash,
    );
  } finally {
    if (crashedLease !== undefined) await retireCrashedLease(crashedLease);
    await closeLifecycle(h);
  }
}, 900_000);

it("refuses each unproven step of an unlanded descendant's proof on its own reason, and proves it again once that step's input is restored", async () => {
  const scenario = await openCorrectionRewindScenario({
    blocks: 2,
    unlandedTail: true,
  });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const [removedHeader, childHeader] = scenario.headers as [string, string];
    const parent = await readJournal(removedHeader);
    const child = await readJournal(childHeader);
    const removal = await scenario.removeTail(removedHeader);
    await scenario.awaitRemovalFinality();
    expect(
      (await scenario.tick(scenario.h.globals)).admittedTransactionHashes,
    ).toEqual([removal.accepted.transaction.txHash]);
    const ready = {
      kind: "ready",
      members: [
        { headerHash: removedHeader, kind: "removed" },
        { headerHash: childHeader, kind: "unlanded" },
      ],
    };
    // The proof steps are exercised while no owner runs: a running owner
    // retries a blocked recovery on its own timer and would act on any window
    // in which the obligation proves.
    let exercised = false;
    const restarted = await scenario.h.restartRuntime({
      afterStop: async () => {
        expect(await inspectObligation(scenario)).toEqual(ready);
        const untouched = await captureStoredState(scenario);
        const notYet = `descendant ${childHeader} of removed block ${removedHeader} is not removed by an admitted correction yet`;
        const blockedReason = async () => {
          const obligation = await inspectObligation(scenario);
          expect(obligation.kind).toBe("blocked");
          return "reason" in obligation ? obligation.reason : "";
        };
        const childBaseTail = child[C.BASE_TAIL_OUT_REF];

        // A journal that records an L1 observation is not a lost submission.
        const status = await updateJournal(childHeader, {
          [C.STATUS]: Pending.Status.ObservedWaitingStability,
        });
        expect(await blockedReason()).toBe(
          `${notYet} (journal status ${Pending.Status.ObservedWaitingStability})`,
        );
        await updateJournal(childHeader, status);
        expect(await inspectObligation(scenario)).toEqual(ready);

        // A header the authenticated state queue holds landed.
        const observerRow = await readObserverRow();
        await writeObserverState(
          withCursorQueueNode(observerRow.state_record, {
            headerHash: childHeader,
            outRef: `${"cd".repeat(32)}#7`,
          }),
        );
        expect(await blockedReason()).toBe(
          `${notYet}; it is on the authenticated state queue`,
        );
        await restoreObserverRow(observerRow);
        expect(await inspectObligation(scenario)).toEqual(ready);

        // A retained signed commit that hashes to its own intent but spends
        // another input: the removed parent's own commit, which spends the
        // parent's base, not the queue node the correction consumed. (Only one
        // journal may record a submission hash, so this intent is unacknowledged.)
        expect(parent[C.SIGNED_TX_CBOR]).not.toBeNull();
        expect(parent[C.INTENDED_TX_HASH]).not.toBeNull();
        expect(parent[C.BASE_TAIL_OUT_REF]).not.toBe(childBaseTail);
        const intent = await updateJournal(childHeader, {
          [C.SUBMITTED_TX_HASH]: null,
          [C.PREPARED_TX_HASH]: parent[C.INTENDED_TX_HASH],
          [C.INTENDED_TX_HASH]: parent[C.INTENDED_TX_HASH],
          [C.SIGNED_TX_CBOR]: parent[C.SIGNED_TX_CBOR],
        });
        expect(await blockedReason()).toBe(
          `${notYet}; its retained signed commit does not spend ${childBaseTail}`,
        );
        await updateJournal(childHeader, intent);
        expect(await inspectObligation(scenario)).toEqual(ready);

        // A submission other than the retained intent. The schema refuses to
        // record one; the proof refuses it independently.
        const foreignSubmission = Buffer.alloc(32, 0xee);
        expect(
          await failureText(
            updateJournal(childHeader, {
              [C.SUBMITTED_TX_HASH]: foreignSubmission,
            }),
          ),
        ).toContain("pending_signed_ack_matches_intent");
        const foreign = await inspectWithForeignSubmission(
          scenario,
          childHeader,
          foreignSubmission,
        );
        expect(foreign.kind).toBe("blocked");
        expect("reason" in foreign ? foreign.reason : "").toBe(
          `${notYet}; it was submitted as ${foreignSubmission.toString("hex")}, not its retained signed intent`,
        );
        expect(await inspectObligation(scenario)).toEqual(ready);

        // A removal that did not consume the child's input cannot be recorded as
        // admitted: the authenticated transition's consumed set is its topology,
        // so the observer state no longer parses.
        const parentNode = (
          (await readObserver()).admitted[0] as unknown as {
            previousQueue: readonly {
              headerHash: string | null;
              outRef: string;
            }[];
          }
        ).previousQueue.find(({ headerHash }) => headerHash === removedHeader);
        expect(parentNode?.outRef).toBe(childBaseTail);
        await writeObserverState(
          withoutConsumedOutRef(observerRow.state_record, childBaseTail),
        );
        expect(await blockedReason()).toBe(
          "the observer state is non-canonical",
        );
        await restoreObserverRow(observerRow);
        expect(await inspectObligation(scenario)).toEqual(ready);
        expect(await readRecoveryPlans()).toEqual([]);
        expect(await captureStoredState(scenario)).toEqual(untouched);
        exercised = true;
      },
    });
    h = restarted;
    expect(exercised).toBe(true);
    // Proven once every input is restored: the restarted owner rewinds both.
    expect(await nativeRoot(restarted)).toBe(parent[C.BASE_UTXOS_ROOT]);
    for (const header of [removedHeader, childHeader])
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.Abandoned,
      );
    expect(
      (await readRecoveryPlans())[0]?.intent.members?.map(
        ({ headerHash }) => headerHash,
      ),
    ).toEqual([removedHeader, childHeader]);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

it("rewinds a removed block whose journal stopped at pending_submission with its signed intent, and refuses one that never signed", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  try {
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    expect(removed[C.SIGNED_TX_CBOR]).not.toBeNull();
    const removal = await scenario.removeTail(removedHeader);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    // The process stopped between handing the signed commit to L1 and
    // recording it.
    const submitted = await updateJournal(removedHeader, {
      [C.STATUS]: Pending.Status.PendingSubmission,
      [C.SUBMITTED_TX_HASH]: null,
    });
    const untouched = await captureState(scenario);
    // Refused: a journal that never signed cannot be the removed header.
    const signed = await updateJournal(removedHeader, {
      [C.INTENDED_TX_HASH]: null,
      [C.SIGNED_TX_CBOR]: null,
    });
    const unsigned = await inspectObligation(scenario);
    expect(unsigned.kind).toBe("blocked");
    expect("reason" in unsigned ? unsigned.reason : "").toBe(
      `removed block ${removedHeader} has journal status pending_submission without a signed intent, so this journal cannot be the removed header`,
    );
    await scenario.nextSourceBlockWhileRefused();
    await expectNoRewind(scenario, untouched);
    // Refused: a journal of another deployment is never this removal's block.
    // Its retained signed intent comes back in the same write, so the
    // obligation never proves in between (see writeAndInspect).
    const deployment = await updateJournal(removedHeader, {
      ...signed,
      [C.DEPLOYMENT_MANIFEST_ID]: foreignManifest(removed),
    });
    expect(await inspectObligation(scenario)).toEqual({
      kind: "blocked",
      reason: `removed block ${removedHeader} belongs to another deployment`,
    });
    await scenario.nextSourceBlockWhileRefused();
    await expectNoRewind(scenario, untouched);
    // Refused: an observer state bound to another deployment is never this
    // deployment's authority, even when it is internally consistent and sits
    // in this deployment's row. Written before the journal's deployment is
    // restored, so the obligation never proves in between.
    const observerRow = await readObserverRow();
    const foreignState = rebindObserverDeployment(
      (await readObserver()) as unknown as Record<string, unknown>,
      foreignManifest(removed),
    );
    expect(parseStateQueueCorrectionObserverState(foreignState)).not.toBeNull();
    await writeObserverState(foreignState);
    await updateJournal(removedHeader, {
      [C.DEPLOYMENT_MANIFEST_ID]: deployment[C.DEPLOYMENT_MANIFEST_ID],
    });
    expect(await inspectObligation(scenario)).toEqual({
      kind: "blocked",
      reason: "the observer state is non-canonical",
    });
    // Proven by its retained signed intent: rewound, abandoned, reincluded.
    expect(
      await writeAndInspect(scenario, observerRowRestore(observerRow)),
    ).toEqual({
      kind: "ready",
      members: [{ headerHash: removedHeader, kind: "removed" }],
    });
    await scenario.nextSourceBlock();
    expect(await nativeRoot(h)).toBe(removed[C.BASE_UTXOS_ROOT]);
    const journal = await readJournal(removedHeader);
    expect(journal[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      (await readObserver()).admitted[0]!.transitionDigest,
    );
    expect(submitted[C.STATUS]).toBe(Pending.Status.LocallyApplied);
    const next = await commitNextBlock(h);
    expect(
      (await readJournal(next.submittedHeaderHash)).depositEventIds.map((id) =>
        id.toString("hex"),
      ),
    ).toEqual(removed.depositEventIds.map((id) => id.toString("hex")));
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

it("accepts a rollback of an admitted removal whose rewind never ran, leaving every local effect untouched", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  try {
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    const removal = await scenario.removeTail(removedHeader);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    const before = await captureState(scenario);
    scenario.simulateRemovalRollback();
    const rolledBack = await scenario.tick(h.globals);
    expect(rolledBack.retractedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    expect((await readObserver()).admitted).toEqual([]);
    expect(await readRecoveryPlans()).toEqual([]);
    expect(await captureState(scenario)).toEqual(before);
    expect(before.journals).toEqual([
      { status: Pending.Status.LocallyApplied, digest: null },
    ]);
    expect(before.native).toBe(removed[C.EXPECTED_UTXOS_ROOT]);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

it("refuses a rollback of a removal after its rewind with an explicit integrity error and never persists the retracting view", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  try {
    const [removedHeader] = scenario.headers as [string];
    const removal = await scenario.removeTail(removedHeader);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    await scenario.nextSourceBlock();
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
    const observerBefore = await readObserverRow();
    const state = parseStateQueueCorrectionObserverState(
      (await readObserver()) as unknown,
    );
    if (state === null) throw new Error("The observer state must parse");
    // The save guard itself: the admitted view stands, one that stops
    // removing a rewound header is refused.
    await read(assertRewoundRemovalsStand(state));
    expect(
      await failureText(
        read(assertRewoundRemovalsStand({ ...state, admitted: [] })),
      ),
    ).toContain(
      `${INTEGRITY_FAILURE}: block ${removedHeader} was rewound out of the native ledger by an admitted correction, but the authenticated state-queue view no longer removes it`,
    );
    const rewound = await captureState(scenario);
    // The removal's authenticated DA outcome: its payload is owed no longer.
    const daOutcomes = await readDaTerminalOutcomes(removedHeader);
    expect(daOutcomes.map((row) => row.terminal_outcome)).toEqual(["removed"]);
    scenario.simulateRemovalRollback();
    for (let attempt = 0; attempt < 2; attempt += 1) {
      const failure = await failureText(scenario.tick(h.globals));
      expect(failure).toContain(
        `${INTEGRITY_FAILURE}: block ${removedHeader} was rewound out of the native ledger by an admitted correction`,
      );
      expect(await readObserverRow()).toEqual(observerBefore);
      expect(await captureState(scenario)).toEqual(rewound);
      // Refused before the outcome is revoked: it survives the refusal.
      expect(await readDaTerminalOutcomes(removedHeader)).toEqual(daOutcomes);
    }
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
