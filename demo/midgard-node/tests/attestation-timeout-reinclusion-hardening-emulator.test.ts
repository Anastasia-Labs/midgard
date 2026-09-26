import { createHash } from "node:crypto";
import { inspect } from "node:util";

import { compareCanonicalJsonKeys } from "@al-ft/midgard-core/canonical-json";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  REWIND_REJECT_CODE_DEPENDENT_INPUT,
  REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
} from "../src/services/state-queue-correction-ledger-restore.js";
import {
  assertRewoundRemovalsStand,
  parseStateQueueCorrectionObserverState,
} from "../src/services/state-queue-correction-observer.js";
import { inspectStateQueueCorrectionRewindObligation } from "../src/services/state-queue-correction-rewind.js";
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
  read,
  readAcceptanceTraces,
  readDeposits,
  readJournal,
  readObserver,
  readObserverRow,
  readRecoveryPlans,
  readSqlLedgerRoot,
  restoreObserverRow,
} from "./helpers/correction-rewind-scenario.js";

const C = Pending.Columns;
const REWIND_DOMAIN = "midgard-history-correction-rewind-intent-v1";
const INTEGRITY_FAILURE = "State-queue correction integrity failure";

type Scenario = Awaited<ReturnType<typeof openCorrectionRewindScenario>>;

/** Every message on a refusal's cause chain. */
const failureText = async (promise: Promise<unknown>) => {
  const caught = await promise.then(
    () => undefined,
    (error: unknown) => error,
  );
  if (caught === undefined) throw new Error("Expected a refusal");
  const seen = new Set<unknown>();
  const texts: string[] = [inspect(caught, { depth: 40 })];
  const walk = (value: unknown, depth: number) => {
    if (depth > 40 || value === null || typeof value !== "object") return;
    if (seen.has(value)) return;
    seen.add(value);
    if (value instanceof Error) texts.push(value.message);
    for (const key of Reflect.ownKeys(value))
      walk((value as Record<PropertyKey, unknown>)[key], depth + 1);
  };
  walk(caught, 0);
  return texts.join("\n");
};

const nativeRoot = async (handle: Pick<Scenario["h"], "evidence">) => {
  const native = (await handle.evidence()).native;
  if (native === undefined) throw new Error("Native owner is not open");
  return native.durableRoot;
};

/** The DA terminal outcomes recorded for `headerHash`. */
const readDaTerminalOutcomes = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<Record<string, unknown>>`
        SELECT * FROM da_payload_terminal_outcomes
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}
        ORDER BY transaction_hash`;
    }),
  );

const inspectObligation = (scenario: Scenario) =>
  read(inspectStateQueueCorrectionRewindObligation(scenario.authority));

/** Direct journal surgery: the adversarial or broken local state a proof
 * must refuse. Returns the previous values so the test can restore them. */
const updateJournal = (
  headerHash: string,
  fields: Readonly<Record<string, unknown>>,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const key = Buffer.from(headerHash, "hex");
      const rows = yield* sql<Record<string, unknown>>`
        SELECT * FROM pending_block_finalizations
        WHERE header_hash = ${key}`;
      expect(rows).toHaveLength(1);
      const before = Object.fromEntries(
        Object.keys(fields).map((column) => [column, rows[0]![column]]),
      );
      const updated = yield* sql`UPDATE pending_block_finalizations
        SET ${sql.update(fields as Record<string, never>)}
        WHERE header_hash = ${key} RETURNING header_hash`;
      expect(updated).toHaveLength(1);
      return before;
    }),
  );

const readLeaseStatus = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ status: string }>`
        SELECT status FROM state_queue_mutation_leases WHERE token = ${token}`;
      expect(rows).toHaveLength(1);
      return rows[0]!.status;
    }),
  );

/**
 * A commit process killed after handing its block to L1 never releases its
 * state-queue mutation lease: it stays active, and blocks every other
 * state-queue mutation, until it expires.
 */
const holdLeaseAsCrashed = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const updated = yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'active', released_at = NULL,
          expires_at = NOW() + INTERVAL '10 minutes'
        WHERE token = ${token} RETURNING token`;
      expect(updated).toHaveLength(1);
    }),
  );

/** Retire an injected crash lease the test left active, so a failure never
 * blocks the shard's next state-queue mutation until the lease expires. */
const retireCrashedLease = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'released', released_at = NOW()
        WHERE token = ${token} AND status = 'active'`;
    }),
  );

/** The canonical key order every observer and transition digest hashes. */
const canonicalJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonicalJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => compareCanonicalJsonKeys(left, right))
    .map(([key, member]) => `${JSON.stringify(key)}:${canonicalJson(member)}`)
    .join(",")}}`;
};
const canonicalDigest = (value: unknown) =>
  createHash("sha256").update(canonicalJson(value)).digest("hex");

type DigestedRecord = Readonly<Record<string, unknown>> & {
  readonly transitionDigest: string;
};
const redigest = (record: Readonly<Record<string, unknown>>) => {
  const { transitionDigest: _prior, ...body } = record;
  return { ...body, transitionDigest: canonicalDigest(body) };
};

/**
 * An internally consistent observer state bound to `deployment`: every
 * transition and the state itself re-digested exactly as their producers do,
 * so only the deployment binding tells it apart from the canonical one.
 */
const rebindObserverDeployment = (
  state: Readonly<Record<string, unknown>>,
  deployment: string,
) => {
  const rebind = (transition: DigestedRecord) => {
    const nested = transition.correctionTransition as DigestedRecord | null;
    return redigest({
      ...transition,
      deploymentIdentityDigest: deployment,
      correctionTransition:
        nested === null
          ? null
          : redigest({ ...nested, deploymentIdentityDigest: deployment }),
    });
  };
  const { stateDigest: _prior, ...body } = state;
  const rebound = {
    ...body,
    deploymentIdentityDigest: deployment,
    pending: (state.pending as DigestedRecord[]).map(rebind),
    admitted: (state.admitted as DigestedRecord[]).map(rebind),
  };
  return { ...rebound, stateDigest: canonicalDigest(rebound) };
};

const writeObserverState = (state: Readonly<{ stateDigest: string }>) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const updated = yield* sql`UPDATE state_queue_terminal_observer_states
        SET state_digest = ${Buffer.from(state.stateDigest, "hex")},
          state_record = ${JSON.stringify(state)}
        RETURNING deployment_identity_digest`;
      expect(updated).toHaveLength(1);
    }),
  );

type ObserverRecord = Readonly<Record<string, unknown>> & {
  readonly cursorQueue: readonly unknown[];
  readonly admitted: readonly DigestedRecord[];
};
const observerRecord = (raw: unknown) =>
  (typeof raw === "string" ? JSON.parse(raw) : raw) as ObserverRecord;
const redigestState = (state: Readonly<Record<string, unknown>>) => {
  const { stateDigest: _prior, ...body } = state;
  return { ...body, stateDigest: canonicalDigest(body) };
};

/** The persisted observer state with one more node on its cursor queue,
 * re-digested exactly as the observer does. */
const withCursorQueueNode = (
  raw: unknown,
  node: Readonly<{ headerHash: string; outRef: string }>,
) => {
  const state = observerRecord(raw);
  return redigestState({ ...state, cursorQueue: [...state.cursorQueue, node] });
};

/** The persisted observer state whose admitted removal claims it did not
 * consume `outRef`, every digest recomputed. */
const withoutConsumedOutRef = (raw: unknown, outRef: string) => {
  const state = observerRecord(raw);
  const strip = (transition: DigestedRecord) => {
    const nested = transition.correctionTransition as DigestedRecord | null;
    const consumed = (transition.consumedQueueOutRefs as string[]).filter(
      (entry) => entry !== outRef,
    );
    return redigest({
      ...transition,
      consumedQueueOutRefs: consumed,
      correctionTransition:
        nested === null
          ? null
          : redigest({ ...nested, consumedQueueOutRefs: consumed }),
    });
  };
  return redigestState({ ...state, admitted: state.admitted.map(strip) });
};

class RolledBack<A> {
  constructor(readonly value: A) {}
}

/** Inspects the obligation while `headerHash`'s journal records `submitted`,
 * a submission other than its retained intent. The schema refuses to record
 * one, so the refusal is lifted in one transaction that is always rolled back:
 * the proof's own check is exercised and nothing outlives the inspection. */
const inspectWithForeignSubmission = (
  scenario: Scenario,
  headerHash: string,
  submitted: Buffer,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const outcome = yield* sql
        .withTransaction(
          Effect.gen(function* () {
            yield* sql.unsafe(`ALTER TABLE pending_block_finalizations
              DROP CONSTRAINT pending_signed_ack_matches_intent`);
            const updated = yield* sql`UPDATE pending_block_finalizations
              SET ${sql.update({ [C.SUBMITTED_TX_HASH]: submitted })}
              WHERE header_hash = ${Buffer.from(headerHash, "hex")}
              RETURNING header_hash`;
            expect(updated).toHaveLength(1);
            const obligation =
              yield* inspectStateQueueCorrectionRewindObligation(
                scenario.authority,
              );
            return yield* Effect.fail(new RolledBack(obligation));
          }),
        )
        .pipe(Effect.flip);
      if (!(outcome instanceof RolledBack)) return yield* Effect.fail(outcome);
      return outcome.value;
    }),
  );

/** Nothing moved: no plan, the same journals, native root and deposits. */
const expectNoRewind = async (
  scenario: Scenario,
  snapshot: Awaited<ReturnType<typeof captureState>>,
) => {
  expect(await readRecoveryPlans()).toEqual([]);
  expect(await captureState(scenario)).toEqual(snapshot);
};

const captureState = async (scenario: Scenario) => ({
  native: await nativeRoot(scenario.h),
  ...(await captureStoredState(scenario)),
});

/** The durable state a stopped runtime leaves: everything but the native
 * owner's root, which only a running generation can report. */
const captureStoredState = async (scenario: Scenario) => ({
  sql: (await readSqlLedgerRoot()).root_hex,
  deposits: await readDeposits(),
  journals: await Promise.all(
    scenario.headers.map(async (header) => {
      const journal = await readJournal(header);
      return {
        status: journal[C.STATUS],
        digest: journal[C.CORRECTION_TRANSITION_DIGEST] ?? null,
      };
    }),
  ),
});

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
    expect(parent[C.STATUS]).toBe(Pending.Status.Finalized);
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
    await updateJournal(childHeader, signed);

    // Refused: a descendant whose commit does not spend the queue node the
    // admitted correction consumed may still land.
    const baseTail = await updateJournal(childHeader, {
      [C.BASE_TAIL_OUT_REF]: `${"ab".repeat(32)}#0`,
    });
    const elsewhere = await inspectObligation(scenario);
    expect(elsewhere.kind).toBe("blocked");
    expect("reason" in elsewhere ? elsewhere.reason : "").toContain(
      `its commit spends ${"ab".repeat(32)}#0, which the admitted correction of ${removedHeader} did not consume`,
    );
    await scenario.nextSourceBlockWhileRefused();
    await expectNoRewind(scenario, untouched);
    await updateJournal(childHeader, baseTail);

    // Proven: the same repair rewinds to the removed block's base and
    // abandons both journals under the removal's admitted correction.
    expect(await inspectObligation(scenario)).toEqual(ready);
    // The running node's commit fiber recorded the lost submission as in
    // flight and awaiting local finalization.
    Effect.runSync(
      Effect.all([
        Ref.set(h.globals.LOCAL_FINALIZATION_PENDING, true),
        Ref.set(
          h.globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          child[C.SUBMITTED_TX_HASH]!.toString("hex"),
        ),
      ]),
    );
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
    // Proven by its retained signed intent: rewound, abandoned, reincluded.
    await updateJournal(removedHeader, signed);
    // Refused: a journal of another deployment is never this removal's block.
    const deployment = await updateJournal(removedHeader, {
      [C.DEPLOYMENT_MANIFEST_ID]: foreignManifest(removed),
    });
    expect(await inspectObligation(scenario)).toEqual({
      kind: "blocked",
      reason: `removed block ${removedHeader} belongs to another deployment`,
    });
    await scenario.nextSourceBlockWhileRefused();
    await expectNoRewind(scenario, untouched);
    await updateJournal(removedHeader, deployment);
    // Refused: an observer state bound to another deployment is never this
    // deployment's authority, even when it is internally consistent and sits
    // in this deployment's row.
    const observerRow = await readObserverRow();
    const foreignState = rebindObserverDeployment(
      (await readObserver()) as unknown as Record<string, unknown>,
      foreignManifest(removed),
    );
    expect(parseStateQueueCorrectionObserverState(foreignState)).not.toBeNull();
    await writeObserverState(foreignState);
    expect(await inspectObligation(scenario)).toEqual({
      kind: "blocked",
      reason: "the observer state is non-canonical",
    });
    await restoreObserverRow(observerRow);
    expect(await inspectObligation(scenario)).toEqual({
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
    expect(submitted[C.STATUS]).toBe(Pending.Status.Finalized);
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
      { status: Pending.Status.Finalized, digest: null },
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

const hex = (value: Buffer) => value.toString("hex");

/** Every local surface a removed block's payloads live on. */
const readPayloadSurfaces = (input: {
  readonly headerHash: string;
  readonly txIds: readonly Buffer[];
  readonly outRefs: readonly Buffer[];
  readonly forcedEventId: Buffer;
  readonly withdrawalEventId: Buffer;
}) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const header = Buffer.from(input.headerHash, "hex");
      const blockTxs = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM blocks WHERE header_hash = ${header}
        ORDER BY tx_id`;
      const immutable = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM immutable WHERE tx_id IN ${sql.in([...input.txIds])}`;
      const mempool = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM mempool`;
      const processed = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM processed_mempool`;
      const rejections = yield* sql<{ tx_id: Buffer; reject_code: string }>`
        SELECT tx_id, reject_code FROM tx_rejections
        WHERE tx_id IN ${sql.in([...input.txIds])}`;
      const ledger = yield* sql<{ outref: Buffer }>`
        SELECT outref FROM mempool_ledger
        WHERE outref IN ${sql.in([...input.outRefs])}`;
      const forced = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM forced_transaction_utxos
        WHERE tx_order_id = ${input.forcedEventId}`;
      const withdrawal = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM withdrawal_utxos
        WHERE event_id = ${input.withdrawalEventId}`;
      return {
        blockTxs: blockTxs.map((row) => hex(row.tx_id)),
        immutable: immutable.map((row) => hex(row.tx_id)).sort(),
        mempool: new Set(mempool.map((row) => hex(row.tx_id))),
        processed: new Set(processed.map((row) => hex(row.tx_id))),
        rejections: Object.fromEntries(
          rejections.map((row) => [hex(row.tx_id), row.reject_code]),
        ),
        ledger: new Set(ledger.map((row) => hex(row.outref))),
        forced: forced.map((row) => ({
          status: row.status,
          header: row.projected_header_hash?.toString("hex") ?? null,
        })),
        withdrawal: withdrawal.map((row) => ({
          status: row.status,
          header: row.projected_header_hash?.toString("hex") ?? null,
        })),
      };
    }),
  );

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

/** Another deployment's manifest id of the same shape. */
const foreignManifest = (record: Pending.Record) =>
  (record[C.DEPLOYMENT_MANIFEST_ID].startsWith("0") ? "1" : "0").concat(
    record[C.DEPLOYMENT_MANIFEST_ID].slice(1),
  );

/** Admit the removal of a one-block scenario's only block at release depth. */
const admitOnlyRemoval = async (scenario: Scenario) => {
  const [removedHeader] = scenario.headers as [string];
  const removal = await scenario.removeTail(removedHeader);
  await scenario.awaitRemovalFinality();
  expect(
    (await scenario.tick(scenario.h.globals)).admittedTransactionHashes,
  ).toEqual([removal.accepted.transaction.txHash]);
  return removedHeader;
};

const openNativeOwner = async (handle: Scenario["h"]) => {
  const owner = await Effect.runPromise(
    Ref.get(handle.globals.NATIVE_MPF_OWNER),
  );
  if (owner === undefined) throw new Error("Native owner is not open");
  return owner;
};

it("aborts the repair when the removed chain stops proving after the native root moved, and resumes the retained plan once it proves again", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    await admitOnlyRemoval(scenario);
    const sqlRoot = (await readSqlLedgerRoot()).root_hex;
    const deposits = await readDeposits();
    // Between the plan and the repair transaction, the journal stops being
    // this deployment's block. The repair re-proves the chain under its own
    // lock and refuses; nothing it would write commits.
    const owner = await openNativeOwner(scenario.h);
    const restore = owner.restoreCanonicalRoot.bind(owner);
    let deployment: Record<string, unknown> | undefined;
    owner.restoreCanonicalRoot = async (plan) => {
      await restore(plan);
      deployment = await updateJournal(removedHeader, {
        [C.DEPLOYMENT_MANIFEST_ID]: foreignManifest(removed),
      });
    };
    expect(await failureText(scenario.nextSourceBlock())).toContain(
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
    // Once the chain proves again, the restarted process resumes the retained
    // plan from the moved native root and completes it.
    const restarted = await scenario.h.restartRuntime({
      afterStop: async () => {
        await updateJournal(removedHeader, deployment!);
      },
    });
    h = restarted;
    expect(await nativeRoot(restarted)).toBe(removed[C.BASE_UTXOS_ROOT]);
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
    await closeLifecycle(h);
  }
}, 900_000);

it("refuses to resume a retained plan whose unlanded member stopped proving, and resumes it once the member proves again", async () => {
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
    const sqlRoot = (await readSqlLedgerRoot()).root_hex;
    const deposits = await readDeposits();
    // Between the plan and the repair transaction, the unlanded member's
    // journal records an L1 observation. The repair re-proves the retained
    // members under its own lock and refuses; nothing it would write commits.
    const owner = await openNativeOwner(scenario.h);
    const restore = owner.restoreCanonicalRoot.bind(owner);
    let observed: Record<string, unknown> | undefined;
    owner.restoreCanonicalRoot = async (plan) => {
      await restore(plan);
      observed ??= await updateJournal(childHeader, {
        [C.STATUS]: Pending.Status.ObservedWaitingStability,
      });
    };
    expect(await failureText(scenario.nextSourceBlock())).toContain(
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
    // Once the member proves again, the restarted process resumes the
    // retained plan and abandons both journals.
    const restarted = await scenario.h.restartRuntime({
      afterStop: async () => {
        await updateJournal(childHeader, observed!);
      },
    });
    h = restarted;
    expect(await nativeRoot(restarted)).toBe(parent[C.BASE_UTXOS_ROOT]);
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
    const next = await commitNextBlock(restarted);
    expect(
      (await readJournal(next.submittedHeaderHash)).depositEventIds
        .map(hex)
        .sort(),
    ).toEqual(
      [...parent.depositEventIds, ...child.depositEventIds].map(hex).sort(),
    );
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

it("refuses to rewind from a native root outside the removed chain without preparing a plan, and rewinds once the root is the chain's", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
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
    const failure = await failureText(scenario.nextSourceBlock());
    owner.diagnostics = diagnostics;
    expect(failure).toContain(
      `Native MPF durable root ${foreignRoot} is outside the removed chain ${removedHeader}; refusing to rewind`,
    );
    await expectNoRewind(scenario, untouched);
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    expect(await nativeRoot(restarted)).toBe(removed[C.BASE_UTXOS_ROOT]);
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

const INJECTED_PLAN_FAILURE = "injected crash while marking the rewind applied";

/** A database fault at the plan's final state change, inside the repair's own
 * transaction. */
const refusePlanApplication = (refuse: boolean) =>
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
