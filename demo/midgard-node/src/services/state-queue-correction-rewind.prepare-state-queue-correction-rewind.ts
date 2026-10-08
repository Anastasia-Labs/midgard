import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  type CorrectionRewindIntent,
  type CorrectionRewindMember,
  prepareCorrectionRewindRecoveryPlan,
  retainedPreparedRecoveryPlan,
} from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import * as StateQueueLeases from "../database/stateQueueMutationLeases.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { executeHistoryDependentRecovery } from "./history-dependent-recovery.js";
import {
  clearLivenessIncident,
  CORRECTION_REWIND_JOURNAL_UNBOUND,
  HISTORY_CORRECTION_REWIND_SOURCE,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.js";
import { reincludeStateQueueCorrectedBlocks } from "./state-queue-correction-recovery.js";
import {
  C,
  chainIdentity,
  failure,
  type Obligation,
  sha,
  type StateQueueCorrectionRewindAuthority,
} from "./state-queue-correction-rewind.admitted-removals.js";
import {
  blockedReasons,
  loadRetainedChain,
  logBlocked,
} from "./state-queue-correction-rewind.load-retained-chain.js";
import { loadObligation } from "./state-queue-correction-rewind.prove-unlanded.js";

/** A gate that is closed for now: aborts the transaction it is raised in
 * (nothing it wrote commits) and is caught below, never escaping the
 * preparation. The disposition stays pending (a prepared plan or an
 * unresolved removed header), so the owner backs off and re-evaluates. */
class Held {
  constructor(
    readonly reason: string,
    readonly nativeState = false,
    readonly journalUnbound = false,
  ) {}
}
const held = (reason: string, journalUnbound = false) =>
  Effect.fail(new Held(reason, false, journalUnbound));
/** Held on the native owner itself: it cannot open yet, or its durable root
 * is not one this rewind can prove it restores from. */
const heldOnNativeState = (reason: string) =>
  Effect.fail(new Held(reason, true));

/** What a preparation that held on the native owner's state returns. The
 * removed local suffix stays this rewind's to resolve, so no later recovery
 * step may act on that native root in the same pass (the landed-block rebase
 * would otherwise try to move it from a root it cannot place). */
export const CORRECTION_REWIND_HELD_ON_NATIVE_STATE =
  "correction_rewind_held_on_native_state" as const;

const recoverableOpenCodes = new Set([
  "LEVEL_LOCKED",
  "EAGAIN",
  "EBUSY",
  "EMFILE",
  "ENFILE",
  "ENOMEM",
]);

/** Why a native owner that failed to open may open on a later attempt: its
 * LevelDB lock is still held (a predecessor process or owner has not yet
 * released it) or the host is briefly out of a resource. Undefined for every
 * other cause (a binary digest or marker mismatch, a corrupt store), which
 * stays a failure. */
export const nativeOwnerOpenWait = (cause: unknown): string | undefined => {
  let current = cause;
  for (let depth = 0; depth < 8 && current instanceof Object; depth += 1) {
    const code = (current as { code?: unknown }).code;
    if (typeof code === "string" && recoverableOpenCodes.has(code))
      return `the native MPF owner could not open yet (${code}): ${current instanceof Error ? current.message : code}`;
    current = (current as { cause?: unknown }).cause;
  }
  return undefined;
};

/**
 * Recovery preparation: resumes a retained rewind plan, or proves a fresh
 * obligation and executes it. Returns without effect when nothing is owed,
 * when another domain's plan is retained (its owner resumes it first), or
 * when the obligation or its native owner is blocked (the disposition keeps
 * the gate closed, and the owner re-evaluates it after a backoff). A hold on
 * the native owner's state returns CORRECTION_REWIND_HELD_ON_NATIVE_STATE.
 * A hold on a removed block's journal that does not describe its removed
 * header raises `correction_rewind_journal_unbound` (readiness reports it);
 * every other outcome of an evaluation clears it.
 */
export const prepareStateQueueCorrectionRewind = (input: {
  readonly bindingDigest: string;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly config: NodeConfigDep;
  readonly authority: StateQueueCorrectionRewindAuthority;
}) =>
  Effect.gen(function* () {
    const { checkpoint, preparation, authority, config } = input;
    const globals = yield* Globals;
    const ready = (obligation: Obligation) =>
      obligation.kind === "ready"
        ? Effect.succeed(obligation)
        : obligation.kind === "blocked"
          ? held(obligation.reason, obligation.journalUnbound === true)
          : held("the removed chain is no longer owed");
    const owned = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      Authority.withRecovery(
        preparation.token,
        preparation.assertCurrent.pipe(
          Effect.zipRight(work),
          Effect.tap(() => preparation.assertCurrent),
        ),
      );
    yield* preparation.assertCurrent;
    const derived = yield* owned(
      Effect.gen(function* () {
        const retained = yield* retainedPreparedRecoveryPlan(
          input.bindingDigest,
        );
        // A signed-header recovery or a signed-intent release resumes its
        // own plan; the rewind waits for it.
        if (
          retained?.kind === "signed_header" ||
          retained?.kind === "signed_intent_release" ||
          retained?.kind === "displaced_block_revival" ||
          retained?.kind === "displacement_compensation"
        )
          return undefined;
        if (retained?.kind === "correction_rewind") {
          const chain = yield* ready(
            yield* loadRetainedChain(authority, retained.intent),
          );
          return { chain, retained: retained.intent };
        }
        const obligation = yield* loadObligation(authority);
        if (obligation.kind === "none") {
          yield* clearLivenessIncident(
            globals,
            HISTORY_CORRECTION_REWIND_SOURCE,
          );
          return yield* logBlocked(input.bindingDigest, undefined);
        }
        return { chain: yield* ready(obligation), retained: undefined };
      }),
    );
    if (derived === undefined) return;
    const proved = derived.chain;
    const records = proved.chain.map(({ record }) => record);
    const members: readonly CorrectionRewindMember[] = proved.chain.map(
      ({ record, transitionDigest, kind }) => ({
        headerHash: record[C.HEADER_HASH].toString("hex"),
        transitionDigest,
        kind,
      }),
    );
    const targetRoot = records[0]![C.BASE_UTXOS_ROOT];
    const acceptedRoots = [
      targetRoot,
      ...records.map((record) => record[C.EXPECTED_UTXOS_ROOT]),
    ];
    const journalDigest = chainIdentity(proved.chain);
    let owner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    if (owner === undefined) {
      // Open only retained native bytes; never genesis-bootstrap or replay a
      // removed block's journal on this path. Create validates the marker.
      owner = yield* Effect.uninterruptible(
        Effect.gen(function* () {
          const opened = yield* Effect.tryPromise({
            try: () =>
              ProductionNativeMpfOwnerService.create({
                levelPath: config.LEDGER_MPF_DB_PATH,
                binaryPath: config.MPF_NATIVE_OWNER_BINARY_PATH,
                binarySha256: config.MPF_NATIVE_OWNER_BINARY_SHA256,
                maxFrameBytes: config.MPF_NATIVE_OWNER_MAX_FRAME_BYTES,
                maxChunkBytes: config.MPF_NATIVE_OWNER_MAX_CHUNK_BYTES,
                requestTimeoutMs: config.MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS,
                restartLimit: config.MPF_NATIVE_OWNER_RESTART_LIMIT,
                sidecarPath: config.MPF_NATIVE_OWNER_SIDECAR_PATH,
              }),
            catch: (cause) => {
              const wait = nativeOwnerOpenWait(cause);
              return wait === undefined
                ? failure("Retained native rewind owner could not open", cause)
                : new Held(wait, true);
            },
          });
          yield* Ref.set(globals.NATIVE_MPF_OWNER, opened);
          return opened;
        }),
      );
    }
    yield* preparation.assertCurrent;
    const diagnostics = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        failure("Retained native rewind diagnostics failed", cause),
    });
    const durableRoot = diagnostics.durableRoot;
    let expectedRoot: string;
    if (derived.retained !== undefined) {
      expectedRoot = derived.retained.expectedRoot;
      if (durableRoot !== expectedRoot && durableRoot !== targetRoot)
        return yield* heldOnNativeState(
          `Native MPF durable root ${durableRoot} is neither the retained rewind base ${expectedRoot} nor its target ${targetRoot}`,
        );
    } else {
      // The native root is the removed chain's replay base (a crash before the
      // first promotion) or one of its blocks' roots. Anything else is not a
      // state this rewind can prove it restores from; never guess a base. The
      // refusal is held, not terminal: nothing is written and the root is
      // read again on every re-evaluation.
      if (!acceptedRoots.includes(durableRoot))
        return yield* heldOnNativeState(
          `Native MPF durable root ${durableRoot} is outside the removed chain ${members.map(({ headerHash }) => headerHash).join(",")}; refusing to rewind`,
        );
      expectedRoot = durableRoot;
    }
    const intent: CorrectionRewindIntent = {
      bindingDigest: input.bindingDigest,
      manifestId: checkpoint.manifestId,
      headerHash: members[0]!.headerHash,
      members,
      expectedRoot,
      targetRoot,
      journalDigest,
    };
    const evidenceDigest = sha(
      eventHistoryCanonicalJson({
        members,
        point: checkpoint.head,
        snapshot: checkpoint.capture.snapshotDigest,
      }),
    );
    // Re-proves the whole chain inside the caller's transaction with the
    // observer row held FOR SHARE: every removal is still admitted, every
    // unlanded descendant is still provably unlanded, and the journal identity
    // is unchanged. No observer save can retract a removal until the
    // transaction that acts on this proof commits.
    const recheck = loadRetainedChain(
      authority,
      derived.retained ?? intent,
      true,
    ).pipe(
      Effect.flatMap(ready),
      Effect.map(({ chain }) => chain),
    );
    const plan = yield* owned(
      recheck.pipe(
        Effect.zipRight(
          prepareCorrectionRewindRecoveryPlan(
            checkpoint,
            derived.retained ?? intent,
            evidenceDigest,
          ),
        ),
      ),
    );
    if (
      plan.intent.targetRoot !== targetRoot ||
      plan.intent.journalDigest !== journalDigest
    )
      return yield* Effect.fail(
        failure("Retained correction rewind identity changed"),
      );
    let submitted: Readonly<{ txHash: string; sinceMs: number }> | undefined;
    yield* executeHistoryDependentRecovery({
      checkpoint,
      preparation,
      plan,
      owner,
      repair: Effect.gen(function* () {
        const current = yield* recheck;
        const sql = yield* SqlClient.SqlClient;
        const results = yield* reincludeStateQueueCorrectedBlocks(
          current.map(({ record, transitionDigest, kind }) => ({
            headerHash: record[C.HEADER_HASH].toString("hex"),
            transitionDigest,
            kind,
          })),
        );
        if (
          results.length !== members.length ||
          results.some(({ journalFound }) => !journalFound)
        )
          return yield* Effect.fail(
            failure("Rewind reinclusion did not resolve every removed block"),
          );
        // Removed and unlanded blocks can never be continued; retire only
        // their own leases, atomically with their abandonment.
        for (const { record } of current)
          yield* StateQueueLeases.release(record[C.STATE_QUEUE_LEASE_TOKEN]);
        // The SQL marker follows the native root, which the plan's CAS already
        // proved. It was stamped by the latest journaled block, which may be
        // a later unsubmitted attempt, so it is replaced, not compared. The
        // aggregate is the replay base's own (its parent journal's) or none,
        // which makes the commit base recompute it from ledger entries.
        const aggregate = proved.parentAggregate;
        const engine = yield* sql`UPDATE mpf_engine_state
          SET root_hex = ${targetRoot},
            utxo_payload_entry_count = ${aggregate?.entryCount ?? null},
            utxo_payload_encoded_tuple_bytes = ${aggregate?.encodedTupleBytes ?? null},
            updated_at = NOW()
          WHERE store_name = 'ledger'
          RETURNING store_name`;
        if (engine.length !== 1)
          return yield* Effect.fail(
            failure("Native SQL marker row is missing"),
          );
        const active = yield* Pending.retrieveActive();
        submitted = Option.match(active, {
          onNone: () => undefined,
          onSome: (record) => ({
            txHash:
              (
                record[C.SUBMITTED_TX_HASH] ?? record[C.INTENDED_TX_HASH]
              )?.toString("hex") ?? "",
            sinceMs: record[C.UPDATED_AT].getTime(),
          }),
        });
      }),
      // The commit preflight re-derives the tail, boundary and any remaining
      // finalization from L1 and the journal once no finalization is pending.
      afterSqlCommit: Effect.gen(function* () {
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          submitted?.txHash ?? "",
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          submitted?.sinceMs ?? 0,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
      }),
    });
    blockedReasons.delete(input.bindingDigest);
    yield* clearLivenessIncident(globals, HISTORY_CORRECTION_REWIND_SOURCE);
    yield* Effect.logInfo(
      `State-queue correction rewind restored native root ${targetRoot} and reincluded block(s) ${members.map(({ headerHash, kind }) => `${headerHash}(${kind})`).join(",")}.`,
    );
  }).pipe(
    Effect.catchIf(
      (error): error is Held => error instanceof Held,
      ({ reason, nativeState, journalUnbound }) =>
        Effect.flatMap(Globals, (globals) =>
          (journalUnbound
            ? raiseLivenessIncident(
                globals,
                HISTORY_CORRECTION_REWIND_SOURCE,
                CORRECTION_REWIND_JOURNAL_UNBOUND,
                `${reason}. The rewind holds with native MPF, the SQL root and the journal unchanged, and the history gate stays closed, which holds block production; every evaluation re-derives it from the journal. Operator action is needed if the journal does not change.`,
                { escalateAfterMs: 0 },
              )
            : clearLivenessIncident(globals, HISTORY_CORRECTION_REWIND_SOURCE)
          ).pipe(
            Effect.zipRight(logBlocked(input.bindingDigest, reason)),
            Effect.as(
              nativeState ? CORRECTION_REWIND_HELD_ON_NATIVE_STATE : undefined,
            ),
          ),
        ),
    ),
  );
