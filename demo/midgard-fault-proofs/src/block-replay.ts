/**
 * The block replay: a block's DA payload replayed over its parent's ledger.
 * Every event is re-run (L2 and forced transactions through the validation
 * machine, user events through the caller's L1 material), and every committed
 * transition and event-to-step mapping must equal the replay's canonical bytes.
 * Validation descriptors are derived data the header does not commit: each
 * replayed transaction needs one, canonical and bounded, carrying the replayed
 * verdict, and nothing more is asked of it (the DA committee's rule). The
 * node's foreign-block import and the watcher's ledger both run this one
 * function; each supplies the parent `Ledger` and its own L1 event material.
 *
 * The replay reads from the ledger only the out-refs the block's events name
 * (a transaction's spend and reference inputs and its outputs, a user event's
 * out-refs) and applies each event's changes with `Ledger.apply`, so its work
 * follows what the block touches, not the ledger's size. It returns the
 * ledger after the block.
 *
 * Faults of the payload are thrown as `BlockReplayFault` defects; the
 * callbacks' failures pass through, and a callback names a local fault of its
 * own with `ReplayLocalFault`. A failure of the ledger's `read` or `apply` is
 * a `ReplayLocalFault`. A defect of the event replay is a `ReplayLocalFault`
 * marked `held`.
 */
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import { replayValidationVerdictEvent } from "@al-ft/midgard-validation";
import type {
  MidgardCommittedScriptRun,
  RejectedTx,
} from "@al-ft/midgard-validation/types";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  encodeEventToStepValueCbor,
  encodeTransactionRootValue,
  encodeTransitionEventKeyCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
  transitionPhaseRank,
} from "./block-replay.encoding.js";
import {
  blockFault,
  isReplayLocalFault,
  locally,
  ReplayLocalFault,
} from "./block-replay.faults.js";
import {
  applyChanges,
  type BlockReplayLedgerChange,
  readOutputs,
  resolvableOutRefs,
} from "./block-replay.ledger.js";
import type { Ledger } from "./ledger-store/index.js";

export {
  encodeEventToStepValueCbor,
  encodeTransactionRootValue,
  encodeTransitionEventKeyCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionPhaseCbor,
  encodeTransitionStepCbor,
  transitionPhaseRank,
} from "./block-replay.encoding.js";
export {
  BlockReplayFault,
  isBlockReplayFault,
  isReplayLocalFault,
  locally,
  ReplayLocalFault,
} from "./block-replay.faults.js";
export {
  type BlockReplayLedgerChange,
  readLedgerOutput,
} from "./block-replay.ledger.js";
export {
  attributeWithdrawalClassificationFault,
  withdrawalClassificationInputReadable,
} from "./withdrawal-classification-fault.js";

/**
 * The verdict of a validation descriptor, which must be canonical for its
 * schema and a bounded V1 descriptor, as the DA committee requires (the SDK
 * decoder both run).
 */
const descriptorVerdict = (valueHex: string) =>
  SDKDa.decodeCanonicalValidationTraceDescriptorData(valueHex).verdict;

/** An L2 transaction a replay applied by its committed verdict. */
export type CommittedTransaction = Readonly<{
  transactionId: Buffer;
  canonicalTransactionCbor: Buffer;
  /** The outputs the transaction's spend and reference inputs resolved to
   * on the ledger it was replayed on (those the ledger held), keyed by
   * out-ref CBOR hex. */
  resolvedOutputs: ReadonlyMap<string, Buffer>;
}>;

export type BlockReplayContext = {
  readonly header: SDK.Header;
  /** The ledger the block starts from. */
  readonly parent: Ledger;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
  /** Authenticated commit clock, separate from coverage end. */
  readonly blockTimeMs: number;
  /** The slot mapping script contexts carry their validity range in. */
  readonly l1SlotConfig: L1SlotConfig;
  /** Observation only: emitted after a transition matches canonical replay,
   * with the out-refs whose output the event changed (`null`: removed).
   * The caller keeps it private until the entire block and prefix validate. */
  readonly onReplayedEvent?: (input: {
    readonly mutations: readonly BlockReplayLedgerChange[];
    readonly root: string;
  }) => void;
  /** Authenticated L1 event material; never take projection authority from
   * DA. Reads what it needs from `ledger` (`readLedgerOutput`). */
  readonly replayUserEvent: (input: {
    readonly step: SDK.TransitionStep;
    readonly source: SDKDa.DaPayloadEntry;
    readonly ledger: Ledger;
  }) => Effect.Effect<readonly BlockReplayLedgerChange[], unknown>;
  /**
   * Set: every L2 transaction is replayed by its committed Accepted verdict
   * and runs no script. Each script run is handed to `onScriptRun`, without
   * its script context, as the replay meets it (the callee need not keep
   * it), then the accepted
   * transaction to `onTransaction`, for a check after the replay. The
   * replay gives the event loop a turn after each event
   * (`yieldToEventLoop`). Unset: every script runs in the replay. Forced
   * transactions always replay in full.
   */
  readonly committedVerdicts?: Readonly<{
    onScriptRun: (run: MidgardCommittedScriptRun) => void;
    onTransaction: (transaction: CommittedTransaction) => void;
  }>;
  readonly verifyForcedSource: (input: {
    readonly source: SDKDa.DaPayloadEntry;
    readonly canonicalTransactionCbor: Buffer;
    readonly rejection?: RejectedTx;
  }) => Effect.Effect<void, unknown>;
};

export const replayBlockEvents = (
  input: BlockReplayContext & {
    readonly payload: SDKDa.DaPayload;
  },
) =>
  Effect.gen(function* () {
    const body = input.payload.block_body;
    let ledger = input.parent;
    const steps = body.transition_trace
      .map(([key, value]) => {
        const step = LucidData.from(
          value,
          SDK.TransitionStep,
        ) as SDK.TransitionStep;
        if (
          key !== encodeTransitionIntegerCbor(step.step_index).toString("hex")
        )
          throw blockFault("foreign transition key mismatch");
        return { step, value };
      })
      .sort((a, b) => Number(a.step.step_index - b.step.step_index));
    // The payload's own faults are defects: the importer calls them invalid.
    let lastRank = -1;
    for (const [index, { step }] of steps.entries()) {
      const rank = transitionPhaseRank(step.phase);
      if (rank < lastRank)
        throw blockFault(
          `Transition source events are not in canonical phase order at source index ${index.toString()}: phase=${step.phase}`,
        );
      lastRank = rank;
    }
    const expectedSources = new Map<
      string,
      { phase: SDK.TransitionPhase; source: SDKDa.DaPayloadEntry }
    >();
    const add = (
      entries: readonly SDKDa.DaPayloadEntry[],
      phase: SDK.TransitionPhase,
      keyFor: (key: string) => SDK.EventKey,
    ) => {
      for (const source of entries) {
        const key = LucidData.to(keyFor(source[0]), SDK.EventKey);
        if (expectedSources.has(key))
          throw blockFault("foreign source event duplicates a key");
        expectedSources.set(key, { phase, source });
      }
    };
    const outref = (key: string) =>
      LucidData.from(key, SDK.OutputReference) as SDK.OutputReference;
    add(body.withdrawals, "Withdrawal", (key) => ({
      WithdrawalEventKey: { withdrawal_id: outref(key) },
    }));
    add(body.forced_transactions, "ForcedTransaction", (key) => ({
      ForcedTransactionEventKey: { tx_order_id: outref(key) },
    }));
    add(body.transactions, "L2Transaction", (key) => ({
      L2TransactionEventKey: { tx_id: key },
    }));
    add(body.deposits, "Deposit", (key) => ({
      DepositEventKey: { deposit_id: outref(key) },
    }));
    const mappings = new Map(body.event_to_step);
    const descriptors = new Map(body.validation_traces);
    const transactionPreimages = new Map(body.transaction_preimages);
    const forcedPreimages = new Map(body.forced_transaction_preimages);
    if (
      transactionPreimages.size !== body.transactions.length ||
      forcedPreimages.size !== body.forced_transactions.length
    )
      throw blockFault("foreign transaction preimage coverage mismatch");
    for (const [index, { step, value }] of steps.entries()) {
      if (step.step_index !== BigInt(index) || step.schema_version !== 1n)
        throw blockFault("foreign transition indices/schema mismatch");
      const key = encodeTransitionEventKeyCbor(step.event_key).toString("hex");
      const source = expectedSources.get(key);
      if (source === undefined || source.phase !== step.phase)
        throw blockFault("foreign transition source coverage/phase mismatch");
      expectedSources.delete(key);
      const priorRoot = ledger.root;
      /** The event's changes, in order (`applyChanges`). */
      let changes: readonly BlockReplayLedgerChange[];
      let strict: boolean;
      /** Outputs already read from `ledger` this event. */
      let known = new Map<string, Buffer | undefined>();
      if (
        step.phase === "L2Transaction" ||
        step.phase === "ForcedTransaction"
      ) {
        const preimage = (
          step.phase === "L2Transaction"
            ? transactionPreimages
            : forcedPreimages
        ).get(source.source[0]);
        if (preimage === undefined)
          throw blockFault(
            "foreign event lacks its canonical transaction preimage",
          );
        const cbor = Buffer.from(preimage, "hex");
        if (
          step.phase === "L2Transaction" &&
          encodeTransactionRootValue(cbor).toString("hex") !== source.source[1]
        )
          throw blockFault("foreign transaction preimage source mismatch");
        // The order authenticates the forced envelope; undecodable items replay
        // to their committed rejection rather than failing program selection.
        if (step.phase === "ForcedTransaction")
          deriveMidgardForcedTxFaultEvidenceMaterial(cbor);
        const sourceKind = step.phase === "L2Transaction" ? "normal" : "forced";
        known = yield* readOutputs(ledger, resolvableOutRefs(cbor, sourceKind));
        const resolvedOutputs = new Map<string, Buffer>();
        for (const [outRef, output] of known)
          if (output !== undefined) resolvedOutputs.set(outRef, output);
        const committed =
          step.phase === "L2Transaction" ? input.committedVerdicts : undefined;
        // Replay runs Phase A over every carried Flat script, then resolves
        // execution sources against this event's pre-state in Phase B.
        // Script bytes are authenticated by the canonical preimage and ledger;
        // no separately published material participates in the verdict.
        const replay = yield* replayValidationVerdictEvent({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          canonicalTransactionCbor: cbor,
          ledgerWitnessEntries: [...resolvedOutputs].map(
            ([outRef, output]) => ({
              outRef: Buffer.from(outRef, "hex"),
              output,
            }),
          ),
          blockTimeMs: input.blockTimeMs,
          expectedNetworkId: input.expectedNetworkId,
          minFeeA: input.minFeeA,
          minFeeB: input.minFeeB,
          blockSlot: input.blockSlot,
          l1SlotConfig: input.l1SlotConfig,
          sourceKind,
          ...(committed === undefined
            ? {}
            : { committedScriptRun: committed.onScriptRun }),
        }).pipe(
          // What the event's bytes and ledger decide is a verdict or a typed
          // failure; a throw past them is this process's own fault, held on
          // the block for as long as it recurs.
          Effect.catchAllDefect((cause) =>
            Effect.die(
              isReplayLocalFault(cause)
                ? cause
                : new ReplayLocalFault({
                    message: `event replay failed locally: ${String(cause)}`,
                    cause,
                    held: true,
                  }),
            ),
          ),
        );
        if (
          step.phase === "L2Transaction" &&
          replay.transactionId.toString("hex") !== source.source[0]
        )
          throw blockFault(
            "foreign normal transaction source key differs from canonical transaction id",
          );
        if (step.phase === "L2Transaction" && replay.verdict !== "accepted")
          throw blockFault("foreign normal transaction was rejected by replay");
        committed?.onTransaction({
          transactionId: replay.transactionId,
          canonicalTransactionCbor: cbor,
          resolvedOutputs,
        });
        if (step.phase === "ForcedTransaction")
          yield* input.verifyForcedSource({
            source: source.source,
            canonicalTransactionCbor: cbor,
            rejection: replay.rejection,
          });
        changes = [
          ...replay.statePatch.deletedOutRefs.map((outRef) => ({
            key: outRef,
            output: null,
          })),
          ...[...replay.statePatch.upsertedOutRefs].map(([outRef, output]) => ({
            key: outRef,
            output,
          })),
        ];
        strict = false;
        const descriptor = descriptors.get(key);
        if (descriptor === undefined)
          throw blockFault("foreign event lacks its validation descriptor");
        if (descriptorVerdict(descriptor) !== replay.verdict)
          throw blockFault(
            "foreign validation descriptor verdict differs from the replayed verdict",
          );
        descriptors.delete(key);
      } else {
        changes = yield* input.replayUserEvent({
          step,
          source: source.source,
          ledger,
        });
        strict = true;
      }
      const next = yield* applyChanges(ledger, changes, known, strict);
      ledger = next.ledger;
      const root = ledger.root;
      const expectedStep = {
        ...step,
        pre_utxos_root: priorRoot,
        post_utxos_root: root,
      };
      if (value !== encodeTransitionStepCbor(expectedStep).toString("hex"))
        throw blockFault(
          "foreign transition differs from deterministic replay",
        );
      // The phase was read and encoded above: encoding it here is local.
      if (
        mappings.get(key) !==
        locally("event-to-step encoding", () =>
          encodeEventToStepValueCbor({
            step_index: BigInt(index),
            phase: step.phase,
          }).toString("hex"),
        )
      )
        throw blockFault("foreign event-to-step mapping mismatch");
      input.onReplayedEvent?.({ mutations: next.applied, root });
      mappings.delete(key);
      if (input.committedVerdicts !== undefined) yield* yieldToEventLoop;
    }
    if (
      expectedSources.size !== 0 ||
      mappings.size !== 0 ||
      descriptors.size !== 0
    )
      throw blockFault(
        "foreign replay omitted committed events, mappings or validation descriptors",
      );
    return ledger;
  });

/**
 * Gives the event loop a turn: resumes from a macrotask (`setImmediate`),
 * so pending timers and I/O run first. `Effect.yieldNow` resumes from a
 * microtask, which runs none of them.
 */
export const yieldToEventLoop: Effect.Effect<void> = Effect.async<void>(
  (resume) => {
    const pending = setImmediate(() => resume(Effect.void));
    return Effect.sync(() => clearImmediate(pending));
  },
);
