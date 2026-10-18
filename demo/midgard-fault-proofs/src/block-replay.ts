/**
 * The block replay: a block's DA payload replayed over its parent's ledger.
 * Every event is re-run (L2 and forced transactions through the validation
 * machine, user events through the caller's L1 material), and every committed
 * transition and event-to-step mapping must equal the replay's canonical bytes.
 * Validation descriptors are derived data the header does not commit: each
 * replayed transaction needs one, canonical and bounded, carrying the replayed
 * verdict, and nothing more is asked of it (the DA committee's rule). CEK
 * program material is likewise uncommitted: replay uses the entries an event
 * reaches and ignores the rest. The node's foreign-block import and the
 * watcher's ledger both run this one function; each supplies its own
 * ledger-root computation and L1 event material.
 *
 * Faults of the payload are thrown as `BlockReplayFault` defects; the
 * callbacks' and the root computation's failures pass through, and a
 * callback names a local fault of its own with `ReplayLocalFault`.
 */
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type MidgardCommittedScriptRun,
  type RejectedTx,
  replayValidationVerdictEvent,
  type ValidationMachineLedgerEntry,
} from "@al-ft/midgard-validation";
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
import { blockFault } from "./block-replay.faults.js";
import {
  importedEventProgramSidecar,
  importedProgramMaterial,
} from "./block-replay.program-material.js";

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
  attributeWithdrawalClassificationFault,
  withdrawalClassificationInputReadable,
} from "./withdrawal-classification-fault.js";

/**
 * The verdict of a validation descriptor, which must be canonical for its
 * schema and a bounded V1 descriptor, as the DA committee requires (the SDK
 * decoder both run).
 */
const descriptorVerdict = (valueHex: string) =>
  SDK.decodeCanonicalValidationTraceDescriptorData(valueHex).verdict;

/** One ledger entry: the out-ref key bytes and the full output CBOR. */
export type BlockReplayLedgerEntry = {
  readonly outRef: Buffer;
  readonly output: Buffer;
};

/** An L2 transaction a replay applied by its committed verdict. */
export type CommittedTransaction = Readonly<{
  transactionId: Buffer;
  canonicalTransactionCbor: Buffer;
  programMaterialSidecarCbor: Buffer;
  /** The ledger the transaction was replayed on, keyed by out-ref CBOR hex;
   * it changes as the replay goes on. */
  ledger: ReadonlyMap<string, Buffer>;
}>;

export type BlockReplayContext = {
  readonly header: SDK.Header;
  readonly parentEntries: readonly BlockReplayLedgerEntry[];
  /** The ledger's MPF root (the trie of canonical ledger descriptors). */
  readonly ledgerRoot: (
    entries: readonly BlockReplayLedgerEntry[],
  ) => Effect.Effect<string, unknown>;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
  /** Authenticated commit clock, separate from coverage end. */
  readonly blockTimeMs: number;
  /** The slot mapping script contexts carry their validity range in. */
  readonly l1SlotConfig: L1SlotConfig;
  /** Observation only: emitted after a transition matches canonical replay.
   * The caller keeps it private until the entire block and prefix validate. */
  readonly onReplayedEvent?: (input: {
    readonly mutations: readonly {
      readonly key: string;
      readonly output: Buffer | null;
    }[];
    readonly root: string;
  }) => void;
  /** Authenticated L1 event material; never take projection authority from DA. */
  readonly replayUserEvent: (input: {
    readonly step: SDK.TransitionStep;
    readonly source: SDK.DaPayloadEntry;
    readonly ledger: ReadonlyMap<string, Buffer>;
  }) => Effect.Effect<
    readonly { readonly key: string; readonly output: Buffer | null }[],
    unknown
  >;
  /**
   * Set: every L2 transaction is replayed by its committed Accepted verdict
   * and runs no script. Each script run is handed to `onScriptRun`, without
   * its script context, as the replay meets it (the callee need not keep
   * it), then the accepted
   * transaction to `onTransaction`, for a check after the replay. Unset:
   * every script runs in the replay. Forced transactions always replay in
   * full.
   */
  readonly committedVerdicts?: Readonly<{
    onScriptRun: (run: MidgardCommittedScriptRun) => void;
    onTransaction: (transaction: CommittedTransaction) => void;
  }>;
  readonly verifyForcedSource: (input: {
    readonly source: SDK.DaPayloadEntry;
    readonly canonicalTransactionCbor: Buffer;
    readonly rejection?: RejectedTx;
  }) => Effect.Effect<void, unknown>;
};

export const replayBlockEvents = (
  input: BlockReplayContext & {
    readonly payload: SDK.DaPayload;
  },
) =>
  Effect.gen(function* () {
    const body = input.payload.block_body;
    const ledger = new Map(
      input.parentEntries.map((entry) => [
        entry.outRef.toString("hex"),
        Buffer.from(entry.output),
      ]),
    );
    const entriesOf = (): BlockReplayLedgerEntry[] =>
      [...ledger].map(([key, output]) => ({
        outRef: Buffer.from(key, "hex"),
        output,
      }));
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
      { phase: SDK.TransitionPhase; source: SDK.DaPayloadEntry }
    >();
    const add = (
      entries: readonly SDK.DaPayloadEntry[],
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
    const material = importedProgramMaterial(body.cek_program_material);
    let root = yield* input.ledgerRoot(input.parentEntries);
    for (const [index, { step, value }] of steps.entries()) {
      if (step.step_index !== BigInt(index) || step.schema_version !== 1n)
        throw blockFault("foreign transition indices/schema mismatch");
      const key = encodeTransitionEventKeyCbor(step.event_key).toString("hex");
      const source = expectedSources.get(key);
      if (source === undefined || source.phase !== step.phase)
        throw blockFault("foreign transition source coverage/phase mismatch");
      expectedSources.delete(key);
      const priorRoot = root;
      const priorLedger =
        input.onReplayedEvent === undefined ? undefined : new Map(ledger);
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
        const witnessEntries: ValidationMachineLedgerEntry[] = [...ledger].map(
          ([outRef, output]) => ({
            outRef: Buffer.from(outRef, "hex"),
            output,
          }),
        );
        const sourceKind = step.phase === "L2Transaction" ? "normal" : "forced";
        const committed =
          step.phase === "L2Transaction" ? input.committedVerdicts : undefined;
        const programMaterialSidecarCbor = importedEventProgramSidecar({
          cbor,
          sourceKind,
          ledger,
          material,
        });
        const replay = yield* replayValidationVerdictEvent({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          canonicalTransactionCbor: cbor,
          programMaterialSidecarCbor,
          ledgerWitnessEntries: witnessEntries,
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
        });
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
          programMaterialSidecarCbor,
          ledger,
        });
        if (step.phase === "ForcedTransaction")
          yield* input.verifyForcedSource({
            source: source.source,
            canonicalTransactionCbor: cbor,
            rejection: replay.rejection,
          });
        for (const key of replay.statePatch.deletedOutRefs) ledger.delete(key);
        for (const [key, output] of replay.statePatch.upsertedOutRefs)
          ledger.set(key, output);
        const descriptor = descriptors.get(key);
        if (descriptor === undefined)
          throw blockFault("foreign event lacks its validation descriptor");
        if (descriptorVerdict(descriptor) !== replay.verdict)
          throw blockFault(
            "foreign validation descriptor verdict differs from the replayed verdict",
          );
        descriptors.delete(key);
      } else {
        const mutations = yield* input.replayUserEvent({
          step,
          source: source.source,
          ledger,
        });
        for (const { key, output } of mutations) {
          if (output === null) {
            if (!ledger.delete(key))
              throw blockFault(
                "foreign user event spends missing ledger output",
              );
          } else {
            if (ledger.has(key))
              throw blockFault("foreign user event substitutes ledger output");
            ledger.set(key, output);
          }
        }
      }
      root = yield* input.ledgerRoot(entriesOf());
      const expectedStep = {
        ...step,
        pre_utxos_root: priorRoot,
        post_utxos_root: root,
      };
      if (value !== encodeTransitionStepCbor(expectedStep).toString("hex"))
        throw blockFault(
          "foreign transition differs from deterministic replay",
        );
      if (
        mappings.get(key) !==
        encodeEventToStepValueCbor({
          step_index: BigInt(index),
          phase: step.phase,
        }).toString("hex")
      )
        throw blockFault("foreign event-to-step mapping mismatch");
      if (priorLedger !== undefined) {
        const mutations: { key: string; output: Buffer | null }[] = [];
        for (const [outRef, output] of priorLedger) {
          const after = ledger.get(outRef);
          if (after === undefined)
            mutations.push({ key: outRef, output: null });
          else if (!after.equals(output))
            mutations.push({ key: outRef, output: Buffer.from(after) });
        }
        for (const [outRef, output] of ledger)
          if (!priorLedger.has(outRef))
            mutations.push({ key: outRef, output: Buffer.from(output) });
        input.onReplayedEvent?.({ mutations: Object.freeze(mutations), root });
      }
      mappings.delete(key);
    }
    if (
      expectedSources.size !== 0 ||
      mappings.size !== 0 ||
      descriptors.size !== 0
    )
      throw blockFault(
        "foreign replay omitted committed events, mappings or validation descriptors",
      );
    return entriesOf();
  });
