import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  replayValidationMachineEvent,
  type RejectedTx,
  type ValidationMachineLedgerEntry,
} from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Ledger from "../database/utils/ledger.js";
import {
  encodeTransactionRootValue,
  computeLedgerMpfRootFromLedgerEntries,
} from "./ledger-hydration.js";
import { eventKeyCbor } from "./trace-events.js";
import {
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
  encodeEventToStepValueCbor,
} from "./transition-cbor.js";
import { assertCanonicalTransitionPhaseOrder } from "./transition-trace.apply-trace-ledger-ops-to-mpf.js";

import {
  importedEventProgramSidecar,
  importedProgramMaterial,
} from "./verified-block-import.program-material.js";

export type ImportedBlockReplayContext = {
  readonly header: SDK.Header;
  readonly parentEntries: readonly Ledger.MinimalEntry[];
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
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
  readonly verifyForcedSource: (input: {
    readonly source: SDK.DaPayloadEntry;
    readonly canonicalTransactionCbor: Buffer;
    readonly rejection?: RejectedTx;
  }) => Effect.Effect<void, unknown>;
};

export const replayImportedBlockEvents = (
  input: ImportedBlockReplayContext & {
    readonly payload: SDK.DaPayload;
  },
) =>
  Effect.gen(function* () {
    const body = input.payload.block_body;
    const ledger = new Map(
      input.parentEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(entry[Ledger.Columns.OUTPUT]),
      ]),
    );
    const steps = body.transition_trace
      .map(([key, value]) => {
        const step = LucidData.from(
          value,
          SDK.TransitionStep,
        ) as SDK.TransitionStep;
        if (
          key !== encodeTransitionIntegerCbor(step.step_index).toString("hex")
        )
          throw new Error("foreign transition key mismatch");
        return { step, value };
      })
      .sort((a, b) => Number(a.step.step_index - b.step.step_index));
    yield* assertCanonicalTransitionPhaseOrder(
      steps.map(({ step }) => ({
        eventKey: step.event_key,
        phase: step.phase,
        ledgerOps: [],
      })),
    );
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
          throw new Error("foreign source event duplicates a key");
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
      throw new Error("foreign transaction preimage coverage mismatch");
    const material = importedProgramMaterial(body.cek_program_material);
    const reached = new Set<string>();
    let root = yield* computeLedgerMpfRootFromLedgerEntries(
      input.parentEntries,
    );
    for (const [index, { step, value }] of steps.entries()) {
      if (step.step_index !== BigInt(index) || step.schema_version !== 1n)
        throw new Error("foreign transition indices/schema mismatch");
      const key = (yield* eventKeyCbor(step.event_key)).toString("hex");
      const source = expectedSources.get(key);
      if (source === undefined || source.phase !== step.phase)
        throw new Error("foreign transition source coverage/phase mismatch");
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
          throw new Error(
            "foreign event lacks its canonical transaction preimage",
          );
        const cbor = Buffer.from(preimage, "hex");
        if (
          step.phase === "L2Transaction" &&
          encodeTransactionRootValue(cbor).toString("hex") !== source.source[1]
        )
          throw new Error("foreign transaction preimage source mismatch");
        const witnessEntries: ValidationMachineLedgerEntry[] = [...ledger].map(
          ([outRef, output]) => ({
            outRef: Buffer.from(outRef, "hex"),
            output,
          }),
        );
        const replay = yield* replayValidationMachineEvent({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          eventKeyCbor: Buffer.from(key, "hex"),
          canonicalTransactionCbor: cbor,
          programMaterialSidecarCbor: importedEventProgramSidecar({
            cbor,
            sourceKind: step.phase === "L2Transaction" ? "normal" : "forced",
            ledger,
            material,
            reached,
          }),
          ledgerWitnessEntries: witnessEntries,
          priorUtxosRoot: root,
          blockEndTimeMs: Number(input.header.endTime),
          expectedNetworkId: input.expectedNetworkId,
          minFeeA: input.minFeeA,
          minFeeB: input.minFeeB,
          blockSlot: input.blockSlot,
          sourceKind: step.phase === "L2Transaction" ? "normal" : "forced",
        });
        if (
          step.phase === "L2Transaction" &&
          replay.replayInput.transactionId.toString("hex") !== source.source[0]
        )
          throw new Error(
            "foreign normal transaction source key differs from canonical transaction id",
          );
        if (
          step.phase === "L2Transaction" &&
          replay.replayInput.expectedVerdict !== "accepted"
        )
          throw new Error("foreign normal transaction was rejected by replay");
        if (step.phase === "ForcedTransaction")
          yield* input.verifyForcedSource({
            source: source.source,
            canonicalTransactionCbor: cbor,
            rejection: replay.rejection,
          });
        for (const key of replay.statePatch.deletedOutRefs) ledger.delete(key);
        for (const [key, output] of replay.statePatch.upsertedOutRefs)
          ledger.set(key, output);
        const descriptor = replay.trace.tree.descriptor;
        const expected: SDK.ValidationTraceDescriptor = {
          schema_version: BigInt(descriptor.schemaVersion),
          machine_version: BigInt(descriptor.machineVersion),
          trace_root: descriptor.traceRoot.toString("hex"),
          step_count: BigInt(descriptor.stepCount),
          initial_state_hash: descriptor.initialStateHash.toString("hex"),
          terminal_state_hash: descriptor.terminalStateHash.toString("hex"),
          verdict: descriptor.verdict === "accepted" ? "Accepted" : "Rejected",
          rejection_code_hash: descriptor.rejectionCodeHash.toString("hex"),
        };
        if (
          descriptors.get(key) !==
          LucidData.to(expected, SDK.ValidationTraceDescriptor)
        )
          throw new Error(
            "foreign validation descriptor differs from deterministic replay",
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
              throw new Error(
                "foreign user event spends missing ledger output",
              );
          } else {
            if (ledger.has(key))
              throw new Error("foreign user event substitutes ledger output");
            ledger.set(key, output);
          }
        }
      }
      root = yield* computeLedgerMpfRootFromLedgerEntries(
        [...ledger].map(([key, output]) => ({
          [Ledger.Columns.OUTREF]: Buffer.from(key, "hex"),
          [Ledger.Columns.OUTPUT]: output,
        })),
      );
      const expectedStep = {
        ...step,
        pre_utxos_root: priorRoot,
        post_utxos_root: root,
      };
      if (value !== encodeTransitionStepCbor(expectedStep).toString("hex"))
        throw new Error("foreign transition differs from deterministic replay");
      if (
        mappings.get(key) !==
        encodeEventToStepValueCbor({
          step_index: BigInt(index),
          phase: step.phase,
        }).toString("hex")
      )
        throw new Error("foreign event-to-step mapping mismatch");
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
    if (reached.size !== material.length)
      throw new Error("foreign block CEK material contains unreachable nodes");
    if (
      expectedSources.size !== 0 ||
      mappings.size !== 0 ||
      descriptors.size !== 0
    )
      throw new Error(
        "foreign replay omitted committed events, mappings or validation descriptors",
      );
    return [...ledger].map(([key, output]) => ({
      [Ledger.Columns.OUTREF]: Buffer.from(key, "hex"),
      [Ledger.Columns.OUTPUT]: output,
    }));
  });
