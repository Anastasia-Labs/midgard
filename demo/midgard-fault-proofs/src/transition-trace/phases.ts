import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { mpfProofFromWitness, normalizedMpfRoot } from "./detect.js";
import {
  encodeTransitionGrammarCheckpoint,
  encodeTransitionWalkCheckpoint,
  hashTransitionGrammarCheckpoint,
  hashTransitionWalkCheckpoint,
  resolveTransitionCheckpointIndex,
  type TransitionFieldLayout,
  transitionFieldLayout,
} from "./field-walk.js";
import {
  nextTransitionDatumPhase,
  nextTransitionOutputPhase,
  transitionOutputIsWhole,
  transitionOutputStepClaims,
} from "./output-phases.js";
import { transitionTraceProofChunks } from "./proof-carriage.js";
import {
  readTransitionProof,
  transitionL2Replay,
  type TransitionProofInput,
  transitionReplayedTransaction,
} from "./proof-material.js";
import type { transitionTraceYieldData } from "./yield-data.js";
import type { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

/** The accepted-transaction field a phase opens: spend inputs or outputs. */
export type TransitionTraceField = 0 | 2;

/** Mirrors `accepted_transaction_yields.outputs_grammar_batch`. */
export const TRANSITION_OUTPUTS_GRAMMAR_BATCH = 32;

export const initialTransitionTraceState = (
  proofInput: TransitionProofInput,
): SDK.TransitionTraceFinalState => {
  const proof = readTransitionProof(proofInput);
  return {
    kind:
      "InvalidOneStepTransition" in proof.fault &&
      "ValidDepositTransition" in proof.fault.InvalidOneStepTransition.witness
        ? 1n
        : "InvalidOneStepTransition" in proof.fault &&
            "ValidForcedTransactionTransition" in
              proof.fault.InvalidOneStepTransition.witness
          ? 2n
          : 0n,
    phase: 6n,
    proof_commitment: { hash: transitionTraceProofChunks(proofInput).hash },
    tx_id: "",
    inputs_walk: "",
    outputs_walk: "",
    output_count: 0n,
    output_hash: "",
    output_index: 0n,
    current_root: "",
    summaries: { summaries: [] },
    scan_cbor: "",
    value_cbor: "",
    value_start: 0n,
    value_end: 0n,
    value_summary: null,
    descriptor_cbor: "",
    deposit_index: 0n,
    deposit_source_cbor: "",
    deposit_asset_count: 0n,
    datum_walk: null,
  };
};
export const transitionTracePhaseIsTerminal = (
  state: SDK.TransitionTraceFinalState,
) => state.phase === 5n;

const ACCEPTED_PHASE_YIELDS: Readonly<
  Record<string, keyof typeof TRANSITION_TRACE_YIELD_REFERENCES>
> = {
  "0": "l2Open",
  "1": "l2Delete",
  "10": "l2Outputs",
  "11": "l2OutputStep",
  "2": "l2Scan",
  "7": "l2Value",
  "8": "l2Summaries",
  "3": "l2Assembly",
  "4": "l2Replay",
  "5": "l2Replay",
};
const DEPOSIT_PHASE_YIELDS: Readonly<
  Record<string, keyof typeof TRANSITION_TRACE_YIELD_REFERENCES>
> = {
  "0": "depositProjection",
  "10": "depositProjection",
  "2": "depositScan",
  "7": "depositValue",
  "8": "depositSummaries",
  "3": "depositAssembly",
  "9": "depositReplay",
  "4": "depositReplay",
  "5": "depositReplay",
};

export const transitionTracePhaseYield = (
  state: SDK.TransitionTraceFinalState,
): keyof typeof TRANSITION_TRACE_YIELD_REFERENCES | null => {
  if (state.phase === 6n) return null;
  const key = (
    state.kind === 1n ? DEPOSIT_PHASE_YIELDS : ACCEPTED_PHASE_YIELDS
  )[state.phase.toString()];
  if (key === undefined)
    throw new Error("transition-trace checkpoint phase invalid");
  return key;
};

/**
 * The field a kind-0 or kind-2 phase opens through the field-opening door, if
 * any. Kind 2 (an accepted forced transaction) takes kind 0's phases.
 */
export const transitionTracePhaseField = (
  state: SDK.TransitionTraceFinalState,
): TransitionTraceField | null =>
  state.kind === 1n
    ? null
    : state.phase === 0n || state.phase === 1n
      ? 0
      : state.phase === 10n || state.phase === 11n
        ? 2
        : null;

/** Whether a kind-0 or kind-2 phase-11 step replays its output whole. */
export const transitionTraceOutputStepIsWhole = (
  state: SDK.TransitionTraceFinalState,
  outputCbors: readonly string[] | undefined,
): boolean => {
  if (state.kind === 1n || state.phase !== 11n) return false;
  const output = outputCbors?.[Number(state.output_index)];
  if (output === undefined) throw new Error("Transition output bytes missing");
  return transitionOutputIsWhole(output);
};

// One layout per field for the dispute's duration: every step re-derives its
// checkpoint position from it.
let cachedLayouts: { key: string; layout: TransitionFieldLayout }[] = [];
const fieldLayout = (
  txId: string,
  fieldIndex: TransitionTraceField,
  items: readonly string[],
): TransitionFieldLayout => {
  const key = `${txId}:${fieldIndex.toString()}:${items.join(",")}`;
  const cached = cachedLayouts.find((entry) => entry.key === key);
  if (cached !== undefined) return cached.layout;
  const layout = transitionFieldLayout({
    txId,
    fieldIndex,
    items: items.map((item) => Buffer.from(item, "hex")),
  });
  cachedLayouts = [
    ...cachedLayouts.filter((entry) => !entry.key.startsWith(`${txId}:`)),
    { key, layout },
  ].slice(-2);
  return layout;
};

export type TransitionTracePhaseStep = {
  readonly state: SDK.TransitionTraceFinalState;
  readonly redeemer: string;
  /** The two output-step verifiers' redeemers of a whole phase-11 replay. */
  readonly verifiers?: {
    readonly descriptor: string;
    readonly summary: string;
  };
};

export const nextTransitionTracePhase = ({
  state,
  proof: proofInput,
  yields,
  fieldOpening,
  fieldCarriage,
}: {
  state: SDK.TransitionTraceFinalState;
  proof: TransitionProofInput;
  yields: ReturnType<typeof transitionTraceYieldData>;
  /** The opening of `field` against this step's reference inputs. */
  fieldOpening?: (field: TransitionTraceField) => SDK.FieldOpening;
  /** The carriage of `field` against this step's reference inputs. */
  fieldCarriage?: (field: TransitionTraceField) => SDK.FieldCarriage;
}): TransitionTracePhaseStep => {
  const proof = readTransitionProof(proofInput);
  const opened = yields.find((item) => item.outputCbors !== undefined);
  if (state.phase === 2n || state.phase === 7n || state.phase === 8n) {
    const output = opened?.outputCbors?.[Number(state.output_index)];
    if (output === undefined)
      throw new Error("Transition output bytes missing");
    if (state.phase === 8n) {
      const summaries = opened?.outputSummaries?.[Number(state.output_index)];
      if (summaries === undefined)
        throw new Error("transition output summary missing");
      return nextTransitionDatumPhase(state, output, summaries);
    }
    return nextTransitionOutputPhase(
      state,
      output,
      yields.find((item) => item.depositAssetIndexes !== undefined)
        ?.depositAssetIndexes,
    );
  }
  const selected = transitionTracePhaseYield(state);
  const redeemer =
    yields.find((item) => item.key === selected)?.redeemer ?? Data.void();
  if (!("InvalidOneStepTransition" in proof.fault))
    throw new Error("transition-trace state disagrees with proof");
  const witness = proof.fault.InvalidOneStepTransition.witness;
  const l2 = transitionReplayedTransaction(witness);
  const deposit =
    "ValidDepositTransition" in witness ? witness.ValidDepositTransition : null;
  const replayMaterial = transitionL2Replay(proofInput);
  if (l2 !== null && replayMaterial === undefined)
    throw new Error(
      "Transition L2 replay requires its field items and ledger witnesses",
    );
  const produced =
    replayMaterial?.producedUtxos ??
    (deposit === null ? [] : [deposit.projected_utxo]);
  const trace = l2?.traceProof ?? deposit?.trace_proof;
  if (trace === undefined)
    throw new Error("transition-trace proof has no ledger replay");
  const replay = (
    item: SDK.LedgerInsertWitness | SDK.LedgerDeleteWitness,
    insert: boolean,
  ) => {
    const proof = mpfProofFromWitness({
      key: Buffer.from(item.key, "hex"),
      value: Buffer.from(item.value, "hex"),
      proof: "insert_proof" in item ? item.insert_proof : item.delete_proof,
      label: "transition checkpoint replay",
    });
    if (
      normalizedMpfRoot(proof.verify(!insert), "transition pre-root") !==
      state.current_root
    )
      throw new Error("transition checkpoint replay pre-root mismatch");
    return normalizedMpfRoot(proof.verify(insert), "transition next-root");
  };
  const requireField = (field: TransitionTraceField) => {
    if (l2 === null || replayMaterial === undefined)
      throw new Error("transition field walk requires an L2 transaction");
    if (fieldOpening === undefined || fieldCarriage === undefined)
      throw new Error("transition field walk requires its field opening");
    const items =
      field === 0 ? replayMaterial.spendInputCbors : replayMaterial.outputCbors;
    return {
      layout: fieldLayout(l2.txId, field, items),
      count: items.length,
    };
  };
  if (state.kind !== 1n)
    switch (state.phase) {
      case 6n:
        return { state: { ...state, phase: 0n }, redeemer };
      case 0n: {
        const { layout, count } = requireField(0);
        return {
          state: {
            ...state,
            phase: count === 0 ? 10n : 1n,
            tx_id: l2!.txId,
            inputs_walk: hashTransitionWalkCheckpoint(layout, 0),
            current_root: trace.value.pre_utxos_root,
          },
          redeemer: Data.to(fieldCarriage!(0), SDK.FieldCarriage),
        };
      }
      case 1n: {
        const { layout, count } = requireField(0);
        const index = resolveTransitionCheckpointIndex({
          layout,
          committed: state.inputs_walk,
          grammar: false,
        });
        const spent = replayMaterial!.spentUtxos[index];
        if (spent === undefined)
          throw new Error("transition spent witness missing");
        return {
          state: {
            ...state,
            phase: index + 1 === count ? 10n : 1n,
            inputs_walk: hashTransitionWalkCheckpoint(layout, index + 1),
            current_root: replay(spent, false),
          },
          redeemer: Data.to(
            {
              opening: fieldOpening!(0),
              checkpoint_bytes: encodeTransitionWalkCheckpoint(
                layout,
                index,
              ).toString("hex"),
              witnesses: [spent],
            },
            SDK.TransitionTraceDeleteStep,
          ),
        };
      }
      case 10n: {
        const { layout, count } = requireField(2);
        const from =
          state.outputs_walk === ""
            ? 0
            : resolveTransitionCheckpointIndex({
                layout,
                committed: state.outputs_walk,
                grammar: true,
                stride: TRANSITION_OUTPUTS_GRAMMAR_BATCH,
              });
        const to = Math.min(count, from + TRANSITION_OUTPUTS_GRAMMAR_BATCH);
        return {
          state:
            to === count
              ? {
                  ...state,
                  phase: count === 0 ? 5n : 11n,
                  outputs_walk: hashTransitionWalkCheckpoint(layout, 0),
                  output_count: BigInt(count),
                }
              : {
                  ...state,
                  outputs_walk: hashTransitionGrammarCheckpoint(layout, to),
                },
          redeemer: Data.to(
            {
              opening: fieldOpening!(2),
              checkpoint_bytes:
                state.outputs_walk === ""
                  ? ""
                  : encodeTransitionGrammarCheckpoint(layout, from).toString(
                      "hex",
                    ),
            },
            SDK.TransitionTraceWalkStep,
          ),
        };
      }
      case 11n: {
        const { layout } = requireField(2);
        const index = Number(state.output_index);
        const output = replayMaterial!.outputCbors[index];
        const insert = produced[index];
        if (output === undefined || insert === undefined)
          throw new Error("transition produced witness missing");
        const step = Data.to(
          {
            walk: {
              opening: fieldOpening!(2),
              checkpoint_bytes: encodeTransitionWalkCheckpoint(
                layout,
                index,
              ).toString("hex"),
            },
            insert,
          },
          SDK.TransitionTraceOutputStep,
        );
        const outputs_walk = hashTransitionWalkCheckpoint(layout, index + 1);
        if (!transitionTraceOutputStepIsWhole(state, opened?.outputCbors))
          return {
            state: {
              ...state,
              phase: 2n,
              outputs_walk,
              output_hash: computeHash32(Buffer.from(output, "hex")).toString(
                "hex",
              ),
            },
            redeemer: step,
          };
        const summaries = opened?.outputSummaries?.[index];
        const descriptor = opened?.outputDescriptors?.[index];
        if (summaries === undefined || descriptor === undefined)
          throw new Error("transition output summary missing");
        const next = state.output_index + 1n;
        return {
          state: {
            ...state,
            phase: next === state.output_count ? 5n : 11n,
            outputs_walk,
            output_index: next,
            current_root: replay(insert, true),
          },
          redeemer: step,
          verifiers: transitionOutputStepClaims({
            index: state.output_index,
            outputCbor: output,
            summaries,
            descriptorCbor: descriptor,
          }),
        };
      }
      default:
        break;
    }
  else
    switch (state.phase) {
      case 6n:
        return { state: { ...state, phase: 0n }, redeemer };
      case 0n: {
        const output = opened?.outputCbors?.[0];
        if (output === undefined || deposit === null)
          throw new Error("Transition deposit output missing");
        return {
          state: {
            ...state,
            phase: 2n,
            output_hash: computeHash32(Buffer.from(output, "hex")).toString(
              "hex",
            ),
            output_count: 1n,
            deposit_source_cbor:
              yields.find((item) => item.depositSourceCbor !== undefined)
                ?.depositSourceCbor ?? "",
            deposit_asset_count: BigInt(
              yields.find((item) => item.depositAssetCount !== undefined)
                ?.depositAssetCount ?? 0,
            ),
            deposit_index: deposit.source_membership.key.outputIndex,
            current_root: trace.value.pre_utxos_root,
          },
          redeemer,
        };
      }
      case 10n:
        return { state: { ...state, phase: 8n }, redeemer };
      case 9n:
        return { state: { ...state, phase: 4n }, redeemer };
      default:
        break;
    }
  switch (state.phase) {
    case 3n: {
      const descriptor =
        opened?.outputDescriptors?.[Number(state.output_index)];
      if (descriptor === undefined)
        throw new Error("Transition produced descriptor missing");
      return {
        state: {
          ...state,
          phase: state.kind === 1n ? 9n : 4n,
          descriptor_cbor: descriptor,
        },
        redeemer,
      };
    }
    case 4n: {
      const item = produced[Number(state.output_index)];
      if (item === undefined)
        throw new Error("transition produced witness missing");
      const index = state.output_index + 1n;
      return {
        state: {
          ...state,
          phase: index === state.output_count ? 5n : 11n,
          output_index: index,
          output_hash: "",
          current_root: replay(item, true),
          summaries: { summaries: [] },
          scan_cbor: "",
          value_cbor: "",
          value_start: 0n,
          value_end: 0n,
          value_summary: null,
          descriptor_cbor: "",
        },
        redeemer:
          state.kind === 1n ? redeemer : Data.to(item, SDK.LedgerInsertWitness),
      };
    }
    default:
      throw new Error("terminal transition checkpoint has no successor");
  }
};
