import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { mpfProofFromWitness, normalizedMpfRoot } from "./detect.js";
import { nextTransitionOutputPhase } from "./output-phases.js";
import { transitionTraceProofChunks } from "./proof-carriage.js";
import {
  readTransitionProof,
  type TransitionProofInput,
} from "./proof-material.js";
import type { transitionTraceYieldData } from "./yield-data.js";
import type { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

export const initialTransitionTraceState = (
  proofInput: TransitionProofInput,
): SDK.TransitionTraceFinalState => {
  const proof = readTransitionProof(proofInput);
  return {
    kind:
      "AcceptedTransactionTransitionMismatch" in proof.fault
        ? 2n
        : "InvalidOneStepTransition" in proof.fault &&
            "ValidDepositTransition" in
              proof.fault.InvalidOneStepTransition.witness
          ? 1n
          : 0n,
    phase: "AcceptedTransactionTransitionMismatch" in proof.fault ? 0n : 6n,
    proof_commitment: { hash: transitionTraceProofChunks(proofInput).hash },
    opened: { spend_input_keys: [], output_hashes: [] },
    input_index: 0n,
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
  };
};
export const transitionTracePhaseIsTerminal = (
  state: SDK.TransitionTraceFinalState,
) => state.phase === (state.kind === 2n ? 3n : 5n);
export const transitionTracePhaseYield = (
  state: SDK.TransitionTraceFinalState,
): keyof typeof TRANSITION_TRACE_YIELD_REFERENCES | null => {
  if (state.kind === 1n && state.phase === 10n) return "depositProjection";
  if (state.kind !== 2n && state.phase === 6n) return null;
  if (state.kind !== 2n && state.phase === 9n)
    return state.kind === 0n ? "l2Replay" : "depositReplay";
  if (state.kind !== 2n && state.phase === 7n)
    return state.kind === 0n ? "l2Value" : "depositValue";
  if (state.kind !== 2n && state.phase === 8n)
    return state.kind === 0n ? "l2Summaries" : "depositSummaries";
  if (state.kind === 2n)
    return ["claimStructure", "claimSource", "claimEndpoints", null][
      Number(state.phase)
    ] as "claimStructure" | "claimSource" | "claimEndpoints" | null;
  const keys =
    state.kind === 0n
      ? ([
          "l2Open",
          "l2Replay",
          "l2Scan",
          "l2Assembly",
          "l2Replay",
          "l2Replay",
        ] as const)
      : ([
          "depositProjection",
          null,
          "depositScan",
          "depositAssembly",
          "depositReplay",
          "depositReplay",
        ] as const);
  const key = keys[Number(state.phase)];
  if (key === undefined || key === null)
    throw new Error("transition-trace checkpoint phase invalid");
  return key;
};
export const nextTransitionTracePhase = ({
  state,
  proof: proofInput,
  yields,
}: {
  state: SDK.TransitionTraceFinalState;
  proof: TransitionProofInput;
  yields: ReturnType<typeof transitionTraceYieldData>;
}): { state: SDK.TransitionTraceFinalState; redeemer: string } => {
  const proof = readTransitionProof(proofInput);
  if (state.kind !== 2n && (state.phase === 2n || state.phase === 7n)) {
    const output = yields.find((item) => item.outputCbors !== undefined)
      ?.outputCbors?.[Number(state.output_index)];
    if (output === undefined)
      throw new Error("Transition output bytes missing");
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
  if (state.kind === 2n)
    return { state: { ...state, phase: state.phase + 1n }, redeemer };
  if (!("InvalidOneStepTransition" in proof.fault))
    throw new Error("transition-trace state disagrees with proof");
  const witness = proof.fault.InvalidOneStepTransition.witness;
  const l2 =
    "L2TransactionTransition" in witness
      ? witness.L2TransactionTransition
      : null;
  const deposit =
    "ValidDepositTransition" in witness ? witness.ValidDepositTransition : null;
  const projected =
    l2?.produced_utxos ?? (deposit === null ? [] : [deposit.projected_utxo]);
  const trace = l2?.trace_proof ?? deposit?.trace_proof;
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
  switch (state.phase) {
    case 10n:
      return { state: { ...state, phase: 8n }, redeemer };
    case 6n:
      return { state: { ...state, phase: 0n }, redeemer };
    case 0n:
      return {
        state: {
          ...state,
          phase: state.kind === 0n ? 1n : 2n,
          deposit_source_cbor:
            yields.find((item) => item.depositSourceCbor !== undefined)
              ?.depositSourceCbor ?? "",
          deposit_asset_count: BigInt(
            yields.find((item) => item.depositAssetCount !== undefined)
              ?.depositAssetCount ?? 0,
          ),
          opened: Data.from(redeemer, SDK.TransitionTraceOpenedOutputs),
          current_root: trace.value.pre_utxos_root,
          deposit_index: deposit?.source_membership.key.outputIndex ?? 0n,
        },
        redeemer,
      };
    case 1n: {
      if (l2 === null)
        throw new Error("deposit checkpoint cannot spend inputs");
      if (state.input_index === BigInt(state.opened.spend_input_keys.length))
        return {
          state: {
            ...state,
            phase: state.opened.output_hashes.length === 0 ? 5n : 2n,
          },
          redeemer,
        };
      const item = l2.spent_utxos[Number(state.input_index)];
      if (item === undefined)
        throw new Error("transition spent witness missing");
      return {
        state: {
          ...state,
          input_index: state.input_index + 1n,
          current_root: replay(item, false),
        },
        redeemer,
      };
    }
    case 8n: {
      const all = Data.from(redeemer, SDK.TransitionTraceOutputSummaries);
      const summary = all.summaries[Number(state.output_index)];
      if (summary === undefined)
        throw new Error("transition output summary missing");
      const summaries = { summaries: [summary] };
      return {
        state: { ...state, phase: 3n, summaries },
        redeemer: Data.to(summaries, SDK.TransitionTraceOutputSummaries),
      };
    }
    case 3n: {
      const item = projected[Number(state.output_index)];
      if (item === undefined)
        throw new Error("Transition produced descriptor missing");
      return {
        state: { ...state, phase: 9n, descriptor_cbor: item.value },
        redeemer,
      };
    }
    case 9n:
      return { state: { ...state, phase: 4n }, redeemer };
    case 4n: {
      const item = projected[Number(state.output_index)];
      if (item === undefined)
        throw new Error("transition produced witness missing");
      const index = state.output_index + 1n;
      return {
        state: {
          ...state,
          phase: index === BigInt(state.opened.output_hashes.length) ? 5n : 2n,
          output_index: index,
          current_root: replay(item, true),
          summaries: { summaries: [] },
          scan_cbor: "",
          value_cbor: "",
          value_start: 0n,
          value_end: 0n,
          value_summary: null,
          descriptor_cbor: "",
        },
        redeemer,
      };
    }
    default:
      throw new Error("terminal transition checkpoint has no successor");
  }
};
