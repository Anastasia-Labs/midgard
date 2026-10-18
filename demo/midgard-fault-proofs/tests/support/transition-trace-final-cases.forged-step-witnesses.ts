/**
 * The witnesses of the forged accepted-transaction steps that replace a key,
 * a value, a walk checkpoint or an output's routing while keeping every other
 * part of the step as the honest one gives it.
 */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  encodeMidgardSpendInputItem,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  mpfProofFromWitness,
  normalizedMpfRoot,
} from "../../src/transition-trace/detect.mpf-proof-from-witness.js";
import {
  encodeTransitionWalkCheckpoint,
  hashTransitionWalkCheckpoint,
  transitionFieldLayout,
} from "../../src/transition-trace/field-walk.js";
import { transitionOutputStepClaims } from "../../src/transition-trace/output-phases.js";
import type * as phasesModule from "../../src/transition-trace/phases.js";
import {
  transitionL2Replay,
  type TransitionProofInput,
} from "../../src/transition-trace/proof-material.js";

/**
 * `insert` with its key or value replaced, and the root it then gives from
 * the same pre-root: the proofs these cases forge are taken against an empty
 * ledger, where one proof serves every key.
 */
export const reinserted = (
  insert: SDK.LedgerInsertWitness,
  preRoot: string,
  replaced: Partial<Pick<SDK.LedgerInsertWitness, "key" | "value">>,
) => {
  const forged = { ...insert, ...replaced };
  const proof = mpfProofFromWitness({
    key: Buffer.from(forged.key, "hex"),
    value: Buffer.from(forged.value, "hex"),
    proof: forged.insert_proof,
    label: "forged output insert",
  });
  expect(normalizedMpfRoot(proof.verify(false), "forged pre-root")).toBe(
    preRoot,
  );
  return {
    insert: forged,
    root: normalizedMpfRoot(proof.verify(true), "forged next-root"),
  };
};

/** A whole phase-11 replay with its insert's key or value replaced. */
export const withInsert = (
  step: phasesModule.TransitionTracePhaseStep,
  preRoot: string,
  replaced: Partial<Pick<SDK.LedgerInsertWitness, "key" | "value">>,
  verifiers: NonNullable<phasesModule.TransitionTracePhaseStep["verifiers"]>,
): phasesModule.TransitionTracePhaseStep => {
  const redeemer = Data.from(step.redeemer, SDK.TransitionTraceOutputStep);
  const { insert, root } = reinserted(redeemer.insert, preRoot, replaced);
  return {
    state: { ...step.state, current_root: root },
    redeemer: Data.to({ ...redeemer, insert }, SDK.TransitionTraceOutputStep),
    verifiers,
  };
};

/** The ledger key of the transaction's output after `state`'s. */
export const nextOutputKey = (state: SDK.TransitionTraceFinalState) =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(state.tx_id, "hex"),
    outputIndex: Number(state.output_index) + 1,
  }).toString("hex");

/**
 * The delete witness of the second spend input against the ledger before any
 * delete, where every spend input is still present.
 */
export const secondInputDelete = async (
  proof: TransitionProofInput,
): Promise<SDK.LedgerDeleteWitness> => {
  const spent = transitionL2Replay(proof)?.spentUtxos ?? [];
  const entries = spent.map(({ key, value }) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));
  const second = entries[1];
  if (second === undefined) throw new Error("deleteKey needs two spend inputs");
  const ledger = await Trie.fromList(entries);
  const proved = await ledger.prove(second.key);
  const opening = await buildMidgardMpfDeletionOpening(
    ledger,
    second.key,
    parseMidgardMpfProofJson(proved.toJSON()),
  );
  return {
    key: second.key.toString("hex"),
    value: second.value.toString("hex"),
    opening: opening.toString("hex"),
    delete_proof: Data.from(proved.toCBOR().toString("hex"), SDK.Proof),
  };
};

export type PhaseInput = Parameters<
  typeof phasesModule.nextTransitionTracePhase
>[0];

/** The phase-11 step of `input` routed the other way. */
export const rerouted = (
  input: PhaseInput,
  step: phasesModule.TransitionTracePhaseStep,
): phasesModule.TransitionTracePhaseStep => {
  const { state } = input;
  const opened = input.yields.find((item) => item.outputCbors !== undefined);
  const output = opened?.outputCbors?.[Number(state.output_index)];
  if (output === undefined) throw new Error("Transition output bytes missing");
  if (step.verifiers !== undefined)
    return {
      state: {
        ...state,
        phase: 2n,
        outputs_walk: step.state.outputs_walk,
        output_hash: computeHash32(Buffer.from(output, "hex")).toString("hex"),
      },
      redeemer: step.redeemer,
    };
  const summaries = opened?.outputSummaries?.[Number(state.output_index)];
  const descriptor = opened?.outputDescriptors?.[Number(state.output_index)];
  if (summaries === undefined || descriptor === undefined)
    throw new Error("transition output summary missing");
  const { insert } = Data.from(step.redeemer, SDK.TransitionTraceOutputStep);
  const next = state.output_index + 1n;
  return {
    state: {
      ...state,
      phase: next === state.output_count ? 5n : 11n,
      outputs_walk: step.state.outputs_walk,
      output_index: next,
      current_root: reinserted(insert, state.current_root, {}).root,
    },
    redeemer: step.redeemer,
    verifiers: transitionOutputStepClaims({
      index: state.output_index,
      outputCbor: output,
      summaries,
      descriptorCbor: descriptor,
    }),
  };
};

/** A whole phase-11 replay resuming the outputs walk one output later. */
export const skippedWalk = (
  input: PhaseInput,
  step: phasesModule.TransitionTracePhaseStep,
): phasesModule.TransitionTracePhaseStep => {
  const outputs = transitionL2Replay(input.proof)?.outputCbors ?? [];
  const index = Number(input.state.output_index);
  expect(outputs[index + 1]).toBe(outputs[index]);
  const layout = transitionFieldLayout({
    txId: input.state.tx_id,
    fieldIndex: 2,
    items: outputs.map((item) => Buffer.from(item, "hex")),
  });
  const redeemer = Data.from(step.redeemer, SDK.TransitionTraceOutputStep);
  return {
    ...step,
    state: {
      ...step.state,
      outputs_walk: hashTransitionWalkCheckpoint(layout, index + 2),
    },
    redeemer: Data.to(
      {
        ...redeemer,
        walk: {
          ...redeemer.walk,
          checkpoint_bytes: encodeTransitionWalkCheckpoint(
            layout,
            index + 1,
          ).toString("hex"),
        },
      },
      SDK.TransitionTraceOutputStep,
    ),
  };
};
