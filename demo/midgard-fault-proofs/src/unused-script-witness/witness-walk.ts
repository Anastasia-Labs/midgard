/** The field-6 witness walk of the `unusedScriptWitness` thread, from the
 * completed all-purposes walk to the terminal's input: the witness entry
 * opens field 6, its grammar is certified, and the walk steps to the audited
 * witness, reads its script hash and stops, or exhausts the field before
 * it. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  grammarPages,
  progressArgs,
  stepDatum,
} from "../direct-purpose-family/custody.js";
import { directWitnessSource } from "../direct-script-derivation/sources.js";
import { UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL as LABEL } from "./contracts.js";
import type {
  UnusedScriptWitnessFieldPlan,
  UnusedScriptWitnessTransition,
} from "./custody.js";

/** The most field-6 items one grammar or witness-walk transaction takes;
 * the validators accept at most `scan_batch` (16). */
export type UnusedScriptWitnessFieldBatch = Readonly<{
  grammar: number;
  scan: number;
}>;

export const UNUSED_SCRIPT_WITNESS_FIELD_BATCH: UnusedScriptWitnessFieldBatch =
  Object.freeze({ grammar: 8, scan: 16 });

const WITNESS_FIELD = 6;

/** The script hash of the field-6 witness at `scriptIndex`; `null` when
 * field 6 ends before it. */
export const unusedScriptWitnessHashAt = (
  items: readonly Buffer[],
  scriptIndex: bigint,
): string | null => {
  if (scriptIndex < 0n || scriptIndex >= BigInt(items.length)) return null;
  const index = Number(scriptIndex);
  return directWitnessSource(items[index]!, index).descriptor.script_hash;
};

export type UnusedScriptWitnessFieldWalk = Readonly<{
  transitions: readonly UnusedScriptWitnessTransition[];
  /** The state the terminal reads. */
  final: SDK.UnusedScriptWitnessWalkState;
}>;

export const unusedScriptWitnessFieldWalk = ({
  scan,
  scriptIndex,
  prover,
  plan,
  batch = UNUSED_SCRIPT_WITNESS_FIELD_BATCH,
}: {
  /** The completed all-purposes walk. */
  readonly scan: SDK.DirectAllPurposesState;
  readonly scriptIndex: bigint;
  readonly prover: string;
  readonly plan: (field: number) => UnusedScriptWitnessFieldPlan;
  readonly batch?: UnusedScriptWitnessFieldBatch;
}): UnusedScriptWitnessFieldWalk => {
  const field = plan(WITNESS_FIELD);
  const count = field.items.length;
  const datum = (state: SDK.UnusedScriptWitnessWalkState) =>
    stepDatum(prover, state, SDK.UnusedScriptWitnessWalkDatum);
  const hash = unusedScriptWitnessHashAt(field.items, scriptIndex);
  // The walk stops after the audited witness, or at the field's end.
  const target = hash === null ? count : Number(scriptIndex) + 1;
  const out: UnusedScriptWitnessTransition[] = [];

  let state: SDK.UnusedScriptWitnessWalkState = {
    result: {
      bound: scan.cursor.bound,
      source_index: scan.source_index,
      facts: scan.facts,
    },
    script_index: scriptIndex,
    cursor: field.position("Certifying", 0).cursor,
    witness_hash: null,
    complete: false,
  };
  out.push({
    label: "audit entry",
    role: "witnessEntry",
    next: "witnessGrammar",
    datum: datum(state),
    fields: [WITNESS_FIELD],
    redeemer: ({ indices, open }) =>
      Data.to(
        {
          Continue: [
            {
              ...indices,
              script_index: scriptIndex,
              opening: open(WITNESS_FIELD),
            },
          ],
        },
        SDK.UnusedScriptWitnessWitnessEntryRedeemer,
      ),
  });

  // Grammar certification always takes at least one step; the last one
  // leaves the cursor walking from the first item.
  for (const [start, budget] of grammarPages(count, batch.grammar)) {
    const last = start + budget >= count;
    state = {
      ...state,
      cursor: last
        ? field.position("Walking", 0).cursor
        : field.position("Certifying", start + budget).cursor,
      complete: last && count === 0,
    };
    out.push({
      label: `audit grammar ${start.toString()}`,
      role: "witnessGrammar",
      next: last ? "witnessScan" : "witnessGrammar",
      datum: datum(state),
      fields: [WITNESS_FIELD],
      redeemer: (args) =>
        Data.to(
          {
            Continue: [progressArgs(field, "Certifying", start, budget)(args)],
          },
          SDK.UnusedScriptWitnessProgressRedeemer,
        ),
    });
  }

  // The walk takes at least one step, which over an empty field reads
  // nothing and hands the exhausted walk to the terminal.
  for (const [start, budget] of grammarPages(target, batch.scan)) {
    const end = Math.min(start + budget, count);
    const read = hash !== null && end === target;
    state = {
      ...state,
      cursor: field.position("Walking", end).cursor,
      witness_hash: read ? hash : null,
      complete: end === count,
    };
    const finished = read || state.complete;
    out.push({
      label: `audit walk ${start.toString()}`,
      role: "witnessScan",
      next: finished ? "terminal" : "witnessScan",
      datum: datum(state),
      fields: [WITNESS_FIELD],
      redeemer: (args) =>
        Data.to(
          { Continue: [progressArgs(field, "Walking", start, budget)(args)] },
          SDK.UnusedScriptWitnessProgressRedeemer,
        ),
    });
  }
  if (out.at(-1)!.next !== "terminal")
    throw new Error(`${LABEL}: the witness walk does not reach the terminal`);
  return { transitions: out, final: state };
};
