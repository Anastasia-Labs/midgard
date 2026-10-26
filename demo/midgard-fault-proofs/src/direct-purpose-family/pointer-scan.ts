/** The bounded field-8 pointer scan shared by the observed route and the
 * claimed redeemer route: grammar certification, then the pointer walk. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { directRedeemerAt } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  type DirectPurposeFieldPlan,
  type DirectPurposeTransition,
  grammarPages,
  pages,
} from "./custody.js";
import type { DirectPurposeSourceEvidence } from "./source-walk.js";

/** `rule.scan_batch`: field-8 items certified or walked per transaction. */
export const DIRECT_PURPOSE_SCAN_BATCH = 16;

export type RouteInputs = {
  readonly material: DirectScriptMaterial;
  readonly evidence: DirectPurposeSourceEvidence;
  readonly prover: string;
  readonly plan: (field: number) => DirectPurposeFieldPlan;
  readonly batch?: number;
};

/** The bound source and its source index, as the family entry carries them. */
export const boundIndex = (
  inputs: RouteInputs,
): SDK.DirectSourceBoundIndex => ({
  bound: inputs.material.bound,
  source_index: inputs.evidence.index,
});

/** Certify then walk field 8 for one purpose's pointer. The start transition
 * (observed start or claimed lookup) is the caller's. */
export const pointerScan = (
  inputs: RouteInputs,
  {
    label,
    authenticated,
    grammar,
    scan,
    datum,
    finish,
  }: {
    readonly label: string;
    readonly authenticated: SDK.DirectSourceSelection;
    readonly grammar: "observedGrammar" | "claimedRedeemerGrammar";
    readonly scan: "observedScan" | "claimedRedeemerScan";
    readonly datum: (state: SDK.DirectRedeemerScanState) => string;
    readonly finish: (decision: SDK.DirectRedeemerDecisionState) => {
      readonly next: DirectPurposeTransition["next"];
      readonly datum: string;
    };
  },
) => {
  const { material, plan } = inputs;
  const batch = inputs.batch ?? DIRECT_PURPOSE_SCAN_BATCH;
  const coordinate = authenticated.selection.coordinate;
  const count = plan(8).items.length;
  const match = directRedeemerAt(material.fields[8]!, coordinate);
  const out: DirectPurposeTransition[] = [];
  let state: SDK.DirectRedeemerScanState = {
    authenticated,
    scan: {
      cursor: plan(8).position("Certifying", 0).cursor,
      coordinate,
      found: false,
      complete: false,
    },
  };
  const initial = datum(state);
  const progress =
    (phase: "Certifying" | "Walking", start: number, budget: number) =>
    ({ indices, open }: Parameters<DirectPurposeTransition["redeemer"]>[0]) =>
      ({
        ...indices,
        opening: open(8),
        checkpoint_bytes: plan(8).position(phase, start).checkpointBytes,
        budget: BigInt(budget),
      }) satisfies SDK.DirectProgressArgs;
  for (const [start, budget] of grammarPages(count, batch)) {
    const last = start + budget >= count;
    state = {
      ...state,
      scan: {
        ...state.scan,
        cursor: plan(8).position(
          last ? "Walking" : "Certifying",
          last ? 0 : start + budget,
        ).cursor,
        complete: last && count === 0,
      },
    };
    const args = progress("Certifying", start, budget);
    out.push({
      label: `${label} grammar ${start.toString()}`,
      role: grammar,
      next: last ? scan : grammar,
      datum: datum(state),
      fields: [8],
      redeemer: (redeemerArgs) =>
        Data.to(
          { Continue: [args(redeemerArgs)] },
          SDK.MissingRedeemerGrammarRedeemer,
        ),
    });
  }
  for (const [start, budget] of pages(count, batch)) {
    const found =
      state.scan.found ||
      (match !== null &&
        match.item_index >= BigInt(start) &&
        match.item_index < BigInt(start + budget));
    state = {
      ...state,
      scan: {
        ...state.scan,
        cursor: plan(8).position("Walking", start + budget).cursor,
        found,
        complete: start + budget === count,
      },
    };
    const args = progress("Walking", start, budget);
    out.push({
      label: `${label} scan ${start.toString()}`,
      role: scan,
      next: scan,
      datum: datum(state),
      fields: [8],
      redeemer: (redeemerArgs) =>
        // The observed and claimed scan roles share one wire shape.
        Data.to(
          { Continue: [{ Scan: { args: args(redeemerArgs) } }] },
          SDK.MissingRedeemerScanRedeemer,
        ),
    });
  }
  const decision: SDK.DirectRedeemerDecisionState = {
    authenticated,
    redeemer_missing: !state.scan.found,
  };
  const finished = finish(decision);
  out.push({
    label: `${label} finish`,
    role: scan,
    next: finished.next,
    datum: finished.datum,
    fields: [],
    redeemer: ({ indices }) =>
      Data.to(
        { Continue: [{ Finish: indices }] },
        SDK.MissingRedeemerScanRedeemer,
      ),
  });
  return { initial, transitions: out, decision };
};
