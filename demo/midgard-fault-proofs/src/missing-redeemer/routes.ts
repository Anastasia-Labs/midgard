/** The observed purpose route and the claim custody route of the
 * `missingRedeemer` thread, from the family entry to the terminal datum. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { directPurposeAt } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { type MissingRedeemerTransition, stepDatum } from "./custody.js";
import { pointerScan, type RouteInputs } from "./pointer-scan.js";

type Coordinate = SDK.DirectPurposeCoordinate;

const OBSERVE = 2n;

export const isRedeemerBearingLanguage = (tag: bigint): boolean =>
  tag === 3n || tag === 128n;

const sameCoordinate = (left: Coordinate, right: Coordinate) =>
  left.kind === right.kind && left.index === right.index;

/** The leaf's own purpose coordinate; null when its reason names none. */
export const missingRedeemerLeafCoordinate = (
  material: DirectScriptMaterial,
): Coordinate | null => {
  const reason = material.bound.subject.rejection_reason;
  return reason === null ? null : SDK.purposeCoordinateOfReason(reason);
};

const boundIndex = (inputs: RouteInputs) => ({
  bound: inputs.material.bound,
  source_index: inputs.evidence.index,
});

/** Observe route over required observer `index`: the purpose, its source
 * lookup and its exhausted field-8 pointer scan, ending at claim start. */
export const missingRedeemerObservedRoute = async (
  inputs: RouteInputs,
  index: bigint,
) => {
  const { material, evidence, prover } = inputs;
  const coordinate = { kind: OBSERVE, index };
  const purpose = directPurposeAt(material, coordinate);
  if (purpose === null)
    throw new Error("missing-redeemer: observed purpose is absent");
  const { selected: source, proof } = await evidence.lookup(
    purpose.script_hash,
  );
  if (source === null || !isRedeemerBearingLanguage(source.language_tag))
    throw new Error("missing-redeemer: observed purpose has no redeemer");
  const selection: SDK.DirectPurposeSelection = {
    ...boundIndex(inputs),
    coordinate,
    purpose,
  };
  const authenticated = { selection, source };
  const scan = pointerScan(inputs, {
    label: "observed",
    authenticated,
    grammar: "observedGrammar",
    scan: "observedScan",
    datum: (state) => stepDatum(prover, state, SDK.MissingRedeemerScanDatum),
    finish: (decision) => {
      const context: SDK.MissingRedeemerClaimContext = {
        bound: boundIndex(inputs),
        observed: decision,
      };
      return {
        next: "claimStart",
        datum: stepDatum(prover, context, SDK.MissingRedeemerClaimStartDatum),
      };
    },
  });
  const transitions: MissingRedeemerTransition[] = [
    {
      label: "entry observe",
      role: "familyEntry",
      next: "purposeSelect",
      datum: stepDatum(
        prover,
        boundIndex(inputs),
        SDK.DirectPurposeSelectDatum,
      ),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Observe: indices }] },
          SDK.MissingRedeemerEntryRedeemer,
        ),
    },
    {
      label: "observe select",
      role: "purposeSelect",
      next: "sourceLookup",
      datum: stepDatum(prover, selection, SDK.DirectSourceLookupDatum),
      fields: [3],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                Observe: { ...indices, purpose_index: index, opening: open(3) },
              },
            ],
          },
          SDK.DirectPurposeSelectRedeemer,
        ),
    },
    {
      label: "observe lookup",
      role: "sourceLookup",
      next: "observedStart",
      datum: stepDatum(prover, authenticated, SDK.MissingRedeemerStartDatum),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ ...indices, source, proof }] },
          SDK.DirectSourceLookupRedeemer,
        ),
    },
    {
      label: "observed start",
      role: "observedStart",
      next: "observedGrammar",
      datum: scan.initial,
      fields: [8],
      redeemer: ({ indices, open }) =>
        Data.to(
          { Continue: [{ ...indices, opening: open(8) }] },
          SDK.MissingRedeemerStartRedeemer,
        ),
    },
    ...scan.transitions,
  ];
  return {
    transitions,
    context: {
      bound: boundIndex(inputs),
      observed: scan.decision,
    } satisfies SDK.MissingRedeemerClaimContext,
  };
};

/** Claim entry: the claimed-only context, no observation carried. */
export const missingRedeemerClaimEntry = (
  inputs: RouteInputs,
): {
  readonly transition: MissingRedeemerTransition;
  readonly context: SDK.MissingRedeemerClaimContext;
} => {
  const context = { bound: boundIndex(inputs), observed: null };
  return {
    context,
    transition: {
      label: "entry claim",
      role: "familyEntry",
      next: "claimStart",
      datum: stepDatum(
        inputs.prover,
        context,
        SDK.MissingRedeemerClaimStartDatum,
      ),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Claim: indices }] },
          SDK.MissingRedeemerEntryRedeemer,
        ),
    },
  };
};

/** Claim start and adjudication for the supported leaf shapes: no coordinate,
 * an invalid coordinate, the observed coordinate, or a distinct observer
 * coordinate. Spend, mint and receive coordinates fail closed. */
export const missingRedeemerClaimRoute = async (
  inputs: RouteInputs,
  context: SDK.MissingRedeemerClaimContext,
): Promise<{
  readonly transitions: readonly MissingRedeemerTransition[];
  readonly final: SDK.MissingRedeemerFinalState;
}> => {
  const { material, evidence, prover } = inputs;
  const coordinate = missingRedeemerLeafCoordinate(material);
  const observed = context.observed;
  const finalDatum = (final: SDK.MissingRedeemerFinalState) =>
    stepDatum(prover, final, SDK.MissingRedeemerFinalDatum);
  const adjudicateDatum = (purpose: SDK.MissingRedeemerClaimedPurposeFact) =>
    stepDatum(prover, { context, purpose }, SDK.MissingRedeemerAdjudicateDatum);
  const resolve = (
    purpose: SDK.MissingRedeemerClaimedPurposeFact,
    claim: SDK.MissingRedeemerClaimedPredicateFact,
  ) => {
    const final = { context, claim };
    return {
      final,
      transition: {
        label: "adjudicate resolve",
        role: "claimAdjudicate",
        next: "terminal",
        datum: finalDatum(final),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Resolve: indices }] },
            SDK.MissingRedeemerAdjudicateRedeemer,
          ),
      } satisfies MissingRedeemerTransition,
      purpose,
    };
  };
  const startComplete = (purpose: SDK.MissingRedeemerClaimedPurposeFact) =>
    ({
      label: "claim complete",
      role: "claimStart",
      next: "claimAdjudicate",
      datum: adjudicateDatum(purpose),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Complete: indices }] },
          SDK.MissingRedeemerClaimStartRedeemer,
        ),
    }) satisfies MissingRedeemerTransition;
  if (coordinate === null) {
    const resolved = resolve("NotRequired", "NotRequired");
    return {
      transitions: [startComplete("NotRequired"), resolved.transition],
      final: resolved.final,
    };
  }
  if (!SDK.purposeCoordinateIsValid(coordinate)) {
    const fact = { ImpossiblePurpose: { coordinate } };
    const resolved = resolve(fact, fact);
    return {
      transitions: [startComplete(fact), resolved.transition],
      final: resolved.final,
    };
  }
  if (
    observed !== null &&
    sameCoordinate(coordinate, observed.authenticated.selection.coordinate)
  ) {
    const purpose = observed.authenticated.selection.purpose;
    if (purpose === null)
      throw new Error("missing-redeemer: observed purpose is absent");
    const fact = { ExistingPurpose: { purpose } };
    const resolved = resolve(
      fact,
      observed.redeemer_missing
        ? { RedeemerAbsent: { decision: observed } }
        : { RedeemerPresent: { decision: observed } },
    );
    return {
      transitions: [startComplete(fact), resolved.transition],
      final: resolved.final,
    };
  }
  if (coordinate.kind !== OBSERVE)
    throw new Error(
      `missing-redeemer: purpose kind ${coordinate.kind.toString()} claim custody is not supported by this prover`,
    );
  const purpose = directPurposeAt(material, coordinate);
  const fact: SDK.MissingRedeemerClaimedPurposeFact =
    purpose === null
      ? { ImpossiblePurpose: { coordinate } }
      : { ExistingPurpose: { purpose } };
  const start: MissingRedeemerTransition = {
    label: "claim observe",
    role: "claimStart",
    next: "claimAdjudicate",
    datum: adjudicateDatum(fact),
    fields: [3],
    redeemer: ({ indices, open }) =>
      Data.to(
        { Continue: [{ Observe: { ...indices, opening: open(3) } }] },
        SDK.MissingRedeemerClaimStartRedeemer,
      ),
  };
  if (purpose === null) {
    const resolved = resolve(
      fact,
      fact as SDK.MissingRedeemerClaimedPredicateFact,
    );
    return { transitions: [start, resolved.transition], final: resolved.final };
  }
  const { selected: source, proof } = await evidence.lookup(
    purpose.script_hash,
  );
  const selection: SDK.DirectSourceSelection = {
    selection: { ...boundIndex(inputs), coordinate, purpose },
    source,
  };
  if (source === null || source.language_tag === 0n) {
    const final: SDK.MissingRedeemerFinalState = {
      context,
      claim:
        source === null
          ? { SourceAbsent: { selection } }
          : { NativeSource: { selection } },
    };
    return {
      transitions: [
        start,
        {
          label: "adjudicate lookup",
          role: "claimAdjudicate",
          next: "terminal",
          datum: finalDatum(final),
          fields: [],
          redeemer: ({ indices }) =>
            Data.to(
              { Continue: [{ Lookup: { ...indices, source, proof } }] },
              SDK.MissingRedeemerAdjudicateRedeemer,
            ),
        },
      ],
      final,
    };
  }
  if (!isRedeemerBearingLanguage(source.language_tag))
    throw new Error(
      `missing-redeemer: source language ${source.language_tag.toString()} has no adjudication route`,
    );
  let final: SDK.MissingRedeemerFinalState | undefined;
  const scan = pointerScan(inputs, {
    label: "claimed",
    authenticated: selection,
    grammar: "claimedRedeemerGrammar",
    scan: "claimedRedeemerScan",
    datum: (state) =>
      stepDatum(
        prover,
        { context, scan: state },
        SDK.MissingRedeemerClaimedScanDatum,
      ),
    finish: (decision) => {
      final = {
        context,
        claim: decision.redeemer_missing
          ? { RedeemerAbsent: { decision } }
          : { RedeemerPresent: { decision } },
      };
      return { next: "terminal", datum: finalDatum(final) };
    },
  });
  return {
    transitions: [
      start,
      {
        label: "adjudicate lookup scan",
        role: "claimAdjudicate",
        next: "claimedRedeemerGrammar",
        datum: scan.initial,
        fields: [8],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                {
                  LookupScan: { ...indices, source, proof, opening: open(8) },
                },
              ],
            },
            SDK.MissingRedeemerAdjudicateRedeemer,
          ),
      },
      ...scan.transitions,
    ],
    final: final!,
  };
};

/** Off-chain twin of `adjudicate.convicts_v1` over the shapes this prover
 * builds; any other shape fails closed. */
export const missingRedeemerConvicts = (
  final: SDK.MissingRedeemerFinalState,
): boolean => {
  const { context, claim } = final;
  if (context.bound.bound.subject.rejection_reason === null) {
    if (context.observed === null || claim !== "NotRequired")
      throw new Error("missing-redeemer: accepted final state differs");
    return context.observed.redeemer_missing;
  }
  if (context.observed !== null)
    throw new Error(
      "missing-redeemer: rejected leaf with an observation is not supported by this prover",
    );
  if (claim === "NotRequired" || "SourcePresent" in claim)
    throw new Error("missing-redeemer: rejected final state differs");
  return !("RedeemerAbsent" in claim);
};
