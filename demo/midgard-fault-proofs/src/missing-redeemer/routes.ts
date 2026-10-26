/** The observed purpose route and the claim custody route of the
 * `missingRedeemer` thread, from the family entry to the terminal datum. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { missingRedeemerClaimDomain } from "./claim-domains.js";
import { type MissingRedeemerTransition, stepDatum } from "./custody.js";
import { boundIndex, pointerScan, type RouteInputs } from "./pointer-scan.js";
import { missingRedeemerObservedSelection } from "./purpose-routes.js";

type Coordinate = SDK.DirectPurposeCoordinate;

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

/** Observed route over the purpose at `coordinate`: its kind's selection
 * domain, its source lookup and its exhausted field-8 pointer scan, ending at
 * claim start. */
export const missingRedeemerObservedRoute = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
) => {
  const { evidence, prover } = inputs;
  const {
    selection,
    purpose,
    transitions: selected,
  } = await missingRedeemerObservedSelection(inputs, coordinate);
  const { selected: source, proof } = await evidence.lookup(
    purpose.script_hash,
  );
  if (source === null || !isRedeemerBearingLanguage(source.language_tag))
    throw new Error("missing-redeemer: observed purpose has no redeemer");
  const authenticated = { selection, source };
  const scan = pointerScan(inputs, {
    label: "observed",
    authenticated,
    grammar: "observedGrammar",
    scan: "observedScan",
    datum: (state) => stepDatum(prover, state, SDK.MissingRedeemerScanDatum),
    finish: (decision) => {
      const context: SDK.DirectClaimContext = {
        bound: boundIndex(inputs),
        observed: decision,
        target: "LeafPurpose",
      };
      return {
        next: "claimStart",
        datum: stepDatum(prover, context, SDK.DirectClaimStartDatum),
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
    ...selected,
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
      target: "LeafPurpose",
    } satisfies SDK.DirectClaimContext,
  };
};

/** Claim entry: the claimed-only context, no observation carried. */
export const missingRedeemerClaimEntry = (
  inputs: RouteInputs,
): {
  readonly transition: MissingRedeemerTransition;
  readonly context: SDK.DirectClaimContext;
} => {
  const context: SDK.DirectClaimContext = {
    bound: boundIndex(inputs),
    observed: null,
    target: "LeafPurpose",
  };
  return {
    context,
    transition: {
      label: "entry claim",
      role: "familyEntry",
      next: "claimStart",
      datum: stepDatum(inputs.prover, context, SDK.DirectClaimStartDatum),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Claim: indices }] },
          SDK.MissingRedeemerEntryRedeemer,
        ),
    },
  };
};

/** Claim start and adjudication: no coordinate, an invalid coordinate, the
 * observed coordinate, or a distinct coordinate of any purpose kind. */
export const missingRedeemerClaimRoute = async (
  inputs: RouteInputs,
  context: SDK.DirectClaimContext,
): Promise<{
  readonly transitions: readonly MissingRedeemerTransition[];
  readonly final: SDK.DirectClaimFinalState;
}> => {
  const { material, evidence, prover } = inputs;
  const coordinate = missingRedeemerLeafCoordinate(material);
  const observed = context.observed;
  const finalDatum = (final: SDK.DirectClaimFinalState) =>
    stepDatum(prover, final, SDK.DirectClaimFinalDatum);
  const adjudicateDatum = (purpose: SDK.DirectClaimedPurposeFact) =>
    stepDatum(prover, { context, purpose }, SDK.DirectClaimAdjudicateDatum);
  const resolve = (
    purpose: SDK.DirectClaimedPurposeFact,
    claim: SDK.DirectClaimedPredicateFact,
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
            SDK.DirectClaimAdjudicateRedeemer,
          ),
      } satisfies MissingRedeemerTransition,
      purpose,
    };
  };
  const startComplete = (purpose: SDK.DirectClaimedPurposeFact) =>
    ({
      label: "claim complete",
      role: "claimStart",
      next: "claimAdjudicate",
      datum: adjudicateDatum(purpose),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Complete: indices }] },
          SDK.DirectClaimStartRedeemer,
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
  const domain = await missingRedeemerClaimDomain(inputs, context, coordinate);
  const purpose = domain.purpose;
  if (purpose === null) {
    const fact = { ImpossiblePurpose: { coordinate } };
    const resolved = resolve(fact, fact);
    return {
      transitions: [...domain.transitions, resolved.transition],
      final: resolved.final,
    };
  }
  const { selected: source, proof } = await evidence.lookup(
    purpose.script_hash,
  );
  const selection: SDK.DirectSourceSelection = {
    selection: { ...boundIndex(inputs), coordinate, purpose },
    source,
  };
  if (source === null || source.language_tag === 0n) {
    const final: SDK.DirectClaimFinalState = {
      context,
      claim:
        source === null
          ? { SourceAbsent: { selection } }
          : { NativeSource: { selection } },
    };
    return {
      transitions: [
        ...domain.transitions,
        {
          label: "adjudicate lookup",
          role: "claimAdjudicate",
          next: "terminal",
          datum: finalDatum(final),
          fields: [],
          redeemer: ({ indices }) =>
            Data.to(
              { Continue: [{ Lookup: { ...indices, source, proof } }] },
              SDK.DirectClaimAdjudicateRedeemer,
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
  let final: SDK.DirectClaimFinalState | undefined;
  const scan = pointerScan(inputs, {
    label: "claimed",
    authenticated: selection,
    grammar: "claimedRedeemerGrammar",
    scan: "claimedRedeemerScan",
    datum: (state) =>
      stepDatum(prover, { context, scan: state }, SDK.DirectClaimedScanDatum),
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
      ...domain.transitions,
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
            SDK.DirectClaimAdjudicateRedeemer,
          ),
      },
      ...scan.transitions,
    ],
    final: final!,
  };
};

/** Whether a rejected leaf whose reason names no purpose coordinate admits a
 * missing redeemer observed at `coordinate`: the off-chain twin of
 * `adjudicate.leaf_admits_observed_v1` for such a reason. */
export const missingRedeemerLeafAdmits = (
  bound: SDK.DirectScriptBound,
  coordinate: Coordinate,
): boolean => {
  const reason = bound.subject.rejection_reason;
  if (reason === null || SDK.purposeCoordinateOfReason(reason) !== null)
    throw new Error(
      "missing-redeemer: leaf reason is absent or names a purpose",
    );
  return SDK.leafAdmitsFault(
    bound.subject,
    bound.network_order_context,
    {
      phase: "purpose",
      position: { coordinate, subcheck: "RedeemerRequired" },
    },
    "NotRequired",
  );
};

/** Off-chain twin of `adjudicate.convicts_v1` over the shapes this prover
 * builds; any other shape fails closed. */
export const missingRedeemerConvicts = (
  final: SDK.DirectClaimFinalState,
): boolean => {
  const { context, claim } = final;
  if (context.bound.bound.subject.rejection_reason === null) {
    if (context.observed === null || claim !== "NotRequired")
      throw new Error("missing-redeemer: accepted final state differs");
    return context.observed.redeemer_missing;
  }
  if (context.observed !== null) {
    if (claim !== "NotRequired")
      throw new Error("missing-redeemer: observed final state differs");
    return (
      context.observed.redeemer_missing &&
      !missingRedeemerLeafAdmits(
        context.bound.bound,
        context.observed.authenticated.selection.coordinate,
      )
    );
  }
  if (claim === "NotRequired" || "SourcePresent" in claim)
    throw new Error("missing-redeemer: rejected final state differs");
  return !("RedeemerAbsent" in claim);
};
