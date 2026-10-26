/** The claim custody route shared by the direct purpose families: the
 * claim-only entry, then claim start and adjudication over the context's
 * target coordinate (the leaf's own purpose, or a walked redeemer's pointer),
 * ending at the terminal datum. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { directPurposeClaimDomain } from "./claim-domains.js";
import { type DirectPurposeTransition, stepDatum } from "./custody.js";
import { boundIndex, pointerScan, type RouteInputs } from "./pointer-scan.js";

type Coordinate = SDK.DirectPurposeCoordinate;

export const isRedeemerBearingLanguage = (tag: bigint): boolean =>
  tag === 3n || tag === 128n;

const sameCoordinate = (left: Coordinate, right: Coordinate) =>
  left.kind === right.kind && left.index === right.index;

/** The leaf's own purpose coordinate; null when its reason names none. */
export const directPurposeLeafCoordinate = (
  material: DirectScriptMaterial,
): Coordinate | null => {
  const reason = material.bound.subject.rejection_reason;
  return reason === null ? null : SDK.purposeCoordinateOfReason(reason);
};

/** Whether a rejected leaf whose reason names no purpose coordinate admits
 * the purpose fault at `position`: such a reason carries no claimed fact. */
export const directPurposeLeafAdmits = (
  bound: SDK.DirectScriptBound,
  position: SDK.PurposeFaultPosition,
): boolean => {
  const reason = bound.subject.rejection_reason;
  if (reason === null || SDK.purposeCoordinateOfReason(reason) !== null)
    throw new Error("direct purpose: leaf reason is absent or names a purpose");
  return SDK.directClaimLeafAdmitsObserved(bound, position, "NotRequired");
};

/** Claim entry: the claimed-only context, no observation carried. */
export const directPurposeClaimEntry = (
  inputs: RouteInputs,
): {
  readonly transition: DirectPurposeTransition;
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
          SDK.DirectFamilyEntryRedeemer,
        ),
    },
  };
};

/** The fact `resolve_v1` derives at the observed route's own purpose. */
const observedFact = (
  subcheck: SDK.PurposeSubcheck,
  observed: SDK.DirectRedeemerDecisionState,
): SDK.DirectClaimedPredicateFact => {
  const selection = observed.authenticated;
  if (selection.source === null) return { SourceAbsent: { selection } };
  if (subcheck === "SourceRequired") return { SourcePresent: { selection } };
  return observed.redeemer_missing
    ? { RedeemerAbsent: { decision: observed } }
    : { RedeemerPresent: { decision: observed } };
};

/** Claim start and adjudication: no coordinate, an invalid coordinate, the
 * observed coordinate, or a distinct coordinate of any purpose kind. A
 * missing-source leaf and a redeemer pointer are decided by the source lookup
 * alone; a missing-redeemer leaf with a redeemer-bearing source also scans
 * field 8. */
export const directPurposeClaimRoute = async (
  inputs: RouteInputs,
  context: SDK.DirectClaimContext,
): Promise<{
  readonly transitions: readonly DirectPurposeTransition[];
  readonly final: SDK.DirectClaimFinalState;
}> => {
  const { material, evidence, prover } = inputs;
  const pointer =
    context.target === "LeafPurpose" ? null : context.target.RedeemerPointer;
  const reason = material.bound.subject.rejection_reason;
  const coordinate =
    pointer === null
      ? directPurposeLeafCoordinate(material)
      : pointer.coordinate;
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
      } satisfies DirectPurposeTransition,
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
    }) satisfies DirectPurposeTransition;
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
  const subcheck =
    pointer === null && reason !== null
      ? SDK.purposeSubcheckOfReason(reason)
      : null;
  if (
    subcheck !== null &&
    observed !== null &&
    sameCoordinate(coordinate, observed.authenticated.selection.coordinate)
  ) {
    const purpose = observed.authenticated.selection.purpose;
    if (purpose === null)
      throw new Error("direct purpose: observed purpose is absent");
    const fact = { ExistingPurpose: { purpose } };
    const resolved = resolve(fact, observedFact(subcheck, observed));
    return {
      transitions: [startComplete(fact), resolved.transition],
      final: resolved.final,
    };
  }
  const domain = await directPurposeClaimDomain(inputs, context, coordinate);
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
  if (
    subcheck !== "RedeemerRequired" ||
    source === null ||
    source.language_tag === 0n
  ) {
    const final: SDK.DirectClaimFinalState = {
      context,
      claim:
        source === null
          ? { SourceAbsent: { selection } }
          : subcheck === "SourceRequired" ||
              (subcheck === null && source.language_tag !== 0n)
            ? { SourcePresent: { selection } }
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
      `direct purpose: source language ${source.language_tag.toString()} has no adjudication route`,
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
