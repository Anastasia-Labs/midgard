/** The observed purpose route of the `missingRedeemer` thread, from the
 * family entry to claim start. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { isRedeemerBearingLanguage } from "../direct-purpose-family/claim-route.js";
import {
  type DirectPurposeTransition,
  stepDatum,
} from "../direct-purpose-family/custody.js";
import {
  boundIndex,
  pointerScan,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";
import { directPurposeObservedSelection } from "../direct-purpose-family/purpose-routes.js";

type Coordinate = SDK.DirectPurposeCoordinate;

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
  } = await directPurposeObservedSelection(inputs, coordinate);
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
  const transitions: DirectPurposeTransition[] = [
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
          SDK.DirectFamilyEntryRedeemer,
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
