/** The observed purpose route of the `missingScriptSource` thread, from the
 * family entry to claim start. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type DirectPurposeTransition,
  stepDatum,
} from "../direct-purpose-family/custody.js";
import {
  boundIndex,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";
import { directPurposeObservedSelection } from "../direct-purpose-family/purpose-routes.js";

type Coordinate = SDK.DirectPurposeCoordinate;

/** The claim context an observed missing source hands to claim start. */
export const missingScriptSourceObservedContext = (
  inputs: RouteInputs,
  authenticated: SDK.DirectSourceSelection,
): SDK.DirectClaimContext => ({
  bound: boundIndex(inputs),
  observed: { authenticated, redeemer_missing: false },
  target: "LeafPurpose",
});

/** Observed route over the purpose at `coordinate`: its kind's selection
 * domain, its source lookup proving the source absent, and the observed
 * decision, ending at claim start. */
export const missingScriptSourceObservedRoute = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
): Promise<{
  readonly transitions: readonly DirectPurposeTransition[];
  readonly context: SDK.DirectClaimContext;
}> => {
  const { evidence, prover } = inputs;
  const {
    selection,
    purpose,
    transitions: selected,
  } = await directPurposeObservedSelection(inputs, coordinate);
  const { selected: source, proof } = await evidence.lookup(
    purpose.script_hash,
  );
  if (source !== null)
    throw new Error("missing-script-source: observed purpose has a source");
  const authenticated: SDK.DirectSourceSelection = { selection, source };
  const context = missingScriptSourceObservedContext(inputs, authenticated);
  return {
    context,
    transitions: [
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
        next: "observedDecision",
        datum: stepDatum(
          prover,
          authenticated,
          SDK.MissingScriptSourceObserveDatum,
        ),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ ...indices, source, proof }] },
            SDK.DirectSourceLookupRedeemer,
          ),
      },
      {
        label: "observed decision",
        role: "observedDecision",
        next: "claimStart",
        datum: stepDatum(prover, context, SDK.DirectClaimStartDatum),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [indices] },
            SDK.MissingScriptSourceObserveRedeemer,
          ),
      },
    ],
  };
};
