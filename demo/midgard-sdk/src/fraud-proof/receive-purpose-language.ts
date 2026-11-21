/** Direct receive-purpose-language wire and verdict twin
 * (`receive-purpose-language/entry.ak`, `lookup.ak`, `terminal.ak`). The
 * family entry hands the source-finish binding to the shared receive route,
 * or a recorded reason naming no receive coordinate straight to the terminal.
 * The lookup step reuses the shared selection datum and adjudication actions;
 * the terminal reuses the shared claim final state. Constructor and field
 * order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import type {
  DirectClaimedPredicateFact,
  DirectClaimFinalState,
} from "./direct-claim.js";
import type { DirectPurposeCoordinate } from "./direct-script-derivation.js";
import type { DirectSourceBoundIndex } from "./direct-source-workers.js";
import { leafAdmitsLaterFault } from "./first-fault-later-phases.js";
import { faultProofStepRedeemerSchema } from "./native.js";

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };

/** 0 Observe, 1 Contradict. */
export const ReceivePurposeLanguageEntryActionSchema = Data.Enum([
  Data.Object({ Observe: Data.Object(indices) }),
  Data.Object({ Contradict: Data.Object(indices) }),
]);
export type ReceivePurposeLanguageEntryAction = Data.Static<
  typeof ReceivePurposeLanguageEntryActionSchema
>;
export const ReceivePurposeLanguageEntryAction =
  asDataType<ReceivePurposeLanguageEntryAction>(
    ReceivePurposeLanguageEntryActionSchema,
  );

export const ReceivePurposeLanguageEntryRedeemerSchema =
  faultProofStepRedeemerSchema(ReceivePurposeLanguageEntryActionSchema);
export type ReceivePurposeLanguageEntryRedeemer = Data.Static<
  typeof ReceivePurposeLanguageEntryRedeemerSchema
>;
export const ReceivePurposeLanguageEntryRedeemer =
  asDataType<ReceivePurposeLanguageEntryRedeemer>(
    ReceivePurposeLanguageEntryRedeemerSchema,
  );

/** `script_proof_v1.language_tag(PlutusV3Script)`. */
export const RECEIVE_PURPOSE_LANGUAGE_PLUTUS_V3_TAG = 3n;

const RECEIVE_KIND = 3n;

const fail = (message: string): never => {
  throw new Error(`receive-purpose-language: ${message}`);
};

/** The recorded `ReceivePurposePlutusV3Forbidden` coordinate, or null. */
export const receivePurposeLanguageRecordedCoordinate = (
  bound: DirectSourceBoundIndex,
): DirectPurposeCoordinate | null => {
  const reason = bound.bound.subject.rejection_reason;
  if (
    reason === null ||
    typeof reason !== "object" ||
    !("ReceivePurposePlutusV3Forbidden" in reason)
  )
    return null;
  const { purpose_kind, purpose_index } =
    reason.ReceivePurposePlutusV3Forbidden;
  return { kind: purpose_kind, index: purpose_index };
};

/** Twin of `entry.contradiction_v1`: a recorded coordinate that is not a
 * receive coordinate names no receive purpose. */
export const receivePurposeLanguageContradiction = (
  bound: DirectSourceBoundIndex,
): DirectClaimFinalState => {
  const coordinate = receivePurposeLanguageRecordedCoordinate(bound);
  if (coordinate === null) return fail("the leaf records another reason");
  if (coordinate.kind === RECEIVE_KIND && coordinate.index >= 0n)
    return fail("a receive coordinate takes the receive route");
  return {
    context: { bound, observed: null, target: "LeafPurpose" },
    claim: { ImpossiblePurpose: { coordinate } },
  };
};

/** Twin of `terminal.coordinate_v1`. */
export const receivePurposeLanguageFactCoordinate = (
  fact: DirectClaimedPredicateFact,
): DirectPurposeCoordinate => {
  if (typeof fact === "object") {
    if ("ImpossiblePurpose" in fact) return fact.ImpossiblePurpose.coordinate;
    if ("SourceAbsent" in fact)
      return fact.SourceAbsent.selection.selection.coordinate;
    if ("SourcePresent" in fact)
      return fact.SourcePresent.selection.selection.coordinate;
    if ("RedeemerPresent" in fact)
      return fact.RedeemerPresent.decision.authenticated.selection.coordinate;
    if ("RedeemerAbsent" in fact)
      return fact.RedeemerAbsent.decision.authenticated.selection.coordinate;
  }
  return fail("the fact is not a receive-language fact");
};

/** Twin of `terminal.convicts_v1`. A leaf recording this arm at the fact's
 * coordinate convicts when the execution is not forbidden; any other leaf,
 * accepted ones included, convicts on a forbidden execution its recorded
 * reason does not precede. */
export const receivePurposeLanguageConvicts = (
  final: DirectClaimFinalState,
): boolean => {
  const bound = final.context.bound.bound;
  const coordinate = receivePurposeLanguageFactCoordinate(final.claim);
  const forbidden =
    typeof final.claim === "object" && "RedeemerPresent" in final.claim;
  const recorded = receivePurposeLanguageRecordedCoordinate(
    final.context.bound,
  );
  if (
    recorded !== null &&
    recorded.kind === coordinate.kind &&
    recorded.index === coordinate.index
  )
    return !forbidden;
  return (
    forbidden &&
    !leafAdmitsLaterFault(bound.subject, bound.network_order_context, {
      phase: "cek",
      position: { coordinate, subcheck: "ReceiveLanguage" },
    })
  );
};
