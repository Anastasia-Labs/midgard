/** Direct `unusedScriptWitness` wire (`unused-script-witness/{entry,walk,
 * terminal}.ak`). The family entry opens the shared all-purposes walk with the
 * audited witness's script hash as its probe; the witness walk then reads that
 * field-6 witness and the terminal decides. Constructor and field order is
 * wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import { DirectAllPurposesFactsSchema } from "./direct-all-purposes-scan.js";
import { DirectProgressArgsSchema } from "./direct-purpose-workers.js";
import {
  DirectFieldCursorSchema,
  DirectScriptBoundSchema,
  DirectScriptSourceSchema,
} from "./direct-script-derivation.js";
import { DirectSourceIndexSchema } from "./direct-source-index-state.js";
import { FieldOpeningSchema } from "./field-opening.js";
import type { NetworkOrderContext } from "./first-fault.js";
import {
  leafAdmitsLaterFault,
  unusedAuditPositionOfReason,
} from "./first-fault-later-phases.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";
import {
  terminalVerdictContradiction,
  type VerdictSubject,
} from "./proof-thread-substrate.js";

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };

export const UnusedScriptWitnessEntryArgsSchema = Data.Object({
  ...indices,
  probe: Data.Bytes(),
  opening: FieldOpeningSchema,
});
export const UnusedScriptWitnessEntryRedeemerSchema =
  faultProofStepRedeemerSchema(UnusedScriptWitnessEntryArgsSchema);
export type UnusedScriptWitnessEntryRedeemer = Data.Static<
  typeof UnusedScriptWitnessEntryRedeemerSchema
>;
export const UnusedScriptWitnessEntryRedeemer =
  asDataType<UnusedScriptWitnessEntryRedeemer>(
    UnusedScriptWitnessEntryRedeemerSchema,
  );

/** The completed all-purposes walk (`all_purposes_scan.ResultV1`). */
export const UnusedScriptWitnessScanResultSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  source_index: DirectSourceIndexSchema,
  facts: DirectAllPurposesFactsSchema,
});

/** `witness_hash`: the script hash of the item at `script_index`, once read.
 * `complete`: field 6 is exhausted. */
export const UnusedScriptWitnessWalkStateSchema = Data.Object({
  result: UnusedScriptWitnessScanResultSchema,
  script_index: Data.Integer(),
  cursor: DirectFieldCursorSchema,
  witness_hash: Data.Nullable(Data.Bytes()),
  complete: Data.Boolean(),
});
export type UnusedScriptWitnessWalkState = Data.Static<
  typeof UnusedScriptWitnessWalkStateSchema
>;
export const UnusedScriptWitnessWalkState =
  asDataType<UnusedScriptWitnessWalkState>(UnusedScriptWitnessWalkStateSchema);

export const UnusedScriptWitnessWalkDatumSchema = faultProofStepDatumSchema(
  UnusedScriptWitnessWalkStateSchema,
);
export type UnusedScriptWitnessWalkDatum = Data.Static<
  typeof UnusedScriptWitnessWalkDatumSchema
>;
export const UnusedScriptWitnessWalkDatum =
  asDataType<UnusedScriptWitnessWalkDatum>(UnusedScriptWitnessWalkDatumSchema);

export const UnusedScriptWitnessWitnessEntryArgsSchema = Data.Object({
  ...indices,
  script_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const UnusedScriptWitnessWitnessEntryRedeemerSchema =
  faultProofStepRedeemerSchema(UnusedScriptWitnessWitnessEntryArgsSchema);
export type UnusedScriptWitnessWitnessEntryRedeemer = Data.Static<
  typeof UnusedScriptWitnessWitnessEntryRedeemerSchema
>;
export const UnusedScriptWitnessWitnessEntryRedeemer =
  asDataType<UnusedScriptWitnessWitnessEntryRedeemer>(
    UnusedScriptWitnessWitnessEntryRedeemerSchema,
  );

/** Grammar certification and the witness walk both take one bounded batch. */
export const UnusedScriptWitnessProgressRedeemerSchema =
  faultProofStepRedeemerSchema(DirectProgressArgsSchema);
export type UnusedScriptWitnessProgressRedeemer = Data.Static<
  typeof UnusedScriptWitnessProgressRedeemerSchema
>;
export const UnusedScriptWitnessProgressRedeemer =
  asDataType<UnusedScriptWitnessProgressRedeemer>(
    UnusedScriptWitnessProgressRedeemerSchema,
  );

/** The source selected for the witness's script hash and its source-map
 * proof. */
export const UnusedScriptWitnessSelectionSchema = Data.Object({
  source: DirectScriptSourceSchema,
  proof: ProofSchema,
});
export type UnusedScriptWitnessSelection = Data.Static<
  typeof UnusedScriptWitnessSelectionSchema
>;

export const UnusedScriptWitnessFinalizeArgsSchema = Data.Object({
  ...indices,
  fraud_proof_mint_redeemer_index: Data.Integer(),
  selection: Data.Nullable(UnusedScriptWitnessSelectionSchema),
});
export const UnusedScriptWitnessFinalizeRedeemerSchema =
  faultProofStepRedeemerSchema(UnusedScriptWitnessFinalizeArgsSchema);
export type UnusedScriptWitnessFinalizeRedeemer = Data.Static<
  typeof UnusedScriptWitnessFinalizeRedeemerSchema
>;
export const UnusedScriptWitnessFinalizeRedeemer =
  asDataType<UnusedScriptWitnessFinalizeRedeemer>(
    UnusedScriptWitnessFinalizeRedeemerSchema,
  );

/** Facts the terminal reads about the witness at `scriptIndex`. `null` when
 * field 6 ends before that index. Otherwise `selected` says whether the
 * witness itself is the source selected for its script hash, and `reached`
 * whether some purpose of the transaction executes that hash. */
export type UnusedScriptWitnessFacts = Readonly<{
  selected: boolean;
  reached: boolean;
}> | null;

/** Twin of `terminal.unused_v1`: a witness is unused when another source is
 * selected for its hash or no purpose reaches it. */
export const unusedScriptWitnessIsUnused = (
  facts: UnusedScriptWitnessFacts,
): boolean => facts !== null && (!facts.selected || !facts.reached);

/** Twin of `terminal.convicts_v1`: an acceptance convicts when the witness is
 * unused; a rejection convicts when it records this witness and the witness
 * is used, or when the witness is unused and the recorded reason ranks after
 * this phase-8 audit position. */
export const unusedScriptWitnessConvicts = (
  subject: VerdictSubject,
  context: NetworkOrderContext,
  scriptIndex: bigint,
  unused: boolean,
): boolean => {
  const reason = subject.rejection_reason;
  const audit = reason === null ? null : unusedAuditPositionOfReason(reason);
  const sameArm =
    audit !== null &&
    audit.kind === "witness" &&
    audit.scriptIndex === scriptIndex;
  return terminalVerdictContradiction(
    subject,
    reason === null || sameArm
      ? unused
      : !unused ||
          leafAdmitsLaterFault(subject, context, {
            phase: "unusedAudit",
            position: { kind: "witness", scriptIndex },
          }),
  );
};
