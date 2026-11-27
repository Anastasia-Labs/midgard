/** Direct `scriptIntegrityHashMismatch` wire (`script-integrity-hash-mismatch/
 * {step-01,entry,terminal}.ak`). Constructor and field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { FieldOpeningSchema } from "./field-opening.js";
import type { NetworkOrderContext } from "./first-fault.js";
import { leafAdmitsLaterFault } from "./first-fault-later-phases.js";
import { faultProofStepRedeemerSchema } from "./native.js";
import { ProofThreadSubjectSourceSchema } from "./proof-thread-source.js";
import {
  terminalVerdictContradiction,
  type VerdictSubject,
} from "./proof-thread-substrate.js";

export const ScriptIntegrityHashMismatchStep01ArgsSchema = Data.Object({
  source: ProofThreadSubjectSourceSchema,
});
export const ScriptIntegrityHashMismatchEntryArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const ScriptIntegrityHashMismatchFinalizeArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  witness_set_compact_cbor: Data.Bytes(),
});

export const ScriptIntegrityHashMismatchStep01RedeemerSchema =
  faultProofStepRedeemerSchema(ScriptIntegrityHashMismatchStep01ArgsSchema);
export type ScriptIntegrityHashMismatchStep01Redeemer = Data.Static<
  typeof ScriptIntegrityHashMismatchStep01RedeemerSchema
>;
export const ScriptIntegrityHashMismatchStep01Redeemer =
  asDataType<ScriptIntegrityHashMismatchStep01Redeemer>(
    ScriptIntegrityHashMismatchStep01RedeemerSchema,
  );

export const ScriptIntegrityHashMismatchEntryRedeemerSchema =
  faultProofStepRedeemerSchema(ScriptIntegrityHashMismatchEntryArgsSchema);
export type ScriptIntegrityHashMismatchEntryRedeemer = Data.Static<
  typeof ScriptIntegrityHashMismatchEntryRedeemerSchema
>;
export const ScriptIntegrityHashMismatchEntryRedeemer =
  asDataType<ScriptIntegrityHashMismatchEntryRedeemer>(
    ScriptIntegrityHashMismatchEntryRedeemerSchema,
  );

export const ScriptIntegrityHashMismatchFinalizeRedeemerSchema =
  faultProofStepRedeemerSchema(ScriptIntegrityHashMismatchFinalizeArgsSchema);
export type ScriptIntegrityHashMismatchFinalizeRedeemer = Data.Static<
  typeof ScriptIntegrityHashMismatchFinalizeRedeemerSchema
>;
export const ScriptIntegrityHashMismatchFinalizeRedeemer =
  asDataType<ScriptIntegrityHashMismatchFinalizeRedeemer>(
    ScriptIntegrityHashMismatchFinalizeRedeemerSchema,
  );

/** Twin of `terminal.convicts_v1` once the mismatch is known: an acceptance
 * convicts on a mismatch; a rejection convicts when it records this arm with
 * no mismatch, or when a mismatch holds and its recorded reason ranks after
 * phase 10. */
export const scriptIntegrityHashMismatchConvicts = (
  subject: VerdictSubject,
  context: NetworkOrderContext,
  mismatch: boolean,
): boolean => {
  const reason = subject.rejection_reason;
  return terminalVerdictContradiction(
    subject,
    reason === null || reason === "ScriptIntegrityHashMismatch"
      ? mismatch
      : !mismatch ||
          leafAdmitsLaterFault(subject, context, { phase: "scriptIntegrity" }),
  );
};
