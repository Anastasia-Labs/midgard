/** Both field prerequisites share the admitted source's exact compact bytes. */
import type { MintingPolicy, UTxO } from "@lucid-evolution/lucid";

import { inputExistenceFieldPlans } from "../input-existence/field-carriage.js";
import type { LinearFamilyPrerequisiteInput } from "./family-definition.js";
import type { FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import { admitLedgerAbsenceArtifact } from "./ledger-absence-artifact.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

type FieldContext = Readonly<{
  signer: Readonly<{ paymentKeyHash: string }>;
  certificate: Readonly<{ policyId: string; mintingScript: MintingPolicy }>;
  references: Readonly<{ fieldPreimageCertificateMint: UTxO }>;
}>;

export const inputExistenceFieldCarriageRequirements = (
  isSelectionAction: (action: FraudProofWorkflowAction) => boolean,
) => {
  const fields: readonly (0 | 1)[] = [0, 1];
  return fields.map((fieldIndex) => ({
    requirementForAction: async (
      context: FieldContext,
      { action, artifact }: LinearFamilyPrerequisiteInput,
    ): Promise<FieldCarriageRequirement | null> => {
      if (!isSelectionAction(action)) return null;
      const { prepared } = admitLedgerAbsenceArtifact(
        artifact,
        context.signer.paymentKeyHash,
      );
      const planned = inputExistenceFieldPlans(prepared).find(
        (plan) => plan.fieldIndex === fieldIndex,
      );
      if (planned === undefined) return null;
      return {
        planned,
        compactCbor: prepared.source.compactCbor,
        witnessSetCompactCbor: prepared.source.witnessSetCompactCbor,
        certificate: {
          policyId: context.certificate.policyId,
          mintingScript: context.certificate.mintingScript,
          referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
        },
      };
    },
  }));
};
