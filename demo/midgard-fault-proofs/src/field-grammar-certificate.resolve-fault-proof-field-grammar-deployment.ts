import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import type { FaultProofFieldGrammarDeployment } from "./field-grammar-certificate.route-fault-proof-field-grammar.js";
import {
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./workflow/deployment-manifest-binding.js";
import type { FraudProofPreSubmitBoundary } from "./workflow/transaction-boundary.js";

/** The deployment members a certified field read runs against. */
export const FIELD_GRAMMAR_CERTIFICATE_SPEND = "fieldGrammarCertificateSpend";
export const FIELD_GRAMMAR_CERTIFICATE_MINT = "fieldGrammarCertificateMint";

/**
 * Resolves the deployed field grammar certificate a family's certified reads
 * run against, at startup: the validator built from the bound blueprint over
 * the bound field-preimage certificate policy, and the `fieldGrammarCertificateSpend`
 * member's published reference script, which must run exactly that validator.
 * A deployment without both certificate members fails here, naming them,
 * rather than leaving the family on the walked route.
 */
export const resolveFaultProofFieldGrammarDeployment = ({
  binding,
  fieldPreimageCertificatePolicyId,
  referenceScriptUtxo,
  label,
}: {
  readonly binding: Pick<
    FraudProofWorkflowDeploymentBinding<SDK.FraudProofCatalogueCategoryName>,
    "blueprint" | "network" | "referenceScriptsByContract"
  >;
  readonly fieldPreimageCertificatePolicyId: string;
  /** The roster-resolved `fieldGrammarCertificateSpend` reference script. */
  readonly referenceScriptUtxo: UTxO | undefined;
  readonly label: string;
}): FaultProofFieldGrammarDeployment => {
  const missing = [
    FIELD_GRAMMAR_CERTIFICATE_SPEND,
    FIELD_GRAMMAR_CERTIFICATE_MINT,
  ].filter((name) => binding.referenceScriptsByContract[name] === undefined);
  if (missing.length > 0 || referenceScriptUtxo === undefined) {
    throw new Error(
      `${label} deployment lacks the field grammar certificate members ${FIELD_GRAMMAR_CERTIFICATE_SPEND} and ${FIELD_GRAMMAR_CERTIFICATE_MINT}${
        missing.length > 0 ? ` (missing ${missing.join(", ")})` : ""
      }`,
    );
  }
  const reference = requireManifestBoundReferenceScriptUtxo({
    binding,
    contractName: FIELD_GRAMMAR_CERTIFICATE_SPEND,
    utxo: referenceScriptUtxo,
  });
  const validator = SDK.buildFieldGrammarCertificateValidator({
    blueprint: SDK.parseFaultProofBlueprint(binding.blueprint),
    network: binding.network,
    fieldPreimageCertificatePolicyId,
  });
  if (
    reference.scriptRef == null ||
    validatorToScriptHash(reference.scriptRef) !== validator.spendingScriptHash
  ) {
    throw new Error(
      `${label} field grammar certificate reference script differs from the blueprint's`,
    );
  }
  if (
    binding.referenceScriptsByContract[FIELD_GRAMMAR_CERTIFICATE_MINT]!
      .scriptHash !== validator.policyId
  ) {
    throw new Error(
      `${label} field grammar certificate minting policy differs from the blueprint's`,
    );
  }
  return Object.freeze({
    certificateValidator: Object.freeze({
      spendingScript: validator.spendingScript,
      spendingScriptHash: validator.spendingScriptHash,
    }),
    certificateReferenceScriptUtxo: reference,
  });
};

/**
 * The resolved deployment a workflow assembled for a family whose steps read
 * through the field grammar certificate. Assembly resolves it at startup, so
 * its absence here is a family wired without it.
 */
export const requireFaultProofFieldGrammarDeployment = (
  fieldGrammar: FaultProofFieldGrammarDeployment | undefined,
  label: string,
): FaultProofFieldGrammarDeployment => {
  if (fieldGrammar === undefined) {
    throw new Error(
      `${label} workflow was assembled without its field grammar certificate deployment`,
    );
  }
  return fieldGrammar;
};

/**
 * The deployment a journaled step hands its submitter. The certificate is the
 * prover's own transaction ahead of the step: under a central journal it is
 * journaled as an auxiliary through `certificateBoundary`, and it never takes
 * the step's own boundary.
 */
export const journalFaultProofFieldGrammar = (
  fieldGrammar: FaultProofFieldGrammarDeployment,
  certificateBoundary: FraudProofPreSubmitBoundary | undefined,
): FaultProofFieldGrammarDeployment =>
  certificateBoundary === undefined
    ? fieldGrammar
    : Object.freeze({
        ...fieldGrammar,
        preSubmitBoundary: certificateBoundary,
      });

/**
 * The certificate and step boundary for a journaled step whose certificate
 * lands after its other auxiliaries were confirmed: the certificate is
 * journaled as its own auxiliary, and the step boundary confirms it before
 * the step itself.
 */
export const journalFaultProofFieldGrammarAfterCarriage = <Stage>({
  fieldGrammar,
  journal,
  familyIdentity,
  stage,
  stepBoundary,
}: {
  readonly fieldGrammar: FaultProofFieldGrammarDeployment;
  readonly journal:
    | Readonly<{
        auxiliaryBoundary: (
          kind: "certificate",
          familyIdentity: string,
          stage: Stage,
          captured: string[],
        ) => FraudProofPreSubmitBoundary;
        confirmAuxiliary: (txHash: string) => Promise<void>;
      }>
    | undefined;
  readonly familyIdentity: string;
  readonly stage: Stage;
  readonly stepBoundary: FraudProofPreSubmitBoundary | undefined;
}): Readonly<{
  fieldGrammar: FaultProofFieldGrammarDeployment;
  preSubmitBoundary: FraudProofPreSubmitBoundary | undefined;
}> => {
  if (journal === undefined)
    return { fieldGrammar, preSubmitBoundary: stepBoundary };
  const certificateHashes: string[] = [];
  return {
    fieldGrammar: journalFaultProofFieldGrammar(
      fieldGrammar,
      journal.auxiliaryBoundary(
        "certificate",
        familyIdentity,
        stage,
        certificateHashes,
      ),
    ),
    preSubmitBoundary:
      stepBoundary === undefined
        ? undefined
        : async (transaction) => {
            for (const txHash of certificateHashes)
              await journal.confirmAuxiliary(txHash);
            await stepBoundary(transaction);
          },
  };
};
