import * as SDK from "@al-ft/midgard-sdk";
import {
  type Network,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  FIELD_GRAMMAR_CERTIFICATE_MINT,
  FIELD_GRAMMAR_CERTIFICATE_SPEND,
  resolveFaultProofFieldGrammarDeployment,
} from "../../src/field-grammar-certificate.js";
import {
  type Blueprint,
  readBlueprint,
  realBlueprintPath,
} from "./emulator/blueprints.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";

let blueprint: Blueprint | undefined;
const validators = new Map<
  string,
  ReturnType<typeof SDK.buildFieldGrammarCertificateValidator>
>();

/** The compiled blueprint, read once per test worker. */
export const realFaultProofBlueprint = (): Blueprint =>
  (blueprint ??= readBlueprint(realBlueprintPath));

/**
 * The field grammar certificate the compiled blueprint applies over a
 * field-preimage certificate policy: the validator a deployment's
 * `fieldGrammarCertificateSpend` reference script must run, and whose policy
 * the `fieldGrammarCertificateMint` manifest member names.
 */
export const fieldGrammarCertificateValidatorFixture = ({
  network,
  fieldPreimageCertificatePolicyId,
}: {
  readonly network: Network;
  readonly fieldPreimageCertificatePolicyId: string;
}) => {
  const key = `${network}:${fieldPreimageCertificatePolicyId}`;
  const cached = validators.get(key);
  if (cached !== undefined) return cached;
  const validator = SDK.buildFieldGrammarCertificateValidator({
    blueprint: SDK.parseFaultProofBlueprint(realFaultProofBlueprint()),
    network,
    fieldPreimageCertificatePolicyId,
  });
  validators.set(key, validator);
  return validator;
};

/**
 * Deploys the field grammar certificate the way a deployment does, then
 * resolves it through the same startup resolver the production workflows
 * call: the funder publishes the `fieldGrammarCertificateSpend` reference
 * script, the manifest names both certificate members, and the resolver checks
 * the published script and the minting policy against the blueprint.
 */
export const deployFieldGrammarCertificate = async ({
  lucid,
  blueprint,
  network,
  fieldPreimageCertificatePolicyId,
  label,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly blueprint: Blueprint;
  readonly network: Network;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly label: string;
}) => {
  const validator = SDK.buildFieldGrammarCertificateValidator({
    blueprint: SDK.parseFaultProofBlueprint(blueprint),
    network,
    fieldPreimageCertificatePolicyId,
  });
  const { utxo } = await publishPlainReferenceScriptUtxo({
    lucid,
    script: validator.spendingScript,
    label: "field grammar certificate",
  });
  const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
  // The certificate's mint and spend are one script: the mint member names
  // the same published reference under its policy.
  const referenceScriptsByContract = Object.freeze({
    [FIELD_GRAMMAR_CERTIFICATE_SPEND]: Object.freeze({
      outRef,
      scriptHash: validatorToScriptHash(utxo.scriptRef!),
    }),
    [FIELD_GRAMMAR_CERTIFICATE_MINT]: Object.freeze({
      outRef,
      scriptHash: validator.policyId,
    }),
  });
  const fieldGrammar = resolveFaultProofFieldGrammarDeployment({
    binding: { blueprint, network, referenceScriptsByContract },
    fieldPreimageCertificatePolicyId,
    referenceScriptUtxo: utxo,
    label,
  });
  return {
    fieldGrammar,
    policyId: validator.policyId,
    referenceScriptUtxo: utxo,
    referenceScriptsByContract,
    /** Both members by name, each the one published reference. */
    roster: [
      [FIELD_GRAMMAR_CERTIFICATE_SPEND, utxo],
      [FIELD_GRAMMAR_CERTIFICATE_MINT, utxo],
    ] as [string, UTxO][],
  };
};

/**
 * {@link deployFieldGrammarCertificate} from an emulator harness: its funder
 * publishes, over its field-preimage certificate policy.
 */
export const deployHarnessFieldGrammarCertificate = async (
  harness: Readonly<{
    funderLucid: Parameters<typeof publishPlainReferenceScriptUtxo>[0]["lucid"];
    realBlueprint: Blueprint;
    contracts: Readonly<{
      fieldPreimageCertificate: Readonly<{ policyId: string }>;
    }>;
  }>,
  label: string,
) =>
  await deployFieldGrammarCertificate({
    lucid: harness.funderLucid,
    blueprint: harness.realBlueprint,
    network: "Custom",
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    label,
  });
