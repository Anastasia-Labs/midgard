import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type Network } from "@lucid-evolution/lucid";

import type { MintingValidator, SpendingValidator } from "../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeAuthenticatedValidator,
} from "./contracts/blueprint.js";

export const FIELD_GRAMMAR_CERTIFICATE_CONTRACT_TITLES = Object.freeze({
  mint: deployedTitle("fieldGrammarCertificateMint"),
  spend: deployedTitle("fieldGrammarCertificateSpend"),
});

/**
 * The field grammar certificate validator, applied to its one parameter: the
 * field-preimage certificate policy, which the door needs to open a tier-3
 * field. Its mint and spend handlers are one script, so the policy id is the
 * spend credential and certificates live at the policy's own address.
 */
export const buildFieldGrammarCertificateValidator = ({
  blueprint,
  network,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly fieldPreimageCertificatePolicyId: string;
}): SpendingValidator & MintingValidator => {
  if (!/^[0-9a-f]{56}$/u.test(fieldPreimageCertificatePolicyId)) {
    throw new Error(
      "field-preimage certificate policy id must be exactly 28 bytes of hexadecimal",
    );
  }
  const titles = FIELD_GRAMMAR_CERTIFICATE_CONTRACT_TITLES;
  const validator = makeAuthenticatedValidator(
    network,
    applyBlueprintParams(blueprint, titles.mint, [
      fieldPreimageCertificatePolicyId,
    ]),
    applyBlueprintParams(blueprint, titles.spend, [
      fieldPreimageCertificatePolicyId,
    ]),
  );
  if (validator.policyId !== validator.spendingScriptHash) {
    throw new Error(
      "field grammar certificate mint and spend handlers must share one script",
    );
  }
  return validator;
};
