import type { LucidEvolution, Network } from "@lucid-evolution/lucid";

import {
  FIELD_GRAMMAR_CERTIFICATE_SPEND,
  resolveFaultProofFieldGrammarDeployment,
} from "./field-grammar-certificate.resolve-fault-proof-field-grammar-deployment.js";
import type { FaultProofFieldGrammarDeployment } from "./field-grammar-certificate.route-fault-proof-field-grammar.js";
import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import { fetchUtxoByOutRef } from "./runtime.js";

/**
 * {@link resolveFaultProofFieldGrammarDeployment} for a command that loads its
 * deployment from files: the certificate members' published reference scripts
 * come from the deployment info, the spend member's reference UTxO from the
 * chain, and the validator from the blueprint, exactly as the workflows
 * resolve them from a bound manifest. A deployment info without both members
 * fails here, naming them.
 */
export const resolveFaultProofFieldGrammarFromDeploymentInfo = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  fieldPreimageCertificatePolicyId,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly label: string;
}): Promise<FaultProofFieldGrammarDeployment> => {
  const contracts = parseContractDeploymentInfo(deploymentInfo);
  const referenceScriptsByContract = Object.freeze(
    Object.fromEntries(
      Object.entries(contracts).flatMap(([name, entry]) =>
        entry.refScriptUTxO == null
          ? []
          : [
              [
                name,
                {
                  outRef: `${entry.refScriptUTxO.txHash}#${entry.refScriptUTxO.outputIndex.toString()}`,
                  scriptHash: entry.scriptHash,
                },
              ] as const,
            ],
      ),
    ),
  );
  const spendReference =
    contracts[FIELD_GRAMMAR_CERTIFICATE_SPEND]?.refScriptUTxO;
  const referenceScriptUtxo =
    spendReference == null
      ? undefined
      : await fetchUtxoByOutRef({
          lucid,
          outRef: spendReference,
          label: `${label} field grammar certificate reference script`,
        });
  return resolveFaultProofFieldGrammarDeployment({
    binding: {
      blueprint: blueprint as never,
      network,
      referenceScriptsByContract,
    },
    fieldPreimageCertificatePolicyId,
    referenceScriptUtxo,
    label,
  });
};
