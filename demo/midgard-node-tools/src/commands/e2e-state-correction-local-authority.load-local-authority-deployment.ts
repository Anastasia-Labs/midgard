import { readFile } from "node:fs/promises";

import type { SpendingValidator } from "@lucid-evolution/lucid";
import { validatorToAddress } from "@lucid-evolution/lucid";
import { parseDeploymentManifestValue } from "midgard-node/deployment-manifest";

import { parseReleaseL1FinalityPolicy } from "./e2e-release-finality-policy.js";
import { requireManifestContract } from "./e2e-state-correction-local-authority.create-local-kupmios-state-correction-authority.js";
import {
  type LocalAuthorityDeployment,
  releaseEconomicsPolicyFromDeploymentManifest,
} from "./e2e-state-correction-local-authority.fetch-json.js";

export const loadLocalAuthorityDeployment = async (
  manifestPath: string,
): Promise<LocalAuthorityDeployment> => {
  const manifest = parseDeploymentManifestValue(
    JSON.parse(await readFile(manifestPath, "utf8")) as unknown,
  );
  if (manifest.network !== "Preprod") {
    throw new Error("Q57 local authority requires a Preprod manifest");
  }
  const stateQueueSpend = requireManifestContract(manifest, "stateQueueSpend");
  const stateQueueMint = requireManifestContract(manifest, "stateQueueMint");
  const reserveSpend = requireManifestContract(manifest, "reserveSpend");
  const spendingScript: SpendingValidator = {
    type: stateQueueSpend.contract.type,
    script: stateQueueSpend.contract.cborHex,
  } as SpendingValidator;
  return {
    manifestId: manifest.manifestId,
    stateQueueAddress: validatorToAddress("Preprod", spendingScript),
    stateQueuePolicyId: stateQueueMint.scriptHash,
    reserveAddress: validatorToAddress("Preprod", {
      type: reserveSpend.contract.type,
      script: reserveSpend.contract.cborHex,
    } as SpendingValidator),
    // The release policy carries the manifest's rollback fields only; the
    // commit-event depth binds event inclusion, not release finality.
    finalityPolicy: parseReleaseL1FinalityPolicy({
      confirmationDepth: manifest.l1Finality.confirmationDepth,
      automaticRecoveryMaxDepth: manifest.l1Finality.automaticRecoveryMaxDepth,
      deepRollbackPolicy: manifest.l1Finality.deepRollbackPolicy,
    }),
    economicsPolicy: releaseEconomicsPolicyFromDeploymentManifest(manifest),
  };
};
