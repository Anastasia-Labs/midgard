import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Network } from "@lucid-evolution/lucid";

import { faultProofMemberFromManifest } from "./midgard-contracts.linear-fault-proof-chain-from-manifest.js";

/** Restore the recorded SDK chain using purpose-checked manifest validators. */
export const validationTraceDisputeFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
): SDK.FaultProofContractChains["validationTraceDispute"] =>
  SDK.restoreValidationTraceDisputeScripts({
    resolve: (contract) =>
      faultProofMemberFromManifest(network, manifest, sourcePath, contract),
    missingMember: (member) => {
      throw new Error(
        `Deployment manifest at "${sourcePath}" does not record validation-trace ${member}; a manifest-sourced contract bundle cannot provide it`,
      );
    },
  });
