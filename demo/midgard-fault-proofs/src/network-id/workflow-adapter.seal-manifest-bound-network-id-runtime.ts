import { type UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import {
  NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
  type ResolvedProverSigner,
} from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertManifestBoundWorkflowSigner,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import { type LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  type NetworkIdRemovalConfig,
  type NetworkIdWorkflowAdapterConfig,
} from "./workflow-adapter.create-network-id-raw-l1-observation-port.js";

export type ManifestBoundNetworkIdWorkflowConfig = Omit<
  NetworkIdWorkflowAdapterConfig,
  | "blueprint"
  | "network"
  | "contracts"
  | "stateQueueAddress"
  | "category"
  | "catalogue"
  | "removal"
  | "rawL1"
  | "terminalFacts"
  | "witnessReferenceScripts"
> & {
  readonly manifest: unknown;
  readonly blueprintJson: string;
  readonly deploymentInfo: unknown;
  readonly headerHash: string;
  readonly source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  readonly removal: Omit<
    NetworkIdRemovalConfig,
    | "deploymentInfo"
    | "category"
    | "isCurrentHead"
    | "requireReferenceScripts"
    | "stateQueueMutationLeaseCoordinator"
  > & {
    readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  };
  readonly witnessReferenceScripts: Required<
    Pick<
      FaultProofWitnessReferenceScripts,
      | "computationThreadMint"
      | "fraudProofMint"
      | "phasMembershipWithdraw"
      | "chunkedVerifyWithdraw"
      | "pexcludesWithdraw"
    >
  >;
};

export type ManifestBoundNetworkIdWorkflow = {
  readonly binding: FraudProofWorkflowDeploymentBinding<"networkId">;
  readonly adapterConfig: NetworkIdWorkflowAdapterConfig;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
};

export type ManifestBoundNetworkIdRuntimeSeal = {
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  readonly forcedStepReferenceScript?: UTxO;
  readonly forcedScanReferenceScript?: UTxO;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly removal: ManifestBoundNetworkIdWorkflowConfig["removal"] & {
    readonly requireReferenceScripts: true;
  };
};

/**
 * Pure manifest/runtime seal used before the production adapter is built.
 * Runtime objects may contain extra JavaScript properties despite their
 * TypeScript shape, so the reference-script-only removal flag is overwritten
 * after the caller object is spread. Every supplied reference UTxO is also
 * matched to its exact finalized manifest role, out-ref, and script hash.
 */
export const sealManifestBoundNetworkIdRuntime = ({
  binding,
  signer,
  stepReferenceScripts,
  forcedStepReferenceScript,
  forcedScanReferenceScript,
  fieldPreimageCertificateReferenceScript,
  witnessReferenceScripts,
  removal,
}: {
  readonly binding: Pick<
    FraudProofWorkflowDeploymentBinding<"networkId">,
    "network" | "referenceScriptsByContract"
  >;
  readonly signer: ResolvedProverSigner;
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  readonly forcedStepReferenceScript?: UTxO;
  readonly forcedScanReferenceScript?: UTxO;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: ManifestBoundNetworkIdWorkflowConfig["witnessReferenceScripts"];
  readonly removal: ManifestBoundNetworkIdWorkflowConfig["removal"];
}): ManifestBoundNetworkIdRuntimeSeal => {
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: signer.address,
    paymentKeyHash: signer.paymentKeyHash,
  });
  const requireReference = (contractName: string, utxo: UTxO): UTxO =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo,
    });
  return {
    stepReferenceScripts: [
      requireReference("fraudProofNetworkId", stepReferenceScripts[0]),
      requireReference("fraudProofNetworkIdStep02", stepReferenceScripts[1]),
    ],
    ...(forcedStepReferenceScript === undefined
      ? {}
      : {
          forcedStepReferenceScript: requireReference(
            "fraudProofNetworkIdForcedStep",
            forcedStepReferenceScript,
          ),
        }),
    ...(forcedScanReferenceScript === undefined
      ? {}
      : {
          forcedScanReferenceScript: requireReference(
            NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
            forcedScanReferenceScript,
          ),
        }),
    fieldPreimageCertificateReferenceScript: requireReference(
      "fieldPreimageCertificateMint",
      fieldPreimageCertificateReferenceScript,
    ),
    witnessReferenceScripts: {
      computationThreadMint: requireReference(
        "computationThreadMint",
        witnessReferenceScripts.computationThreadMint,
      ),
      fraudProofMint: requireReference(
        "fraudProofMint",
        witnessReferenceScripts.fraudProofMint,
      ),
      phasMembershipWithdraw: requireReference(
        "phasMembershipWithdraw",
        witnessReferenceScripts.phasMembershipWithdraw,
      ),
      chunkedVerifyWithdraw: requireReference(
        "chunkedVerifyWithdraw",
        witnessReferenceScripts.chunkedVerifyWithdraw,
      ),
      pexcludesWithdraw: requireReference(
        "pexcludesWithdraw",
        witnessReferenceScripts.pexcludesWithdraw,
      ),
    },
    removal: {
      ...removal,
      requireReferenceScripts: true,
    },
  };
};
