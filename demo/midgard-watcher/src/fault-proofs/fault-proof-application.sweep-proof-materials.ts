import { bindWorkflowRunActuation } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { runProofMaterialsSweep } from "@al-ft/midgard-fault-proofs/workflow/proof-materials-sweep-run";
import {
  createWorkflowObjectiveWallet,
  createWorkflowRun,
} from "@al-ft/midgard-fault-proofs/workflow/workflow-run";

import { watcherDeploymentReleaseFinalityAuthority } from "../runtime/deployment-identity.js";
import { buildCommonInfrastructure } from "./fault-proof-application.build-common-infrastructure.js";
import type { WatcherFaultProofApplication } from "./fault-proof-application.production-dependencies.js";

/**
 * The application's sweep of an objective's proof materials once the chain
 * alone completed it. The generic loader's infrastructure build installs
 * the prover's restricted wallet and the process's shared proof-materials
 * view on the objective's wallet wiring, as for any run of it, and the
 * sweep-only run (`runProofMaterialsSweep`) signs under the objective's
 * reconciliation permit. An application that only observes sweeps nothing
 * and says so.
 */
export const sweepProofMaterialsFrom =
  ({
    isExecutable,
    ...authority
  }: Omit<
    Parameters<typeof buildCommonInfrastructure>[0],
    "invocation" | "replayContext"
  > & {
    readonly isExecutable: () => boolean;
  }): WatcherFaultProofApplication["sweepProofMaterials"] =>
  async ({ terminal, actuationPermit, ...invocation }) => {
    if (!isExecutable()) return Object.freeze({ kind: "observing" });
    const deploymentFingerprint = authority.deploymentIdentity.manifestId;
    const { category, headerHash, decisionDigest } = invocation;
    const objectiveWallet = createWorkflowObjectiveWallet({
      category,
      headerHash,
    });
    // Installs the objective wallet's proof-materials view and sweep wallet.
    await buildCommonInfrastructure({
      ...authority,
      invocation: { ...invocation, deploymentFingerprint, objectiveWallet },
      replayContext: undefined,
    });
    const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
      authority.deploymentIdentity,
    ).verifyForWorkflow({ deploymentFingerprint });
    return await runProofMaterialsSweep({
      deploymentFingerprint,
      category,
      headerHash,
      run: bindWorkflowRunActuation({
        run: createWorkflowRun(objectiveWallet),
        permit: actuationPermit,
        decisionDigest,
        deploymentFingerprint,
        category,
        headerHash,
      }),
      terminal,
      releaseFinality,
    });
  };
