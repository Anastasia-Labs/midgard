import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ProofMaterialsView } from "./proof-materials-view.js";

/** The wallet a workflow sweeps its proof materials through, and the view
 * its finds go through: installed by its restricted wallet. */
export type WorkflowSweepWallet = Readonly<{
  lucid: LucidEvolution;
  fundingAddress: string;
  view: ProofMaterialsView;
}>;

/**
 * One proof objective's wallet wiring: the sweep wallet and proof-materials
 * view its restricted wallet installs (`workflow-wallet.ts`), read by the
 * objective's proof-materials sweep. The restricted wallet and every run of
 * the objective share the one object.
 */
export type WorkflowObjectiveWallet = Readonly<{
  category: FraudProofCatalogueCategoryName;
  headerHash: string;
  /** The sweep wallet the objective's restricted wallet installed, if any. */
  sweepWallet: () => WorkflowSweepWallet | undefined;
  installSweepWallet: (wallet: WorkflowSweepWallet) => void;
  /** The proof-materials view of the objective's signer, if known. */
  proofMaterialsView: () => ProofMaterialsView | undefined;
  installProofMaterialsView: (view: ProofMaterialsView) => void;
}>;

export const createWorkflowObjectiveWallet = ({
  category,
  headerHash,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
}): WorkflowObjectiveWallet => {
  let sweep: WorkflowSweepWallet | undefined;
  let view: ProofMaterialsView | undefined;
  return Object.freeze({
    category,
    headerHash,
    sweepWallet: () => sweep,
    installSweepWallet: (wallet) => {
      sweep = wallet;
    },
    proofMaterialsView: () => view,
    installProofMaterialsView: (installed) => {
      view = installed;
    },
  });
};

/**
 * One invocation of a workflow over its objective's wallet wiring. A fresh
 * object per invocation, so the actuation permit the invocation holds binds
 * to it alone (`actuation-permit.ts`).
 */
export type WorkflowRun = Readonly<{
  objectiveWallet: WorkflowObjectiveWallet;
}>;

export const createWorkflowRun = (
  objectiveWallet: WorkflowObjectiveWallet,
): WorkflowRun => Object.freeze({ objectiveWallet });
