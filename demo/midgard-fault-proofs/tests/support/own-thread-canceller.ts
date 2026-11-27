/** The header-free canceller of our own threads over an emulator harness,
 * with the deployment as the manifest names it: the double-spend step
 * contracts by name and their published references. */
import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { UTxO } from "@lucid-evolution/lucid";

import { createOwnThreadCanceller } from "../../src/own-thread-cancel.js";
import type { makeFaultProofEmulatorHarness } from "./submit-init-emulator-shared.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

/** The deployment as the manifest names it: the double-spend step
 * contracts by name, and their published references. */
export const ownThreadCancelDeploymentOf = (h: Harness) => {
  const steps = h.contracts.fraudProofContracts.doubleSpend.steps;
  const contractScriptHashes: Record<string, string> = Object.fromEntries(
    deploymentFamilyStepNames("doubleSpend").map((name, index) => [
      name,
      steps[index]!.spendingScriptHash,
    ]),
  );
  const published = Object.values(h.faultProofReferenceScripts);
  return {
    contractScriptHashes,
    resolveReferenceScript: async ({
      role,
      contractName,
    }: {
      readonly role: string;
      readonly contractName: string;
    }): Promise<UTxO> => {
      if (role === "computationThreadMint")
        return h.witnessReferenceScripts.computationThreadMint!;
      const reference = published.find(
        ({ scriptHash }) => scriptHash === contractScriptHashes[contractName],
      );
      if (reference === undefined)
        throw new Error(`no published reference for ${contractName}`);
      return reference.utxo;
    },
  };
};

/** A fresh canceller over the harness, as a watcher (re)started now opens
 * one: nothing carried over but the chain. */
export const ownThreadCancellerOf = (h: Harness) => ({
  canceller: createOwnThreadCanceller({
    lucid: h.proverLucid,
    signer: h.proverSigner,
    deployment: ownThreadCancelDeploymentOf(h),
  }),
});
