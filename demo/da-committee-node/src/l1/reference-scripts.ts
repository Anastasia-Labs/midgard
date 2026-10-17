import {
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type {
  MidgardDeploymentContract,
  MidgardDeploymentOutRef,
  MidgardNodeDeployment,
} from "./deployment.js";

/**
 * The reference scripts the attestation round reads: the DA attestation mint
 * (init, apply burn) and spend (add-signatures, apply), and the state-queue
 * mint and spend that apply runs. Apply reads the pooled DA bond as a plain
 * reference input and never runs the pool's scripts, and nothing in the round
 * mints under the availability policy, so neither is here.
 */
export type DaAttestationReferenceScripts = {
  readonly daAttestationMinting: UTxO;
  readonly daAttestationSpending: UTxO;
  readonly stateQueueMinting: UTxO;
  readonly stateQueueSpending: UTxO;
};

export const fetchDaAttestationReferenceScripts = async (
  lucid: Pick<LucidEvolution, "utxosByOutRef">,
  deployment: MidgardNodeDeployment,
): Promise<DaAttestationReferenceScripts> => {
  const targets = [
    {
      name: "DA attestation minting",
      contract: deployment.daAttestation.mint,
    },
    {
      name: "DA attestation spending",
      contract: deployment.daAttestation.spend,
    },
    {
      name: "state queue minting",
      contract: deployment.stateQueue.mint,
    },
    {
      name: "state queue spending",
      contract: deployment.stateQueue.spend,
    },
  ] as const;
  const utxos = await lucid.utxosByOutRef(
    targets.map((target) =>
      requireDeploymentOutRef(target.name, target.contract),
    ),
  );
  const byOutRef = new Map(utxos.map((utxo) => [outRefKey(utxo), utxo]));
  const resolved = targets.map((target) =>
    requireReferenceScript(target.name, target.contract, byOutRef),
  );
  return {
    daAttestationMinting: resolved[0]!,
    daAttestationSpending: resolved[1]!,
    stateQueueMinting: resolved[2]!,
    stateQueueSpending: resolved[3]!,
  };
};

const requireReferenceScript = (
  name: string,
  contract: MidgardDeploymentContract,
  byOutRef: ReadonlyMap<string, UTxO>,
): UTxO => {
  const refLabel = deploymentOutRefKey(requireDeploymentOutRef(name, contract));
  const utxo = byOutRef.get(refLabel);
  if (utxo === undefined) {
    throw new Error(`missing ${name} reference script UTxO at ${refLabel}`);
  }
  if (utxo.scriptRef === undefined) {
    throw new Error(
      `${name} reference script UTxO ${refLabel} has no scriptRef`,
    );
  }
  const actualHash = validatorToScriptHash(utxo.scriptRef as never);
  if (actualHash !== contract.scriptHash) {
    throw new Error(
      `${name} reference script hash mismatch at ${refLabel}: expected=${contract.scriptHash}, actual=${actualHash}`,
    );
  }
  return utxo;
};

const requireDeploymentOutRef = (
  name: string,
  contract: MidgardDeploymentContract,
): MidgardDeploymentOutRef => {
  if (contract.refScriptOutRef === null) {
    throw new Error(`${name} reference script outRef is required`);
  }
  return contract.refScriptOutRef;
};

export const deploymentOutRefKey = (outRef: MidgardDeploymentOutRef): string =>
  `${outRef.txHash}#${outRef.outputIndex.toString()}`;

const outRefKey = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
