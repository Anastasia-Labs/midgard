import {
  validatorToScriptHash,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import type {
  MidgardDeploymentContract,
  MidgardNodeDeployment,
  MidgardDeploymentOutRef,
} from "./deployment.js";

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
    targets.map((target) => target.contract.refScriptOutRef),
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
  const refLabel = deploymentOutRefKey(contract.refScriptOutRef);
  const utxo = byOutRef.get(refLabel);
  if (utxo === undefined) {
    throw new Error(`missing ${name} reference script UTxO at ${refLabel}`);
  }
  if (utxo.scriptRef === undefined) {
    throw new Error(`${name} reference script UTxO ${refLabel} has no scriptRef`);
  }
  const actualHash = validatorToScriptHash(utxo.scriptRef as never);
  if (actualHash !== contract.scriptHash) {
    throw new Error(
      `${name} reference script hash mismatch at ${refLabel}: expected=${contract.scriptHash}, actual=${actualHash}`,
    );
  }
  return utxo;
};

export const deploymentOutRefKey = (outRef: MidgardDeploymentOutRef): string =>
  `${outRef.txHash}#${outRef.outputIndex.toString()}`;

const outRefKey = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
