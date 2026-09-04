import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  daAttestationValidatorsFromDeployment,
  type MidgardNodeDeployment,
} from "../src/l1/deployment.js";
import { fetchDaAttestationReferenceScripts } from "../src/l1/reference-scripts.js";
import { loadDaDeploymentFixture } from "./helpers/deployment-fixture.js";

describe("DA attestation reference script resolver", () => {
  it("resolves explicit reference script UTxOs from deployment info", async () => {
    const deployment = await loadDaDeploymentFixture("Preview");
    const lucid = lucidWithReferenceScripts(referenceScriptUtxos(deployment));

    await expect(
      fetchDaAttestationReferenceScripts(lucid, deployment),
    ).resolves.toMatchObject({
      daAttestationMinting: {
        txHash: deployment.daAttestation.mint.refScriptOutRef!.txHash,
        outputIndex: deployment.daAttestation.mint.refScriptOutRef!.outputIndex,
      },
      stateQueueSpending: {
        txHash: deployment.stateQueue.spend.refScriptOutRef!.txHash,
        outputIndex: deployment.stateQueue.spend.refScriptOutRef!.outputIndex,
      },
    });
  });

  it("creates SDK validator objects from deployment info", async () => {
    const deployment = await loadDaDeploymentFixture("Preview");
    const validators = daAttestationValidatorsFromDeployment(deployment);

    expect(validators.daAttestation).toMatchObject({
      policyId: deployment.daAttestation.policyId,
      spendingScriptAddress: deployment.daAttestation.spendingScriptAddress,
    });
    expect(validators.stateQueue.spendingScriptHash).toBe(
      deployment.stateQueue.spendingScriptHash,
    );
  });

  it("fails closed when a resolved UTxO has the wrong scriptRef", async () => {
    const deployment = await loadDaDeploymentFixture("Preview");
    const utxos = referenceScriptUtxos(deployment);
    const badStateQueueSpend = {
      ...utxos[4]!,
      scriptRef: deployment.daAttestation.spend.script,
    };
    const lucid = lucidWithReferenceScripts([
      utxos[0]!,
      utxos[1]!,
      utxos[2]!,
      utxos[3]!,
      badStateQueueSpend,
    ]);

    await expect(
      fetchDaAttestationReferenceScripts(lucid, deployment),
    ).rejects.toThrow(/state queue spending reference script hash mismatch/);
  });
});

type ParsedDeployment = MidgardNodeDeployment;

const referenceScriptUtxos = (deployment: ParsedDeployment): UTxO[] => [
  referenceScriptUtxo(
    deployment.availabilityChallenge.mint.refScriptOutRef,
    deployment.availabilityChallenge.mint.script,
  ),
  referenceScriptUtxo(
    deployment.daAttestation.mint.refScriptOutRef,
    deployment.daAttestation.mint.script,
  ),
  referenceScriptUtxo(
    deployment.daAttestation.spend.refScriptOutRef,
    deployment.daAttestation.spend.script,
  ),
  referenceScriptUtxo(
    deployment.stateQueue.mint.refScriptOutRef,
    deployment.stateQueue.mint.script,
  ),
  referenceScriptUtxo(
    deployment.stateQueue.spend.refScriptOutRef,
    deployment.stateQueue.spend.script,
  ),
];

const referenceScriptUtxo = (
  outRef: { readonly txHash: string; readonly outputIndex: number } | null,
  scriptRef: UTxO["scriptRef"],
): UTxO => {
  if (outRef === null) {
    throw new Error("fixture contract is expected to carry a refScriptUTxO");
  }
  return {
    txHash: outRef.txHash,
    outputIndex: outRef.outputIndex,
    address: "addr_test1vrm9x2c9s7ccg8vlt8l2f33frf84m9e8u9d7jwsu4kkwkgg7d4u73",
    assets: { lovelace: 4_000_000n },
    scriptRef,
  } as UTxO;
};

const lucidWithReferenceScripts = (
  utxos: readonly UTxO[],
): Pick<LucidEvolution, "utxosByOutRef"> => ({
  utxosByOutRef: async (outRefs) =>
    outRefs.flatMap((outRef) =>
      utxos.filter(
        (utxo) =>
          utxo.txHash === outRef.txHash &&
          utxo.outputIndex === outRef.outputIndex,
      ),
    ),
});
