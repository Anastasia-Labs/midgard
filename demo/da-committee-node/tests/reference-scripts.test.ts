import { readFile } from "node:fs/promises";
import { join } from "node:path";

import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  daAttestationValidatorsFromDeployment,
  parseMidgardNodeDeploymentInfo,
} from "../src/l1/deployment.js";
import { fetchDaAttestationReferenceScripts } from "../src/l1/reference-scripts.js";

describe("DA attestation reference script resolver", () => {
  it("resolves explicit reference script UTxOs from deployment info", async () => {
    const deployment = await loadRealDeployment();
    const lucid = lucidWithReferenceScripts(
      referenceScriptUtxos(deployment),
    );

    await expect(
      fetchDaAttestationReferenceScripts(lucid, deployment),
    ).resolves.toMatchObject({
      daAttestationMinting: {
        txHash: deployment.daAttestation.mint.refScriptOutRef.txHash,
        outputIndex: deployment.daAttestation.mint.refScriptOutRef.outputIndex,
      },
      stateQueueSpending: {
        txHash: deployment.stateQueue.spend.refScriptOutRef.txHash,
        outputIndex: deployment.stateQueue.spend.refScriptOutRef.outputIndex,
      },
    });
  });

  it("creates SDK validator objects from deployment info", async () => {
    const deployment = await loadRealDeployment();
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
    const deployment = await loadRealDeployment();
    const utxos = referenceScriptUtxos(deployment);
    const badStateQueueSpend = {
      ...utxos[3]!,
      scriptRef: deployment.daAttestation.spend.script,
    };
    const lucid = lucidWithReferenceScripts([
      utxos[0]!,
      utxos[1]!,
      utxos[2]!,
      badStateQueueSpend,
    ]);

    await expect(
      fetchDaAttestationReferenceScripts(lucid, deployment),
    ).rejects.toThrow(/state queue spending reference script hash mismatch/);
  });
});

const loadRealDeployment = async () => {
  const path = join(
    process.cwd(),
    "../midgard-node/deploymentInfo/contract-deployment-info.json",
  );
  const parsed = JSON.parse(await readFile(path, "utf8")) as Record<
    string,
    unknown
  >;
  const deployment = parseMidgardNodeDeploymentInfo(
    withReferenceScriptOutRefs(parsed),
    "Preview",
  );
  if (deployment === undefined) {
    throw new Error("real Midgard deployment fixture did not parse");
  }
  return deployment;
};

const withReferenceScriptOutRefs = (
  deploymentInfo: Record<string, unknown>,
): Record<string, unknown> => {
  const clone = structuredClone(deploymentInfo) as Record<string, unknown>;
  const contracts = clone.contracts;
  if (typeof contracts !== "object" || contracts === null) {
    return clone;
  }
  [
    "daAttestationMint",
    "daAttestationSpend",
    "daParamsGovernorMint",
    "daParamsGovernorSpend",
    "stateQueueMint",
    "stateQueueSpend",
  ].forEach((key, index) => {
    const entry = (contracts as Record<string, unknown>)[key];
    if (typeof entry !== "object" || entry === null) {
      return;
    }
    (entry as Record<string, unknown>).refScriptUTxO = {
      txHash: (index + 1).toString(16).padStart(2, "0").repeat(32),
      outputIndex: index,
    };
  });
  return clone;
};

type ParsedDeployment = Awaited<ReturnType<typeof loadRealDeployment>>;

const referenceScriptUtxos = (deployment: ParsedDeployment): UTxO[] => [
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
  outRef: { readonly txHash: string; readonly outputIndex: number },
  scriptRef: UTxO["scriptRef"],
): UTxO =>
  ({
    txHash: outRef.txHash,
    outputIndex: outRef.outputIndex,
    address: "addr_test1vrm9x2c9s7ccg8vlt8l2f33frf84m9e8u9d7jwsu4kkwkgg7d4u73",
    assets: { lovelace: 4_000_000n },
    scriptRef,
  }) as UTxO;

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
