import { readFile } from "node:fs/promises";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, it } from "vitest";

import { buildAddSignaturesTx } from "../src/coordinator/tx-builders.js";
import {
  daAttestationValidatorsFromDeployment,
  parseMidgardNodeDeploymentInfo,
  type MidgardNodeDeployment,
} from "../src/l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../src/l1/reference-scripts.js";
import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import { bytesToHex } from "../src/utils/hex.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;

describe("Scalus DA add-signatures evaluation", () => {
  it("completes the one-signature AddSignatures transaction with local UPLC", async () => {
    const deployment = await loadRealDeployment();
    const contracts = daAttestationValidatorsFromDeployment(deployment);
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
    );
    const headerHash = "ab".repeat(28);
    const daParamsDatum: SDK.DaParamsDatum = {
      committee: signer.publicKeyHex,
      committee_signers_hash: committeeSignersHash,
      da_threshold: 1n,
      owners: ["22".repeat(28)],
      update_threshold: 1n,
    };
    const attestationDatum: SDK.DaAttestationDatum = {
      header_hash: headerHash,
      da_threshold: 1n,
      committee_signers_hash: committeeSignersHash,
      attested_signers: SDK.EMPTY_ATTESTED_SIGNER_BITMAP,
      attestation_count: 0n,
    };
    const daParamsUtxo = fixtureUtxo({
      txHash: "10".repeat(32),
      address: contracts.daParamsGovernor.spendingScriptAddress,
      assets: {
        lovelace: 5_000_000n,
        [SDK.daParamsUnit(contracts.daParamsGovernor)]: 1n,
      },
      datum: Data.to(daParamsDatum as never, SDK.DaParamsDatum as never),
    });
    const attestationUtxo = fixtureUtxo({
      txHash: "11".repeat(32),
      address: contracts.daAttestation.spendingScriptAddress,
      assets: {
        lovelace: 5_000_000n,
        [SDK.daAttestationUnit(contracts.daAttestation, headerHash)]: 1n,
      },
      datum: Data.to(
        attestationDatum as never,
        SDK.DaAttestationDatum as never,
      ),
    });
    const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([operator], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom", {
      evaluator: createScalusEvaluator(),
    });
    lucid.selectWallet.fromSeed(operator.seedPhrase);
    const referenceScripts = referenceScriptUtxos(
      deployment,
      await lucid.wallet().address(),
    );

    try {
      await buildAddSignaturesTx({
        lucid,
        contracts,
        daParamsUtxo,
        attestationUtxo,
        attestationDatum,
        packedWitnessesHex: signDaAttestation({
          signer,
          signerIndex: 0,
          headerHash,
        }),
        signerIndexes: [0],
        referenceScripts,
      });
    } catch (error) {
      throw new Error(formatErrorChain(error));
    }
  });
});

const formatErrorChain = (error: unknown): string => {
  const parts: string[] = [];
  let current: unknown = error;
  while (current instanceof Error) {
    parts.push(current.message);
    current = current.cause;
  }
  if (current !== undefined) {
    parts.push(String(current));
  }
  return parts.join(" -> ");
};

const loadRealDeployment = async (): Promise<MidgardNodeDeployment> => {
  const path = join(
    process.cwd(),
    "../midgard-node/deploymentInfo/contract-deployment-info.json",
  );
  const parsed = JSON.parse(await readFile(path, "utf8")) as Record<
    string,
    unknown
  >;
  const deployment = parseMidgardNodeDeploymentInfo(parsed, "Preprod");
  if (deployment === undefined) {
    throw new Error("real Midgard deployment fixture did not parse");
  }
  return deployment;
};

const referenceScriptUtxos = (
  deployment: MidgardNodeDeployment,
  address: string,
): DaAttestationReferenceScripts => ({
  daAttestationMinting: referenceScriptUtxo(
    "20",
    address,
    deployment.daAttestation.mint.script,
  ),
  daAttestationSpending: referenceScriptUtxo(
    "21",
    address,
    deployment.daAttestation.spend.script,
  ),
  stateQueueMinting: referenceScriptUtxo(
    "22",
    address,
    deployment.stateQueue.mint.script,
  ),
  stateQueueSpending: referenceScriptUtxo(
    "23",
    address,
    deployment.stateQueue.spend.script,
  ),
});

const referenceScriptUtxo = (
  byte: string,
  address: string,
  scriptRef: Script,
): UTxO =>
  fixtureUtxo({
    txHash: byte.repeat(32),
    address,
    assets: { lovelace: 5_000_000n },
    scriptRef,
  });

const fixtureUtxo = ({
  txHash,
  address,
  assets,
  datum,
  scriptRef,
}: {
  readonly txHash: string;
  readonly address: string;
  readonly assets: UTxO["assets"];
  readonly datum?: string;
  readonly scriptRef?: Script;
}): UTxO =>
  ({
    txHash,
    outputIndex: 0,
    address,
    assets,
    ...(datum === undefined ? {} : { datum }),
    ...(scriptRef === undefined ? {} : { scriptRef }),
  }) as UTxO;
