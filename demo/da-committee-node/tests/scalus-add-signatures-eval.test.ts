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
import { packSortedSignatureWitnesses } from "../src/coordinator/witnesses.js";
import {
  daAttestationValidatorsFromDeployment,
  type MidgardNodeDeployment,
} from "../src/l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../src/l1/reference-scripts.js";
import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import { bytesToHex } from "../src/utils/hex.js";
import { loadDaDeploymentFixture } from "./helpers/deployment-fixture.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;

describe("Scalus DA add-signatures evaluation", () => {
  // Q63 (F04 §4) floors `da_threshold` at two, so the smallest attestation the
  // governor can represent needs two genuine committee signatures. This is the
  // only suite that runs the real Plutus validator over an AddSignatures body,
  // so it is where a wrong bitmap, witness ordering, or signed message would
  // actually be caught.
  it("completes the two-signature AddSignatures transaction with local UPLC", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    const contracts = daAttestationValidatorsFromDeployment(deployment);
    // `valid_datum` measures `committee` with a sorted-unique walker, so the
    // members are ordered by key and each signer's index follows that order.
    const committeeSigners = (
      await Promise.all([
        loadDaSigner(`hex:${"00".repeat(31)}01`),
        loadDaSigner(`hex:${"00".repeat(31)}02`),
      ])
    ).sort((left, right) => (left.publicKeyHex < right.publicKeyHex ? -1 : 1));
    const committeeHex = committeeSigners
      .map((signer) => signer.publicKeyHex)
      .join("");
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const headerHash = "ab".repeat(28);
    const daParamsDatum: SDK.DaParamsDatum = {
      committee: committeeHex,
      committee_signers_hash: committeeSignersHash,
      da_threshold: 2n,
      owners: ["22".repeat(28), "33".repeat(28)],
      update_threshold: 2n,
    };
    const availabilityCommitment = SDK.buildDaAvailabilityCommitmentV1({
      deploymentIdentity: deployment.hubOraclePolicyId,
      headerHash,
      payload: Buffer.from("public retained DA"),
      bondOwner: "76".repeat(28),
      responseGeometry: SDK.availabilityResponseGeometryV1({
        chunkByteLength: 14_020,
        trancheByteLength: 4 * 1_024 * 1_024,
        maxTrancheCount: 16,
      }),
    });
    const attestationDatum: SDK.DaAttestationDatum = {
      header_hash: headerHash,
      availability_commitment: availabilityCommitment,
      da_threshold: 2n,
      committee_signers_hash: committeeSignersHash,
      rescue_beneficiary: {
        paymentCredential: { PublicKeyCredential: ["56".repeat(28)] },
        stakeCredential: null,
      },
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
        packedWitnessesHex: packSortedSignatureWitnesses(
          committeeSigners.map((signer, signerIndex) =>
            signDaAttestation({
              signer,
              signerIndex,
              availabilityCommitment,
            }),
          ),
        ),
        signerIndexes: [0, 1],
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

const referenceScriptUtxos = (
  deployment: MidgardNodeDeployment,
  address: string,
): DaAttestationReferenceScripts => ({
  availabilityChallengeMinting: referenceScriptUtxo(
    "24",
    address,
    deployment.availabilityChallenge.mint.script,
  ),
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
