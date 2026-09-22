import type {
  FraudProofRawL1Transaction,
  FraudProofRawL1Utxo,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { reconstructWatcherAvailabilityPublishedPayload } from "../../src/availability/published-payload.js";

const policy = "11".repeat(28);
const queuePolicy = "12".repeat(28);
const deployment = "13".repeat(28);
const headerHash = "14".repeat(28);
const address = credentialToAddress("Preprod", {
  type: "Script",
  hash: "15".repeat(28),
});
const out = (index: number, assets: UTxO["assets"], datum: string): UTxO => ({
  txHash: "16".repeat(32),
  outputIndex: index,
  address,
  assets,
  datum,
});
const rawOutput = (utxo: UTxO): FraudProofRawL1Utxo => ({
  outRef: `${utxo.txHash}#${utxo.outputIndex}`,
  outputCbor: utxoToCore(utxo).output().to_canonical_cbor_hex(),
  datumCbor: utxo.datum ?? null,
  referenceScriptCbor: null,
});
const transaction = (
  inputs: readonly UTxO[],
  outputs: readonly UTxO[],
  blockNo: number,
) => {
  const bodyInputs = CML.TransactionInputList.new();
  for (const input of inputs)
    bodyInputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(input.txHash),
        BigInt(input.outputIndex),
      ),
    );
  const bodyOutputs = CML.TransactionOutputList.new();
  for (const output of outputs) bodyOutputs.add(utxoToCore(output).output());
  const body = CML.TransactionBody.new(bodyInputs, bodyOutputs, 500_000n);
  body.set_ttl(2_000n + BigInt(blockNo));
  const txHash = CML.hash_transaction(body).to_hex();
  const raw: FraudProofRawL1Transaction = {
    txHash,
    bodyCbor: body.to_canonical_cbor_hex(),
    witnessSetCbor: "a0",
    redeemersCbor: null,
    isValid: true,
    inclusionPoint: {
      slot: blockNo.toString(),
      blockNo: blockNo.toString(),
      blockHash: "17".repeat(32),
      pointId: "18".repeat(32),
    },
    confirmationDepth: 30,
    resolvedInputs: inputs.map(rawOutput),
    resolvedReferenceInputs: [],
  };
  return {
    raw,
    outputs: outputs.map((output, outputIndex) => ({
      ...output,
      txHash,
      outputIndex,
    })),
  };
};

const historyFixture = () => {
  const parameters = SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(
      SDK.DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
    ),
    daBondLovelace: 10_000_000_000n,
    challengerBondLovelace: 10_000_000_000n,
    maxOpenFeeLovelace: 500_000n,
    maxPublicationFeeLovelace: 500_000n,
    maxSettlementFeeLovelace: 500_000n,
    maxCloseFeeLovelace: 1_000_000n,
    maxTimeoutFeeLovelace: 1_200_000n,
  });
  const bytes = Uint8Array.from({ length: 16_000 }, (_, index) => index % 251);
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: deployment,
    headerHash,
    payload: bytes,
    bondOwner: "19".repeat(28),
    responseGeometry: parameters.response_geometry,
  });
  const bondAsset = SDK.daAvailabilityBondAssetName({
    transactionId: "20".repeat(32),
    outputIndex: 0n,
  });
  const available: SDK.DaAvailabilityBondDatum = {
    Available: {
      commitment,
      da_bond_asset_name: bondAsset,
      committee_signers_hash: "21".repeat(32),
      attested_signers: "80" + "00".repeat(31),
    },
  };
  const bond = out(
    0,
    { lovelace: parameters.da_bond_lovelace, [policy + bondAsset]: 1n },
    SDK.encodeDaAvailabilityBondDatum(available),
  );
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    availableBond: available,
    bondInputOutRef: SDK.outputReferenceFromUTxO(bond),
    challenger: "22".repeat(28),
    openedAt: 1_000_000n,
    parameters,
  });
  const unit =
    policy +
    SDK.daAvailabilityTrancheAssetName({
      challengeAssetName: plan.challengeAssetName,
      trancheIndex: 0,
    });
  const open = transaction(
    [bond],
    [
      out(
        0,
        { ...bond.assets, [policy + plan.challengeAssetName]: 1n },
        SDK.encodeDaAvailabilityBondDatum(plan.challengedBond),
      ),
      out(
        1,
        { lovelace: plan.trancheFunding[0]!.initialLovelace, [unit]: 1n },
        SDK.encodeDaAvailabilityTrancheDatum(plan.trancheThreads[0]!),
      ),
    ],
    1,
  );
  const publications = SDK.planDaAvailabilityPublications({
    commitment,
    challengeAssetName: plan.challengeAssetName,
    payload: bytes,
  })[0]!.publications;
  let state = plan.trancheThreads[0]!;
  let thread = open.outputs[1]!;
  const history: FraudProofRawL1Transaction[] = [open.raw];
  for (const [index, publication] of publications.entries()) {
    state = SDK.advanceDaAvailabilityTranche({
      active: state,
      publication,
      responseGeometry: parameters.response_geometry,
      inclusiveValidityUpper: 2_000_000n + BigInt(index + 2) * 1_000n,
      carrierOutputIndex: 1n,
    });
    const published = transaction(
      [thread],
      [
        out(0, thread.assets, SDK.encodeDaAvailabilityTrancheDatum(state)),
        out(
          1,
          { lovelace: 2_000_000n },
          SDK.encodeDaAvailabilityPublicationDatum(
            publication,
            parameters.response_geometry,
            commitment.tranche_descriptors[0]!,
          ),
        ),
      ],
      index + 2,
    );
    history.push(published.raw);
    thread = published.outputs[0]!;
  }
  const input = {
    headerHash,
    terminalCommitment:
      SDK.daAvailabilityPublishedTerminalCommitment(commitment),
    deploymentIdentity: deployment,
    availabilityAddress: address,
    availabilityPolicyId: policy,
    stateQueuePolicyId: queuePolicy,
    parameters,
    slotToUnixTime: (slot: number) => slot * 1_000,
    readHistory: async (requested: string) =>
      requested.startsWith(queuePolicy) ? [open.raw] : [...history].reverse(),
  };
  return { input, bytes, history, open, unit };
};

describe("watcher public L1 payload reconstruction", () => {
  it("reconstructs spent carrier bytes by causal token spends even when history arrives out of order", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload(fixture.input),
    ).resolves.toEqual(Buffer.from(fixture.bytes));
  });

  it("rejects incomplete publication history instead of substituting retained private bytes", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        readHistory: async (requested) =>
          requested.startsWith(queuePolicy)
            ? [fixture.open.raw]
            : [fixture.open.raw, fixture.history[2]!],
      }),
    ).rejects.toThrow("missing or conflicting canonical successor");
  });

  it("binds reconstructed bytes to the exact Published marker and deployment", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        terminalCommitment: "ff".repeat(32),
      }),
    ).rejects.toThrow("Published queue commitment differs");
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        deploymentIdentity: "ff".repeat(28),
      }),
    ).rejects.toThrow("Published queue commitment differs");
  });

  it("rejects a duplicated canonical spend of the same tranche", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        readHistory: async (requested) =>
          requested.startsWith(queuePolicy)
            ? [fixture.open.raw]
            : [...fixture.history, fixture.history[1]!],
      }),
    ).rejects.toThrow("missing or conflicting canonical successor");
  });
});
