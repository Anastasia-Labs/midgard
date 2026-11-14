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

const policy = "11".repeat(28);
export const queuePolicy = "12".repeat(28);
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
  ttlSlot = 2_000 + blockNo,
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
  body.set_ttl(BigInt(ttlSlot));
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

/**
 * Slots are whole seconds (`slotToUnixTime(slot) = slot * 1000`). A ledger
 * ttl is the EXCLUSIVE upper validity end, so a publication's inclusive upper
 * bound is `ttl * 1000 - 1`.
 */
export const historyFixture = (
  options: {
    /** Ttl of the final publication, as milliseconds past the response deadline. */
    finalTtlPastDeadlineMs?: bigint;
  } = {},
) => {
  const parameters = SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(
      SDK.DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
    ),
    ...SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
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
    responseGeometry: parameters.response_geometry,
  });
  // The challenger's plain funding coin; OpenChallenge derives the DACH
  // identity from its outref.
  const funding: UTxO = {
    txHash: "20".repeat(32),
    outputIndex: 0,
    address: credentialToAddress("Preprod", {
      type: "Key",
      hash: "19".repeat(28),
    }),
    assets: { lovelace: parameters.challenger_bond_lovelace },
  };
  // The open lands in block 1: its inclusive upper validity bound, the ttl
  // slot's start less one millisecond, anchors the response window.
  const openedAt = (2_000n + 1n) * 1_000n - 1n;
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    commitment,
    challengerFundingOutRef: SDK.outputReferenceFromUTxO(funding),
    challenger: "22".repeat(28),
    openedAt,
    parameters,
  });
  const unit =
    policy +
    SDK.daAvailabilityTrancheAssetName({
      challengeAssetName: plan.challengeAssetName,
      trancheIndex: 0,
    });
  const open = transaction(
    [funding],
    [
      out(
        0,
        {
          lovelace: plan.recordLovelace,
          [policy + plan.challengeAssetName]: 1n,
        },
        SDK.encodeDaAvailabilityChallengeRecord(plan.record, parameters),
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
    const isFinal = index === publications.length - 1;
    const finalTtlMs =
      options.finalTtlPastDeadlineMs === undefined
        ? undefined
        : plan.responseDeadline + options.finalTtlPastDeadlineMs;
    if (finalTtlMs !== undefined && finalTtlMs % 1_000n !== 0n)
      throw new Error("final publication ttl must land on a slot boundary");
    const ttlSlot =
      isFinal && finalTtlMs !== undefined
        ? Number(finalTtlMs / 1_000n)
        : 2_000 + index + 2;
    const inclusiveValidityUpper = BigInt(ttlSlot) * 1_000n - 1n;
    state = SDK.advanceDaAvailabilityTranche({
      active: state,
      publication,
      responseGeometry: parameters.response_geometry,
      // The datum transition does not record the bound; the negative case
      // builds its (on-chain inadmissible) successor with the deadline so the
      // watcher, not the fixture, is what refuses it.
      inclusiveValidityUpper:
        inclusiveValidityUpper > plan.responseDeadline
          ? plan.responseDeadline
          : inclusiveValidityUpper,
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
      ttlSlot,
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
  return { input, bytes, history, open, unit, plan, funding, parameters };
};
