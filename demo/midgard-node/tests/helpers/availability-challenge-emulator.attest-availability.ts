import { writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { availabilityScriptNames } from "./availability-challenge-emulator.availability-redeemer-script.js";
import {
  assertAvailabilityRefusal,
  confirmedRefusalCount,
} from "./availability-challenge-emulator.create-fixture.js";
import {
  AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
  type AvailabilityLayout,
  mintIndex,
} from "./availability-challenge-emulator.measure-availability-transaction.js";
import { type AvailabilityFixture } from "./availability-challenge-emulator.open-availability.js";

export type AttestedAvailability = Awaited<
  ReturnType<typeof attestAvailability>
>;

export const reportAvailabilityScenario = (
  name: string,
  fixture: AvailabilityFixture,
) => {
  const worst = (
    key: "signedBytes" | "memory" | "steps" | "referencedScriptBytes",
  ) =>
    fixture.measurements.reduce((left, right) =>
      left[key] >= right[key] ? left : right,
    );
  const summary = {
    scenario: name,
    transactionCount: fixture.measurements.length,
    largestTransaction: worst("signedBytes"),
    highestMemory: worst("memory"),
    highestCpu: worst("steps"),
    mostReferencedScriptBytes: worst("referencedScriptBytes"),
  };
  const bigintJson = (_key: string, value: unknown) =>
    typeof value === "bigint" ? value.toString() : value;
  console.info("availability mainnet fit", JSON.stringify(summary, bigintJson));
  const reportDirectory = process.env.MIDGARD_AVAILABILITY_FIT_REPORT_DIR;
  if (reportDirectory)
    writeFileSync(
      join(reportDirectory, `${name}.json`),
      JSON.stringify(
        {
          summary,
          // On-chain refusals confirmed while this fixture was live.
          onchainRefusalCount:
            confirmedRefusalCount - fixture.refusalsAtCreation,
          measurements: fixture.measurements,
        },
        bigintJson,
        2,
      ) + "\n",
    );
};

export type AvailabilityScriptName = keyof ReturnType<
  typeof availabilityScriptNames
>;

/** The SDK challenge builders' deployment view of the fixture. */
export const availabilityDeployment = (
  f: AvailabilityFixture,
): SDK.DaAvailabilityDeployment => {
  const names = [
    "availability-challenge spending",
    "availability-challenge minting",
    ...(["open", "settle", "close", "timeout"] as const).map(
      (arm) => `availability-challenge ${arm} withdrawal`,
    ),
    "state-queue spending",
    "state-queue minting",
    "state-queue unavailable-timeout withdrawal",
    "correction-lock spending",
    "da-bond-pool spending",
  ];
  return {
    contracts: f.contracts,
    hubOraclePolicyId: f.contracts.hubOracle.policyId,
    referenceScriptAuthPolicyId: f.authPolicy.policyId,
    parameters: f.parameters,
    referenceScripts: Object.fromEntries(
      names.map((name) => [name, f.reference(name)]),
    ),
    hubOracleRefInput: f.hubOracleRefInput,
  };
};

/**
 * Attests the fixture's block: init, threshold signatures, then Apply, which
 * references the pool and writes `Attested{commitment_hash}`. Returns the
 * attested queue node and the full commitment an Open needs.
 */
export const attestAvailability = async (
  f: AvailabilityFixture,
  options: { refuseCommitmentPreimageMismatch?: boolean } = {},
) => {
  const { lucid, contracts } = f;
  lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
  const init = await Effect.runPromise(
    SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      target: f.target,
      referenceScripts: f.daReferences,
      attestationOutputLovelace: AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
      rescueBeneficiary: await Effect.runPromise(
        SDK.addressDataFromBech32(f.responder.address),
      ),
      availabilityCommitment: f.commitment,
    }),
  );
  await f.submit("attestation init", init, true);
  const attestationUnit = SDK.daAttestationUnit(
    contracts.daAttestation,
    f.target.headerHash,
  );
  const getAttestation = async (): Promise<SDK.DaAttestationUtxo> => {
    const [utxo] = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      attestationUnit,
    );
    if (!utxo?.datum) throw new Error("Missing attestation");
    return { utxo, datum: Data.from(utxo.datum, SDK.DaAttestationDatum) };
  };
  const message = SDK.daAvailabilityAttestationMessage(f.commitment);
  const add = await Effect.runPromise(
    SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      attestation: await getAttestation(),
      witnesses: f.committeeKeys.map((key, signerIndex) => ({
        signerIndex,
        signatureHex: Buffer.from(key.sign(message).to_raw_bytes()).toString(
          "hex",
        ),
      })),
      referenceScripts: f.daReferences,
    }),
  );
  await f.submit("attestation threshold signatures", add, true);
  const threshold = await getAttestation();
  const applyConfig = {
    daParamsUtxo: f.daParamsUtxo,
    daParamsDatum: f.daParamsDatum,
    attestation: threshold,
    target: f.target,
    referenceScripts: f.daReferences,
    availabilityParameters: f.parameters,
    validityRange: {
      validFrom: BigInt(f.emulator.now()),
      validTo: BigInt(f.emulator.now() + 60_000),
    },
  };
  if (options.refuseCommitmentPreimageMismatch) {
    // The consumed attestation carries the committee-signed commitment. A
    // builder fed another commitment writes Attested{hash(other)}, which is
    // not the hash of the preimage the chain holds: Apply must refuse.
    const [first, ...rest] =
      threshold.datum.availability_commitment.tranche_descriptors;
    const substituted = await Effect.runPromise(
      SDK.incompleteApplyDaAttestationToStateQueueTxProgram(lucid, contracts, {
        ...applyConfig,
        attestation: {
          ...threshold,
          datum: {
            ...threshold.datum,
            availability_commitment: {
              ...threshold.datum.availability_commitment,
              tranche_descriptors: [
                { ...first!, chunk_commitment: "00".repeat(32) },
                ...rest,
              ],
            },
          },
        },
      }),
    );
    await assertAvailabilityRefusal(
      substituted.complete({ coinSelection: true, localUPLCEval: true }),
      { purpose: "mint", script: "da-attestation minting" },
      f.scriptNames,
    );
  }
  const apply = await Effect.runPromise(
    SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
      lucid,
      contracts,
      applyConfig,
    ),
  );
  await f.submit("attestation apply against the pooled bond", apply, true);
  const [queue] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    f.queueUnit,
  );
  if (!queue?.datum) throw new Error("Apply omitted the queue node");
  const commitmentHash = SDK.daAvailabilityCommitmentHash(f.commitment);
  const node = Data.castFrom(
    (await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(queue))).data,
    SDK.StateQueueNode,
  );
  expect(node.da_attestation).toEqual({
    Attested: { commitment_hash: commitmentHash },
  });
  return { queue, commitment: f.commitment, commitmentHash };
};

export const coordinate = (ctx: AvailabilityLayout, policy: string) =>
  Data.to(
    { Coordinate: { mint_redeemer_index: mintIndex(ctx, policy) } },
    SDK.DaAvailabilitySpendRedeemer,
  );
