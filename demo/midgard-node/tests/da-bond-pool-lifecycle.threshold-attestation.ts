import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  assertAvailabilityRefusal,
  AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
  type AvailabilityFixture,
  lastAvailabilityEvaluationFailure,
} from "./helpers/availability-challenge-emulator.js";

/**
 * Pooled DA bond lifecycle on the real pool, DA attestation and state-queue
 * validators (#689 acceptance criteria 1 and 2, pool half). Every negative is
 * paired with its honest control, is refused by the named script under the
 * named purpose and redeemer index, and its SDK pre-build refusal is asserted
 * separately; the test-only skip flags reach the on-chain check.
 */

export const LIFECYCLE_TIMEOUT_MS = 600_000;

/**
 * Fixture setup publishes every reference script, one emulator block each, so
 * the header ends this far ahead to keep Apply inside the attestation deadline.
 */
export const HEADER_END_TIME_LEAD_MS = 900_000;

/** The withdraw delay the preprod-testing blueprint compiles in. */
export const PREPROD_TESTING_WITHDRAW_DELAY_MS = BigInt(
  DEPLOYMENT_PROFILES["preprod-testing"].timing.da_bond_withdraw_delay_ms,
);

export const flip = <E>(program: Effect.Effect<unknown, E>): Promise<E> =>
  Effect.runPromise(Effect.flip(program));

export const sameOutRef = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

/** The ledger (sorted) position of `utxo` among the inputs of `txCbor`. */
const sortedInputIndex = (txCbor: string, utxo: UTxO): number => {
  const inputs = CML.Transaction.from_cbor_hex(txCbor).body().inputs();
  const found = Array.from({ length: inputs.len() }, (_, i) => ({
    txHash: inputs.get(i).transaction_id().to_hex(),
    outputIndex: Number(inputs.get(i).index()),
  }))
    .sort((a, b) =>
      a.txHash < b.txHash
        ? -1
        : a.txHash > b.txHash
          ? 1
          : a.outputIndex - b.outputIndex,
    )
    .findIndex(
      (input) =>
        input.txHash === utxo.txHash && input.outputIndex === utxo.outputIndex,
    );
  if (found < 0) throw new Error("The pool is not an input of the refused tx");
  return found;
};

/**
 * Asserts the refusal came from the pool's own spend of `pool`: purpose
 * `spend`, script = the pool policy, index = the pool's sorted input position.
 */
export const assertPoolSpendRefusal = async (
  f: AvailabilityFixture,
  attempt: Promise<unknown>,
  pool: UTxO,
) => {
  const refusal = await assertAvailabilityRefusal(
    attempt,
    { purpose: "spend", script: "da-bond-pool" },
    f.scriptNames,
  );
  expect(refusal.scriptHash).toBe(f.contracts.daBondPool.policyId);
  expect(refusal.index).toBe(
    sortedInputIndex(lastAvailabilityEvaluationFailure()!.tx, pool),
  );
  return refusal;
};

/** Pool datum, lovelace and the full asset set. */
export const poolView = (pool: UTxO) => ({
  datum: SDK.decodeDaBondPoolDatum(pool.datum!),
  lovelace: pool.assets.lovelace,
  units: Object.keys(pool.assets).sort(),
});

/** The owner-quorum spend config the SDK withdraw builders take. */
export const quorumConfig = async (f: AvailabilityFixture) => ({
  poolValidator: f.contracts.daBondPool,
  parameters: f.parameters,
  pool: { utxo: await f.getPool() },
  daParamsUtxo: f.daParamsUtxo,
  signerKeyHashes: f.daParamsDatum.owners,
  referenceScripts: {
    daBondPoolSpending: f.poolReferences.daBondPoolSpending,
  },
});

export const topUpProgram = async (
  f: AvailabilityFixture,
  amount: bigint,
  options: { skipMinimumPrecheck?: true } = {},
) =>
  SDK.buildTopUpDaBondPoolTxProgram(f.lucid, {
    poolValidator: f.contracts.daBondPool,
    parameters: f.parameters,
    pool: { utxo: await f.getPool() },
    amount,
    referenceScripts: {
      daBondPoolSpending: f.poolReferences.daBondPoolSpending,
    },
    ...options,
  });

/**
 * Lands the attestation init and the committee's threshold signatures, and
 * returns the threshold attestation Apply consumes (the first half of the
 * harness `attestAvailability`, stopping before Apply).
 */
export const thresholdAttestation = async (
  f: AvailabilityFixture,
): Promise<SDK.DaAttestationUtxo> => {
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
  const unit = SDK.daAttestationUnit(
    contracts.daAttestation,
    f.target.headerHash,
  );
  const current = async (): Promise<SDK.DaAttestationUtxo> => {
    const [utxo] = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      unit,
    );
    if (!utxo?.datum) throw new Error("Missing attestation");
    return { utxo, datum: Data.from(utxo.datum, SDK.DaAttestationDatum) };
  };
  const message = SDK.daAvailabilityAttestationMessage(f.commitment);
  const add = await Effect.runPromise(
    SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      attestation: await current(),
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
  return current();
};

/** The SDK Apply against the live pool (re-fetched on every build, H8). */
export const applyProgram = (
  f: AvailabilityFixture,
  attestation: SDK.DaAttestationUtxo,
  options: { skipPoolPrecheck?: true } = {},
) =>
  SDK.incompleteApplyDaAttestationToStateQueueTxProgram(f.lucid, f.contracts, {
    daParamsUtxo: f.daParamsUtxo,
    daParamsDatum: f.daParamsDatum,
    attestation,
    target: f.target,
    referenceScripts: f.daReferences,
    availabilityParameters: f.parameters,
    validityRange: {
      validFrom: BigInt(f.emulator.now()),
      validTo: BigInt(f.emulator.now() + 60_000),
    },
    ...options,
  });

/** The DA attestation mint (`ApplyToStateQueue`) is the refusing check. */
export const assertApplyRefusedByDaAttestationMint = async (
  f: AvailabilityFixture,
  attestation: SDK.DaAttestationUtxo,
) => {
  const tx = await Effect.runPromise(
    applyProgram(f, attestation, { skipPoolPrecheck: true }),
  );
  const refusal = await assertAvailabilityRefusal(
    tx.complete({ coinSelection: true, localUPLCEval: true }),
    // Apply mints under exactly one policy (the DAAT burn), so Mint[0].
    { purpose: "mint", script: "da-attestation minting", index: 0 },
    f.scriptNames,
  );
  expect(refusal.scriptHash).toBe(f.contracts.daAttestation.policyId);
};

/** Lands Apply and asserts the node is `Attested{commitment_hash}`. */
export const applyAndAssertAttested = async (
  f: AvailabilityFixture,
  attestation: SDK.DaAttestationUtxo,
) => {
  const poolBefore = await f.getPool();
  const apply = await Effect.runPromise(applyProgram(f, attestation));
  await f.submit("attestation apply against the pooled bond", apply, true);
  const [queue] = await f.lucid.utxosAtWithUnit(
    f.contracts.stateQueue.spendingScriptAddress,
    f.queueUnit,
  );
  if (!queue?.datum) throw new Error("Apply omitted the queue node");
  const node = Data.castFrom(
    (await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(queue))).data,
    SDK.StateQueueNode,
  );
  expect(node.da_attestation).toEqual({
    Attested: {
      commitment_hash: SDK.daAvailabilityCommitmentHash(f.commitment),
    },
  });
  // The pool is a reference input: Apply neither spends nor changes it.
  expect(sameOutRef(await f.getPool(), poolBefore)).toBe(true);
  return queue;
};
