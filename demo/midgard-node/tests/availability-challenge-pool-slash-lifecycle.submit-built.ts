import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  attestAvailability,
  AVAILABILITY_EMULATOR_PARAMETERS,
  type AvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";

// ---------------------------------------------------------------------------
// File-local helpers.
// ---------------------------------------------------------------------------

/** H12: the ledger and budget bounds every submitted transaction meets. */
export const MAX_SIGNED_BYTES = 15_872;

export const MAX_MEMORY = 13_200_000n;

export const MAX_STEPS = 8_000_000_000n;

export type Measurement = {
  readonly signedBytes: number;
  readonly memory: bigint;
  readonly steps: bigint;
  readonly fee: bigint;
  readonly outputs: number;
};

const measure = (cbor: string): Measurement => {
  const tx = CML.Transaction.from_cbor_hex(cbor);
  const redeemers = tx.witness_set().redeemers()?.to_flat_format();
  let memory = 0n;
  let steps = 0n;
  for (let i = 0; i < (redeemers?.len() ?? 0); i++) {
    memory += redeemers!.get(i).ex_units().mem();
    steps += redeemers!.get(i).ex_units().steps();
  }
  return {
    signedBytes: cbor.length / 2,
    memory,
    steps,
    fee: tx.body().fee(),
    outputs: tx.body().outputs().len(),
  };
};

/**
 * Signs and submits an SDK-built availability transaction after checking the
 * H12 bounds and the G9/H1 collateral rule: one to three plain-ADA coins
 * covering 150% of the exact fee.
 */
export const submitBuilt = async (
  f: AvailabilityFixture,
  built: SDK.BuiltDaAvailabilityTransaction,
) => {
  const signed = await built.tx.sign.withWallet().complete();
  const measurement = measure(signed.toCBOR());
  expect(measurement.signedBytes).toBeLessThanOrEqual(MAX_SIGNED_BYTES);
  expect(measurement.memory).toBeGreaterThan(0n);
  expect(measurement.memory).toBeLessThanOrEqual(MAX_MEMORY);
  expect(measurement.steps).toBeLessThanOrEqual(MAX_STEPS);
  expect(measurement.fee).toBe(built.feeLovelace);
  expect(signed.toHash()).toBe(built.txId);
  expect(built.collateralOutRefs.length).toBeGreaterThanOrEqual(1);
  expect(built.collateralOutRefs.length).toBeLessThanOrEqual(3);
  expect(
    built.collateralOutRefs.reduce((t, u) => t + u.assets.lovelace, 0n),
  ).toBeGreaterThanOrEqual((built.feeLovelace * 150n + 99n) / 100n);
  for (const c of built.collateralOutRefs)
    expect(Object.keys(c.assets)).toEqual(["lovelace"]);
  const id = await signed.submit();
  f.emulator.awaitBlock(1);
  const outputs = await f.lucid.utxosByOutRef(
    built.expectedOutputs.map((_, outputIndex) => ({
      txHash: id,
      outputIndex,
    })),
  );
  return { outputs, measurement };
};

export const resources = async (
  f: AvailabilityFixture,
  feeLovelace: bigint,
  /** A publication's range must close by the challenge's response deadline. */
  responseDeadline?: bigint,
): Promise<SDK.DaAvailabilityTransactionResources> => {
  const collateralInputs = await f.collateralInputs();
  const validFrom = BigInt(f.emulator.now());
  const validTo =
    responseDeadline !== undefined &&
    responseDeadline + 1n < validFrom + 60_000n
      ? responseDeadline + 1n
      : validFrom + 60_000n;
  return { collateralInputs, feeLovelace, validFrom, validTo };
};

/** The timeout derives its own exact fee: resources without one. */
const timeoutResources = async (f: AvailabilityFixture) => {
  const { feeLovelace: _fee, ...rest } = await resources(f, 1n);
  return rest;
};

export const OPEN_FUNDING_LOVELACE =
  parameters.challenger_bond_lovelace +
  parameters.challenge_record_lovelace +
  parameters.max_open_fee_lovelace;

/** Selects the challenger's wallet and funds one exact Open input. */
const fundChallenger = async (f: AvailabilityFixture) => {
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const funding = await f.submit(
    "prepare challenger funding",
    f.lucid.newTx().pay.ToAddress(f.challenger.address, {
      lovelace: OPEN_FUNDING_LOVELACE,
    }),
    true,
  );
  return funding.find((u) => u.assets.lovelace === OPEN_FUNDING_LOVELACE)!;
};

export const snapshot = (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
) => SDK.fetchDaAvailabilityChallengeSnapshot(f.lucid, d, f.target.headerHash);

export const recordOf = (s: SDK.DaAvailabilityChallengeSnapshot) => {
  if (!s.recordDatum) throw new Error("Expected a challenge record");
  return s.recordDatum;
};

export const nodeOf = (queue: UTxO) =>
  Data.castFrom(
    Effect.runSync(SDK.getLinkedListNodeViewFromUTxO(queue)).data,
    SDK.StateQueueNode,
  );

/** Attests the fixture block and opens a challenge with the SDK builder. */
export const attestAndOpen = async (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
) => {
  const attested = await attestAvailability(f);
  const challengerFunding = await fundChallenger(f);
  await submitBuilt(
    f,
    await Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, {
        ...(await resources(f, parameters.max_open_fee_lovelace)),
        commitment: attested.commitment,
        queue: attested.queue,
        challengerFunding,
        challenger: f.challengerKey,
        daChallengeWindowMs: f.timing.daChallengeWindowMs,
      }),
    ),
  );
  return snapshot(f, d);
};

/** Lets every tranche expire unanswered and settles it: `has_timed_out`. */
export const expireAndSettle = async (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
  s: SDK.DaAvailabilityChallengeSnapshot,
) => {
  f.advanceToMs(recordOf(s).response_deadline + 1_000n);
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const { outputs } = await submitBuilt(
    f,
    await Effect.runPromise(
      SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
        ...(await resources(f, parameters.max_settlement_fee_lovelace)),
        record: s.record!,
        terminal: s.terminal!,
        thread: s.tranches[0]!.utxo,
      }),
    ),
  );
  return outputs[0]!;
};

/** The production timeout builder's parameters for the head-only fixture. */
export const timeoutParams = async (
  f: AvailabilityFixture,
  s: SDK.DaAvailabilityChallengeSnapshot,
  terminal: UTxO,
  pool: UTxO,
): Promise<SDK.TimeoutDaAvailabilityChallengeParams> => ({
  ...(await timeoutResources(f)),
  record: s.record!,
  terminal,
  pool,
  queue: s.queue!.utxo,
  confirmedState: s.confirmedState.utxo,
  correctionLock: s.correctionLock,
  headerHash: f.target.headerHash,
  challengeAssetName: recordOf(s).challenge_asset_name,
  rentRefundAddress: f.responder.address,
});

/** The SDK error a program fails with (never a success). */
export const refusalOf = async <A>(
  program: Effect.Effect<A, SDK.DaAvailabilityTransactionError>,
): Promise<SDK.DaAvailabilityTransactionError> => {
  const result = await Effect.runPromise(Effect.either(program));
  if (result._tag === "Right")
    throw new Error("Expected the SDK builder to refuse, but it built");
  return result.left;
};

export const backingOf = (pool: UTxO) =>
  SDK.daBondPoolBacking({ lovelace: pool.assets.lovelace, parameters });

/** The min-UTxO of a lone plain-ADA output to `address` (stabilised). */
export const plainOutputMinAda = (address: string) => {
  let lovelace = 0n;
  for (let attempt = 0; attempt < 4; attempt++) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(lovelace),
        undefined,
        undefined,
      ),
      AVAILABILITY_EMULATOR_PARAMETERS.coinsPerUtxoByte,
    );
    if (required <= lovelace) return lovelace;
    lovelace = required;
  }
  throw new Error("min-UTxO did not stabilise");
};

// --- Ledger-layout helpers for the hand-built mirrors ----------------------

export type Layout = {
  readonly inputs: readonly UTxO[];
  readonly references: readonly UTxO[];
  readonly policies: readonly string[];
};

const compareOutRef = (a: UTxO, b: UTxO) =>
  a.txHash < b.txHash
    ? -1
    : a.txHash > b.txHash
      ? 1
      : a.outputIndex - b.outputIndex;

export const position = (utxos: readonly UTxO[], utxo: UTxO) => {
  const found = [...utxos]
    .sort(compareOutRef)
    .findIndex(
      (u) => u.txHash === utxo.txHash && u.outputIndex === utxo.outputIndex,
    );
  if (found < 0) throw new Error("Missing authored input");
  return BigInt(found);
};

export const inputIndex = (layout: Layout, utxo: UTxO) =>
  position(layout.inputs, utxo);

export const refIndex = (layout: Layout, utxo: UTxO) =>
  position(layout.references, utxo);

export const inline = (value: string) => ({ kind: "inline" as const, value });

const isScriptAddress = (address: string) =>
  CML.Address.from_bech32(address).payment_cred()?.as_script() !== undefined;

/** Redeemers sort spends first, then mints by policy. */
const scriptSpendCount = (layout: Layout) =>
  layout.inputs.filter((u) => isScriptAddress(u.address)).length;

export const mintIndex = (layout: Layout, policy: string) =>
  BigInt(
    scriptSpendCount(layout) + [...layout.policies].sort().indexOf(policy),
  );

export const unavailableTimeoutYield = (f: AvailabilityFixture) =>
  SDK.scriptRewardAddress(
    "Preprod",
    f.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
  );

export const timeoutYield = (f: AvailabilityFixture) =>
  SDK.scriptRewardAddress(
    "Preprod",
    f.contracts.availabilityChallenge.yields.timeout.withdrawalScript,
  );

/**
 * The ledger index of the timeout yield's withdrawal redeemer in the mirror:
 * withdrawals sort by reward-account bytes, and the yield hashes depend on the
 * fixture's reference-script auth policy.
 */
export const timeoutYieldWithdrawIndex = (f: AvailabilityFixture) =>
  [unavailableTimeoutYield(f), timeoutYield(f)]
    .map((address) => CML.Address.from_bech32(address).to_hex())
    .sort()
    .indexOf(CML.Address.from_bech32(timeoutYield(f)).to_hex());
