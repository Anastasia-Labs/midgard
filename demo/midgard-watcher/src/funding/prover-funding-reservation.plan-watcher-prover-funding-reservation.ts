import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { isPlainFundingOutput } from "@al-ft/midgard-fault-proofs";
import { CML, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.parse-trust-roots.js";
import { assertVerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.verify-watcher-user-event-script-binding.js";
import {
  assertWatcherRuntimeProverFundingCalculation,
  type WatcherRuntimeProverFundingCalculation,
} from "./prover-funding-calculation.aggregate-watcher-prover-funding-sweep.js";
import {
  assetEntries,
  type Candidate,
  compareLargestFirst,
  compareOutRef,
  parseCandidate,
  parseWatcherProverFundingReservationRecord,
} from "./prover-funding-reservation.parse-watcher-prover-funding-reservation-record.js";
import {
  admittedPlans,
  HEX_28,
  HEX_32,
  WATCHER_PROVER_FUNDING_RESERVATION_PLAN,
  type WatcherProverFundingReservationPlan,
  WatcherProverFundingShortfallError,
} from "./prover-funding-reservation.watcher-prover-funding-reservation-store.js";

/**
 * Throws unless at most `maximumInputs` of the plain-Ada `candidates`, the
 * largest, together hold `required`: the most collateral one transaction may
 * post from the pool.
 */
const assertCollateralCoverage = (input: {
  readonly candidates: readonly Candidate[];
  readonly required: bigint;
  readonly maximumInputs: number;
}): void => {
  const coverage = input.candidates
    .filter((candidate) => candidate.assets.size === 0)
    .sort(compareLargestFirst)
    .slice(0, input.maximumInputs)
    .reduce((total, candidate) => total + candidate.lovelace, 0n);
  if (coverage < input.required) {
    throw new WatcherProverFundingShortfallError(
      `prover wallet has insufficient plain-Ada collateral: required=${input.required.toString()} available=${coverage.toString()}`,
    );
  }
};

type ReservationIdentityInput = Readonly<{
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  calculation: WatcherRuntimeProverFundingCalculation;
  decisionDigest: string;
  walletAddress: string;
}>;

const reservationIdentity = (input: ReservationIdentityInput) => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherRuntimeProverFundingCalculation(input.calculation);
  if (
    input.calculation.deploymentFingerprint !==
    input.deploymentIdentity.manifestId
  ) {
    throw new Error("prover funding reservation deployment mismatch");
  }
  if (!HEX_32.test(input.decisionDigest)) {
    throw new Error("prover funding reservation decision digest is invalid");
  }
  const rawAddress = CML.Address.from_bech32(
    input.walletAddress,
  ).to_raw_bytes();
  if (rawAddress.length !== 29 || rawAddress[0]! >> 4 !== 6) {
    throw new Error(
      "prover funding reservation requires an enterprise key address",
    );
  }
  const paymentCredential = getAddressDetails(
    input.walletAddress,
  ).paymentCredential;
  if (
    paymentCredential?.type !== "Key" ||
    !HEX_28.test(paymentCredential.hash) ||
    paymentCredential.hash !== input.calculation.fundingPaymentKeyHash
  ) {
    throw new Error("prover wallet differs from runtime funding key");
  }
  return Object.freeze({
    schemaVersion: WATCHER_PROVER_FUNDING_RESERVATION_PLAN,
    deploymentFingerprint: input.deploymentIdentity.manifestId,
    decisionDigest: input.decisionDigest,
    policyDigest: input.calculation.policyDigest,
    reservationBasisDigest: input.calculation.reservationBasisDigest,
    fundingPaymentKeyHash: input.calculation.fundingPaymentKeyHash,
    walletAddress: input.walletAddress,
  });
};

/**
 * Deterministically plans one pool of reserved coins: every plain-Ada coin of
 * the wallet, any of which a transaction may spend, post as collateral, or
 * both. Its plain-Ada coins must cover the largest collateral any action
 * posts; the same coins also fund the action, so the two are not added.
 * The plan is not a live reservation: the durable coordinator must atomically
 * persist it and reauthenticate every out-ref before minting an actuation
 * permit for a runner.
 */
export const planWatcherProverFundingReservation = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly calculation: WatcherRuntimeProverFundingCalculation;
  readonly decisionDigest: string;
  readonly walletAddress: string;
  readonly utxos: readonly UTxO[];
  readonly selectionCalculation?: WatcherRuntimeProverFundingCalculation;
  /** Superseded attempts' inputs, reserved even when they hold a token: a
   * replacement must spend one. */
  readonly supersededInputOutRefs?: readonly string[];
}): WatcherProverFundingReservationPlan => {
  const identity = reservationIdentity(input);
  const selection = input.selectionCalculation ?? input.calculation;
  assertWatcherRuntimeProverFundingCalculation(selection);
  if (
    selection.deploymentFingerprint !== identity.deploymentFingerprint ||
    selection.fundingPaymentKeyHash !== identity.fundingPaymentKeyHash ||
    selection.economicsPolicyDigest !== input.calculation.economicsPolicyDigest
  )
    throw new Error("prover funding selection changed reservation authority");
  const superseded = new Set(input.supersededInputOutRefs ?? []);
  const seen = new Set<string>();
  const candidates: Candidate[] = [];
  for (const utxo of input.utxos) {
    const candidate = parseCandidate(utxo, input.walletAddress);
    if (candidate === null) continue;
    if (seen.has(candidate.outRef)) {
      throw new Error("prover wallet returned a duplicate output reference");
    }
    seen.add(candidate.outRef);
    // Only plain-ADA coins fund: a coin holding a token is never spent, save
    // a superseded attempt's input, which its replacement must spend.
    if (!isPlainFundingOutput(utxo) && !superseded.has(candidate.outRef))
      continue;
    candidates.push(candidate);
  }
  const maximumCollateralInputs = Number(selection.maximumCollateralInputs);
  if (
    !Number.isSafeInteger(maximumCollateralInputs) ||
    maximumCollateralInputs < 1
  ) {
    throw new Error("prover funding maximum collateral inputs is invalid");
  }
  const collateralRequired = [
    selection.collateralFloorLovelace,
    selection.maximumCollateralLovelace,
    selection.maximumSlashCollateralLovelace,
  ].reduce((maximum, value) => {
    const required = BigInt(value);
    return required > maximum ? required : maximum;
  }, 0n);
  assertCollateralCoverage({
    candidates,
    required: collateralRequired,
    maximumInputs: maximumCollateralInputs,
  });
  const pool = [...candidates].sort(compareOutRef);
  if (pool.length === 0)
    throw new WatcherProverFundingShortfallError(
      "prover wallet has no available funding inputs",
    );
  const poolAssets = new Map<string, bigint>();
  for (const candidate of pool)
    for (const [unit, quantity] of candidate.assets)
      poolAssets.set(unit, (poolAssets.get(unit) ?? 0n) + quantity);
  const reservationInput = Object.freeze({
    ...identity,
    inputs: Object.freeze(
      pool
        .map((candidate) =>
          Object.freeze({
            outRef: candidate.outRef,
            lovelace: candidate.lovelace.toString(),
            assets: assetEntries(candidate.assets),
          }),
        )
        .sort((left, right) => left.outRef.localeCompare(right.outRef)),
    ),
    fundingLovelace: pool
      .reduce((total, candidate) => total + candidate.lovelace, 0n)
      .toString(),
    assets: assetEntries(poolAssets),
  });
  const plan = Object.freeze({
    ...reservationInput,
    // This identity survives confirmed input rotation. The store retains and
    // validates the exact active leases; fresh wallet topups cannot change it.
    reservationId: computeDeploymentManifestJsonDigest(identity),
  });
  admittedPlans.add(plan);
  return plan;
};

/**
 * Plans an objective's first reservation. A validation dispute's holds its
 * deployment-bound VerifySource custody, the bond and the case floor, and
 * the reserve for a move our wallet pays; later reservations spend from it
 * and never re-require the bond.
 */
export const planFirstWatcherProverFundingReservation = (
  input: Parameters<typeof planWatcherProverFundingReservation>[0],
): WatcherProverFundingReservationPlan => {
  const plan = planWatcherProverFundingReservation(input);
  const required = input.calculation.validationDisputeCustodyLovelace;
  if (required !== undefined && BigInt(plan.fundingLovelace) < BigInt(required))
    throw new WatcherProverFundingShortfallError(
      `prover wallet cannot reserve the validation dispute bond, case floor and wallet-move reserve: required=${required} available=${plan.fundingLovelace}`,
    );
  return plan;
};

/** Reattaches exact persisted leases, including pending consumption, without coin selection. */
export const restoreWatcherProverFundingReservationPlan = (
  input: ReservationIdentityInput &
    Readonly<{
      record: unknown;
    }>,
): WatcherProverFundingReservationPlan => {
  const identity = reservationIdentity(input);
  const record = parseWatcherProverFundingReservationRecord(input.record);
  const reservationId = computeDeploymentManifestJsonDigest(identity);
  if (
    record.reservationId !== reservationId ||
    record.deploymentFingerprint !== identity.deploymentFingerprint ||
    record.decisionDigest !== identity.decisionDigest ||
    record.policyDigest !== identity.policyDigest ||
    record.reservationBasisDigest !== identity.reservationBasisDigest
  )
    throw new Error("restored prover funding reservation identity mismatch");
  if (record.state === "conflict")
    throw new Error("restored prover funding reservation is conflicted");
  const poolAssets = new Map<string, bigint>();
  let fundingLovelace = 0n;
  for (const entry of record.activeInputs) {
    fundingLovelace += BigInt(entry.lovelace);
    for (const { unit, quantity } of entry.assets)
      poolAssets.set(unit, (poolAssets.get(unit) ?? 0n) + BigInt(quantity));
  }
  const plan = Object.freeze({
    ...identity,
    inputs: record.activeInputs,
    fundingLovelace: fundingLovelace.toString(),
    assets: assetEntries(poolAssets),
    reservationId,
  });
  admittedPlans.add(plan);
  return plan;
};
