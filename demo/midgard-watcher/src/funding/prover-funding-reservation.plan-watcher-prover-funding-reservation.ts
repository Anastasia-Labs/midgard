import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherRuntimeProverFundingCalculation,
  type WatcherRuntimeProverFundingCalculation,
} from "./prover-funding-calculation.js";
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
  WatcherProverFundingUnavailableError,
} from "./prover-funding-reservation.watcher-prover-funding-reservation-store.js";

const selectCollateral = (input: {
  readonly candidates: readonly Candidate[];
  readonly required: bigint;
  readonly maximumInputs: number;
}): readonly Candidate[] => {
  if (input.required === 0n) return Object.freeze([]);
  const pureAda = input.candidates.filter(
    (candidate) => candidate.assets.size === 0,
  );
  const one = pureAda
    .filter((candidate) => candidate.lovelace >= input.required)
    .sort((left, right) =>
      left.lovelace === right.lovelace
        ? compareOutRef(left, right)
        : left.lovelace < right.lovelace
          ? -1
          : 1,
    )[0];
  if (one !== undefined) return Object.freeze([one]);
  const selected: Candidate[] = [];
  let total = 0n;
  for (const candidate of pureAda.sort(compareLargestFirst)) {
    if (selected.length === input.maximumInputs) break;
    selected.push(candidate);
    total += candidate.lovelace;
    if (total >= input.required) break;
  }
  if (total < input.required) {
    throw new WatcherProverFundingUnavailableError(
      "prover wallet has insufficient plain-Ada collateral",
    );
  }
  return Object.freeze(selected.sort(compareOutRef));
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
 * Deterministically plans disjoint funding and pure-Ada collateral inputs.
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
  const seen = new Set<string>();
  const candidates: Candidate[] = [];
  for (const utxo of input.utxos) {
    const candidate = parseCandidate(utxo, input.walletAddress);
    if (candidate === null) continue;
    if (seen.has(candidate.outRef)) {
      throw new Error("prover wallet returned a duplicate output reference");
    }
    seen.add(candidate.outRef);
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
  const collateral = selectCollateral({
    candidates,
    required: collateralRequired,
    maximumInputs: maximumCollateralInputs,
  });
  const collateralOutRefs = new Set(
    collateral.map((candidate) => candidate.outRef),
  );
  const funding = candidates
    .filter((candidate) => !collateralOutRefs.has(candidate.outRef))
    .sort(compareOutRef);
  if (funding.length === 0)
    throw new WatcherProverFundingUnavailableError(
      "prover wallet has no available funding inputs after collateral reservation",
    );
  const fundingAssets = new Map<string, bigint>();
  for (const candidate of funding)
    for (const [unit, quantity] of candidate.assets)
      fundingAssets.set(unit, (fundingAssets.get(unit) ?? 0n) + quantity);
  const reservationInput = Object.freeze({
    ...identity,
    inputs: Object.freeze(
      [
        ...funding.map((candidate) =>
          Object.freeze({
            outRef: candidate.outRef,
            role: "funding" as const,
            lovelace: candidate.lovelace.toString(),
            assets: assetEntries(candidate.assets),
          }),
        ),
        ...collateral.map((candidate) =>
          Object.freeze({
            outRef: candidate.outRef,
            role: "collateral" as const,
            lovelace: candidate.lovelace.toString(),
            assets: assetEntries(candidate.assets),
          }),
        ),
      ].sort((left, right) => left.outRef.localeCompare(right.outRef)),
    ),
    fundingLovelace: funding
      .reduce((total, candidate) => total + candidate.lovelace, 0n)
      .toString(),
    collateralLovelace: collateral
      .reduce((total, candidate) => total + candidate.lovelace, 0n)
      .toString(),
    assets: assetEntries(fundingAssets),
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
  const fundingAssets = new Map<string, bigint>();
  let fundingLovelace = 0n;
  let collateralLovelace = 0n;
  for (const entry of record.activeInputs) {
    if (entry.role === "collateral") {
      collateralLovelace += BigInt(entry.lovelace);
    } else {
      fundingLovelace += BigInt(entry.lovelace);
      for (const { unit, quantity } of entry.assets)
        fundingAssets.set(
          unit,
          (fundingAssets.get(unit) ?? 0n) + BigInt(quantity),
        );
    }
  }
  const plan = Object.freeze({
    ...identity,
    inputs: record.activeInputs,
    fundingLovelace: fundingLovelace.toString(),
    collateralLovelace: collateralLovelace.toString(),
    assets: assetEntries(fundingAssets),
    reservationId,
  });
  admittedPlans.add(plan);
  return plan;
};
