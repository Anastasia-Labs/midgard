import { createHash } from "node:crypto";

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

export const WATCHER_PROVER_FUNDING_RESERVATION_PLAN =
  "midgard-watcher-production-prover-funding-reservation-plan-v1" as const;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const ASSET_UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const ACTION_KIND = /^[a-z][a-zA-Z0-9_.:-]{0,127}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type WatcherProverFundingReservationInput = Readonly<{
  outRef: string;
  role: "funding" | "collateral";
  lovelace: string;
  assets: readonly Readonly<{ unit: string; quantity: string }>[];
}>;

export type WatcherProverFundingReservationPlan = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_RESERVATION_PLAN;
  deploymentFingerprint: string;
  decisionDigest: string;
  policyDigest: string;
  reservationBasisDigest: string;
  fundingPaymentKeyHash: string;
  walletAddress: string;
  inputs: readonly WatcherProverFundingReservationInput[];
  fundingLovelace: string;
  collateralLovelace: string;
  assets: readonly Readonly<{ unit: string; quantity: string }>[];
  reservationId: string;
}>;

export type WatcherProverFundingReservationTransition = Readonly<{
  actionKind: string;
  transactionHash: string;
  transactionBodySha256: string;
  signedTransactionCborHex: string;
  /** Reserved wallet funding inputs; empty for an admitted protocol-funded tx. */
  consumedOutRefs: readonly string[];
  producedInputs: readonly WatcherProverFundingReservationInput[];
  transitionDigest: string;
}>;

export type WatcherProverFundingReservationRecord = Readonly<{
  reservationId: string;
  deploymentFingerprint: string;
  decisionDigest: string;
  policyDigest: string;
  reservationBasisDigest: string;
  revision: string;
  state: "active" | "released" | "conflict";
  activeInputs: readonly WatcherProverFundingReservationInput[];
  pendingTransition: WatcherProverFundingReservationTransition | null;
  lastConfirmedTransitionDigest: string | null;
  conflictCode: "unexpected_spend" | "reservation_collision" | null;
  recordDigest: string;
}>;

export type WatcherProverFundingReservationStore = Readonly<{
  readAll(): Promise<readonly unknown[]>;
  readConfirmedInput(input: {
    readonly reservationId: string;
    readonly outRef: string;
  }): Promise<unknown | null>;
  reserve(
    plan: WatcherProverFundingReservationPlan,
  ): Promise<"reserved" | "unchanged">;
  prepareTransition(input: {
    readonly plan: WatcherProverFundingReservationPlan;
    readonly expectedRevision: string;
    readonly actionKind: string;
    readonly signedTransactionCborHex: string;
    readonly transactionHash: string;
    readonly transactionBodySha256: string;
    readonly consumedOutRefs: readonly string[];
    readonly producedInputs: readonly WatcherProverFundingReservationInput[];
  }): Promise<WatcherProverFundingReservationRecord>;
  confirmTransition(input: {
    readonly plan: WatcherProverFundingReservationPlan;
    readonly expectedRevision: string;
    readonly transactionHash: string;
    readonly transitionDigest: string;
  }): Promise<WatcherProverFundingReservationRecord>;
  abandonPendingTransition(input: {
    readonly plan: WatcherProverFundingReservationPlan;
    readonly expectedRevision: string;
    readonly transitionDigest: string;
  }): Promise<WatcherProverFundingReservationRecord>;
  markConflict(input: {
    readonly plan: WatcherProverFundingReservationPlan;
    readonly expectedRevision: string;
    readonly code: "unexpected_spend" | "reservation_collision";
  }): Promise<WatcherProverFundingReservationRecord>;
  release(input: {
    readonly plan: WatcherProverFundingReservationPlan;
    readonly expectedRevision: string;
  }): Promise<WatcherProverFundingReservationRecord>;
}>;

const admittedPlans = new WeakSet<object>();

export const assertWatcherProverFundingReservationPlan = (
  plan: WatcherProverFundingReservationPlan,
): void => {
  if (!admittedPlans.has(plan)) {
    throw new Error("prover funding reservation plan is not admitted");
  }
};

const exactRecord = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} is not an exact plain object`);
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
  return record;
};

const parseReservationInput = (
  value: unknown,
  label: string,
): WatcherProverFundingReservationInput => {
  const record = exactRecord(
    value,
    ["outRef", "role", "lovelace", "assets"],
    label,
  );
  if (
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    (record.role !== "funding" && record.role !== "collateral") ||
    typeof record.lovelace !== "string" ||
    !NATURAL.test(record.lovelace) ||
    BigInt(record.lovelace) <= 0n ||
    !Array.isArray(record.assets)
  ) {
    throw new Error(`${label} is invalid`);
  }
  const assets = record.assets.map((asset, index) => {
    const parsed = exactRecord(
      asset,
      ["unit", "quantity"],
      `${label}.assets[${index.toString()}]`,
    );
    if (
      typeof parsed.unit !== "string" ||
      !ASSET_UNIT.test(parsed.unit) ||
      typeof parsed.quantity !== "string" ||
      !NATURAL.test(parsed.quantity) ||
      BigInt(parsed.quantity) <= 0n
    ) {
      throw new Error(`${label}.assets[${index.toString()}] is invalid`);
    }
    return Object.freeze({ unit: parsed.unit, quantity: parsed.quantity });
  });
  if (
    assets.some(
      (asset, index) =>
        index > 0 && assets[index - 1]!.unit.localeCompare(asset.unit) >= 0,
    ) ||
    (record.role === "collateral" && assets.length !== 0)
  ) {
    throw new Error(`${label} asset ordering is invalid`);
  }
  return Object.freeze({
    outRef: record.outRef,
    role: record.role,
    lovelace: record.lovelace,
    assets: Object.freeze(assets),
  });
};

const parseReservationInputs = (
  value: unknown,
  label: string,
): readonly WatcherProverFundingReservationInput[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  const inputs = value.map((entry, index) =>
    parseReservationInput(entry, `${label}[${index.toString()}]`),
  );
  if (
    inputs.some(
      (input, index) =>
        index > 0 && inputs[index - 1]!.outRef.localeCompare(input.outRef) >= 0,
    )
  ) {
    throw new Error(`${label} output references are not ordered and unique`);
  }
  return Object.freeze(inputs);
};

const parseTransition = (
  value: unknown,
  label: string,
): WatcherProverFundingReservationTransition => {
  const record = exactRecord(
    value,
    [
      "actionKind",
      "transactionHash",
      "transactionBodySha256",
      "signedTransactionCborHex",
      "consumedOutRefs",
      "producedInputs",
      "transitionDigest",
    ],
    label,
  );
  if (
    typeof record.actionKind !== "string" ||
    !ACTION_KIND.test(record.actionKind) ||
    typeof record.transactionHash !== "string" ||
    !HEX_32.test(record.transactionHash) ||
    typeof record.transactionBodySha256 !== "string" ||
    !HEX_32.test(record.transactionBodySha256) ||
    typeof record.signedTransactionCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(record.signedTransactionCborHex) ||
    typeof record.transitionDigest !== "string" ||
    !HEX_32.test(record.transitionDigest) ||
    !Array.isArray(record.consumedOutRefs)
  ) {
    throw new Error(`${label} is invalid`);
  }
  const signed = CML.Transaction.from_cbor_hex(record.signedTransactionCborHex);
  if (
    signed.to_cbor_hex() !== record.signedTransactionCborHex ||
    CML.hash_transaction(signed.body()).to_hex() !== record.transactionHash ||
    createHash("sha256")
      .update(Buffer.from(signed.body().to_cbor_hex(), "hex"))
      .digest("hex") !== record.transactionBodySha256
  )
    throw new Error(`${label} differs from its exact signed transaction wire`);
  const consumedOutRefs = record.consumedOutRefs.map((outRef, index) => {
    if (typeof outRef !== "string" || !OUT_REF.test(outRef)) {
      throw new Error(
        `${label}.consumedOutRefs[${index.toString()}] is invalid`,
      );
    }
    return outRef;
  });
  if (
    consumedOutRefs.some(
      (outRef, index) =>
        index > 0 && consumedOutRefs[index - 1]!.localeCompare(outRef) >= 0,
    )
  ) {
    throw new Error(`${label}.consumedOutRefs are not ordered and unique`);
  }
  const producedInputs = parseReservationInputs(
    record.producedInputs,
    `${label}.producedInputs`,
  );
  const transitionInput = Object.freeze({
    actionKind: record.actionKind,
    transactionHash: record.transactionHash,
    transactionBodySha256: record.transactionBodySha256,
    signedTransactionCborHex: record.signedTransactionCborHex,
    consumedOutRefs: Object.freeze(consumedOutRefs),
    producedInputs,
  });
  if (
    computeDeploymentManifestJsonDigest(transitionInput) !==
    record.transitionDigest
  ) {
    throw new Error(`${label} digest mismatch`);
  }
  return Object.freeze({
    ...transitionInput,
    transitionDigest: record.transitionDigest,
  });
};

export const parseWatcherProverFundingReservationRecord = (
  value: unknown,
): WatcherProverFundingReservationRecord => {
  const record = exactRecord(
    value,
    [
      "reservationId",
      "deploymentFingerprint",
      "decisionDigest",
      "policyDigest",
      "reservationBasisDigest",
      "revision",
      "state",
      "activeInputs",
      "pendingTransition",
      "lastConfirmedTransitionDigest",
      "conflictCode",
      "recordDigest",
    ],
    "prover funding reservation record",
  );
  if (
    typeof record.reservationId !== "string" ||
    !HEX_32.test(record.reservationId) ||
    typeof record.deploymentFingerprint !== "string" ||
    !HEX_32.test(record.deploymentFingerprint) ||
    typeof record.decisionDigest !== "string" ||
    !HEX_32.test(record.decisionDigest) ||
    typeof record.policyDigest !== "string" ||
    !HEX_32.test(record.policyDigest) ||
    typeof record.reservationBasisDigest !== "string" ||
    !HEX_32.test(record.reservationBasisDigest) ||
    typeof record.revision !== "string" ||
    !NATURAL.test(record.revision) ||
    !["active", "released", "conflict"].includes(record.state as string) ||
    (record.lastConfirmedTransitionDigest !== null &&
      (typeof record.lastConfirmedTransitionDigest !== "string" ||
        !HEX_32.test(record.lastConfirmedTransitionDigest))) ||
    (record.conflictCode !== null &&
      record.conflictCode !== "unexpected_spend" &&
      record.conflictCode !== "reservation_collision") ||
    typeof record.recordDigest !== "string" ||
    !HEX_32.test(record.recordDigest)
  ) {
    throw new Error("prover funding reservation record is invalid");
  }
  const activeInputs = parseReservationInputs(
    record.activeInputs,
    "prover funding reservation record.activeInputs",
  );
  const pendingTransition =
    record.pendingTransition === null
      ? null
      : parseTransition(
          record.pendingTransition,
          "prover funding reservation record.pendingTransition",
        );
  if (
    (record.state === "active" && record.conflictCode !== null) ||
    (record.state === "released" &&
      (activeInputs.length !== 0 ||
        pendingTransition !== null ||
        record.conflictCode !== null)) ||
    (record.state === "conflict" &&
      (record.conflictCode === null || pendingTransition !== null))
  ) {
    throw new Error("prover funding reservation record state is inconsistent");
  }
  const recordInput = Object.freeze({
    reservationId: record.reservationId,
    deploymentFingerprint: record.deploymentFingerprint,
    decisionDigest: record.decisionDigest,
    policyDigest: record.policyDigest,
    reservationBasisDigest: record.reservationBasisDigest,
    revision: record.revision,
    state: record.state as WatcherProverFundingReservationRecord["state"],
    activeInputs,
    pendingTransition,
    lastConfirmedTransitionDigest: record.lastConfirmedTransitionDigest as
      | string
      | null,
    conflictCode:
      record.conflictCode as WatcherProverFundingReservationRecord["conflictCode"],
  });
  if (
    computeDeploymentManifestJsonDigest(recordInput) !== record.recordDigest
  ) {
    throw new Error("prover funding reservation record digest mismatch");
  }
  return Object.freeze({ ...recordInput, recordDigest: record.recordDigest });
};

export const makeWatcherProverFundingReservationRecord = (input: {
  readonly plan: WatcherProverFundingReservationPlan;
}): WatcherProverFundingReservationRecord => {
  assertWatcherProverFundingReservationPlan(input.plan);
  const recordInput = Object.freeze({
    reservationId: input.plan.reservationId,
    deploymentFingerprint: input.plan.deploymentFingerprint,
    decisionDigest: input.plan.decisionDigest,
    policyDigest: input.plan.policyDigest,
    reservationBasisDigest: input.plan.reservationBasisDigest,
    revision: "0",
    state: "active" as const,
    activeInputs: input.plan.inputs,
    pendingTransition: null,
    lastConfirmedTransitionDigest: null,
    conflictCode: null,
  });
  return Object.freeze({
    ...recordInput,
    recordDigest: computeDeploymentManifestJsonDigest(recordInput),
  });
};

type Candidate = Readonly<{
  outRef: string;
  lovelace: bigint;
  assets: ReadonlyMap<string, bigint>;
}>;

const outRef = (utxo: UTxO): string => {
  if (
    !HEX_32.test(utxo.txHash) ||
    !Number.isSafeInteger(utxo.outputIndex) ||
    utxo.outputIndex < 0
  ) {
    throw new Error("prover wallet returned a malformed output reference");
  }
  return `${utxo.txHash}#${utxo.outputIndex.toString()}`;
};

const compareOutRef = (left: Candidate, right: Candidate): number =>
  left.outRef.localeCompare(right.outRef);

const compareLargestFirst = (left: Candidate, right: Candidate): number =>
  left.lovelace === right.lovelace
    ? compareOutRef(left, right)
    : left.lovelace > right.lovelace
      ? -1
      : 1;

const parseCandidate = (
  utxo: UTxO,
  expectedAddress: string,
): Candidate | null => {
  const reference = outRef(utxo);
  if (
    utxo.address !== expectedAddress ||
    (utxo.datum !== undefined && utxo.datum !== null) ||
    (utxo.datumHash !== undefined && utxo.datumHash !== null) ||
    (utxo.scriptRef !== undefined && utxo.scriptRef !== null)
  ) {
    return null;
  }
  const assets = new Map<string, bigint>();
  for (const [unit, quantity] of Object.entries(utxo.assets)) {
    if (
      (unit !== "lovelace" && !ASSET_UNIT.test(unit)) ||
      typeof quantity !== "bigint" ||
      quantity <= 0n
    ) {
      throw new Error(`prover wallet output ${reference} has invalid assets`);
    }
    assets.set(unit, quantity);
  }
  const lovelace = assets.get("lovelace");
  if (lovelace === undefined) {
    throw new Error(`prover wallet output ${reference} omitted lovelace`);
  }
  assets.delete("lovelace");
  return Object.freeze({ outRef: reference, lovelace, assets });
};

const assetEntries = (
  assets: ReadonlyMap<string, bigint>,
): readonly Readonly<{ unit: string; quantity: string }>[] =>
  Object.freeze(
    [...assets.entries()]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([unit, quantity]) =>
        Object.freeze({ unit, quantity: quantity.toString() }),
      ),
  );

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
    throw new Error("prover wallet has insufficient plain-Ada collateral");
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
}): WatcherProverFundingReservationPlan => {
  const identity = reservationIdentity(input);
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
  const maximumCollateralInputs = Number(
    input.calculation.maximumCollateralInputs,
  );
  if (
    !Number.isSafeInteger(maximumCollateralInputs) ||
    maximumCollateralInputs < 1
  ) {
    throw new Error("prover funding maximum collateral inputs is invalid");
  }
  const collateralRequired = [
    input.calculation.collateralFloorLovelace,
    input.calculation.maximumCollateralLovelace,
    input.calculation.maximumSlashCollateralLovelace,
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
    throw new Error(
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
  if (record.state !== "active")
    throw new Error("restored prover funding reservation is not active");
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
