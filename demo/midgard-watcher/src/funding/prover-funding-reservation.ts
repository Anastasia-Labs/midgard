import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherProverFundingCalculation,
  type WatcherProverFundingCalculation,
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
  profileDigest: string;
  calculationDigest: string;
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
  consumedOutRefs: readonly string[];
  producedInputs: readonly WatcherProverFundingReservationInput[];
  transitionDigest: string;
}>;

export type WatcherProverFundingReservationRecord = Readonly<{
  reservationId: string;
  deploymentFingerprint: string;
  decisionDigest: string;
  profileDigest: string;
  calculationDigest: string;
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
  readConfirmedActionOutput(input: {
    readonly reservationId: string;
    readonly sourceActionKind: string;
    readonly sourceOutputIndex: number;
  }): Promise<unknown>;
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
    typeof record.transitionDigest !== "string" ||
    !HEX_32.test(record.transitionDigest) ||
    !Array.isArray(record.consumedOutRefs)
  ) {
    throw new Error(`${label} is invalid`);
  }
  const consumedOutRefs = record.consumedOutRefs.map((outRef, index) => {
    if (typeof outRef !== "string" || !OUT_REF.test(outRef)) {
      throw new Error(
        `${label}.consumedOutRefs[${index.toString()}] is invalid`,
      );
    }
    return outRef;
  });
  if (
    consumedOutRefs.length === 0 ||
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
      "profileDigest",
      "calculationDigest",
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
    typeof record.profileDigest !== "string" ||
    !HEX_32.test(record.profileDigest) ||
    typeof record.calculationDigest !== "string" ||
    !HEX_32.test(record.calculationDigest) ||
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
    profileDigest: record.profileDigest,
    calculationDigest: record.calculationDigest,
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
    profileDigest: input.plan.profileDigest,
    calculationDigest: input.plan.calculationDigest,
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
  const selected = pureAda
    .sort(compareLargestFirst)
    .slice(0, input.maximumInputs);
  if (
    selected.length === 0 ||
    selected.reduce((total, candidate) => total + candidate.lovelace, 0n) <
      input.required
  ) {
    throw new Error("prover wallet has insufficient plain-Ada collateral");
  }
  return Object.freeze(selected.sort(compareOutRef));
};

const remainingAssetNeeds = (
  required: ReadonlyMap<string, bigint>,
  selected: readonly Candidate[],
): Map<string, bigint> => {
  const remaining = new Map(required);
  for (const candidate of selected) {
    for (const [unit, quantity] of candidate.assets) {
      const needed = remaining.get(unit) ?? 0n;
      remaining.set(unit, needed > quantity ? needed - quantity : 0n);
    }
  }
  return remaining;
};

const selectFunding = (input: {
  readonly candidates: readonly Candidate[];
  readonly requiredLovelace: bigint;
  readonly requiredAssets: ReadonlyMap<string, bigint>;
  readonly maximumInputs: number;
}): readonly Candidate[] => {
  const selected: Candidate[] = [];
  const selectedOutRefs = new Set<string>();
  let remainingAssets = new Map(input.requiredAssets);
  for (const candidate of [...input.candidates].sort(compareOutRef)) {
    const contributes = [...candidate.assets].some(
      ([unit, quantity]) =>
        quantity > 0n && (remainingAssets.get(unit) ?? 0n) > 0n,
    );
    if (!contributes) continue;
    selected.push(candidate);
    if (selected.length > input.maximumInputs) {
      throw new Error(
        "prover funding exceeds the measured ordinary input bound",
      );
    }
    selectedOutRefs.add(candidate.outRef);
    remainingAssets = remainingAssetNeeds(input.requiredAssets, selected);
    if ([...remainingAssets.values()].every((quantity) => quantity === 0n)) {
      break;
    }
  }
  if ([...remainingAssets.values()].some((quantity) => quantity > 0n)) {
    throw new Error("prover wallet has insufficient required native assets");
  }
  let selectedLovelace = selected.reduce(
    (total, candidate) => total + candidate.lovelace,
    0n,
  );
  for (const candidate of [...input.candidates].sort(compareLargestFirst)) {
    if (selectedLovelace >= input.requiredLovelace) break;
    if (selectedOutRefs.has(candidate.outRef)) continue;
    selected.push(candidate);
    if (selected.length > input.maximumInputs) {
      throw new Error(
        "prover funding exceeds the measured ordinary input bound",
      );
    }
    selectedOutRefs.add(candidate.outRef);
    selectedLovelace += candidate.lovelace;
  }
  if (selectedLovelace < input.requiredLovelace) {
    throw new Error("prover wallet has insufficient deterministic funding");
  }
  return Object.freeze(selected.sort(compareOutRef));
};

/**
 * Deterministically plans disjoint funding and pure-Ada collateral inputs.
 * The plan is not a live reservation: the durable coordinator must atomically
 * persist it and reauthenticate every out-ref before minting an actuation
 * permit for a runner.
 */
export const planWatcherProverFundingReservation = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly calculation: WatcherProverFundingCalculation;
  readonly decisionDigest: string;
  readonly walletAddress: string;
  readonly utxos: readonly UTxO[];
}): WatcherProverFundingReservationPlan => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherProverFundingCalculation(input.calculation);
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
    throw new Error("prover wallet differs from measured funding key");
  }
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
    input.calculation.totals.maximumCollateralInputs,
  );
  if (
    !Number.isSafeInteger(maximumCollateralInputs) ||
    maximumCollateralInputs < 1
  ) {
    throw new Error("prover funding maximum collateral inputs is invalid");
  }
  const collateralRequired = BigInt(
    input.calculation.totals.reusableCollateralLovelace,
  );
  const collateral = selectCollateral({
    candidates,
    required: collateralRequired,
    maximumInputs: maximumCollateralInputs,
  });
  const collateralOutRefs = new Set(
    collateral.map((candidate) => candidate.outRef),
  );
  const requiredAssets = new Map(
    input.calculation.totals.requiredNativeAssets.map(({ unit, quantity }) => [
      unit,
      BigInt(quantity),
    ]),
  );
  const requiredLovelace =
    BigInt(input.calculation.totals.requiredLovelace) - collateralRequired;
  const funding = selectFunding({
    candidates: candidates.filter(
      (candidate) => !collateralOutRefs.has(candidate.outRef),
    ),
    requiredLovelace,
    requiredAssets,
    maximumInputs: Number(input.calculation.totals.maximumFundingInputs),
  });
  const reservationInput = Object.freeze({
    schemaVersion: WATCHER_PROVER_FUNDING_RESERVATION_PLAN,
    deploymentFingerprint: input.deploymentIdentity.manifestId,
    decisionDigest: input.decisionDigest,
    profileDigest: input.calculation.profileDigest,
    calculationDigest: input.calculation.calculationDigest,
    fundingPaymentKeyHash: input.calculation.fundingPaymentKeyHash,
    walletAddress: input.walletAddress,
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
    assets: assetEntries(requiredAssets),
  });
  const plan = Object.freeze({
    ...reservationInput,
    reservationId: computeDeploymentManifestJsonDigest(reservationInput),
  });
  admittedPlans.add(plan);
  return plan;
};
