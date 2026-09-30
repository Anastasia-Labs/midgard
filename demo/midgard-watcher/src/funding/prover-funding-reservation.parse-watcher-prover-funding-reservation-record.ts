import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML, type UTxO } from "@lucid-evolution/lucid";

import {
  ACTION_KIND,
  assertWatcherProverFundingReservationPlan,
  ASSET_UNIT,
  exactRecord,
  HEX_32,
  NATURAL,
  OUT_REF,
  parseReservationInputs,
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationTransition,
} from "./prover-funding-reservation.watcher-prover-funding-reservation-store.js";

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

export type Candidate = Readonly<{
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

export const compareOutRef = (left: Candidate, right: Candidate): number =>
  left.outRef.localeCompare(right.outRef);

export const compareLargestFirst = (
  left: Candidate,
  right: Candidate,
): number =>
  left.lovelace === right.lovelace
    ? compareOutRef(left, right)
    : left.lovelace > right.lovelace
      ? -1
      : 1;

export const parseCandidate = (
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

export const assetEntries = (
  assets: ReadonlyMap<string, bigint>,
): readonly Readonly<{ unit: string; quantity: string }>[] =>
  Object.freeze(
    [...assets.entries()]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([unit, quantity]) =>
        Object.freeze({ unit, quantity: quantity.toString() }),
      ),
  );
