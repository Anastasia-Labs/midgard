import { type UTxO } from "@lucid-evolution/lucid";

import {
  canonicalOutRefs,
  DIGEST,
  exact,
  NATURAL,
  OUT_REF,
  type PermitState,
  reservedInput,
  type WorkflowFundingReservationSnapshot,
  WorkflowFundingReservationUnavailableError,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";

export const parseSnapshot = (
  value: unknown,
): WorkflowFundingReservationSnapshot => {
  const record = exact(
    value,
    [
      "reservationId",
      "deploymentFingerprint",
      "decisionDigest",
      "policyDigest",
      "reservationBasisDigest",
      "rollbackGeneration",
      "revision",
      "walletAddress",
      "fundingPaymentKeyHash",
      "state",
      "activeInputs",
    ],
    "production funding reservation snapshot",
  );
  if (
    typeof record.reservationId !== "string" ||
    !DIGEST.test(record.reservationId) ||
    typeof record.deploymentFingerprint !== "string" ||
    !DIGEST.test(record.deploymentFingerprint) ||
    typeof record.decisionDigest !== "string" ||
    !DIGEST.test(record.decisionDigest) ||
    typeof record.policyDigest !== "string" ||
    !DIGEST.test(record.policyDigest) ||
    typeof record.reservationBasisDigest !== "string" ||
    !DIGEST.test(record.reservationBasisDigest) ||
    typeof record.rollbackGeneration !== "string" ||
    !NATURAL.test(record.rollbackGeneration) ||
    typeof record.revision !== "string" ||
    !NATURAL.test(record.revision) ||
    typeof record.walletAddress !== "string" ||
    record.walletAddress.length === 0 ||
    typeof record.fundingPaymentKeyHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(record.fundingPaymentKeyHash) ||
    (record.state !== "active" &&
      record.state !== "released" &&
      record.state !== "conflict") ||
    !Array.isArray(record.activeInputs)
  ) {
    throw new Error("production funding reservation snapshot is invalid");
  }
  const activeInputs = record.activeInputs.map((entry, index) =>
    reservedInput(
      entry,
      `production funding reservation snapshot.activeInputs[${index.toString()}]`,
    ),
  );
  canonicalOutRefs(
    activeInputs.map(({ outRef }) => outRef),
    "production funding reservation active inputs",
  );
  // An active reservation may yield idle inputs after authenticated abandonment.
  // Submission funding selection still requires sufficient reserved inputs.
  // Conflicted leases stay quarantined until canonical lineage is resolved.
  if (record.state === "released" && activeInputs.length !== 0) {
    throw new Error("released production funding reservation retains inputs");
  }
  return Object.freeze({
    reservationId: record.reservationId,
    deploymentFingerprint: record.deploymentFingerprint,
    decisionDigest: record.decisionDigest,
    policyDigest: record.policyDigest,
    reservationBasisDigest: record.reservationBasisDigest,
    rollbackGeneration: record.rollbackGeneration,
    revision: record.revision,
    walletAddress: record.walletAddress,
    fundingPaymentKeyHash: record.fundingPaymentKeyHash,
    state: record.state,
    activeInputs: Object.freeze(activeInputs),
  });
};

export const assertSnapshotInputBounds = ({
  snapshot,
  maximumCollateralInputs,
}: {
  readonly snapshot: WorkflowFundingReservationSnapshot;
  readonly maximumCollateralInputs: number;
}): void => {
  if (
    snapshot.activeInputs.filter(({ role }) => role === "collateral").length >
    maximumCollateralInputs
  ) {
    throw new Error(
      "production funding reservation exceeds its collateral input bound",
    );
  }
};

export const parseStateSnapshot = (
  state: PermitState,
  value: unknown,
): WorkflowFundingReservationSnapshot => {
  const snapshot = parseSnapshot(value);
  assertSnapshotInputBounds({
    snapshot,
    maximumCollateralInputs: state.reservationMaximumCollateralInputs,
  });
  return snapshot;
};

export const exactUtxos = ({
  snapshot,
  utxos,
}: {
  readonly snapshot: WorkflowFundingReservationSnapshot;
  readonly utxos: readonly UTxO[];
}): ReadonlyMap<string, UTxO> => {
  const resolved = new Map<string, UTxO>();
  for (const utxo of utxos) {
    const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (!OUT_REF.test(outRef) || resolved.has(outRef)) {
      throw new Error("production funding resolver returned malformed inputs");
    }
    resolved.set(outRef, utxo);
  }
  const expected = snapshot.activeInputs.map(({ outRef }) => outRef);
  const actual = [...resolved.keys()].sort();
  if (actual.some((outRef) => !expected.includes(outRef))) {
    throw new Error(
      "production funding resolver changed the reserved input set",
    );
  }
  for (const reserved of snapshot.activeInputs) {
    const utxo = resolved.get(reserved.outRef);
    if (utxo === undefined) continue;
    if (utxo.address !== snapshot.walletAddress) {
      throw new Error("production funding resolver returned a foreign address");
    }
    const lovelace = utxo.assets.lovelace;
    if (
      typeof lovelace !== "bigint" ||
      lovelace.toString() !== reserved.lovelace
    ) {
      throw new Error("production funding resolver changed reserved lovelace");
    }
    const actualAssets = Object.entries(utxo.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([left], [right]) => left.localeCompare(right));
    if (
      actualAssets.length !== reserved.assets.length ||
      actualAssets.some(
        ([unit, quantity], index) =>
          typeof quantity !== "bigint" ||
          unit !== reserved.assets[index]!.unit ||
          quantity.toString() !== reserved.assets[index]!.quantity,
      )
    ) {
      throw new Error("production funding resolver changed reserved assets");
    }
  }
  if (actual.length !== expected.length)
    throw new WorkflowFundingReservationUnavailableError();
  return resolved;
};
