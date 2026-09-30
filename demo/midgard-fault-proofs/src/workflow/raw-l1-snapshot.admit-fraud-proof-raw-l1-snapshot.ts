import { CML } from "@lucid-evolution/lucid";

import {
  address,
  admitFraudProofRawL1Transaction,
  array,
  computeFraudProofRawL1RollbackCursor,
  outputAddress,
  outputContainsUnit,
  point,
  utxo,
} from "./raw-l1-snapshot.admit-fraud-proof-raw-l1-transaction.js";
import {
  assetUnit,
  digest,
  exact,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofL1ObservationDepth,
  type FraudProofRawL1ScopeRole,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotRequest,
  HEX_28,
  OUT_REF,
  RAW_L1_SCOPE_ROLES,
  string,
} from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import {
  historyEntry,
  sameStringSet,
  transactionTouchesUnit,
} from "./raw-l1-snapshot.history-entry.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

export const admitFraudProofRawL1Snapshot = ({
  value,
  request,
  releaseFinality,
  observationDepth = "release_finality",
}: {
  readonly value: unknown;
  readonly request: FraudProofRawL1SnapshotRequest;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly observationDepth?: FraudProofL1ObservationDepth;
}): FraudProofRawL1Snapshot => {
  const minimumConfirmationDepth =
    observationDepth === "inclusion"
      ? 1
      : releaseFinality.policy.confirmationDepth;
  const root = exact(
    value,
    [
      "schemaVersion",
      "deploymentIdentityDigest",
      "blueprintHash",
      "finalityPolicyDigest",
      "headerHash",
      "provenance",
      "cursor",
      "scopes",
      "historyUnits",
      "history",
      "transactions",
    ],
    "raw L1 snapshot",
  );
  if (root.schemaVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION) {
    throw new Error("raw L1 snapshot has an unsupported schema");
  }
  const deploymentIdentityDigest = digest(
    root.deploymentIdentityDigest,
    "raw L1 snapshot deploymentIdentityDigest",
  );
  const blueprintHash = digest(
    root.blueprintHash,
    "raw L1 snapshot blueprintHash",
  );
  const finalityPolicyDigest = digest(
    root.finalityPolicyDigest,
    "raw L1 snapshot finalityPolicyDigest",
  );
  const headerHash = string(root.headerHash, "raw L1 snapshot headerHash");
  if (!HEX_28.test(headerHash))
    throw new Error("raw L1 snapshot headerHash must be 28-byte hex");
  if (
    request.deploymentIdentityDigest !==
      releaseFinality.deploymentIdentityDigest ||
    request.blueprintHash !== releaseFinality.blueprintHash ||
    request.finalityPolicyDigest !== releaseFinality.policyDigest ||
    !HEX_28.test(request.headerHash)
  ) {
    throw new Error(
      "raw L1 request is not bound to the verified release identity",
    );
  }
  if (
    deploymentIdentityDigest !== request.deploymentIdentityDigest ||
    deploymentIdentityDigest !== releaseFinality.deploymentIdentityDigest ||
    blueprintHash !== request.blueprintHash ||
    blueprintHash !== releaseFinality.blueprintHash ||
    finalityPolicyDigest !== request.finalityPolicyDigest ||
    finalityPolicyDigest !== releaseFinality.policyDigest ||
    headerHash !== request.headerHash
  ) {
    throw new Error(
      "raw L1 snapshot changed a deployment/finality/header identity",
    );
  }
  const provenanceRecord = exact(
    root.provenance,
    [
      "trustClass",
      "sourceId",
      "grade",
      "sourceMode",
      "kupoCheckpoint",
      "ogmiosTip",
    ],
    "raw L1 snapshot provenance",
  );
  if (
    provenanceRecord.trustClass !== "authenticated_cardano_l1" ||
    provenanceRecord.grade !== "security" ||
    provenanceRecord.sourceMode !== "local_kupo_ogmios"
  ) {
    throw new Error(
      "raw L1 snapshot lacks security-grade Kupo/Ogmios provenance",
    );
  }
  const provenance = {
    trustClass: "authenticated_cardano_l1" as const,
    sourceId: string(provenanceRecord.sourceId, "raw L1 snapshot sourceId"),
    grade: "security" as const,
    sourceMode: "local_kupo_ogmios" as const,
    kupoCheckpoint: point(
      provenanceRecord.kupoCheckpoint,
      "raw L1 Kupo checkpoint",
    ),
    ogmiosTip: point(provenanceRecord.ogmiosTip, "raw L1 Ogmios tip"),
  };
  const cursorRecord = exact(
    root.cursor,
    ["point", "tip", "confirmationDepth", "rollbackCursor"],
    "raw L1 snapshot cursor",
  );
  if (
    !Number.isSafeInteger(cursorRecord.confirmationDepth) ||
    (cursorRecord.confirmationDepth as number) < minimumConfirmationDepth
  ) {
    throw new Error(
      "raw L1 snapshot cursor is below the required observation depth",
    );
  }
  const cursor = {
    point: point(cursorRecord.point, "raw L1 cursor point"),
    tip: point(cursorRecord.tip, "raw L1 cursor tip"),
    confirmationDepth: cursorRecord.confirmationDepth as number,
    rollbackCursor: digest(
      cursorRecord.rollbackCursor,
      "raw L1 rollback cursor",
    ),
  };
  if (
    provenance.kupoCheckpoint.pointId !== cursor.point.pointId ||
    provenance.ogmiosTip.pointId !== cursor.tip.pointId
  ) {
    throw new Error(
      "raw L1 provider checkpoints disagree with the rollback cursor",
    );
  }
  const expectedCursorDepth =
    BigInt(cursor.tip.blockNo) - BigInt(cursor.point.blockNo) + 1n;
  if (
    expectedCursorDepth <= 0n ||
    expectedCursorDepth !== BigInt(cursor.confirmationDepth) ||
    BigInt(cursor.point.slot) > BigInt(cursor.tip.slot)
  ) {
    throw new Error(
      "raw L1 cursor confirmation depth disagrees with chain points",
    );
  }
  if (
    cursor.rollbackCursor !==
    computeFraudProofRawL1RollbackCursor({
      deploymentIdentityDigest,
      blueprintHash,
      finalityPolicyDigest,
      sourceId: provenance.sourceId,
      pointId: cursor.point.pointId,
    })
  ) {
    throw new Error(
      "raw L1 rollback cursor does not bind the release and chain point",
    );
  }
  const requestedScopes = new Map(
    request.scopes.map((scope, index) => {
      if (!RAW_L1_SCOPE_ROLES.has(scope.role)) {
        throw new Error(
          `raw L1 request scopes[${index.toString()}].role is unsupported`,
        );
      }
      return [
        scope.role,
        address(
          scope.address,
          `raw L1 request scopes[${index.toString()}].address`,
        ),
      ] as const;
    }),
  );
  if (requestedScopes.size !== request.scopes.length) {
    throw new Error("raw L1 request contains duplicate scope roles");
  }
  const seenRoles = new Set<string>();
  const scopes = array(root.scopes, "raw L1 snapshot scopes").map(
    (candidate, index) => {
      const label = `raw L1 snapshot scopes[${index.toString()}]`;
      const parsed = exact(candidate, ["role", "address", "utxos"], label);
      const role = string(
        parsed.role,
        `${label}.role`,
      ) as FraudProofRawL1ScopeRole;
      const scopedAddress = address(parsed.address, `${label}.address`);
      if (
        !RAW_L1_SCOPE_ROLES.has(role) ||
        seenRoles.has(role) ||
        requestedScopes.get(role) !== scopedAddress
      ) {
        throw new Error(`${label} is duplicate or was not exactly requested`);
      }
      seenRoles.add(role);
      const utxos = array(parsed.utxos, `${label}.utxos`).map(
        (entry, utxoIndex) =>
          utxo(entry, `${label}.utxos[${utxoIndex.toString()}]`),
      );
      if (new Set(utxos.map((entry) => entry.outRef)).size !== utxos.length) {
        throw new Error(`${label} contains duplicate outRefs`);
      }
      if (utxos.some((entry) => outputAddress(entry) !== scopedAddress)) {
        throw new Error(`${label} contains an output from a different address`);
      }
      return { role, address: scopedAddress, utxos };
    },
  );
  if (seenRoles.size !== requestedScopes.size) {
    throw new Error("raw L1 snapshot omitted an address scope");
  }
  const historyUnits = array(
    root.historyUnits,
    "raw L1 snapshot historyUnits",
  ).map((unit, index) =>
    assetUnit(unit, `raw L1 snapshot historyUnits[${index.toString()}]`),
  );
  const requestedHistoryUnits = request.historyUnits.map((unit, index) =>
    assetUnit(unit, `raw L1 request historyUnits[${index.toString()}]`),
  );
  if (
    new Set(requestedHistoryUnits).size !== requestedHistoryUnits.length ||
    new Set(historyUnits).size !== historyUnits.length ||
    !sameStringSet(historyUnits, requestedHistoryUnits)
  ) {
    throw new Error("raw L1 snapshot changed the requested history units");
  }
  const transactions = array(
    root.transactions,
    "raw L1 snapshot transactions",
  ).map((candidate, index) =>
    admitFraudProofRawL1Transaction(
      candidate,
      `raw L1 snapshot transactions[${index.toString()}]`,
      minimumConfirmationDepth,
    ),
  );
  if (
    new Set(transactions.map((entry) => entry.txHash)).size !==
    transactions.length
  ) {
    throw new Error("raw L1 snapshot contains duplicate transactions");
  }
  const transactionHashes = new Set(transactions.map((entry) => entry.txHash));
  const transactionByHash = new Map(
    transactions.map((entry) => [entry.txHash, entry] as const),
  );
  for (const [index, entry] of transactions.entries()) {
    const expectedDepth =
      BigInt(cursor.tip.blockNo) - BigInt(entry.inclusionPoint.blockNo) + 1n;
    if (
      expectedDepth <= 0n ||
      expectedDepth !== BigInt(entry.confirmationDepth) ||
      BigInt(entry.inclusionPoint.blockNo) > BigInt(cursor.point.blockNo) ||
      BigInt(entry.inclusionPoint.slot) > BigInt(cursor.point.slot)
    ) {
      throw new Error(
        `raw L1 snapshot transactions[${index.toString()}] has inconsistent inclusion finality`,
      );
    }
  }
  const history = array(root.history, "raw L1 snapshot history").map(
    (candidate, index) =>
      historyEntry(candidate, `raw L1 snapshot history[${index.toString()}]`),
  );
  if (
    new Set(history.map((entry) => entry.unit)).size !== history.length ||
    !sameStringSet(
      history.map((entry) => entry.unit),
      requestedHistoryUnits,
    )
  ) {
    throw new Error(
      "raw L1 snapshot omitted or duplicated unit history coverage",
    );
  }
  for (const [index, entry] of history.entries()) {
    const actualHashes = transactions
      .filter((candidate) => transactionTouchesUnit(candidate, entry.unit))
      .map((candidate) => candidate.txHash);
    if (
      entry.completeThroughPointId !== cursor.point.pointId ||
      entry.transactionHashes.some(
        (txHash) => !transactionHashes.has(txHash),
      ) ||
      !sameStringSet(entry.transactionHashes, actualHashes)
    ) {
      throw new Error(
        `raw L1 snapshot history[${index.toString()}] is incomplete or references a transaction that does not touch its unit`,
      );
    }
  }
  for (const [scopeIndex, scope] of scopes.entries()) {
    for (const [utxoIndex, candidate] of scope.utxos.entries()) {
      const touchesRequestedHistoryUnit = historyUnits.some((unit) =>
        outputContainsUnit(
          CML.TransactionOutput.from_cbor_hex(candidate.outputCbor),
          unit,
        ),
      );
      if (!touchesRequestedHistoryUnit) continue;
      const match = OUT_REF.exec(candidate.outRef);
      const creation =
        match === null ? undefined : transactionByHash.get(match[1]!);
      const outputIndex = match === null ? -1 : Number(match[2]);
      const createdOutputs =
        creation?.bodyCbor === undefined
          ? undefined
          : CML.TransactionBody.from_cbor_hex(creation.bodyCbor).outputs();
      const createdOutput =
        createdOutputs === undefined || outputIndex >= createdOutputs.len()
          ? undefined
          : createdOutputs.get(outputIndex);
      if (
        createdOutput === undefined ||
        createdOutput.to_canonical_cbor_hex() !== candidate.outputCbor
      ) {
        throw new Error(
          `raw L1 snapshot scopes[${scopeIndex.toString()}].utxos[${utxoIndex.toString()}] lacks its exact admitted creation transaction`,
        );
      }
    }
  }
  return {
    schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
    deploymentIdentityDigest,
    blueprintHash,
    finalityPolicyDigest,
    headerHash,
    provenance,
    cursor,
    scopes,
    historyUnits,
    history,
    transactions,
  };
};
