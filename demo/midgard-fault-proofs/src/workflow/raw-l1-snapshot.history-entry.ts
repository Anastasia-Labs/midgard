import { array } from "./raw-l1-snapshot.admit-fraud-proof-raw-l1-transaction.js";
import {
  assetUnit,
  digest,
  exact,
  type FraudProofRawL1UnitHistory,
} from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";

export const historyEntry = (
  value: unknown,
  label: string,
): FraudProofRawL1UnitHistory => {
  const parsed = exact(
    value,
    ["unit", "fromGenesis", "completeThroughPointId", "transactionHashes"],
    label,
  );
  if (parsed.fromGenesis !== true) {
    throw new Error(`${label} must cover unit history from genesis`);
  }
  const transactionHashes = array(
    parsed.transactionHashes,
    `${label}.transactionHashes`,
  ).map((candidate, index) =>
    digest(candidate, `${label}.transactionHashes[${index.toString()}]`),
  );
  if (new Set(transactionHashes).size !== transactionHashes.length) {
    throw new Error(`${label} contains duplicate transaction hashes`);
  }
  return {
    unit: assetUnit(parsed.unit, `${label}.unit`),
    fromGenesis: true,
    completeThroughPointId: digest(
      parsed.completeThroughPointId,
      `${label}.completeThroughPointId`,
    ),
    transactionHashes,
  };
};

export const sameStringSet = (
  left: readonly string[],
  right: readonly string[],
): boolean => {
  if (left.length !== right.length) return false;
  const sortedLeft = [...left].sort();
  const sortedRight = [...right].sort();
  return sortedLeft.every((value, index) => value === sortedRight[index]);
};
