import { CML } from "@lucid-evolution/lucid";

import {
  array,
  outputContainsUnit,
} from "./raw-l1-snapshot.admit-fraud-proof-raw-l1-transaction.js";
import {
  assetUnit,
  digest,
  exact,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1UnitHistory,
} from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";

export const transactionTouchesUnit = (
  candidate: FraudProofRawL1Transaction,
  unit: string,
): boolean => {
  // Each decoded object is freed here, not left to its finalizer: the
  // outputs a history spends can carry datums of hundreds of kilobytes.
  const contains = (output: CML.TransactionOutput): boolean => {
    try {
      return outputContainsUnit(output, unit);
    } finally {
      output.free();
    }
  };
  if (
    candidate.resolvedInputs.some((input) =>
      contains(CML.TransactionOutput.from_cbor_hex(input.outputCbor)),
    )
  ) {
    return true;
  }
  const body = CML.TransactionBody.from_cbor_hex(candidate.bodyCbor);
  try {
    const outputs = body.outputs();
    try {
      for (let index = 0; index < outputs.len(); index += 1) {
        if (contains(outputs.get(index))) return true;
      }
    } finally {
      outputs.free();
    }
    const mint = body.mint();
    if (mint === undefined) return false;
    const policy = CML.ScriptHash.from_hex(unit.slice(0, 56));
    const name = CML.AssetName.from_hex(unit.slice(56));
    const minted = mint.get_assets(policy);
    try {
      return (minted?.get(name) ?? 0n) !== 0n;
    } finally {
      minted?.free();
      name.free();
      policy.free();
      mint.free();
    }
  } finally {
    body.free();
  }
};

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
