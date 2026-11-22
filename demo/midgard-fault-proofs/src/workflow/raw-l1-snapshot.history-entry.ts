import { CML } from "@lucid-evolution/lucid";

import { array } from "./raw-l1-snapshot.admit-fraud-proof-raw-l1-transaction.js";
import {
  assetUnit,
  digest,
  exact,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1UnitHistory,
} from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";

type UnitKey = Readonly<{
  unit: string;
  policy: CML.ScriptHash;
  name: CML.AssetName;
}>;

/** `read` over the decoded policy and name of each of `units`. */
export const withUnitKeys = <T>(
  units: readonly string[],
  read: (keys: readonly UnitKey[]) => T,
): T => {
  const keys = units.map((unit) => ({
    unit,
    policy: CML.ScriptHash.from_hex(unit.slice(0, 56)),
    name: CML.AssetName.from_hex(unit.slice(56)),
  }));
  try {
    return read(keys);
  } finally {
    for (const { policy, name } of keys) {
      policy.free();
      name.free();
    }
  }
};

/** Adds to `found` the units of `keys` that `output` holds, and frees it:
 * the outputs a history spends can carry datums of hundreds of kilobytes. */
const addUnitsHeld = (
  output: CML.TransactionOutput,
  keys: readonly UnitKey[],
  found: Set<string>,
): void => {
  const value = output.amount();
  const assets = value.multi_asset();
  try {
    for (const { unit, policy, name } of keys) {
      if ((assets.get(policy, name) ?? 0n) !== 0n) found.add(unit);
    }
  } finally {
    assets.free();
    value.free();
    output.free();
  }
};

/** The units of `keys` the output `outputCbor` holds. */
export const unitsHeldBy = (
  outputCbor: string,
  keys: readonly UnitKey[],
): ReadonlySet<string> => {
  const found = new Set<string>();
  addUnitsHeld(CML.TransactionOutput.from_cbor_hex(outputCbor), keys, found);
  return found;
};

/** The units of `keys` that `candidate` spends, creates or mints, from one
 * decode of each of its outputs. */
export const unitsTouchedBy = (
  candidate: FraudProofRawL1Transaction,
  keys: readonly UnitKey[],
): ReadonlySet<string> => {
  const found = new Set<string>();
  for (const input of candidate.resolvedInputs) {
    addUnitsHeld(
      CML.TransactionOutput.from_cbor_hex(input.outputCbor),
      keys,
      found,
    );
  }
  const body = CML.TransactionBody.from_cbor_hex(candidate.bodyCbor);
  try {
    const outputs = body.outputs();
    try {
      for (let index = 0; index < outputs.len(); index += 1) {
        addUnitsHeld(outputs.get(index), keys, found);
      }
    } finally {
      outputs.free();
    }
    const mint = body.mint();
    if (mint === undefined) return found;
    try {
      for (const { unit, policy, name } of keys) {
        const minted = mint.get_assets(policy);
        try {
          if ((minted?.get(name) ?? 0n) !== 0n) found.add(unit);
        } finally {
          minted?.free();
        }
      }
    } finally {
      mint.free();
    }
    return found;
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
