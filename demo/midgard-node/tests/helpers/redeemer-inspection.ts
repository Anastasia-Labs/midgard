import { CML } from "@lucid-evolution/lucid";

export type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

const txInfoRedeemerPurposeRank = (tag: number): number => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return 0;
    case CML.RedeemerTag.Mint:
      return 1;
    case CML.RedeemerTag.Cert:
      return 2;
    case CML.RedeemerTag.Reward:
      return 3;
    case CML.RedeemerTag.Voting:
      return 4;
    case CML.RedeemerTag.Proposing:
      return 5;
    default:
      return Number.MAX_SAFE_INTEGER;
  }
};

export const getRedeemerPointersInContextOrder = (
  tx: CML.Transaction,
): readonly RedeemerPointer[] => {
  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    return [];
  }

  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    const pointers: RedeemerPointer[] = [];
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      pointers.push({ tag: redeemer.tag(), index: redeemer.index() });
    }
    return pointers;
  }

  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map === undefined) {
    return [];
  }
  const pointers: RedeemerPointer[] = [];
  const keys = map.keys();
  for (let index = 0; index < keys.len(); index += 1) {
    const key = keys.get(index);
    pointers.push({ tag: key.tag(), index: key.index() });
  }
  return pointers;
};

export const getTxInfoRedeemerIndexes = (
  pointers: readonly RedeemerPointer[],
): readonly number[] => {
  const inContextOrder = pointers.map((pointer, contextIndex) => ({
    pointer,
    contextIndex,
  }));
  const inTxInfoOrder = [...inContextOrder].sort((left, right) => {
    const leftRank = txInfoRedeemerPurposeRank(left.pointer.tag);
    const rightRank = txInfoRedeemerPurposeRank(right.pointer.tag);
    if (leftRank !== rightRank) {
      return leftRank - rightRank;
    }
    if (left.pointer.index !== right.pointer.index) {
      return left.pointer.index < right.pointer.index ? -1 : 1;
    }
    return left.contextIndex - right.contextIndex;
  });

  const txInfoIndexes: number[] = [];
  inTxInfoOrder.forEach(({ contextIndex }, txInfoIndex) => {
    txInfoIndexes[contextIndex] = txInfoIndex;
  });
  return txInfoIndexes;
};

export const findRedeemerDataCbor = (
  tx: CML.Transaction,
  pointer: RedeemerPointer | undefined,
): string | undefined => {
  if (pointer === undefined) {
    return undefined;
  }
  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    return undefined;
  }

  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      if (
        redeemer.tag() === pointer.tag &&
        redeemer.index() === pointer.index
      ) {
        return redeemer.data().to_cbor_hex();
      }
    }
    return undefined;
  }

  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map === undefined) {
    return undefined;
  }
  const keys = map.keys();
  for (let index = 0; index < keys.len(); index += 1) {
    const key = keys.get(index);
    if (key.tag() !== pointer.tag || key.index() !== pointer.index) {
      continue;
    }
    return map.get(key)?.data().to_cbor_hex();
  }
  return undefined;
};

export const resolveRedeemerTxInfoIndex = ({
  pointers,
  target,
  label = `tag=${target.tag.toString()},index=${target.index.toString()}`,
}: {
  readonly pointers: readonly RedeemerPointer[];
  readonly target: RedeemerPointer;
  readonly label?: string;
}): bigint => {
  const contextIndex = pointers.findIndex(
    (pointer) => pointer.tag === target.tag && pointer.index === target.index,
  );
  if (contextIndex < 0) {
    throw new Error(`Redeemer pointer not found for ${label}.`);
  }
  return BigInt(getTxInfoRedeemerIndexes(pointers)[contextIndex]);
};

export const resolveMintPolicyContextIndex = ({
  policyIds,
  targetPolicyId,
}: {
  readonly policyIds: readonly string[];
  readonly targetPolicyId: string;
}): bigint => {
  const index = [...new Set(policyIds)].sort().indexOf(targetPolicyId);
  if (index < 0) {
    throw new Error(`Mint policy ${targetPolicyId} missing from policy set.`);
  }
  return BigInt(index);
};
