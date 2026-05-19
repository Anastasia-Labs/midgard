import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

export type ProviderRedeemerTag =
  | "spend"
  | "mint"
  | "publish"
  | "withdraw"
  | "vote"
  | "propose";

export type ProviderEvaluationResult = {
  readonly redeemer_tag: ProviderRedeemerTag;
  readonly redeemer_index: number;
  readonly ex_units: { readonly mem: number; readonly steps: number };
};

export type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

const DUMMY_REDEEMER_EX_UNITS = {
  mem: 1_000_000,
  steps: 1_000_000,
} as const;

const compareHex = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

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

export const toProviderRedeemerTag = (tag: number): ProviderRedeemerTag => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "publish";
    case CML.RedeemerTag.Reward:
      return "withdraw";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      throw new Error(`Unsupported redeemer tag: ${tag.toString()}`);
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
      pointers.push({
        tag: redeemer.tag(),
        index: redeemer.index(),
      });
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
    pointers.push({
      tag: key.tag(),
      index: key.index(),
    });
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

  const txInfoIndexes = Array<number>(pointers.length).fill(-1);
  for (
    let txInfoIndex = 0;
    txInfoIndex < inTxInfoOrder.length;
    txInfoIndex += 1
  ) {
    const { contextIndex } = inTxInfoOrder[txInfoIndex]!;
    txInfoIndexes[contextIndex] = txInfoIndex;
  }
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
    const value = map.get(key);
    return value?.data().to_cbor_hex();
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
  const txInfoIndexes = getTxInfoRedeemerIndexes(pointers);
  const txInfoIndex = txInfoIndexes[contextIndex];
  if (txInfoIndex === undefined || txInfoIndex < 0) {
    throw new Error(`Failed to derive tx-info redeemer index for ${label}.`);
  }
  return BigInt(txInfoIndex);
};

export const resolveMintPolicyContextIndex = ({
  policyIds,
  targetPolicyId,
}: {
  readonly policyIds: readonly string[];
  readonly targetPolicyId: string;
}): bigint => {
  const normalizedTarget = targetPolicyId.toLowerCase();
  const sortedPolicyIds = [
    ...new Set(policyIds.map((policyId) => policyId.toLowerCase())),
  ].sort(compareHex);
  const index = sortedPolicyIds.indexOf(normalizedTarget);
  if (index < 0) {
    throw new Error(`Mint policy ${targetPolicyId} missing from policy set.`);
  }
  return BigInt(index);
};

export const resolveMintPolicyTxInfoRedeemerIndexFromPolicySet = ({
  policyIds,
  targetPolicyId,
  precedingSpendRedeemerCount = 0,
}: {
  readonly policyIds: readonly string[];
  readonly targetPolicyId: string;
  readonly precedingSpendRedeemerCount?: number;
}): bigint =>
  BigInt(precedingSpendRedeemerCount) +
  resolveMintPolicyContextIndex({ policyIds, targetPolicyId });

export const resolveMintPolicyRedeemerTxInfoIndex = ({
  tx,
  policyIds,
  targetPolicyId,
}: {
  readonly tx: CML.Transaction;
  readonly policyIds: readonly string[];
  readonly targetPolicyId: string;
}): bigint => {
  const pointerIndex = resolveMintPolicyContextIndex({
    policyIds,
    targetPolicyId,
  });
  return resolveRedeemerTxInfoIndex({
    pointers: getRedeemerPointersInContextOrder(tx),
    target: { tag: CML.RedeemerTag.Mint, index: pointerIndex },
    label: `mint policy ${targetPolicyId}`,
  });
};

type ProviderWithEvaluateTx = {
  evaluateTx?: (
    tx: string,
    additionalUTxOs?: readonly UTxO[],
  ) => Promise<readonly ProviderEvaluationResult[]>;
};

export const withStubbedProviderEvaluation = async <A>(
  lucid: LucidEvolution,
  run: () => Promise<A>,
  resolveExUnits: (
    pointers: readonly RedeemerPointer[],
  ) =>
    | ProviderEvaluationResult["ex_units"]
    | Promise<ProviderEvaluationResult["ex_units"]> = () =>
    DUMMY_REDEEMER_EX_UNITS,
): Promise<A> => {
  const provider = lucid.config().provider as ProviderWithEvaluateTx;
  if (typeof provider.evaluateTx !== "function") {
    return await run();
  }

  const originalEvaluateTx = provider.evaluateTx.bind(provider);
  provider.evaluateTx = async (txCbor) => {
    const tx = CML.Transaction.from_cbor_hex(txCbor);
    const pointers = getRedeemerPointersInContextOrder(tx);
    const exUnits = await resolveExUnits(pointers);
    return pointers.map((pointer) => ({
      redeemer_tag: toProviderRedeemerTag(pointer.tag),
      redeemer_index: Number(pointer.index),
      ex_units: exUnits,
    }));
  };
  try {
    return await run();
  } finally {
    provider.evaluateTx = originalEvaluateTx;
  }
};
