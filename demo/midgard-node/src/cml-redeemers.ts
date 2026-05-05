/**
 * Shared redeemer-ordering helpers that bridge CML transaction bodies with the
 * ledger's script-context redeemer layout.
 * Workers and transaction builders use this to keep off-chain indexes aligned
 * with on-chain evaluation order.
 */
import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

/**
 * Redeemer tags exposed by provider evaluation APIs.
 */
export type ProviderRedeemerTag =
  | "spend"
  | "mint"
  | "publish"
  | "withdraw"
  | "vote"
  | "propose";

/**
 * Provider-side execution-unit result for one redeemer.
 */
export type ProviderEvaluationResult = {
  readonly redeemer_tag: ProviderRedeemerTag;
  readonly redeemer_index: number;
  readonly ex_units: { readonly mem: number; readonly steps: number };
};

/**
 * CML redeemer pointer as carried in a transaction witness set.
 */
export type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

const DUMMY_REDEEMER_EX_UNITS = {
  mem: 1_000_000,
  steps: 1_000_000,
} as const;

/**
 * Sort rank used to translate context-order redeemers into tx-info order.
 */
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

/**
 * Converts a CML redeemer tag into the provider-facing string variant.
 */
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
      throw new Error(`Unsupported redeemer tag: ${tag}`);
  }
};

/**
 * Extracts all redeemer pointers from a transaction in the context order used
 * by the witness set.
 */
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
    for (let i = 0; i < legacy.len(); i += 1) {
      const redeemer = legacy.get(i);
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
  for (let i = 0; i < keys.len(); i += 1) {
    const key = keys.get(i);
    pointers.push({
      tag: key.tag(),
      index: key.index(),
    });
  }
  return pointers;
};

/**
 * Converts context-order redeemers into the tx-info order used by Plutus
 * evaluation.
 */
export const getTxInfoRedeemerIndexes = (
  pointers: readonly RedeemerPointer[],
): readonly number[] => {
  const inContextOrder = pointers.map((pointer, contextIndex) => ({
    pointer,
    contextIndex,
  }));
  const inTxInfoOrder = [...inContextOrder].sort((a, b) => {
    const rankA = txInfoRedeemerPurposeRank(a.pointer.tag);
    const rankB = txInfoRedeemerPurposeRank(b.pointer.tag);
    if (rankA !== rankB) {
      return rankA - rankB;
    }
    if (a.pointer.index !== b.pointer.index) {
      return a.pointer.index < b.pointer.index ? -1 : 1;
    }
    return a.contextIndex - b.contextIndex;
  });

  const txInfoIndexes = Array<number>(pointers.length).fill(-1);
  for (
    let txInfoIndex = 0;
    txInfoIndex < inTxInfoOrder.length;
    txInfoIndex += 1
  ) {
    const { contextIndex } = inTxInfoOrder[txInfoIndex];
    txInfoIndexes[contextIndex] = txInfoIndex;
  }
  return txInfoIndexes;
};

/**
 * Finds the CBOR payload for one specific redeemer pointer.
 */
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
    for (let i = 0; i < legacy.len(); i += 1) {
      const redeemer = legacy.get(i);
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
  for (let i = 0; i < keys.len(); i += 1) {
    const key = keys.get(i);
    if (key.tag() !== pointer.tag || key.index() !== pointer.index) {
      continue;
    }
    const value = map.get(key);
    return value?.data().to_cbor_hex();
  }
  return undefined;
};

/**
 * Minimal provider surface needed for temporary redeemer-evaluation stubbing.
 */
type ProviderWithEvaluateTx = {
  evaluateTx?: (
    tx: string,
    additionalUTxOs?: readonly UTxO[],
  ) => Promise<readonly ProviderEvaluationResult[]>;
};

/**
 * Temporarily replaces provider `evaluateTx` with a deterministic stub while a
 * callback runs.
 */
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
    return run();
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
