import { compareOutRefs, type OutRefLike } from "@al-ft/midgard-core/out-ref";
import { CML } from "@lucid-evolution/lucid";

import {
  decodeLedgerSnapshotOutput,
  type LedgerSnapshotOutput,
} from "./l1-ledger-snapshot.js";

/** Raw Ogmios purpose names, ordered as ledger redeemer pointers. In particular
 * Ogmios uses `withdraw`, unlike the watcher's normalized `withdrawal` name. */
const purposes = [
  "spend",
  "mint",
  "publish",
  "withdraw",
  "vote",
  "propose",
] as const;
type Purpose = (typeof purposes)[number];
export type HistoryTransactionRedeemer = Readonly<{
  purpose: Purpose;
  index: number;
  cbor: string;
}>;
export type HistoryTransactionWithdrawal = Readonly<{
  account: string;
  networkId: number;
  credential: Readonly<{ kind: "script" | "key"; hash: string }>;
  amount: bigint;
}>;
export type HistoryChainTransaction = Readonly<{
  txHash: string;
  /** Only successful transactions affect normal inputs/outputs/mint/observers. */
  spends: "inputs" | "collaterals";
  inputs: readonly OutRefLike[];
  references: readonly OutRefLike[];
  collaterals: readonly OutRefLike[];
  outputs: readonly LedgerSnapshotOutput[];
  collateralReturn?: LedgerSnapshotOutput;
  mint: Readonly<Record<string, bigint>>;
  withdrawals: readonly HistoryTransactionWithdrawal[];
  redeemers: readonly HistoryTransactionRedeemer[];
  invalidBefore?: number;
  invalidAfter?: number;
}>;

const object = (value: unknown): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("History transaction expected an object");
  return value as Record<string, unknown>;
};
const array = (value: unknown): readonly unknown[] => {
  if (!Array.isArray(value))
    throw new Error("History transaction expected an array");
  return value;
};
const natural = (value: unknown): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new Error("History transaction expected a safe natural number");
  return value;
};
const bytes = (value: unknown, size?: number): string => {
  if (
    typeof value !== "string" ||
    !/^(?:[0-9a-f]{2})*$/u.test(value) ||
    (size !== undefined && value.length !== size * 2)
  )
    throw new Error("History transaction expected exact lowercase base16");
  return value;
};
const integer = (value: unknown): bigint => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number" && Number.isSafeInteger(value))
    return BigInt(value);
  throw new Error("History transaction requires a lossless integer quantity");
};
const refs = (value: unknown): readonly OutRefLike[] => {
  const sorted = array(value)
    .map((entry) => {
      const raw = object(entry);
      return Object.freeze({
        txHash: bytes(object(raw.transaction).id, 32),
        outputIndex: natural(raw.index),
      });
    })
    .sort(compareOutRefs);
  if (
    sorted.some(
      (ref, index) =>
        index > 0 && compareOutRefs(sorted[index - 1]!, ref) === 0,
    )
  )
    throw new Error("History transaction repeats an input reference");
  return Object.freeze(sorted);
};

const withdrawals = (
  value: unknown,
): readonly HistoryTransactionWithdrawal[] => {
  // CML maps preserve insertion order. Ledger RewardAccount Ord compares
  // network, then ScriptHashObj before KeyHashObj, then hash (not address bytes).
  // See Cardano.Ledger.{Address,Credential} and pinned UPLC sort_reward_accounts.
  const ordered = CML.MapRewardAccountToCoin.new();
  for (const [account, raw] of Object.entries(object(value))) {
    const reward = CML.RewardAddress.from_address(
      CML.Address.from_bech32(account),
    );
    if (reward === undefined)
      throw new Error("History withdrawal requires a reward account");
    const amount = integer(object(object(raw).ada).lovelace);
    if (amount < 0n || amount > 0xffffffffffffffffn)
      throw new Error("History withdrawal amount is outside the ledger domain");
    if (ordered.insert(reward, amount) !== undefined)
      throw new Error("History transaction repeats a reward account");
  }
  const keys = ordered.keys();
  return Object.freeze(
    Array.from({ length: keys.len() }, (_, index) => {
      const account = keys.get(index);
      const script = account.payment().as_script();
      const key = account.payment().as_pub_key();
      if (script === undefined && key === undefined)
        throw new Error("History withdrawal has no credential");
      return Object.freeze({
        account: account.to_address().to_bech32(),
        networkId: account.network_id(),
        credential: Object.freeze(
          script === undefined
            ? { kind: "key" as const, hash: key!.to_hex() }
            : { kind: "script" as const, hash: script.to_hex() },
        ),
        amount: ordered.get(account)!,
      });
    }).sort(
      (a, b) =>
        a.networkId - b.networkId ||
        (a.credential.kind === b.credential.kind
          ? 0
          : a.credential.kind === "script"
            ? -1
            : 1) ||
        (a.credential.hash < b.credential.hash
          ? -1
          : a.credential.hash > b.credential.hash
            ? 1
            : 0),
    ),
  );
};

/** Decode the admitted source's lossless JSON transaction. This neither proves
 * a block canonical nor resolves historical inputs. The generation owner must
 * retain those authorities separately. A phase-2 failed transaction is marked
 * explicitly; its ordinary outputs and observer intents are never ledger effects.
 * This does not alter the independent forced-order carriage observation API. */
export const decodeHistoryChainTransaction = (
  value: unknown,
): HistoryChainTransaction => {
  const raw = object(value);
  const txHash = bytes(raw.id, 32);
  if (raw.spends !== "inputs" && raw.spends !== "collaterals")
    throw new Error(
      "History transaction is missing its ledger validity disposition",
    );
  const inputs = refs(raw.inputs);
  const references = refs(raw.references === undefined ? [] : raw.references);
  const collaterals = refs(
    raw.collaterals === undefined ? [] : raw.collaterals,
  );
  if (raw.spends === "collaterals" && collaterals.length === 0)
    throw new Error("Failed history transaction has no collateral inputs");
  // Ogmios v7 omits empty transaction outputs (unlike block transactions).
  // Explicit null or another malformed value is still invalid evidence.
  const rawOutputs = array(raw.outputs === undefined ? [] : raw.outputs);
  const output = (entry: unknown, index: number) => {
    const fields = object(entry);
    if (typeof fields.address !== "string" || fields.address.length === 0)
      throw new Error("History transaction output has no address");
    return decodeLedgerSnapshotOutput(
      { ...fields, transaction: { id: txHash }, index },
      new Set([fields.address]),
    );
  };
  const mint: Record<string, bigint> = {};
  const rawMint = object(raw.mint === undefined ? {} : raw.mint);
  for (const [policy, names] of Object.entries(rawMint)) {
    bytes(policy, 28);
    for (const [name, amount] of Object.entries(object(names))) {
      bytes(name);
      if (name.length > 64)
        throw new Error("History mint asset name exceeds 32 bytes");
      mint[policy + name] = integer(amount);
    }
  }
  const rewards = withdrawals(
    raw.withdrawals === undefined ? {} : raw.withdrawals,
  );
  const redeemers = array(raw.redeemers === undefined ? [] : raw.redeemers)
    .map((entry) => {
      const fields = object(entry);
      const pointer = object(fields.validator);
      if (!purposes.includes(pointer.purpose as Purpose))
        throw new Error("History transaction has an unknown redeemer purpose");
      const cbor = bytes(fields.redeemer);
      CML.PlutusData.from_cbor_hex(cbor);
      return Object.freeze({
        purpose: pointer.purpose as Purpose,
        index: natural(pointer.index),
        cbor,
      });
    })
    .sort(
      (a, b) =>
        purposes.indexOf(a.purpose) - purposes.indexOf(b.purpose) ||
        a.index - b.index,
    );
  for (let index = 0; index < redeemers.length; index++) {
    const current = redeemers[index]!;
    const previous = redeemers[index - 1];
    if (
      previous?.purpose === current.purpose &&
      previous.index === current.index
    )
      throw new Error("History transaction repeats a redeemer pointer");
    const length =
      current.purpose === "spend"
        ? inputs.length
        : current.purpose === "mint"
          ? Object.keys(rawMint).length
          : current.purpose === "withdraw"
            ? rewards.length
            : undefined;
    if (length !== undefined && current.index >= length)
      throw new Error("History redeemer pointer exceeds its ledger roster");
  }
  const interval = object(
    raw.validityInterval === undefined ? {} : raw.validityInterval,
  );
  return Object.freeze({
    txHash,
    spends: raw.spends,
    inputs,
    references,
    collaterals,
    outputs: Object.freeze(rawOutputs.map(output)),
    ...(raw.collateralReturn === undefined
      ? {}
      : { collateralReturn: output(raw.collateralReturn, rawOutputs.length) }),
    mint: Object.freeze(mint),
    withdrawals: rewards,
    redeemers: Object.freeze(redeemers),
    ...(interval.invalidBefore === undefined
      ? {}
      : { invalidBefore: natural(interval.invalidBefore) }),
    ...(interval.invalidAfter === undefined
      ? {}
      : { invalidAfter: natural(interval.invalidAfter) }),
  });
};

/** An observer is an actual zero withdrawal in a successful transaction. A
 * mint redeemer, key credential, or same-hash wrong-network account is not it. */
export const historyZeroWithdrawal = (
  transaction: HistoryChainTransaction,
  scriptHash: string,
  networkId: number,
) => {
  bytes(scriptHash, 28);
  if (transaction.spends !== "inputs") return null;
  const matches = transaction.withdrawals.flatMap((account, index) =>
    account.networkId === networkId &&
    account.credential.kind === "script" &&
    account.credential.hash === scriptHash
      ? [{ account, index }]
      : [],
  );
  if (matches.length !== 1 || matches[0]!.account.amount !== 0n) return null;
  const withdrawalIndex = matches[0]!.index;
  const redeemers = transaction.redeemers.filter(
    (entry) => entry.purpose === "withdraw" && entry.index === withdrawalIndex,
  );
  return redeemers.length === 1
    ? Object.freeze({ withdrawalIndex, redeemer: redeemers[0]! })
    : null;
};
