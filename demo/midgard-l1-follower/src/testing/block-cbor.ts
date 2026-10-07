import { blake2b256 } from "../codec.js";
import type { OutRef, RedeemerPurpose } from "../types.js";
import * as c from "./cbor-writer.js";

/** One output of a simulated transaction (map form, no script reference). */
export type SimOutput = Readonly<{
  address: Buffer;
  lovelace: bigint;
  /** `{policyHex: {assetNameHex: quantity}}`. */
  assets?: ReadonlyMap<string, ReadonlyMap<string, bigint>>;
  /** An inline datum: the exact Plutus data CBOR. */
  datum?: Buffer;
}>;

/** One witness-set redeemer of a simulated transaction (no evaluation). */
export type SimRedeemer = Readonly<{
  purpose: RedeemerPurpose;
  index: number;
  /** The exact redeemer data CBOR. */
  data: Buffer;
}>;

/**
 * A simulated transaction as the ledger would carry it. Encoding is
 * deterministic, so the same spec always has the same id: a re-landed
 * transaction is the same spec in another block, and a changed field (such
 * as `invalidAfter`) is a different transaction spending the same inputs.
 */
export type SimTx = Readonly<{
  inputs: readonly OutRef[];
  referenceInputs?: readonly OutRef[];
  collaterals?: readonly OutRef[];
  outputs: readonly SimOutput[];
  collateralReturn?: SimOutput;
  /** Signed quantities; negative burns. */
  mint?: ReadonlyMap<string, ReadonlyMap<string, bigint>>;
  /** Reward withdrawals (body key 5), encoded in this order. */
  withdrawals?: readonly Readonly<{ rewardAccount: Buffer; amount: bigint }>[];
  invalidBefore?: number;
  invalidAfter?: number;
  /** False: the transaction failed phase 2 (listed in the block's invalid set). */
  isValid?: boolean;
  /**
   * Witness-set redeemers, in the Conway map form. As on chain, a transaction
   * with redeemers carries a `script_data_hash` (body key 11): here the
   * blake2b-256 of the redeemer map, a stand-in nothing evaluates.
   */
  redeemers?: readonly SimRedeemer[];
  /** Distinguishes otherwise equal transactions (the fee field). */
  nonce: number;
}>;

export type SimBlock = Readonly<{
  height: number;
  slot: number;
  /** Null only for a chain's first block. */
  prevHash: Buffer | null;
  /** Separates sibling blocks at one height (a fork's branch id). */
  branch: number;
  txs: readonly SimTx[];
}>;

export type EncodedBlock = Readonly<{
  raw: Buffer;
  hash: Buffer;
  /** Each transaction's id, in block order. */
  txHashes: readonly Buffer[];
}>;

const outRefSet = (outRefs: readonly OutRef[]): Buffer =>
  c.tag(
    258,
    c.array(
      ...[...outRefs]
        .sort((a, b) => Buffer.compare(a.txHash, b.txHash) || a.index - b.index)
        .map((outRef) => c.array(c.bytes(outRef.txHash), c.uint(outRef.index))),
    ),
  );

const multiAsset = (
  assets: ReadonlyMap<string, ReadonlyMap<string, bigint>>,
  signed: boolean,
): Buffer =>
  c.map(
    ...[...assets].map(([policy, names]): [Buffer, Buffer] => [
      c.bytes(Buffer.from(policy, "hex")),
      c.map(
        ...[...names].map(([name, quantity]): [Buffer, Buffer] => [
          c.bytes(Buffer.from(name, "hex")),
          signed && quantity < 0n ? c.nint(quantity) : c.uint(quantity),
        ]),
      ),
    ]),
  );

const output = (out: SimOutput): Buffer => {
  const fields: [Buffer, Buffer][] = [
    [c.uint(0), c.bytes(out.address)],
    [
      c.uint(1),
      out.assets === undefined || out.assets.size === 0
        ? c.uint(out.lovelace)
        : c.array(c.uint(out.lovelace), multiAsset(out.assets, false)),
    ],
  ];
  if (out.datum !== undefined)
    fields.push([c.uint(2), c.array(c.uint(1), c.tag(24, c.bytes(out.datum)))]);
  return c.map(...fields);
};

const REDEEMER_TAGS: Readonly<Record<RedeemerPurpose, number>> = {
  spend: 0,
  mint: 1,
  cert: 2,
  reward: 3,
  voting: 4,
  proposing: 5,
};

/** `{[tag, index] => [data, [mem, steps]]}`, the Conway redeemer map. */
const redeemerMap = (redeemers: readonly SimRedeemer[]): Buffer =>
  c.map(
    ...redeemers.map((redeemer): [Buffer, Buffer] => [
      c.array(c.uint(REDEEMER_TAGS[redeemer.purpose]), c.uint(redeemer.index)),
      c.array(redeemer.data, c.array(c.uint(0), c.uint(0))),
    ]),
  );

/** The exact body bytes of a simulated transaction; its id is their blake2b-256. */
export const encodeTxBody = (tx: SimTx): Buffer => {
  const fields: [Buffer, Buffer][] = [
    [c.uint(0), outRefSet(tx.inputs)],
    [c.uint(1), c.array(...tx.outputs.map(output))],
    [c.uint(2), c.uint(170_000 + tx.nonce)],
  ];
  if (tx.invalidAfter !== undefined)
    fields.push([c.uint(3), c.uint(tx.invalidAfter)]);
  if (tx.withdrawals !== undefined && tx.withdrawals.length > 0)
    fields.push([
      c.uint(5),
      c.map(
        ...tx.withdrawals.map((w): [Buffer, Buffer] => [
          c.bytes(w.rewardAccount),
          c.uint(w.amount),
        ]),
      ),
    ]);
  if (tx.invalidBefore !== undefined)
    fields.push([c.uint(8), c.uint(tx.invalidBefore)]);
  if (tx.mint !== undefined && tx.mint.size > 0)
    fields.push([c.uint(9), multiAsset(tx.mint, true)]);
  if (tx.redeemers !== undefined && tx.redeemers.length > 0)
    fields.push([c.uint(11), c.bytes(blake2b256(redeemerMap(tx.redeemers)))]);
  if (tx.collaterals !== undefined && tx.collaterals.length > 0)
    fields.push([c.uint(13), outRefSet(tx.collaterals)]);
  if (tx.collateralReturn !== undefined)
    fields.push([c.uint(16), output(tx.collateralReturn)]);
  if (tx.referenceInputs !== undefined && tx.referenceInputs.length > 0)
    fields.push([c.uint(18), outRefSet(tx.referenceInputs)]);
  return c.map(...fields);
};

export const simTxHash = (tx: SimTx): Buffer => blake2b256(encodeTxBody(tx));

/** The witness set: empty, or key 5 holding the redeemer map. */
export const encodeWitnessSet = (tx: SimTx): Buffer =>
  tx.redeemers === undefined || tx.redeemers.length === 0
    ? c.map()
    : c.map([c.uint(5), redeemerMap(tx.redeemers)]);

/**
 * A raw Conway-shaped block `[header, bodies, witnesses, aux, invalid]`
 * whose header body is `[blockNo, slot, prevHash, branch]`; the block hash
 * is blake2b-256 of the header, as the follower's decoder computes it.
 */
export const encodeBlock = (block: SimBlock): EncodedBlock => {
  const header = c.array(
    c.array(
      c.uint(block.height),
      c.uint(block.slot),
      block.prevHash === null ? c.nul : c.bytes(block.prevHash),
      c.uint(block.branch),
    ),
    c.bytes(Buffer.alloc(8)),
  );
  const bodies = block.txs.map(encodeTxBody);
  const invalid = block.txs.flatMap((tx, index) =>
    tx.isValid === false ? [c.uint(index)] : [],
  );
  const raw = c.array(
    header,
    c.array(...bodies),
    c.array(...block.txs.map(encodeWitnessSet)),
    c.map(),
    c.array(...invalid),
  );
  return {
    raw,
    hash: blake2b256(header),
    txHashes: bodies.map((body) => blake2b256(body)),
  };
};

/**
 * A ledger `utxo_by_address` answer, `{[txHash, index] => output}`, as the
 * node encodes it (for fake ledger-state readers).
 */
export const encodeUtxoAnswer = (
  utxos: readonly Readonly<{ outRef: OutRef; output: SimOutput }>[],
): Buffer =>
  c.map(
    ...utxos.map((utxo): [Buffer, Buffer] => [
      c.array(c.bytes(utxo.outRef.txHash), c.uint(utxo.outRef.index)),
      output(utxo.output),
    ]),
  );
