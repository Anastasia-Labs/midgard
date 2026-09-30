import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";

/**
 * A local L1 observation surface: one Ogmios chain-sync endpoint and one Kupo
 * endpoint, both serving a chain built out of **real signed transactions**.
 *
 * This is not a fixture of an observation. Every field either comes off the
 * transaction's own CBOR — the transaction id, its reference inputs in the order
 * the ledger serialised them, its minted policies, its redeemers with the
 * validator pointers the ledger assigned — or is chain metadata (slots, header
 * hashes) that an emulated ledger has no opinion about and this file therefore
 * has to name. Nothing that the reader authenticates against is authored here.
 *
 * The two protocols are implemented rather than mocked, because the thing under
 * test is a read: the node opens a real WebSocket, speaks Ogmios's JSON-RPC
 * chain-sync (`findIntersection`, then `nextBlock` forward), and fetches Kupo's
 * `/matches/{index}@{id}` and `/checkpoints/{slot}` over HTTP. A stub at the
 * function boundary would leave exactly the layer this ticket adds untested.
 *
 * **Fidelity notes, so a reader knows what this does and does not prove.**
 * - Transaction JSON follows Ogmios' v6/v7 schema (`redeemers` as an array of
 *   `{redeemer, executionUnits, validator: {purpose, index}}`, `references` as
 *   `{transaction: {id}, index}`); `docker-compose.kupmios.yaml` pins
 *   v7.0.0, which kept those shapes and only stopped emitting empty
 *   `validityInterval`/`outputs` fields. `cbor` is deliberately absent: a
 *   default Ogmios omits it unless started with `--include-transaction-cbor`.
 * - Kupo match records are the **v2.11.0** `Match` shape, which is what
 *   `docker-compose.kupmios.yaml` pins: `additionalProperties: false` over
 *   `transaction_index, transaction_id, output_index, address, value, datum_hash,
 *   datum, datum_type, script_hash, script, created_at, spent_at`. `datum_type` is
 *   *omitted* rather than nulled for an output with no datum, because the schema
 *   says it "is only present when `datum_hash` is not `null`".
 * - **`datum` and `script` are served if and only if the request carries the bare
 *   `?resolve_hashes` flag**, which is the v2.10.0 contract the deployment floor
 *   now buys: those two fields are "only and always present (yet may be `null`) if
 *   `?resolve_hashes` was set". *Only* — a flagless request gets a match with
 *   neither key, so a reader that forgot the flag cannot be greened by a harness
 *   that volunteers the bytes anyway. *Always* — with the flag, `datum` is present
 *   on every match including outputs that never had one, where it is `null`. A
 *   `null` under the flag is also Kupo's answer for a datum whose bytes it does
 *   not hold, and the reader has to survive both.
 *   {@link LocalL1.ignoreResolveHashes} serves the flag-ignoring shape a Kupo
 *   below the floor answers with, so the deployment-mismatch refusal is testable.
 * - The datum hash is content-derived here rather than Blake2b-256: nothing
 *   authenticates it, it is only the key joining a match to the datum store the
 *   `?resolve_hashes` join reads, and every byte it leads to still terminates in
 *   the §4 hash door.
 * - `/checkpoints/{slot}` answers with the most recent checkpoint **before or at**
 *   the requested slot, which is Kupo's documented flexible lookup and the whole
 *   reason the point-fetch can find an ancestor to intersect at. Blocks are
 *   spaced several slots apart here so that lookup has to do that work rather
 *   than hit an exact match.
 * - Header hashes are content-derived from the block's transaction ids. An
 *   emulated ledger produces no headers, and the reader only ever compares them
 *   for equality against the one Kupo reported.
 * - **`spent_at` is not modelled.** A match is served with `spent_at: null`
 *   while its output is unspent, and once a later block spends it the match
 *   answers HTTP 501 instead. What Kupo puts in a spent match's `spent_at` —
 *   v2.11.0 mirrors `input_index` and the redeemer read at it — is recorded off
 *   a live index in `@al-ft/midgard-test-support/l1-recordings` and tested
 *   against those recordings (`kupo-spent-at.test.ts`), so a reader of spends is
 *   never greened by this file's belief about them.
 */

export const WEBSOCKET_GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

/** Slots between consecutive blocks, so checkpoint lookup is never exact. */
export const SLOTS_PER_BLOCK = 20;

export type LocalL1Point = {
  readonly slot: number;
  readonly headerHash: string;
};

export type LocalL1Block = LocalL1Point & {
  readonly height: number;
  readonly transactions: readonly unknown[];
};

export type LocalL1 = {
  /** Value for `L1_OGMIOS_KEY`. */
  readonly ogmiosUrl: string;
  /** Value for `L1_KUPO_KEY`. */
  readonly kupoUrl: string;
  /** Appends one block carrying the given signed transactions. */
  readonly appendBlock: (transactionCbors: readonly string[]) => LocalL1Block;
  /**
   * Rewrites the datum the `?resolve_hashes` join returns on a match, for the
   * negatives that prove the read stays fail-closed. The rewrite is handed the
   * real datum and returns either replacement base16 or `null`, which serves
   * Kupo's answer for a datum it does not hold: the `datum` field present, as the
   * flag always makes it, and `null`. Passing `null` restores honest answers.
   */
  readonly rewriteDatums: (
    rewrite: ((datum: string) => string | null) | null,
  ) => void;
  /**
   * Serves matches the way a Kupo **below the v2.10.0 deployment floor** does:
   * `?resolve_hashes` is an unknown query flag, so it is silently ignored rather
   * than rejected and the match comes back with no `datum` field at all. This is
   * the deployment-mismatch shape, and the read must refuse it loudly instead of
   * reading a missing field as an output that carries no carriage.
   */
  readonly ignoreResolveHashes: (ignore: boolean) => void;
  readonly close: () => Promise<void>;
};

/**
 * A match as Kupo serves it **without** `?resolve_hashes` — the schema's required
 * properties and nothing else. `datum` and `script` are not optional members of
 * this type on purpose: they exist only on the resolved shape below.
 */
export type KupoMatch = {
  readonly transaction_index: number;
  readonly transaction_id: string;
  readonly output_index: number;
  readonly address: string;
  readonly value: { readonly coins: string; readonly assets: object };
  readonly datum_hash: string | null;
  /** Only present when `datum_hash` is not `null`, as the schema states. */
  readonly datum_type?: "hash" | "inline";
  readonly script_hash: string | null;
  readonly created_at: {
    readonly slot_no: number;
    readonly header_hash: string;
  };
  /** Always `null`: a spent match is not served at all. */
  readonly spent_at: null;
};

/** The same match under `?resolve_hashes`: both joins present, either may be null. */
export type KupoResolvedMatch = KupoMatch & {
  readonly datum: string | null;
  readonly script: null;
};

/** The key joining a match to its bytes. Not Blake2b-256; nothing checks it. */
export const datumHashOf = (datumCbor: string): string =>
  createHash("sha256").update(datumCbor).digest("hex");

const redeemerPurpose = (tag: CML.RedeemerTag): string => {
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
      throw new Error(`unsupported redeemer tag ${String(tag)}`);
  }
};

type ObservedRedeemer = {
  readonly redeemer: string;
  readonly executionUnits: { readonly memory: number; readonly cpu: number };
  readonly validator: { readonly purpose: string; readonly index: number };
};

export const transactionRedeemers = (
  witnessSet: CML.TransactionWitnessSet,
): readonly ObservedRedeemer[] => {
  const redeemers = witnessSet.redeemers();
  if (redeemers === undefined) {
    return [];
  }
  const collected: ObservedRedeemer[] = [];
  const push = (
    tag: CML.RedeemerTag,
    index: bigint,
    data: CML.PlutusData,
    exUnits: CML.ExUnits,
  ): void => {
    collected.push({
      redeemer: data.to_cbor_hex(),
      executionUnits: {
        memory: Number(exUnits.mem()),
        cpu: Number(exUnits.steps()),
      },
      validator: { purpose: redeemerPurpose(tag), index: Number(index) },
    });
  };
  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      push(
        redeemer.tag(),
        redeemer.index(),
        redeemer.data(),
        redeemer.ex_units(),
      );
    }
    return collected;
  }
  const mapped = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (mapped === undefined) {
    throw new Error("witness set carries an unreadable redeemer map");
  }
  const keys = mapped.keys();
  for (let index = 0; index < keys.len(); index += 1) {
    const key = keys.get(index);
    const value = mapped.get(key);
    if (value === undefined) {
      throw new Error("witness set redeemer map is missing a value");
    }
    push(key.tag(), key.index(), value.data(), value.ex_units());
  }
  return collected;
};

export const transactionMint = (
  body: CML.TransactionBody,
): Record<string, Record<string, string>> => {
  const mint = body.mint();
  const assets: Record<string, Record<string, string>> = {};
  if (mint === undefined) {
    return assets;
  }
  const policies = mint.keys();
  for (let index = 0; index < policies.len(); index += 1) {
    const policy = policies.get(index);
    const minted = mint.get_assets(policy);
    if (minted === undefined) {
      continue;
    }
    const names = minted.keys();
    const byName: Record<string, string> = {};
    for (let nameIndex = 0; nameIndex < names.len(); nameIndex += 1) {
      const name = names.get(nameIndex);
      byName[name.to_hex()] = (minted.get(name) ?? 0n).toString();
    }
    assets[policy.to_hex()] = byName;
  }
  return assets;
};

export const outRefsOf = (
  inputs: CML.TransactionInputList | undefined,
): readonly { transaction: { id: string }; index: number }[] => {
  if (inputs === undefined) {
    return [];
  }
  const collected: { transaction: { id: string }; index: number }[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    collected.push({
      transaction: { id: input.transaction_id().to_hex() },
      index: Number(input.index()),
    });
  }
  return collected;
};

export type DecodedTransaction = {
  readonly txHash: string;
  readonly json: unknown;
  /** Spent inputs, in the transaction's own CBOR order. */
  readonly inputs: readonly { transaction: { id: string }; index: number }[];
  readonly outputs: readonly {
    readonly address: string;
    readonly datum: string | null;
  }[];
};
