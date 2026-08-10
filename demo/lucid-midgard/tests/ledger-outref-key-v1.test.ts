import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  decodeMidgardSpendInputItemV1,
  encodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core/codec";
import { midgardOutRefToCbor } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import { outRefToCbor, utxoOutRefCbor } from "../src/core/output.js";

/**
 * `docs/spec/midgard-tx.md` §5.3: an out-ref has exactly one byte form, the
 * field-0/1 item `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` (fixed 38 bytes), and
 * that same value is the ledger MPF trie key and the ledger database `outref`
 * column. On-chain `ledger_outref_key`
 * (`onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak`) is a
 * direct call to `encode_midgard_tx_input`.
 *
 * These vectors are read from the generated golden fixture rather than written
 * down here, and that is the point: the identical `ledgerOutRefKeys` entries are
 * pinned on the Aiken side by `golden_ledger_outref_key_matches_typescript` in
 * `onchain/aiken/lib/midgard/native-tx-field-items-v1-golden.test.ak`. One
 * vector, both languages — so proving each TypeScript trie-key producer against
 * it proves it against `ledger_outref_key` too. Regenerate with
 * `pnpm --filter @al-ft/midgard-core run fixtures:native-tx-field-items-v1:sync`;
 * never edit the fixture by hand.
 */
type LedgerOutRefKeyVector = {
  readonly txIdHex: string;
  readonly outputIndex: number;
  readonly keyHex: string;
};

const GOLDEN_PATH = join(
  dirname(fileURLToPath(import.meta.url)),
  "..",
  "..",
  "midgard-core",
  "tests",
  "fixtures",
  "native-tx-field-items-v1.generated.json",
);

const vectors: readonly LedgerOutRefKeyVector[] = (
  JSON.parse(readFileSync(GOLDEN_PATH, "utf8")) as {
    readonly ledgerOutRefKeys: readonly LedgerOutRefKeyVector[];
  }
).ledgerOutRefKeys;

describe("ledger out-ref key (§5.3 field-0/1 item)", () => {
  it("covers both sides of the minimal-CBOR index boundary", () => {
    // The whole reason this encoding needs pinning is that a minimal-index
    // encoder agrees with it for some indices and not others. A vector set that
    // sat entirely on one side of 23/24 would pass against the old bytes too.
    expect(vectors.length).toBeGreaterThan(0);
    expect(vectors.some((vector) => vector.outputIndex <= 23)).toBe(true);
    expect(vectors.some((vector) => vector.outputIndex >= 24)).toBe(true);
  });

  it.each(vectors)(
    "derives the on-chain key for output index $outputIndex",
    ({ txIdHex, outputIndex, keyHex }) => {
      const expected = Buffer.from(keyHex, "hex");

      // Every item is 38 bytes: this is what makes stride-40 arithmetic access
      // sound, and it is where a minimal index (36 bytes for 0..23) diverges.
      expect(expected.length).toBe(38);

      // The three TypeScript spellings of the one value: the builder's out-ref
      // bytes, the UTxO accessor the field-0/1 preimage items go through, and
      // the ledger/DB producer in the validation package.
      expect(outRefToCbor({ txHash: txIdHex, outputIndex })).toEqual(expected);
      expect(utxoOutRefCbor({ txHash: txIdHex, outputIndex })).toEqual(
        expected,
      );
      expect(
        midgardOutRefToCbor({
          txId: Buffer.from(txIdHex, "hex"),
          index: BigInt(outputIndex),
        }),
      ).toEqual(expected);

      // And the core twin they all reach, so a future refactor that stops
      // delegating still has to produce these bytes.
      expect(
        encodeMidgardSpendInputItemV1({
          txId: Buffer.from(txIdHex, "hex"),
          outputIndex,
        }),
      ).toEqual(expected);
    },
  );

  it.each(vectors)(
    "round-trips the on-chain key at output index $outputIndex",
    ({ txIdHex, outputIndex, keyHex }) => {
      const decoded = decodeMidgardSpendInputItemV1(Buffer.from(keyHex, "hex"));
      expect(Buffer.from(decoded.txId).toString("hex")).toBe(txIdHex);
      expect(decoded.outputIndex).toBe(outputIndex);
    },
  );

  it("rejects the minimal-index spelling the old producers emitted", () => {
    // CML's TransactionInput CBOR for a low index: `82 5820 <32> <index>`, with
    // the index in one byte. Accepting it would mean two byte forms for one
    // ledger key, which is exactly what §6.1 forbids and what on-chain
    // `decode_midgard_tx_input_cbor` rejects at `byte_at(bytes, offset) == 25`.
    const txId = Buffer.alloc(32, 0x11);
    const minimal = Buffer.concat([
      Buffer.from([0x82, 0x58, 0x20]),
      txId,
      Buffer.from([0x07]),
    ]);
    expect(minimal.length).toBe(36);
    expect(() => decodeMidgardSpendInputItemV1(minimal)).toThrow();
  });

  it("refuses an output index outside the uint16 domain", () => {
    expect(() =>
      outRefToCbor({ txHash: "11".repeat(32), outputIndex: 65_536 }),
    ).toThrow();
  });
});
