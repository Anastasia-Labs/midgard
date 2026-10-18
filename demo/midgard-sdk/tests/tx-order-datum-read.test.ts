/**
 * A tx order read exactly as the chain admits it. The spend and the proofs
 * cast the order datum's decoded Data to its type, whatever its encoding, and
 * a cast checks no hash length; the order id they key it by is that Data
 * re-serialised. So an order the SDK reads is one those casts accept, under
 * the id Aiken's `cbor.serialise` gives it: the canonical vectors are
 * `tx_order_v1_matches_the_canonical_typescript_abi_vectors` and
 * `forced_inclusion_key_value_matches_the_canonical_v1_vectors`
 * (`onchain/aiken/lib/midgard/user-events/tx-order-v1.test.ak`).
 */
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const POLICY = "ab".repeat(28);

// The vector datum, as Aiken serialises it.
const DATUM = `d8799fd8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffffff187b581c${"66".repeat(28)}d8799fd8799f581c${"77".repeat(28)}ffd87a80ffd87980ff`;
// Its order id (the forced-inclusion key) and payload, as Aiken serialises them.
const ID_KEY = `d8799f5820${"33".repeat(32)}04ff`;
const PAYLOAD = `d8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffff`;

const orderUtxo = (datum: string): UTxO => ({
  txHash: "99".repeat(32),
  outputIndex: 0,
  address: "addr_test1wq",
  assets: { lovelace: 3_000_000n, [`${POLICY}6f72646572`]: 1n },
  datum,
});

const read = (datum: string) =>
  Effect.runSync(SDK.utxosToTxOrderUTxOs([orderUtxo(datum)], POLICY));

const field = (path: readonly number[]) => plutusConstrFieldCbor(DATUM, path);

/** The datum's five fields under a definite-length header. */
const definiteFields = `d87985${[0, 1, 2, 3, 4].map((i) => field([i])).join("")}`;

const ENCODINGS = [
  {
    name: "an overlong inclusion time",
    datum: DATUM.replace("187b581c", "1b000000000000007b581c"),
  },
  { name: "definite-length datum fields", datum: definiteFields },
  {
    name: "an id with definite-length fields and an overlong index",
    datum: replacePlutusConstrFieldCbor(
      DATUM,
      [0, 0],
      `d87982${field([0, 0, 0])}1a00000004`,
    ),
  },
  {
    name: "a payload with definite-length fields",
    datum: replacePlutusConstrFieldCbor(
      DATUM,
      [0, 1],
      `d87983${[0, 1, 2].map((i) => field([0, 1, i])).join("")}`,
    ),
  },
  {
    name: "an id transaction hash in uneven chunks",
    datum: replacePlutusConstrFieldCbor(
      DATUM,
      [0, 0, 0],
      `5f4b${"33".repeat(11)}55${"33".repeat(21)}ff`,
    ),
  },
  {
    name: "a witness in chunks",
    datum: replacePlutusConstrFieldCbor(
      DATUM,
      [2],
      `5f4e${"66".repeat(14)}4e${"66".repeat(14)}ff`,
    ),
  },
  {
    name: "an empty refund datum as an indefinite-length list",
    datum: replacePlutusConstrFieldCbor(DATUM, [4], "d8799fff"),
  },
] as const;

describe("tx-order datum read as the chain reads it", () => {
  it("reads the vector datum under Aiken's id and payload", () => {
    const [order, ...more] = read(DATUM);
    expect(more).toEqual([]);
    expect(Buffer.from(order!.idCbor).toString("hex")).toBe(ID_KEY);
    expect(Buffer.from(order!.infoCbor).toString("hex")).toBe(PAYLOAD);
  });

  it.each(ENCODINGS)(
    "reads an order datum with $name as the vector, under the same id",
    ({ datum }) => {
      expect(datum).not.toBe(DATUM);
      const [order, ...more] = read(datum);
      expect(more).toEqual([]);
      expect(order!.datum).toEqual(read(DATUM)[0]!.datum);
      expect(order!.utxo.datum).toBe(datum);
      expect(Buffer.from(order!.idCbor).toString("hex")).toBe(ID_KEY);
      expect(Buffer.from(order!.infoCbor).toString("hex")).toBe(PAYLOAD);
      expect(SDK.decodeTxOrderDatumCbor(Buffer.from(datum, "hex"))).toEqual(
        order!.datum,
      );
    },
  );

  it.each([
    {
      name: "a 27-byte payment key hash",
      path: [3, 0, 0],
      to: `581b${"77".repeat(27)}`,
    },
    { name: "an empty payment key hash", path: [3, 0, 0], to: "40" },
    {
      name: "a 29-byte inline stake script hash",
      path: [3, 1],
      to: `d8799fd8799fd87a9f581d${"88".repeat(29)}ffffff`,
    },
  ])(
    "reads a refund address with $name, which no cast length-checks",
    ({ path, to }) => {
      const [order] = read(replacePlutusConstrFieldCbor(DATUM, path, to));
      expect(Buffer.from(order!.idCbor).toString("hex")).toBe(ID_KEY);
    },
  );

  it.each([
    { name: "another outer constructor", datum: `d87a${DATUM.slice(4)}` },
    { name: "an extra trailing field", datum: `${DATUM.slice(0, -2)}00ff` },
    {
      name: "no refund datum",
      datum: `d8799f${[0, 1, 2, 3].map((i) => field([i])).join("")}ff`,
    },
    {
      name: "a payload of another constructor",
      datum: DATUM.replace(
        `d8799f5820${"44".repeat(32)}`,
        `d87a9f5820${"44".repeat(32)}`,
      ),
    },
  ])(
    "refuses an order datum with $name, as the order casts do",
    ({ datum }) => {
      expect(read(datum)).toEqual([]);
      expect(() =>
        SDK.decodeTxOrderDatumCbor(Buffer.from(datum, "hex")),
      ).toThrow();
      const reading = Effect.runSync(
        Effect.either(SDK.utxoToTxOrderUTxO(orderUtxo(datum), POLICY)),
      );
      expect(reading).toMatchObject({
        _tag: "Left",
        left: { _tag: "DataCoercionError" },
      });
    },
  );

  it("encodes the id key as Aiken serialises it, whatever shape the id has", () => {
    // Aiken's `cbor.serialise` of each id (v1.1.24): an integer past 64 bits
    // is a tag-2 bignum with no leading zero, and a bytestring past 64 bytes
    // (which a datum carries in chunks of at most 64) is indefinite, in
    // 64-byte chunks.
    const idKey = (id: string) =>
      Buffer.from(
        SDK.userEventCborFieldsFromInlineDatum(
          orderUtxo(replacePlutusConstrFieldCbor(DATUM, [0, 0], id)),
        ).idCbor,
      ).toString("hex");
    const hash = "5820" + "33".repeat(32);
    const bignum = `d8799f${hash}c249400000000000000000ff`;
    expect(idKey(`d8799f${hash}c249400000000000000000ff`)).toBe(bignum);
    expect(idKey(`d8799f${hash}c24a00400000000000000000ff`)).toBe(bignum);
    expect(idKey(`d87982${hash}1b0000000000000004`)).toBe(ID_KEY);
    const long = `d8799f5f5840${"ab".repeat(64)}46${"ab".repeat(6)}ff04ff`;
    expect(
      idKey(`d8799f5f5821${"ab".repeat(33)}5825${"ab".repeat(37)}ff04ff`),
    ).toBe(long);
  });

  it("keeps the refusals of the strict datum schema on what it encodes", () => {
    expect(() =>
      Data.from(
        replacePlutusConstrFieldCbor(
          DATUM,
          [3, 0, 0],
          `581b${"77".repeat(27)}`,
        ),
        SDK.TxOrderDatum,
      ),
    ).toThrow();
  });
});
