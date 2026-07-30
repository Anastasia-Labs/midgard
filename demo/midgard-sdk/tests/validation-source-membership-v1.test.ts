import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { Constr, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { ValidationSourceMembershipV1Schema } from "../src/index.js";

type ValidationSourceMembershipV1 = Data.Static<
  typeof ValidationSourceMembershipV1Schema
>;

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const digest = (hex: string): string =>
  Buffer.from(blake2b(Buffer.from(hex, "hex"), { dkLen: 32 })).toString("hex");

const forced: ValidationSourceMembershipV1 = {
  ForcedValidationSource: {
    membership: {
      domain: "ForcedTransactionsV1RootDomain",
      root: h32(0x11),
      phas_root: h32(0x12),
      count: 1n,
      key: {
        transactionId: h32(0x13),
        outputIndex: 4n,
      },
      value: {
        tx_id: h32(0x14),
        transaction_commitment: h32(0x15),
        source: {
          compact_cbor: "8101",
          witness_set_compact_cbor: "8102",
          field_preimage_lengths_cbor: "8103",
        },
        operator_validity: "FailedScript",
      },
      proof: [],
    },
  },
};

const normal: ValidationSourceMembershipV1 = {
  NormalValidationSource: {
    membership: {
      domain: "TransactionsV1RootDomain",
      root: h32(0x21),
      phas_root: h32(0x22),
      count: 1n,
      key: h32(0x23),
      value: {
        tx_id: h32(0x23),
        transaction_commitment: h32(0x24),
        source: {
          compact_cbor: "820102",
          witness_set_compact_cbor: "8104",
          field_preimage_lengths_cbor: "8105",
        },
      },
      proof: [],
    },
  },
};

const EXPECTED = {
  forcedCbor:
    "d8799fd8799fd87a80582011111111111111111111111111111111111111111111111111111111111111115820121212121212121212121212121212121212121212121212121212121212121201d8799f5820131313131313131313131313131313131313131313131313131313131313131304ffd8799f5820141414141414141414141414141414141414141414141414141414141414141458201515151515151515151515151515151515151515151515151515151515151515d8799f428101428102428103ffd87c80ff80ffff",
  normalCbor:
    "d87a9fd8799fd87b8058202121212121212121212121212121212121212121212121212121212121212121582022222222222222222222222222222222222222222222222222222222222222220158202323232323232323232323232323232323232323232323232323232323232323d8799f5820232323232323232323232323232323232323232323232323232323232323232358202424242424242424242424242424242424242424242424242424242424242424d8799f43820102428104428105ffff80ffff",
  corpusHash:
    "bf590321bb99709ee69e7eba470f1b087ed7e2de3aa823799e56122404462ede",
} as const;

const encodeExact = (value: ValidationSourceMembershipV1): string =>
  Data.to(value as never, ValidationSourceMembershipV1Schema as never);

const decodeExact = (cborHex: string): ValidationSourceMembershipV1 => {
  const decoded = Data.from(
    cborHex,
    ValidationSourceMembershipV1Schema as never,
  ) as unknown as ValidationSourceMembershipV1;
  if (encodeExact(decoded) !== cborHex) {
    throw new Error("ValidationSourceMembershipV1 CBOR is not canonical");
  }
  return decoded;
};

describe("ValidationSourceMembershipV1 ABI", () => {
  it("freezes forced/normal tags and exact nested membership proof vectors", () => {
    const forcedCbor = encodeExact(forced);
    const normalCbor = encodeExact(normal);
    const corpusCbor = Data.to([Data.from(forcedCbor), Data.from(normalCbor)]);
    expect({
      forcedCbor,
      normalCbor,
      corpusHash: digest(corpusCbor),
    }).toEqual(EXPECTED);
    expect(decodeExact(forcedCbor)).toEqual(forced);
    expect(decodeExact(normalCbor)).toEqual(normal);

    const forcedData = Data.from(forcedCbor);
    const normalData = Data.from(normalCbor);
    expect(forcedData).toBeInstanceOf(Constr);
    expect(normalData).toBeInstanceOf(Constr);
    expect([
      (forcedData as Constr<Data>).index,
      (forcedData as Constr<Data>).fields.length,
    ]).toEqual([0, 1]);
    expect([
      (normalData as Constr<Data>).index,
      (normalData as Constr<Data>).fields.length,
    ]).toEqual([1, 1]);
  });

  it("rejects adjacent tags, wrong arity, swapped kinds, and wrong nesting", () => {
    const forcedData = Data.from(encodeExact(forced)) as Constr<Data>;
    expect(() =>
      decodeExact(Data.to(new Constr(2, forcedData.fields))),
    ).toThrow();
    expect(() =>
      decodeExact(
        Data.to(new Constr(0, [...forcedData.fields, new Constr(0, [])])),
      ),
    ).toThrow();
    expect(() =>
      decodeExact(Data.to(new Constr(1, forcedData.fields))),
    ).toThrow();
    expect(() =>
      decodeExact(Data.to(new Constr(0, [new Constr(0, [])]))),
    ).toThrow();
    expect(() => decodeExact(`${encodeExact(forced)}00`)).toThrow();
  });

  it("contains no retired V2/V3 source-membership production identity", () => {
    const sources = [
      "../src/fraud-proof/validation-dispute.ts",
      "../../../onchain/aiken/lib/midgard/validation-claim-v1.ak",
    ].map((path) =>
      readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8"),
    );
    expect(
      sources.flatMap(
        (source) => source.match(/\bValidationSourceMembershipV[23]\b/gu) ?? [],
      ),
    ).toEqual([]);
  });
});
