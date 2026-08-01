import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { encodeCbor } from "../src/codec/cbor.js";
import {
  cardanoRedeemersToMidgardPreimageCbor,
  midgardRedeemersToCardano,
} from "../src/codec/native-redeemer.js";

const unaryConstructorDataCborHex = (depth: number): string =>
  "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

const spendRedeemerPreimage = (dataCborHex: string): Buffer =>
  encodeCbor([
    [
      CML.RedeemerTag.Spend,
      0n,
      Buffer.from(dataCborHex, "hex"),
      [1n, 2n],
    ],
  ]);

const buildDeepCmlUnaryData = (depth: number): CML.PlutusData => {
  let node = CML.PlutusData.new_integer(CML.BigInteger.from_str("0"));
  for (let level = 0; level < depth; level += 1) {
    const fields = CML.PlutusDataList.new();
    fields.add(node);
    node = CML.PlutusData.new_constr_plutus_data(
      CML.ConstrPlutusData.new(0n, fields),
    );
  }
  return node;
};

const wrapInSpendRedeemers = (data: CML.PlutusData): CML.Redeemers => {
  const redeemerMap = CML.MapRedeemerKeyToRedeemerVal.new();
  redeemerMap.insert(
    CML.RedeemerKey.new(CML.RedeemerTag.Spend, 0n),
    CML.RedeemerVal.new(data, CML.ExUnits.new(1n, 2n)),
  );
  return CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemerMap);
};

describe("redeemer Data validation without a CML probe", () => {
  it("normalizes a depth-4,043 Cardano redeemer into the exact Midgard preimage", () => {
    // The deep CML value is constructed iteratively — building never
    // recurses in wasm, only parsing does, and the admission direction
    // serializes rather than parses.
    const redeemers = wrapInSpendRedeemers(buildDeepCmlUnaryData(4_043));
    const preimage = cardanoRedeemersToMidgardPreimageCbor(redeemers);
    expect(preimage.toString("hex")).toBe(
      spendRedeemerPreimage(unaryConstructorDataCborHex(4_043)).toString(
        "hex",
      ),
    );
    // Measured 12.1 s on a 2-core CI runner (first-ever CI execution of
    // this suite); the default 5 s budget was calibrated on 32 cores.
  }, 60_000);

  it("validates deep Midgard redeemer entries without parsing them through CML", () => {
    // Depth 1,024 exercises the recursion-free entry validation and stays
    // inside CML's own ceiling (~1,522) for the unavoidable materialization
    // step of the reverse bridge, so the round-trip must be exact. Depths
    // beyond that ceiling still trap inside the reverse bridge's required
    // `CML.PlutusData.from_cbor_bytes` materialization (and a wasm trap
    // poisons the shared instance), so the reverse direction is exercised at
    // the maximum only in wasm-patched environments, never in this suite.
    const preimage = spendRedeemerPreimage(unaryConstructorDataCborHex(1_024));
    const redeemers = midgardRedeemersToCardano(preimage);
    expect(redeemers).toBeDefined();
    const roundTripped = cardanoRedeemersToMidgardPreimageCbor(redeemers);
    expect(roundTripped.toString("hex")).toBe(preimage.toString("hex"));
  });

  it("keeps the exact rejection classes for malformed redeemer Data", () => {
    const expectFailure = (dataCborHex: string, message: string): void => {
      expect(
        () => midgardRedeemersToCardano(spendRedeemerPreimage(dataCborHex)),
        dataCborHex,
      ).toThrow(message);
    };
    // domain violations reject at the well-formedness gate
    expectFailure("f93c00", "must contain one Plutus Data item");
    expectFailure("d8789f00ff", "must contain one Plutus Data item");
    expectFailure(`5841${"aa".repeat(65)}`, "must contain one Plutus Data item");
    expectFailure("d866824080", "must contain one Plutus Data item");
    expectFailure("0000", "must contain one Plutus Data item");
    expectFailure("d879", "must contain one Plutus Data item");
    // well-formed but noncanonical encodings reject at the canonicity gate
    expectFailure("5fff", "must contain canonical Plutus Data CBOR");
    expectFailure("8100", "must contain canonical Plutus Data CBOR");
    expectFailure("d8798100", "must contain canonical Plutus Data CBOR");
  });
});
