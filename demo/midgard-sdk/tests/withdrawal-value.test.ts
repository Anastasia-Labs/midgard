/** The `l2_value` bytes the SDK and CLI write into a withdrawal body hold
 * policies and asset names in bytewise order, the order Aiken's
 * `assets.from_asset_list` requires of asset names at admission, whatever
 * order the in-memory value lists them in (the caller's assets, or a ledger
 * output's names in the Midgard L2 codec's length-first order). The encoder
 * (Lucid's `Data.to`, over CML) writes map entries sorted by key; nothing in
 * the SDK sorts them, so this pins the encoder behaviour admission relies on.
 * `Data.from` keeps the order the bytes hold. */
import {
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { Value } from "../src/common.js";
import { assetsToValue } from "../src/reserve-payout/assets.js";

const P = "0a".repeat(28);
const Q = "ab".repeat(28);

/** The policies and names in the order the encoded bytes hold them. */
const encodedOrder = (value: Value) =>
  [...(Data.from(Data.to(value, Value), Value) as Value).entries()].map(
    ([policy, names]) => [policy, [...names.keys()]],
  );

describe("withdrawal l2_value byte order", () => {
  it("as submit-withdrawal writes it, from assets in any order", () => {
    const value = assetsToValue({
      [`${Q}01`]: 1n,
      [`${P}ff`]: 1n,
      [`${P}0000`]: 2n,
      lovelace: 7_000_000n,
    });
    expect(encodedOrder(value)).toEqual([
      ["", [""]],
      [P, ["0000", "ff"]],
      [Q, ["01"]],
    ]);
  });

  it("from a ledger output's names in the L2 codec's order", () => {
    const { value } = decodeMidgardTxOutput(
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
        value: {
          lovelace: 7_000_000n,
          assets: new Map([
            [
              P,
              new Map([
                ["0000", 2n],
                ["ff", 1n],
              ]),
            ],
          ]),
        },
      }),
    );
    expect([...value.assets.get(P)!.keys()]).toEqual(["ff", "0000"]);
    // As the devnet journey's withdrawal body builds it.
    expect(
      encodedOrder(
        new Map([
          ["", new Map([["", value.lovelace]])],
          ...Array.from(
            value.assets,
            ([policy, assets]) => [policy, new Map(assets)] as const,
          ),
        ]),
      ),
    ).toEqual([
      ["", [""]],
      [P, ["0000", "ff"]],
    ]);
  });
});
