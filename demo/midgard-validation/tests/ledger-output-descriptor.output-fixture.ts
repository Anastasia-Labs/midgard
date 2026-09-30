import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Data } from "@lucid-evolution/lucid";

export const protectedScriptAddress = Buffer.concat([
  Buffer.from([0x78]),
  Buffer.alloc(28, 0x11),
]);

export const outputFixture = (): {
  readonly output: MidgardTxOutput;
  readonly cbor: Buffer;
} => {
  const output: MidgardTxOutput = {
    address: protectedScriptAddress,
    value: {
      lovelace: 8_000_000n,
      assets: new Map([
        [
          "55".repeat(28),
          new Map([
            ["", 1n],
            ["0102", 42n],
          ]),
        ],
      ]),
    },
    datum: decodeMidgardDatum(Buffer.from(Data.to("ab".repeat(5_000)), "hex")),
    script_ref: {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(100, 0x6b),
    },
  };
  return { output, cbor: encodeMidgardTxOutput(output) };
};
