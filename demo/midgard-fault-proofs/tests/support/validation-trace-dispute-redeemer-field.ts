/** A failing transaction's redeemer sized so the transaction's redeemer
 * field preimage is a given length: the field a canonical-decode step's
 * observe stage opens whole. */
import { MidgardRedeemerTag } from "@al-ft/midgard-validation";
import { makeRedeemersCbor } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Constr, Data } from "@lucid-evolution/lucid";

const fieldLength = (data: Buffer, exUnits?: readonly [bigint, bigint]) =>
  makeRedeemersCbor([
    {
      tag: MidgardRedeemerTag.Spend,
      index: 0n,
      data,
      ...(exUnits === undefined ? {} : { exUnits }),
    },
  ]).length;

/** Redeemer data (Plutus Data CBOR) under which the transaction's redeemer
 * field preimage is exactly `bytes` long, its spend declaring `exUnits`
 * when given and the default units otherwise. */
export const redeemerDataForFieldPreimage = (
  bytes: number,
  exUnits?: readonly [bigint, bigint],
): Buffer => {
  for (let payload = bytes; payload >= 0; payload -= 1)
    for (const tail of [[], [0n]] as const) {
      const data = Buffer.from(
        Data.to(new Constr(0, ["ab".repeat(payload), ...tail])),
        "hex",
      );
      if (fieldLength(data, exUnits) === bytes) return data;
    }
  throw new Error(`no redeemer gives a ${bytes.toString()}-byte field`);
};
