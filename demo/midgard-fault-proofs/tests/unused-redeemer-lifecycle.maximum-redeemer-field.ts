import { MidgardRedeemerTag } from "@al-ft/midgard-validation";
import { makeRedeemersCbor } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";

export const network = "Custom" as const;

/** Pads a selected canonical Data byte string to the exact field-8 carriage bound. */
export const redeemerItems = (
  spendCount: number,
): Parameters<typeof makeRedeemersCbor>[0] => [
  ...Array.from({ length: spendCount }, (_, index) => ({
    tag: MidgardRedeemerTag.Spend,
    index: BigInt(index),
  })),
  { tag: MidgardRedeemerTag.Mint, index: 0n },
];

export const maximumRedeemerField = (
  direction: "accepted" | "forced",
  spendCount: number,
) => {
  let padding = 32_000;
  for (let attempt = 0; attempt < 8; attempt++) {
    const data = Buffer.from(Data.to("a5".repeat(padding)), "hex");
    const selected = direction === "accepted" ? spendCount : spendCount - 1;
    const field = makeRedeemersCbor(
      redeemerItems(spendCount).map((item, index) => ({
        ...item,
        ...(index === selected ? { data } : {}),
      })),
    );
    if (field.length === 32_768) return field;
    padding += 32_768 - field.length;
  }
  throw new Error("Unable to construct exact maximum unused-redeemer field");
};
