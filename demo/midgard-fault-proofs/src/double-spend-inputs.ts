import { parseSafeNonNegativeInteger } from "./json-file.js";

export const parseDoubleSpentInputIndex = ({
  value,
  inputCount,
  inputLabel,
}: {
  readonly value: string;
  readonly inputCount: number;
  readonly inputLabel: "tx1" | "tx2";
}): bigint => {
  const index = parseSafeNonNegativeInteger(
    value,
    "--double-spent-input-index",
  );
  if (index >= BigInt(inputCount)) {
    throw new Error(
      `--double-spent-input-index ${index.toString()} is out of bounds for ${inputCount.toString()} ${inputLabel} inputs.`,
    );
  }
  return index;
};
