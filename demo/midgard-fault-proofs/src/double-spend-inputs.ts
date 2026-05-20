import { parseInteger } from "./json-file.js";

export const parseDoubleSpentInputIndex = ({
  value,
  inputCount,
  inputLabel,
}: {
  readonly value: string;
  readonly inputCount: number;
  readonly inputLabel: "tx1" | "tx2";
}): bigint => {
  const index = parseInteger(value, "--double-spent-input-index");
  if (index >= BigInt(inputCount)) {
    throw new Error(
      `--double-spent-input-index ${index.toString()} is out of bounds for ${inputCount.toString()} ${inputLabel} inputs.`,
    );
  }
  if (index > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(
      "--double-spent-input-index exceeds the safe integer range.",
    );
  }
  return index;
};
