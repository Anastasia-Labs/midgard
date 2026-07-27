import { readFileSync, writeFileSync } from "node:fs";

const [inputPath, outputPath] = process.argv.slice(2);
if (inputPath === undefined || outputPath === undefined) {
  throw new Error("expected input JSONL and output JSON paths");
}

const expectedLabels = [
  "balanced-nested-datum",
  "balanced-nested-redeemer",
  "maximum-inline-datum-blob",
  "maximum-mint-and-native-policies",
  "maximum-nested-value",
  "maximum-observers-and-native-scripts",
  "maximum-outputs",
  "maximum-redeemers",
  "maximum-reference-inputs",
  "maximum-signers-and-witnesses",
  "maximum-spend-inputs",
  "mixed-size-balanced",
];
const entries = readFileSync(inputPath, "utf8")
  .trim()
  .split("\n")
  .map((line) => JSON.parse(line))
  .sort((left, right) => left.label.localeCompare(right.label));
const labels = entries.map(({ label }) => label);
if (JSON.stringify(labels) !== JSON.stringify(expectedLabels)) {
  throw new Error(
    `boundary corpus labels differ: ${JSON.stringify(labels)}`,
  );
}
for (const entry of entries) {
  if (
    !/^[0-9a-f]+$/.test(entry.canonicalCborHex) ||
    entry.canonicalCborHex.length % 2 !== 0 ||
    !/^[0-9a-f]{64}$/.test(entry.transactionIdHex) ||
    !/^[0-9a-f]{64}$/.test(entry.transactionCommitmentHex)
  ) {
    throw new Error(`invalid corpus row ${entry.label}`);
  }
}
writeFileSync(
  outputPath,
  `${JSON.stringify(
    {
      schema: "midgard-cardano-capability-p2-boundary-corpus-v1",
      entries,
    },
    null,
    2,
  )}\n`,
  "utf8",
);
