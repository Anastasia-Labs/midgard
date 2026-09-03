import { readFileSync, writeFileSync } from "node:fs";

const [inputPath, outputPath] = process.argv.slice(2);
if (inputPath === undefined || outputPath === undefined) {
  throw new Error("expected input JSONL and output JSON paths");
}

const expectedLabels = [
  "balanced-nested-datum",
  "balanced-nested-redeemer",
  "maximum-constructor-datum-breadth",
  "maximum-constructor-redeemer-breadth",
  "maximum-inline-datum-blob",
  "maximum-list-datum-breadth",
  "maximum-list-redeemer-breadth",
  "maximum-map-datum-breadth",
  "maximum-map-redeemer-breadth",
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
const materialLabels = new Set([
  "balanced-nested-redeemer",
  "maximum-constructor-redeemer-breadth",
  "maximum-list-redeemer-breadth",
  "maximum-map-redeemer-breadth",
  "maximum-redeemers",
]);
const resolvedReferenceUtxoLabels = new Set(["maximum-reference-inputs"]);
const baseKeys = [
  "canonicalCborHex",
  "label",
  "admission",
  "transactionCommitmentHex",
  "transactionIdHex",
];
const canonicalHex = (value) =>
  typeof value === "string" && /^(?:[0-9a-f]{2})+$/.test(value);
const validResolvedReferenceUtxos = (value) => {
  if (!Array.isArray(value) || value.length === 0) {
    return false;
  }
  let previousKey;
  for (const entry of value) {
    if (
      !Array.isArray(entry) ||
      entry.length !== 2 ||
      !canonicalHex(entry[0]) ||
      !canonicalHex(entry[1]) ||
      (previousKey !== undefined && entry[0] <= previousKey)
    ) {
      return false;
    }
    previousKey = entry[0];
  }
  return true;
};
const entries = readFileSync(inputPath, "utf8")
  .trim()
  .split("\n")
  .map((line) => JSON.parse(line))
  .sort((left, right) => left.label.localeCompare(right.label));
const labels = entries.map(({ label }) => label);
if (JSON.stringify(labels) !== JSON.stringify(expectedLabels)) {
  throw new Error(`boundary corpus labels differ: ${JSON.stringify(labels)}`);
}
for (const entry of entries) {
  const expectedKeys = [
    ...baseKeys,
    ...(materialLabels.has(entry.label)
      ? ["canonicalMaterialSidecarCborHex", "sourceRawScriptAuditHash"]
      : []),
    ...(resolvedReferenceUtxoLabels.has(entry.label)
      ? ["resolvedReferenceUtxos"]
      : []),
  ].sort();
  const actualKeys = Object.keys(entry).sort();
  const expectedProductionAdmission =
    entry.label === "mixed-size-balanced"
      ? "diagnostic-synthetic-script-witnesses"
      : "required";
  if (
    JSON.stringify(actualKeys) !== JSON.stringify(expectedKeys) ||
    entry.admission !== expectedProductionAdmission ||
    !canonicalHex(entry.canonicalCborHex) ||
    !/^[0-9a-f]{64}$/.test(entry.transactionIdHex) ||
    !/^[0-9a-f]{64}$/.test(entry.transactionCommitmentHex) ||
    (materialLabels.has(entry.label) &&
      (!canonicalHex(entry.canonicalMaterialSidecarCborHex) ||
        !/^[0-9a-f]{56}$/.test(entry.sourceRawScriptAuditHash))) ||
    (resolvedReferenceUtxoLabels.has(entry.label) &&
      !validResolvedReferenceUtxos(entry.resolvedReferenceUtxos))
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
