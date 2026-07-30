import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");
const evidencePath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-fault-proof-reconciliation-v1.json",
);
const evidence = JSON.parse(await readFile(evidencePath, "utf8"));
const matrixPath = resolve(repositoryRoot, evidence.source.path);
const matrixBytes = await readFile(matrixPath);
const matrixLines = matrixBytes.toString("utf8").split(/\r?\n/u);
const sha256 = createHash("sha256").update(matrixBytes).digest("hex");

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-fault-proof-reconciliation.v1",
);
assert.equal(sha256, evidence.source.sha256, "coverage matrix bytes drifted");
assert.deepEqual(evidence.summary, {
  total: 70,
  coverageRows: 61,
  structuralRows: 9,
  locallyComplete: 4,
  structuralOrNA: 12,
  open: 54,
  preprodComplete: 0,
  focusedChecksPassed: 44,
  focusedChecksTotal: 45,
});

const allRows = [...evidence.coverageRows, ...evidence.structuralRows];
assert.equal(new Set(allRows).size, 70, "row identifiers must be unique");
assert.equal(
  evidence.locallyCompleteRows.length +
    evidence.structuralOrNARows.length +
    evidence.openRows.length,
  70,
  "every row must have exactly one disposition",
);
assert.deepEqual(
  [
    ...new Set([
      ...evidence.locallyCompleteRows,
      ...evidence.structuralOrNARows,
      ...evidence.openRows,
    ]),
  ].sort((left, right) => left - right),
  [...allRows].sort((left, right) => left - right),
  "dispositions must cover the reconciled row set",
);

for (const lineNumber of evidence.coverageRows) {
  const row = matrixLines[lineNumber - 1];
  assert.ok(
    row?.startsWith("| "),
    `missing coverage row at line ${lineNumber}`,
  );
  const requiredColumns = lineNumber >= 182 ? 7 : 11;
  assert.ok(
    row.split("|").length >= requiredColumns,
    `coverage row ${lineNumber} lost required reconciliation columns`,
  );
}

for (const lineNumber of evidence.structuralRows) {
  const row = matrixLines[lineNumber - 1];
  assert.ok(
    row?.startsWith("| "),
    `missing structural row at line ${lineNumber}`,
  );
  assert.ok(
    row.split("|").length >= 5,
    `structural row ${lineNumber} lost its executable-evidence contract`,
  );
}

assert.deepEqual(evidence.locallyCompleteRows, [94, 96, 137, 182]);
assert.deepEqual(
  evidence.structuralRows,
  [294, 295, 296, 297, 298, 299, 300, 301, 302],
);
assert.equal(evidence.bindingInventory.nativeV1Families.length, 3);
assert.equal(evidence.bindingInventory.legacyPlutusDataFamilies.length, 8);
assert.deepEqual(evidence.structuralAudit.summary, {
  rows: 9,
  pass: 8,
  partial: 1,
  open: 0,
});
const structuralAuditRows = Object.entries(evidence.structuralAudit.rows);
assert.equal(structuralAuditRows.length, 9);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "PASS").length,
  8,
);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "PARTIAL").length,
  1,
);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "OPEN").length,
  0,
);
for (const [rowId, row] of structuralAuditRows) {
  if (row.disposition === "PASS") {
    assert.ok(
      ["L294", "L295", "L296", "L298", "L299", "L300", "L301", "L302"].includes(
        rowId,
      ),
    );
    assert.equal(row.remainingTask, null);
  } else {
    assert.match(row.remainingTask, /^Q49-L\d{3}$/u);
  }
}
const [
  phaseBSource,
  phaseBTestSource,
  depositTypeSource,
  depositValidatorSource,
  depositSdkSource,
  blueprintSource,
] = await Promise.all([
  readFile(
    resolve(repositoryRoot, "demo/midgard-validation/src/phase-b.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-validation/tests/phase-b.test.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/lib/midgard/user-events/deposit.ak"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/validators/user-events/deposit.ak"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-sdk/src/user-events/deposit.ts"),
    "utf8",
  ),
  readFile(resolve(repositoryRoot, "onchain/aiken/plutus.json"), "utf8"),
]);
for (const requiredFeeSymbol of [
  "valuePreservationDelta(",
  "ledgerTx.fee",
  "RejectCodes.ValueNotPreserved",
  '"valueAndMint"',
]) {
  assert.ok(
    phaseBSource.includes(requiredFeeSymbol),
    `L298 production fee invariant lost ${requiredFeeSymbol}`,
  );
}
assert.ok(
  phaseBTestSource.includes(
    "burns the exact L2 fee in production value accounting and rejects fee redirection",
  ),
  "L298 exact fee-burn control is absent",
);
for (const source of [
  depositTypeSource,
  depositValidatorSource,
  depositSdkSource,
]) {
  assert.doesNotMatch(source, /\bRefund\b/u, "L302 deposit refund path exists");
}
for (const requiredDepositGuard of [
  "singular_utxo_indexer.one_to_one(",
  "BurnEventNFT",
  "settlement.valid_counted_membership(",
  "output_datum == NoDatum",
  "output_address == reserve_addr",
]) {
  assert.ok(
    depositValidatorSource.includes(requiredDepositGuard),
    `L302 deposit absorption guard lost ${requiredDepositGuard}`,
  );
}
assert.ok(
  depositSdkSource.includes(
    "export const DepositSpendRedeemerSchema = Data.Object({",
  ),
  "L302 SDK deposit spend schema is not one exact record",
);
const blueprint = JSON.parse(blueprintSource);
const depositSpendSchema =
  blueprint.definitions?.["midgard/user_events/deposit/SpendRedeemer"];
assert.equal(depositSpendSchema?.anyOf?.length, 1);
assert.equal(depositSpendSchema.anyOf[0]?.index, 0);
assert.deepEqual(
  depositSpendSchema.anyOf[0]?.fields?.map((field) => field.title),
  [
    "input_index",
    "output_index",
    "hub_ref_input_index",
    "settlement_ref_input_index",
    "mint_redeemer_index",
    "membership_proof",
    "inclusion_proof_script_withdraw_redeemer_index",
  ],
);
assert.equal(evidence.structuralAudit.adjacentRegression.status, "PASS");
assert.equal(evidence.acceptance.F20, "PASS");
assert.equal(evidence.acceptance.F21, "PASS");
assert.equal(evidence.acceptance.QG1, "OPEN");
assert.equal(evidence.acceptance.QG2, "OPEN");
assert.equal(evidence.acceptance.QG3, "OPEN");

for (const finding of evidence.criticalFindings) {
  assert.ok(
    finding.remainingTasks.length > 0,
    `${finding.id} has no closure task`,
  );
  for (const source of finding.sources) {
    await readFile(resolve(repositoryRoot, source));
  }
}

console.log(
  `canonical V1 fault-proof reconciliation verified: ${allRows.length} rows, ${evidence.openRows.length} open`,
);
