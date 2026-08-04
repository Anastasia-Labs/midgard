import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
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

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-fault-proof-reconciliation.v1",
);
assert.deepEqual(evidence.summary, {
  total: 70,
  coverageRows: 61,
  structuralRows: 9,
  locallyComplete: 5,
  structuralOrNA: 12,
  open: 53,
  preprodComplete: 0,
  focusedChecksPassed: 45,
  focusedChecksTotal: 45,
  focusedCheckName: "family-scaffold-v1.test.ts strict scanner",
  focusedCommand:
    "pnpm --dir demo/midgard-fault-proofs exec vitest run tests/family-scaffold-v1.test.ts",
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

assert.deepEqual(evidence.locallyCompleteRows, [94, 95, 96, 137, 182]);
assert.deepEqual(
  evidence.structuralRows,
  [295, 296, 297, 298, 299, 300, 301, 302, 303],
);
assert.deepEqual(evidence.initialLaunchScope, {
  sourceAnchor:
    "GOAL_SPEC.md §9.1 and §9.3; current deployed positional catalogue",
  status: "INITIAL_ONLY",
  families: [
    "double-spend",
    "no-input",
    "input-no-idx",
    "invalid-range",
    "transition-trace",
    "zero-input",
    "validation-trace-dispute",
    "da-hash-preimage",
  ],
  count: 8,
  note: "This is F20's concrete initial §9.1 list, not a final launch decision or a LOCAL_PASS/LIVE_PASS promotion. Q50/Q55 own final route and enabled-state integration.",
});
assert.deepEqual(evidence.taskResidues, {
  completedPrerequisites: ["Q00", "Q02", "Q03", "Q24", "Q25", "Q44", "Q54"],
  completedFamilyLifecycle: ["Q13"],
  openFamilyClosures: ["Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20"],
  openStructural: ["Q49-L298", "Q49-L302"],
  openLifecycleAndAcceptance: [
    "Q50",
    "Q51",
    "Q52",
    "Q53",
    "Q55",
    "Q56",
    "Q57",
    "Q58",
    "Q59",
    "Q60",
    "Q61",
    "Q62",
    "Q63",
    "QG1",
    "QG2",
    "QG3",
  ],
});
assert.deepEqual(evidence.structuralAudit.summary, {
  rows: 9,
  pass: 7,
  partial: 2,
  open: 0,
});
const structuralAuditRows = Object.entries(evidence.structuralAudit.rows);
assert.equal(structuralAuditRows.length, 9);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "PASS").length,
  7,
);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "PARTIAL").length,
  2,
);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "OPEN").length,
  0,
);
for (const [rowId, row] of structuralAuditRows) {
  if (row.disposition === "PASS") {
    assert.ok(
      ["L295", "L296", "L297", "L299", "L300", "L301", "L303"].includes(rowId),
    );
    assert.equal(row.remainingTask, null);
  } else {
    assert.match(row.remainingTask, /^Q49-L\d{3}$/u);
  }
}
const structuralContract = {
  L295: ["Duplicate TxId in a block", "PASS", null],
  L296: ["Header carry-over fields", "PASS", null],
  L297: ["Oversized deposits", "PASS", null],
  L298: ["Cross-block replay", "PARTIAL", "Q49-L298"],
  L299: ["L2 fee misdirection", "PASS", null],
  L300: ["Withdrawal payout amount/destination", "PASS", null],
  L301: ["Deposit `inclusion_time` value forgery", "PASS", null],
  L302: ["Malformed validity interval", "PARTIAL", "Q49-L302"],
  L303: ["Deposit refund double-representation", "PASS", null],
};
for (const [rowId, [concern, disposition, remainingTask]] of Object.entries(
  structuralContract,
)) {
  const lineNumber = Number(rowId.slice(1));
  assert.ok(
    matrixLines[lineNumber - 1]?.includes(concern),
    `${rowId} no longer identifies ${concern}`,
  );
  const row = evidence.structuralAudit.rows[rowId];
  assert.equal(row?.disposition, disposition, `${rowId} disposition drifted`);
  assert.equal(row?.remainingTask, remainingTask, `${rowId} task drifted`);
  assert.ok(row.executableEvidence?.length > 0, `${rowId} lacks evidence`);
}
assert.ok(
  matrixLines[293]?.startsWith("| ---"),
  "L294 must remain the table separator, never a structural claim",
);
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
    `L299 production fee invariant lost ${requiredFeeSymbol}`,
  );
}
assert.ok(
  phaseBTestSource.includes(
    "burns the exact L2 fee in production value accounting and rejects fee redirection",
  ),
  "L299 exact fee-burn control is absent",
);
for (const source of [
  depositTypeSource,
  depositValidatorSource,
  depositSdkSource,
]) {
  assert.doesNotMatch(source, /\bRefund\b/u, "L303 deposit refund path exists");
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
    `L303 deposit absorption guard lost ${requiredDepositGuard}`,
  );
}
assert.ok(
  depositSdkSource.includes(
    "export const DepositSpendRedeemerSchema = Data.Object({",
  ),
  "L303 SDK deposit spend schema is not one exact record",
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

const [
  catalogueSource,
  faultProofContractsSource,
  submitInitSource,
  inspectContractsSource,
  commonFaultProofSource,
  transitionTraceSource,
] = await Promise.all([
  readFile(
    resolve(repositoryRoot, "demo/midgard-sdk/src/fraud-proof/catalogue.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-sdk/src/fraud-proof/contracts.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-fault-proofs/src/submit-init.ts"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/src/inspect-contracts.ts",
    ),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/lib/midgard/fraud-proofs/common.ak"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak",
    ),
    "utf8",
  ),
]);
const stringLiteralsBetween = (source, start, end) => {
  const startIndex = source.indexOf(start);
  assert.notEqual(startIndex, -1, `missing source anchor ${start}`);
  const endIndex = source.indexOf(end, startIndex);
  assert.notEqual(endIndex, -1, `missing source anchor ${end}`);
  return [...source.slice(startIndex, endIndex).matchAll(/"([^"\n]+)"/gu)].map(
    ([, value]) => value,
  );
};
const registeredCategoryNames = stringLiteralsBetween(
  catalogueSource,
  "export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = [",
  "] as const",
);
assert.equal(evidence.bindingInventory.compiledStandaloneFamilies, 12);
assert.deepEqual(
  evidence.bindingInventory.registeredCategoryNames,
  registeredCategoryNames,
  "evidence category inventory diverged from the SDK catalogue",
);
assert.equal(
  evidence.bindingInventory.registeredCatalogueCategories,
  registeredCategoryNames.length,
);
assert.equal(
  evidence.bindingInventory.initializationCliCategories,
  registeredCategoryNames.length,
);
assert.equal(
  evidence.bindingInventory.inspectionCategories,
  registeredCategoryNames.length,
);
assert.deepEqual(
  stringLiteralsBetween(
    submitInitSource,
    "export type SubmitInitFraudCategory =",
    ";",
  ),
  registeredCategoryNames,
  "submit-init category union diverged from the catalogue",
);
const inspectionCategoryNames = stringLiteralsBetween(
  inspectContractsSource,
  "export type InspectContractsProofCategory =",
  ";",
);
assert.equal(
  inspectionCategoryNames.length,
  8,
  "inspect-contracts category count changed",
);
assert.deepEqual(
  [...inspectionCategoryNames].sort(),
  [...registeredCategoryNames].sort(),
  "inspect-contracts category set diverged from the catalogue",
);
const nativeHelperDefinition =
  /\bpub\s+fn\s+pass_native_tx_to_next_step\s*\(/gu;
assert.equal(
  [...commonFaultProofSource.matchAll(nativeHelperDefinition)].length,
  1,
  "native V1 transition helper must retain one exact identifier definition",
);
assert.doesNotMatch(
  commonFaultProofSource,
  /verify_tx_in_state_queue_node/u,
  "legacy PlutusData binding helper returned",
);
for (const requiredTransitionTraceAnchor of [
  "header.transition_trace_root",
  "validation_claim_v1.committed_claim_is_valid",
]) {
  assert.ok(
    transitionTraceSource.includes(requiredTransitionTraceAnchor),
    `transition-trace native V1 binding lost ${requiredTransitionTraceAnchor}`,
  );
}
const [
  computationThreadTestSource,
  catalogueTestSource,
  stateQueueTestSource,
  onchainReferenceSource,
  testingStatusSource,
] = await Promise.all([
  readFile(
    resolve(repositoryRoot, "onchain/aiken/validators/computation-thread.ak"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "onchain/aiken/validators/fraud-proof-catalogue.ak",
    ),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/validators/state-queue.ak"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "docs/fault-proofs/onchain-reference.md"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "docs/fault-proofs/testing-status.md"),
    "utf8",
  ),
]);
const aikenTestSelectors = (source) =>
  [...source.matchAll(/^test\s+([^\s(]+)\(/gmu)].map(
    ([, selector]) => selector,
  );
const computationThreadSelectors = aikenTestSelectors(
  computationThreadTestSource,
);
const catalogueSelectors = aikenTestSelectors(catalogueTestSource);
const stateQueueSelectors = aikenTestSelectors(stateQueueTestSource);
assert.equal(computationThreadSelectors.length, 15);
assert.equal(catalogueSelectors.length, 4);
assert.equal(stateQueueSelectors.length, 6);
assert.ok(
  computationThreadSelectors.every((selector) => selector.startsWith("ct_")),
  "computation-thread direct-test inventory lost its exact selectors",
);
assert.ok(
  catalogueSelectors.every((selector) => selector.startsWith("catalogue_")),
  "catalogue direct-test inventory lost its exact selectors",
);
assert.ok(
  stateQueueSelectors.every((selector) => selector.startsWith("q49_l295_")),
  "state-queue HeaderV1 selector inventory drifted",
);
for (const anchor of [
  "validators/computation-thread.ak:280-513`                                       | 15",
  "validators/fraud-proof-catalogue.ak:50-76`                                      | 4",
  "validators/state-queue.ak:841-1065`                                             | 6",
]) {
  assert.ok(
    onchainReferenceSource.includes(anchor),
    `onchain reference lost direct-test inventory anchor ${anchor}`,
  );
}
for (const anchor of [
  "computation-thread (15)",
  "immutable catalogue (4)",
  "state-queue commit controls (6)",
]) {
  assert.ok(
    testingStatusSource.includes(anchor),
    `testing status lost direct-test inventory anchor ${anchor}`,
  );
}
for (const [path, anchor] of Object.entries(evidence.documentationAnchors)) {
  const source = await readFile(resolve(repositoryRoot, path), "utf8");
  assert.ok(source.includes(anchor), `${path} lost reconciliation anchor`);
}
const completedDocTaskIds = ["Q13", "Q24", "Q25", "Q44", "Q54"];
const currentBlockerWord =
  /\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\b/iu;
const historicalStatement =
  /\b(?:historical(?:ly)?|superseded|formerly|previously|prior)\b/iu;
const semanticDocUnits = (source) =>
  source
    .split(/\r?\n/u)
    .flatMap((line) =>
      line.startsWith("|")
        ? line
            .split("|")
            .map((cell) => cell.trim())
            .filter(Boolean)
        : line.split(/(?<=[.!?])\s+/u),
    )
    .flatMap((unit) => unit.split(/[;:]/u))
    .map((unit) => unit.trim())
    .filter(Boolean);
for (const path of Object.keys(evidence.documentationAnchors)) {
  const source = await readFile(resolve(repositoryRoot, path), "utf8");
  for (const unit of semanticDocUnits(source)) {
    if (historicalStatement.test(unit) || !currentBlockerWord.test(unit))
      continue;
    for (const taskId of completedDocTaskIds) {
      const gapWithoutAnotherTask =
        "(?:(?!\\bQ[A-Z0-9][A-Z0-9-]*\\b)[\\s\\S]){0,120}";
      const currentTaskBlocker = new RegExp(
        `(?:\\b${taskId}\\b${gapWithoutAnotherTask}\\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\\b|\\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\\b${gapWithoutAnotherTask}\\b${taskId}\\b)`,
        "iu",
      );
      if (!currentTaskBlocker.test(unit)) continue;
      const explicitlyNegatesBlocker = new RegExp(
        `(?:\\b(?:not|no|never)\\b[^.]{0,120}\\b${taskId}\\b|\\b${taskId}\\b[^.]{0,120}\\b(?:not|no|never)\\b)[^.]{0,120}\\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\\b`,
        "iu",
      );
      const explicitlyExcludesCompletedTask = new RegExp(
        `\\b${taskId}\\b\\s+is\\s+no\\s+longer\\b`,
        "iu",
      );
      if (
        explicitlyNegatesBlocker.test(unit) ||
        explicitlyExcludesCompletedTask.test(unit)
      )
        continue;
      assert.ok(
        !new RegExp(`\\b${taskId}\\b`, "u").test(unit),
        `${path} presents completed ${taskId} as a current blocker/open residue: ${unit}`,
      );
    }
  }
}

const [
  goalProgressSource,
  taskManifestSource,
  goalSpecSource,
  watcherDependencyMapSource,
  binSource,
  inputNoIdxSource,
  inputNoIdxEmulatorTest,
  inputNoIdxPreparationTest,
  familyScaffoldTestSource,
  deployedValidatorEntries,
] = await Promise.all([
  readFile(resolve(repositoryRoot, "GOAL_PROGRESS.md"), "utf8"),
  readFile(
    resolve(
      repositoryRoot,
      "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
    ),
    "utf8",
  ),
  readFile(resolve(repositoryRoot, "GOAL_SPEC.md"), "utf8"),
  readFile(
    resolve(
      repositoryRoot,
      "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
    ),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-fault-proofs/src/bin.ts"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/src/submit-input-no-idx-step-04.ts",
    ),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/tests/submit-init-emulator-input-no-idx.test.ts",
    ),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/tests/prepare-input-no-idx.test.ts",
    ),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/tests/family-scaffold-v1.test.ts",
    ),
    "utf8",
  ),
  readdir(resolve(repositoryRoot, "onchain/aiken/validators/fraud-proofs"), {
    withFileTypes: true,
  }),
]);
const queueSource = goalProgressSource.slice(
  goalProgressSource.indexOf("## Task queue"),
);
const firstQueueStatuses = new Map();
const firstQueueDetails = new Map();
for (const line of queueSource.split(/\r?\n/u)) {
  const cells = line.split("|").map((cell) => cell.trim());
  const taskId = cells[1];
  const status = cells[5];
  if (
    /^[A-Z][A-Z0-9-]*$/u.test(taskId) &&
    status &&
    !firstQueueStatuses.has(taskId)
  ) {
    firstQueueStatuses.set(taskId, status);
    firstQueueDetails.set(taskId, cells.at(-2) ?? "");
  }
}
assert.match(
  firstQueueDetails.get("F20") ?? "",
  /61 coverage rows and nine (?:physical )?structural claims: five locally complete, 12 structural\/N\/A, 53 open/u,
  "first F20 queue record retained stale reconciliation totals",
);
assert.match(
  firstQueueDetails.get("F21") ?? "",
  /PARTIAL L298 cross-block replay and L302 malformed interval.*7 PASS\s*\/\s*2 PARTIAL\s*\/\s*0 OPEN/iu,
  "first F21 queue record retained stale physical structural dispositions",
);
assert.doesNotMatch(
  firstQueueDetails.get("F21") ?? "",
  /Q49-L29[7]|8 PASS\s*\/\s*1 PARTIAL|L297 is the sole remaining/u,
  "first F21 queue record retained stale physical-row identity",
);
assert.ok(
  goalSpecSource.includes(
    "F20 also emits the initial concrete §9.1 launch-scope family list.",
  ),
  "GOAL_SPEC F20 authority anchor is absent",
);
assert.ok(
  goalSpecSource.includes(
    "Unsupported prose-only N/A claims become open tasks.",
  ),
  "GOAL_SPEC F21 authority anchor is absent",
);
const passIds = new Set(
  [...firstQueueStatuses]
    .filter(([, status]) => status.startsWith("PASS"))
    .map(([taskId]) => taskId),
);
for (const taskId of evidence.taskResidues.completedPrerequisites) {
  assert.ok(
    passIds.has(taskId),
    `${taskId} is not PASS in the first task queue`,
  );
}
assert.ok(passIds.has("Q13"), "Q13 is not PASS in the first task queue");
const residueTaskIds = [
  ...evidence.taskResidues.openFamilyClosures,
  ...evidence.taskResidues.openLifecycleAndAcceptance,
  ...evidence.criticalFindings.flatMap((finding) => finding.remainingTasks),
];
for (const taskId of residueTaskIds) {
  assert.ok(!passIds.has(taskId), `${taskId} is PASS but remains open`);
}
const criticalFindingById = new Map(
  evidence.criticalFindings.map((finding) => [finding.id, finding]),
);
assert.deepEqual(criticalFindingById.get("F20-04")?.remainingTasks, [
  "Q51",
  "Q57",
  "Q58",
  "Q59",
]);
assert.deepEqual(criticalFindingById.get("F20-06")?.remainingTasks, [
  "Q23",
  "Q26",
  "Q27",
  "Q28",
  "Q29",
  "Q30",
  "Q31",
  "Q32",
  "Q33",
  "Q34",
  "Q35",
  "Q36",
  "Q37",
  "Q38",
  "Q39",
  "Q40",
  "Q41",
  "Q42",
  "Q43",
  "Q45",
  "Q46",
  "Q47",
  "Q48",
  "Q49",
  "Q50",
  "Q55",
]);
assert.deepEqual(criticalFindingById.get("F20-06")?.sources, [
  "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
  "onchain/aiken/validators/fraud-proofs/input-no-idx/step-01.ak",
]);
assert.ok(!evidence.taskResidues.openLifecycleAndAcceptance.includes("Q54"));
assert.ok(
  !evidence.criticalFindings
    .find(({ id }) => id === "F20-04")
    .remainingTasks.includes("Q54"),
);
assert.ok(
  !evidence.criticalFindings
    .find(({ id }) => id === "F20-06")
    .remainingTasks.includes("Q24"),
);
assert.ok(
  !evidence.criticalFindings
    .find(({ id }) => id === "F20-06")
    .remainingTasks.includes("Q25"),
);
const taskManifest = JSON.parse(taskManifestSource);
const manifestTasks = [];
const visitManifest = (value) => {
  if (Array.isArray(value)) value.forEach(visitManifest);
  else if (value && typeof value === "object") {
    if (typeof value.id === "string") manifestTasks.push(value);
    Object.values(value).forEach(visitManifest);
  }
};
visitManifest(taskManifest);
const manifestTaskById = new Map(manifestTasks.map((task) => [task.id, task]));
const f20Manifest = manifestTaskById.get("F20");
const f21Manifest = manifestTaskById.get("F21");
const f20ManifestText = JSON.stringify(f20Manifest);
const f21ManifestText = JSON.stringify(f21Manifest);
assert.match(
  f20ManifestText,
  /5 locally complete, 12 structural\/N\/A, and 53 open/u,
  "F20 manifest retained stale reconciliation totals",
);
assert.doesNotMatch(
  f20ManifestText,
  /6 registered|5 initialization|54 open|44 of 45/u,
  "F20 manifest retained stale completed-task mismatch wording",
);
assert.match(
  f21ManifestText,
  /7 PASS, 2 PARTIAL.*L298.*Q49-L298.*L302.*Q49-L302/su,
  "F21 manifest retained stale physical structural contract",
);
assert.doesNotMatch(
  f21ManifestText,
  /8 PASS|Q49-L29[7]|L297 is exactly the 1 PARTIAL/u,
  "F21 manifest retained stale completed-task structural wording",
);
for (const task of manifestTasks) {
  if (typeof task.blockedBecause !== "string") continue;
  assert.doesNotMatch(
    task.blockedBecause,
    /\b(?:Q00|Q02|Q03|Q13|Q24|Q25|Q44|Q54)\b\s+(?:is|are|remains?)\s+(?:OPEN|PENDING|BLOCKED|not PASS)\b/iu,
    `${task.id} retained a completed task as a blocker`,
  );
}
const watcherDependencyMap = JSON.parse(watcherDependencyMapSource);
const watcherRemainingTaskArrays = [];
const collectRemainingTasks = (value) => {
  if (Array.isArray(value)) {
    value.forEach(collectRemainingTasks);
  } else if (value && typeof value === "object") {
    if (Array.isArray(value.remainingTasks)) {
      watcherRemainingTaskArrays.push(value.remainingTasks);
    }
    Object.values(value).forEach(collectRemainingTasks);
  }
};
collectRemainingTasks(watcherDependencyMap);
assert.ok(
  watcherRemainingTaskArrays.every((tasks) => !tasks.includes("F20")),
  "watcher dependency map retained completed F20 as a remaining task",
);
for (const taskId of evidence.taskResidues.openFamilyClosures) {
  const task = manifestTasks.find(({ id }) => id === taskId);
  assert.ok(task?.blockedBecause, `${taskId} lacks a current manifest blocker`);
  assert.ok(!passIds.has(taskId), `${taskId} is queue PASS but manifest-open`);
}
assert.ok(binSource.includes("submit-input-no-idx-step-04"));
assert.ok(inputNoIdxSource.includes("submitInputNoIdxStep04"));
assert.ok(
  inputNoIdxEmulatorTest.includes(
    "input-no-idx fault-proof emulator lifecycle",
  ),
  "Q13 emulator lifecycle test is absent",
);
assert.ok(
  inputNoIdxEmulatorTest.includes(
    "cannot finalize an input-no-idx thread against a valid block",
  ),
  "Q13 valid-block negative is absent",
);
assert.ok(
  inputNoIdxPreparationTest.includes("Q13 input-no-idx canonical evidence"),
  "Q13 preparation evidence test is absent",
);
const familyScaffoldSelectors = [
  ...familyScaffoldTestSource.matchAll(/^\s*it\(/gmu),
];
assert.equal(
  familyScaffoldSelectors.length,
  evidence.summary.focusedChecksTotal,
  "focused family-scaffold total no longer derives from the full test file",
);
assert.equal(
  evidence.summary.focusedChecksPassed,
  familyScaffoldSelectors.length,
  "evidence may not bless a focused pass count that the full test file does not collect",
);
assert.ok(
  taskManifestSource.includes(evidence.summary.focusedCommand),
  "focused 45/45 external gate is not named by the task authority",
);
const parsedCliCategoryNames = [
  ...binSource
    .slice(
      binSource.indexOf("export const parseFraudCategory"),
      binSource.indexOf("export const parseArgs"),
    )
    .matchAll(/value === "([^"\n]+)"/gu),
].map(([, name]) => name);
assert.deepEqual(
  [...parsedCliCategoryNames].sort(),
  [...registeredCategoryNames].sort(),
  "CLI accepted fraud-category set drifted",
);
const deployedValidatorDirectories = deployedValidatorEntries
  .filter((entry) => entry.isDirectory())
  .map((entry) => entry.name)
  .sort();
const registeredValidatorDirectories = [
  ...new Set(
    [...faultProofContractsSource.matchAll(/"fraud_proofs\/([^/]+)\//gu)].map(
      ([, directory]) => directory.replaceAll("_", "-"),
    ),
  ),
].sort();
const unregisteredValidatorDirectories = deployedValidatorDirectories.filter(
  (directory) => !registeredValidatorDirectories.includes(directory),
);
assert.deepEqual(
  evidence.bindingInventory.deployedValidatorDirectories,
  deployedValidatorDirectories,
  "evidence deployed-validator inventory diverged from the validators tree",
);
assert.equal(
  registeredValidatorDirectories.length,
  registeredCategoryNames.length,
  "compiled category-title groups diverged from the catalogue category count",
);
assert.deepEqual(
  evidence.bindingInventory.unregisteredValidatorDirectories,
  unregisteredValidatorDirectories,
  "six catalogue-decision residues must derive from source titles and deployed directories",
);
assert.equal(
  unregisteredValidatorDirectories.length,
  6,
  "the deployed inventory must expose exactly six unregistered validator directories",
);
const nativeStepSources = await Promise.all(
  deployedValidatorDirectories.map(async (directory) => {
    try {
      return [
        directory,
        await readFile(
          resolve(
            repositoryRoot,
            `onchain/aiken/validators/fraud-proofs/${directory}/step-01.ak`,
          ),
          "utf8",
        ),
      ];
    } catch {
      return [directory, null];
    }
  }),
);
const nativeHelperCall = /\bpass_native_tx_to_next_step\s*\(/gu;
const nativeV1StepFamilies = nativeStepSources
  .filter(
    ([, source]) =>
      [...(source?.matchAll(nativeHelperCall) ?? [])].length === 1,
  )
  .map(([directory]) => directory);
for (const [directory, source] of nativeStepSources) {
  if (source === null) continue;
  const callCount = [...source.matchAll(nativeHelperCall)].length;
  if (callCount !== 0) {
    assert.equal(
      callCount,
      1,
      `${directory} must call pass_native_tx_to_next_step exactly once`,
    );
  }
}
const nativeV1Families = [
  ...nativeV1StepFamilies,
  ...(transitionTraceSource.includes("header.transition_trace_root") &&
  transitionTraceSource.includes("validation_claim_v1.committed_claim_is_valid")
    ? ["transition-trace"]
    : []),
].sort();
assert.deepEqual(
  evidence.bindingInventory.nativeV1Families,
  nativeV1Families,
  "native V1 family inventory must derive from step sources and transition-trace proof source",
);
assert.equal(
  evidence.bindingInventory.compiledStandaloneFamilies,
  nativeV1Families.length,
);
assert.deepEqual(evidence.bindingInventory.legacyPlutusDataFamilies, []);
for (const stalePattern of [
  /7 registered\s+categories/u,
  /5 tooled legacy/u,
  /30 ms\s+maturity/u,
  /zero code/u,
  /nothing binds retention windows/u,
  /Witness-set encoding split in the shipped signature proofs/u,
  /DA hash-preimage proofs/u,
]) {
  for (const path of Object.keys(evidence.documentationAnchors)) {
    const source = await readFile(resolve(repositoryRoot, path), "utf8");
    assert.doesNotMatch(
      source,
      stalePattern,
      `${path} retained stale documentation`,
    );
  }
}

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
