import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");
const evidencePath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-capability-reconciliation-v1.json",
);
const evidence = JSON.parse(await readFile(evidencePath, "utf8"));

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-capability-reconciliation.v1",
);

// GOAL_SPEC §13.4 / §0 Integrity (owner amendment 2026-08-01): tracked
// sources are bound by path, not by byte hash. The content assertions below
// are what actually protect this reconciliation — they fail if the specific
// rows this evidence depends on are removed or renamed. A byte hash instead
// turned every unrelated edit to a 1,400-line spec into a red gate, which is
// exactly how this check came to be failing against a two-amendments-stale
// digest while the reconciliation it guards was still correct.
const sourceText = {};
for (const [name, source] of Object.entries(evidence.sources)) {
  const bytes = await readFile(resolve(repositoryRoot, source.path));
  sourceText[name] = bytes.toString("utf8");
}

const controlStatuses = Object.values(evidence.controlPlane);
assert.deepEqual(controlStatuses, ["PASS", "PASS", "PASS", "PASS", "PASS"]);

const taskEntries = Object.entries(evidence.p2Tasks).filter(
  ([task]) => task !== "CG2",
);
const statusCount = (status) =>
  taskEntries.filter(([, disposition]) => disposition === status).length;
assert.deepEqual(evidence.p2Summary, {
  tasks: 22,
  // C20-6/C20-7 were promoted 2026-08-03. C20-2/C20-4/C20-5 were promoted
  // 2026-08-04 after independent exact Aiken/TypeScript boundary and terminal
  // replay. CG2 stays OPEN while any P2 task is PARTIAL.
  pass: 15,
  partial: 7,
  open: 0,
  authoritativeConflict: 0,
  gate: "OPEN",
});
assert.equal(taskEntries.length, evidence.p2Summary.tasks);
assert.equal(statusCount("PASS"), evidence.p2Summary.pass);
assert.equal(statusCount("PARTIAL"), evidence.p2Summary.partial);
assert.equal(statusCount("OPEN"), evidence.p2Summary.open);
assert.equal(
  statusCount("AUTHORITATIVE_CONFLICT"),
  evidence.p2Summary.authoritativeConflict,
);

assert.match(
  sourceText.goalSpec,
  /\| C20-6 \| Field 6 native\/non-native script witnesses/u,
);
assert.match(
  sourceText.goalSpec,
  /\| C20-7 \| Field 7 vkey witnesses and exact signer identities/u,
);
assert.match(
  sourceText.canonicalTransaction,
  /5 mint, 6 script witnesses, 7 address witnesses, 8 redeemers/u,
);
assert.deepEqual(evidence.authorityConflict.tasks, ["C20-6", "C20-7"]);
assert.equal(evidence.authorityConflict.disposition, "RESOLVED");

assert.doesNotMatch(
  sourceText.validationAuxiliaryTypeScript,
  /TransactionFieldPreimageWitness/u,
);
assert.doesNotMatch(
  sourceText.validationMachineTypeScript,
  /transactionFieldPreimage|TransactionFieldPreimageWitness/u,
);
assert.doesNotMatch(
  sourceText.validationMachineDataTypeScript,
  /transactionFieldPreimage|TransactionFieldPreimageWitness/u,
);
assert.doesNotMatch(
  sourceText.validationMachineAiken,
  /TransactionFieldPreimageWitness \{ preimage_cbor: ByteArray \}/u,
);
assert.equal(evidence.wholePreimageFinding.status, "RESOLVED_ABI");
assert.deepEqual(evidence.wholePreimageFinding.removedFrom, [
  "demo/midgard-sdk/src/fraud-proof/validation-auxiliary-witness-v1.ts",
  "demo/midgard-validation/src/validation-machine.ts",
  "demo/midgard-validation/src/validation-machine-data.ts",
  "onchain/aiken/lib/midgard/validation-machine-v1.ak",
]);

assert.equal(evidence.acceptance.F10, "PASS");
assert.equal(evidence.acceptance.CG1, "PASS");
assert.equal(evidence.acceptance.CG2, "OPEN");
assert.equal(evidence.p2Tasks.CG2, "OPEN");

console.log(
  `canonical V1 capability reconciliation verified: ${taskEntries.length} P2 tasks, ${evidence.p2Summary.pass} pass, CG2 open`,
);
