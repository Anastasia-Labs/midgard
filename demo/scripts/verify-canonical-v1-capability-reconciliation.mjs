import assert from "node:assert/strict";
import { createHash } from "node:crypto";
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

const sourceText = {};
for (const [name, source] of Object.entries(evidence.sources)) {
  const bytes = await readFile(resolve(repositoryRoot, source.path));
  const digest = createHash("sha256").update(bytes).digest("hex");
  assert.equal(digest, source.sha256, `${name} source bytes drifted`);
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
  pass: 10,
  partial: 12,
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
