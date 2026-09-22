import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const packages = [
  ["@al-ft/midgard-core", "midgard-core", "index"],
  ["@al-ft/midgard-core/codec/cbor", "midgard-core", "codec/cbor"],
  ["@al-ft/midgard-validation", "midgard-validation", "index"],
  ["@al-ft/midgard-sdk", "midgard-sdk", "index"],
];
const loaded = { import: [], require: [] };
for (const [name, directory, entry] of packages) {
  const base = new URL(`../../${directory}/dist/${entry}`, import.meta.url);
  assert.equal(import.meta.resolve(name), `${base.href}.js`);
  assert.equal(require.resolve(name), fileURLToPath(`${base.href}.cjs`));
  loaded.import.push(await import(name));
  loaded.require.push(require(name));
}

const { Effect } = await import("effect");
const { credentialToAddress } = await import("@lucid-evolution/lucid");
const rawBlueprint = JSON.parse(
  readFileSync(new URL("../../../onchain/aiken/plutus.json", import.meta.url)),
);
const policyId = "11".repeat(28);
for (const [mode, [core, cbor, validation, sdk]] of Object.entries(loaded)) {
  assert.equal(
    core.encodeCbor([1n, Buffer.alloc(0)]).toString("hex"),
    "820140",
  );
  assert.deepEqual(cbor.decodeSingleCbor(Buffer.from("820140", "hex")), [
    1,
    new Uint8Array(),
  ]);
  // Independent rejection vector also pinned by the Aiken terminal ABI tests.
  assert.equal(
    validation
      .encodeValidationTerminalWitnessCbor({
        verdict: "rejected",
        rejectionCode: "E_VALUE_NOT_PRESERVED",
        priorLedgerRoot: Buffer.alloc(32, 0x73),
      })
      .toString("hex"),
    "840255455f56414c55455f4e4f545f505245534552564544582073737373737373737373737373737373737373737373737373737373737373734180",
  );
  const blueprint = sdk.parseFaultProofBlueprint(rawBlueprint);
  const lockParams = {
    blueprint,
    network: "Preprod",
    hubOraclePolicyId: policyId,
    availabilityChallengePolicyId: policyId,
  };
  const lock = await Effect.runPromise(
    sdk.buildCorrectionLockValidator(lockParams),
  );
  const queue = await Effect.runPromise(
    sdk.buildStateQueueValidator({
      ...lockParams,
      correctionLockScriptHash: lock.spendingScriptHash,
      activeOperatorsPolicyId: policyId,
      activeOperatorsAddress: credentialToAddress("Preprod", {
        type: "Script",
        hash: policyId,
      }),
      retiredOperatorsPolicyId: policyId,
      schedulerPolicyId: policyId,
      fraudProofPolicyId: policyId,
      settlementPolicyId: policyId,
      daAttestationPolicyId: policyId,
      referenceScriptAuthPolicyId: policyId,
    }),
  );
  assert.equal(queue.policyId.length, 56);
  assert.equal(Object.keys(queue.yields).length, 5);
  await assert.rejects(
    Effect.runPromise(
      sdk.buildCorrectionLockValidator({
        ...lockParams,
        hubOraclePolicyId: "aa",
      }),
    ),
    /28-byte hash/,
  );
  for (const name of [
    "applyBlueprintParams",
    "getUnappliedScript",
    "makeAuthenticatedValidator",
    "makeMintingPolicy",
    "makeSpendingValidator",
    "makeWithdrawalValidator",
  ]) {
    assert.equal(typeof sdk[name], "function", `${mode}: ${name}`);
  }
  console.log(
    `${mode}: built package resolution, CBOR, terminal witness, queue/lock construction and refusal passed`,
  );
}
