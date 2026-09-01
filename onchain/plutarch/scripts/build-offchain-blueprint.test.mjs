import assert from "node:assert/strict";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";

import {
  buildOffchainBlueprint,
  offchainTitleForGeneratedFile,
  unwrapTextEnvelopeCborHex,
} from "./build-offchain-blueprint.mjs";

test("maps core and staged Plutarch artifacts to off-chain titles", () => {
  assert.equal(
    offchainTitleForGeneratedFile("state-queue-mint.unapplied.plutus.json"),
    "state_queue.mint.mint",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-double-spend-step-04.unapplied.plutus.json",
    ),
    "fraud_proofs/double_spend/step_04.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-transition-trace-l1-event-v1.unapplied.plutus.json",
    ),
    "fraud_proofs/transition_trace/l1_event_v1.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-validation-trace-script-sources-stage-seven-finish-semantic-v1.unapplied.plutus.json",
    ),
    "fraud_proofs/validation_trace/script_sources_stage_seven_finish_semantic_v1.main.spend",
  );
});

test("unwraps exactly one Cardano text-envelope CBOR layer", () => {
  assert.equal(unwrapTextEnvelopeCborHex("43420102"), "420102");
  assert.equal(
    unwrapTextEnvelopeCborHex(`5818${"40".repeat(24)}`),
    "40".repeat(24),
  );
  assert.throws(
    () => unwrapTextEnvelopeCborHex("420102"),
    /single-CBOR script encoding/u,
  );
  assert.throws(
    () => unwrapTextEnvelopeCborHex("434201"),
    /length does not match/u,
  );
});

test("assembles every generated Plutarch artifact exactly once", async () => {
  const temporaryDir = await mkdtemp(
    path.join(os.tmpdir(), "midgard-plutarch-blueprint-"),
  );
  const outputPath = path.join(temporaryDir, "plutus.json");
  try {
    const result = await buildOffchainBlueprint({ outputPath });
    assert.equal(result.validatorCount, 200);
    assert.equal(new Set(result.titles).size, 200);

    const blueprint = JSON.parse(await readFile(outputPath, "utf8"));
    assert.equal(blueprint.preamble.compiler.name, "Plutarch");
    assert.equal(blueprint.validators.length, 200);
    assert.ok(
      blueprint.validators.every(
        ({ title, compiledCode }) =>
          typeof title === "string" &&
          typeof compiledCode === "string" &&
          /^[0-9a-f]+$/u.test(compiledCode),
      ),
    );
    const hubOracle = blueprint.validators.find(
      ({ title }) => title === "hub_oracle.mint.mint",
    );
    assert.ok(hubOracle.compiledCode.startsWith("59018b010100"));
    const daParamsGovernorMint = blueprint.validators.find(
      ({ title }) => title === "da_params_governor.da_params_governor.mint",
    );
    const daParamsGovernorSpend = blueprint.validators.find(
      ({ title }) => title === "da_params_governor.da_params_governor.spend",
    );
    assert.equal(
      daParamsGovernorMint.compiledCode,
      daParamsGovernorSpend.compiledCode,
    );
    const daAttestationMint = blueprint.validators.find(
      ({ title }) => title === "da_attestation.da_attestation.mint",
    );
    const daAttestationSpend = blueprint.validators.find(
      ({ title }) => title === "da_attestation.da_attestation.spend",
    );
    assert.equal(
      daAttestationMint.compiledCode,
      daAttestationSpend.compiledCode,
    );
  } finally {
    await rm(temporaryDir, { recursive: true, force: true });
  }
});
