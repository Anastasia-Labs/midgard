import assert from "node:assert/strict";
import { mkdtemp, readFile, readdir, rm } from "node:fs/promises";
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
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-native-script-invalid-accepted-inline-source.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-native-script-invalid-accepted-reference-source.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-native-script-invalid-accepted-spend-prefix.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-native-script-invalid-accepted-mint-prefix.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-native-script-invalid-accepted-observer-prefix.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-native-script-invalid-accepted-receive-prefix.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-native-script-invalid-accepted-reconstruction-init.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
  );
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
      "fraud-proof-field-item-width-illegal-step-03.unapplied.plutus.json",
    ),
    "fraud_proofs/field_item_width_illegal/step_03.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-field-preimage-length-mismatch-step-02-forced.unapplied.plutus.json",
    ),
    "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-observers-forbidden-on-untagged-network-step-02.unapplied.plutus.json",
    ),
    "fraud_proofs/observers_forbidden_on_untagged_network/step_02.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-redeemer-canonicity-step-03.unapplied.plutus.json",
    ),
    "fraud_proofs/redeemer_canonicity/step_03.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-observer-order-invalid-step-04.unapplied.plutus.json",
    ),
    "fraud_proofs/observer_order_invalid/step_04.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-script-integrity-hash-mismatch-step-05.unapplied.plutus.json",
    ),
    "fraud_proofs/script_integrity_hash_mismatch/step_05.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-receive-purpose-language-step-03.unapplied.plutus.json",
    ),
    "fraud_proofs/receive_purpose_language/step_03.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-distinct-asset-accumulation-limit-step-06.unapplied.plutus.json",
    ),
    "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-mint-declared-asset-limit-step-04.unapplied.plutus.json",
    ),
    "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-witness-script-decoding-step-04.unapplied.plutus.json",
    ),
    "fraud_proofs/witness_script_decoding/step_04.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-output-reference-script-decoding-step-06.unapplied.plutus.json",
    ),
    "fraud_proofs/output_reference_script_decoding/step_06.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-execution-source-script-decoding-step-05.unapplied.plutus.json",
    ),
    "fraud_proofs/execution_source_script_decoding/step_05.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-missing-redeemer-step-02a.unapplied.plutus.json",
    ),
    "fraud_proofs/missing_redeemer/step_02a.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-missing-redeemer-step-05.unapplied.plutus.json",
    ),
    "fraud_proofs/missing_redeemer/step_05.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-unused-redeemer-step-02c.unapplied.plutus.json",
    ),
    "fraud_proofs/unused_redeemer/step_02c.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-missing-script-source-step-06.unapplied.plutus.json",
    ),
    "fraud_proofs/missing_script_source/step_06.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-script-integrity-hash-missing-redeemer-grammar.unapplied.plutus.json",
    ),
    "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-protected-output-signer-missing-step-05.unapplied.plutus.json",
    ),
    "fraud_proofs/protected_output_signer_missing/step_05.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-resolved-output-non-canonical-step-05.unapplied.plutus.json",
    ),
    "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-spend-input-signer-missing-step-05.unapplied.plutus.json",
    ),
    "fraud_proofs/spend_input_signer_missing/step_05.main.spend",
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
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-validation-trace-script-sources-stage-seven-observer-item-yield-v1.unapplied.plutus.json",
    ),
    "fraud_proofs/validation_trace/script_sources_stage_seven_observer_item_yield_v1.main.withdraw",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-validation-trace-script-sources-stage-seven-observer-bound-yield-v1.unapplied.plutus.json",
    ),
    "fraud_proofs/validation_trace/script_sources_stage_seven_observer_bound_yield_v1.main.withdraw",
  );
  assert.equal(
    offchainTitleForGeneratedFile(
      "fraud-proof-validation-trace-script-sources-redeemer-item-step-yield-v1.unapplied.plutus.json",
    ),
    "fraud_proofs/validation_trace/script_sources_redeemer_item_step_yield_v1.main.withdraw",
  );
});

test("maps every operational yield to its exact target title", () => {
  for (const arm of [
    "commit",
    "remove-unattested",
    "remove-unavailable",
    "remove-fraudulent",
    "merge",
  ]) {
    assert.equal(
      offchainTitleForGeneratedFile(
        `state-queue-yield-${arm}.unapplied.plutus.json`,
      ),
      `state_queue_yields.${arm.replaceAll("-", "_")}.withdraw`,
    );
  }
  for (const arm of ["bond", "open", "settle", "close", "timeout"]) {
    assert.equal(
      offchainTitleForGeneratedFile(
        `availability-challenge-yield-${arm}.unapplied.plutus.json`,
      ),
      `availability_challenge_yields.${arm}.withdraw`,
    );
  }
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
    const generatedNames = (
      await readdir(new URL("../generated/", import.meta.url))
    ).filter((name) => name.endsWith(".plutus.json"));
    assert.ok(generatedNames.length > 0);
    assert.equal(result.validatorCount, generatedNames.length);
    assert.equal(new Set(result.titles).size, generatedNames.length);

    const blueprint = JSON.parse(await readFile(outputPath, "utf8"));
    assert.equal(blueprint.preamble.compiler.name, "Plutarch");
    assert.equal(blueprint.validators.length, generatedNames.length);
    assert.ok(
      blueprint.validators.every(
        ({ title, compiledCode }) =>
          typeof title === "string" &&
          typeof compiledCode === "string" &&
          /^[0-9a-f]+$/u.test(compiledCode),
      ),
    );
    const stateQueueMint = blueprint.validators.find(
      ({ title }) => title === "state_queue.mint.mint",
    );
    assert.equal(stateQueueMint.parameters.length, 11);
    assert.equal(
      stateQueueMint.parameters[10].title,
      "reference_script_auth_policy_id",
    );
    for (const [title, parameterTitles] of [
      [
        "operator_directory/active_operators.mint.mint",
        [
          "hub_oracle_script_hash",
          "registered_operators_policy_id",
          "retired_operators_policy_id",
        ],
      ],
      [
        "operator_directory/active_operators.spend.spend",
        ["active_operators_mint_script_hash", "hub_oracle_script_hash"],
      ],
      [
        "correction_lock.spend.spend",
        ["hub_oracle_policy_id", "availability_policy_id"],
      ],
      [
        "da_attestation.da_attestation.mint",
        [
          "da_params_policy_id",
          "reference_script_auth_policy_id",
          "availability_policy_id",
          "availability_parameters",
        ],
      ],
      [
        "da_attestation.da_attestation.spend",
        [
          "da_params_policy_id",
          "reference_script_auth_policy_id",
          "availability_policy_id",
          "availability_parameters",
        ],
      ],
      [
        "fraud_proofs/validation_trace/dispute_v1.main.spend",
        [
          "source_validator_script_hash",
          "computation_thread_token_policy_id",
          "hub_oracle",
        ],
      ],
      [
        "fraud_proofs/validation_trace/source_v1.main.spend",
        [
          "game_validator_script_hash",
          "award_validator_script_hash",
          "computation_thread_token_policy_id",
        ],
      ],
      [
        "operator_directory/retired_operators.mint.mint",
        ["hub_oracle_script_hash"],
      ],
      [
        "operator_directory/retired_operators.spend.spend",
        ["retired_operators_mint_script_hash"],
      ],
      ["settlement.mint.mint", ["hub_oracle"]],
      ["settlement.spend.spend", ["hub_oracle", "settlement_policy_id"]],
    ]) {
      const validator = blueprint.validators.find(
        (entry) => entry.title === title,
      );
      assert.deepEqual(
        validator.parameters.map(({ title: parameterTitle }) => parameterTitle),
        parameterTitles,
        title,
      );
    }
    const availabilityMint = blueprint.validators.find(
      ({ title }) =>
        title === "availability_challenge.availability_challenge.mint",
    );
    assert.deepEqual(
      availabilityMint.parameters.map(({ title }) => title),
      ["hub_oracle_policy_id", "reference_script_auth_policy_id", "parameters"],
    );
    const hubOracle = blueprint.validators.find(
      ({ title }) => title === "hub_oracle.mint.mint",
    );
    const hubEnvelope = JSON.parse(
      await readFile(
        new URL(
          "../generated/hub-oracle-mint.unapplied.plutus.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    assert.equal(
      hubOracle.compiledCode,
      unwrapTextEnvelopeCborHex(hubEnvelope.cborHex),
    );
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
