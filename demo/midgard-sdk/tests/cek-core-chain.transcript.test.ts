/** The CEK core chain's wiring, as `buildCekCoreStages` applies it over the
 * real blueprint: each stage's validator and parameters, with every stage
 * hash a parameter names written as that stage's role and the builder's
 * fixed inputs by name, then the entry stage of each core group and the hop
 * count the settle carries for it. The transcript holds no hash, so a change
 * to the validators' code leaves it as it is, while a change to the wiring
 * changes it; the same transcript over the same blueprint applies the same
 * scripts. */
import { readFileSync } from "node:fs";

import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  blueprintApplicationRecords,
  buildCekCoreStages,
  CEK_CORE_ARM_HOP_COUNTS,
  CEK_CORE_STAGE_TITLES,
  cekCoreEntryHashes,
  parseFaultProofBlueprint,
} from "../src/index.js";

const FIXED_INPUTS = {
  award: "a0".repeat(28),
  computationThreadPolicy: "b0".repeat(28),
  threadExpiry: "c0".repeat(28),
} as const;

const TRANSCRIPT = [
  "blsBudget fraud_proofs/validation_trace/cek_core_bls_budget.main.spend 581c<blsRoots> 581c<computationThreadPolicy>",
  "blsFinal fraud_proofs/validation_trace/cek_core_bls_final.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "blsRoots fraud_proofs/validation_trace/cek_core_bls_roots.main.spend 581c<blsFinal> 581c<computationThreadPolicy>",
  "caseConstant fraud_proofs/validation_trace/cek_core_arm_case_constant.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "compute fraud_proofs/validation_trace/cek_core_arm_compute.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "directScalar fraud_proofs/validation_trace/cek_core_direct_scalar.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "directScalarBudget fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend 9f581c<directScalar>ff 581c<computationThreadPolicy>",
  "directScalarRoots fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend 581c<directScalarBudget> 581c<computationThreadPolicy>",
  "directStructured fraud_proofs/validation_trace/cek_core_direct_structured.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "directStructuredBudget fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend 9f581c<directStructured>ff 581c<computationThreadPolicy>",
  "directStructuredRoots fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend 581c<directStructuredBudget> 581c<computationThreadPolicy>",
  "failureBlsUncompress fraud_proofs/validation_trace/cek_core_failure_bls_uncompress.main.spend 581c<failureBudget> 581c<computationThreadPolicy>",
  "failureBudget fraud_proofs/validation_trace/cek_core_failure_budget.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "failureKnown fraud_proofs/validation_trace/cek_core_failure_known.main.spend 581c<failureBudget> 581c<computationThreadPolicy>",
  "machine fraud_proofs/validation_trace/cek_core_arm_machine.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "mapConversion fraud_proofs/validation_trace/cek_core_arm_map_conversion.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "mapStartBudget fraud_proofs/validation_trace/cek_core_map_start_budget.main.spend 581c<mapStartNodes> 581c<computationThreadPolicy>",
  "mapStartNodes fraud_proofs/validation_trace/cek_core_map_start_nodes.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "mapStartRoots fraud_proofs/validation_trace/cek_core_map_start_roots.main.spend 581c<mapStartBudget> 581c<computationThreadPolicy>",
  "pv11Budget fraud_proofs/validation_trace/cek_core_pv11_budget.main.spend 581c<pv11Semantics> 581c<computationThreadPolicy>",
  "pv11Roots fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend 581c<pv11Budget> 581c<computationThreadPolicy>",
  "pv11Semantics fraud_proofs/validation_trace/cek_core_pv11_semantics.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticBudget fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend 9f581c<semanticPair>581c<semanticListConstruct>581c<semanticListSelect>581c<semanticChoose>581c<semanticDataConstruct>581c<semanticDataScalar>581c<semanticDataMisc>581c<stagedBytes>581c<stagedInteger>581c<stagedProduct>581c<semanticSerialiseLoop>581c<stagedConversion>581c<stagedWriteBitsLoop>ff 581c<computationThreadPolicy>",
  "semanticChoose fraud_proofs/validation_trace/cek_core_semantic_choose.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticDataConstruct fraud_proofs/validation_trace/cek_core_semantic_data_construct.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticDataMisc fraud_proofs/validation_trace/cek_core_semantic_data_misc.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticDataScalar fraud_proofs/validation_trace/cek_core_semantic_data_scalar.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticFailureMaterial fraud_proofs/validation_trace/cek_core_semantic_failure_material.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticFailureRoots fraud_proofs/validation_trace/cek_core_semantic_failure_roots.main.spend 581c<semanticFailureMaterial> 581c<stagedFailure> 581c<stagedWriteBitsLoop> 581c<computationThreadPolicy>",
  "semanticListConstruct fraud_proofs/validation_trace/cek_core_semantic_list_construct.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticListSelect fraud_proofs/validation_trace/cek_core_semantic_list_select.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticPair fraud_proofs/validation_trace/cek_core_semantic_pair.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "semanticResult fraud_proofs/validation_trace/cek_core_semantic_result.main.spend 581c<semanticBudget> 581c<computationThreadPolicy>",
  "semanticRoots fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend 581c<semanticResult> 581c<computationThreadPolicy>",
  "semanticSerialiseLoop fraud_proofs/validation_trace/cek_core_semantic_serialise_loop.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "settle fraud_proofs/validation_trace/cek_core_settle.main.spend 581c<award> 9f010101030304030203020201020302ff 581c<computationThreadPolicy>",
  "stagedBytes fraud_proofs/validation_trace/cek_core_staged_bytes.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "stagedConversion fraud_proofs/validation_trace/cek_core_staged_conversion.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "stagedFailure fraud_proofs/validation_trace/cek_core_staged_failure.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "stagedInteger fraud_proofs/validation_trace/cek_core_staged_integer.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "stagedProduct fraud_proofs/validation_trace/cek_core_staged_product.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "stagedWriteBitsLoop fraud_proofs/validation_trace/cek_core_staged_write_bits_loop.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "typeFailureKinds fraud_proofs/validation_trace/cek_core_type_failure_kinds.main.spend 581c<settle> 581c<computationThreadPolicy>",
  "typeFailureRoots fraud_proofs/validation_trace/cek_core_type_failure_roots.main.spend 581c<typeFailureKinds> 581c<computationThreadPolicy>",
  "entries compute machine mapConversion directScalarRoots directStructuredRoots semanticRoots mapStartRoots semanticFailureRoots blsBudget failureKnown typeFailureRoots caseConstant failureBlsUncompress pv11Roots pv11Budget",
  "hops 1 1 1 3 3 4 3 2 3 2 2 1 2 3 2",
];

const applied = () => {
  const blueprint = parseFaultProofBlueprint(
    JSON.parse(
      readFileSync(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ),
  );
  const stages = buildCekCoreStages(
    blueprint,
    "Preprod",
    FIXED_INPUTS.award,
    FIXED_INPUTS.computationThreadPolicy,
    FIXED_INPUTS.threadExpiry,
  );
  const records = new Map(
    blueprintApplicationRecords(blueprint).map((record) => [
      validatorToScriptHash({
        type: "PlutusV3",
        script: record.appliedScriptCborHex,
      }),
      record,
    ]),
  );
  return { stages, records };
};

describe("the CEK core chain's wiring", () => {
  const { stages, records } = applied();
  const names = new Map<string, string>([
    ...Object.entries(stages).map(
      ([role, { spendingScriptHash }]) => [spendingScriptHash, role] as const,
    ),
    ...Object.entries(FIXED_INPUTS).map(
      ([name, hash]) => [hash, name] as const,
    ),
  ]);
  const symbolic = (cborHex: string): string =>
    [...names].reduce(
      (text, [hash, name]) => text.split(hash).join(`<${name}>`),
      cborHex,
    );

  it("names each stage by the hash of its own applied script, one stage a hash", () => {
    expect(names.size).toBe(Object.keys(stages).length + 3);
    for (const [role, { spendingScriptHash }] of Object.entries(stages))
      expect(records.get(spendingScriptHash)?.validatorTitle, role).toBe(
        CEK_CORE_STAGE_TITLES[role as keyof typeof CEK_CORE_STAGE_TITLES],
      );
  });

  it("applies each stage's validator to the parameters of the transcript", () => {
    const transcript = [
      ...Object.entries(stages)
        .map(([role, { spendingScriptHash }]) => {
          const record = records.get(spendingScriptHash)!;
          return [
            role,
            record.validatorTitle,
            ...record.parameters.map(({ cborHex }) => symbolic(cborHex)),
          ].join(" ");
        })
        .sort(),
      `entries ${cekCoreEntryHashes(stages)
        .map((hash) => names.get(hash))
        .join(" ")}`,
      `hops ${CEK_CORE_ARM_HOP_COUNTS.join(" ")}`,
    ];
    expect(transcript).toEqual(TRANSCRIPT);
  });
});
