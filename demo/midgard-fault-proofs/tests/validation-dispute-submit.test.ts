import { readFileSync } from "node:fs";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

import {
  ValidationAwardSpendRedeemerV1,
  ValidationDirectResolveSpendRedeemerV1,
  type ValidationMachineStateV1,
  ValidationOneStepWitnessV1,
  ValidationPrepareSelectedSpendRedeemerV1,
} from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { parseExactAikenDataCbor } from "../src/aiken-blueprint-data.js";
import { readValidationDisputeCborFile } from "../src/validation-dispute/from-files.js";
import {
  encodeValidationSemanticResolutionRedeemerV1,
  validationDisputeTimeoutValidityRange,
  validationDisputeValidityRange,
  validationOneStepEvidenceHashV1,
} from "../src/validation-dispute/submit.js";

const blueprint = JSON.parse(
  readFileSync(
    resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
    "utf8",
  ),
) as unknown;

describe("validation-dispute transaction validity", () => {
  it("uses a bounded closed range with the validator timestamp at its upper bound", () => {
    expect(validationDisputeValidityRange(1_000_000)).toEqual({
      validFrom: 940_000,
      validTo: 1_060_000,
    });
  });

  it("places timeout lower bound strictly after the response deadline", () => {
    expect(validationDisputeTimeoutValidityRange(1_000_000, 990_000)).toEqual({
      validFrom: 990_001,
      validTo: 1_060_000,
    });
    expect(() =>
      validationDisputeTimeoutValidityRange(1_000_000, 1_000_000),
    ).toThrow(/has not passed/);
  });

  it("hashes exact canonical one-step evidence and rejects ambiguous data", () => {
    const emptyConstructor = Buffer.from("d87980", "hex");
    expect(
      validationOneStepEvidenceHashV1({
        transitionCbor: emptyConstructor,
        auxiliaryCbor: emptyConstructor,
      }),
    ).toBe(
      "a9ee2618651193d3a6c6c658f3f3d19f6a296103ac660e0071b45d903bc1e192",
    );
    expect(() =>
      validationOneStepEvidenceHashV1({
        transitionCbor: Buffer.from("d8799fff", "hex"),
        auxiliaryCbor: emptyConstructor,
      }),
    ).toThrow(/not exact canonical V1 Plutus Data/u);
    expect(() =>
      validationOneStepEvidenceHashV1({
        transitionCbor: new Uint8Array(),
        auxiliaryCbor: emptyConstructor,
      }),
    ).toThrow(/non-empty/u);
    expect(() =>
      validationOneStepEvidenceHashV1({
        transitionCbor: new Uint8Array(16 * 1024),
        auxiliaryCbor: emptyConstructor,
      }),
    ).toThrow(/strictly below the L1 proof envelope/u);
  });

  it("matches the exact prepare, direct, and award Aiken redeemer ABIs", () => {
    const state: ValidationMachineStateV1 = {
      machine_version: 1n,
      event_key_hash: "01".repeat(32),
      transaction_id: "02".repeat(32),
      transaction_commitment: "03".repeat(32),
      validation_context_hash: "04".repeat(32),
      source_kind: "Forced",
      prior_ledger_root: "05".repeat(32),
      phase: "CanonicalDecode",
      program_counter: 0n,
      work_root: "06".repeat(32),
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: "00".repeat(32),
      ledger_delta_root: "07".repeat(32),
    };
    const transition = {
      work_witness_cbor: "8100",
      claimed_successor: { ...state, program_counter: 1n },
    };
    const auxiliary = new Constr(0, []);
    const redeemers = [
      {
        definition:
          "midgard/validation_resolver_v1/PrepareSelectedSpendRedeemer",
        cbor: Data.to(
          {
            Continue: [
              {
                input_index: 0n,
                output_index: 0n,
                semantic_resolver_index: 0n,
                transition,
                auxiliary,
              },
            ],
          },
          ValidationPrepareSelectedSpendRedeemerV1,
        ),
      },
      {
        definition: "midgard/validation_resolver_v1/SpendRedeemer",
        cbor: Data.to(
          {
            Continue: [
              {
                input_index: 0n,
                output_index: 0n,
                fraud_proof_mint_redeemer_index: 0n,
                challenger_evidence: { transition, auxiliary },
              },
            ],
          },
          ValidationDirectResolveSpendRedeemerV1,
        ),
      },
      {
        definition: "midgard/validation_award_v1/SpendRedeemer",
        cbor: Data.to(
          {
            Continue: [
              {
                input_index: 0n,
                output_index: 0n,
                fraud_proof_mint_redeemer_index: 0n,
              },
            ],
          },
          ValidationAwardSpendRedeemerV1,
        ),
      },
    ] as const;
    for (const redeemer of redeemers) {
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName: redeemer.definition,
          cbor: redeemer.cbor,
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }

    const transitionCbor = Buffer.from(
      Data.to(transition, ValidationOneStepWitnessV1),
      "hex",
    );
    const sourceFields = [
      0n,
      0n,
      "00",
      3n,
      "11".repeat(28),
      100n,
      "22".repeat(32),
      [],
    ] as const;
    const redeemerItemProof = new Constr(0, [
      1n,
      8n,
      1n,
      0n,
      1n,
      "22".repeat(32),
      [],
      [],
    ]);
    const redeemerWitness = new Constr(0, [
      new Constr(0, []),
      0n,
      "00",
      new Constr(0, [1n, 1n]),
    ]);
    for (const selected of [
      {
        index: 10,
        auxiliary: new Constr(14, [...sourceFields]),
        module: "script_sources_stage_nine_mismatch_semantic_v1",
      },
      {
        index: 11,
        auxiliary: new Constr(14, [
          ...sourceFields.slice(0, 3),
          0n,
          ...sourceFields.slice(4),
        ]),
        module: "script_sources_stage_nine_native_match_semantic_v1",
      },
      {
        index: 12,
        auxiliary: new Constr(14, [...sourceFields]),
        module:
          "script_sources_stage_nine_effectful_match_semantic_v1",
      },
      {
        index: 13,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_nine_missing_semantic_v1",
      },
      {
        index: 14,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_one_finish_semantic_v1",
      },
      {
        index: 15,
        auxiliary: new Constr(34, [
          redeemerItemProof,
          redeemerWitness,
        ]),
        module: "script_sources_stage_one_redeemer_semantic_v1",
      },
      {
        index: 16,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_eleven_finish_semantic_v1",
      },
      {
        index: 17,
        auxiliary: new Constr(14, [...sourceFields]),
        module: "script_sources_stage_eleven_source_semantic_v1",
      },
      {
        index: 18,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_twelve_finish_semantic_v1",
      },
      {
        index: 19,
        auxiliary: new Constr(15, [
          0n,
          redeemerWitness,
          [],
        ]),
        module: "script_sources_stage_twelve_redeemer_semantic_v1",
      },
    ] as const) {
      const auxiliaryCbor = Buffer.from(
        Data.to(selected.auxiliary as never),
        "hex",
      );
      const cbor = encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 8,
          semanticResolverIndex: selected.index,
          transitionCbor,
          auxiliaryCbor,
        },
        inputIndex: 0n,
        outputIndex: 0n,
      });
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName:
            `fraud_proofs/validation_trace/${selected.module}/SpendRedeemer`,
          cbor: cbor.toString("hex"),
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 8,
          semanticResolverIndex: 13,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(new Constr(14, [...sourceFields]) as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(
      "does not match the selected ScriptSources proof family",
    );

    const nativeChunkProof = new Constr(0, [
      1n,
      7n,
      0n,
      3n,
      0n,
      "010203",
      [],
      [],
    ]);
    const nativeDescriptorFields = [
      0n,
      0n,
      0n,
      0n,
      "33".repeat(28),
      "44".repeat(32),
      [],
      0n,
      0n,
      "55".repeat(32),
      3n,
      "66".repeat(32),
      [],
      "",
      [],
      new Constr(0, [nativeChunkProof]),
      [],
    ] as const;
    for (const selected of [
      {
        index: 0,
        auxiliary: new Constr(0, []),
        module: "native_scripts_terminal_semantic_v1",
      },
      {
        index: 1,
        auxiliary: new Constr(42, [...nativeDescriptorFields]),
        module: "native_scripts_native_semantic_v1",
      },
      {
        index: 2,
        auxiliary: new Constr(42, [
          nativeDescriptorFields[0],
          3n,
          ...nativeDescriptorFields.slice(2, 15),
          new Constr(1, []),
          [],
        ]),
        module: "native_scripts_effectful_semantic_v1",
      },
    ] as const) {
      const cbor = encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 9,
          semanticResolverIndex: selected.index,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(selected.auxiliary as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      });
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName:
            `fraud_proofs/validation_trace/${selected.module}/SpendRedeemer`,
          cbor: cbor.toString("hex"),
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 9,
          semanticResolverIndex: 2,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(
              new Constr(42, [...nativeDescriptorFields]) as never,
            ),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(
      "validation NativeScripts effectful first chunk must be constructor 1 with 0 fields",
    );
  });

  it("reads exact lowercase CBOR files and rejects ambiguous wrappers", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-dispute-cbor-"));
    const rawPath = join(directory, "raw.cbor");
    const wrappedPath = join(directory, "wrapped.json");
    const ambiguousPath = join(directory, "ambiguous.json");
    await Promise.all([
      writeFile(rawPath, "d87980\n"),
      writeFile(wrappedPath, '{"cborHex":"d87980"}\n'),
      writeFile(ambiguousPath, '{"cborHex":"d87980","unexpected":true}\n'),
    ]);
    await expect(
      readValidationDisputeCborFile(rawPath, "fixture"),
    ).resolves.toBe("d87980");
    await expect(
      readValidationDisputeCborFile(wrappedPath, "fixture"),
    ).resolves.toBe("d87980");
    await expect(
      readValidationDisputeCborFile(ambiguousPath, "fixture"),
    ).rejects.toThrow(/exactly a cborHex field/u);
  });
});
