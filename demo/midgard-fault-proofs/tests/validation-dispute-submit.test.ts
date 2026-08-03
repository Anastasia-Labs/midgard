import { readFileSync } from "node:fs";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

import {
  buildMidgardValidationTraceTree,
  MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS,
} from "@al-ft/midgard-core";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  Proof,
  ValidationAwardSpendRedeemerV1,
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema,
  type ValidationMachineStateV1,
  ValidationOneStepWitnessV1,
  ValidationPrepareSelectedSpendRedeemerV1Schema,
} from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { parseExactAikenDataCbor } from "../src/aiken-blueprint-data.js";
import { readValidationDisputeCborFile } from "../src/validation-dispute/from-files.js";
import {
  encodeScriptSourcesStageOneSpendRedeemerV1,
  encodeValidationDirectResolveSpendRedeemerV1,
  encodeValidationSemanticResolutionRedeemerV1,
  openValidationDisputeAfterSourceVerification,
  refreshExpiredValidationDisputeValidityRange,
  requireValidationItemSemanticReferenceScriptOutRef,
  selectValidationCompleteItemCarriageV1,
  VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  validationDisputeTimeoutValidityRange,
  validationDisputeValidityRange,
  validationOneStepEvidenceHashV1,
  validationSemanticResolverGlobalIndexV1,
} from "../src/validation-dispute/submit.js";

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown;
const encodeRuntimeSchema = Data.to as unknown as (
  value: unknown,
  schema: unknown,
) => string;

describe("validation-dispute transaction validity", () => {
  it("uses a bounded closed range with the validator timestamp at its upper bound", () => {
    expect(validationDisputeValidityRange(1_000_000)).toEqual({
      validFrom: 940_000,
      validTo: 1_060_000,
    });
  });

  it("refreshes an expired staged range against current ledger time", () => {
    const range = { validFrom: 940_000, validTo: 1_060_000 };
    expect(
      refreshExpiredValidationDisputeValidityRange({
        range,
        currentLedgerTime: 1_059_999,
      }),
    ).toBe(range);
    expect(
      refreshExpiredValidationDisputeValidityRange({
        range,
        currentLedgerTime: 1_080_000,
      }),
    ).toEqual({
      validFrom: 1_020_000,
      validTo: 1_140_000,
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

  it("selects direct then automatic reference carriage at measured boundaries", () => {
    const direct =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const publication =
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes;
    expect(selectValidationCompleteItemCarriageV1(direct)).toBe("direct");
    expect(selectValidationCompleteItemCarriageV1(direct + 1)).toBe(
      "reference",
    );
    expect(selectValidationCompleteItemCarriageV1(publication)).toBe(
      "reference",
    );
    expect(() =>
      selectValidationCompleteItemCarriageV1(publication + 1),
    ).toThrow(/single-publication envelope/u);
  });

  it("requires the published item-semantic reference script from deployment info", () => {
    const scriptHash = "ab".repeat(28);
    const otherScriptHash = "cd".repeat(28);
    const refScriptUTxO = { txHash: "12".repeat(32), outputIndex: 3 };
    expect(
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash,
            refScriptUTxO,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toEqual(refScriptUTxO);
    expect(() =>
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {},
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(
      /missing "validationTraceDisputeItemSemantic"; publish the V1 canonical-decode item-semantic reference script/u,
    );
    expect(() =>
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash,
            refScriptUTxO: null,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/is missing refScriptUTxO; publish the V1 canonical-decode/u);
    expect(() =>
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash: otherScriptHash,
            refScriptUTxO,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/script hash mismatch/u);
  });

  it("starts the response deadline at the authenticated source upper bound", () => {
    const operator = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 3)],
      "accepted",
    );
    const challenger = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 4)],
      "accepted",
    );
    const sourceValidityRange = {
      validFrom: 1_000_000,
      validTo: 1_000_101,
    };

    const dispute = openValidationDisputeAfterSourceVerification({
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      openTimeUpper: 1_000_000n,
      challengedBlockEndTime: 1_000_000n,
      sourceValidityRange,
    });

    expect(dispute.responseDeadline).toBe(
      sourceValidityRange.validTo -
        1 +
        MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS,
    );
  });

  it("rejects absent, invalid, time-travelling, and stale source timing", () => {
    const operator = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 3)],
      "accepted",
    );
    const challenger = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 4)],
      "accepted",
    );
    const base = {
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      openTimeUpper: 1_000_000n,
      challengedBlockEndTime: 1_000_000n,
    };

    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        sourceValidityRange: undefined as never,
      }),
    ).toThrow(/validity range/u);
    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        sourceValidityRange: { validFrom: 1_000_000, validTo: 1_000_000 },
      }),
    ).toThrow(/validity range/u);
    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        openTimeUpper: 1_000_100n,
        sourceValidityRange: { validFrom: 999_900, validTo: 1_000_001 },
      }),
    ).toThrow(/cannot precede/u);
    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        challengedBlockEndTime: 0n,
        sourceValidityRange: {
          validFrom: MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs - 100,
          validTo: MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs + 1,
        },
      }),
    ).toThrow(/cannot complete before the challenged block matures/u);
  });

  it("hashes exact canonical one-step evidence and rejects ambiguous data", () => {
    const emptyConstructor = Buffer.from("d87980", "hex");
    expect(
      validationOneStepEvidenceHashV1({
        transitionCbor: emptyConstructor,
        auxiliaryCbor: emptyConstructor,
      }),
    ).toBe("a9ee2618651193d3a6c6c658f3f3d19f6a296103ac660e0071b45d903bc1e192");
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
    const auxiliary = "NoAuxiliaryWitness" as const;
    const redeemers = [
      {
        definition:
          "midgard/validation_resolver_v1/PrepareSelectedSpendRedeemer",
        cbor: encodeRuntimeSchema(
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
          ValidationPrepareSelectedSpendRedeemerV1Schema,
        ),
      },
      {
        definition:
          "fraud_proofs/validation_trace/canonical_decode_v1/SpendRedeemer",
        cbor: encodeRuntimeSchema(
          {
            Continue: [
              {
                PrepareSelectedByEvidenceHash: {
                  input_index: 0n,
                  output_index: 0n,
                  semantic_resolver_index: 0n,
                  transition,
                  evidence_hash: "09".repeat(32),
                },
              },
            ],
          },
          ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema,
        ),
      },
      {
        definition: "midgard/validation_resolver_v1/SpendRedeemer",
        cbor: encodeValidationDirectResolveSpendRedeemerV1({
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
          challenger_evidence: { transition, auxiliary },
        }),
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
    const completeCollectionProof = new Constr(0, [
      1n,
      0n,
      1n,
      0n,
      1n,
      "08".repeat(32),
      [],
      [],
    ]);
    const completeItemAuxiliaryCbor = Buffer.from(
      Data.to(new Constr(30, [completeCollectionProof, "00"]) as never),
      "hex",
    );
    for (const proofItemReferenceInputIndex of [undefined, 0n] as const) {
      const cbor = encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 0,
          semanticResolverIndex: 1,
          transitionCbor,
          auxiliaryCbor: completeItemAuxiliaryCbor,
        },
        inputIndex: 0n,
        outputIndex: 0n,
        proofItemReferenceInputIndex,
      });
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName:
            "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/SpendRedeemer",
          cbor: cbor.toString("hex"),
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 0,
          semanticResolverIndex: 1,
          transitionCbor,
          auxiliaryCbor: Buffer.from(Data.to(new Constr(0, [])), "hex"),
        },
        inputIndex: 0n,
        outputIndex: 0n,
        proofItemReferenceInputIndex: 0n,
      }),
    ).toThrow(/complete item/u);
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
    const redeemerChunkProof = new Constr(0, [
      1n,
      8n,
      0n,
      8n,
      0n,
      "8400004100820101",
      [],
      [],
    ]);
    const redeemerItemControl = new Constr(0, [
      1n,
      0n,
      0n,
      0n,
      1n,
      8n,
      "22".repeat(32),
      -1n,
      -1n,
      -1n,
      -1n,
      0n,
      0n,
      -1n,
      -1n,
      new Constr(1, []),
    ]);
    const redeemerItemWitness = new Constr(0, [
      new Constr(0, []),
      new Constr(0, [redeemerChunkProof]),
      new Constr(1, []),
    ]);
    for (const selected of [
      {
        index: 10,
        auxiliary: new Constr(9, [...sourceFields]),
        module: "script_sources_stage_nine_mismatch_semantic_v1",
      },
      {
        index: 11,
        auxiliary: new Constr(9, [
          ...sourceFields.slice(0, 3),
          0n,
          ...sourceFields.slice(4),
        ]),
        module: "script_sources_stage_nine_native_match_semantic_v1",
      },
      {
        index: 12,
        auxiliary: new Constr(9, [...sourceFields]),
        module: "script_sources_stage_nine_effectful_match_semantic_v1",
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
        auxiliary: new Constr(29, [redeemerItemProof]),
        module: "script_sources_stage_one_redeemer_semantic_v1",
      },
      {
        index: 16,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_eleven_finish_semantic_v1",
      },
      {
        index: 17,
        auxiliary: new Constr(9, [...sourceFields]),
        module: "script_sources_stage_eleven_source_semantic_v1",
      },
      {
        index: 18,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_twelve_finish_semantic_v1",
      },
      {
        index: 19,
        auxiliary: new Constr(10, [0n, 1n, 8n, "22".repeat(32), []]),
        module: "script_sources_stage_twelve_redeemer_semantic_v1",
      },
      {
        index: 20,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_ten_missing_semantic_v1",
      },
      {
        index: 21,
        auxiliary: new Constr(10, [0n, 1n, 8n, "22".repeat(32), []]),
        module: "script_sources_stage_ten_mismatch_semantic_v1",
      },
      {
        index: 22,
        auxiliary: new Constr(18, [
          new Constr(1, []),
          redeemerItemControl,
          redeemerItemWitness,
        ]),
        module: "script_sources_stage_ten_match_semantic_v1",
      },
      {
        index: 23,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_eight_finish_semantic_v1",
      },
      {
        index: 24,
        auxiliary: new Constr(8, [0n, 0n, "11".repeat(28), "00", []]),
        module: "script_sources_stage_eight_purpose_semantic_v1",
      },
      {
        index: 25,
        auxiliary: new Constr(1, [
          new Constr(0, [1n, 3n, 1n, 0n, 28n, "11".repeat(32), [], []]),
          new Constr(0, [1n, 3n, 0n, 28n, 0n, "11".repeat(28), [], []]),
        ]),
        module: "script_sources_stage_seven_observer_semantic_v1",
      },
      {
        index: 26,
        auxiliary: new Constr(8, [
          3n,
          0n,
          "11".repeat(28),
          "11".repeat(28),
          [],
        ]),
        module: "script_sources_stage_seven_receive_semantic_v1",
      },
      {
        index: 27,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_seven_finish_semantic_v1",
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
          definitionName: `fraud_proofs/validation_trace/${selected.module}/SpendRedeemer`,
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
            Data.to(new Constr(9, [...sourceFields]) as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow("does not match the selected ScriptSources proof family");
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 8,
          semanticResolverIndex: 10,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(new Constr(11, [...sourceFields]) as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow();

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
        auxiliary: new Constr(37, [...nativeDescriptorFields]),
        module: "native_scripts_native_semantic_v1",
      },
      {
        index: 2,
        auxiliary: new Constr(37, [
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
          definitionName: `fraud_proofs/validation_trace/${selected.module}/SpendRedeemer`,
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
            Data.to(new Constr(37, [...nativeDescriptorFields]) as never),
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

  it("maps and encodes the split ScriptSources stage-one route without replacing the legacy route", () => {
    expect(validationSemanticResolverGlobalIndexV1(8, 28)).toBe(75);
    expect(validationSemanticResolverGlobalIndexV1(8, 15)).toBe(47);

    const state: ValidationMachineStateV1 = {
      machine_version: 1n,
      event_key_hash: "01".repeat(32),
      transaction_id: "02".repeat(32),
      transaction_commitment: "03".repeat(32),
      validation_context_hash: "04".repeat(32),
      source_kind: "Forced",
      prior_ledger_root: "05".repeat(32),
      phase: "ScriptSources",
      program_counter: 0n,
      work_root: "06".repeat(32),
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: "00".repeat(32),
      ledger_delta_root: "07".repeat(32),
    };
    const transitionData = Data.from(
      Data.to(
        {
          work_witness_cbor: "8100",
          claimed_successor: { ...state, program_counter: 1n },
        },
        ValidationOneStepWitnessV1,
      ),
    );
    const none = new Constr(1, []);
    const summary = new Constr(0, ["11".repeat(32), 1n, 1n]);
    const sequence = new Constr(0, ["12".repeat(32), 0n, 0n, 0n]);
    const frame = new Constr(0, [
      3n,
      0n,
      "",
      0n,
      0n,
      "",
      1n,
      0n,
      [],
      0n,
      sequence,
    ]);
    const traversalControl = new Constr(0, [
      1n,
      6n,
      0n,
      1n,
      1n,
      "13".repeat(32),
      none,
      none,
      none,
      none,
    ]);
    const itemControl = new Constr(0, [
      1n,
      0n,
      2n,
      0n,
      1n,
      1n,
      "14".repeat(32),
      0n,
      0n,
      0n,
      0n,
      0n,
      1n,
      0n,
      0n,
      new Constr(0, [traversalControl]),
    ]);
    const foldMapAction = new Constr(7, [
      frame,
      0n,
      summary,
      summary,
      [],
      [],
    ]);
    const auxiliary = new Constr(18, [
      none,
      itemControl,
      new Constr(0, [
        new Constr(2, [foldMapAction]),
        none,
        none,
      ]),
    ]);
    if (!(transitionData instanceof Constr)) {
      throw new Error("test transition must be a constructor");
    }
    const resolution = new Constr(0, [
      1n,
      transitionData.fields[1]!,
      "24".repeat(32),
      "25".repeat(32),
    ]);
    const envelope = new Constr(0, [
      1n,
      "15",
      "16".repeat(32),
      new Constr(0, [1n, resolution, "17".repeat(32)]),
      "18".repeat(32),
      0n,
      "19".repeat(32),
      "1a".repeat(32),
      "1b".repeat(32),
      "1c".repeat(32),
      0n,
      1n,
      "1d".repeat(28),
      "1e".repeat(28),
      "1f".repeat(28),
      "20".repeat(28),
      "21".repeat(28),
      "22".repeat(32),
    ]);
    const redeemers = [
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "envelope",
          inputIndex: 0n,
          outputIndex: 0n,
          transition: transitionData,
          auxiliary,
          expectedNextItemControlHash: "23".repeat(32),
          family: 0,
        }),
      },
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "traversal",
          inputIndex: 0n,
          outputIndex: 0n,
          auxiliary,
          currentItemControl: itemControl,
          traversalAction: foldMapAction,
        }),
      },
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "outer",
          inputIndex: 0n,
          outputIndex: 0n,
        }),
      },
      ...[
        "script_sources_stage_one_redeemer_fold_map_executor_v1",
        "script_sources_stage_one_redeemer_finalize_frame_executor_v1",
      ].map((module) => ({
        definition: `fraud_proofs/validation_trace/${module}/SpendRedeemer`,
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "executor",
          inputIndex: 0n,
          outputIndex: 0n,
          traversalAction:
            module.includes("fold_map")
              ? foldMapAction
              : new Constr(8, [frame, none]),
        }),
      })),
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "settlement",
          inputIndex: 0n,
          outputIndex: 0n,
          envelope,
        }),
      },
    ];
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
    expect(() =>
      encodeScriptSourcesStageOneSpendRedeemerV1({
        stage: "envelope",
        inputIndex: 0n,
        outputIndex: 0n,
        transition: transitionData,
        auxiliary,
        expectedNextItemControlHash: "23".repeat(32),
        family: 2,
      }),
    ).toThrow(/FoldMap or FinalizeFrame/u);
  });

  it("emits the deployed 5-field complete-item Verify redeemer with the item unwrapped", () => {
    // C21-DISPUTE-SUBMIT defect 1: the standalone encoder previously wrapped
    // the whole auxiliary into a 4-field Verify action. The deployed
    // canonical_decode_item_semantic_v1 ABI takes 5 fields with
    // collection_proof and item_cbor as separate arguments, item bytes
    // unwrapped. Pin the exact emitted shape and its blueprint parse.
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
    const transitionCbor = Buffer.from(
      Data.to(
        {
          work_witness_cbor: "8100",
          claimed_successor: { ...state, program_counter: 1n },
        },
        ValidationOneStepWitnessV1,
      ),
      "hex",
    );
    const collectionProof = new Constr(0, [
      1n,
      0n,
      1n,
      0n,
      1n,
      "08".repeat(32),
      [],
      [],
    ]);
    const itemCborHex = "0102030405";
    const oneStepArgument = {
      resolverIndex: 0,
      semanticResolverIndex: 1,
      transitionCbor,
      auxiliaryCbor: Buffer.from(
        Data.to(new Constr(30, [collectionProof, itemCborHex]) as never),
        "hex",
      ),
    };
    const directRedeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument,
      inputIndex: 5n,
      outputIndex: 7n,
    });
    const direct = Data.from(directRedeemer.toString("hex"));
    expect(direct).toBeInstanceOf(Constr);
    const directOuter = direct as Constr<unknown>;
    expect(directOuter.index).toBe(1);
    expect(directOuter.fields).toHaveLength(1);
    const directAction = directOuter.fields[0] as Constr<unknown>;
    expect(directAction).toBeInstanceOf(Constr);
    expect(directAction.index).toBe(0);
    expect(directAction.fields).toHaveLength(5);
    expect(directAction.fields[0]).toBe(5n);
    expect(directAction.fields[1]).toBe(7n);
    expect(directAction.fields[2]).toEqual(
      Data.from(transitionCbor.toString("hex")),
    );
    expect(directAction.fields[3]).toEqual(collectionProof);
    expect(directAction.fields[4]).toBe(itemCborHex);
    expect(
      parseExactAikenDataCbor({
        blueprint,
        definitionName:
          "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/SpendRedeemer",
        cbor: directRedeemer.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      }),
    ).toBeInstanceOf(Constr);

    const referenceRedeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument,
      inputIndex: 5n,
      outputIndex: 7n,
      proofItemReferenceInputIndex: 2n,
    });
    const reference = Data.from(referenceRedeemer.toString("hex"));
    const referenceOuter = reference as Constr<unknown>;
    expect(referenceOuter.index).toBe(1);
    expect(referenceOuter.fields).toHaveLength(1);
    const referenceAction = referenceOuter.fields[0] as Constr<unknown>;
    expect(referenceAction.index).toBe(1);
    expect(referenceAction.fields).toHaveLength(4);
    expect(referenceAction.fields[0]).toBe(5n);
    expect(referenceAction.fields[1]).toBe(7n);
    expect(referenceAction.fields[3]).toBe(2n);
    expect(
      parseExactAikenDataCbor({
        blueprint,
        definitionName:
          "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/SpendRedeemer",
        cbor: referenceRedeemer.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      }),
    ).toBeInstanceOf(Constr);
  });

  it("encodes resolver-7 non-membership evidence into the exact semantic ABI", () => {
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
    const transitionCbor = Buffer.from(
      Data.to(
        {
          work_witness_cbor: "8100",
          claimed_successor: { ...state, program_counter: 1n },
        },
        ValidationOneStepWitnessV1,
      ),
      "hex",
    );

    // Canonical divergent-leaf fixture from transition-trace.test.ak; unlike
    // an empty proof, it remains valid when RF-002's terminal-key check is applied.
    const nonMembershipProof: Proof = [
      {
        Leaf: {
          skip: 0n,
          key: "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25",
          value:
            "55951e629cad560ea5f8be280c35d8788ee84324b842fee1b41c546efb62d2d5",
        },
      },
    ];
    const proofData = Data.from(Data.to(nonMembershipProof, Proof));
    const sourceKind = 0n;
    const key = "02";
    const nextScheduleHash = "11".repeat(32);
    const auxiliaryCbor = Buffer.from(
      Data.to(
        new Constr(6, [sourceKind, key, nextScheduleHash, proofData]) as never,
      ),
      "hex",
    );

    const redeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument: {
        resolverIndex: 7,
        semanticResolverIndex: 5,
        transitionCbor,
        auxiliaryCbor,
      },
      inputIndex: 5n,
      outputIndex: 7n,
    });
    const decoded = Data.from(redeemer.toString("hex"));
    expect(decoded).toBeInstanceOf(Constr);
    const outer = decoded as Constr<unknown>;
    expect(outer.index).toBe(1);
    expect(outer.fields).toHaveLength(1);
    const action = outer.fields[0];
    expect(action).toBeInstanceOf(Constr);
    const actionData = action as Constr<unknown>;
    expect(actionData.index).toBe(0);
    expect(actionData.fields).toHaveLength(7);
    expect(actionData.fields[0]).toBe(5n);
    expect(actionData.fields[1]).toBe(7n);
    expect(actionData.fields[2]).toEqual(
      Data.from(transitionCbor.toString("hex")),
    );
    expect(actionData.fields[3]).toBe(sourceKind);
    expect(actionData.fields[4]).toBe(key);
    expect(actionData.fields[5]).toBe(nextScheduleHash);
    expect(actionData.fields[6]).toEqual(proofData);
    expect(
      parseExactAikenDataCbor({
        blueprint,
        definitionName:
          "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1/SpendRedeemer",
        cbor: redeemer.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      }),
    ).toBeInstanceOf(Constr);

    const replayAuxiliaryCbor = Buffer.from(
      Data.to(
        new Constr(7, [sourceKind, key, nextScheduleHash, "00"]) as never,
      ),
      "hex",
    );
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 7,
          semanticResolverIndex: 5,
          transitionCbor,
          auxiliaryCbor: replayAuxiliaryCbor,
        },
        inputIndex: 5n,
        outputIndex: 7n,
      }),
    ).toThrow(/ResolveInputs auxiliary witness/u);
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
