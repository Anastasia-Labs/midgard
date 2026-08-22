import {
  buildMidgardValidationTraceTree,
  openMidgardValidationDispute,
} from "@al-ft/midgard-core";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  PreparedValidationResolutionDatumV1,
  type PreparedValidationResolutionDatumV1 as PreparedValidationResolutionDatumV1Data,
  ValidationAwardSpendRedeemerV1,
  type ValidationAwardSpendRedeemerV1 as ValidationAwardSpendRedeemerV1Data,
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1,
  type ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1 as ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Data,
  ValidationCekMaterialRouteV1,
  type ValidationCekMaterialRouteV1 as ValidationCekMaterialRouteV1Data,
  ValidationCekSpendRedeemerV1,
  type ValidationCekSpendRedeemerV1 as ValidationCekSpendRedeemerV1Data,
  ValidationDirectResolveSpendRedeemerV1,
  type ValidationDirectResolveSpendRedeemerV1 as ValidationDirectResolveSpendRedeemerV1Data,
  validationDisputeDataFromCore,
  ValidationDisputeTurnV1Schema,
  ValidationDisputeV1,
  ValidationGameSpendRedeemerV1,
  ValidationMachinePhaseV1Schema,
  ValidationMachineSourceKindV1Schema,
  type ValidationMachineStateV1,
  ValidationMachineVerdictV1Schema,
  ValidationOneStepEvidenceV1,
  type ValidationOneStepEvidenceV1 as ValidationOneStepEvidenceV1Data,
  ValidationOneStepWitnessV1,
  ValidationPrepareSelectedSpendRedeemerV1,
  type ValidationPrepareSelectedSpendRedeemerV1 as ValidationPrepareSelectedSpendRedeemerV1Data,
  type ValidationResolutionStateV1,
  validationTraceDescriptorDataFromCore,
  validationTraceProofDataFromCore,
  ValidationTraceProofV1,
  WinningValidationResolutionDatumV1,
  type WinningValidationResolutionDatumV1 as WinningValidationResolutionDatumV1Data,
} from "../src/index.js";

const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

describe("validation dispute ABI", () => {
  it("freezes every dispute-turn, phase, verdict, and source-kind tag", () => {
    const nullaryVectors = [
      ...[
        "CanonicalDecode",
        "CompactBinding",
        "StaticLedgerRules",
        "InputSets",
        "Signatures",
        "PhaseANativeScripts",
        "PhaseAScriptPreconditions",
        "ResolveInputs",
        "ScriptSources",
        "NativeScripts",
        "ScriptIntegrity",
        "Cek",
        "ValueAndMint",
        "LedgerDelta",
        "Terminal",
      ].map((value, tag) => ({
        value,
        schema: ValidationMachinePhaseV1Schema,
        expected:
          tag <= 6
            ? `d8${(0x79 + tag).toString(16)}80`
            : `d905${(tag - 7).toString(16).padStart(2, "0")}80`,
      })),
      ...["Pending", "Accepted", "Rejected"].map((value, tag) => ({
        value,
        schema: ValidationMachineVerdictV1Schema,
        expected: `d8${(0x79 + tag).toString(16)}80`,
      })),
      ...["Normal", "Forced"].map((value, tag) => ({
        value,
        schema: ValidationMachineSourceKindV1Schema,
        expected: `d8${(0x79 + tag).toString(16)}80`,
      })),
    ] as const;
    for (const { value, schema, expected } of nullaryVectors) {
      expect(Data.to(value as never, schema as never)).toBe(expected);
    }

    expect(
      Data.to(
        { AwaitingOperator: { midpoint: 1n } } as never,
        ValidationDisputeTurnV1Schema as never,
      ),
    ).toBe("d8799f01ff");
    expect(
      Data.to(
        {
          AwaitingChallenger: {
            midpoint: 1n,
            operator_midpoint_hash: "aa".repeat(32),
          },
        } as never,
        ValidationDisputeTurnV1Schema as never,
      ),
    ).toBe(`d87a9f015820${"aa".repeat(32)}ff`);
    expect(
      Data.to(
        "ReadyForOneStep" as never,
        ValidationDisputeTurnV1Schema as never,
      ),
    ).toBe("d87b80");

    expect(() => Data.from("d87c80", ValidationDisputeTurnV1Schema)).toThrow();
    expect(() =>
      Data.from("d8799f0102ff", ValidationDisputeTurnV1Schema),
    ).toThrow();
    expect(() =>
      Data.from("d9050f80", ValidationMachinePhaseV1Schema),
    ).toThrow();
  });

  it("round-trips exact descriptors, proofs, disputes, and reveal redeemers", () => {
    const operator = buildMidgardValidationTraceTree(
      [hash(1), hash(2), hash(3)],
      "accepted",
    );
    const challenger = buildMidgardValidationTraceTree(
      [hash(1), hash(2), hash(4)],
      "accepted",
    );
    const dispute = openMidgardValidationDispute({
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      currentTime: 1_000,
    });
    const disputeData = validationDisputeDataFromCore(dispute);
    expect(
      Data.from(Data.to(disputeData, ValidationDisputeV1), ValidationDisputeV1),
    ).toEqual(disputeData);

    const proofData = validationTraceProofDataFromCore(operator.proofs[1]!);
    expect(
      Data.from(
        Data.to(proofData, ValidationTraceProofV1),
        ValidationTraceProofV1,
      ),
    ).toEqual(proofData);

    const redeemer: ValidationGameSpendRedeemerV1 = {
      Continue: [
        {
          RevealOperator: {
            input_index: 0n,
            output_index: 0n,
            proof: proofData,
          },
        },
      ],
    };
    expect(
      Data.from(
        Data.to(redeemer, ValidationGameSpendRedeemerV1),
        ValidationGameSpendRedeemerV1,
      ),
    ).toEqual(redeemer);

    expect(validationTraceDescriptorDataFromCore(operator.descriptor)).toEqual(
      disputeData.operator_descriptor,
    );
  });

  it("round-trips staged, direct, and award resolution V1 shapes", () => {
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
    const resolution: ValidationResolutionStateV1 = {
      version: 1n,
      pre_state: state,
      operator_successor_hash: "08".repeat(32),
      challenger_successor_hash: "09".repeat(32),
    };
    const transition = {
      work_witness_cbor: "8100",
      claimed_successor: { ...state, program_counter: 1n },
    };
    const auxiliary = "NoAuxiliaryWitness" as const;
    const evidence: ValidationOneStepEvidenceV1Data = {
      transition,
      auxiliary,
    };
    expect(
      Data.from(
        Data.to(transition, ValidationOneStepWitnessV1),
        ValidationOneStepWitnessV1,
      ),
    ).toEqual(transition);
    const prepared: PreparedValidationResolutionDatumV1Data = {
      fraud_prover: "0a".repeat(28),
      data: {
        version: 1n,
        resolution,
        evidence_hash: "0b".repeat(32),
      },
    };
    expect(
      Data.from(
        Data.to(prepared, PreparedValidationResolutionDatumV1),
        PreparedValidationResolutionDatumV1,
      ),
    ).toEqual(prepared);

    const prepareSelected: ValidationPrepareSelectedSpendRedeemerV1Data = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          semantic_resolver_index: 0n,
          transition,
          auxiliary,
        },
      ],
    };
    expect(
      Data.from(
        Data.to(prepareSelected, ValidationPrepareSelectedSpendRedeemerV1),
        ValidationPrepareSelectedSpendRedeemerV1,
      ),
    ).toEqual(prepareSelected);
    // Option B (#620): the canonical-decode prepare redeemer is transition-only
    // — one four-field constructor, no auxiliary, and no retired by-hash arm.
    const canonicalDecodePrepareSelected: ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Data =
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            semantic_resolver_index: 0n,
            transition,
          },
        ],
      };
    const canonicalDecodePrepareSelectedCbor = Data.to(
      canonicalDecodePrepareSelected,
      ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1,
    );
    expect(
      Data.from(
        canonicalDecodePrepareSelectedCbor,
        ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1,
      ),
    ).toEqual(canonicalDecodePrepareSelected);
    // Wire pin: `Continue(PrepareSelected{input, output, resolver, transition})`
    // — constructor 1 wrapping constructor 0 with exactly four fields.
    expect(canonicalDecodePrepareSelectedCbor).toBe(
      Data.to(
        new Constr(1, [
          new Constr(0, [
            0n,
            0n,
            0n,
            Data.from(Data.to(transition, ValidationOneStepWitnessV1)),
          ]),
        ]),
      ),
    );

    const direct: ValidationDirectResolveSpendRedeemerV1Data = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
          challenger_evidence: evidence,
        },
      ],
    };
    expect(
      Data.from(
        Data.to(direct, ValidationDirectResolveSpendRedeemerV1),
        ValidationDirectResolveSpendRedeemerV1,
      ),
    ).toEqual(direct);
    const cekRoutes = [
      "NoCekMaterial",
      {
        DirectCekMaterial: {
          envelope_cbor: "0102",
          sidecar_cbor: "0304",
        },
      },
      {
        SinglePublicationCekMaterial: {
          envelope_cbor: "0102",
          reference_input_index: 2n,
        },
      },
      {
        MinimumMultiOutputCekMaterial: {
          envelope_cbor: "0102",
          reference_input_indices: [4n, 1n],
        },
      },
      {
        IncrementalCekMaterial: {
          program_envelope_hash: "0c".repeat(32),
        },
      },
    ] satisfies readonly ValidationCekMaterialRouteV1Data[];
    for (const [tag, material_route] of cekRoutes.entries()) {
      const cek: ValidationCekSpendRedeemerV1Data = {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            fraud_proof_mint_redeemer_index: 0n,
            challenger_evidence: evidence,
            material_route,
          },
        ],
      };
      const cbor = Data.to(cek, ValidationCekSpendRedeemerV1);
      expect(Data.from(cbor, ValidationCekSpendRedeemerV1)).toEqual(cek);
      const outer = Data.from(cbor) as { readonly fields: readonly unknown[] };
      const action = outer.fields[0] as {
        readonly fields: readonly unknown[];
      };
      const route = action.fields[4] as { readonly index: number };
      expect(route.index).toBe(tag);
    }
    expect(
      Data.from(
        Data.to(direct, ValidationDirectResolveSpendRedeemerV1),
        ValidationDirectResolveSpendRedeemerV1,
      ),
    ).toEqual(direct);

    const materialRouteVectors = [
      ["NoCekMaterial", "d87980"],
      [
        {
          DirectCekMaterial: {
            envelope_cbor: "0102",
            sidecar_cbor: "0304",
          },
        },
        "d87a9f420102420304ff",
      ],
      [
        {
          SinglePublicationCekMaterial: {
            envelope_cbor: "0102",
            reference_input_index: 2n,
          },
        },
        "d87b9f42010202ff",
      ],
      [
        {
          MinimumMultiOutputCekMaterial: {
            envelope_cbor: "0102",
            reference_input_indices: [4n, 1n],
          },
        },
        "d87c9f4201029f0401ffff",
      ],
      [
        {
          IncrementalCekMaterial: {
            program_envelope_hash: "0c".repeat(32),
          },
        },
        `d87d9f5820${"0c".repeat(32)}ff`,
      ],
    ] satisfies readonly (readonly [
      ValidationCekMaterialRouteV1Data,
      string,
    ])[];
    for (const [route, expectedCbor] of materialRouteVectors) {
      expect(Data.to(route, ValidationCekMaterialRouteV1)).toBe(expectedCbor);
      expect(Data.from(expectedCbor, ValidationCekMaterialRouteV1)).toEqual(
        route,
      );
    }

    const cancel: ValidationCekSpendRedeemerV1Data = {
      Cancel: {
        input_index: 1n,
        computation_thread_mint_redeemer_index: 2n,
      },
    };
    expect(Data.to(cancel, ValidationCekSpendRedeemerV1)).toBe("d8799f0102ff");
    const cekDirect = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
          challenger_evidence: evidence,
          material_route: "NoCekMaterial",
        },
      ],
    } satisfies ValidationCekSpendRedeemerV1Data;
    const evidenceCbor = Data.to(evidence, ValidationOneStepEvidenceV1);
    expect(Data.to(cekDirect, ValidationCekSpendRedeemerV1)).toBe(
      `d87a9fd8799f000000${evidenceCbor}d87980ffff`,
    );

    for (const malformed of [
      "d87e80", // adjacent route tag 5
      "d87a9f420102ff", // DirectCekMaterial wrong arity
      "d87c9f42010201ff", // multi indices field is not a list
    ]) {
      expect(() =>
        Data.from(malformed, ValidationCekMaterialRouteV1),
      ).toThrow();
    }
    for (const malformed of [
      "d87b80", // adjacent outer StepRedeemer tag 2
      "d8799f01ff", // Cancel wrong arity
      "d87a80", // Continue wrong arity
    ]) {
      expect(() =>
        Data.from(malformed, ValidationCekSpendRedeemerV1),
      ).toThrow();
    }
    expect(
      Data.from(
        Data.to(evidence, ValidationOneStepEvidenceV1),
        ValidationOneStepEvidenceV1,
      ),
    ).toEqual(evidence);

    const winning: WinningValidationResolutionDatumV1Data = {
      fraud_prover: prepared.fraud_prover,
      data: { version: 1n },
    };
    expect(
      Data.from(
        Data.to(winning, WinningValidationResolutionDatumV1),
        WinningValidationResolutionDatumV1,
      ),
    ).toEqual(winning);

    const award: ValidationAwardSpendRedeemerV1Data = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
        },
      ],
    };
    expect(
      Data.from(
        Data.to(award, ValidationAwardSpendRedeemerV1),
        ValidationAwardSpendRedeemerV1,
      ),
    ).toEqual(award);
  });
});
