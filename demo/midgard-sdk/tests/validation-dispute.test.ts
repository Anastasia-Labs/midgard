import {
  buildMidgardValidationTraceTree,
  openMidgardValidationDispute,
} from "@al-ft/midgard-core";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  PreparedValidationResolutionDatum,
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  ValidationAwardSpendRedeemer,
  type ValidationAwardSpendRedeemer as ValidationAwardSpendRedeemerData,
  ValidationCanonicalDecodePrepareSelectedSpendRedeemer,
  type ValidationCanonicalDecodePrepareSelectedSpendRedeemer as ValidationCanonicalDecodePrepareSelectedSpendRedeemerData,
  ValidationCekMaterialRoute,
  type ValidationCekMaterialRoute as ValidationCekMaterialRouteData,
  ValidationDispute,
  validationDisputeDataFromCore,
  ValidationDisputeTurnSchema,
  ValidationGameSpendRedeemer,
  ValidationMachinePhaseSchema,
  ValidationMachineSourceKindSchema,
  type ValidationMachineState,
  ValidationMachineVerdictSchema,
  ValidationOneStepEvidence,
  type ValidationOneStepEvidence as ValidationOneStepEvidenceData,
  ValidationOneStepWitness,
  ValidationPrepareSelectedSpendRedeemer,
  type ValidationPrepareSelectedSpendRedeemer as ValidationPrepareSelectedSpendRedeemerData,
  type ValidationResolutionState,
  validationTraceDescriptorDataFromCore,
  ValidationTraceProof,
  validationTraceProofDataFromCore,
  WinningValidationResolutionDatum,
  type WinningValidationResolutionDatum as WinningValidationResolutionDatumData,
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
        schema: ValidationMachinePhaseSchema,
        expected:
          tag <= 6
            ? `d8${(0x79 + tag).toString(16)}80`
            : `d905${(tag - 7).toString(16).padStart(2, "0")}80`,
      })),
      ...["Pending", "Accepted", "Rejected"].map((value, tag) => ({
        value,
        schema: ValidationMachineVerdictSchema,
        expected: `d8${(0x79 + tag).toString(16)}80`,
      })),
      ...["Normal", "Forced"].map((value, tag) => ({
        value,
        schema: ValidationMachineSourceKindSchema,
        expected: `d8${(0x79 + tag).toString(16)}80`,
      })),
    ] as const;
    for (const { value, schema, expected } of nullaryVectors) {
      expect(Data.to(value as never, schema as never)).toBe(expected);
    }

    expect(
      Data.to(
        { AwaitingOperator: { midpoint: 1n } } as never,
        ValidationDisputeTurnSchema as never,
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
        ValidationDisputeTurnSchema as never,
      ),
    ).toBe(`d87a9f015820${"aa".repeat(32)}ff`);
    expect(
      Data.to("ReadyForOneStep" as never, ValidationDisputeTurnSchema as never),
    ).toBe("d87b80");

    expect(() => Data.from("d87c80", ValidationDisputeTurnSchema)).toThrow();
    expect(() =>
      Data.from("d8799f0102ff", ValidationDisputeTurnSchema),
    ).toThrow();
    expect(() => Data.from("d9050f80", ValidationMachinePhaseSchema)).toThrow();
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
      Data.from(Data.to(disputeData, ValidationDispute), ValidationDispute),
    ).toEqual(disputeData);

    const proofData = validationTraceProofDataFromCore(operator.proofs[1]!);
    expect(
      Data.from(Data.to(proofData, ValidationTraceProof), ValidationTraceProof),
    ).toEqual(proofData);

    const redeemer: ValidationGameSpendRedeemer = {
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
        Data.to(redeemer, ValidationGameSpendRedeemer),
        ValidationGameSpendRedeemer,
      ),
    ).toEqual(redeemer);

    expect(validationTraceDescriptorDataFromCore(operator.descriptor)).toEqual(
      disputeData.operator_descriptor,
    );
  });

  it("round-trips staged, direct, and award resolution V1 shapes", () => {
    const state: ValidationMachineState = {
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
    const resolution: ValidationResolutionState = {
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
    const evidence: ValidationOneStepEvidenceData = {
      transition,
      auxiliary,
    };
    expect(
      Data.from(
        Data.to(transition, ValidationOneStepWitness),
        ValidationOneStepWitness,
      ),
    ).toEqual(transition);
    const prepared: PreparedValidationResolutionDatumData = {
      fraud_prover: "0a".repeat(28),
      data: {
        version: 1n,
        resolution,
        evidence_hash: "0b".repeat(32),
      },
    };
    expect(
      Data.from(
        Data.to(prepared, PreparedValidationResolutionDatum),
        PreparedValidationResolutionDatum,
      ),
    ).toEqual(prepared);

    const prepareSelected: ValidationPrepareSelectedSpendRedeemerData = {
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
        Data.to(prepareSelected, ValidationPrepareSelectedSpendRedeemer),
        ValidationPrepareSelectedSpendRedeemer,
      ),
    ).toEqual(prepareSelected);
    // Option B (#620): the canonical-decode prepare redeemer is transition-only
    // — one four-field constructor, no auxiliary, and no retired by-hash arm.
    const canonicalDecodePrepareSelected: ValidationCanonicalDecodePrepareSelectedSpendRedeemerData =
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
      ValidationCanonicalDecodePrepareSelectedSpendRedeemer,
    );
    expect(
      Data.from(
        canonicalDecodePrepareSelectedCbor,
        ValidationCanonicalDecodePrepareSelectedSpendRedeemer,
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
            Data.from(Data.to(transition, ValidationOneStepWitness)),
          ]),
        ]),
      ),
    );

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
    ] satisfies readonly (readonly [ValidationCekMaterialRouteData, string])[];
    for (const [route, expectedCbor] of materialRouteVectors) {
      expect(Data.to(route, ValidationCekMaterialRoute)).toBe(expectedCbor);
      expect(Data.from(expectedCbor, ValidationCekMaterialRoute)).toEqual(
        route,
      );
    }

    for (const malformed of [
      "d87e80", // adjacent route tag 5
      "d87a9f420102ff", // DirectCekMaterial wrong arity
      "d87c9f42010201ff", // multi indices field is not a list
    ]) {
      expect(() => Data.from(malformed, ValidationCekMaterialRoute)).toThrow();
    }
    expect(
      Data.from(
        Data.to(evidence, ValidationOneStepEvidence),
        ValidationOneStepEvidence,
      ),
    ).toEqual(evidence);

    const winning: WinningValidationResolutionDatumData = {
      fraud_prover: prepared.fraud_prover,
      data: { version: 1n },
    };
    expect(
      Data.from(
        Data.to(winning, WinningValidationResolutionDatum),
        WinningValidationResolutionDatum,
      ),
    ).toEqual(winning);

    const award: ValidationAwardSpendRedeemerData = {
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
        Data.to(award, ValidationAwardSpendRedeemer),
        ValidationAwardSpendRedeemer,
      ),
    ).toEqual(award);
  });
});
