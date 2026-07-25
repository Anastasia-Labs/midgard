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
  ValidationDirectResolveSpendRedeemerV1,
  type ValidationDirectResolveSpendRedeemerV1 as ValidationDirectResolveSpendRedeemerV1Data,
  validationDisputeDataFromCore,
  ValidationDisputeV1,
  ValidationGameSpendRedeemerV1,
  type ValidationMachineStateV1,
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
    const auxiliary = new Constr(0, []);
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
        Data.to(
          prepareSelected,
          ValidationPrepareSelectedSpendRedeemerV1,
        ),
        ValidationPrepareSelectedSpendRedeemerV1,
      ),
    ).toEqual(prepareSelected);

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
