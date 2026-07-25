import {
  buildMidgardValidationTraceTree,
  openMidgardValidationDispute,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  validationDisputeDataFromCore,
  ValidationGameSpendRedeemerV1,
  ValidationDisputeV1,
  validationTraceDescriptorDataFromCore,
  validationTraceProofDataFromCore,
  ValidationTraceProofV1,
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
});
