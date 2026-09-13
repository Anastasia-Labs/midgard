import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { createDoubleSpendWorkflowRunner } from "../src/workflow/runtime.js";
import {
  assertWorkflowRuntimeFundingPolicyRunner,
  createWorkflowRuntimeFundingPolicy,
  readWorkflowRuntimeFundingPolicy,
  workflowRuntimeFundingMinimumFee,
} from "../src/workflow/runtime-funding-policy.js";
import {
  RUNTIME_FUNDING_TEST_PARAMETERS,
  runtimeFundingPolicyFixture,
} from "./helpers/runtime-funding-policy-fixture.js";

const fixture = () =>
  runtimeFundingPolicyFixture({
    deploymentFingerprint: "11".repeat(32),
    fundingPaymentKeyHash: CML.PrivateKey.from_normal_bytes(
      Buffer.alloc(32, 0x23),
    )
      .to_public()
      .hash()
      .to_hex(),
  });

describe("runtime-owned workflow funding policy", () => {
  it("admits a fixed runner without a measured profile and binds its exact identity", () => {
    const { policy, runner } = fixture();
    const value = readWorkflowRuntimeFundingPolicy(policy);
    expect(BigInt(value.maximumFeeLovelace)).toBeGreaterThan(200_000n);
    expect(value.maximumCollateralInputs).toBe("3");
    expect(() =>
      assertWorkflowRuntimeFundingPolicyRunner({
        policy,
        runner,
        category: "doubleSpend",
      }),
    ).not.toThrow();
    expect(() => readWorkflowRuntimeFundingPolicy({ ...policy })).toThrow(
      "not admitted",
    );
    const another = createDoubleSpendWorkflowRunner(async () => {
      throw new Error("not executed");
    });
    expect(() =>
      assertWorkflowRuntimeFundingPolicyRunner({
        policy,
        runner: another,
        category: "doubleSpend",
      }),
    ).toThrow("fixed runner");
  });

  it("computes exact rational reference fees across a tier boundary", () => {
    expect(
      workflowRuntimeFundingMinimumFee({
        parameters: RUNTIME_FUNDING_TEST_PARAMETERS,
        transactionBytes: 0n,
        memory: 0n,
        steps: 0n,
        referenceScriptBytes: 25_601n,
      }),
    ).toBe(155_381n + 25_600n * 15n + 18n);
  });

  it("refuses foreign economics, repeated contracts and foreign script addresses", () => {
    const { constructorInput } = fixture();
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        deploymentFingerprint: "12".repeat(32),
      }),
    ).toThrow("another deployment");
    const contract = constructorInput.contracts[0]!;
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        contracts: [contract, contract],
      }),
    ).toThrow("repeated");
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        contracts: [
          {
            ...contract,
            address: credentialToAddress("Preprod", {
              type: "Key",
              hash: contract.scriptHash,
            }),
          },
        ],
      }),
    ).toThrow("identity");
  });

  it("binds the exact deployed reference outref and script hash separately from custody", () => {
    const { constructorInput, policy } = fixture();
    const reference = {
      outRef: "31".repeat(32) + "#0",
      scriptHash: "32".repeat(28),
    };
    const scoped = createWorkflowRuntimeFundingPolicy({
      ...constructorInput,
      referenceScripts: [reference],
    });
    expect(readWorkflowRuntimeFundingPolicy(scoped).policyDigest).not.toBe(
      readWorkflowRuntimeFundingPolicy(policy).policyDigest,
    );
    expect(readWorkflowRuntimeFundingPolicy(scoped).referenceScripts).toEqual([
      reference,
    ]);
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        referenceScripts: [reference, reference],
      }),
    ).toThrow("repeated");
  });
});
