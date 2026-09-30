import "@lucid-evolution/lucid";
import "vitest";
import "../src/workflow/funding-requirements.js";
import "../src/workflow/funding-requirements-test-support.js";
import "../src/workflow/runner-admission.js";
import "./funding-requirements.input.js";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitWorkflowFundingRequirements,
  assertAdmittedWorkflowFundingRequirements,
  createWorkflowFundingRequirements,
  isProtocolFundedWorkflowAction,
  workflowFundingRequirementsForRunner,
} from "../src/workflow/funding-requirements.js";
import { unsafeCreateMeasuredWorkflowRunnerForTest } from "../src/workflow/funding-requirements-test-support.js";
import { createAdmittedWorkflowRunner } from "../src/workflow/runner-admission.js";
import {
  fundingAddress,
  fundingInputCbor,
  input,
  lockedAddress,
  protocolFundedInput,
} from "./funding-requirements.input.js";

describe("production workflow funding requirements V1", () => {
  it("accounts for protocol-funded fee and reward without claiming prover capital", () => {
    const profile = createWorkflowFundingRequirements(protocolFundedInput());
    expect(isProtocolFundedWorkflowAction(profile.actions[0]!)).toBe(true);
    expect(
      profile.actions[0]!.fundingControlledInputs[0]!.fundingLovelace,
    ).toBe("0");
    expect(admitWorkflowFundingRequirements(profile)).toEqual(profile);
  });

  it("refuses an unfunded protocol reward and falsely attributed prover principal", () => {
    const value = protocolFundedInput();
    const action = value.actions[0]!;
    expect(() =>
      createWorkflowFundingRequirements({
        ...value,
        actions: [
          {
            ...action,
            fundingControlledInputs: [
              {
                ...action.fundingControlledInputs[0]!,
                resolvedOutputCborHex: CML.TransactionOutput.new(
                  CML.Address.from_bech32(lockedAddress),
                  CML.Value.from_coin(3_169_999n),
                ).to_canonical_cbor_hex(),
              },
            ],
          },
        ],
      }),
    ).toThrow("protocol-funded Ada is not conserved");
    expect(() =>
      createWorkflowFundingRequirements({
        ...value,
        actions: [
          {
            ...action,
            fundingControlledOutputs: [
              {
                ...action.fundingControlledOutputs[0]!,
                fundingLovelace: "3000000",
              },
            ],
          },
        ],
      }),
    ).toThrow("role differs from its exact output authority");
  });

  it("keeps mixed prover funding out of the protocol-only reward branch", () => {
    const value = protocolFundedInput();
    const action = value.actions[0]!;
    expect(() =>
      createWorkflowFundingRequirements({
        ...value,
        actions: [
          {
            ...action,
            fundingControlledInputs: [
              {
                ...action.fundingControlledInputs[0]!,
                role: "wallet_funding",
                semanticRole: "wallet_funding",
                contractAddress: fundingAddress,
                fundingLovelace: "3170000",
                resolvedOutputCborHex: CML.TransactionOutput.new(
                  CML.Address.from_bech32(fundingAddress),
                  CML.Value.from_coin(3_170_000n),
                ).to_canonical_cbor_hex(),
              },
            ],
          },
        ],
      }),
    ).toThrow("protocol rewards require exclusively protocol-funded");
  });

  it("derives exact canonical transaction/output measurements and re-admits the profile", () => {
    const profile = createWorkflowFundingRequirements(input());
    const transaction = CML.Transaction.from_cbor_hex(
      profile.actions[0]!.signedTransactionCborHex,
    );
    expect(profile.actions[0]).toMatchObject({
      txBodyCborHex: transaction.body().to_canonical_cbor_hex(),
      txBodyBytes: transaction.body().to_canonical_cbor_hex().length / 2,
      signedTransactionBytes:
        profile.actions[0]!.signedTransactionCborHex.length / 2,
      signedTransactionSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
      executionUnits: { memory: "0", steps: "0" },
      outputCborHex: [0, 1, 2].map((outputIndex) =>
        transaction.body().outputs().get(outputIndex).to_canonical_cbor_hex(),
      ),
    });
    expect(admitWorkflowFundingRequirements(profile)).toEqual(profile);
    expect(Object.isFrozen(profile.actions[0]!.outputCborHex)).toBe(true);
  });

  it("rejects digest, deployment, derived measurement, and transaction substitution", () => {
    const profile = createWorkflowFundingRequirements(input());
    for (const substituted of [
      { ...profile, profileDigest: "ff".repeat(32) },
      { ...profile, deploymentFingerprint: "ff".repeat(32) },
      { ...profile, fundingPaymentKeyHash: "ff".repeat(28) },
      {
        ...profile,
        actions: [{ ...profile.actions[0]!, txBodyBytes: 1 }],
      },
      {
        ...profile,
        actions: [{ ...profile.actions[0]!, outputCborHex: ["80"] }],
      },
    ]) {
      expect(() => admitWorkflowFundingRequirements(substituted)).toThrow();
    }
  });

  it("rejects noncanonical amounts, dynamic action identities, and duplicate assets/actions", () => {
    const valid = input();
    const invoke = (actions: unknown[]) =>
      createWorkflowFundingRequirements({
        ...valid,
        actions: actions as typeof valid.actions,
      });
    expect(() =>
      invoke([{ ...valid.actions[0]!, requiredBondLovelace: "0900000000" }]),
    ).toThrow("canonical non-negative decimal");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          actionKind: `proof-init:${"aa".repeat(32)}#0`,
        },
      ]),
    ).toThrow("stable action identifier");
    expect(() => invoke([valid.actions[0]!, valid.actions[0]!])).toThrow(
      "must be unique",
    );
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [
            valid.actions[0]!.requiredNativeAssets[0]!,
            valid.actions[0]!.requiredNativeAssets[0]!,
          ],
        },
      ]),
    ).toThrow("strictly unit-sorted");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [
            { unit: `${"aa".repeat(28)}0`, quantity: "1" },
          ],
        },
      ]),
    ).toThrow("canonical Cardano asset unit");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [{ unit: "lovelace", quantity: "1" }],
        },
      ]),
    ).toThrow("canonical Cardano asset unit");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [
            { unit: `${"aa".repeat(28)}${"bb".repeat(33)}`, quantity: "1" },
          ],
        },
      ]),
    ).toThrow("canonical Cardano asset unit");
  });

  it("rejects malformed transaction CBOR and unknown fields", () => {
    const valid = input();
    expect(() =>
      createWorkflowFundingRequirements({
        ...valid,
        actions: [{ ...valid.actions[0]!, signedTransactionCborHex: "80" }],
      }),
    ).toThrow("not a Cardano transaction");
    expect(() =>
      admitWorkflowFundingRequirements({
        ...createWorkflowFundingRequirements(valid),
        callerOverride: true,
      }),
    ).toThrow("unknown or missing fields");
  });

  it("rejects omitted wallet inputs, forged custody semantics, and unbalanced funding flow", () => {
    const valid = input();
    expect(() =>
      createWorkflowFundingRequirements({
        ...valid,
        actions: [{ ...valid.actions[0]!, fundingControlledInputs: [] }],
      }),
    ).toThrow("classify every exact transaction input");
    expect(() =>
      createWorkflowFundingRequirements({
        ...valid,
        actions: [
          {
            ...valid.actions[0]!,
            fundingControlledOutputs:
              valid.actions[0]!.fundingControlledOutputs.map((output) =>
                output.outputIndex === 1
                  ? { ...output, contractAddress: fundingAddress }
                  : output,
              ),
          },
        ],
      }),
    ).toThrow("semantic authority is invalid");
    expect(() =>
      createWorkflowFundingRequirements({
        ...valid,
        actions: [
          {
            ...valid.actions[0]!,
            fundingControlledInputs: [
              {
                ...valid.actions[0]!.fundingControlledInputs[0]!,
                resolvedOutputCborHex: fundingInputCbor(1_003_170_001n),
                fundingLovelace: "1003170001",
              },
            ],
          },
        ],
      }),
    ).toThrow("funding-controlled value is not conserved");
  });

  it("admits the distinct authenticated Q58 availability lifecycle scope", () => {
    const profile = createWorkflowFundingRequirements({
      ...input(),
      scope: {
        kind: "da_availability_lifecycle",
        lifecycle: "challenge_response_timeout_correction",
      },
    });
    expect(profile.scope).toEqual({
      kind: "da_availability_lifecycle",
      lifecycle: "challenge_response_timeout_correction",
    });
    expect(admitWorkflowFundingRequirements(profile)).toEqual(profile);
  });

  it("returns only the exact profile bound by a fixed admitted runner factory", () => {
    const profile = createWorkflowFundingRequirements(input());
    const runner = unsafeCreateMeasuredWorkflowRunnerForTest({
      category: "doubleSpend",
      fundingRequirements: profile,
    });
    expect(
      workflowFundingRequirementsForRunner({
        category: "doubleSpend",
        runner,
      }),
    ).toBe(profile);
    expect(() =>
      assertAdmittedWorkflowFundingRequirements(profile),
    ).not.toThrow();
    expect(() =>
      assertAdmittedWorkflowFundingRequirements({ ...profile }),
    ).toThrow("not factory-admitted");
    expect(() =>
      workflowFundingRequirementsForRunner({
        category: "minFee",
        runner,
      }),
    ).toThrow("not category-admitted");
  });

  it("keeps unmeasured admitted runners explicitly not ready for W31", () => {
    const runner = createAdmittedWorkflowRunner({
      category: "doubleSpend",
      runOrResume: async () => undefined,
    });
    expect(() =>
      workflowFundingRequirementsForRunner({
        category: "doubleSpend",
        runner,
      }),
    ).toThrow("no admitted measured funding profile");
  });
});
