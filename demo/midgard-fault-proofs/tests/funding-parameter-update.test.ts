import { expect, it } from "vitest";

import {
  assertWorkflowFundingReservationReadyToSubmit,
  confirmWorkflowFundingReservationTransaction,
  readWorkflowFundingRecovery,
  WorkflowFundingReservationUnavailableError,
} from "../src/workflow/funding-reservation-permit.js";
import { createWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

it("admits a current fee policy while preserving the original reservation identity", async () => {
  const funding = await runtimeFunding("step-one", {
    amendPolicy: (input) =>
      createWorkflowRuntimeFundingPolicy({
        ...input,
        protocolParameters: {
          ...input.protocolParameters,
          minFeeA: "45",
          collateralPercentage: "175",
        },
      }),
  });
  expect(funding.snapshot.policyDigest).toBeDefined();
  expect(funding.selected.fundingOutRefs).toEqual([
    `${"71".repeat(32)}#0`,
    `${"72".repeat(32)}#0`,
    `${"73".repeat(32)}#0`,
  ]);
  expect(funding.prepare).not.toHaveBeenCalled();
});

it.each(["credential", "contracts", "economics", "references"] as const)(
  "does not let a parameter update replace %s authority",
  async (changed) => {
    await expect(
      runtimeFunding("step-one", {
        amendPolicy: (input) => {
          const common = {
            ...input,
            protocolParameters: { ...input.protocolParameters, minFeeA: "45" },
          };
          if (changed === "credential")
            return createWorkflowRuntimeFundingPolicy({
              ...common,
              fundingPaymentKeyHash: "cc".repeat(28),
            });
          if (changed === "contracts")
            return createWorkflowRuntimeFundingPolicy({
              ...common,
              contracts: input.contracts.map((contract) => ({
                ...contract,
                role: "field_carrier",
              })),
            });
          if (changed === "references")
            return createWorkflowRuntimeFundingPolicy({
              ...common,
              referenceScripts: [
                { outRef: `${"dd".repeat(32)}#0`, scriptHash: "ee".repeat(28) },
              ],
            });
          return createWorkflowRuntimeFundingPolicy({
            ...common,
            economics: { ...input.economics, blueprintHash: "cf".repeat(32) },
          });
        },
      }),
    ).rejects.toThrow("funding reservation policy");
  },
);

it("does not let a fee update change the fixed runner category", async () => {
  await expect(
    runtimeFunding("step-one", {
      amendPolicy: (input) =>
        createWorkflowRuntimeFundingPolicy({
          ...input,
          category: "doubleWithdraw",
          protocolParameters: { ...input.protocolParameters, minFeeA: "45" },
        }),
    }),
  ).rejects.toThrow("fixed category runner");
});

it("reconciles the exact signed old intent before refusing a fresh build under a lower collateral limit", async () => {
  const original = await runtimeFunding("step-one", { collateral: 2 });
  const action = { actionId: "step-one", input: { actionKind: "step-one" } };
  const signed = signedFundingTransaction({
    inputOutRefs: original.selected.fundingOutRefs,
    outputLovelace: 14_800_000n,
    redeemerMemory: 1n,
    collateral: {
      outRefs: original.selected.collateralOutRefs,
      total: 300_000n,
      returned: 9_700_000n,
    },
  });
  await original.prepareTransaction({
    action,
    preflight: bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: signed.toHash() }),
      signed,
    ),
  });
  const recovery = original.prepare.mock.calls[0]![0];
  const restarted = await runtimeFunding("step-one", {
    collateral: 2,
    begin: false,
    recovery,
    amendPolicy: (input) =>
      createWorkflowRuntimeFundingPolicy({
        ...input,
        protocolParameters: {
          ...input.protocolParameters,
          maxCollateralInputs: "1",
        },
      }),
  });
  const saved = await readWorkflowFundingRecovery(restarted.journal);
  expect(saved.transition!.signedTransactionCborHex).toBe(
    signed.toTransaction().to_cbor_hex(),
  );
  await expect(
    assertWorkflowFundingReservationReadyToSubmit({
      journal: restarted.journal,
      transactionHash: signed.toHash(),
    }),
  ).rejects.toBeInstanceOf(WorkflowFundingReservationUnavailableError);
  await confirmWorkflowFundingReservationTransaction({
    journal: restarted.journal,
    transactionHash: signed.toHash(),
  });
  expect(restarted.confirm).toHaveBeenCalledOnce();
  await expect(restarted.begin()).rejects.toBeInstanceOf(
    WorkflowFundingReservationUnavailableError,
  );
  expect(restarted.prepare).not.toHaveBeenCalled();
});

it.each([
  "credential",
  "contracts",
  "economics",
  "references",
  "category",
] as const)(
  "does not let historical capacity replace %s authority",
  async (changed) => {
    await expect(
      runtimeFunding("step-one", {
        amendCapacityPolicy: (input) => {
          const common = {
            ...input,
            protocolParameters: {
              ...input.protocolParameters,
              maxCollateralInputs: "5",
            },
          };
          if (changed === "credential")
            return createWorkflowRuntimeFundingPolicy({
              ...common,
              fundingPaymentKeyHash: "cc".repeat(28),
            });
          if (changed === "contracts")
            return createWorkflowRuntimeFundingPolicy({
              ...common,
              contracts: input.contracts.map((contract) => ({
                ...contract,
                role: "field_carrier",
              })),
            });
          if (changed === "references")
            return createWorkflowRuntimeFundingPolicy({
              ...common,
              referenceScripts: [
                { outRef: `${"dd".repeat(32)}#0`, scriptHash: "ee".repeat(28) },
              ],
            });
          if (changed === "category")
            return createWorkflowRuntimeFundingPolicy({
              ...common,
              category: "doubleWithdraw",
            });
          return createWorkflowRuntimeFundingPolicy({
            ...common,
            economics: { ...input.economics, blueprintHash: "cf".repeat(32) },
          });
        },
      }),
    ).rejects.toThrow(
      changed === "category"
        ? "fixed category runner"
        : "funding reservation policy",
    );
  },
);
