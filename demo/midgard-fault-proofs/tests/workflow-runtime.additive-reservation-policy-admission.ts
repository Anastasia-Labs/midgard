import "./workflow-runtime.compiled-manifest-bound-production-runtime-v1.js";

import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createWorkflowRuntimeFundingPolicy,
  readWorkflowRuntimeFundingPolicy,
} from "../src/workflow/runtime-funding-policy.js";
import { runtimeFundingPolicyFixture } from "./helpers/runtime-funding-policy-fixture.js";
import {
  DEPLOYMENT,
  fundingKey,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";
import { prepareRuntimeFunding } from "./workflow-runtime.slash-funding-fixture.js";

describe("additive reservation policy admission", () => {
  it("keeps the original snapshot policy while validating newly admitted proof custody", async () => {
    const address = credentialToAddress("Preprod", {
      type: "Script",
      hash: "d2".repeat(28),
    });
    const runtime = await runtimeFunding("step-one", {
      amendPolicy: (input) =>
        createWorkflowRuntimeFundingPolicy({
          ...input,
          contracts: [
            ...input.contracts,
            { address, scriptHash: "d2".repeat(28), role: "proof_thread" },
          ],
        }),
    });
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("00")),
    );
    const minimum = CML.min_ada_required(output, 4310n);
    const signed = signedFundingTransaction({
      inputOutRefs: runtime.selected.fundingOutRefs,
      outputLovelace: 14_800_000n - minimum,
      additionalOutputs: [
        CML.TransactionOutput.new(
          CML.Address.from_bech32(address),
          CML.Value.from_coin(minimum),
          CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("00")),
        ),
      ],
    });
    await prepareRuntimeFunding(runtime, signed);
    expect(runtime.prepare).toHaveBeenCalledOnce();
    expect(runtime.snapshot.policyDigest).toBe(
      readWorkflowRuntimeFundingPolicy(runtime.policy).policyDigest,
    );
  });

  it.each([
    "removed",
    "role",
    "key",
    "economics",
    "references",
    "runner",
  ] as const)(
    "rejects %s changes under the additive reservation policy bridge",
    async (change) => {
      await expect(
        runtimeFunding("step-one", {
          amendPolicy: (input) =>
            createWorkflowRuntimeFundingPolicy({
              ...input,
              ...(change === "removed"
                ? {
                    contracts: [
                      {
                        address: credentialToAddress("Preprod", {
                          type: "Script",
                          hash: "d2".repeat(28),
                        }),
                        scriptHash: "d2".repeat(28),
                        role: "proof_thread",
                      },
                    ],
                  }
                : {}),
              ...(change === "role"
                ? {
                    contracts: input.contracts.map((entry) => ({
                      ...entry,
                      role: "field_carrier" as const,
                    })),
                  }
                : {}),
              ...(change === "key"
                ? { fundingPaymentKeyHash: "dc".repeat(28) }
                : {}),
              ...(change === "economics"
                ? {
                    economics: {
                      ...input.economics,
                      blueprintHash: "ed".repeat(32),
                    },
                  }
                : {}),
              ...(change === "references"
                ? {
                    referenceScripts: [
                      {
                        outRef: `${"aa".repeat(32)}#0`,
                        scriptHash: "aa".repeat(28),
                      },
                    ],
                  }
                : {}),
              ...(change === "runner"
                ? {
                    runner: runtimeFundingPolicyFixture({
                      deploymentFingerprint: DEPLOYMENT,
                      fundingPaymentKeyHash: fundingKey
                        .to_public()
                        .hash()
                        .to_hex(),
                    }).runner,
                  }
                : {}),
            }),
        }),
      ).rejects.toThrow(
        change === "runner" ? /runner/ : /funding reservation policy/,
      );
    },
  );
});
