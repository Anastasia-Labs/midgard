import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  validationDisputeWalletMoveReserve,
  workflowRuntimeFundingMinimumFee,
} from "../src/workflow/runtime-funding-fee.js";
import { RUNTIME_FUNDING_TEST_PARAMETERS } from "./helpers/runtime-funding-policy-fixture.js";

const FUNDING_PAYMENT_KEY_HASH = CML.PrivateKey.from_normal_bytes(
  Buffer.alloc(32, 0x23),
)
  .to_public()
  .hash()
  .to_hex();

describe("workflow runtime funding fees", () => {
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

  it("prices the validation dispute's wallet-move reserve at the protocol's largest signed transaction, whatever size the moves measured so far", () => {
    const reserveAt = (maxTxSize: bigint) =>
      validationDisputeWalletMoveReserve({
        parameters: {
          ...RUNTIME_FUNDING_TEST_PARAMETERS,
          maxTxSize: maxTxSize.toString(),
        },
        fundingPaymentKeyHash: FUNDING_PAYMENT_KEY_HASH,
      });
    const minFeeA = BigInt(RUNTIME_FUNDING_TEST_PARAMETERS.minFeeA);
    // Each byte the protocol admits is a byte our wallet may pay for.
    expect(reserveAt(16_384n) - reserveAt(8_192n)).toBe(8_192n * minFeeA);
    expect(reserveAt(16_384n)).toBeGreaterThan(16_384n * minFeeA);
  });
});
