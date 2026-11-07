import { expect, it } from "vitest";

import { assertWorkflowFundingReservationReadyToSubmit } from "../src/workflow/funding-reservation-permit.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { slashFundingFixture } from "./workflow-runtime.slash-funding-fixture.js";

/** Admission of signed bond-backed slashes against the release economics. */
export const registerSlashFundingEconomicsTests = (): void => {
  it.each([900_000_000n, 800_000_000n, 800_000_001n, 1_800_000_000n])(
    "admits exact signed slash economics of a %s bond without spending ordinary wallet funds",
    async (bondLovelace) => {
      const runtime = await slashFundingFixture({ bondLovelace });
      try {
        await runtime.admit();
        expect(runtime.protocolAuthority).toHaveBeenCalledTimes(2);
        expect(runtime.prepare).toHaveBeenCalledOnce();
        expect(runtime.prepare).toHaveBeenCalledWith(
          expect.objectContaining({
            transition: expect.objectContaining({
              actionKind: "remove",
              consumedOutRefs: [],
              signedTransactionCborHex: runtime.signed
                .toTransaction()
                .to_cbor_hex(),
              producedInputs: [
                expect.objectContaining({ lovelace: "400000000" }),
              ],
            }),
          }),
        );
        const policy = readWorkflowRuntimeFundingPolicy(runtime.policy);
        expect(policy.maximumSlashCollateralLovelace).toBe("750000000");
        expect(BigInt(policy.maximumFeeLovelace)).toBeLessThan(400_000_000n);
        expect(BigInt(policy.maximumCollateralLovelace)).toBeLessThan(
          600_000_000n,
        );
        await expect(
          assertWorkflowFundingReservationReadyToSubmit({
            journal: runtime.journal,
            transactionHash: runtime.signed.toHash(),
          }),
        ).resolves.toBeUndefined();
      } finally {
        runtime.close();
      }
    },
  );
  it.each([
    { label: "fee below the bond slash", feeDelta: -1n },
    { label: "fee above the bond slash", feeDelta: 1n },
    { label: "padded fee", bondLovelace: 900_000_001n, feeDelta: 1n },
    { label: "padded fee below", bondLovelace: 900_000_001n, feeDelta: -1n },
    { label: "bond below inactivity tranche", bondLovelace: 799_999_999n },
    { label: "reward below release", rewardDelta: -1n },
    { label: "reward above release", rewardDelta: 1n },
    { label: "different action", actionStage: "step-one" },
    { label: "missing private capability", capability: false },
    { label: "ordinary high fee", capability: false, actionStage: "step-one" },
    { label: "ordinary wallet input", includeWalletInput: true },
    { label: "foreign protocol authority", foreignAuthority: true },
  ])(
    "rejects signed slash funding with $label before durable preparation",
    async (options) => {
      const runtime = await slashFundingFixture(options);
      try {
        await expect(runtime.admit()).rejects.toThrow();
        expect(runtime.prepare).not.toHaveBeenCalled();
      } finally {
        runtime.close();
      }
    },
  );
};
