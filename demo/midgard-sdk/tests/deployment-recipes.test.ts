import { describe, expect, it } from "vitest";

import {
  DEPLOYMENT_RECIPES,
  type MidgardValidators,
  type SpendingValidator,
  type WithdrawalValidator,
} from "../src/index.js";

const context = {
  phasMembership: () => ({ type: "PlutusV3" as const, script: "00" }),
};

const spending = (hash: string): SpendingValidator => ({
  spendingScriptCBOR: hash,
  spendingScript: { type: "PlutusV3", script: hash },
  spendingScriptHash: hash,
  spendingScriptAddress: `addr_test_${hash}`,
});

const withdrawal = (hash: string): WithdrawalValidator => ({
  withdrawalScriptCBOR: hash,
  withdrawalScript: { type: "PlutusV3", script: hash },
  withdrawalScriptHash: hash,
});

const withMinAda = (minAda: unknown): MidgardValidators =>
  ({ fraudProofContracts: { minAda } }) as unknown as MidgardValidators;

describe("derived fault-proof deployment recipes", () => {
  const steps = ["01", "02", "03", "04"].map((step) => spending(`s${step}`));
  // A restored chain may expose a member under one path only, so each recipe
  // reads its member's first family path: `firstStep`, not `steps[0]`.
  const firstStep = spending("first");
  const contracts = withMinAda({
    firstStep,
    steps,
    yields: { tx: withdrawal("tx") },
  });

  it("reads each family member from its own family path with its purpose's script", () => {
    expect(
      DEPLOYMENT_RECIPES.fraudProofMinAda.resolve(contracts, context),
    ).toEqual({ script: firstStep.spendingScript, scriptHash: "first" });
    expect(
      DEPLOYMENT_RECIPES.fraudProofMinAdaStep03.resolve(contracts, context),
    ).toEqual({ script: steps[2]!.spendingScript, scriptHash: "s03" });
    expect(
      DEPLOYMENT_RECIPES.fraudProofMinAdaStep02TxWithdraw.resolve(
        contracts,
        context,
      ),
    ).toEqual({
      script: { type: "PlutusV3", script: "tx" },
      scriptHash: "tx",
    });
  });

  it("names the member and its path when the family chain lacks it", () => {
    const withoutYield = withMinAda({
      firstStep: steps[0],
      steps,
      yields: {},
    });
    expect(() =>
      DEPLOYMENT_RECIPES.fraudProofMinAdaStep02TxWithdraw.resolve(
        withoutYield,
        context,
      ),
    ).toThrow(
      "Deployment member fraudProofMinAdaStep02TxWithdraw is absent at fraudProofContracts.minAda.yields.tx",
    );
    expect(() =>
      DEPLOYMENT_RECIPES.fraudProofMinAdaStep04.resolve(
        withMinAda(undefined),
        context,
      ),
    ).toThrow(
      "Deployment member fraudProofMinAdaStep04 is absent at fraudProofContracts.minAda.steps.3",
    );
  });

  it("publishes the validation-trace control scripts only when that family is installed", () => {
    const installed = {
      fraudProofs: { validationTraceDispute: spending("vtd") },
    } as unknown as MidgardValidators;
    const uninstalled = { fraudProofs: {} } as unknown as MidgardValidators;
    expect(
      DEPLOYMENT_RECIPES.validationTraceDispute.publishable?.(installed),
    ).toBe(true);
    expect(
      DEPLOYMENT_RECIPES.validationTraceDispute.publishable?.(uninstalled),
    ).toBe(false);
    expect(
      DEPLOYMENT_RECIPES.fraudProofMinAda.publishable?.(uninstalled) ?? true,
    ).toBe(true);
  });
});
