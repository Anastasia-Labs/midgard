import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  ADA,
  challenger,
  coin,
  parameters,
} from "./da-bond-pool-live-port.da-bond-pool-live-port-endpoints-and-preconditions.js";
import {
  assertDistinctChallengerKey,
  attestRefusalResult,
  DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE,
  DaBondJourneySigningMaterialError,
  daBondPoolApplyRefusal,
  daBondPoolChallengerFundingShortfall,
  daBondPoolJourneyParamsOf,
  planDaBondOwnerQuorum,
  planDaBondPoolChallengerFunding,
  requireJourneySeed,
  selectDaBondPoolChallengerCoins,
} from "./da-bond-pool-live-port.js";

describe("DA bond pool live port: parameters and funding", () => {
  it("maps the manifest amounts and timing to the journey parameters", () => {
    expect(
      daBondPoolJourneyParamsOf(parameters, {
        da_bond_withdraw_delay_ms: 2_340_000,
        da_attestation_timeout_ms: "600000",
      }),
    ).toEqual({
      daBond: parameters.da_bond_lovelace,
      penalty: 100n * ADA,
      floor: parameters.da_bond_pool_floor_lovelace,
      minTopUp: parameters.da_bond_min_top_up_lovelace,
      maxTimeoutFee: 3n * ADA,
      challengeRecordLovelace: parameters.challenge_record_lovelace,
      withdrawDelayMs: 2_340_000,
      attestationTimeoutMs: 600_000,
    });
    expect(() =>
      daBondPoolJourneyParamsOf(parameters, {
        da_bond_withdraw_delay_ms: 0,
        da_attestation_timeout_ms: 600_000,
      }),
    ).toThrow("da_bond_withdraw_delay_ms must be a positive integer");
  });

  it("plans exact Open coins and a G9 collateral coin", () => {
    const plan = planDaBondPoolChallengerFunding({
      parameters,
      collateralPercentage: 150,
    });
    expect(plan.openCoinLovelace).toBe(
      parameters.challenger_bond_lovelace +
        parameters.challenge_record_lovelace +
        parameters.max_open_fee_lovelace,
    );
    expect(plan.openCoins).toBe(2);
    // 150% of (100 + 3) ADA, the worst Timeout fee.
    expect(plan.timeoutCollateralLovelace).toBe(154_500_000n);
    expect(plan.collateralLovelace).toBe(
      154_500_000n + DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE,
    );
    expect(() =>
      planDaBondPoolChallengerFunding({
        parameters,
        collateralPercentage: 150,
        operatingLovelace: 10n * ADA,
      }),
    ).toThrow("operating coin must hold at least 17000000 lovelace");
  });

  it("funds only what the challenger wallet lacks", () => {
    const plan = planDaBondPoolChallengerFunding({
      parameters,
      collateralPercentage: 150,
    });
    expect(
      daBondPoolChallengerFundingShortfall({
        utxos: [],
        address: challenger,
        plan,
      }),
    ).toEqual([
      plan.openCoinLovelace,
      plan.openCoinLovelace,
      plan.collateralLovelace,
      plan.operatingLovelace,
    ]);
    const funded = [
      coin(1, plan.openCoinLovelace),
      coin(2, plan.openCoinLovelace),
      coin(3, plan.collateralLovelace),
      coin(4, plan.operatingLovelace),
    ];
    expect(
      daBondPoolChallengerFundingShortfall({
        utxos: funded,
        address: challenger,
        plan,
      }),
    ).toEqual([]);
    // A 10 ADA coin is below G9 and cannot collateralize a Timeout; a coin
    // at another address or carrying a token is not the challenger's cash.
    expect(
      daBondPoolChallengerFundingShortfall({
        utxos: [
          coin(1, plan.openCoinLovelace),
          coin(2, plan.openCoinLovelace, { address: "addr_test1_other" }),
          coin(3, 10n * ADA),
          coin(4, plan.operatingLovelace, {
            assets: { lovelace: plan.operatingLovelace, ["ab".repeat(28)]: 1n },
          }),
        ],
        address: challenger,
        plan,
      }),
    ).toEqual([
      plan.openCoinLovelace,
      plan.collateralLovelace,
      plan.operatingLovelace,
    ]);
  });

  it("selects coins by role and never picks a reserved coin", () => {
    const plan = planDaBondPoolChallengerFunding({
      parameters,
      collateralPercentage: 150,
    });
    const open1 = coin(1, plan.openCoinLovelace);
    const open2 = coin(2, plan.openCoinLovelace);
    const collateral = coin(3, plan.collateralLovelace);
    const operating = coin(4, 400n * ADA);
    const utxos = [operating, open2, collateral, open1];
    expect(
      selectDaBondPoolChallengerCoins({ utxos, address: challenger, plan }),
    ).toEqual({ collateral, openFunding: open1, operating });
    // An unconfirmed intent reserves its inputs and its collateral; the
    // planned collateral coin may back the next intent too.
    expect(
      selectDaBondPoolChallengerCoins({
        utxos,
        address: challenger,
        plan,
        reserved: new Set([`${open1.txHash}#0`, `${collateral.txHash}#0`]),
      }),
    ).toEqual({ collateral, openFunding: open2, operating });
    // Without the planned coin, a reserved coin is never posted or spent.
    expect(
      selectDaBondPoolChallengerCoins({
        utxos: [open1, operating, coin(5, 300n * ADA)],
        address: challenger,
        plan,
        reserved: new Set([`${open1.txHash}#0`, `${operating.txHash}#0`]),
      }),
    ).toEqual({ collateral: coin(5, 300n * ADA) });
  });

  it("refuses a challenger key another role already uses", () => {
    expect(() =>
      assertDistinctChallengerKey("aa", { operator: "bb", availability: "cc" }),
    ).not.toThrow();
    expect(() =>
      assertDistinctChallengerKey("aa", {
        operator: "bb",
        availability: "aa",
        cosigner: "aa",
      }),
    ).toThrow("also the availability, cosigner key");
  });
});

describe("DA bond pool live port: signing material", () => {
  const operator = { role: "operator", keyHash: "01".repeat(28) };
  const cosigner = { role: "cosigner", keyHash: "02".repeat(28) };
  const stranger = "03".repeat(28);

  it("takes the threshold of held owners, in owner order", () => {
    expect(
      planDaBondOwnerQuorum({
        owners: [cosigner.keyHash, operator.keyHash],
        updateThreshold: 2n,
        held: [operator, cosigner],
        source: "accounts",
      }),
    ).toEqual([cosigner, operator]);
    expect(
      planDaBondOwnerQuorum({
        owners: [stranger, operator.keyHash],
        updateThreshold: 1n,
        held: [operator, cosigner],
        source: "accounts",
      }),
    ).toEqual([operator]);
  });

  it("names every owner key the run lacks", () => {
    const attempt = () =>
      planDaBondOwnerQuorum({
        owners: [operator.keyHash, stranger],
        updateThreshold: 2n,
        held: [operator, cosigner],
        source: "secrets/journey-accounts.json",
      });
    expect(attempt).toThrow(DaBondJourneySigningMaterialError);
    try {
      attempt();
    } catch (error) {
      expect((error as DaBondJourneySigningMaterialError).missing).toEqual([
        `the signing key of DA params owner ${stranger}`,
      ]);
      expect((error as Error).message).toContain(
        "needs 2 of 2 owners; secrets/journey-accounts.json holds 1",
      );
    }
    expect(() =>
      planDaBondOwnerQuorum({
        owners: [operator.keyHash],
        updateThreshold: 2n,
        held: [operator],
        source: "accounts",
      }),
    ).toThrow("update_threshold 2 cannot be met by 1 owner(s)");
  });

  it("names a missing seed phrase", () => {
    expect(
      requireJourneySeed(
        { cosigner: { seedPhrase: " a b " } },
        "cosigner",
        "s",
      ),
    ).toBe("a b");
    expect(() =>
      requireJourneySeed({ operator: {} }, "cosigner", "secrets/x.json"),
    ).toThrow(
      "missing signing material: the cosigner seedPhrase in secrets/x.json",
    );
  });
});

describe("DA bond pool live port: Apply refusals", () => {
  const poolError = (reason: SDK.DaAttestationBuildFailureReason) =>
    new SDK.DaAttestationBuildError({
      message: `Apply refused: ${reason}`,
      cause: undefined,
      reason,
    });

  it("reads the pool refusal out of the Effect failure Effect.runPromise rejects with", async () => {
    const rejected = await Effect.runPromise(
      Effect.fail(poolError("pool-under-backed")),
    ).catch((error: unknown) => error);
    expect(daBondPoolApplyRefusal(rejected)).toEqual({
      reason: "pool-under-backed",
      message: "Apply refused: pool-under-backed",
    });
    expect(attestRefusalResult(rejected)).toEqual({
      kind: "refused",
      reason: "pool-under-backed: Apply refused: pool-under-backed",
    });
  });

  it("finds it through a cause chain or an aggregate", () => {
    expect(
      daBondPoolApplyRefusal(
        new Error("attest failed", { cause: poolError("pool-withdrawing") }),
      )?.reason,
    ).toBe("pool-withdrawing");
    expect(
      daBondPoolApplyRefusal(
        new AggregateError([new Error("x"), poolError("pool-unavailable")]),
      )?.reason,
    ).toBe("pool-unavailable");
  });

  it("does not turn any other failure into a refusal", async () => {
    const other = await Effect.runPromise(
      Effect.fail(poolError("invalid_validity_range")),
    ).catch((error: unknown) => error);
    expect(daBondPoolApplyRefusal(other)).toBeUndefined();
    expect(attestRefusalResult(new Error("pool-under-backed"))).toBeUndefined();
    const cyclic = new Error("loop");
    (cyclic as { cause?: unknown }).cause = cyclic;
    expect(daBondPoolApplyRefusal(cyclic)).toBeUndefined();
  });
});
