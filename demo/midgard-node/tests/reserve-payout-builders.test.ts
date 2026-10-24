import "node:crypto";
import "node:fs";
import "@al-ft/midgard-core/out-ref";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/commands/reserve-payout.js";
import "@al-ft/midgard-core/ogmios-slot";
import "../src/transactions/reserve-payout.js";
import "./helpers/real-midgard-contracts.js";
import "./helpers/redeemer-inspection.js";
import "./reserve-payout-builders.submit-with-wallet.js";
import "./reserve-payout-builders.make-reserve-payout-builder-fixture.js";
import "./reserve-payout-builders.make-user-event-builder-fixture.js";
import "./reserve-payout-builders.make-reserve-lifecycle-builder-fixture.js";
import "./reserve-payout-builders.expect-add-funds-redeemer-layout.js";

import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";

import { SUBMIT_SLOT_LENGTH_MS } from "@al-ft/midgard-core/ogmios-slot";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import {
  submitAbsorbAfterProtectionProgram,
  submitInitializePayoutAfterProtectionProgram,
} from "../src/commands/reserve-payout.js";
import { openPlan } from "../src/services/intent-journal.js";
import {
  __reservePayoutTest,
  buildAbsorbConfirmedDepositToReserveTxProgram,
  buildAddReserveFundsToPayoutTxProgram,
  buildConcludePayoutTxProgram,
  buildInitializePayoutTxProgram,
  buildRefundInvalidWithdrawalTxProgram,
} from "../src/transactions/reserve-payout.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  expectAbsorbRedeemerLayout,
  expectAddFundsRedeemerLayout,
  expectConcludeRedeemerLayout,
  expectInitializeRedeemerLayout,
  expectRefundRedeemerLayout,
  expectRetirementLayout,
} from "./reserve-payout-builders.expect-add-funds-redeemer-layout.js";
import { makeReserveLifecycleBuilderFixture } from "./reserve-payout-builders.make-reserve-lifecycle-builder-fixture.js";
import {
  expectAuthenticateMintRedeemerLayout,
  makeReservePayoutBuilderFixture,
} from "./reserve-payout-builders.make-reserve-payout-builder-fixture.js";
import { makeUserEventBuilderFixture } from "./reserve-payout-builders.make-user-event-builder-fixture.js";
import {
  deploymentRecords,
  expectLeft,
  findUtxoWithUnit,
  loadRealContracts,
  mkUtxo,
  requireTxInputIndex,
  scriptRef,
  signedMeasurements,
  submitWithWallet,
  validityWindowSlots,
} from "./reserve-payout-builders.submit-with-wallet.js";

afterAll(() => {
  if (process.env.MIDGARD_BUILDERS_EVIDENCE_PATH !== undefined)
    writeFileSync(
      process.env.MIDGARD_BUILDERS_EVIDENCE_PATH,
      JSON.stringify(
        {
          blueprint: {
            path:
              process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
              new URL("../../../onchain/aiken/plutus.json", import.meta.url)
                .pathname,
            sha256: createHash("sha256")
              .update(
                readFileSync(
                  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
                    new URL(
                      "../../../onchain/aiken/plutus.json",
                      import.meta.url,
                    ),
                ),
              )
              .digest("hex"),
          },
          deployments: [...deploymentRecords.values()],
          transactions: signedMeasurements,
        },
        (_key, value) => (typeof value === "bigint" ? value.toString() : value),
        2,
      ) + "\n",
    );
});

describe("reserve/payout transaction builder primitives", () => {
  it("round-trips canonical SDK Value maps through Lucid assets", () => {
    const assets: Assets = {
      lovelace: 4_200_000n,
      [`${"ab".repeat(28)}${"cd".repeat(3)}`]: 17n,
      [`${"12".repeat(28)}${"34".repeat(2)}`]: 9n,
    };

    expect(
      __reservePayoutTest.valueToAssets(
        __reservePayoutTest.assetsToValue(assets),
      ),
    ).toEqual(assets);
  });

  it("normalizes PlutusData maps to Aiken cbor.serialise encoding for PHAS", () => {
    const outputReferenceCbor = Data.to(
      { transactionId: "01".repeat(32), outputIndex: 0n },
      SDK.OutputReference,
    );
    expect(
      __reservePayoutTest.aikenSerialisedPlutusDataCbor(outputReferenceCbor),
    ).toBe(
      "d8799f5820010101010101010101010101010101010101010101010101010101010101010100ff",
    );

    const valueCbor = Data.to(
      __reservePayoutTest.assetsToValue({ lovelace: 3_000_000n }),
      SDK.Value,
    );
    expect(__reservePayoutTest.aikenSerialisedPlutusDataCbor(valueCbor)).toBe(
      "a140a1401a002dc6c0",
    );
  });

  it("models a full reserve-funded withdrawal lifecycle with exact accounting", () => {
    const withdrawalPolicyId = "aa".repeat(28);
    const payoutPolicyId = "bb".repeat(28);
    const assetName = "01";
    const withdrawalUnit = `${withdrawalPolicyId}${assetName}`;
    const payoutUnit = `${payoutPolicyId}${assetName}`;
    const withdrawalAssets: Assets = {
      lovelace: 2_000_000n,
      [withdrawalUnit]: 1n,
    };
    const targetAssets: Assets = { lovelace: 7_000_000n };
    const reserveAssets: Assets = { lovelace: 8_000_000n };

    const initialPayoutAssets = __reservePayoutTest.addAssets(
      __reservePayoutTest.removeAssetUnit(withdrawalAssets, withdrawalUnit, 1n),
      { [payoutUnit]: 1n },
    );
    const currentPayoutAssets = __reservePayoutTest.removeAssetUnit(
      initialPayoutAssets,
      payoutUnit,
      1n,
    );
    const neededAssets = __reservePayoutTest.subtractAssets(
      targetAssets,
      currentPayoutAssets,
    );
    const collectedAssets = __reservePayoutTest.minPositiveAssets(
      reserveAssets,
      neededAssets,
    );
    const fundedPayoutAssets = __reservePayoutTest.addAssets(
      initialPayoutAssets,
      collectedAssets,
    );
    const reserveChangeAssets = __reservePayoutTest.subtractAssets(
      reserveAssets,
      collectedAssets,
    );
    const concludedL1Assets = __reservePayoutTest.removeAssetUnit(
      fundedPayoutAssets,
      payoutUnit,
      1n,
    );

    expect(initialPayoutAssets).toEqual({
      lovelace: 2_000_000n,
      [payoutUnit]: 1n,
    });
    expect(collectedAssets).toEqual({ lovelace: 5_000_000n });
    expect(fundedPayoutAssets).toEqual({
      lovelace: 7_000_000n,
      [payoutUnit]: 1n,
    });
    expect(reserveChangeAssets).toEqual({ lovelace: 3_000_000n });
    expect(
      __reservePayoutTest.assetsEqual(concludedL1Assets, targetAssets),
    ).toBe(true);
  });

  it("builds deposit authenticate mint redeemers from the final tx layout", async () => {
    const {
      beneficiary,
      contracts,
      depositMintingReference,
      hubOracleRefInput,
      lucid,
    } = await makeUserEventBuilderFixture();

    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, {
        additionalAssets: {},
        l2Address: beneficiary.address,
        l2Datum: null,
        lovelace: 5_000_000n,
        referenceScripts: {
          depositMinting: depositMintingReference,
        },
      }),
    );

    expectAuthenticateMintRedeemerLayout({
      tx: built.tx,
      policyId: contracts.deposit.policyId,
      eventAddress: built.metadata.depositAddress,
      eventUnit: built.metadata.depositAuthUnit,
      nonceInput: built.metadata.nonceInput,
      hubOracleRefInput,
    });
  });

  it("builds withdrawal authenticate mint redeemers from the final tx layout", async () => {
    const {
      beneficiary,
      contracts,
      hubOracleRefInput,
      lucid,
      withdrawalMintingReference,
    } = await makeUserEventBuilderFixture();
    const refundAddress = await Effect.runPromise(
      SDK.addressDataFromBech32(beneficiary.address),
    );

    const built = await Effect.runPromise(
      SDK.buildUnsignedWithdrawalTxWithMetadataProgram(lucid, contracts, {
        body: {
          l2_outref: {
            transactionId: "33".repeat(32),
            outputIndex: 0n,
          },
          l2_owner: "44".repeat(28),
          l2_value: __reservePayoutTest.assetsToValue({
            lovelace: 7_000_000n,
          }),
          l1_address: refundAddress,
          l1_datum: "NoDatum",
        },
        refundAddress,
        referenceScripts: {
          withdrawalMinting: withdrawalMintingReference,
        },
        signature: ["01", "02"],
      }),
    );

    expectAuthenticateMintRedeemerLayout({
      tx: built.tx,
      policyId: contracts.withdrawal.policyId,
      eventAddress: built.metadata.withdrawalAddress,
      eventUnit: built.metadata.withdrawalAuthUnit,
      nonceInput: built.metadata.nonceInput,
      hubOracleRefInput,
    });
  });

  it("builds, locally evaluates, and submits reserve funding plus payout conclusion", async () => {
    const {
      contracts,
      feeInputs,
      hubOracleRefInput,
      l1Address,
      lucid,
      payoutInput,
      payoutUnit,
      referenceScripts,
      reserveInput,
    } = await makeReservePayoutBuilderFixture();

    const addFunds = await Effect.runPromise(
      buildAddReserveFundsToPayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[0],
        payoutInput,
        referenceScripts,
        reserveInput,
      }),
    );
    expect(addFunds.layout.reserveChangeOutputIndex).not.toBeNull();
    expectAddFundsRedeemerLayout(addFunds);
    await lucid.awaitTx(await submitWithWallet(addFunds.tx));

    const fundedPayout = findUtxoWithUnit(
      await lucid.utxosAt(contracts.payout.spendingScriptAddress),
      payoutUnit,
    );
    expect(fundedPayout.assets.lovelace).toBe(7_000_000n);
    expect(
      (await lucid.utxosAt(contracts.reserve.spendingScriptAddress)).some(
        (utxo) => utxo.assets.lovelace === 4_000_000n,
      ),
    ).toBe(true);

    const conclude = await Effect.runPromise(
      buildConcludePayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[1],
        payoutInput: fundedPayout,
        referenceScripts,
      }),
    );
    expect(conclude.layout.l1OutputIndex).toBe(0n);
    expectConcludeRedeemerLayout(conclude);
    await lucid.awaitTx(await submitWithWallet(conclude.tx));

    expect(
      (await lucid.utxosAt(contracts.payout.spendingScriptAddress)).some(
        (utxo) => utxo.assets[payoutUnit] === 1n,
      ),
    ).toBe(false);
    expect(
      (await lucid.utxosAt(l1Address)).some(
        (utxo) => utxo.assets.lovelace === 7_000_000n,
      ),
    ).toBe(true);
  });

  it("skips a planted datum reserve UTxO, which the validators refuse, and funds from the honest reserve", async () => {
    const {
      contracts,
      feeInputs,
      hubOracleRefInput,
      lucid,
      payoutInput,
      payoutUnit,
      referenceScripts,
      reserveInput,
    } = await makeReservePayoutBuilderFixture({ plantedDatumReserve: true });
    const reserveUtxos = await lucid.utxosAt(
      contracts.reserve.spendingScriptAddress,
    );
    const planted = reserveUtxos.find((utxo) => utxo.datum != null);
    if (planted === undefined) throw new Error("Missing planted reserve UTxO");
    // The planted UTxO is listed first and ties on contribution, so without
    // the shape filter both first-match and canonical out-ref order take it.
    expect(reserveUtxos[0]).toEqual(planted);
    expect(compareOutRefs(planted, reserveInput)).toBeLessThan(0);
    const funding = (reserve: UTxO) =>
      buildAddReserveFundsToPayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[0],
        payoutInput,
        referenceScripts,
        reserveInput: reserve,
      });

    const refused = expectLeft(
      await Effect.runPromise(Effect.either(funding(planted))),
    );
    expect(refused.message).toMatch(/failed script execution\s+Spend\[\d+\]/);

    const selected = SDK.selectReserveFundingInput(
      reserveUtxos,
      SDK.subtractAssets(
        { lovelace: 7_000_000n },
        SDK.removeAssetUnit(payoutInput.assets, payoutUnit, 1n),
      ),
      lucid.config().protocolParameters!.coinsPerUtxoByte,
    );
    expect(selected).toEqual(reserveInput);
    const addFunds = await Effect.runPromise(funding(selected!));
    await lucid.awaitTx(await submitWithWallet(addFunds.tx));
    expect(
      findUtxoWithUnit(
        await lucid.utxosAt(contracts.payout.spendingScriptAddress),
        payoutUnit,
      ).assets.lovelace,
    ).toBe(7_000_000n);
    expect(await lucid.utxosByOutRef([planted])).toHaveLength(1);
  });

  it("builds and submits absorb, initialize, reserve collection, and payout conclusion", async () => {
    const {
      beneficiary,
      contracts,
      deposit,
      depositUnit,
      feeInputs,
      hubOracleRefInput,
      lucid,
      depositMembershipProof,
      withdrawalMembershipProof,
      referenceScriptsAddress,
      nowMs,
      payoutUnit,
      referenceScripts,
      reserveAddress,
      settlementRefInput,
      withdrawal,
    } = await makeReserveLifecycleBuilderFixture();

    const absorb = await Effect.runPromise(
      buildAbsorbConfirmedDepositToReserveTxProgram(lucid, contracts, {
        deposit,
        feeInput: feeInputs[0],
        hubOracleRefInput,
        membershipProof: depositMembershipProof,
        referenceScriptsAddress,
        nowMs,
        referenceScripts,
        settlementRefInput,
      }),
    );
    expect(absorb.layout.reserveOutputIndex).toBeGreaterThanOrEqual(0n);
    expectAbsorbRedeemerLayout(absorb);
    // The continued predecessor is protected until this window's upper bound
    // plus the deployment duration, so retirements keep the window short.
    expect(validityWindowSlots(absorb.tx)).toBe(180n);
    await lucid.awaitTx(await submitWithWallet(absorb.tx));
    expect(
      (await lucid.utxosAt(contracts.deposit.spendingScriptAddress)).some(
        (utxo) => utxo.assets[depositUnit] === 1n,
      ),
    ).toBe(false);

    const reserveInput = (await lucid.utxosAt(reserveAddress)).find(
      (utxo) =>
        utxo.assets.lovelace === 8_000_000n &&
        Object.keys(utxo.assets).length === 1,
    );
    if (reserveInput === undefined) {
      throw new Error(
        "Deposit absorption did not create the expected reserve UTxO",
      );
    }

    const initialize = await Effect.runPromise(
      buildInitializePayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[1],
        membershipProof: withdrawalMembershipProof,
        referenceScriptsAddress,
        nowMs,
        referenceScripts,
        settlementRefInput,
        withdrawal,
      }),
    );
    expect(initialize.layout.payoutOutputIndex).toBeGreaterThanOrEqual(0n);
    expectInitializeRedeemerLayout(initialize, contracts);
    expect(validityWindowSlots(initialize.tx)).toBe(180n);
    await lucid.awaitTx(await submitWithWallet(initialize.tx));

    const initializedPayout = findUtxoWithUnit(
      await lucid.utxosAt(contracts.payout.spendingScriptAddress),
      payoutUnit,
    );
    expect(initializedPayout.assets.lovelace).toBe(3_000_000n);

    const addFunds = await Effect.runPromise(
      buildAddReserveFundsToPayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[2],
        payoutInput: initializedPayout,
        referenceScripts,
        reserveInput,
      }),
    );
    expect(addFunds.layout.reserveChangeOutputIndex).not.toBeNull();
    expectAddFundsRedeemerLayout(addFunds);
    await lucid.awaitTx(await submitWithWallet(addFunds.tx));

    const fundedPayout = findUtxoWithUnit(
      await lucid.utxosAt(contracts.payout.spendingScriptAddress),
      payoutUnit,
    );
    expect(fundedPayout.assets.lovelace).toBe(7_000_000n);
    expect(
      (await lucid.utxosAt(reserveAddress)).some(
        (utxo) =>
          utxo.assets.lovelace === 4_000_000n &&
          Object.keys(utxo.assets).length === 1,
      ),
    ).toBe(true);

    const conclude = await Effect.runPromise(
      buildConcludePayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[3],
        payoutInput: fundedPayout,
        referenceScripts,
      }),
    );
    expect(conclude.layout.l1OutputIndex).toBe(0n);
    expectConcludeRedeemerLayout(conclude);
    await lucid.awaitTx(await submitWithWallet(conclude.tx));

    expect(
      (await lucid.utxosAt(contracts.payout.spendingScriptAddress)).some(
        (utxo) => utxo.assets[payoutUnit] === 1n,
      ),
    ).toBe(false);
    expect(
      (await lucid.utxosAt(beneficiary.address)).some(
        (utxo) => utxo.assets.lovelace === 7_000_000n,
      ),
    ).toBe(true);
  });

  it("builds absorption and initialization with resolved history observer references", async () => {
    const {
      contracts,
      deposit,
      feeInputs,
      hubOracleRefInput,
      lucid,
      depositMembershipProof,
      withdrawalMembershipProof,
      referenceScriptsAddress,
      nowMs,
      referenceScripts,
      settlementRefInput,
      withdrawal,
    } = await makeReserveLifecycleBuilderFixture();
    const staticReferenceScripts = {
      depositMinting: referenceScripts.depositMinting,
      depositSpending: referenceScripts.depositSpending,
      withdrawalMinting: referenceScripts.withdrawalMinting,
      withdrawalSpending: referenceScripts.withdrawalSpending,
      payoutMinting: referenceScripts.payoutMinting,
    };

    const absorb = await Effect.runPromise(
      buildAbsorbConfirmedDepositToReserveTxProgram(lucid, contracts, {
        deposit,
        feeInput: feeInputs[0],
        hubOracleRefInput,
        membershipProof: depositMembershipProof,
        referenceScriptsAddress,
        nowMs,
        referenceScripts: staticReferenceScripts,
        settlementRefInput,
      }),
    );
    expect(absorb.layout.reserveOutputIndex).toBeGreaterThanOrEqual(0n);
    expectAbsorbRedeemerLayout(absorb);

    const initialize = await Effect.runPromise(
      buildInitializePayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[1],
        membershipProof: withdrawalMembershipProof,
        referenceScriptsAddress,
        nowMs,
        referenceScripts: staticReferenceScripts,
        settlementRefInput,
        withdrawal,
      }),
    );
    expect(initialize.layout.payoutOutputIndex).toBeGreaterThanOrEqual(0n);
    expectInitializeRedeemerLayout(initialize, contracts);
  });

  it("builds and submits the invalid-withdrawal refund path", async () => {
    const {
      beneficiary,
      contracts,
      feeInputs,
      hubOracleRefInput,
      lucid,
      withdrawalMembershipProof,
      referenceScriptsAddress,
      nowMs,
      referenceScripts,
      settlementRefInput,
      withdrawal,
      withdrawalUnit,
    } = await makeReserveLifecycleBuilderFixture({
      settlementWithdrawalValidity: "UnpayableWithdrawalValue",
    });

    const refund = await Effect.runPromise(
      buildRefundInvalidWithdrawalTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[0],
        membershipProof: withdrawalMembershipProof,
        referenceScriptsAddress,
        nowMs,
        referenceScripts,
        settlementRefInput,
        validityOverride: "UnpayableWithdrawalValue",
        withdrawal,
      }),
    );
    expect(refund.layout.refundOutputIndex).toBe(1n);
    expectRefundRedeemerLayout(refund, "UnpayableWithdrawalValue");
    await lucid.awaitTx(await submitWithWallet(refund.tx));

    expect(
      (await lucid.utxosAt(contracts.withdrawal.spendingScriptAddress)).some(
        (utxo) => utxo.assets[withdrawalUnit] === 1n,
      ),
    ).toBe(false);
    expect(
      (await lucid.utxosAt(beneficiary.address)).some(
        (utxo) => utxo.assets.lovelace === 3_000_000n,
      ),
    ).toBe(true);
  });

  it.each([
    { kind: "Deposit", refund: false },
    { kind: "Withdrawal", refund: false },
    { kind: "Withdrawal", refund: true },
  ] as const)(
    "retires external $kind data (refund=$refund), then reclaims through its exact owner",
    async ({ kind, refund }) => {
      const f = await makeReserveLifecycleBuilderFixture({
        externalKind: kind,
        settlementWithdrawalValidity: refund
          ? "UnpayableWithdrawalValue"
          : "WithdrawalIsValid",
        scriptOwner: kind === "Withdrawal",
      });
      const reclaimConfig: SDK.ReclaimEventHistoryDataConfig = {
        kind,
        retainedInput: f.retainedInput!,
        hubOracleRefInput: f.hubOracleRefInput,
        ...(kind === "Withdrawal"
          ? {
              scriptAuthorization: {
                script: f.authorizationScript,
                redeemer: Data.void(),
              },
            }
          : {}),
      };
      const live = await Effect.runPromise(
        Effect.either(
          SDK.buildReclaimEventHistoryDataTxProgram(
            f.lucid,
            f.contracts,
            reclaimConfig,
          ),
        ),
      );
      expect(String(expectLeft(live).cause)).toContain("still present");
      const config = {
        hubOracleRefInput: f.hubOracleRefInput,
        settlementRefInput: f.settlementRefInput,
        referenceScriptsAddress: f.referenceScriptsAddress,
        nowMs: f.nowMs,
      };
      const retired =
        kind === "Deposit"
          ? await Effect.runPromise(
              buildAbsorbConfirmedDepositToReserveTxProgram(
                f.lucid,
                f.contracts,
                {
                  ...config,
                  deposit: f.deposit,
                  membershipProof: f.depositMembershipProof,
                },
              ),
            )
          : refund
            ? await Effect.runPromise(
                buildRefundInvalidWithdrawalTxProgram(f.lucid, f.contracts, {
                  ...config,
                  withdrawal: f.withdrawal,
                  membershipProof: f.withdrawalMembershipProof,
                  validityOverride: "UnpayableWithdrawalValue",
                }),
              )
            : await Effect.runPromise(
                buildInitializePayoutTxProgram(f.lucid, f.contracts, {
                  ...config,
                  withdrawal: f.withdrawal,
                  membershipProof: f.withdrawalMembershipProof,
                }),
              );
      expect(retired.layout.witness.external_reference_index).not.toBeNull();
      expectRetirementLayout(retired);
      await f.lucid.awaitTx(await submitWithWallet(retired.tx));
      expect(await f.lucid.utxosByOutRef([f.retainedInput!])).toHaveLength(1);
      if (kind === "Withdrawal") {
        const unauthorized = await Effect.runPromise(
          Effect.either(
            SDK.buildReclaimEventHistoryDataTxProgram(f.lucid, f.contracts, {
              ...reclaimConfig,
              scriptAuthorization: undefined,
            }),
          ),
        );
        expect(String(expectLeft(unauthorized).cause)).toContain(
          "exact retained script credential",
        );
      }
      const reclaimed = await Effect.runPromise(
        SDK.buildReclaimEventHistoryDataTxProgram(
          f.lucid,
          f.contracts,
          reclaimConfig,
        ),
      );
      expect(reclaimed.layout.absenceReferenceIndex).toBe(
        requireTxInputIndex(
          reclaimed.tx.toTransaction().body().reference_inputs(),
          (
            await f.lucid.utxosAt(
              (kind === "Deposit" ? f.history.deposit : f.history.withdrawal)
                .list.spendingScriptAddress,
            )
          )[0]!,
          "absence witness",
        ),
      );
      await f.lucid.awaitTx(await submitWithWallet(reclaimed.tx));
      expect(await f.lucid.utxosByOutRef([f.retainedInput!])).toHaveLength(0);
    },
  );

  it("refreshes a moved Order and preserves its current predecessor when retiring", async () => {
    const f = await makeReserveLifecycleBuilderFixture();
    const candidates = await Promise.all(
      f.feeInputs.map(async (nonce) => ({
        nonce,
        key: await Effect.runPromise(
          SDK.eventHistoryKey({
            transactionId: nonce.txHash,
            outputIndex: BigInt(nonce.outputIndex),
          }),
        ),
      })),
    );
    const following = candidates.find(
      (candidate) => candidate.key > f.deposit.assetName,
    );
    if (following === undefined)
      throw new Error("Fixture has no insertion nonce after the deposit");
    const inserted = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(f.lucid, f.contracts, {
        nonceInput: following.nonce,
        l2Address: f.beneficiary.address,
        l2Datum: null,
        lovelace: 4_000_000n,
        additionalAssets: {},
        referenceScripts: { depositMinting: f.referenceScripts.depositMinting },
        validity: {
          validFrom: f.emulator.time - 60_000,
          validTo: f.emulator.time + 20_000,
        },
      }),
    );
    await f.lucid.awaitTx(await submitWithWallet(inserted.tx));
    const moved = (
      await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(
          f.lucid,
          SDK.eventHistoryDeploymentFromContracts(f.history.deposit),
        ),
      )
    ).find((event) => event.assetName === f.deposit.assetName)!;
    expect(moved.utxo.txHash).not.toBe(f.deposit.utxo.txHash);
    expect(moved.history.anchor.node.next).toBe(following.key);
    f.emulator.awaitBlock(5);
    const rootBefore = (
      await f.lucid.utxosAt(f.history.deposit.list.spendingScriptAddress)
    ).find((utxo) => utxo.assets[f.history.deposit.list.policyId] === 1n)!;
    const retired = await Effect.runPromise(
      buildAbsorbConfirmedDepositToReserveTxProgram(f.lucid, f.contracts, {
        deposit: f.deposit,
        hubOracleRefInput: f.hubOracleRefInput,
        settlementRefInput: f.settlementRefInput,
        referenceScriptsAddress: f.referenceScriptsAddress,
        nowMs: f.emulator.time,
        membershipProof: f.depositMembershipProof,
      }),
    );
    expect(retired.layout.depositInputIndex).toBe(
      requireTxInputIndex(
        retired.tx.toTransaction().body().inputs(),
        moved.utxo,
        "refreshed Order",
      ),
    );
    const continued = coreToTxOutput(
      retired.tx
        .toTransaction()
        .body()
        .outputs()
        .get(Number(retired.layout.witness.predecessor_output_index)),
    );
    expect(continued.assets).toEqual(rootBefore.assets);
    expect(Data.from(continued.datum!, SDK.EventHistoryNode)).toMatchObject({
      position: "Root",
      next: following.key,
      payload: "RootContent",
    });
    await f.lucid.awaitTx(await submitWithWallet(retired.tx));
    const remaining = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        f.lucid,
        SDK.eventHistoryDeploymentFromContracts(f.history.deposit),
      ),
    );
    expect(remaining.map((event) => event.assetName)).toEqual([following.key]);
  });

  it.each(["absorb", "initialize", "refund"] as const)(
    "refuses %s with settlement membership that does not authenticate the event",
    async (purpose) => {
      const f = await makeReserveLifecycleBuilderFixture({
        settlementWithdrawalValidity:
          purpose === "refund"
            ? "UnpayableWithdrawalValue"
            : "WithdrawalIsValid",
      });
      const config = {
        hubOracleRefInput: f.hubOracleRefInput,
        settlementRefInput: f.settlementRefInput,
        referenceScriptsAddress: f.referenceScriptsAddress,
        nowMs: f.nowMs,
      };
      const program: Effect.Effect<
        unknown,
        | SDK.ReservePayoutTxError
        | SDK.HubOracleError
        | SDK.LucidError
        | SDK.Bech32DeserializationError
        | SDK.StateQueueError
      > =
        purpose === "absorb"
          ? buildAbsorbConfirmedDepositToReserveTxProgram(
              f.lucid,
              f.contracts,
              {
                ...config,
                deposit: f.deposit,
                membershipProof: { ...f.depositMembershipProof, count: 2n },
              },
            )
          : purpose === "initialize"
            ? buildInitializePayoutTxProgram(f.lucid, f.contracts, {
                ...config,
                withdrawal: f.withdrawal,
                membershipProof: { ...f.withdrawalMembershipProof, count: 2n },
              })
            : buildRefundInvalidWithdrawalTxProgram(f.lucid, f.contracts, {
                ...config,
                withdrawal: f.withdrawal,
                membershipProof: { ...f.withdrawalMembershipProof, count: 2n },
                validityOverride: "UnpayableWithdrawalValue",
              });
      const failure = await Effect.runPromise(Effect.either(program));
      expect(expectLeft(failure).message).toContain("local UPLC evaluation");
    },
  );

  it("refuses deposit retirement before its eligibility interval is confirmed", async () => {
    const f = await makeReserveLifecycleBuilderFixture({ confirmedEnd: 0n });
    const result = await Effect.runPromise(
      Effect.either(
        buildAbsorbConfirmedDepositToReserveTxProgram(f.lucid, f.contracts, {
          deposit: f.deposit,
          hubOracleRefInput: f.hubOracleRefInput,
          settlementRefInput: f.settlementRefInput,
          referenceScriptsAddress: f.referenceScriptsAddress,
          nowMs: f.nowMs,
          membershipProof: f.depositMembershipProof,
        }),
      ),
    );
    expect(String(expectLeft(result).cause)).toContain("not confirmed");
  });

  it("rejects immutable Value drift and refuses to backdate below protection", async () => {
    const f = await makeReserveLifecycleBuilderFixture();
    const base = {
      hubOracleRefInput: f.hubOracleRefInput,
      settlementRefInput: f.settlementRefInput,
      referenceScriptsAddress: f.referenceScriptsAddress,
      nowMs: f.nowMs,
      membershipProof: f.depositMembershipProof,
    };
    const drift = await Effect.runPromise(
      Effect.either(
        buildAbsorbConfirmedDepositToReserveTxProgram(f.lucid, f.contracts, {
          ...base,
          deposit: { ...f.deposit, originalAssets: { lovelace: 1n } },
        }),
      ),
    );
    expect(String(expectLeft(drift).cause)).toContain(
      "immutable facts or original Value",
    );
    const protectedUntil = BigInt(Date.now() + 1_000_000);
    const protectedFixture = await makeReserveLifecycleBuilderFixture({
      protectedUntil,
    });
    const protectedResult = await Effect.runPromise(
      Effect.either(
        buildAbsorbConfirmedDepositToReserveTxProgram(
          protectedFixture.lucid,
          protectedFixture.contracts,
          {
            ...base,
            deposit: protectedFixture.deposit,
            hubOracleRefInput: protectedFixture.hubOracleRefInput,
            settlementRefInput: protectedFixture.settlementRefInput,
            referenceScriptsAddress: protectedFixture.referenceScriptsAddress,
            nowMs: protectedFixture.nowMs,
            membershipProof: protectedFixture.depositMembershipProof,
          },
        ),
      ),
    );
    const protectedCause = expectLeft(protectedResult).cause;
    expect(String(protectedCause)).toContain("still protected");
    // Callers wait on the exact bound the builder refused below.
    expect(protectedCause).toBeInstanceOf(SDK.HistoryRetirementProtectedError);
    expect(
      (protectedCause as SDK.HistoryRetirementProtectedError).protectedUntilMs,
    ).toBe(protectedUntil);
    expect(
      (protectedCause as SDK.HistoryRetirementProtectedError)
        .protectionDurationMs,
    ).toBe(protectedFixture.history.deposit.recipe.protectionDurationMs);
  });

  it.each(["absorb", "initialize"] as const)(
    "%s waits out a protected Order, then rebuilds and submits",
    async (retirement) => {
      // Far enough ahead to outlast a cold fixture build and the first build.
      const protectedUntil = BigInt(Date.now() + 12_000);
      const f = await makeReserveLifecycleBuilderFixture({ protectedUntil });
      const common = {
        hubOracleRefInput: f.hubOracleRefInput,
        referenceScripts: f.referenceScripts,
        referenceScriptsAddress: f.referenceScriptsAddress,
        settlementRefInput: f.settlementRefInput,
      };
      // Without protection at the first build nothing here would wait.
      expect(Number(protectedUntil) - Date.now()).toBeGreaterThan(4_000);
      const txHash = await runWithoutFollower(
        Effect.flatMap(openPlan, (plan) =>
          retirement === "absorb"
            ? submitAbsorbAfterProtectionProgram(
                f.lucid,
                f.contracts,
                {
                  ...common,
                  deposit: f.deposit,
                  membershipProof: f.depositMembershipProof,
                },
                plan,
              )
            : submitInitializePayoutAfterProtectionProgram(
                f.lucid,
                f.contracts,
                {
                  ...common,
                  withdrawal: f.withdrawal,
                  membershipProof: f.withdrawalMembershipProof,
                },
                plan,
              ),
        ),
      );
      expect(Date.now()).toBeGreaterThanOrEqual(
        Number(protectedUntil) + SUBMIT_SLOT_LENGTH_MS,
      );
      const [retired] = await f.lucid.utxosByOutRef([
        retirement === "absorb" ? f.deposit.utxo : f.withdrawal.utxo,
      ]);
      expect(retired).toBeUndefined();
      expect(txHash).toMatch(/^[0-9a-f]{64}$/);
    },
  );

  it("rejects explicit fee inputs that overlap protected protocol inputs", async () => {
    const protocolInput = mkUtxo("10", 0);
    const result = await Effect.runPromise(
      Effect.either(
        __reservePayoutTest.selectFeeInputProgram(
          {} as LucidEvolution,
          protocolInput,
          [protocolInput],
        ),
      ),
    );

    expect(expectLeft(result).message).toContain("overlaps");
  });

  it("rejects explicit fee inputs that carry non-ADA assets", async () => {
    const feeInput = mkUtxo("20", 0, {
      lovelace: 5_000_000n,
      [`${"ab".repeat(28)}${"cd".repeat(3)}`]: 1n,
    });
    const result = await Effect.runPromise(
      Effect.either(
        __reservePayoutTest.selectFeeInputProgram(
          {} as LucidEvolution,
          feeInput,
          [],
        ),
      ),
    );

    expect(expectLeft(result).message).toContain("pure ADA");
  });

  it("rejects explicit fee inputs that carry reference scripts", async () => {
    const feeInput = {
      ...mkUtxo("30", 0),
      scriptRef,
    };
    const result = await Effect.runPromise(
      Effect.either(
        __reservePayoutTest.selectFeeInputProgram(
          {} as LucidEvolution,
          feeInput,
          [],
        ),
      ),
    );

    expect(expectLeft(result).message).toContain("reference script");
  });

  it("rejects explicit fee inputs that carry datum payloads", async () => {
    const inlineDatumFeeInput = {
      ...mkUtxo("31", 0),
      datum: "d87980",
    };
    const inlineDatumResult = await Effect.runPromise(
      Effect.either(
        __reservePayoutTest.selectFeeInputProgram(
          {} as LucidEvolution,
          inlineDatumFeeInput,
          [],
        ),
      ),
    );

    expect(expectLeft(inlineDatumResult).message).toContain("inline datum");

    const datumHashFeeInput = {
      ...mkUtxo("32", 0),
      datumHash: "ab".repeat(32),
    };
    const datumHashResult = await Effect.runPromise(
      Effect.either(
        __reservePayoutTest.selectFeeInputProgram(
          {} as LucidEvolution,
          datumHashFeeInput,
          [],
        ),
      ),
    );

    expect(expectLeft(datumHashResult).message).toContain("datum hash");
  });

  it("rejects explicit fee inputs that do not belong to the selected wallet", async () => {
    const feeInput = {
      ...mkUtxo("33", 0),
      address: "addr_test1other",
    };
    const lucid = {
      wallet: () => ({
        address: async () => "addr_test1operator",
      }),
    } as unknown as LucidEvolution;

    const result = await Effect.runPromise(
      Effect.either(
        __reservePayoutTest.selectFeeInputProgram(lucid, feeInput, []),
      ),
    );

    expect(expectLeft(result).message).toContain("selected wallet");
  });

  it("filters unsafe wallet UTxOs out of automatic fee and completion candidates", async () => {
    const referenceScriptUtxo = {
      ...mkUtxo("30", 0, { lovelace: 20_000_000n }),
      scriptRef,
    };
    const inlineDatumUtxo = {
      ...mkUtxo("31", 0, { lovelace: 30_000_000n }),
      datum: "d87980",
    };
    const datumHashUtxo = {
      ...mkUtxo("32", 0, { lovelace: 40_000_000n }),
      datumHash: "cd".repeat(32),
    };
    const nonAdaUtxo = mkUtxo("33", 0, {
      lovelace: 50_000_000n,
      [`${"ab".repeat(28)}${"cd".repeat(3)}`]: 1n,
    });
    const plainUtxo = mkUtxo("40", 0, { lovelace: 3_000_000n });
    const lucid = {
      config: () => ({ provider: {} }),
      wallet: () => ({
        address: async () => "addr_test1operator",
      }),
      utxosAt: async () => [
        referenceScriptUtxo,
        inlineDatumUtxo,
        datumHashUtxo,
        nonAdaUtxo,
        plainUtxo,
      ],
    } as unknown as LucidEvolution;

    const selected = await Effect.runPromise(
      __reservePayoutTest.selectFeeInputProgram(lucid, undefined, []),
    );

    expect(selected).toEqual(plainUtxo);
    expect(
      __reservePayoutTest.disposableFeeInputCandidates(
        [
          referenceScriptUtxo,
          inlineDatumUtxo,
          datumHashUtxo,
          nonAdaUtxo,
          plainUtxo,
        ],
        [],
      ),
    ).toEqual([plainUtxo]);
  });

  it("fails with missing reference-script diagnostics for refund builders", async () => {
    const fixture = await makeReserveLifecycleBuilderFixture({
      settlementWithdrawalValidity: "UnpayableWithdrawalValue",
    });
    const result = await Effect.runPromise(
      Effect.either(
        buildRefundInvalidWithdrawalTxProgram(
          fixture.lucid,
          fixture.contracts,
          {
            withdrawal: fixture.withdrawal,
            hubOracleRefInput: fixture.hubOracleRefInput,
            settlementRefInput: fixture.settlementRefInput,
            membershipProof: fixture.withdrawalMembershipProof,
            validityOverride: "UnpayableWithdrawalValue",
            nowMs: fixture.nowMs,
            referenceScriptsAddress: fixture.beneficiary.address,
          },
        ),
      ),
    );
    const left = expectLeft(result);
    expect(String(left.cause)).toContain("withdrawal spending");
    expect(String(left.cause)).toContain(fixture.beneficiary.address);
  });

  it("validates explicit hub oracle reference inputs before builder assembly", async () => {
    const lucid = {
      config: () => ({ network: "Preprod" }),
    } as unknown as LucidEvolution;
    const contracts = await loadRealContracts({
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
    const result = await Effect.runPromise(
      Effect.either(
        buildRefundInvalidWithdrawalTxProgram(lucid, contracts, {
          hubOracleRefInput: mkUtxo("60", 0),
          withdrawal: {
            assetName: "bb".repeat(32),
            utxo: mkUtxo("61", 0),
          },
        } as any),
      ),
    );

    const left = expectLeft(result);
    expect(left.message).toContain("not authenticated");
    expect(left.cause).toMatchObject({
      hubOracleRefInput: `${"60".repeat(32)}#0`,
    });
  });
});
