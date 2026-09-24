import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

import { expect, it, vi } from "vitest";

import {
  absorbConfirmedDepositToReserveProgram,
  advanceEmulatorPastLatestBlockEndTime,
  assetsToValue,
  CML,
  commitConfirmRecoverAndMerge,
  Data,
  Database,
  Effect,
  ensureSeparateCollateralUtxo,
  fetchWithdrawalsOnceProgram,
  paymentCredentialOf,
  resolveEventSettlementProofProgram,
  runNodeCommandProgram,
  SDK,
  submitDepositWithDiagnostics,
  submitWithdrawalWithDiagnostics,
  utxosProgram,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProjectionLifecycle } from "./helpers/history-projection-lifecycle.js";

it("projects an ordinary invalid withdrawal through real settlement, refund and owner reclamation", async () => {
  const h = await openHistoryProjectionLifecycle();
  const { fixture, lucidService, globals } = h;
  const context = { fixture, lucidService, globals };
  const lucid = fixture.operatorLucid;
  const owner = fixture.depositorLucid;
  const pair = SDK.requireEventHistoryContracts(fixture.contracts);
  const command = <A>(effect: Parameters<typeof runNodeCommandProgram<A>>[0]) =>
    runNodeCommandProgram(effect, context);
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    vi.setSystemTime(fixture.emulator.now());
    const ownerAddress = await owner.wallet().address();
    await submitDepositWithDiagnostics(fixture, {
      l2Address: ownerAddress,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      ),
    );
    expect(deposits).toHaveLength(1);
    const deposit = deposits[0]!;
    await h.deployment.chain.awaitLedgerTime(
      Number(deposit.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await commitConfirmRecoverAndMerge(context);
    await ensureSeparateCollateralUtxo(lucid);
    await command(
      absorbConfirmedDepositToReserveProgram({
        eventId: deposit.idCbor.toString("hex"),
      }),
    );
    const reserves = await lucid.utxosAt(
      fixture.contracts.reserve.spendingScriptAddress,
    );
    expect(reserves.map((output) => output.assets)).toContainEqual(
      deposit.originalAssets,
    );
    const l2State = await Effect.runPromise(
      utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(l2State.utxoCount).toBe(1);
    expect(l2State.totals.lovelace).toBe(12_000_000n);
    const target = l2State.utxos[0]!;
    const ownerKey = CML.PrivateKey.from_bech32(
      walletFromSeed(fixture.depositorAccount.seedPhrase, {
        network: "Preprod",
      }).paymentKey,
    );
    const ownerAddressData = await Effect.runPromise(
      SDK.addressDataFromBech32(ownerAddress),
    );
    // An owner-signed ordinary request names the real UTxO but the wrong value.
    const body: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: target.txHash,
        outputIndex: BigInt(target.outputIndex),
      },
      l2_owner: paymentCredentialOf(ownerAddress).hash,
      l2_value: assetsToValue({ lovelace: 11_000_000n }),
      l1_address: ownerAddressData,
      l1_datum: "NoDatum",
    };
    const refundData = "ab".repeat(600);
    const submitted = await submitWithdrawalWithDiagnostics(fixture, {
      body,
      signature: SDK.signWithdrawalBody(ownerKey, body),
      refundAddress: ownerAddressData,
      refundDatum: { InlineDatum: { data: refundData } },
    });
    const withdrawals = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
      ),
    );
    expect(withdrawals).toHaveLength(1);
    const withdrawal = withdrawals[0]!;
    expect(submitted.withdrawalEventId).toBe(withdrawal.idCbor.toString("hex"));
    expect("External" in withdrawal.facts.location).toBe(true);
    const retained = withdrawal.history.retainedDataUtxo;
    if (retained === undefined)
      throw new Error("External admission must resolve its retained payload");
    await h.deployment.chain.awaitLedgerTime(
      Number(withdrawal.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    expect((await command(fetchWithdrawalsOnceProgram)).reconciledCount).toBe(
      1,
    );
    const block = await commitConfirmRecoverAndMerge(context);
    const resolution = await command(
      resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId: withdrawal.idCbor,
      }),
    );
    if (resolution.kind !== "withdrawal")
      throw new Error("Expected withdrawal settlement resolution");
    expect(resolution.validity).toBe("IncorrectWithdrawalValue");
    if (resolution.validity !== "IncorrectWithdrawalValue")
      throw new Error("Expected the exact incorrect-value settlement verdict");
    expect(resolution.root).toBe(block.queuedHeader.withdrawalsRoot);
    expect(resolution.settlementRefInput).toEqual(block.settlementUtxo);
    expect(
      await Effect.runPromise(
        utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
      ),
    ).toEqual(l2State);
    await ensureSeparateCollateralUtxo(lucid);
    const refunded = await Effect.runPromise(
      SDK.buildRefundInvalidWithdrawalTxProgram(lucid, fixture.contracts, {
        withdrawal,
        settlementRefInput: resolution.settlementRefInput,
        membershipProof: resolution.proof,
        validityOverride: resolution.validity,
        nowMs: fixture.emulator.now(),
        referenceScripts: {
          historyList: h.deployment.references.get("withdrawalSpend"),
          historyRetirement: h.deployment.references.get(
            "withdrawalHistoryRetirementWithdraw",
          ),
        },
      }),
    );
    const refundSigned = await refunded.tx.sign.withWallet().complete();
    const refundHash = await refundSigned.submit();
    expect(await lucid.awaitTx(refundHash)).toBe(true);
    await h.observer.flush();
    const [refundOutput] = await lucid.utxosByOutRef([
      {
        txHash: refundHash,
        outputIndex: Number(refunded.layout.refundOutputIndex),
      },
    ]);
    expect(refundOutput?.address).toBe(ownerAddress);
    expect(refundOutput?.assets).toEqual(withdrawal.originalAssets);
    expect(refundOutput?.datum).toBe(Data.to(refundData));
    expect(h.capture().history.withdrawals).toHaveLength(0);
    expect(await lucid.utxosByOutRef([retained])).toEqual([retained]);
    expect(
      await lucid.utxosAt(fixture.contracts.payout.spendingScriptAddress),
    ).toHaveLength(0);

    await ensureSeparateCollateralUtxo(owner);
    const hub = await Effect.runPromise(
      SDK.fetchHubOracleUTxOProgram(owner, {
        hubOracleAddress: fixture.contracts.hubOracle.spendingScriptAddress,
        hubOraclePolicyId: fixture.contracts.hubOracle.policyId,
      }),
    );
    const reclaimed = await Effect.runPromise(
      SDK.buildReclaimEventHistoryDataTxProgram(owner, fixture.contracts, {
        kind: "Withdrawal",
        retainedInput: retained,
        hubOracleRefInput: hub.utxo,
      }),
    );
    const reclaimSigned = await reclaimed.tx.sign.withWallet().complete();
    const reclaimHash = await reclaimSigned.submit();
    expect(await owner.awaitTx(reclaimHash)).toBe(true);
    await h.observer.flush();
    expect(h.observer.pendingCount()).toBe(0);
    expect(await lucid.utxosByOutRef([retained])).toHaveLength(0);
    expect(
      await lucid.utxosAt(fixture.contracts.reserve.spendingScriptAddress),
    ).toEqual(reserves);
    expect(
      await Effect.runPromise(
        utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
      ),
    ).toEqual(l2State);
    expect(h.capture().history.deposits).toHaveLength(0);
    expect(h.capture().history.withdrawals).toHaveLength(0);
    expect(
      h.transitions.map((transition) => ({
        kind: transition.kind,
        operation: transition.operation,
        reason: transition.retirement?.reason,
      })),
    ).toEqual([
      { kind: "deposit", operation: "InsertOrder", reason: undefined },
      { kind: "deposit", operation: "RetireOrder", reason: "absorbed" },
      { kind: "withdrawal", operation: "InsertOrder", reason: undefined },
      { kind: "withdrawal", operation: "RetireOrder", reason: "refunded" },
    ]);
    for (const [kind, original] of [
      ["deposit", deposit],
      ["withdrawal", withdrawal],
    ] as const) {
      const admitted = h.transitions.find(
        (transition) =>
          transition.kind === kind && transition.admission !== undefined,
      )!.admission!;
      const retired = h.transitions.find(
        (transition) =>
          transition.kind === kind && transition.retirement !== undefined,
      )!.retirement!.event;
      expect(retired.idCbor).toBe(original.idCbor.toString("hex"));
      expect(retired.factsCbor).toBe(admitted.factsCbor);
      expect(retired.originalAssetsCbor).toBe(admitted.originalAssetsCbor);
    }
    expect(
      h.receipts.some(({ transaction }) => transaction.txHash === refundHash),
    ).toBe(true);
    expect(
      h.receipts.some(({ transaction }) => transaction.txHash === reclaimHash),
    ).toBe(true);
    const evidencePath = process.env.MIDGARD_HISTORY_PROJECTION_EVIDENCE_PATH;
    if (evidencePath !== undefined) {
      mkdirSync(dirname(evidencePath), { recursive: true });
      writeFileSync(
        evidencePath,
        JSON.stringify(
          {
            scope:
              "Real owner-signed incorrect-value Withdrawal, node classification/commitment/attestation/mature merge, exact refund and owner-authorized external retention reclamation; synthetic observation transport only",
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            deploymentInfoSha256: h.deploymentInfoSha256,
            protocolParameters: h.deployment.manifest.cardanoProtocolParameters,
            validity: resolution.validity,
            refundHash,
            refundLayout: refunded.layout,
            refundOutput,
            retainedInput: retained,
            reclaimHash,
            reclaimLayout: reclaimed.layout,
            receipts: h.receipts,
            transitions: h.transitions,
          },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
  } finally {
    h.observer.restore();
    vi.useRealTimers();
  }
});
