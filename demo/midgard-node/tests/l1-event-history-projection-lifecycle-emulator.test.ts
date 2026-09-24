import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

import { expect, it, vi } from "vitest";

import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  advanceEmulatorPastLatestBlockEndTime,
  assetsToValue,
  CML,
  commitConfirmRecoverAndMerge,
  concludePayoutProgram,
  Database,
  Effect,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchWithdrawalsOnceProgram,
  initializePayoutProgram,
  paymentCredentialOf,
  payoutStatusProgram,
  resolveEventSettlementProofProgram,
  runNodeCommandProgram,
  SDK,
  submitDepositWithDiagnostics,
  submitWithdrawalWithDiagnostics,
  utxosProgram,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProjectionLifecycle } from "./helpers/history-projection-lifecycle.js";

/** Successful node classification and actual mature merge establish both
 * settlement frontiers. The observation transport labels remain synthetic. */
it("projects a real node deposit settlement and withdrawal payout to conclusion", async () => {
  const h = await openHistoryProjectionLifecycle();
  const { fixture, lucidService, globals } = h;
  const context = { fixture, lucidService, globals };
  const lucid = fixture.operatorLucid;
  const pair = SDK.requireEventHistoryContracts(fixture.contracts);
  const command = <A>(effect: Parameters<typeof runNodeCommandProgram<A>>[0]) =>
    runNodeCommandProgram(effect, context);
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    vi.setSystemTime(fixture.emulator.now());
    const ownerAddress = await fixture.depositorLucid.wallet().address();
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
    const depositBlock = await commitConfirmRecoverAndMerge(context);
    const depositRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.deposits,
      [{ key: deposit.idCbor, value: deposit.infoCbor }],
    );
    expect(depositBlock.queuedHeader.depositsRoot).toBe(depositRoot);
    const depositId = deposit.idCbor.toString("hex");
    const depositResolution = await command(
      resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId: deposit.idCbor,
      }),
    );
    expect(depositResolution.root).toBe(depositRoot);
    expect(depositResolution.settlementRefInput).toEqual(
      depositBlock.settlementUtxo,
    );
    await ensureSeparateCollateralUtxo(lucid);
    const absorbed = await command(
      absorbConfirmedDepositToReserveProgram({ eventId: depositId }),
    );
    expect(absorbed.details.depositOutRef).toBe(
      `${deposit.utxo.txHash}#${deposit.utxo.outputIndex}`,
    );
    expect(h.capture().history.deposits).toHaveLength(0);
    const reserve = (
      await lucid.utxosAt(fixture.contracts.reserve.spendingScriptAddress)
    ).find((output) => output.assets.lovelace === 12_000_000n);
    expect(reserve).toBeDefined();

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
    const body: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: target.txHash,
        outputIndex: BigInt(target.outputIndex),
      },
      l2_owner: paymentCredentialOf(ownerAddress).hash,
      l2_value: assetsToValue({ lovelace: 12_000_000n }),
      l1_address: ownerAddressData,
      l1_datum: "NoDatum",
    };
    const submitted = await submitWithdrawalWithDiagnostics(fixture, {
      body,
      signature: SDK.signWithdrawalBody(ownerKey, body),
      refundAddress: ownerAddressData,
      refundDatum: "NoDatum",
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
    await h.deployment.chain.awaitLedgerTime(
      Number(withdrawal.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    expect((await command(fetchWithdrawalsOnceProgram)).reconciledCount).toBe(
      1,
    );
    const withdrawalBlock = await commitConfirmRecoverAndMerge(context);
    const resolution = await command(
      resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId: withdrawal.idCbor,
      }),
    );
    expect(resolution.kind).toBe("withdrawal");
    if (resolution.kind !== "withdrawal")
      throw new Error("Expected withdrawal resolution");
    expect(resolution.validity).toBe("WithdrawalIsValid");
    expect(resolution.settlementRefInput).toEqual(
      withdrawalBlock.settlementUtxo,
    );
    expect(resolution.root).toBe(withdrawalBlock.queuedHeader.withdrawalsRoot);
    const emptyL2 = await Effect.runPromise(
      utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(emptyL2.utxoCount).toBe(0);
    const eventId = withdrawal.idCbor.toString("hex");
    await command(initializePayoutProgram({ eventId }));
    expect(h.capture().history.withdrawals).toHaveLength(0);
    await ensureSeparateCollateralUtxo(lucid);
    const added = await command(addReserveFundsToPayoutProgram({ eventId }));
    expect(added.details.reserveOutRef).toBe(
      `${reserve!.txHash}#${reserve!.outputIndex}`,
    );
    expect((await command(payoutStatusProgram(eventId))).phase).toBe("funded");
    await command(concludePayoutProgram({ eventId }));
    expect((await command(payoutStatusProgram(eventId))).phase).toBe(
      "concluded",
    );
    expect(
      (await lucid.utxosAt(ownerAddress)).some(
        (output) => output.assets.lovelace === 12_000_000n,
      ),
    ).toBe(true);
    await h.observer.flush();
    expect(h.observer.pendingCount()).toBe(0);
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
      {
        kind: "withdrawal",
        operation: "RetireOrder",
        reason: "payout_initialized",
      },
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
    const evidencePath = process.env.MIDGARD_HISTORY_PROJECTION_EVIDENCE_PATH;
    if (evidencePath !== undefined) {
      mkdirSync(dirname(evidencePath), { recursive: true });
      writeFileSync(
        evidencePath,
        JSON.stringify(
          {
            scope:
              "Real node L2 classification, commitment, attestation, mature merge, retirement and payout; synthetic observation transport only",
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            deploymentInfoSha256: h.deploymentInfoSha256,
            protocolParameters: h.deployment.manifest.cardanoProtocolParameters,
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
