import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

import { expect, it, vi } from "vitest";

import {
  absorbConfirmedDepositToReserveProgram,
  advanceEmulatorPastLatestBlockEndTime,
  commitConfirmRecoverAndMerge,
  Data,
  Database,
  Effect,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  resolveEventSettlementProofProgram,
  runNodeCommandProgram,
  SDK,
  submitDepositWithDiagnostics,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProjectionLifecycle } from "./helpers/history-projection-lifecycle.js";

it("projects external deposit admission, real settlement, absorption and owner reclamation", async () => {
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
    const l2Datum = Data.to("ab".repeat(600));
    await submitDepositWithDiagnostics(fixture, {
      l2Address: ownerAddress,
      l2Datum,
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
    expect("External" in deposit.facts.location).toBe(true);
    const retained = deposit.history.retainedDataUtxo;
    if (retained === undefined)
      throw new Error("External admission must resolve its retained payload");
    expect(await lucid.utxosByOutRef([retained])).toEqual([retained]);
    await h.deployment.chain.awaitLedgerTime(
      Number(deposit.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    const block = await commitConfirmRecoverAndMerge(context);
    const root = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.deposits,
      [{ key: deposit.idCbor, value: deposit.infoCbor }],
    );
    expect(block.queuedHeader.depositsRoot).toBe(root);
    const resolution = await command(
      resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId: deposit.idCbor,
      }),
    );
    expect(resolution.root).toBe(root);
    expect(resolution.settlementRefInput).toEqual(block.settlementUtxo);
    await ensureSeparateCollateralUtxo(lucid);
    await command(
      absorbConfirmedDepositToReserveProgram({
        eventId: deposit.idCbor.toString("hex"),
      }),
    );
    expect(h.capture().history.deposits).toHaveLength(0);
    expect(await lucid.utxosByOutRef([retained])).toEqual([retained]);
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

    await ensureSeparateCollateralUtxo(owner);
    const hub = await Effect.runPromise(
      SDK.fetchHubOracleUTxOProgram(owner, {
        hubOracleAddress: fixture.contracts.hubOracle.spendingScriptAddress,
        hubOraclePolicyId: fixture.contracts.hubOracle.policyId,
      }),
    );
    const reclaimed = await Effect.runPromise(
      SDK.buildReclaimEventHistoryDataTxProgram(owner, fixture.contracts, {
        kind: "Deposit",
        retainedInput: retained,
        hubOracleRefInput: hub.utxo,
      }),
    );
    const signed = await reclaimed.tx.sign.withWallet().complete();
    const reclaimHash = await signed.submit();
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
    ]);
    const admitted = h.transitions[0]!.admission!;
    const retired = h.transitions[1]!.retirement!.event;
    expect(retired.idCbor).toBe(deposit.idCbor.toString("hex"));
    expect(retired.factsCbor).toBe(admitted.factsCbor);
    expect(retired.originalAssetsCbor).toBe(admitted.originalAssetsCbor);
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
              "Real external Deposit admission, node commitment/attestation/mature merge, absorption and owner-authorized retention reclamation; synthetic observation transport only",
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            deploymentInfoSha256: h.deploymentInfoSha256,
            protocolParameters: h.deployment.manifest.cardanoProtocolParameters,
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
