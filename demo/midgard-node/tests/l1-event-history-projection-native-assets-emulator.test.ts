import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import {
  fromText,
  mintingPolicyToId,
  scriptFromNative,
  toUnit,
} from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  advanceEmulatorPastLatestBlockEndTime,
  assetsToValue,
  CML,
  commitConfirmRecoverAndMerge,
  concludePayoutProgram,
  Data,
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

// Opt-in diagnostics wrap the real WASM entry point before Lucid imports it.
// The default adapter, exact-request cache and completion paths stay in use.
const evaluatorCapture = vi.hoisted(() => {
  type RawArguments = [
    Uint8Array,
    Uint8Array[],
    Uint8Array[],
    Uint8Array,
    bigint,
    bigint,
    bigint,
    bigint,
    number,
  ];
  type RawEvaluator = (...args: RawArguments) => Uint8Array[];
  type CapturedEvaluation = {
    txCbor: string;
    inputCbor: string[];
    outputCbor: string[];
    costModelsCbor: string;
    maxSteps: bigint;
    maxMemory: bigint;
    zeroTime: bigint;
    zeroSlot: bigint;
    slotLength: number;
    resultRedeemers?: string[];
    failure?: string;
  };
  const state = {
    path: process.env.MIDGARD_HISTORY_EVALUATION_CAPTURE_PATH,
    enabled: false,
    evaluatorModulePath: "",
    requests: [] as CapturedEvaluation[],
    restore: () => {},
    namespaceReady: Promise.resolve(),
  };
  if (state.path === undefined) return state;
  const { createRequire } = process.getBuiltinModule("node:module");
  const { pathToFileURL } = process.getBuiltinModule("node:url");
  const require = createRequire(import.meta.url);
  const lucidRequire = createRequire(require.resolve("@lucid-evolution/lucid"));
  const uplcPath = lucidRequire.resolve("@lucid-evolution/uplc");
  const uplc = lucidRequire("@lucid-evolution/uplc") as {
    eval_phase_two_raw: RawEvaluator;
  };
  const descriptor = Object.getOwnPropertyDescriptor(
    uplc,
    "eval_phase_two_raw",
  );
  if (
    !descriptor?.writable ||
    !descriptor.configurable ||
    typeof descriptor.value !== "function"
  )
    throw new Error(
      "Real evaluator entry point cannot be transparently captured",
    );
  const original = uplc.eval_phase_two_raw;
  const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
  const forwarding: RawEvaluator = function (this: unknown, ...args) {
    if (!state.enabled) return Reflect.apply(original, this, args);
    const record: CapturedEvaluation = {
      txCbor: hex(args[0]),
      inputCbor: args[1].map(hex),
      outputCbor: args[2].map(hex),
      costModelsCbor: hex(args[3]),
      maxSteps: args[4],
      maxMemory: args[5],
      zeroTime: args[6],
      zeroSlot: args[7],
      slotLength: args[8],
    };
    state.requests.push(record);
    try {
      const result = Reflect.apply(original, this, args) as Uint8Array[];
      record.resultRedeemers = result.map(hex);
      return result;
    } catch (error) {
      record.failure = String(error);
      throw error;
    }
  };
  Object.defineProperty(uplc, "eval_phase_two_raw", {
    ...descriptor,
    value: forwarding,
  });
  state.restore = () =>
    Object.defineProperty(uplc, "eval_phase_two_raw", descriptor);
  state.evaluatorModulePath = uplcPath;
  state.namespaceReady = import(pathToFileURL(uplcPath).href).then(
    (namespace: { eval_phase_two_raw: RawEvaluator }) => {
      if (namespace.eval_phase_two_raw !== forwarding) {
        state.restore();
        throw new Error(
          "Lucid evaluator namespace was loaded before the capture hook",
        );
      }
    },
  );
  return state;
});

/** Successful node classification and actual mature merge establish both
 * settlement frontiers. The observation transport labels remain synthetic. */
it("preserves native assets through real node deposit settlement and withdrawal payout", async () => {
  await evaluatorCapture.namespaceReady;
  const h = await openHistoryProjectionLifecycle().catch((error: unknown) => {
    evaluatorCapture.restore();
    throw error;
  });
  const { fixture, lucidService, globals } = h;
  const context = { fixture, lucidService, globals };
  const lucid = fixture.operatorLucid;
  const pair = SDK.requireEventHistoryContracts(fixture.contracts);
  const owner = fixture.depositorLucid;
  const diagnostic: Record<string, unknown> = {};
  const command = <A>(effect: Parameters<typeof runNodeCommandProgram<A>>[0]) =>
    runNodeCommandProgram(effect, context);
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    vi.setSystemTime(fixture.emulator.now());
    const ownerAddress = await owner.wallet().address();
    const nativePolicy = scriptFromNative({
      type: "sig",
      keyHash: paymentCredentialOf(ownerAddress).hash,
    });
    const nativePolicyId = mintingPolicyToId(nativePolicy);
    const tokenAssets = {
      [toUnit(nativePolicyId, fromText("alpha"))]: 5n,
      [toUnit(nativePolicyId, fromText("beta"))]: 7n,
    };
    const depositedAssets = { lovelace: 12_000_000n, ...tokenAssets };
    // Ordinary minting is accepted by the same ledger before user admission.
    const mint = await owner
      .newTx()
      .mintAssets(tokenAssets)
      .attach.MintingPolicy(nativePolicy)
      .pay.ToAddress(ownerAddress, { lovelace: 20_000_000n, ...tokenAssets })
      .complete({ localUPLCEval: true });
    const mintSigned = await mint.sign.withWallet().complete();
    const mintHash = await mintSigned.submit();
    expect(await owner.awaitTx(mintHash)).toBe(true);
    owner.overrideUTxOs(await owner.utxosAt(ownerAddress));
    await h.observer.flush();
    expect(
      h.receipts.find(({ transaction }) => transaction.txHash === mintHash)
        ?.transaction.mint,
    ).toEqual(tokenAssets);
    await submitDepositWithDiagnostics(fixture, {
      l2Address: ownerAddress,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: tokenAssets,
    });
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      ),
    );
    expect(deposits).toHaveLength(1);
    const deposit = deposits[0]!;
    expect(deposit.originalAssets).toEqual(depositedAssets);
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
    expect(reserve!.assets).toEqual(depositedAssets);

    const l2State = await Effect.runPromise(
      utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(l2State.utxoCount).toBe(1);
    expect(l2State.totals).toEqual(depositedAssets);
    const target = l2State.utxos[0]!;
    expect(target.assets).toEqual(depositedAssets);
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
      l2_value: assetsToValue(target.assets),
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
    diagnostic.phase = "initializePayoutProgram";
    diagnostic.withdrawal = {
      idCbor: withdrawal.idCbor.toString("hex"),
      infoCbor: withdrawal.infoCbor.toString("hex"),
      requestedBodyCbor: Data.to(body, SDK.WithdrawalBody),
      l2Value: [...body.l2_value].map(([policy, assets]) => [
        policy,
        [...assets],
      ]),
      observedL2Value: [...withdrawal.event.info.body.l2_value].map(
        ([policy, assets]) => [policy, [...assets]],
      ),
      originalAssets: withdrawal.originalAssets,
      settlementRoot: resolution.root,
      settlementProof: resolution.proof,
      settlement: resolution.settlementRefInput,
    };
    evaluatorCapture.enabled = evaluatorCapture.path !== undefined;
    try {
      await command(initializePayoutProgram({ eventId }));
    } catch (error) {
      if (evaluatorCapture.path !== undefined) {
        expect(evaluatorCapture.requests.length).toBeGreaterThan(0);
        expect(
          evaluatorCapture.requests.some(
            (request) => request.failure !== undefined,
          ),
        ).toBe(true);
      }
      throw error;
    } finally {
      evaluatorCapture.enabled = false;
    }
    diagnostic.phase = "reserve funding and payout conclusion";
    const payoutUnit = toUnit(
      fixture.contracts.payout.policyId,
      withdrawal.assetName,
    );
    const [initialized] = await lucid.utxosAtWithUnit(
      fixture.contracts.payout.spendingScriptAddress,
      payoutUnit,
    );
    expect(initialized?.assets).toEqual({
      ...withdrawal.originalAssets,
      [payoutUnit]: 1n,
    });
    expect(withdrawal.originalAssets.lovelace).toBeGreaterThan(0n);
    expect(withdrawal.originalAssets.lovelace).toBeLessThan(
      depositedAssets.lovelace,
    );
    expect(h.capture().history.withdrawals).toHaveLength(0);
    await ensureSeparateCollateralUtxo(lucid);
    const added = await command(addReserveFundsToPayoutProgram({ eventId }));
    expect(added.details.reserveOutRef).toBe(
      `${reserve!.txHash}#${reserve!.outputIndex}`,
    );
    expect((await command(payoutStatusProgram(eventId))).phase).toBe("funded");
    const [funded] = await lucid.utxosAtWithUnit(
      fixture.contracts.payout.spendingScriptAddress,
      payoutUnit,
    );
    expect(funded?.assets).toEqual({ ...depositedAssets, [payoutUnit]: 1n });
    const reserveChange = await lucid.utxosAt(
      fixture.contracts.reserve.spendingScriptAddress,
    );
    expect(reserveChange).toHaveLength(1);
    // The original request ADA funds the accumulator; exactly that ADA remains
    // in reserve after all target native assets and the missing ADA are taken.
    expect(reserveChange[0]!.assets).toEqual({
      lovelace: withdrawal.originalAssets.lovelace,
    });
    expect(added.details.remainingAssetsBeforeFunding).toEqual({
      ...tokenAssets,
      lovelace: depositedAssets.lovelace - withdrawal.originalAssets.lovelace!,
    });
    const concluded = await command(concludePayoutProgram({ eventId }));
    expect(concluded.details.paidAssets).toEqual(depositedAssets);
    expect((await command(payoutStatusProgram(eventId))).phase).toBe(
      "concluded",
    );
    const paidOutputs = (await lucid.utxosAt(ownerAddress)).filter(
      (output) => output.txHash === concluded.txHash,
    );
    expect(paidOutputs).toHaveLength(1);
    expect(paidOutputs[0]!.assets).toEqual(depositedAssets);
    expect(
      await lucid.utxosAt(fixture.contracts.reserve.spendingScriptAddress),
    ).toEqual(reserveChange);
    expect(
      await lucid.utxosAtWithUnit(
        fixture.contracts.payout.spendingScriptAddress,
        payoutUnit,
      ),
    ).toHaveLength(0);
    for (const [unit, quantity] of Object.entries(tokenAssets)) {
      expect(
        (await lucid.utxosAt(ownerAddress)).reduce(
          (total, output) => total + (output.assets[unit] ?? 0n),
          0n,
        ),
      ).toBe(quantity);
    }
    await h.observer.flush();
    expect(h.observer.pendingCount()).toBe(0);
    const netMint = (unit: string) =>
      h.receipts.reduce(
        (total, receipt) => total + (receipt.transaction.mint[unit] ?? 0n),
        0n,
      );
    for (const [unit, quantity] of Object.entries(tokenAssets))
      expect(netMint(unit)).toBe(quantity);
    for (const unit of [
      toUnit(pair.deposit.list.policyId, deposit.assetName),
      toUnit(pair.withdrawal.list.policyId, withdrawal.assetName),
      payoutUnit,
    ])
      expect(netMint(unit)).toBe(0n);
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
              "Ordinary accepted native mint and exact multiasset Value through real node L2 classification, commitment, attestation, mature merge, retirement and payout; synthetic observation transport only",
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            deploymentInfoSha256: h.deploymentInfoSha256,
            protocolParameters: h.deployment.manifest.cardanoProtocolParameters,
            nativePolicyId,
            tokenAssets,
            depositedAssets,
            mintHash,
            initializedPayout: initialized,
            fundedPayout: funded,
            reserveChange,
            paidOutputs,
            receipts: h.receipts,
            transitions: h.transitions,
          },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
  } catch (error) {
    const failure = inspect(error, { depth: null, customInspect: false });
    const evidencePath = process.env.MIDGARD_HISTORY_PROJECTION_EVIDENCE_PATH;
    if (evidencePath !== undefined) {
      mkdirSync(dirname(evidencePath), { recursive: true });
      writeFileSync(
        evidencePath,
        JSON.stringify(
          {
            scope:
              "Partial actual accepted native-asset lifecycle; synthetic observation transport only; failed retirement is not claimed accepted",
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            deploymentInfoSha256: h.deploymentInfoSha256,
            protocolParameters: h.deployment.manifest.cardanoProtocolParameters,
            diagnostic,
            failure,
            receipts: h.receipts,
            transitions: h.transitions,
          },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
    throw new Error(failure);
  } finally {
    evaluatorCapture.enabled = false;
    evaluatorCapture.restore();
    if (evaluatorCapture.path !== undefined) {
      mkdirSync(dirname(evaluatorCapture.path), { recursive: true });
      writeFileSync(
        evaluatorCapture.path,
        JSON.stringify(
          {
            scope:
              "Exact real default Aiken evaluator calls during payout initialization; arguments/results/errors forwarded unchanged",
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            protocolParameters: h.deployment.manifest.cardanoProtocolParameters,
            evaluatorModulePath: evaluatorCapture.evaluatorModulePath,
            requests: evaluatorCapture.requests,
          },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
    h.observer.restore();
    vi.useRealTimers();
  }
});
