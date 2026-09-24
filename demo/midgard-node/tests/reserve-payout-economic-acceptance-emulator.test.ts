import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as ordered,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Cbor, CborArray, CborMap, CborUInt } from "@harmoniclabs/cbor";
import {
  type Assets,
  coreToTxOutput,
  type LucidEvolution,
  SLOT_CONFIG_NETWORK,
  utxoToTransactionInput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";
import { eval_phase_two_raw } from "@lucid-evolution/uplc";
import { expect, it, vi } from "vitest";

import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  absorbConfirmedDepositToReserveProgram,
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  assetsToValue,
  CML,
  commitConfirmRecoverAndMerge,
  Data,
  Effect,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  initializePayoutProgram,
  paymentCredentialOf,
  payoutStatusProgram,
  refreshWalletUtxosFromProvider,
  resolveEventSettlementProofProgram,
  SDK,
  utxosProgram,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import { findRedeemerDataCbor } from "./helpers/redeemer-inspection.js";

const outputValue = (txCbor: string): Assets => {
  const outputs = CML.Transaction.from_cbor_hex(txCbor).body().outputs();
  const total: Assets = {};
  for (let i = 0; i < outputs.len(); i++)
    for (const [unit, quantity] of Object.entries(
      coreToTxOutput(outputs.get(i)).assets,
    ))
      total[unit] = (total[unit] ?? 0n) + quantity;
  return total;
};

// CBOR objects preserve map pairs, including the destination's duplicate keys.
// This changes outputs only; every other transaction field is retained.
const withOutputs = (
  signedCbor: string,
  replace: ReadonlyMap<number, CML.TransactionOutput>,
): string => {
  const tx = Cbor.parse(signedCbor);
  if (!(tx instanceof CborArray) || !(tx.array[0] instanceof CborMap))
    throw new Error("Expected a complete Cardano transaction");
  const entry = tx.array[0].map.find(
    ({ k }) => k instanceof CborUInt && k.num === 1n,
  );
  if (!(entry?.v instanceof CborArray))
    throw new Error("Missing transaction outputs");
  for (const [index, output] of replace) {
    if (entry.v.array[index] === undefined)
      throw new Error("Invalid output mutation index");
    entry.v.array[index] = Cbor.parse(output.to_cbor_hex());
  }
  return Cbor.encode(tx).toString();
};

const withRedeemer = (
  signedCbor: string,
  inputIndex: bigint,
  data: string,
): string => {
  const tx = Cbor.parse(signedCbor);
  if (!(tx instanceof CborArray) || !(tx.array[1] instanceof CborMap))
    throw new Error("Expected complete transaction witnesses");
  const redeemers = tx.array[1].map.find(
    ({ k }) => k instanceof CborUInt && k.num === 5n,
  )?.v;
  const targeted = (tag: unknown, index: unknown) =>
    tag instanceof CborUInt &&
    tag.num === BigInt(CML.RedeemerTag.Spend) &&
    index instanceof CborUInt &&
    index.num === inputIndex;
  let replacements = 0;
  // Both ledger encodings are valid. Replace only the data item, retaining
  // the original map/list representation, order, indices and execution units.
  if (redeemers instanceof CborMap) {
    for (const { k, v } of redeemers.map) {
      if (
        !(k instanceof CborArray) ||
        k.array.length !== 2 ||
        !(v instanceof CborArray) ||
        v.array.length !== 2
      )
        throw new Error("Invalid completed redeemer map entry");
      if (targeted(k.array[0], k.array[1])) {
        v.array[0] = Cbor.parse(data);
        replacements++;
      }
    }
  } else if (redeemers instanceof CborArray) {
    for (const entry of redeemers.array) {
      if (!(entry instanceof CborArray) || entry.array.length !== 4)
        throw new Error("Invalid completed legacy redeemer entry");
      if (targeted(entry.array[0], entry.array[1])) {
        entry.array[2] = Cbor.parse(data);
        replacements++;
      }
    }
  } else {
    throw new Error("Expected completed redeemer map or list");
  }
  if (replacements !== 1)
    throw new Error("Missing targeted payout spend redeemer");
  const mutated = Cbor.encode(tx).toString();
  expect(CML.Transaction.from_cbor_hex(mutated).body().to_cbor_hex()).toBe(
    CML.Transaction.from_cbor_hex(signedCbor).body().to_cbor_hex(),
  );
  return mutated;
};

const evaluationContext = async (lucid: LucidEvolution, signedCbor: string) => {
  const body = CML.Transaction.from_cbor_hex(signedCbor).body();
  const refs = [body.inputs(), body.reference_inputs()].flatMap((inputs) =>
    Array.from({ length: inputs?.len() ?? 0 }, (_, i) => {
      const input = inputs!.get(i);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    }),
  );
  const resolved = await lucid.utxosByOutRef(refs);
  expect(resolved).toHaveLength(refs.length);
  const utxos = refs.map((ref) => {
    const found = resolved.find(
      (utxo) =>
        utxo.txHash === ref.txHash && utxo.outputIndex === ref.outputIndex,
    );
    if (found === undefined) throw new Error("Missing actual evaluation input");
    return found;
  });
  const config = lucid.config();
  const slots = config.slotConfig ?? SLOT_CONFIG_NETWORK[config.network!];
  if (
    slots === undefined ||
    config.costModels === undefined ||
    config.protocolParameters === undefined
  )
    throw new Error("Missing actual evaluator parameters");
  const inputCbor = utxos.map((utxo) =>
    utxoToTransactionInput(utxo).to_cbor_hex(),
  );
  const outputCbor = utxos.map((utxo) =>
    utxoToTransactionOutput(utxo).to_cbor_hex(),
  );
  const costModelsCbor = config.costModels.to_cbor_hex();
  const parameters = config.protocolParameters;
  const evaluate = (cbor: string) =>
    eval_phase_two_raw(
      Buffer.from(cbor, "hex"),
      inputCbor.map((value) => Buffer.from(value, "hex")),
      outputCbor.map((value) => Buffer.from(value, "hex")),
      Buffer.from(costModelsCbor, "hex"),
      parameters.maxTxExSteps,
      parameters.maxTxExMem,
      BigInt(slots.zeroTime),
      BigInt(slots.zeroSlot),
      slots.slotLength,
    ).map((bytes) => Buffer.from(bytes).toString("hex"));
  const positive = evaluate(signedCbor);
  const redeemers = CML.Transaction.from_cbor_hex(signedCbor)
    .witness_set()
    .redeemers()!
    .to_flat_format();
  expect(positive).toHaveLength(redeemers.len());
  expect(signedCbor.length / 2).toBeLessThanOrEqual(parameters.maxTxSize);
  let memory = 0n;
  let steps = 0n;
  for (let i = 0; i < redeemers.len(); i++) {
    memory += redeemers.get(i).ex_units().mem();
    steps += redeemers.get(i).ex_units().steps();
  }
  expect(memory).toBeLessThanOrEqual(parameters.maxTxExMem);
  expect(steps).toBeLessThanOrEqual(parameters.maxTxExSteps);
  // Establish that the mutation serializer itself preserves the usable script
  // context, including ordered duplicate datum pairs, before testing changes.
  expect(evaluate(withOutputs(signedCbor, new Map()))).toEqual(positive);
  return {
    evaluate,
    evidence: {
      signedCbor,
      inputCbor,
      outputCbor,
      costModelsCbor,
      parameters,
      slots,
      positive,
    },
  };
};

/** Real owner-backed settlement establishes payout authority. Mutants are
 * evaluated only; their unchanged signatures/integrity hashes do not constitute
 * ledger-valid transactions, and none are submitted to the emulator. */
it("enforces reserve sibling/accounting and exact payout economics on a real settled withdrawal", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  const { fixture, lucidService, globals, production } = h;
  const context = { fixture, lucidService, globals, production };
  const lucid = fixture.operatorLucid;
  const owner = fixture.depositorLucid;
  const pair = SDK.requireEventHistoryContracts(fixture.contracts);
  const diagnostic: Record<string, unknown> = { stage: "deposit" };
  const failures: Record<string, unknown>[] = [];
  const rawDestinationDatum = "a3020a010b020c";
  const align = async () => {
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
  };
  const reject = (
    replay: Awaited<ReturnType<typeof evaluationContext>>,
    label: string,
    mutatedCbor: string,
    targetedSpendIndex: bigint,
  ) => {
    const originalBody = CML.Transaction.from_cbor_hex(
      replay.evidence.signedCbor,
    ).body();
    const mutatedBody = CML.Transaction.from_cbor_hex(mutatedCbor).body();
    const inputBytes = (inputs: CML.TransactionInputList | undefined) =>
      inputs === undefined
        ? undefined
        : Array.from({ length: inputs.len() }, (_, index) =>
            inputs.get(index).to_cbor_hex(),
          );
    expect(inputBytes(mutatedBody.inputs())).toEqual(
      inputBytes(originalBody.inputs()),
    );
    expect(inputBytes(mutatedBody.reference_inputs())).toEqual(
      inputBytes(originalBody.reference_inputs()),
    );
    expect(mutatedBody.fee()).toBe(originalBody.fee());
    expect(outputValue(mutatedCbor)).toEqual(
      outputValue(replay.evidence.signedCbor),
    );
    let failure: unknown;
    try {
      replay.evaluate(mutatedCbor);
    } catch (error) {
      failure = error;
    }
    expect(
      failure,
      `${label} must fail actual phase-two evaluation`,
    ).toBeDefined();
    const message = String(failure);
    failures.push({ label, mutatedCbor, targetedSpendIndex, error: message });
    expect(message).toMatch(
      /(?:Validator returned false|EvaluationFailure|failed script execution)/i,
    );
    // The error must identify this real spending purpose, not a decode failure.
    expect(message).toMatch(
      new RegExp(
        `Spend[\\[(: ]+${targetedSpendIndex.toString()}[\\]): ,]`,
        "i",
      ),
    );
  };
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(owner);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await align();
    const ownerAddress = await owner.wallet().address();
    const depositTx = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(owner, fixture.contracts, {
        l2Address: ownerAddress,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const depositSigned = await depositTx.tx.sign.withWallet().complete();
    expect(await owner.awaitTx(await depositSigned.submit())).toBe(true);
    owner.overrideUTxOs(await owner.utxosAt(ownerAddress));
    await h.synchronize();
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      ),
    );
    expect(deposits).toHaveLength(1);
    const deposit = deposits[0]!;
    expect(deposit.originalAssets).toEqual({ lovelace: 12_000_000n });
    await h.deployment.chain.awaitLedgerTime(
      Number(deposit.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const depositBlock = await commitConfirmRecoverAndMerge(context);
    expect(depositBlock.queuedHeader.depositsRoot).toBe(
      await expectedAuthenticatedEventRoot(SDK.ROOT_DOMAINS.deposits, [
        { key: deposit.idCbor, value: deposit.infoCbor },
      ]),
    );
    await ensureSeparateCollateralUtxo(lucid);
    await h.command(
      absorbConfirmedDepositToReserveProgram({
        eventId: deposit.idCbor.toString("hex"),
      }),
    );
    const reserves = await lucid.utxosAt(
      fixture.contracts.reserve.spendingScriptAddress,
    );
    expect(reserves).toHaveLength(1);
    const reserve = reserves[0]!;
    expect(reserve.assets).toEqual(deposit.originalAssets);
    const l2 = await h.command(utxosProgram(ownerAddress));
    expect(l2.utxoCount).toBe(1);
    const target = l2.utxos[0]!;
    const ownerAddressData = await Effect.runPromise(
      SDK.addressDataFromBech32(ownerAddress),
    );
    const body: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: target.txHash,
        outputIndex: BigInt(target.outputIndex),
      },
      l2_owner: paymentCredentialOf(ownerAddress).hash,
      l2_value: assetsToValue(deposit.originalAssets),
      l1_address: ownerAddressData,
      l1_datum: { InlineDatum: { data: 0n } },
    };
    const bodyCbor = ordered(
      replacePlutusConstrFieldCbor(
        Data.to(body, SDK.WithdrawalBody),
        [4, 0],
        rawDestinationDatum,
      ),
    );
    const key = CML.PrivateKey.from_bech32(
      walletFromSeed(fixture.depositorAccount.seedPhrase, {
        network: "Preprod",
      }).paymentKey,
    );
    diagnostic.stage = "withdrawal";
    await ensureSeparateCollateralUtxo(owner);
    await advanceHistoryAdmissionClock(fixture, "withdrawal");
    await align();
    const withdrawalTx = await Effect.runPromise(
      SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
        owner,
        fixture.contracts,
        {
          bodyCbor,
          signature: SDK.signWithdrawalBodyCbor(key, bodyCbor),
          refundAddress: ownerAddressData,
          refundDatum: "NoDatum",
          referenceScripts: fixture.referenceScripts.withdrawal,
        },
      ),
    );
    const withdrawalSigned = await withdrawalTx.tx.sign.withWallet().complete();
    expect(await owner.awaitTx(await withdrawalSigned.submit())).toBe(true);
    owner.overrideUTxOs(await owner.utxosAt(ownerAddress));
    await h.synchronize();
    const withdrawals = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
      ),
    );
    expect(withdrawals).toHaveLength(1);
    const withdrawal = withdrawals[0]!;
    await h.deployment.chain.awaitLedgerTime(
      Number(withdrawal.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const withdrawalBlock = await commitConfirmRecoverAndMerge(context);
    const resolution = await h.command(
      resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId: withdrawal.idCbor,
      }),
    );
    expect(resolution.kind).toBe("withdrawal");
    if (resolution.kind !== "withdrawal")
      throw new Error("Expected withdrawal settlement");
    expect(resolution.validity).toBe("WithdrawalIsValid");
    expect(resolution.settlementRefInput).toEqual(
      withdrawalBlock.settlementUtxo,
    );
    expect(resolution.root).toBe(
      await expectedAuthenticatedEventRoot(SDK.ROOT_DOMAINS.withdrawals, [
        { key: withdrawal.idCbor, value: withdrawal.infoCbor },
      ]),
    );
    expect((await h.command(utxosProgram(ownerAddress))).utxoCount).toBe(0);
    const eventId = withdrawal.idCbor.toString("hex");
    await h.command(initializePayoutProgram({ eventId }));
    const payoutUnit = fixture.contracts.payout.policyId + withdrawal.assetName;
    const payoutInputs = await lucid.utxosAtWithUnit(
      fixture.contracts.payout.spendingScriptAddress,
      payoutUnit,
    );
    expect(payoutInputs).toHaveLength(1);
    const payout = payoutInputs[0]!;
    const initialized = await h.command(payoutStatusProgram(eventId));
    // Retirement carries the withdrawal Order's original funds into the
    // accumulator; structural list funds are refunded separately.
    expect(initialized.phase).toBe("partially_funded");
    expect(initialized.currentAssets).toEqual(withdrawal.originalAssets);
    expect(initialized.targetAssets).toEqual({ lovelace: 12_000_000n });
    expect(initialized.remainingAssets).toEqual({
      lovelace: 12_000_000n - withdrawal.originalAssets.lovelace!,
    });
    expect(payout.assets).toEqual({
      ...withdrawal.originalAssets,
      [payoutUnit]: 1n,
    });
    diagnostic.authority = {
      deposit,
      depositBlock,
      reserve,
      withdrawal,
      withdrawalBlock,
      resolution,
      payout,
    };

    diagnostic.stage = "reserve-funding";
    await ensureSeparateCollateralUtxo(lucid);
    await refreshWalletUtxosFromProvider(lucid);
    const references = {
      referenceScriptsAddress: fixture.referenceScriptsAccount.address,
    };
    const funding = await Effect.runPromise(
      SDK.buildAddReserveFundsToPayoutTxProgram(lucid, fixture.contracts, {
        ...references,
        payoutInput: payout,
        reserveInput: reserve,
      }),
    );
    const fundingSigned = await funding.tx.sign.withWallet().complete();
    const fundingCbor = fundingSigned.toCBOR();
    const fundingReplay = await evaluationContext(lucid, fundingCbor);
    diagnostic.fundingEvaluation = fundingReplay.evidence;
    const addFundsCbor = findRedeemerDataCbor(
      CML.Transaction.from_cbor_hex(fundingCbor),
      { tag: CML.RedeemerTag.Spend, index: funding.layout.payoutInputIndex },
    );
    if (addFundsCbor === undefined) throw new Error("Missing funding redeemer");
    expect(
      fundingReplay.evaluate(
        withRedeemer(
          fundingCbor,
          funding.layout.payoutInputIndex,
          addFundsCbor,
        ),
      ),
    ).toEqual(fundingReplay.evidence.positive);
    const addFunds = Data.from(addFundsCbor, SDK.PayoutSpendRedeemer);
    if (!("AddFunds" in addFunds)) throw new Error("Expected AddFunds");
    expect(addFunds.AddFunds.reserve_spend_redeemer_index).toBe(
      funding.layout.reserveSpendRedeemerIndex,
    );
    expect(funding.layout.reserveSpendRedeemerIndex).not.toBe(
      funding.layout.payoutSpendRedeemerIndex,
    );
    const sibling = withRedeemer(
      fundingCbor,
      funding.layout.payoutInputIndex,
      Data.to(
        {
          AddFunds: {
            ...addFunds.AddFunds,
            reserve_spend_redeemer_index:
              funding.layout.payoutSpendRedeemerIndex,
          },
        },
        SDK.PayoutSpendRedeemer,
      ),
    );
    reject(
      fundingReplay,
      "wrong-reserve-sibling-redeemer",
      sibling,
      funding.layout.payoutInputIndex < funding.layout.reserveInputIndex
        ? funding.layout.payoutInputIndex
        : funding.layout.reserveInputIndex,
    );
    if (funding.layout.reserveChangeOutputIndex === null)
      throw new Error("Expected exact reserve change");
    const fundingBody = CML.Transaction.from_cbor_hex(fundingCbor).body();
    const reserveIndex = Number(funding.layout.reserveChangeOutputIndex);
    const operatorAddress = await lucid.wallet().address();
    const changeIndex = Array.from(
      { length: fundingBody.outputs().len() },
      (_, i) => i,
    ).find(
      (i) =>
        coreToTxOutput(fundingBody.outputs().get(i)).address ===
        operatorAddress,
    );
    if (changeIndex === undefined)
      throw new Error("Missing ordinary fee change");
    const moveLovelace = (
      cbor: string,
      sourceIndex: number,
      destinationIndex: number,
      quantity: bigint,
    ) => {
      const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
      const source = outputs.get(sourceIndex);
      const destination = outputs.get(destinationIndex);
      const sourceValue = source.amount();
      const destinationValue = destination.amount();
      expect(sourceValue.coin()).toBeGreaterThan(quantity);
      source.set_amount(
        CML.Value.new(sourceValue.coin() - quantity, sourceValue.multi_asset()),
      );
      destination.set_amount(
        CML.Value.new(
          destinationValue.coin() + quantity,
          destinationValue.multi_asset(),
        ),
      );
      const mutated = withOutputs(
        cbor,
        new Map([
          [sourceIndex, source],
          [destinationIndex, destination],
        ]),
      );
      expect(outputValue(mutated)).toEqual(outputValue(cbor));
      return mutated;
    };
    // Increase reserve change by 1m and reduce ordinary fee change by exactly
    // 1m. Global Value balances; the reserve/payout sub-transition does not.
    const unbalanced = moveLovelace(
      fundingCbor,
      changeIndex,
      reserveIndex,
      1_000_000n,
    );
    reject(
      fundingReplay,
      "unbalanced-reserve-payout-accounting",
      unbalanced,
      funding.layout.payoutInputIndex,
    );
    expect(await lucid.utxosByOutRef([payout, reserve])).toHaveLength(2);
    const fundingHash = await fundingSigned.submit();
    expect(await lucid.awaitTx(fundingHash)).toBe(true);
    await h.synchronize();
    const fundedOutputs = await lucid.utxosByOutRef([
      {
        txHash: fundingHash,
        outputIndex: Number(funding.layout.payoutOutputIndex),
      },
      { txHash: fundingHash, outputIndex: reserveIndex },
    ]);
    expect(fundedOutputs).toHaveLength(2);
    const funded = fundedOutputs.find(
      (utxo) => utxo.assets[payoutUnit] === 1n,
    )!;
    const reserveChange = fundedOutputs.find(
      (utxo) => utxo.outputIndex === reserveIndex,
    )!;
    expect(funded.assets).toEqual({
      ...deposit.originalAssets,
      [payoutUnit]: 1n,
    });
    expect(funded.datum).toBe(payout.datum);
    expect(SDK.addAssets(funded.assets, reserveChange.assets)).toEqual(
      SDK.addAssets(payout.assets, reserve.assets),
    );
    expect((await h.command(payoutStatusProgram(eventId))).phase).toBe(
      "funded",
    );
    diagnostic.funded = {
      fundingHash,
      layout: funding.layout,
      funded,
      reserveChange,
    };

    diagnostic.stage = "terminal-payout";
    await refreshWalletUtxosFromProvider(lucid);
    const conclusion = await Effect.runPromise(
      SDK.buildConcludePayoutTxProgram(lucid, fixture.contracts, {
        ...references,
        payoutInput: funded,
      }),
    );
    const conclusionSigned = await conclusion.tx.sign.withWallet().complete();
    const conclusionCbor = conclusionSigned.toCBOR();
    const conclusionReplay = await evaluationContext(lucid, conclusionCbor);
    diagnostic.conclusionEvaluation = conclusionReplay.evidence;
    const conclusionBody = CML.Transaction.from_cbor_hex(conclusionCbor).body();
    const paymentIndex = Number(conclusion.layout.l1OutputIndex);
    const payment = conclusionBody.outputs().get(paymentIndex);
    const paid = coreToTxOutput(payment);
    expect(paid.address).toBe(ownerAddress);
    expect(paid.assets).toEqual(deposit.originalAssets);
    expect(ordered(paid.datum!)).toBe(ordered(rawDestinationDatum));
    const burn = conclusionBody
      .mint()!
      .get_assets(CML.ScriptHash.from_hex(fixture.contracts.payout.policyId))!;
    expect(burn.get(CML.AssetName.from_hex(withdrawal.assetName))).toBe(-1n);
    const terminalChangeIndex = Array.from(
      { length: conclusionBody.outputs().len() },
      (_, i) => i,
    ).find(
      (i) =>
        i !== paymentIndex &&
        coreToTxOutput(conclusionBody.outputs().get(i)).address ===
          operatorAddress,
    );
    if (terminalChangeIndex === undefined)
      throw new Error("Missing terminal fee change");
    reject(
      conclusionReplay,
      "terminal-underpayment",
      moveLovelace(
        conclusionCbor,
        paymentIndex,
        terminalChangeIndex,
        1_000_000n,
      ),
      conclusion.layout.payoutInputIndex,
    );
    const wrongAddress = CML.TransactionOutput.from_cbor_hex(
      payment.to_cbor_hex(),
    );
    wrongAddress.set_address(CML.Address.from_bech32(operatorAddress));
    reject(
      conclusionReplay,
      "terminal-wrong-address",
      withOutputs(conclusionCbor, new Map([[paymentIndex, wrongAddress]])),
      conclusion.layout.payoutInputIndex,
    );
    const wrongDatum = CML.TransactionOutput.new(
      payment.address(),
      payment.amount(),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("a3020a010b020d")),
    );
    reject(
      conclusionReplay,
      "terminal-wrong-raw-datum",
      withOutputs(conclusionCbor, new Map([[paymentIndex, wrongDatum]])),
      conclusion.layout.payoutInputIndex,
    );
    expect(await lucid.utxosByOutRef([funded])).toHaveLength(1);
    const conclusionHash = await conclusionSigned.submit();
    expect(await lucid.awaitTx(conclusionHash)).toBe(true);
    await h.synchronize();
    const terminalOutputs = await lucid.utxosByOutRef([
      { txHash: conclusionHash, outputIndex: paymentIndex },
    ]);
    expect(terminalOutputs).toHaveLength(1);
    expect(terminalOutputs[0]!.address).toBe(ownerAddress);
    expect(terminalOutputs[0]!.assets).toEqual(deposit.originalAssets);
    expect(ordered(terminalOutputs[0]!.datum!)).toBe(
      ordered(rawDestinationDatum),
    );
    expect(await lucid.utxosByOutRef([funded])).toHaveLength(0);
    expect(
      await lucid.utxosAtWithUnit(
        fixture.contracts.payout.spendingScriptAddress,
        payoutUnit,
      ),
    ).toHaveLength(0);
    expect((await h.command(payoutStatusProgram(eventId))).phase).toBe(
      "concluded",
    );
    for (const [hash, signedCbor] of [
      [fundingHash, fundingCbor],
      [conclusionHash, conclusionCbor],
    ]) {
      const receipts = h.receipts.filter(
        ({ transaction }) => transaction.txHash === hash,
      );
      expect(receipts).toHaveLength(1);
      expect(receipts[0]!.signedCbor).toBe(signedCbor);
    }
    expect(failures).toHaveLength(5);
    diagnostic.terminal = {
      conclusionHash,
      layout: conclusion.layout,
      output: terminalOutputs[0],
    };
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    throw error;
  } finally {
    try {
      const path = process.env.MIDGARD_RESERVE_PAYOUT_ECONOMICS_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Real owner-backed Deposit/Withdrawal settlement and honest signed reserve funding/terminal payout. Mutated contexts are phase-two evaluator probes, not submitted or ledger-valid transactions. Synthetic observation ancestry; no global watcher authority.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              failures,
              receipts: h.receipts,
              production: await h.evidence(),
            },
            (_key, value) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ) + "\n",
        );
      }
    } finally {
      try {
        await h.close();
      } finally {
        vi.useRealTimers();
      }
    }
  }
});
