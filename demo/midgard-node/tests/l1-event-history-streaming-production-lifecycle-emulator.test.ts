import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as ordered,
  plutusConstrFieldCbor as field,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { coreToTxOutput, datumToHash, type UTxO } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import * as DepositsDB from "../src/database/deposits.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  assetsToValue,
  CML,
  commitConfirmRecoverAndMerge,
  concludePayoutProgram,
  Data,
  Database,
  Effect,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  initializePayoutProgram,
  paymentCredentialOf,
  payoutStatusProgram,
  refreshWalletUtxosFromProvider,
  resolveEventSettlementProofProgram,
  runCommitWorker,
  SDK,
  utxosProgram,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

/** Successful node classification and actual mature merge establish both
 * settlement frontiers. The observation transport labels remain synthetic. */
it("streams public raw events through production reconciliation, native commitment, refund, payout and reclaim", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  const { fixture, lucidService, globals } = h;
  const context = { fixture, lucidService, globals, production: h.production };
  const lucid = fixture.operatorLucid;
  const pair = SDK.requireEventHistoryContracts(fixture.contracts);
  const owner = fixture.depositorLucid;
  const diagnostic: Record<string, unknown> = { stage: "setup" };
  const rawDepositDatum = "a302" + Data.to("ab".repeat(600)) + "010b020c";
  const rawDestinationDatum = "a302" + Data.to("cd".repeat(600)) + "010b020c";
  const rawRefundDatum = "a3020a010b020c";
  const publish = async (prepared: {
    context: SDK.EventHistoryBuildContext;
    plan: SDK.EventHistoryPayloadPlan;
    request: { nonce: UTxO; reclaimAuth: SDK.CredentialD };
  }) => {
    expect(prepared.plan.kind).toBe("External");
    const publication = await SDK.buildEventHistoryPublication(
      prepared.context,
      prepared.plan.payloadCbor,
      prepared.request.reclaimAuth,
    );
    const signed = await publication.tx.sign.withWallet().complete();
    const hash = await signed.submit();
    expect(await owner.awaitTx(hash)).toBe(true);
    owner.overrideUTxOs(await owner.utxosAt(await owner.wallet().address()));
    const outputs = await owner.utxosByOutRef([
      { txHash: hash, outputIndex: publication.publicationOutputIndex },
    ]);
    expect(outputs).toHaveLength(1);
    expect(await owner.utxosByOutRef([prepared.request.nonce])).toHaveLength(1);
    expect(field(outputs[0]!.datum!, [1])).toBe(prepared.plan.payloadCbor);
    await h.synchronize();
    vi.setSystemTime(fixture.emulator.now());
    return outputs[0]!;
  };
  const command = h.command;
  const commitmentWindows: unknown[] = [];
  diagnostic.commitmentWindows = commitmentWindows;
  const alignBeforeAdmission = async () => {
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
  };
  const assertCommitWindow = async (
    block: Awaited<ReturnType<typeof commitConfirmRecoverAndMerge>>,
    futureEvent?: SDK.DepositUTxO | SDK.WithdrawalUTxO,
  ) => {
    const attempts = h.commitAttempts.filter(
      ({ output }) =>
        output.type === "SubmittedAwaitingConfirmationOutput" &&
        output.submittedTxHash === block.commitOutput.submittedTxHash,
    );
    expect(attempts).toHaveLength(1);
    const attempt = attempts[0]!;
    const receipts = h.receipts.filter(
      ({ transaction }) =>
        transaction.txHash === block.commitOutput.submittedTxHash,
    );
    expect(receipts).toHaveLength(1);
    const body = CML.Transaction.from_cbor_hex(receipts[0]!.signedCbor).body();
    expect(CML.hash_transaction(body).to_hex()).toBe(
      block.commitOutput.submittedTxHash,
    );
    const unit =
      fixture.contracts.stateQueue.policyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      block.queuedHeaderHash;
    const outputs = Array.from({ length: body.outputs().len() }, (_, i) =>
      coreToTxOutput(body.outputs().get(i)),
    ).filter((output) => output.assets[unit] === 1n);
    expect(outputs).toHaveLength(1);
    const signedHeader = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(
        SDK.linkedListDatumToNodeView(
          Data.from(outputs[0]!.datum!, SDK.LinkedListDatum),
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.queuedHeaderHash,
        ),
      ),
    );
    expect(signedHeader).toEqual(block.queuedHeader);
    expect(Number(signedHeader.endTime)).toBe(
      block.commitOutput.blockEndTimeMs,
    );
    const horizon =
      attempt.coverage.includedThroughMs + SDK.EVENT_WAIT_DURATION_MS - 1;
    expect(Number(signedHeader.endTime)).toBeLessThanOrEqual(horizon);
    expect(lucid.slotToUnixTime(Number(body.ttl())) - 1).toBe(
      Number(signedHeader.endTime),
    );
    if (futureEvent !== undefined) {
      expect(Number(futureEvent.facts.inclusion_time)).toBeGreaterThan(
        attempt.coverage.includedThroughMs,
      );
      expect(Number(futureEvent.facts.inclusion_time)).toBeLessThanOrEqual(
        Number(signedHeader.endTime),
      );
    }
    commitmentWindows.push({
      attempt,
      signedHeader,
      signedTxHash: block.commitOutput.submittedTxHash,
      horizon,
      futureEventId: futureEvent?.idCbor.toString("hex"),
      inclusionTime: futureEvent?.facts.inclusion_time,
    });
  };
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    vi.setSystemTime(fixture.emulator.now());
    const ownerAddress = await fixture.depositorLucid.wallet().address();
    await ensureSeparateCollateralUtxo(owner);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignBeforeAdmission();
    const depositConfig: SDK.SubmitDepositConfig = {
      l2Address: ownerAddress,
      l2Datum: rawDepositDatum,
      lovelace: 12_000_000n,
      additionalAssets: {},
      referenceScripts: fixture.referenceScripts.deposit,
    };
    diagnostic.stage = "deposit-publication";
    const preparedDeposit = await Effect.runPromise(
      SDK.prepareDepositSubmissionProgram(
        owner,
        fixture.contracts,
        depositConfig,
      ),
    );
    const depositRetained = await publish(preparedDeposit);
    diagnostic.stage = "deposit-admission";
    const builtDeposit = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(owner, fixture.contracts, {
        ...depositConfig,
        nonceInput: preparedDeposit.request.nonce,
        externalData: depositRetained,
      }),
    );
    const signedDeposit = await builtDeposit.tx.sign.withWallet().complete();
    const depositHash = await signedDeposit.submit();
    expect(await owner.awaitTx(depositHash)).toBe(true);
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
    expect(deposit.history.retainedDataUtxo).toEqual(depositRetained);
    expect(field(deposit.infoCbor.toString("hex"), [2, 0])).toBe(
      ordered(rawDepositDatum),
    );
    const depositCapture = SDK.captureEventHistoryWitness(
      deposit.history,
      pair.deposit.list.policyId,
      "Deposit",
    );
    expect(depositCapture.payloadCbor).toBe(preparedDeposit.plan.payloadCbor);
    expect(depositCapture.commitment.payload_hash).toBe(
      datumToHash(preparedDeposit.plan.payloadCbor),
    );
    expect(field(depositCapture.openingCbor, [0])).toBe(
      preparedDeposit.plan.payloadCbor,
    );
    diagnostic.deposit = {
      metadata: builtDeposit.metadata,
      retained: depositRetained,
      infoCbor: deposit.infoCbor.toString("hex"),
      capture: depositCapture,
    };
    diagnostic.stage = "deposit-settlement";
    await h.deployment.chain.awaitLedgerTime(
      Number(deposit.facts.inclusion_time) - 20_000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const depositRows = await Effect.runPromise(
      DepositsDB.retrieveAllEntries().pipe(Effect.provide(Database.layer)),
    );
    expect(
      depositRows.find((row) =>
        row[DepositsDB.Columns.ID].equals(deposit.idCbor),
      )?.status,
    ).toBe(DepositsDB.Status.Awaiting);
    const depositBlock = await commitConfirmRecoverAndMerge(context);
    await assertCommitWindow(depositBlock, deposit);
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
    expect(ordered(target.datum!)).toBe(ordered(rawDepositDatum));
    diagnostic.l2Output = target;
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
      l1_datum: { InlineDatum: { data: 0n } },
    };
    const bodyCbor = ordered(
      replacePlutusConstrFieldCbor(
        Data.to(body, SDK.WithdrawalBody),
        [4, 0],
        rawDestinationDatum,
      ),
    );
    const refundDatumCbor = replacePlutusConstrFieldCbor(
      Data.to({ InlineDatum: { data: 0n } }, SDK.CardanoDatum),
      [0],
      rawRefundDatum,
    );
    // A well-formed signature for a different destination does not authorize this body.
    // The real L2 outref, owner and full Value remain correct.
    const badSignature = SDK.signWithdrawalBodyCbor(
      ownerKey,
      replacePlutusConstrFieldCbor(bodyCbor, [4, 0], Data.to(123n)),
    );
    expect(
      SDK.verifyWithdrawalSignatureCbor(bodyCbor, badSignature, body.l2_owner),
    ).toMatchObject({ valid: false, reason: "invalid_signature" });
    await ensureSeparateCollateralUtxo(owner);
    await advanceHistoryAdmissionClock(fixture, "withdrawal");
    await alignBeforeAdmission();
    const invalidConfig: SDK.SubmitWithdrawalConfig = {
      bodyCbor,
      signature: badSignature,
      refundAddress: ownerAddressData,
      refundDatumCbor,
      referenceScripts: fixture.referenceScripts.withdrawal,
    };
    diagnostic.stage = "invalid-withdrawal-publication";
    const preparedInvalid = await Effect.runPromise(
      SDK.prepareWithdrawalSubmissionProgram(
        owner,
        fixture.contracts,
        invalidConfig,
      ),
    );
    const invalidRetained = await publish(preparedInvalid);
    const builtInvalid = await Effect.runPromise(
      SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
        owner,
        fixture.contracts,
        {
          ...invalidConfig,
          nonceInput: preparedInvalid.request.nonce,
          externalData: invalidRetained,
        },
      ),
    );
    const signedInvalid = await builtInvalid.tx.sign.withWallet().complete();
    const invalidHash = await signedInvalid.submit();
    expect(await owner.awaitTx(invalidHash)).toBe(true);
    owner.overrideUTxOs(await owner.utxosAt(ownerAddress));
    await h.synchronize();
    const invalidOrders = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
      ),
    );
    expect(invalidOrders).toHaveLength(1);
    const invalidWithdrawal = invalidOrders[0]!;
    expect(invalidWithdrawal.idCbor.toString("hex")).toBe(
      builtInvalid.metadata.withdrawalEventIdCbor,
    );
    expect(invalidWithdrawal.history.retainedDataUtxo).toEqual(invalidRetained);
    expect(field(invalidWithdrawal.infoCbor.toString("hex"), [0])).toBe(
      bodyCbor,
    );
    expect(invalidWithdrawal.event.info.validity).toBe("WithdrawalIsValid");
    const hub = await Effect.runPromise(
      SDK.fetchHubOracleUTxOProgram(owner, {
        hubOracleAddress: fixture.contracts.hubOracle.spendingScriptAddress,
        hubOraclePolicyId: fixture.contracts.hubOracle.policyId,
      }),
    );
    const beforeRefusal = {
      digest: h.capture().snapshotDigest,
      receipts: h.receipts.length,
    };
    const prematureReclaim = await Effect.runPromise(
      SDK.buildReclaimEventHistoryDataTxProgram(owner, fixture.contracts, {
        kind: "Withdrawal",
        retainedInput: invalidRetained,
        hubOracleRefInput: hub.utxo,
      }).pipe(Effect.either),
    );
    expect(prematureReclaim._tag).toBe("Left");
    if (prematureReclaim._tag !== "Left")
      throw new Error(
        "Present Order reclamation must refuse before construction",
      );
    expect(inspect(prematureReclaim.left, { depth: 10 })).toContain(
      "Event is still present; retained data cannot be reclaimed",
    );
    expect(h.capture().snapshotDigest).toBe(beforeRefusal.digest);
    expect(h.receipts).toHaveLength(beforeRefusal.receipts);
    expect(await owner.utxosByOutRef([invalidRetained])).toEqual([
      invalidRetained,
    ]);
    diagnostic.stage = "invalid-withdrawal-settlement";
    await h.deployment.chain.awaitLedgerTime(
      Number(invalidWithdrawal.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    // Advance the actual ledger beyond this sealed source's fixed horizon.
    // Leave the transport tip untouched until the refused attempt has returned.
    const beforeState = await h.evidence();
    const beforeStale = {
      points: structuredClone(beforeState.points),
      journal: beforeState.journal,
      native: beforeState.native,
    };
    const nativeRootBefore = beforeStale.native?.durableRoot;
    const receiptsBefore = h.receipts.length;
    const latestBefore = await fetchLatestCommittedBlock(
      lucid,
      fixture.contracts,
    );
    fixture.emulator.awaitSlot(61);
    vi.setSystemTime(fixture.emulator.now());
    let staleFailure: unknown;
    let staleOutput: unknown;
    try {
      staleOutput = await runCommitWorker(
        fixture.contracts,
        lucidService,
        latestBefore,
        h.production.nodeConfig,
        fixture.runtimeOverrides!.deploymentIdentity,
        { ...h.production, globals },
      );
    } catch (error) {
      staleFailure = error;
    }
    diagnostic.staleSourceAttempt = {
      output: staleOutput,
      failure: inspect(staleFailure, { depth: 20 }),
      before: beforeStale,
      observedSlotAfter: fixture.emulator.slot,
    };
    expect(staleOutput).toBeUndefined();
    expect(inspect(staleFailure, { depth: 20 })).toContain(
      "Resolved commit transaction validity exceeds the selected scheduler window cap",
    );
    expect(h.receipts).toHaveLength(receiptsBefore);
    expect(Object.keys(fixture.emulator.mempool)).toHaveLength(0);
    expect(await fetchLatestCommittedBlock(lucid, fixture.contracts)).toEqual(
      latestBefore,
    );
    const afterStale = await h.evidence();
    expect(afterStale.points).toEqual(beforeStale.points);
    expect(afterStale.journal).toEqual(beforeStale.journal);
    expect(afterStale.native?.durableRoot).toBe(nativeRootBefore);
    await h.synchronize();
    const invalidBlock = await commitConfirmRecoverAndMerge(context);
    await assertCommitWindow(invalidBlock);
    const invalidResolution = await command(
      resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId: invalidWithdrawal.idCbor,
      }),
    );
    if (invalidResolution.kind !== "withdrawal")
      throw new Error("Expected invalid Withdrawal settlement");
    expect(invalidResolution.validity).toBe("IncorrectWithdrawalSignature");
    if (invalidResolution.validity !== "IncorrectWithdrawalSignature")
      throw new Error("Expected exact invalid signature verdict");
    expect(invalidResolution.settlementRefInput).toEqual(
      invalidBlock.settlementUtxo,
    );
    const settledInfo =
      invalidResolution.entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
    expect(settledInfo).not.toBeNull();
    const expectedSettledInfo = ordered(
      replacePlutusConstrFieldCbor(
        invalidWithdrawal.infoCbor.toString("hex"),
        [2],
        Data.to("IncorrectWithdrawalSignature", SDK.WithdrawalValidity),
      ),
    );
    expect(settledInfo!.toString("hex")).toBe(expectedSettledInfo);
    expect(field(expectedSettledInfo, [0])).toBe(bodyCbor);
    expect(field(expectedSettledInfo, [1])).toBe(
      field(invalidWithdrawal.infoCbor.toString("hex"), [1]),
    );
    const invalidRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      [{ key: invalidWithdrawal.idCbor, value: settledInfo! }],
    );
    expect(invalidResolution.root).toBe(invalidRoot);
    expect(invalidBlock.queuedHeader.withdrawalsRoot).toBe(invalidRoot);
    expect(
      await Effect.runPromise(
        utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
      ),
    ).toEqual(l2State);
    const reservesBeforeRefund = await lucid.utxosAt(
      fixture.contracts.reserve.spendingScriptAddress,
    );
    const withdrawalDeployment = SDK.eventHistoryDeploymentFromContracts(
      pair.withdrawal,
    );
    const refundNodes = SDK.authenticateHistoryNodes(
      await lucid.utxosAt(withdrawalDeployment.address),
      withdrawalDeployment,
    );
    const refundOrder = refundNodes.find(
      ({ key }) => key === invalidWithdrawal.assetName,
    )!;
    const refundPredecessor = refundNodes.find(
      ({ node }) => node.next === invalidWithdrawal.assetName,
    )!;
    const refundProtection =
      refundOrder.node.protected_until > refundPredecessor.node.protected_until
        ? refundOrder.node.protected_until
        : refundPredecessor.node.protected_until;
    expect(BigInt(fixture.emulator.now())).toBeGreaterThanOrEqual(
      refundProtection,
    );
    await ensureSeparateCollateralUtxo(lucid);
    diagnostic.stage = "invalid-withdrawal-refund";
    const refundWalletBefore = await lucid.wallet().getUtxos();
    const refunded = await Effect.runPromise(
      SDK.buildRefundInvalidWithdrawalTxProgram(lucid, fixture.contracts, {
        withdrawal: invalidWithdrawal,
        settlementRefInput: invalidResolution.settlementRefInput,
        membershipProof: invalidResolution.proof,
        validityOverride: invalidResolution.validity,
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
    const refundLower = CML.Transaction.from_cbor_hex(refundSigned.toCBOR())
      .body()
      .validity_interval_start();
    expect(refundLower).toBeDefined();
    expect(
      BigInt(lucid.slotToUnixTime(Number(refundLower))),
    ).toBeGreaterThanOrEqual(refundProtection);
    const refundHash = await refundSigned.submit();
    expect(await lucid.awaitTx(refundHash)).toBe(true);
    await h.synchronize();
    const refundReceipt = h.receipts.find(
      ({ transaction }) => transaction.txHash === refundHash,
    )!;
    const label = (input: { txHash: string; outputIndex: number }) =>
      `${input.txHash}#${input.outputIndex}`;
    const refundInputs = new Set(refundReceipt.transaction.inputs.map(label));
    const consumedWalletInputs = refundWalletBefore.filter((input) =>
      refundInputs.has(label(input)),
    );
    expect(consumedWalletInputs.length).toBeGreaterThan(0);
    expect(await lucid.utxosByOutRef(consumedWalletInputs)).toHaveLength(0);
    const staleWalletInputs = (await lucid.wallet().getUtxos()).filter(
      (input) =>
        consumedWalletInputs.some((spent) => label(input) === label(spent)),
    );
    await refreshWalletUtxosFromProvider(lucid);
    const refreshedWallet = await lucid.wallet().getUtxos();
    expect(
      refreshedWallet.some((input) =>
        consumedWalletInputs.some((spent) => label(input) === label(spent)),
      ),
    ).toBe(false);
    diagnostic.refundWalletRefresh = {
      refundHash,
      consumedWalletInputs,
      staleWalletInputs,
      refreshedOutRefs: refreshedWallet.map(label),
    };
    const refundOutputs = await lucid.utxosByOutRef([
      {
        txHash: refundHash,
        outputIndex: Number(refunded.layout.refundOutputIndex),
      },
    ]);
    expect(refundOutputs).toHaveLength(1);
    expect(refundOutputs[0]!.address).toBe(ownerAddress);
    expect(refundOutputs[0]!.assets).toEqual(invalidWithdrawal.originalAssets);
    expect(ordered(refundOutputs[0]!.datum!)).toBe(ordered(rawRefundDatum));
    expect(h.capture().history.withdrawals).toHaveLength(0);
    expect(
      await lucid.utxosAt(fixture.contracts.payout.spendingScriptAddress),
    ).toHaveLength(0);
    expect(
      await lucid.utxosAt(fixture.contracts.reserve.spendingScriptAddress),
    ).toEqual(reservesBeforeRefund);
    expect(
      await Effect.runPromise(
        utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
      ),
    ).toEqual(l2State);
    diagnostic.invalidWithdrawal = {
      metadata: builtInvalid.metadata,
      retained: invalidRetained,
      admittedInfoCbor: invalidWithdrawal.infoCbor.toString("hex"),
      settledInfoCbor: expectedSettledInfo,
      validity: invalidResolution.validity,
      settlementRoot: invalidRoot,
      settlement: invalidResolution.settlementRefInput,
      refundHash,
      refundOutput: refundOutputs[0],
      refundLayout: refunded.layout,
      predecessorProtection: refundPredecessor.node.protected_until,
      orderProtection: refundOrder.node.protected_until,
      signedLowerSlot: refundLower,
      prematureReclaim: inspect(prematureReclaim.left, { depth: 10 }),
    };
    // The invalid request leaves this same L2 input available for the honest request.
    const signature = SDK.signWithdrawalBodyCbor(ownerKey, bodyCbor);
    expect(
      SDK.verifyWithdrawalSignatureCbor(bodyCbor, signature, body.l2_owner)
        .valid,
    ).toBe(true);
    await ensureSeparateCollateralUtxo(owner);
    await advanceHistoryAdmissionClock(fixture, "withdrawal");
    await alignBeforeAdmission();
    const withdrawalConfig: SDK.SubmitWithdrawalConfig = {
      bodyCbor,
      signature,
      refundAddress: ownerAddressData,
      refundDatumCbor,
      referenceScripts: fixture.referenceScripts.withdrawal,
    };
    diagnostic.stage = "withdrawal-publication";
    const preparedWithdrawal = await Effect.runPromise(
      SDK.prepareWithdrawalSubmissionProgram(
        owner,
        fixture.contracts,
        withdrawalConfig,
      ),
    );
    const withdrawalRetained = await publish(preparedWithdrawal);
    diagnostic.stage = "withdrawal-admission";
    const builtWithdrawal = await Effect.runPromise(
      SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
        owner,
        fixture.contracts,
        {
          ...withdrawalConfig,
          nonceInput: preparedWithdrawal.request.nonce,
          externalData: withdrawalRetained,
        },
      ),
    );
    const signedWithdrawal = await builtWithdrawal.tx.sign
      .withWallet()
      .complete();
    const withdrawalHash = await signedWithdrawal.submit();
    expect(await owner.awaitTx(withdrawalHash)).toBe(true);
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
    expect(builtWithdrawal.metadata.withdrawalEventIdCbor).toBe(
      withdrawal.idCbor.toString("hex"),
    );
    expect(withdrawal.history.retainedDataUtxo).toEqual(withdrawalRetained);
    expect(field(withdrawal.infoCbor.toString("hex"), [0])).toBe(bodyCbor);
    expect(field(withdrawal.history.payloadCbor, [2])).toBe(
      ordered(refundDatumCbor),
    );
    const withdrawalCapture = SDK.captureEventHistoryWitness(
      withdrawal.history,
      pair.withdrawal.list.policyId,
      "Withdrawal",
    );
    expect(withdrawalCapture.commitment.payload_hash).toBe(
      datumToHash(preparedWithdrawal.plan.payloadCbor),
    );
    expect(field(withdrawalCapture.openingCbor, [0])).toBe(
      preparedWithdrawal.plan.payloadCbor,
    );
    diagnostic.withdrawal = {
      metadata: builtWithdrawal.metadata,
      retained: withdrawalRetained,
      infoCbor: withdrawal.infoCbor.toString("hex"),
      bodyCbor,
      signature,
      capture: withdrawalCapture,
    };
    diagnostic.stage = "withdrawal-settlement";
    await h.deployment.chain.awaitLedgerTime(
      Number(withdrawal.facts.inclusion_time) - 20_000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const withdrawalRows = await Effect.runPromise(
      WithdrawalsDB.retrieveAllEntries().pipe(Effect.provide(Database.layer)),
    );
    expect(
      withdrawalRows.find((row) =>
        row[WithdrawalsDB.Columns.ID].equals(withdrawal.idCbor),
      )?.status,
    ).toBe(WithdrawalsDB.Status.Awaiting);
    const withdrawalBlock = await commitConfirmRecoverAndMerge(context);
    await assertCommitWindow(withdrawalBlock, withdrawal);
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
    const withdrawalRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      [{ key: withdrawal.idCbor, value: withdrawal.infoCbor }],
    );
    expect(resolution.root).toBe(withdrawalRoot);
    diagnostic.settlementRoots = {
      deposit: depositRoot,
      withdrawal: withdrawalRoot,
    };
    const emptyL2 = await Effect.runPromise(
      utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(emptyL2.utxoCount).toBe(0);
    const eventId = withdrawal.idCbor.toString("hex");
    diagnostic.stage = "initialize-payout";
    await command(initializePayoutProgram({ eventId }));
    const payoutUnit = fixture.contracts.payout.policyId + withdrawal.assetName;
    const payout = (
      await lucid.utxosAt(fixture.contracts.payout.spendingScriptAddress)
    ).find((output) => output.assets[payoutUnit] === 1n);
    expect(payout).toBeDefined();
    expect(field(payout!.datum!, [2, 0])).toBe(ordered(rawDestinationDatum));
    for (let index = 0; index < 3; index++)
      expect(ordered(field(payout!.datum!, [index]))).toBe(
        ordered(field(bodyCbor, [index + 2])),
      );
    diagnostic.initialPayout = payout;
    expect(h.capture().history.withdrawals).toHaveLength(0);
    await ensureSeparateCollateralUtxo(lucid);
    const added = await command(addReserveFundsToPayoutProgram({ eventId }));
    expect(added.details.reserveOutRef).toBe(
      `${reserve!.txHash}#${reserve!.outputIndex}`,
    );
    expect((await command(payoutStatusProgram(eventId))).phase).toBe("funded");
    const fundedPayout = (
      await lucid.utxosAt(fixture.contracts.payout.spendingScriptAddress)
    ).find((output) => output.assets[payoutUnit] === 1n);
    expect(fundedPayout?.datum).toBe(payout!.datum);
    diagnostic.fundedPayout = fundedPayout;
    diagnostic.stage = "conclude-payout";
    await command(concludePayoutProgram({ eventId }));
    expect((await command(payoutStatusProgram(eventId))).phase).toBe(
      "concluded",
    );
    const destination = (await lucid.utxosAt(ownerAddress)).find(
      (output) =>
        output.assets.lovelace === 12_000_000n &&
        output.datum != null &&
        ordered(output.datum) === ordered(rawDestinationDatum),
    );
    expect(destination).toBeDefined();
    expect(destination!.assets).toEqual(deposit.originalAssets);
    expect(
      await lucid.utxosByOutRef([
        depositRetained,
        invalidRetained,
        withdrawalRetained,
      ]),
    ).toHaveLength(3);
    diagnostic.destination = destination;
    await h.synchronize();
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
      { kind: "withdrawal", operation: "RetireOrder", reason: "refunded" },
      { kind: "withdrawal", operation: "InsertOrder", reason: undefined },
      {
        kind: "withdrawal",
        operation: "RetireOrder",
        reason: "payout_initialized",
      },
    ]);
    for (const [kind, original] of [
      ["deposit", deposit],
      ["withdrawal", invalidWithdrawal],
      ["withdrawal", withdrawal],
    ] as const) {
      const admitted = h.transitions.find(
        (transition) =>
          transition.kind === kind &&
          transition.admission?.idCbor === original.idCbor.toString("hex"),
      )!.admission!;
      const retired = h.transitions.find(
        (transition) =>
          transition.kind === kind &&
          transition.retirement?.event.idCbor ===
            original.idCbor.toString("hex"),
      )!.retirement!.event;
      expect(retired.idCbor).toBe(original.idCbor.toString("hex"));
      expect(retired.factsCbor).toBe(admitted.factsCbor);
      expect(retired.originalAssetsCbor).toBe(admitted.originalAssetsCbor);
    }
    const finalReserves = await lucid.utxosAt(
      fixture.contracts.reserve.spendingScriptAddress,
    );
    const finalL2 = await Effect.runPromise(
      utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
    );
    const transitionsBeforeReclaim = h.transitions.length;
    const reclamations: Record<string, unknown>[] = [];
    diagnostic.reclamations = reclamations;
    for (const [kind, retained] of [
      ["Deposit", depositRetained],
      ["Withdrawal", invalidRetained],
      ["Withdrawal", withdrawalRetained],
    ] as const) {
      diagnostic.stage = `reclaim-${kind}-${retained.txHash}`;
      owner.overrideUTxOs(await owner.utxosAt(ownerAddress));
      await ensureSeparateCollateralUtxo(owner);
      const reclaimed = await Effect.runPromise(
        SDK.buildReclaimEventHistoryDataTxProgram(owner, fixture.contracts, {
          kind,
          retainedInput: retained,
          hubOracleRefInput: hub.utxo,
        }),
      );
      const signed = await reclaimed.tx.sign.withWallet().complete();
      const hash = await signed.submit();
      expect(await owner.awaitTx(hash)).toBe(true);
      await h.synchronize();
      expect(await owner.utxosByOutRef([retained])).toHaveLength(0);
      expect(h.transitions).toHaveLength(transitionsBeforeReclaim);
      expect(
        await lucid.utxosAt(fixture.contracts.reserve.spendingScriptAddress),
      ).toEqual(finalReserves);
      expect(
        await Effect.runPromise(
          utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
        ),
      ).toEqual(finalL2);
      expect(
        h.receipts.some(({ transaction }) => transaction.txHash === hash),
      ).toBe(true);
      reclamations.push({ kind, retained, hash, layout: reclaimed.layout });
    }
    expect(h.capture().history.deposits).toHaveLength(0);
    expect(h.capture().history.withdrawals).toHaveLength(0);
    expect(h.observer.pendingCount()).toBe(0);
    expect(commitmentWindows).toHaveLength(3);
    expect(
      h.commitAttempts.filter(
        ({ output }) => output.type === "SubmittedAwaitingConfirmationOutput",
      ),
    ).toHaveLength(3);
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    throw error;
  } finally {
    try {
      const evidencePath =
        process.env.MIDGARD_STREAMING_PRODUCTION_LIFECYCLE_EVIDENCE_PATH;
      if (evidencePath !== undefined) {
        mkdirSync(dirname(evidencePath), { recursive: true });
        writeFileSync(
          evidencePath,
          JSON.stringify(
            {
              scope:
                "Actual public SDK external raw Deposit and invalid/valid Withdrawal admission; real node signature classification, commitment, attestation, mature merge, reserve absorption, exact raw refund/payout and owner-authorized reclamation of all3retained payloads; synthetic observation transport labels only. No fraud-proof or challenge transaction construction or invented reclaim time lock.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              rawDepositDatum,
              rawDestinationDatum,
              rawRefundDatum,
              diagnostic,
              production: await h.evidence(),
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
      try {
        await h.close();
      } finally {
        vi.useRealTimers();
      }
    }
  }
});
