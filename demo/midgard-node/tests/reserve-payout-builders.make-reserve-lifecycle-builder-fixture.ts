import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid as makeLucid,
  scriptFromNative,
  scriptHashToCredential,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { __reservePayoutTest } from "../src/transactions/reserve-payout.js";
import {
  makeSeededScriptAccount,
  registerZeroRewardScript,
} from "./reserve-payout-builders.make-reserve-payout-builder-fixture.js";
import {
  countedSingletonMembershipRoot,
  EMULATOR_PROTOCOL_PARAMETERS,
  findPureAdaUtxo,
  findReferenceScriptUtxo,
  findUtxoWithUnit,
  loadRealContracts,
} from "./reserve-payout-builders.submit-with-wallet.js";

export const makeReserveLifecycleBuilderFixture = async ({
  settlementWithdrawalValidity = "WithdrawalIsValid",
  externalKind,
  structuralLovelace = 2_000_000n,
  confirmedEnd = 1n,
  protectedUntil = 0n,
  scriptOwner = false,
}: {
  readonly settlementWithdrawalValidity?: SDK.WithdrawalValidity;
  readonly externalKind?: "Deposit" | "Withdrawal";
  readonly structuralLovelace?: bigint;
  readonly confirmedEnd?: bigint;
  readonly protectedUntil?: bigint;
  readonly scriptOwner?: boolean;
} = {}) => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const beneficiary = generateEmulatorAccount({
    lovelace: 2_000_000n,
  });
  const contracts = await loadRealContracts({
    txHash: "00".repeat(32),
    outputIndex: 0,
  });
  const l1AddressData = await Effect.runPromise(
    SDK.addressDataFromBech32(beneficiary.address),
  );

  const depositId = { transactionId: "11".repeat(32), outputIndex: 0n };
  const withdrawalId = { transactionId: "22".repeat(32), outputIndex: 0n };
  const depositAssetName = await Effect.runPromise(
    SDK.eventHistoryKey(depositId),
  );
  const withdrawalAssetName = await Effect.runPromise(
    SDK.eventHistoryKey(withdrawalId),
  );
  const settlementAssetName = "cc";
  const history = SDK.requireEventHistoryContracts(contracts);
  const owner = getAddressDetails(operator.address).paymentCredential!.hash;
  const authorizationScript = scriptFromNative({ type: "sig", keyHash: owner });
  const depositRetirementScript = history.deposit.retirement.withdrawalScript;
  const withdrawalRetirementScript =
    history.withdrawal.retirement.withdrawalScript;
  const depositUnit = toUnit(contracts.deposit.policyId, depositAssetName);
  const withdrawalUnit = toUnit(
    contracts.withdrawal.policyId,
    withdrawalAssetName,
  );
  const settlementUnit = toUnit(
    contracts.settlement.policyId,
    settlementAssetName,
  );
  const hubUnit = toUnit(
    contracts.hubOracle.policyId,
    SDK.HUB_ORACLE_ASSET_NAME,
  );
  const depositEvent: SDK.DepositEvent = {
    id: depositId,
    info: { l2_address: l1AddressData, l2_network_id: 0n, l2_datum: null },
  };
  const withdrawalEvent: SDK.WithdrawalEvent = {
    id: withdrawalId,
    info: {
      body: {
        l2_outref: { transactionId: "33".repeat(32), outputIndex: 0n },
        l2_owner: "44".repeat(28),
        l2_value: __reservePayoutTest.assetsToValue({ lovelace: 7_000_000n }),
        l1_address: l1AddressData,
        l1_datum: "NoDatum",
      },
      signature: ["01", "02"],
      validity: "WithdrawalIsValid",
    },
  };
  const payload = (withdrawal: boolean): SDK.EventHistoryPayload =>
    withdrawal
      ? {
          WithdrawalPayload: {
            event: withdrawalEvent,
            refund_address: l1AddressData,
            refund_datum: "NoDatum",
          },
        }
      : { DepositPayload: { event: depositEvent } };
  const retainedData: SDK.EventHistoryData | undefined =
    externalKind === undefined
      ? undefined
      : {
          event_key:
            externalKind === "Deposit" ? depositAssetName : withdrawalAssetName,
          event_payload: Data.from(
            Data.to(
              payload(externalKind === "Withdrawal"),
              SDK.EventHistoryPayload,
            ),
          ),
          reclaim_auth: scriptOwner
            ? { ScriptCredential: [validatorToScriptHash(authorizationScript)] }
            : { PublicKeyCredential: [owner] },
        };
  const order = (
    event: SDK.DepositEvent | SDK.WithdrawalEvent,
    key: string,
    withdrawal: boolean,
  ): SDK.EventHistoryNode => ({
    position: { Key: [key] },
    next: null,
    protected_until: protectedUntil,
    payload: {
      Order: {
        facts: {
          event_id: event.id,
          inclusion_time: 1n,
          structural_lovelace: withdrawal ? 0n : structuralLovelace,
          structural_refund_key: owner,
          location:
            externalKind === (withdrawal ? "Withdrawal" : "Deposit")
              ? {
                  External: {
                    storage_datum_hash: SDK.eventHistoryDataHash(retainedData!),
                  },
                }
              : { Inline: { payload: payload(withdrawal) } },
        },
      },
    },
  });
  const depositDatumCbor = Data.to(
    order(depositEvent, depositAssetName, false),
    SDK.EventHistoryNode,
  );
  const withdrawalDatumCbor = Data.to(
    order(withdrawalEvent, withdrawalAssetName, true),
    SDK.EventHistoryNode,
  );
  const eventCbors = (
    event: SDK.DepositEvent | SDK.WithdrawalEvent,
    withdrawal: boolean,
  ) => ({
    idCbor: Buffer.from(
      __reservePayoutTest.aikenSerialisedPlutusDataCbor(
        Data.to(event.id, SDK.OutputReference),
      ),
      "hex",
    ),
    infoCbor: Buffer.from(
      __reservePayoutTest.aikenSerialisedPlutusDataCbor(
        withdrawal
          ? Data.to(withdrawalEvent.info, SDK.WithdrawalInfo)
          : Data.to(depositEvent.info, SDK.DepositInfo),
      ),
      "hex",
    ),
  });
  const depositEventCbors = eventCbors(depositEvent, false);
  const withdrawalEventCbors = eventCbors(withdrawalEvent, true);
  const settlementWithdrawalInfo: SDK.WithdrawalInfo = {
    ...withdrawalEvent.info,
    validity: settlementWithdrawalValidity,
  };
  const withdrawalValueCbor =
    settlementWithdrawalValidity === withdrawalEvent.info.validity
      ? withdrawalEventCbors.infoCbor.toString("hex")
      : __reservePayoutTest.aikenSerialisedPlutusDataCbor(
          Data.to(settlementWithdrawalInfo, SDK.WithdrawalInfo),
        );
  const [depositsRoot, withdrawalsRoot] = await Promise.all([
    countedSingletonMembershipRoot(
      SDK.ROOT_DOMAINS.deposits,
      depositEventCbors.idCbor.toString("hex"),
      depositEventCbors.infoCbor.toString("hex"),
    ),
    countedSingletonMembershipRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalEventCbors.idCbor.toString("hex"),
      withdrawalValueCbor,
    ),
  ]);
  const settlementDatum: SDK.SettlementDatum = {
    deposits_root: depositsRoot.root,
    withdrawals_root: withdrawalsRoot.root,
    forced_transactions_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactions_root: "77".repeat(32),
    resolution_claim: null,
  };
  const hubDatum = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
  const hubOracleAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(contracts.hubOracle.policyId),
  );
  const root = (key: string) =>
    Data.to(
      {
        position: "Root",
        next: key,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    );
  const confirmedDatum = Data.to(
    new Constr(0, [
      new Constr(0, [
        Data.from(
          Data.to(
            {
              headerHash: "01".repeat(28),
              prevHeaderHash: "02".repeat(28),
              utxoRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              startTime: 0n,
              endTime: confirmedEnd,
              protocolVersion: 1n,
            },
            SDK.ConfirmedState,
          ),
        ),
      ]),
      new Constr(1, []),
    ]),
  );
  // Published as the live resolution requires: each script holds its role
  // token under the deployment's reference-script auth policy.
  const publishedReferenceScripts = [
    ["deposit minting", contracts.deposit.mintingScript],
    ["deposit spending", contracts.deposit.spendingScript],
    ["withdrawal minting", contracts.withdrawal.mintingScript],
    ["withdrawal spending", contracts.withdrawal.spendingScript],
    ["reserve spending", contracts.reserve.spendingScript],
    ["payout spending", contracts.payout.spendingScript],
    ["payout minting", contracts.payout.mintingScript],
    ["deposit history retirement", depositRetirementScript],
    ["withdrawal history retirement", withdrawalRetirementScript],
  ] as const;
  const emulator = new Emulator(
    [
      makeSeededScriptAccount({
        address: contracts.deposit.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [contracts.deposit.policyId]: 1n },
        inlineDatum: root(depositAssetName),
      }),
      makeSeededScriptAccount({
        address: contracts.withdrawal.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [contracts.withdrawal.policyId]: 1n },
        inlineDatum: root(withdrawalAssetName),
      }),
      makeSeededScriptAccount({
        address: contracts.stateQueue.spendingScriptAddress,
        assets: {
          lovelace: 3_000_000n,
          [contracts.stateQueue.policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME]: 1n,
        },
        inlineDatum: confirmedDatum,
      }),
      ...(retainedData === undefined
        ? []
        : [
            makeSeededScriptAccount({
              address: (externalKind === "Deposit"
                ? history.deposit
                : history.withdrawal
              ).retention.spendingScriptAddress,
              assets: { lovelace: 3_000_000n },
              inlineDatum: SDK.encodeEventHistoryData(retainedData),
            }),
          ]),
      operator,
      beneficiary,
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 10_000_000n },
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 11_000_000n },
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 12_000_000n },
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 13_000_000n },
      }),
      ...publishedReferenceScripts.map(([name, script]) =>
        makeSeededScriptAccount({
          address: operator.address,
          assets: {
            lovelace: 3_000_000n,
            [SDK.referenceScriptAuthUnit(
              contracts.referenceScriptAuth.policyId,
              name,
            )]: 1n,
          },
          scriptRef: script,
        }),
      ),
      makeSeededScriptAccount({
        address: hubOracleAddress,
        assets: { lovelace: 3_000_000n, [hubUnit]: 1n },
        inlineDatum: Data.to(hubDatum, SDK.HubOracleDatum),
      }),
      makeSeededScriptAccount({
        address: contracts.deposit.spendingScriptAddress,
        assets: {
          lovelace: 8_000_000n + structuralLovelace,
          [depositUnit]: 1n,
        },
        inlineDatum: depositDatumCbor,
      }),
      makeSeededScriptAccount({
        address: contracts.withdrawal.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [withdrawalUnit]: 1n },
        inlineDatum: withdrawalDatumCbor,
      }),
      makeSeededScriptAccount({
        address: contracts.settlement.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [settlementUnit]: 1n },
        inlineDatum: Data.to(settlementDatum, SDK.SettlementDatum),
      }),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  registerZeroRewardScript(emulator, depositRetirementScript);
  registerZeroRewardScript(emulator, withdrawalRetirementScript);
  registerZeroRewardScript(emulator, history.deposit.list.withdrawalScript);
  registerZeroRewardScript(emulator, history.withdrawal.list.withdrawalScript);

  registerZeroRewardScript(emulator, authorizationScript);
  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);

  emulator.awaitBlock(5);
  const hubOracleRefInput = findUtxoWithUnit(
    await lucid.utxosAt(hubOracleAddress),
    hubUnit,
  );
  const settlementRefInput = findUtxoWithUnit(
    await lucid.utxosAt(contracts.settlement.spendingScriptAddress),
    settlementUnit,
  );
  const referenceUtxos = await lucid.utxosAt(operator.address);
  const referenceScripts = {
    depositMinting: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.deposit.mintingScript,
    ),
    depositSpending: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.deposit.spendingScript,
    ),
    withdrawalMinting: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.withdrawal.mintingScript,
    ),
    withdrawalSpending: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.withdrawal.spendingScript,
    ),
    reserveSpending: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.reserve.spendingScript,
    ),
    payoutSpending: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.payout.spendingScript,
    ),
    payoutMinting: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.payout.mintingScript,
    ),
  };

  const depositMembershipProof: SDK.RawRootMembershipProof = {
    domain: SDK.ROOT_DOMAINS.deposits,
    root: depositsRoot.root,
    phas_root: depositsRoot.phasRoot,
    count: 1n,
    key: depositEventCbors.idCbor.toString("hex"),
    value: depositEventCbors.infoCbor.toString("hex"),
    proof: [] as SDK.Proof,
  };
  const withdrawalMembershipProof: SDK.RawRootMembershipProof = {
    domain: SDK.ROOT_DOMAINS.withdrawals,
    root: withdrawalsRoot.root,
    phas_root: withdrawalsRoot.phasRoot,
    count: 1n,
    key: withdrawalEventCbors.idCbor.toString("hex"),
    value: withdrawalValueCbor,
    proof: [] as SDK.Proof,
  };

  return {
    operator,
    history,
    authorizationScript,
    retainedInput:
      externalKind === undefined
        ? undefined
        : (
            await lucid.utxosAt(
              (externalKind === "Deposit"
                ? history.deposit
                : history.withdrawal
              ).retention.spendingScriptAddress,
            )
          )[0]!,
    beneficiary,
    contracts,
    deposit: (
      await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(
          lucid,
          SDK.eventHistoryDeploymentFromContracts(history.deposit),
        ),
      )
    )[0]!,
    depositUnit,
    feeInputs: [
      findPureAdaUtxo(referenceUtxos, 10_000_000n),
      findPureAdaUtxo(referenceUtxos, 11_000_000n),
      findPureAdaUtxo(referenceUtxos, 12_000_000n),
      findPureAdaUtxo(referenceUtxos, 13_000_000n),
    ],
    hubOracleRefInput,
    lucid,
    depositMembershipProof,
    withdrawalMembershipProof,
    referenceScriptsAddress: operator.address,
    nowMs: emulator.time,
    emulator,
    payoutUnit: toUnit(contracts.payout.policyId, withdrawalAssetName),
    referenceScripts,
    reserveAddress: contracts.reserve.spendingScriptAddress,
    settlementRefInput,
    withdrawal: (
      await Effect.runPromise(
        SDK.fetchWithdrawalUTxOsProgram(
          lucid,
          SDK.eventHistoryDeploymentFromContracts(history.withdrawal),
        ),
      )
    )[0]!,
    withdrawalUnit,
  };
};
