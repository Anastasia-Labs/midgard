import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import {
  CML,
  Data,
  Emulator,
  Lucid as makeLucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  credentialToAddress,
  generateEmulatorAccount,
  scriptHashToCredential,
  toUnit,
  type Assets,
  type EmulatorAccount,
  type LucidEvolution,
  type Script,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import {
  __reservePayoutTest,
  buildAddReserveFundsToPayoutTxProgram,
  buildAbsorbConfirmedDepositToReserveTxProgram,
  buildConcludePayoutTxProgram,
  buildInitializePayoutTxProgram,
  buildRefundInvalidWithdrawalTxProgram,
} from "@/transactions/reserve-payout.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import * as SDK from "@al-ft/midgard-sdk";

const mkUtxo = (
  txHashByte: string,
  outputIndex: number,
  assets: Assets = { lovelace: 5_000_000n },
): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  address: "addr_test1qpz4js6k2c6un3h8y8sh2nmkg7u9s8w7up0psd4w6zv6r9u9gq3h",
  assets,
});

const scriptRef = {
  type: "PlutusV3",
  script: "5900",
} as const;

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  maxCollateralInputs: 3,
} as const;

const hashHexBlake2b256 = (hex: string): Promise<string> =>
  Effect.runPromise(SDK.hashHexWithBlake2b256(hex));

const singletonMembershipRoot = async (
  keyCbor: string,
  valueCbor: string,
): Promise<string> => {
  const keyBytes = __reservePayoutTest.aikenSerialisedPlutusDataCbor(keyCbor);
  const valueBytes =
    __reservePayoutTest.aikenSerialisedPlutusDataCbor(valueCbor);
  const [keyHash, valueHash] = await Promise.all([
    hashHexBlake2b256(keyBytes),
    hashHexBlake2b256(valueBytes),
  ]);
  return hashHexBlake2b256(`ff${keyHash}${valueHash}`);
};

const loadRealContracts = (oneShotOutRef: {
  readonly txHash: string;
  readonly outputIndex: number;
}) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const placeholder = yield* AlwaysSucceedsContract;
      return yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholder,
        oneShotOutRef,
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

const findUtxoWithUnit = (
  utxos: readonly UTxO[],
  unit: string,
  quantity = 1n,
): UTxO => {
  const utxo = utxos.find((candidate) => candidate.assets[unit] === quantity);
  if (utxo === undefined) {
    throw new Error(
      `Missing UTxO with ${unit} quantity ${quantity.toString()}`,
    );
  }
  return utxo;
};

const findReferenceScriptUtxo = (
  utxos: readonly UTxO[],
  script: Script,
): UTxO => {
  const expectedHash = validatorToScriptHash(script);
  const utxo = utxos.find(
    (candidate) =>
      candidate.scriptRef != null &&
      validatorToScriptHash(candidate.scriptRef) === expectedHash,
  );
  if (utxo === undefined) {
    throw new Error(`Missing reference script UTxO for ${expectedHash}`);
  }
  return utxo;
};

const findPureAdaUtxo = (utxos: readonly UTxO[], lovelace: bigint): UTxO => {
  const utxo = utxos.find(
    (candidate) =>
      candidate.scriptRef === undefined &&
      Object.keys(candidate.assets).length === 1 &&
      candidate.assets.lovelace === lovelace,
  );
  if (utxo === undefined) {
    throw new Error(
      `Missing pure ADA UTxO with ${lovelace.toString()} lovelace`,
    );
  }
  return utxo;
};

const submitWithWallet = async (tx: TxSignBuilder): Promise<string> => {
  try {
    const signed = await tx.sign.withWallet().complete();
    expect(signed.toCBOR().length / 2).toBeLessThanOrEqual(
      PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    );
    return await signed.submit();
  } catch (cause) {
    const message =
      cause instanceof Error && cause.message.length > 0
        ? cause.message
        : String(cause);
    throw new Error(
      `Failed to sign or submit reserve/payout test tx: ${message}`,
      {
        cause,
      },
    );
  }
};

const scriptRewardAddress = (script: Script): string => {
  const credential = CML.Credential.new_script(
    CML.ScriptHash.from_hex(validatorToScriptHash(script)),
  );
  return CML.RewardAddress.new(0, credential).to_address().to_bech32();
};

const registerZeroRewardScript = (emulator: Emulator, script: Script): void => {
  emulator.chain[scriptRewardAddress(script)] = {
    registeredStake: true,
    delegation: {
      poolId: null,
      rewards: 0n,
    },
  };
};

const makeSeededScriptAccount = ({
  address,
  assets,
  inlineDatum,
  scriptRef,
}: {
  readonly address: string;
  readonly assets: Assets;
  readonly inlineDatum?: string;
  readonly scriptRef?: Script;
}): EmulatorAccount => ({
  seedPhrase: "",
  privateKey: "",
  address,
  assets,
  ...(inlineDatum === undefined && scriptRef === undefined
    ? {}
    : {
        outputData: {
          ...(inlineDatum === undefined ? {} : { inline: inlineDatum }),
          ...(scriptRef === undefined ? {} : { scriptRef }),
        },
      }),
});

const makeDepositUTxO = ({
  assetName,
  utxo,
  datum,
}: {
  readonly assetName: string;
  readonly utxo: UTxO;
  readonly datum: SDK.DepositDatum;
}): SDK.DepositUTxO => ({
  utxo,
  datum,
  assetName,
  idCbor: Buffer.from(Data.to(datum.event.id, SDK.OutputReference), "hex"),
  infoCbor: Buffer.from(Data.to(datum.event.info, SDK.DepositInfo), "hex"),
  inclusionTime: new Date(Number(datum.inclusion_time)),
});

const makeWithdrawalUTxO = ({
  assetName,
  utxo,
  datum,
}: {
  readonly assetName: string;
  readonly utxo: UTxO;
  readonly datum: SDK.WithdrawalOrderDatum;
}): SDK.WithdrawalUTxO => ({
  utxo,
  datum,
  assetName,
  idCbor: Buffer.from(Data.to(datum.event.id, SDK.OutputReference), "hex"),
  infoCbor: Buffer.from(Data.to(datum.event.info, SDK.WithdrawalInfo), "hex"),
  inclusionTime: new Date(Number(datum.inclusion_time)),
});

const makeReservePayoutBuilderFixture = async () => {
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
  const l1Address = beneficiary.address;
  const l1AddressData = await Effect.runPromise(
    SDK.addressDataFromBech32(l1Address),
  );

  const payoutAssetName = "aa";
  const payoutUnit = toUnit(contracts.payout.policyId, payoutAssetName);
  const hubUnit = toUnit(
    contracts.hubOracle.policyId,
    SDK.HUB_ORACLE_ASSET_NAME,
  );
  const targetAssets: Assets = { lovelace: 7_000_000n };
  const payoutDatum: SDK.PayoutDatum = {
    l2_value: __reservePayoutTest.assetsToValue(targetAssets),
    l1_address: l1AddressData,
    l1_datum: "NoDatum",
  };
  const payoutDatumCbor = Data.to(payoutDatum, SDK.PayoutDatum);
  const hubDatum = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
  const hubDatumCbor = Data.to(hubDatum, SDK.HubOracleDatum);
  const hubOracleAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(contracts.hubOracle.policyId),
  );
  const emulator = new Emulator(
    [
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
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.reserve.spendingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.payout.spendingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.payout.mintingScript,
      }),
      makeSeededScriptAccount({
        address: hubOracleAddress,
        assets: { lovelace: 3_000_000n, [hubUnit]: 1n },
        inlineDatum: hubDatumCbor,
      }),
      makeSeededScriptAccount({
        address: contracts.payout.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [payoutUnit]: 1n },
        inlineDatum: payoutDatumCbor,
      }),
      makeSeededScriptAccount({
        address: contracts.reserve.spendingScriptAddress,
        assets: { lovelace: 8_000_000n },
      }),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);

  const hubOracleRefInput = findUtxoWithUnit(
    await lucid.utxosAt(hubOracleAddress),
    hubUnit,
  );
  const payoutInput = findUtxoWithUnit(
    await lucid.utxosAt(contracts.payout.spendingScriptAddress),
    payoutUnit,
  );
  const reserveInput = (
    await lucid.utxosAt(contracts.reserve.spendingScriptAddress)
  ).find((utxo) => utxo.assets.lovelace === 8_000_000n);
  if (reserveInput === undefined) {
    throw new Error("Missing seeded reserve input");
  }
  const referenceUtxos = await lucid.utxosAt(operator.address);

  return {
    contracts,
    hubOracleRefInput,
    l1Address,
    lucid,
    payoutInput,
    payoutUnit,
    feeInputs: [
      findPureAdaUtxo(referenceUtxos, 10_000_000n),
      findPureAdaUtxo(referenceUtxos, 11_000_000n),
    ],
    referenceScripts: {
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
    },
    reserveInput,
  };
};

const makeReserveLifecycleBuilderFixture = async ({
  settlementWithdrawalValidity = "WithdrawalIsValid",
}: {
  readonly settlementWithdrawalValidity?: SDK.WithdrawalValidity;
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

  const depositAssetName = "dd".repeat(32);
  const withdrawalAssetName = "ee".repeat(32);
  const settlementAssetName = "cc";
  const depositWitnessScript =
    SDK.buildUserEventWitnessCertificateValidator(depositAssetName);
  const withdrawalWitnessScript =
    SDK.buildUserEventWitnessCertificateValidator(withdrawalAssetName);
  const membershipProofScript = loadPhasMembershipWithdrawalScript();
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
  const depositDatum: SDK.DepositDatum = {
    event: {
      id: {
        transactionId: "11".repeat(32),
        outputIndex: 0n,
      },
      info: {
        l2_address: l1AddressData,
        l2_datum: null,
      },
    },
    inclusion_time: 0n,
    witness: SDK.userEventWitnessScriptHash(depositAssetName),
  };
  const withdrawalDatum: SDK.WithdrawalOrderDatum = {
    event: {
      id: {
        transactionId: "22".repeat(32),
        outputIndex: 0n,
      },
      info: {
        body: {
          l2_outref: {
            transactionId: "33".repeat(32),
            outputIndex: 0n,
          },
          l2_owner: "44".repeat(28),
          l2_value: __reservePayoutTest.assetsToValue({ lovelace: 7_000_000n }),
          l1_address: l1AddressData,
          l1_datum: "NoDatum",
        },
        signature: ["01", "02"],
        validity: "WithdrawalIsValid",
      },
    },
    inclusion_time: 0n,
    witness: SDK.userEventWitnessScriptHash(withdrawalAssetName),
    refund_address: l1AddressData,
    refund_datum: "NoDatum",
  };
  const depositKeyCbor = Data.to(depositDatum.event.id, SDK.OutputReference);
  const depositValueCbor = Data.to(depositDatum.event.info, SDK.DepositInfo);
  const settlementWithdrawalInfo: SDK.WithdrawalInfo = {
    ...withdrawalDatum.event.info,
    validity: settlementWithdrawalValidity,
  };
  const withdrawalKeyCbor = Data.to(
    withdrawalDatum.event.id,
    SDK.OutputReference,
  );
  const withdrawalValueCbor = Data.to(
    settlementWithdrawalInfo,
    SDK.WithdrawalInfo,
  );
  const [depositsRoot, withdrawalsRoot] = await Promise.all([
    singletonMembershipRoot(depositKeyCbor, depositValueCbor),
    singletonMembershipRoot(withdrawalKeyCbor, withdrawalValueCbor),
  ]);
  const settlementDatum: SDK.SettlementDatum = {
    deposits_root: depositsRoot,
    withdrawals_root: withdrawalsRoot,
    transactions_root: "77".repeat(32),
    resolution_claim: null,
  };
  const hubDatum = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
  const hubOracleAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(contracts.hubOracle.policyId),
  );
  const emulator = new Emulator(
    [
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
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.deposit.mintingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.deposit.spendingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.withdrawal.mintingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.withdrawal.spendingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.reserve.spendingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.payout.spendingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: contracts.payout.mintingScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: depositWitnessScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: withdrawalWitnessScript,
      }),
      makeSeededScriptAccount({
        address: hubOracleAddress,
        assets: { lovelace: 3_000_000n, [hubUnit]: 1n },
        inlineDatum: Data.to(hubDatum, SDK.HubOracleDatum),
      }),
      makeSeededScriptAccount({
        address: contracts.deposit.spendingScriptAddress,
        assets: { lovelace: 8_000_000n, [depositUnit]: 1n },
        inlineDatum: Data.to(depositDatum, SDK.DepositDatum),
      }),
      makeSeededScriptAccount({
        address: contracts.withdrawal.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [withdrawalUnit]: 1n },
        inlineDatum: Data.to(withdrawalDatum, SDK.WithdrawalOrderDatum),
      }),
      makeSeededScriptAccount({
        address: contracts.settlement.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [settlementUnit]: 1n },
        inlineDatum: Data.to(settlementDatum, SDK.SettlementDatum),
      }),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  registerZeroRewardScript(emulator, depositWitnessScript);
  registerZeroRewardScript(emulator, withdrawalWitnessScript);
  registerZeroRewardScript(emulator, membershipProofScript);

  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);

  const hubOracleRefInput = findUtxoWithUnit(
    await lucid.utxosAt(hubOracleAddress),
    hubUnit,
  );
  const depositInput = findUtxoWithUnit(
    await lucid.utxosAt(contracts.deposit.spendingScriptAddress),
    depositUnit,
  );
  const withdrawalInput = findUtxoWithUnit(
    await lucid.utxosAt(contracts.withdrawal.spendingScriptAddress),
    withdrawalUnit,
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
    depositWitnessCertificate: findReferenceScriptUtxo(
      referenceUtxos,
      depositWitnessScript,
    ),
    withdrawalMinting: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.withdrawal.mintingScript,
    ),
    withdrawalSpending: findReferenceScriptUtxo(
      referenceUtxos,
      contracts.withdrawal.spendingScript,
    ),
    withdrawalWitnessCertificate: findReferenceScriptUtxo(
      referenceUtxos,
      withdrawalWitnessScript,
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

  return {
    beneficiary,
    contracts,
    deposit: makeDepositUTxO({
      assetName: depositAssetName,
      utxo: depositInput,
      datum: depositDatum,
    }),
    depositUnit,
    feeInputs: [
      findPureAdaUtxo(referenceUtxos, 10_000_000n),
      findPureAdaUtxo(referenceUtxos, 11_000_000n),
      findPureAdaUtxo(referenceUtxos, 12_000_000n),
      findPureAdaUtxo(referenceUtxos, 13_000_000n),
    ],
    hubOracleRefInput,
    lucid,
    membershipProof: [] as SDK.Proof,
    membershipProofWithdrawal: {
      script: membershipProofScript,
    },
    payoutUnit: toUnit(contracts.payout.policyId, withdrawalAssetName),
    referenceScripts,
    reserveAddress: contracts.reserve.spendingScriptAddress,
    settlementRefInput,
    withdrawal: makeWithdrawalUTxO({
      assetName: withdrawalAssetName,
      utxo: withdrawalInput,
      datum: withdrawalDatum,
    }),
    withdrawalUnit,
  };
};

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

  it("derives AddFunds ledger indexes and tx-info redeemer indexes from TxOutRef order", () => {
    const reserveInput = mkUtxo("10", 0);
    const payoutInput = mkUtxo("80", 0);
    const feeInput = mkUtxo("f0", 0);
    const hubRef = mkUtxo("40", 0);

    const layout = __reservePayoutTest.initialAddReserveFundsLayout({
      inputs: [payoutInput, feeInput, reserveInput],
      referenceInputs: [hubRef],
      payoutInput,
      reserveInput,
      hubOracleRefInput: hubRef,
      reserveChangeAssets: { lovelace: 1_000_000n },
    });

    expect(layout.reserveInputIndex).toBe(0n);
    expect(layout.payoutInputIndex).toBe(1n);
    expect(layout.payoutOutputIndex).toBe(0n);
    expect(layout.reserveChangeOutputIndex).toBe(1n);
    expect(layout.reserveSpendRedeemerIndex).toBe(0n);
    expect(layout.payoutSpendRedeemerIndex).toBe(1n);
    expect(layout.hubRefInputIndex).toBe(0n);
  });

  it("derives ConcludeWithdrawal spend and burn redeemer tx-info indexes", () => {
    const payoutInput = mkUtxo("80", 0);
    const feeInput = mkUtxo("10", 0);
    const hubRef = mkUtxo("40", 0);

    const layout = __reservePayoutTest.initialConcludePayoutLayout({
      inputs: [payoutInput, feeInput],
      referenceInputs: [hubRef],
      payoutInput,
      hubOracleRefInput: hubRef,
    });

    expect(layout.payoutInputIndex).toBe(1n);
    expect(layout.l1OutputIndex).toBe(0n);
    expect(layout.payoutSpendRedeemerIndex).toBe(0n);
    expect(layout.burnRedeemerIndex).toBe(1n);
    expect(layout.hubRefInputIndex).toBe(0n);
  });

  it("derives absorption, initialization, and refund layouts in ledger order", () => {
    const protocolInput = mkUtxo("10", 0);
    const feeInput = mkUtxo("f0", 0);
    const hubRef = mkUtxo("20", 0);
    const settlementRef = mkUtxo("70", 0);
    const withdrawalPolicyId = "55".repeat(28);
    const payoutPolicyId = "44".repeat(28);

    const absorptionLayout = __reservePayoutTest.initialAbsorbDepositLayout({
      inputs: [feeInput, protocolInput],
      referenceInputs: [settlementRef, hubRef],
      deposit: { utxo: protocolInput } as SDK.DepositUTxO,
      hubOracleRefInput: hubRef,
      settlementRefInput: settlementRef,
    });
    expect(absorptionLayout.depositInputIndex).toBe(0n);
    expect(absorptionLayout.hubRefInputIndex).toBe(0n);
    expect(absorptionLayout.settlementRefInputIndex).toBe(1n);
    expect(absorptionLayout.burnRedeemerIndex).toBe(1n);
    expect(absorptionLayout.witnessUnregistrationRedeemerIndex).toBe(2n);
    expect(absorptionLayout.inclusionProofWithdrawalRedeemerIndex).toBe(3n);

    const initializationLayout =
      __reservePayoutTest.initialInitializePayoutLayout({
        inputs: [feeInput, protocolInput],
        referenceInputs: [settlementRef, hubRef],
        withdrawal: { utxo: protocolInput } as SDK.WithdrawalUTxO,
        hubOracleRefInput: hubRef,
        settlementRefInput: settlementRef,
        withdrawalPolicyId,
        payoutPolicyId,
      });
    expect(initializationLayout.withdrawalInputIndex).toBe(0n);
    expect(initializationLayout.payoutMintRedeemerIndex).toBe(1n);
    expect(initializationLayout.withdrawalBurnRedeemerIndex).toBe(2n);
    expect(initializationLayout.witnessUnregistrationRedeemerIndex).toBe(3n);
    expect(initializationLayout.inclusionProofWithdrawalRedeemerIndex).toBe(4n);

    const refundLayout = __reservePayoutTest.initialRefundWithdrawalLayout({
      inputs: [feeInput, protocolInput],
      referenceInputs: [settlementRef, hubRef],
      withdrawal: { utxo: protocolInput } as SDK.WithdrawalUTxO,
      hubOracleRefInput: hubRef,
      settlementRefInput: settlementRef,
    });
    expect(refundLayout.withdrawalInputIndex).toBe(0n);
    expect(refundLayout.hubRefInputIndex).toBe(0n);
    expect(refundLayout.settlementRefInputIndex).toBe(1n);
    expect(refundLayout.burnRedeemerIndex).toBe(1n);
    expect(refundLayout.witnessUnregistrationRedeemerIndex).toBe(2n);
    expect(refundLayout.inclusionProofWithdrawalRedeemerIndex).toBe(3n);
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

  it("builds and submits absorb, initialize, reserve collection, and payout conclusion", async () => {
    const {
      beneficiary,
      contracts,
      deposit,
      depositUnit,
      feeInputs,
      hubOracleRefInput,
      lucid,
      membershipProof,
      membershipProofWithdrawal,
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
        membershipProof,
        membershipProofWithdrawal,
        referenceScripts,
        settlementRefInput,
      }),
    );
    expect(absorb.layout.reserveOutputIndex).toBeGreaterThanOrEqual(0n);
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
        membershipProof,
        membershipProofWithdrawal,
        referenceScripts,
        settlementRefInput,
        withdrawal,
      }),
    );
    expect(initialize.layout.payoutOutputIndex).toBeGreaterThanOrEqual(0n);
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

  it("builds absorption and initialization with attached dynamic witness scripts", async () => {
    const {
      contracts,
      deposit,
      feeInputs,
      hubOracleRefInput,
      lucid,
      membershipProof,
      membershipProofWithdrawal,
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
        membershipProof,
        membershipProofWithdrawal,
        referenceScripts: staticReferenceScripts,
        settlementRefInput,
      }),
    );
    expect(absorb.layout.reserveOutputIndex).toBeGreaterThanOrEqual(0n);

    const initialize = await Effect.runPromise(
      buildInitializePayoutTxProgram(lucid, contracts, {
        hubOracleRefInput,
        feeInput: feeInputs[1],
        membershipProof,
        membershipProofWithdrawal,
        referenceScripts: staticReferenceScripts,
        settlementRefInput,
        withdrawal,
      }),
    );
    expect(initialize.layout.payoutOutputIndex).toBeGreaterThanOrEqual(0n);
  });

  it("builds and submits the invalid-withdrawal refund path", async () => {
    const {
      beneficiary,
      contracts,
      feeInputs,
      hubOracleRefInput,
      lucid,
      membershipProof,
      membershipProofWithdrawal,
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
        membershipProof,
        membershipProofWithdrawal,
        referenceScripts,
        settlementRefInput,
        validityOverride: "UnpayableWithdrawalValue",
        withdrawal,
      }),
    );
    expect(refund.layout.refundOutputIndex).toBe(0n);
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

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("overlaps");
    }
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

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("pure ADA");
    }
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

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("reference script");
    }
  });

  it("skips provider-visible wallet reference-script UTxOs during automatic fee selection", async () => {
    const referenceScriptUtxo = {
      ...mkUtxo("30", 0, { lovelace: 20_000_000n }),
      scriptRef,
    };
    const plainUtxo = mkUtxo("40", 0, { lovelace: 3_000_000n });
    const lucid = {
      config: () => ({ provider: {} }),
      wallet: () => ({
        address: async () => "addr_test1operator",
      }),
      utxosAt: async () => [referenceScriptUtxo, plainUtxo],
    } as unknown as LucidEvolution;

    const selected = await Effect.runPromise(
      __reservePayoutTest.selectFeeInputProgram(lucid, undefined, []),
    );

    expect(selected).toEqual(plainUtxo);
  });

  it("fails with missing reference-script diagnostics for refund builders", async () => {
    const lucid = {
      config: () => ({ network: "Preprod" }),
      utxosAt: async () => [],
    } as unknown as LucidEvolution;
    const contracts = {
      withdrawal: {
        mintingScript: scriptRef,
        spendingScript: scriptRef,
      },
    } as SDK.MidgardValidators;
    const assetName = "aa".repeat(32);

    const result = await Effect.runPromise(
      Effect.either(
        buildRefundInvalidWithdrawalTxProgram(lucid, contracts, {
          hubOracleRefInput: mkUtxo("50", 0),
          membershipProofWithdrawal: { script: scriptRef },
          referenceScriptsAddress: "addr_test1reference",
          withdrawal: {
            assetName,
            utxo: mkUtxo("51", 0),
          },
        } as any),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(String(result.left.cause)).toContain("withdrawal minting");
      expect(String(result.left.cause)).toContain("addr_test1reference");
    }
  });
});
