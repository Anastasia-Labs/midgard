import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  credentialToAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  scriptHashToCredential,
  toUnit,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";
import {
  __reservePayoutTest,
  buildAbsorbConfirmedDepositToReserveTxProgram,
  buildAddReserveFundsToPayoutTxProgram,
  buildConcludePayoutTxProgram,
  buildInitializePayoutTxProgram,
  buildRefundInvalidWithdrawalTxProgram,
} from "@/transactions/reserve-payout.js";

import {
  findRedeemerDataCbor,
  getRedeemerPointersInContextOrder,
  type RedeemerPointer,
  resolveMintPolicyContextIndex,
  resolveRedeemerTxInfoIndex,
} from "./helpers/redeemer-inspection.js";

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
  Effect.runPromise(SDK.hashHexWithBlake2b(hex, 32));

const canonicalDatumCbor = (cbor: string): string =>
  CML.PlutusData.from_cbor_hex(cbor).to_canonical_cbor_hex();

const userEventCborFieldsFromDatumCbor = (datum: string) =>
  SDK.userEventCborFieldsFromInlineDatum({
    txHash: "00".repeat(32),
    outputIndex: 0,
    datum,
  });

const expectLeft = <E, A>(
  result:
    | { readonly _tag: "Left"; readonly left: E }
    | { readonly _tag: "Right"; readonly right: A },
): E => {
  expect(result._tag).toBe("Left");
  if (result._tag !== "Left") {
    throw new Error("Expected Left");
  }
  return result.left;
};

const singletonMembershipRoot = async (
  keyCbor: string,
  valueCbor: string,
): Promise<string> => {
  const [keyHash, valueHash] = await Promise.all([
    hashHexBlake2b256(keyCbor),
    hashHexBlake2b256(valueCbor),
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
        { referenceScriptAuth: placeholder.referenceScriptAuth },
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

const findReferenceScriptUtxoBefore = (
  utxos: readonly UTxO[],
  script: Script,
  later: UTxO,
): UTxO => {
  const expectedHash = validatorToScriptHash(script);
  const utxo = utxos.find(
    (candidate) =>
      candidate.scriptRef != null &&
      validatorToScriptHash(candidate.scriptRef) === expectedHash &&
      compareOutRefs(candidate, later) < 0,
  );
  if (utxo === undefined) {
    throw new Error(
      `Missing reference script UTxO for ${expectedHash} that sorts before ${later.txHash}#${later.outputIndex.toString()}`,
    );
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

const decodeRedeemer = <T>(
  tx: CML.Transaction,
  pointer: RedeemerPointer,
  schema: unknown,
): T => {
  const cbor = findRedeemerDataCbor(tx, pointer);
  if (cbor === undefined) {
    throw new Error(
      `Missing redeemer tag=${pointer.tag.toString()} index=${pointer.index.toString()}`,
    );
  }
  return Data.from(cbor, schema as never) as T;
};

const mintPointer = (
  policyIds: readonly string[],
  targetPolicyId: string,
): RedeemerPointer => ({
  tag: CML.RedeemerTag.Mint,
  index: resolveMintPolicyContextIndex({ policyIds, targetPolicyId }),
});

type CmlInputSet = {
  len(): number;
  get(index: number): CML.TransactionInput;
};

const requireTxInputIndex = (
  inputs: CmlInputSet | undefined,
  target: Pick<UTxO, "txHash" | "outputIndex">,
  label: string,
): bigint => {
  if (inputs === undefined) {
    throw new Error(`${label} inputs are missing from final tx`);
  }
  const outRefs = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  for (let index = 0; index < outRefs.length; index += 1) {
    const input = outRefs[index]!;
    if (
      input.txHash === target.txHash &&
      input.outputIndex === target.outputIndex
    ) {
      return BigInt(index);
    }
  }
  throw new Error(
    `${label} input ${target.txHash}#${target.outputIndex.toString()} is missing from final tx`,
  );
};

const requireEventOutputIndex = (
  tx: CML.Transaction,
  eventAddress: string,
  eventUnit: string,
): bigint => {
  const outputs = tx.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === eventAddress &&
      output.datum != null &&
      output.assets[eventUnit] === 1n
    ) {
      return BigInt(index);
    }
  }
  throw new Error(`Missing event output for ${eventUnit} at ${eventAddress}`);
};

const expectAuthenticateMintRedeemerLayout = ({
  tx,
  policyId,
  eventAddress,
  eventUnit,
  nonceInput,
  hubOracleRefInput,
}: {
  readonly tx: TxSignBuilder;
  readonly policyId: string;
  readonly eventAddress: string;
  readonly eventUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly hubOracleRefInput: UTxO;
}): void => {
  const transaction = tx.toTransaction();
  const redeemer = decodeRedeemer<SDK.UserEventMintRedeemer>(
    transaction,
    mintPointer([policyId], policyId),
    SDK.UserEventMintRedeemer,
  );
  if (!("AuthenticateEvent" in redeemer)) {
    throw new Error("Expected AuthenticateEvent mint redeemer");
  }

  const hubRefInputIndex = requireTxInputIndex(
    transaction.body().reference_inputs(),
    hubOracleRefInput,
    "hub oracle reference",
  );
  expect(hubRefInputIndex).toBeGreaterThan(0n);
  expect(redeemer.AuthenticateEvent.nonce_input_index).toBe(
    requireTxInputIndex(transaction.body().inputs(), nonceInput, "nonce"),
  );
  expect(redeemer.AuthenticateEvent.event_output_index).toBe(
    requireEventOutputIndex(transaction, eventAddress, eventUnit),
  );
  expect(redeemer.AuthenticateEvent.hub_ref_input_index).toBe(hubRefInputIndex);
  expect(redeemer.AuthenticateEvent.witness_registration_redeemer_index).toBe(
    resolveRedeemerTxInfoIndex({
      pointers: getRedeemerPointersInContextOrder(transaction),
      target: { tag: CML.RedeemerTag.Cert, index: 0n },
    }),
  );
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
  ...SDK.userEventCborFieldsFromInlineDatum(utxo),
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
  ...SDK.userEventCborFieldsFromInlineDatum(utxo),
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
        inlineDatum: canonicalDatumCbor(payoutDatumCbor),
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

const makeUserEventBuilderFixture = async () => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const referenceHosts = Array.from({ length: 24 }, () =>
    generateEmulatorAccount({
      lovelace: 2_000_000n,
    }),
  );
  const beneficiary = generateEmulatorAccount({
    lovelace: 2_000_000n,
  });
  const contracts = await loadRealContracts({
    txHash: "00".repeat(32),
    outputIndex: 0,
  });
  const hubUnit = toUnit(
    contracts.hubOracle.policyId,
    SDK.HUB_ORACLE_ASSET_NAME,
  );
  const hubDatum = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
  const hubOracleAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(contracts.hubOracle.policyId),
  );
  const emulator = new Emulator(
    [
      operator,
      ...referenceHosts,
      beneficiary,
      ...referenceHosts.flatMap((host) => [
        makeSeededScriptAccount({
          address: host.address,
          assets: { lovelace: 3_000_000n },
          scriptRef: contracts.deposit.mintingScript,
        }),
        makeSeededScriptAccount({
          address: host.address,
          assets: { lovelace: 3_000_000n },
          scriptRef: contracts.withdrawal.mintingScript,
        }),
      ]),
      makeSeededScriptAccount({
        address: hubOracleAddress,
        assets: { lovelace: 3_000_000n, [hubUnit]: 1n },
        inlineDatum: Data.to(hubDatum, SDK.HubOracleDatum),
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
  const referenceUtxos = (
    await Promise.all(referenceHosts.map((host) => lucid.utxosAt(host.address)))
  ).flat();
  return {
    beneficiary,
    contracts,
    depositMintingReference: findReferenceScriptUtxoBefore(
      referenceUtxos,
      contracts.deposit.mintingScript,
      hubOracleRefInput,
    ),
    hubOracleRefInput,
    lucid,
    withdrawalMintingReference: findReferenceScriptUtxoBefore(
      referenceUtxos,
      contracts.withdrawal.mintingScript,
      hubOracleRefInput,
    ),
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
  const depositDatumCbor = Data.to(depositDatum, SDK.DepositDatum);
  const withdrawalDatumCbor = Data.to(
    withdrawalDatum,
    SDK.WithdrawalOrderDatum,
  );
  const depositEventCbors = userEventCborFieldsFromDatumCbor(depositDatumCbor);
  const withdrawalEventCbors =
    userEventCborFieldsFromDatumCbor(withdrawalDatumCbor);
  const settlementWithdrawalInfo: SDK.WithdrawalInfo = {
    ...withdrawalDatum.event.info,
    validity: settlementWithdrawalValidity,
  };
  const withdrawalValueCbor =
    settlementWithdrawalValidity === withdrawalDatum.event.info.validity
      ? withdrawalEventCbors.infoCbor.toString("hex")
      : __reservePayoutTest.aikenSerialisedPlutusDataCbor(
          Data.to(settlementWithdrawalInfo, SDK.WithdrawalInfo),
        );
  const [depositsRoot, withdrawalsRoot] = await Promise.all([
    singletonMembershipRoot(
      depositEventCbors.idCbor.toString("hex"),
      depositEventCbors.infoCbor.toString("hex"),
    ),
    singletonMembershipRoot(
      withdrawalEventCbors.idCbor.toString("hex"),
      withdrawalValueCbor,
    ),
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

const expectAbsorbRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<{
    readonly depositInputIndex: bigint;
    readonly reserveOutputIndex: bigint;
    readonly hubRefInputIndex: bigint;
    readonly settlementRefInputIndex: bigint;
    readonly burnRedeemerIndex: bigint;
    readonly inclusionProofWithdrawalRedeemerIndex: bigint;
  }>,
): void => {
  const redeemer = decodeRedeemer<SDK.DepositSpendRedeemer>(
    built.tx.toTransaction(),
    { tag: CML.RedeemerTag.Spend, index: built.layout.depositInputIndex },
    SDK.DepositSpendRedeemer,
  );
  expect(redeemer.input_index).toBe(built.layout.depositInputIndex);
  expect(redeemer.output_index).toBe(built.layout.reserveOutputIndex);
  expect(redeemer.hub_ref_input_index).toBe(built.layout.hubRefInputIndex);
  expect(redeemer.settlement_ref_input_index).toBe(
    built.layout.settlementRefInputIndex,
  );
  expect(redeemer.mint_redeemer_index).toBe(built.layout.burnRedeemerIndex);
  expect(redeemer.inclusion_proof_script_withdraw_redeemer_index).toBe(
    built.layout.inclusionProofWithdrawalRedeemerIndex,
  );
};

const expectInitializeRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<{
    readonly withdrawalInputIndex: bigint;
    readonly payoutOutputIndex: bigint;
    readonly hubRefInputIndex: bigint;
    readonly settlementRefInputIndex: bigint;
    readonly withdrawalBurnRedeemerIndex: bigint;
    readonly payoutMintRedeemerIndex: bigint;
    readonly withdrawalSpendRedeemerIndex: bigint;
    readonly inclusionProofWithdrawalRedeemerIndex: bigint;
  }>,
  contracts: SDK.MidgardValidators,
): void => {
  const tx = built.tx.toTransaction();
  const withdrawalSpend = decodeRedeemer<SDK.WithdrawalSpendRedeemer>(
    tx,
    { tag: CML.RedeemerTag.Spend, index: built.layout.withdrawalInputIndex },
    SDK.WithdrawalSpendRedeemer,
  );
  expect(withdrawalSpend.input_index).toBe(built.layout.withdrawalInputIndex);
  expect(withdrawalSpend.output_index).toBe(built.layout.payoutOutputIndex);
  expect(withdrawalSpend.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
  expect(withdrawalSpend.settlement_ref_input_index).toBe(
    built.layout.settlementRefInputIndex,
  );
  expect(withdrawalSpend.burn_redeemer_index).toBe(
    built.layout.withdrawalBurnRedeemerIndex,
  );
  expect(withdrawalSpend.payout_mint_redeemer_index).toBe(
    built.layout.payoutMintRedeemerIndex,
  );
  expect(withdrawalSpend.inclusion_proof_script_withdraw_redeemer_index).toBe(
    built.layout.inclusionProofWithdrawalRedeemerIndex,
  );
  expect(withdrawalSpend.purpose).toBe("InitializePayout");

  const payoutMint = decodeRedeemer<SDK.PayoutMintRedeemer>(
    tx,
    mintPointer(
      [contracts.withdrawal.policyId, contracts.payout.policyId],
      contracts.payout.policyId,
    ),
    SDK.PayoutMintRedeemer,
  );
  if (!("MintPayout" in payoutMint)) {
    throw new Error("Expected MintPayout redeemer");
  }
  expect(payoutMint.MintPayout.withdrawal_input_index).toBe(
    built.layout.withdrawalInputIndex,
  );
  expect(payoutMint.MintPayout.withdrawal_spend_redeemer_index).toBe(
    built.layout.withdrawalSpendRedeemerIndex,
  );
  expect(payoutMint.MintPayout.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
};

const expectAddFundsRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<{
    readonly payoutInputIndex: bigint;
    readonly reserveInputIndex: bigint;
    readonly payoutOutputIndex: bigint;
    readonly reserveChangeOutputIndex: bigint | null;
    readonly payoutSpendRedeemerIndex: bigint;
    readonly reserveSpendRedeemerIndex: bigint;
    readonly hubRefInputIndex: bigint;
  }>,
): void => {
  const tx = built.tx.toTransaction();
  const payoutSpend = decodeRedeemer<SDK.PayoutSpendRedeemer>(
    tx,
    { tag: CML.RedeemerTag.Spend, index: built.layout.payoutInputIndex },
    SDK.PayoutSpendRedeemer,
  );
  if (!("AddFunds" in payoutSpend)) {
    throw new Error("Expected AddFunds payout redeemer");
  }
  expect(payoutSpend.AddFunds.payout_input_index).toBe(
    built.layout.payoutInputIndex,
  );
  expect(payoutSpend.AddFunds.payout_output_index).toBe(
    built.layout.payoutOutputIndex,
  );
  expect(payoutSpend.AddFunds.reserve_input_index).toBe(
    built.layout.reserveInputIndex,
  );
  expect(payoutSpend.AddFunds.reserve_change_output_index).toBe(
    built.layout.reserveChangeOutputIndex,
  );
  expect(payoutSpend.AddFunds.reserve_spend_redeemer_index).toBe(
    built.layout.reserveSpendRedeemerIndex,
  );
  expect(payoutSpend.AddFunds.payout_spend_redeemer_index).toBe(
    built.layout.payoutSpendRedeemerIndex,
  );
  expect(payoutSpend.AddFunds.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );

  const reserveSpend = decodeRedeemer<any>(
    tx,
    { tag: CML.RedeemerTag.Spend, index: built.layout.reserveInputIndex },
    SDK.ReserveSpendRedeemer,
  );
  const reserveSpendBody = reserveSpend.Spend ?? reserveSpend;
  expect(reserveSpendBody.reserve_input_index).toBe(
    built.layout.reserveInputIndex,
  );
  expect(reserveSpendBody.payout_input_index).toBe(
    built.layout.payoutInputIndex,
  );
  expect(reserveSpendBody.payout_spend_redeemer_index).toBe(
    built.layout.payoutSpendRedeemerIndex,
  );
  expect(reserveSpendBody.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
};

const expectConcludeRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<{
    readonly payoutInputIndex: bigint;
    readonly l1OutputIndex: bigint;
    readonly payoutSpendRedeemerIndex: bigint;
    readonly burnRedeemerIndex: bigint;
    readonly hubRefInputIndex: bigint;
  }>,
): void => {
  const tx = built.tx.toTransaction();
  const payoutSpend = decodeRedeemer<SDK.PayoutSpendRedeemer>(
    tx,
    { tag: CML.RedeemerTag.Spend, index: built.layout.payoutInputIndex },
    SDK.PayoutSpendRedeemer,
  );
  if (!("ConcludeWithdrawal" in payoutSpend)) {
    throw new Error("Expected ConcludeWithdrawal payout redeemer");
  }
  expect(payoutSpend.ConcludeWithdrawal.payout_input_index).toBe(
    built.layout.payoutInputIndex,
  );
  expect(payoutSpend.ConcludeWithdrawal.l1_output_index).toBe(
    built.layout.l1OutputIndex,
  );
  expect(payoutSpend.ConcludeWithdrawal.burn_redeemer_index).toBe(
    built.layout.burnRedeemerIndex,
  );
  expect(payoutSpend.ConcludeWithdrawal.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
};

const expectRefundRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<{
    readonly withdrawalInputIndex: bigint;
    readonly refundOutputIndex: bigint;
    readonly hubRefInputIndex: bigint;
    readonly settlementRefInputIndex: bigint;
    readonly burnRedeemerIndex: bigint;
    readonly inclusionProofWithdrawalRedeemerIndex: bigint;
  }>,
  validityOverride: SDK.WithdrawalValidity,
): void => {
  const withdrawalSpend = decodeRedeemer<SDK.WithdrawalSpendRedeemer>(
    built.tx.toTransaction(),
    { tag: CML.RedeemerTag.Spend, index: built.layout.withdrawalInputIndex },
    SDK.WithdrawalSpendRedeemer,
  );
  expect(withdrawalSpend.input_index).toBe(built.layout.withdrawalInputIndex);
  expect(withdrawalSpend.output_index).toBe(built.layout.refundOutputIndex);
  expect(withdrawalSpend.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
  expect(withdrawalSpend.settlement_ref_input_index).toBe(
    built.layout.settlementRefInputIndex,
  );
  expect(withdrawalSpend.burn_redeemer_index).toBe(
    built.layout.burnRedeemerIndex,
  );
  expect(withdrawalSpend.payout_mint_redeemer_index).toBe(0n);
  expect(withdrawalSpend.inclusion_proof_script_withdraw_redeemer_index).toBe(
    built.layout.inclusionProofWithdrawalRedeemerIndex,
  );
  expect(withdrawalSpend.purpose).toEqual({
    Refund: { validity_override: validityOverride },
  });
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
    expectAbsorbRedeemerLayout(absorb);
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
    expectInitializeRedeemerLayout(initialize, contracts);
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
    expectAbsorbRedeemerLayout(absorb);

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
    expectInitializeRedeemerLayout(initialize, contracts);
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
    const lucid = {
      config: () => ({ network: "Preprod" }),
      utxosAt: async () => [],
    } as unknown as LucidEvolution;
    const contracts = await loadRealContracts({
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
    const assetName = "aa".repeat(32);
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    const hubOracleDatum = await Effect.runPromise(
      SDK.makeHubOracleDatum(contracts),
    );

    const result = await Effect.runPromise(
      Effect.either(
        buildRefundInvalidWithdrawalTxProgram(lucid, contracts, {
          hubOracleRefInput: {
            ...mkUtxo("50", 0, {
              lovelace: 5_000_000n,
              [hubOracleUnit]: 1n,
            }),
            datum: Data.to(hubOracleDatum, SDK.HubOracleDatum),
          },
          membershipProofWithdrawal: { script: scriptRef },
          referenceScriptsAddress: "addr_test1reference",
          withdrawal: {
            assetName,
            utxo: mkUtxo("51", 0),
          },
        } as any),
      ),
    );

    const left = expectLeft(result);
    expect(String(left.cause)).toContain("withdrawal minting");
    expect(String(left.cause)).toContain("addr_test1reference");
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
          membershipProofWithdrawal: { script: scriptRef },
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
