import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";

import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  Constr,
  coreToTxOutput,
  credentialToAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid as makeLucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  scriptFromNative,
  scriptHashToCredential,
  toUnit,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import {
  __reservePayoutTest,
  buildAbsorbConfirmedDepositToReserveTxProgram,
  buildAddReserveFundsToPayoutTxProgram,
  buildConcludePayoutTxProgram,
  buildInitializePayoutTxProgram,
  buildRefundInvalidWithdrawalTxProgram,
} from "../src/transactions/reserve-payout.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  findRedeemerDataCbor,
  getRedeemerPointersInContextOrder,
  type RedeemerPointer,
  resolveMintPolicyContextIndex,
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

const countedSingletonMembershipRoot = async (
  domain: SDK.RootDomain,
  keyCbor: string,
  valueCbor: string,
): Promise<{ readonly root: string; readonly phasRoot: string }> => {
  const phasRoot = await singletonMembershipRoot(keyCbor, valueCbor);
  const root = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain,
      phasRoot,
      count: 1n,
    }),
  );
  return { root, phasRoot };
};

const deploymentRecords = new Map<string, unknown>();
const loadRealContracts: typeof loadRealMidgardContractsForTest = async (
  ...args
) => {
  const contracts = await loadRealMidgardContractsForTest(...args);
  const pair = SDK.requireEventHistoryContracts(contracts);
  const historyIdentity = (history: SDK.EventHistoryContracts) => ({
    recipe: history.recipe,
    listPolicyId: history.list.policyId,
    listAddress: history.list.spendingScriptAddress,
    retirementHash: validatorToScriptHash(history.retirement.withdrawalScript),
    retentionHash: history.retention.spendingScriptHash,
  });
  deploymentRecords.set(contracts.payout.policyId, {
    provenance:
      "Real applied list/retirement/retention/payout scripts; seeded fixture hub, confirmed-state and settlement UTxOs. This is not real frontier establishment or live acceptance.",
    hubPolicyId: contracts.hubOracle.policyId,
    confirmedPolicyId: contracts.stateQueue.policyId,
    settlementPolicyId: contracts.settlement.policyId,
    payoutPolicyId: contracts.payout.policyId,
    reserveHash: contracts.reserve.spendingScriptHash,
    deposit: historyIdentity(pair.deposit),
    withdrawal: historyIdentity(pair.withdrawal),
  });
  return contracts;
};

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

const signedMeasurements: {
  txHash: string;
  testName: string | undefined;
  transactionCbor: string;
  bytes: number;
  memory: string;
  steps: string;
  fee: string;
}[] = [];
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

const submitWithWallet = async (tx: TxSignBuilder): Promise<string> => {
  try {
    const signed = await tx.sign.withWallet().complete();
    expect(signed.toCBOR().length / 2).toBeLessThanOrEqual(
      PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    );
    const transaction = CML.Transaction.from_cbor_hex(signed.toCBOR());
    const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
    let memory = 0n;
    let steps = 0n;
    for (let index = 0; index < (redeemers?.len() ?? 0); index++) {
      memory += redeemers!.get(index).ex_units().mem();
      steps += redeemers!.get(index).ex_units().steps();
    }
    expect(memory).toBeLessThanOrEqual(PROTOCOL_PARAMETERS_DEFAULT.maxTxExMem);
    expect(steps).toBeLessThanOrEqual(PROTOCOL_PARAMETERS_DEFAULT.maxTxExSteps);
    const txHash = await signed.submit();
    signedMeasurements.push({
      txHash,
      testName: expect.getState().currentTestName,
      transactionCbor: signed.toCBOR(),
      bytes: signed.toCBOR().length / 2,
      memory: memory.toString(),
      steps: steps.toString(),
      fee: transaction.body().fee().toString(),
    });
    return txHash;
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
  const withdrawPointers = getRedeemerPointersInContextOrder(
    transaction,
  ).filter((pointer) => pointer.tag === CML.RedeemerTag.Reward);
  expect(withdrawPointers).toHaveLength(1);
  const redeemer = decodeRedeemer<SDK.EventHistoryObserve>(
    transaction,
    withdrawPointers[0]!,
    SDK.EventHistoryObserve,
  );
  if (!("Apply" in redeemer) || !("InsertOrder" in redeemer.Apply.operation))
    throw new Error("Expected history insertion");
  expect(redeemer.Apply.hub_reference_index).toBe(
    requireTxInputIndex(
      transaction.body().reference_inputs(),
      hubOracleRefInput,
      "hub oracle reference",
    ),
  );
  expect(redeemer.Apply.hub_reference_index).toBeGreaterThan(0n);
  expect(redeemer.Apply.operation.InsertOrder.nonce_input_index).toBe(
    requireTxInputIndex(transaction.body().inputs(), nonceInput, "nonce"),
  );
  expect(redeemer.Apply.operation.InsertOrder.order_output_index).toBe(
    requireEventOutputIndex(transaction, eventAddress, eventUnit),
  );
  expect(
    Data.from(
      findRedeemerDataCbor(transaction, mintPointer([policyId], policyId))!,
    ),
  ).toEqual(Data.from(Data.void()));
  expect(transaction.body().certs()?.len() ?? 0).toBe(0);
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
  const history = SDK.requireEventHistoryContracts(contracts);
  const emptyRoot = Data.to(
    {
      position: "Root",
      next: null,
      protected_until: 0n,
      payload: "RootContent",
    },
    SDK.EventHistoryNode,
  );
  const emulator = new Emulator(
    [
      makeSeededScriptAccount({
        address: contracts.deposit.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [contracts.deposit.policyId]: 1n },
        inlineDatum: emptyRoot,
      }),
      makeSeededScriptAccount({
        address: contracts.withdrawal.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [contracts.withdrawal.policyId]: 1n },
        inlineDatum: emptyRoot,
      }),
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
  registerZeroRewardScript(emulator, history.deposit.list.withdrawalScript);
  registerZeroRewardScript(emulator, history.withdrawal.list.withdrawalScript);
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
        scriptRef: depositRetirementScript,
      }),
      makeSeededScriptAccount({
        address: operator.address,
        assets: { lovelace: 3_000_000n },
        scriptRef: withdrawalRetirementScript,
      }),
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

type RetirementLayout = {
  readonly witness: SDK.EventHistoryRetirementWitness;
  readonly hubRefInputIndex: bigint;
  readonly retirementWithdrawalRedeemerIndex: bigint;
  readonly listWithdrawalRedeemerIndex: bigint;
};
const expectRetirementLayout = (
  built: SDK.BuiltReservePayoutTx<RetirementLayout>,
) => {
  const tx = built.tx.toTransaction();
  const pointers = getRedeemerPointersInContextOrder(tx);
  const retirement = decodeRedeemer<SDK.EventHistoryRetirementArgs>(
    tx,
    pointers[Number(built.layout.retirementWithdrawalRedeemerIndex)]!,
    SDK.EventHistoryRetirementArgs,
  );
  const observe = decodeRedeemer<SDK.EventHistoryObserve>(
    tx,
    pointers[Number(built.layout.listWithdrawalRedeemerIndex)]!,
    SDK.EventHistoryObserve,
  );
  expect(retirement).toEqual({
    hub_reference_index: built.layout.hubRefInputIndex,
    witness: built.layout.witness,
  });
  expect(observe).toEqual({
    Apply: {
      hub_reference_index: built.layout.hubRefInputIndex,
      operation: SDK.eventHistoryRetirementOperation(built.layout.witness),
    },
  });
  const spendCbor = findRedeemerDataCbor(tx, {
    tag: CML.RedeemerTag.Spend,
    index: built.layout.witness.order_input_index,
  });
  expect(Data.from(spendCbor!)).toBe(built.layout.witness.order_input_index);
  expect(tx.body().certs()?.len() ?? 0).toBe(0);
  const claims = [
    built.layout.witness.predecessor_output_index,
    built.layout.witness.funds_output_index,
    built.layout.witness.structural_refund_output_index,
  ].filter((index) => index !== null);
  expect(new Set(claims).size).toBe(claims.length);
};
const expectAbsorbRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<
    RetirementLayout & { depositInputIndex: bigint; reserveOutputIndex: bigint }
  >,
) => {
  expectRetirementLayout(built);
  expect(built.layout.witness.order_input_index).toBe(
    built.layout.depositInputIndex,
  );
  expect(built.layout.witness.funds_output_index).toBe(
    built.layout.reserveOutputIndex,
  );
  expect(built.layout.witness.purpose).toBe("AbsorbDeposit");
};
const expectInitializeRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<
    RetirementLayout & {
      withdrawalInputIndex: bigint;
      payoutOutputIndex: bigint;
    }
  >,
  contracts: SDK.MidgardValidators,
) => {
  expectRetirementLayout(built);
  expect(built.layout.witness.order_input_index).toBe(
    built.layout.withdrawalInputIndex,
  );
  expect(built.layout.witness.funds_output_index).toBe(
    built.layout.payoutOutputIndex,
  );
  expect(built.layout.witness.purpose).toBe("InitializeWithdrawalPayout");
  const payoutMint = decodeRedeemer<SDK.PayoutMintRedeemer>(
    built.tx.toTransaction(),
    mintPointer(
      [contracts.withdrawal.policyId, contracts.payout.policyId],
      contracts.payout.policyId,
    ),
    SDK.PayoutMintRedeemer,
  );
  if (!("MintPayout" in payoutMint)) throw new Error("Expected payout mint");
  expect(payoutMint.MintPayout.withdrawal_input_index).toBe(
    built.layout.withdrawalInputIndex,
  );
  expect(payoutMint.MintPayout.retirement_withdraw_redeemer_index).toBe(
    built.layout.retirementWithdrawalRedeemerIndex,
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
  built: SDK.BuiltReservePayoutTx<
    RetirementLayout & {
      withdrawalInputIndex: bigint;
      refundOutputIndex: bigint;
    }
  >,
  validityOverride: SDK.WithdrawalValidity,
) => {
  expectRetirementLayout(built);
  expect(built.layout.witness.order_input_index).toBe(
    built.layout.withdrawalInputIndex,
  );
  expect(built.layout.witness.funds_output_index).toBe(
    built.layout.refundOutputIndex,
  );
  expect(built.layout.witness.purpose).toEqual({
    RefundInvalidWithdrawal: { validity: validityOverride },
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
    const protectedFixture = await makeReserveLifecycleBuilderFixture({
      protectedUntil: BigInt(Date.now() + 1_000_000),
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
    expect(String(expectLeft(protectedResult).cause)).toContain(
      "still protected",
    );
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
