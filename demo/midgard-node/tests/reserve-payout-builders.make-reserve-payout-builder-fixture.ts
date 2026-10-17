import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  credentialToAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type Script,
  scriptHashToCredential,
  toUnit,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { __reservePayoutTest } from "../src/transactions/reserve-payout.js";
import {
  findRedeemerDataCbor,
  getRedeemerPointersInContextOrder,
} from "./helpers/redeemer-inspection.js";
import {
  canonicalDatumCbor,
  decodeRedeemer,
  EMULATOR_PROTOCOL_PARAMETERS,
  findPureAdaUtxo,
  findReferenceScriptUtxo,
  findUtxoWithUnit,
  loadRealContracts,
  mintPointer,
  requireEventOutputIndex,
  requireTxInputIndex,
} from "./reserve-payout-builders.submit-with-wallet.js";

export const expectAuthenticateMintRedeemerLayout = ({
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

export const registerZeroRewardScript = (
  emulator: Emulator,
  script: Script,
): void => {
  emulator.chain[scriptRewardAddress(script)] = {
    registeredStake: true,
    delegation: {
      poolId: null,
      rewards: 0n,
    },
  };
};

export const makeSeededScriptAccount = ({
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

export const makeReservePayoutBuilderFixture = async ({
  plantedDatumReserve = false,
}: {
  /** Anyone can pay a datum-bearing UTxO to the reserve address. */
  readonly plantedDatumReserve?: boolean;
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
      ...(plantedDatumReserve
        ? [
            makeSeededScriptAccount({
              address: contracts.reserve.spendingScriptAddress,
              assets: { lovelace: 20_000_000n },
              inlineDatum: Data.void(),
            }),
          ]
        : []),
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
