import * as SDK from "@/reserve-payout/primitives.js";
import {
  type Assets,
  CML,
  type Credential,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type OutputDatum,
  type Script,
  scriptHashToCredential,
  toUnit,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  addAssets,
  assertAssetsNonNegative,
  assertNoAssetExceeds,
  assetsEqual,
  assetsToValue,
  hasNonZeroAssetQuantity,
  minPositiveAssets,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "@/reserve-payout/assets.js";
import {
  type BuiltReservePayoutTx,
  completeWithTwoPassLayoutProgram,
} from "@/reserve-payout/completion.js";
import { formatLayout } from "@/reserve-payout/diagnostics.js";
import { fail, ReservePayoutTxError } from "@/reserve-payout/errors.js";
import {
  disposableFeeInputCandidates,
  isProviderSpendableUtxo,
  selectFeeInputProgram,
} from "@/reserve-payout/inputs.js";
import {
  type AbsorbDepositLayout,
  type AddReserveFundsLayout,
  type ConcludePayoutLayout,
  deriveAbsorbDepositLayout,
  deriveAddReserveFundsLayout,
  deriveConcludePayoutLayout,
  deriveInitializePayoutLayout,
  deriveRefundWithdrawalLayout,
  initialAbsorbDepositLayout,
  initialAddReserveFundsLayout,
  initialConcludePayoutLayout,
  initialInitializePayoutLayout,
  type InitializePayoutLayout,
  initialRefundWithdrawalLayout,
  type RefundWithdrawalLayout,
  sameAbsorbDepositLayout,
  sameAddReserveFundsLayout,
  sameConcludePayoutLayout,
  sameInitializePayoutLayout,
  sameRefundWithdrawalLayout,
  settlementDatumFromInput,
} from "@/reserve-payout/layout.js";
import {
  attachIfMissing,
  mergeReferenceScripts,
  referenceInputs,
  type ReservePayoutReferenceScripts,
  resolveReferenceScriptsProgram,
} from "@/reserve-payout/references.js";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";

export {
  addAssets,
  assetsToValue,
  assetsEqual,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "@/reserve-payout/assets.js";
export type { BuiltReservePayoutTx } from "@/reserve-payout/completion.js";
export { ReservePayoutTxError } from "@/reserve-payout/errors.js";
export { mergeReferenceScripts } from "@/reserve-payout/references.js";
export type { ReservePayoutReferenceScripts } from "@/reserve-payout/references.js";

export type MembershipProofWithdrawalWitness = {
  readonly script: Script;
  readonly amount?: bigint;
};

type CommonBuilderConfig = {
  readonly hubOracleRefInput?: UTxO;
  readonly feeInput?: UTxO;
  readonly referenceScripts?: ReservePayoutReferenceScripts;
  readonly referenceScriptsAddress?: string;
};

export type AbsorbConfirmedDepositConfig = CommonBuilderConfig & {
  readonly deposit: SDK.DepositUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.Proof;
  readonly membershipProofWithdrawal: MembershipProofWithdrawalWitness;
};

export type InitializePayoutConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.Proof;
  readonly membershipProofWithdrawal: MembershipProofWithdrawalWitness;
};

export type AddReserveFundsConfig = CommonBuilderConfig & {
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
};

export type ConcludePayoutConfig = CommonBuilderConfig & {
  readonly payoutInput: UTxO;
};

export type RefundInvalidWithdrawalConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.Proof;
  readonly membershipProofWithdrawal: MembershipProofWithdrawalWitness;
  readonly validityOverride: Exclude<
    SDK.WithdrawalValidity,
    "WithdrawalIsValid"
  >;
};

const encodeHexBytesData = (hex: string): unknown =>
  Data.from(Data.to(hex as any, Data.Bytes()));

const requireNetwork = (
  lucid: LucidEvolution,
): Effect.Effect<Network, ReservePayoutTxError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* fail(
        "Cardano network not found while preparing reserve/payout transaction",
        "Lucid network configuration is undefined",
      );
    }
    return network;
  });

const networkId = (network: Network): number => (network === "Mainnet" ? 1 : 0);

const scriptRewardAddress = (network: Network, script: Script): string => {
  const credential = CML.Credential.new_script(
    CML.ScriptHash.from_hex(validatorToScriptHash(script)),
  );
  return CML.RewardAddress.new(networkId(network), credential)
    .to_address()
    .to_bech32();
};

const credentialFromAddressData = (credential: SDK.CredentialD): Credential => {
  if ("PublicKeyCredential" in credential) {
    return { type: "Key", hash: credential.PublicKeyCredential[0] };
  }
  return { type: "Script", hash: credential.ScriptCredential[0] };
};

const addressDataToBech32 = (
  network: Network,
  address: SDK.AddressData,
): string => {
  const paymentCredential = credentialFromAddressData(
    address.paymentCredential,
  );
  if (address.stakeCredential === null) {
    return credentialToAddress(network, paymentCredential);
  }
  if ("Inline" in address.stakeCredential) {
    return credentialToAddress(
      network,
      paymentCredential,
      credentialFromAddressData(address.stakeCredential.Inline[0]),
    );
  }
  throw new Error(
    "Pointer stake credentials are not supported by node builders",
  );
};

const cardanoDatumToOutputDatum = (
  datum: SDK.CardanoDatum,
): OutputDatum | undefined => {
  if (datum === "NoDatum") {
    return undefined;
  }
  if ("DatumHash" in datum) {
    return {
      kind: "hash",
      value: datum.DatumHash.hash,
    };
  }
  return {
    kind: "inline",
    value: Data.to(datum.InlineDatum.data as any, Data.Any() as any),
  };
};

const payToAddressWithCardanoDatum = (
  tx: TxBuilder,
  address: string,
  datum: SDK.CardanoDatum,
  assets: Assets,
): TxBuilder => {
  const outputDatum = cardanoDatumToOutputDatum(datum);
  return outputDatum === undefined
    ? tx.pay.ToAddress(address, assets)
    : tx.pay.ToAddressWithData(address, outputDatum, assets);
};

const validateHubOracleReferenceProgram = (
  contracts: SDK.MidgardValidators,
  actual: UTxO,
): Effect.Effect<UTxO, ReservePayoutTxError | SDK.Bech32DeserializationError> =>
  Effect.gen(function* () {
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    if ((actual.assets[hubOracleUnit] ?? 0n) !== 1n) {
      return yield* fail("Hub oracle reference UTxO is not authenticated", {
        hubOracleRefInput: outRefLabel(actual),
        unit: hubOracleUnit,
        quantity: (actual.assets[hubOracleUnit] ?? 0n).toString(),
      });
    }
    if (actual.datum === undefined) {
      return yield* fail("Hub oracle reference UTxO has no inline datum", {
        hubOracleRefInput: outRefLabel(actual),
      });
    }
    const expectedDatum = yield* SDK.makeHubOracleDatum(contracts);
    const actualDatum = yield* Effect.try({
      try: () => Data.from(actual.datum!, SDK.HubOracleDatum),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: "Failed to decode hub oracle reference datum",
          cause,
        }),
    });
    const actualDatumCbor = Data.to(actualDatum, SDK.HubOracleDatum);
    const expectedDatumCbor = Data.to(expectedDatum, SDK.HubOracleDatum);
    if (actualDatumCbor !== expectedDatumCbor) {
      return yield* fail(
        "On-chain hub oracle deployment does not match the locally configured contracts",
        {
          expectedDatumCbor,
          actualDatumCbor,
        },
      );
    }
    return actual;
  });

const fetchHubOracleReferenceProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  explicit: UTxO | undefined,
): Effect.Effect<
  UTxO,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
> =>
  Effect.gen(function* () {
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    if (explicit !== undefined) {
      return yield* validateHubOracleReferenceProgram(contracts, explicit);
    }
    const network = yield* requireNetwork(lucid);
    const hubOracleAddress = credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    );
    const hubOracleUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch hub oracle reference UTxO",
          cause,
        }),
    });
    const spendableHubOracleUtxos = hubOracleUtxos.filter((utxo) =>
      isProviderSpendableUtxo(lucid, utxo),
    );
    if (spendableHubOracleUtxos.length !== 1) {
      return yield* fail("Failed to fetch the hub oracle reference UTxO", {
        address: hubOracleAddress,
        unit: hubOracleUnit,
        found: spendableHubOracleUtxos.map(outRefLabel),
      });
    }
    const actual = spendableHubOracleUtxos[0]!;
    return yield* validateHubOracleReferenceProgram(contracts, actual);
  });

const encodeMembershipProofWithdrawalRedeemer = (
  root: string,
  keyCbor: string,
  valueCbor: string,
  proof: SDK.Proof,
): string => {
  const rootData = Data.from(Data.to(root, SDK.MerkleRoot));
  const keyData = encodeHexBytesData(aikenSerialisedPlutusDataCbor(keyCbor));
  const valueData = encodeHexBytesData(
    aikenSerialisedPlutusDataCbor(valueCbor),
  );
  const proofData = Data.from(Data.to(proof, SDK.Proof));
  return Data.to(
    [rootData, keyData, valueData, proofData] as any,
    Data.Array(Data.Any()) as any,
  );
};

const applyEventWitnessUnregistration = (
  tx: TxBuilder,
  network: Network,
  witnessScript: Script,
  redeemer: string,
  referenceScript: UTxO | undefined,
): TxBuilder =>
  attachIfMissing(
    tx.deregister.Stake(scriptRewardAddress(network, witnessScript), redeemer),
    witnessScript,
    referenceScript,
  );

const applyMembershipProofWithdrawal = (
  tx: TxBuilder,
  network: Network,
  witness: MembershipProofWithdrawalWitness,
  redeemer: string,
  referenceScript: UTxO | undefined,
): TxBuilder =>
  (referenceScript === undefined
    ? tx.attach.Script(witness.script)
    : tx
  ).withdraw(
    scriptRewardAddress(network, witness.script),
    witness.amount ?? 0n,
    redeemer,
  );

export const buildAbsorbConfirmedDepositToReserveTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AbsorbConfirmedDepositConfig,
): Effect.Effect<
  BuiltReservePayoutTx<AbsorbDepositLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const witnessScript = SDK.buildUserEventWitnessCertificateValidator(
      config.deposit.assetName,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "deposit minting", script: contracts.deposit.mintingScript },
        { name: "deposit spending", script: contracts.deposit.spendingScript },
        { name: "deposit witness certificate", script: witnessScript },
        {
          name: "membership proof withdrawal",
          script: config.membershipProofWithdrawal.script,
        },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const depositUnit = toUnit(
      contracts.deposit.policyId,
      config.deposit.assetName,
    );
    const reserveAssets = removeAssetUnit(
      config.deposit.utxo.assets,
      depositUnit,
      1n,
    );
    const settlementDatum = settlementDatumFromInput(config.settlementRefInput);
    const membershipRedeemer = encodeMembershipProofWithdrawalRedeemer(
      settlementDatum.deposits_root,
      Data.to(config.deposit.datum.event.id, SDK.OutputReference),
      Data.to(config.deposit.datum.event.info, SDK.DepositInfo),
      config.membershipProof,
    );
    const witnessRedeemer = SDK.encodeUserEventWitnessMintOrBurnRedeemer(
      contracts.deposit.policyId,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.deposit.utxo,
      config.settlementRefInput,
      hubOracleRefInput,
      ...(refs.depositMinting === undefined ? [] : [refs.depositMinting]),
      ...(refs.depositSpending === undefined ? [] : [refs.depositSpending]),
      ...(refs.depositWitnessCertificate === undefined
        ? []
        : [refs.depositWitnessCertificate]),
      ...(refs.membershipProofWithdrawal === undefined
        ? []
        : [refs.membershipProofWithdrawal]),
    ]);
    const txInputs = [config.deposit.utxo, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      config.settlementRefInput,
      refs.depositMinting,
      refs.depositSpending,
      refs.depositWitnessCertificate,
      refs.membershipProofWithdrawal,
    ]);
    const initialLayout = initialAbsorbDepositLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      deposit: config.deposit,
      hubOracleRefInput,
      settlementRefInput: config.settlementRefInput,
    });
    const makeTx = (layout: AbsorbDepositLayout): TxBuilder => {
      const depositSpendRedeemer: SDK.DepositSpendRedeemer = {
        input_index: layout.depositInputIndex,
        output_index: layout.reserveOutputIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
        settlement_ref_input_index: layout.settlementRefInputIndex,
        mint_redeemer_index: layout.burnRedeemerIndex,
        membership_proof: config.membershipProof,
        inclusion_proof_script_withdraw_redeemer_index:
          layout.inclusionProofWithdrawalRedeemerIndex,
      };
      const depositBurnRedeemer: SDK.UserEventMintRedeemer = {
        BurnEventNFT: {
          nonce_asset_name: config.deposit.assetName,
          witness_unregistration_redeemer_index:
            layout.witnessUnregistrationRedeemerIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.deposit.utxo],
          Data.to(depositSpendRedeemer, SDK.DepositSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [depositUnit]: -1n },
          Data.to(depositBurnRedeemer, SDK.UserEventMintRedeemer),
        )
        .pay.ToAddress(contracts.reserve.spendingScriptAddress, reserveAssets);
      tx = applyEventWitnessUnregistration(
        tx,
        network,
        witnessScript,
        witnessRedeemer,
        refs.depositWitnessCertificate,
      );
      tx = applyMembershipProofWithdrawal(
        tx,
        network,
        config.membershipProofWithdrawal,
        membershipRedeemer,
        refs.membershipProofWithdrawal,
      );
      tx = attachIfMissing(
        tx,
        contracts.deposit.spendingScript,
        refs.depositSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.deposit.mintingScript,
        refs.depositMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "deposit absorption",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveAbsorbDepositLayout({
          tx,
          deposit: config.deposit,
          depositUnit,
          reserveAddress: contracts.reserve.spendingScriptAddress,
          reserveAssets,
          hubOracleRefInput,
          settlementRefInput: config.settlementRefInput,
        }),
      sameLayout: sameAbsorbDepositLayout,
    }).pipe(
      Effect.mapError((cause) =>
        cause instanceof ReservePayoutTxError
          ? cause
          : new ReservePayoutTxError({
              message: "Failed to build deposit absorption transaction",
              cause,
            }),
      ),
    );
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(
        `Reserve deposit absorption layout: ${formatLayout(built.layout)}`,
      ),
    ),
  );

export const buildInitializePayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: InitializePayoutConfig,
): Effect.Effect<
  BuiltReservePayoutTx<InitializePayoutLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const witnessScript = SDK.buildUserEventWitnessCertificateValidator(
      config.withdrawal.assetName,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        {
          name: "withdrawal minting",
          script: contracts.withdrawal.mintingScript,
        },
        {
          name: "withdrawal spending",
          script: contracts.withdrawal.spendingScript,
        },
        { name: "payout minting", script: contracts.payout.mintingScript },
        { name: "withdrawal witness certificate", script: witnessScript },
        {
          name: "membership proof withdrawal",
          script: config.membershipProofWithdrawal.script,
        },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const withdrawalUnit = toUnit(
      contracts.withdrawal.policyId,
      config.withdrawal.assetName,
    );
    const payoutUnit = toUnit(
      contracts.payout.policyId,
      config.withdrawal.assetName,
    );
    const payoutAssets = addAssets(
      removeAssetUnit(config.withdrawal.utxo.assets, withdrawalUnit, 1n),
      { [payoutUnit]: 1n },
    );
    const payoutDatum: SDK.PayoutDatum = {
      l2_value: config.withdrawal.datum.event.info.body.l2_value,
      l1_address: config.withdrawal.datum.event.info.body.l1_address,
      l1_datum: config.withdrawal.datum.event.info.body.l1_datum,
    };
    const payoutDatumCbor = Data.to(payoutDatum, SDK.PayoutDatum);
    const initialAccumulatorAssets = removeAssetUnit(
      payoutAssets,
      payoutUnit,
      1n,
    );
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    assertNoAssetExceeds(
      initialAccumulatorAssets,
      targetAssets,
      "Initial payout accumulator",
    );
    const settlementDatum = settlementDatumFromInput(config.settlementRefInput);
    const membershipRedeemer = encodeMembershipProofWithdrawalRedeemer(
      settlementDatum.withdrawals_root,
      Data.to(config.withdrawal.datum.event.id, SDK.OutputReference),
      Data.to(config.withdrawal.datum.event.info, SDK.WithdrawalInfo),
      config.membershipProof,
    );
    const witnessRedeemer = SDK.encodeUserEventWitnessMintOrBurnRedeemer(
      contracts.withdrawal.policyId,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.withdrawal.utxo,
      config.settlementRefInput,
      hubOracleRefInput,
      ...(refs.payoutMinting === undefined ? [] : [refs.payoutMinting]),
      ...(refs.withdrawalMinting === undefined ? [] : [refs.withdrawalMinting]),
      ...(refs.withdrawalSpending === undefined
        ? []
        : [refs.withdrawalSpending]),
      ...(refs.withdrawalWitnessCertificate === undefined
        ? []
        : [refs.withdrawalWitnessCertificate]),
      ...(refs.membershipProofWithdrawal === undefined
        ? []
        : [refs.membershipProofWithdrawal]),
    ]);
    const txInputs = [config.withdrawal.utxo, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      config.settlementRefInput,
      refs.payoutMinting,
      refs.withdrawalMinting,
      refs.withdrawalSpending,
      refs.withdrawalWitnessCertificate,
      refs.membershipProofWithdrawal,
    ]);
    const initialLayout = initialInitializePayoutLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      withdrawal: config.withdrawal,
      hubOracleRefInput,
      settlementRefInput: config.settlementRefInput,
      withdrawalPolicyId: contracts.withdrawal.policyId,
      payoutPolicyId: contracts.payout.policyId,
    });
    const makeTx = (layout: InitializePayoutLayout): TxBuilder => {
      const withdrawalSpendRedeemer: SDK.WithdrawalSpendRedeemer = {
        input_index: layout.withdrawalInputIndex,
        output_index: layout.payoutOutputIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
        settlement_ref_input_index: layout.settlementRefInputIndex,
        burn_redeemer_index: layout.withdrawalBurnRedeemerIndex,
        payout_mint_redeemer_index: layout.payoutMintRedeemerIndex,
        membership_proof: config.membershipProof,
        inclusion_proof_script_withdraw_redeemer_index:
          layout.inclusionProofWithdrawalRedeemerIndex,
        purpose: "InitializePayout",
      };
      const withdrawalBurnRedeemer: SDK.UserEventMintRedeemer = {
        BurnEventNFT: {
          nonce_asset_name: config.withdrawal.assetName,
          witness_unregistration_redeemer_index:
            layout.witnessUnregistrationRedeemerIndex,
        },
      };
      const payoutMintRedeemer: SDK.PayoutMintRedeemer = {
        MintPayout: {
          withdrawal_utxo_out_ref: {
            transactionId: config.withdrawal.utxo.txHash,
            outputIndex: BigInt(config.withdrawal.utxo.outputIndex),
          },
          withdrawal_input_index: layout.withdrawalInputIndex,
          withdrawal_spend_redeemer_index: layout.withdrawalSpendRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.withdrawal.utxo],
          Data.to(withdrawalSpendRedeemer, SDK.WithdrawalSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [withdrawalUnit]: -1n },
          Data.to(withdrawalBurnRedeemer, SDK.UserEventMintRedeemer),
        )
        .mintAssets(
          { [payoutUnit]: 1n },
          Data.to(payoutMintRedeemer, SDK.PayoutMintRedeemer),
        )
        .pay.ToAddressWithData(
          contracts.payout.spendingScriptAddress,
          { kind: "inline", value: payoutDatumCbor },
          payoutAssets,
        );
      tx = applyEventWitnessUnregistration(
        tx,
        network,
        witnessScript,
        witnessRedeemer,
        refs.withdrawalWitnessCertificate,
      );
      tx = applyMembershipProofWithdrawal(
        tx,
        network,
        config.membershipProofWithdrawal,
        membershipRedeemer,
        refs.membershipProofWithdrawal,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.spendingScript,
        refs.withdrawalSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.mintingScript,
        refs.withdrawalMinting,
      );
      tx = attachIfMissing(
        tx,
        contracts.payout.mintingScript,
        refs.payoutMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "payout initialization",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveInitializePayoutLayout({
          tx,
          withdrawal: config.withdrawal,
          payoutAddress: contracts.payout.spendingScriptAddress,
          payoutAssets,
          payoutDatumCbor,
          hubOracleRefInput,
          settlementRefInput: config.settlementRefInput,
          withdrawalPolicyId: contracts.withdrawal.policyId,
          payoutPolicyId: contracts.payout.policyId,
        }),
      sameLayout: sameInitializePayoutLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(
        `Payout initialization layout: ${formatLayout(built.layout)}`,
      ),
    ),
  );

const decodePayoutDatum = (payoutInput: UTxO): SDK.PayoutDatum => {
  if (payoutInput.datum == null) {
    throw new Error(
      `Payout input ${outRefLabel(payoutInput)} has no inline datum`,
    );
  }
  return Data.from(payoutInput.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

const payoutAssetNameFromInput = (
  payoutInput: UTxO,
  payoutPolicyId: string,
): string => {
  const matches = Object.entries(payoutInput.assets).filter(
    ([unit, quantity]) =>
      unit.startsWith(payoutPolicyId) && unit.length >= 56 && quantity === 1n,
  );
  if (matches.length !== 1) {
    throw new Error(
      `Expected payout input ${outRefLabel(
        payoutInput,
      )} to contain exactly one payout NFT for policy ${payoutPolicyId}, found ${matches.length.toString()}`,
    );
  }
  return matches[0]![0].slice(56);
};

export const buildAddReserveFundsToPayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AddReserveFundsConfig,
): Effect.Effect<
  BuiltReservePayoutTx<AddReserveFundsLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const payoutDatum = decodePayoutDatum(config.payoutInput);
    const payoutDatumCbor = config.payoutInput.datum!;
    const payoutAssetName = payoutAssetNameFromInput(
      config.payoutInput,
      contracts.payout.policyId,
    );
    const payoutUnit = toUnit(contracts.payout.policyId, payoutAssetName);
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentPayoutAssets = removeAssetUnit(
      config.payoutInput.assets,
      payoutUnit,
      1n,
    );
    assertNoAssetExceeds(
      currentPayoutAssets,
      targetAssets,
      "Current payout input",
    );
    const neededAssets = subtractAssets(targetAssets, currentPayoutAssets);
    assertAssetsNonNegative(neededAssets, "Payout needed value");
    const takenAssets = minPositiveAssets(
      config.reserveInput.assets,
      neededAssets,
    );
    if (Object.keys(takenAssets).length === 0) {
      return yield* fail(
        "Reserve input does not contribute to any still-needed payout asset",
        {
          reserveInput: outRefLabel(config.reserveInput),
          neededAssets,
        },
      );
    }
    const payoutOutputAssets = addAssets(
      config.payoutInput.assets,
      takenAssets,
    );
    const reserveChangeAssets = subtractAssets(
      config.reserveInput.assets,
      takenAssets,
    );
    assertAssetsNonNegative(reserveChangeAssets, "Reserve change value");
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "reserve spending", script: contracts.reserve.spendingScript },
        { name: "payout spending", script: contracts.payout.spendingScript },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.payoutInput,
      config.reserveInput,
      hubOracleRefInput,
      ...(refs.reserveSpending === undefined ? [] : [refs.reserveSpending]),
      ...(refs.payoutSpending === undefined ? [] : [refs.payoutSpending]),
    ]);
    const txInputs = [config.payoutInput, config.reserveInput, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      refs.reserveSpending,
      refs.payoutSpending,
    ]);
    const initialLayout = initialAddReserveFundsLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      payoutInput: config.payoutInput,
      reserveInput: config.reserveInput,
      hubOracleRefInput,
      reserveChangeAssets,
    });
    const makeTx = (layout: AddReserveFundsLayout): TxBuilder => {
      const payoutSpendRedeemer: SDK.PayoutSpendRedeemer = {
        AddFunds: {
          payout_input_index: layout.payoutInputIndex,
          payout_output_index: layout.payoutOutputIndex,
          reserve_input_index: layout.reserveInputIndex,
          reserve_change_output_index: layout.reserveChangeOutputIndex,
          reserve_spend_redeemer_index: layout.reserveSpendRedeemerIndex,
          payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      const reserveSpendRedeemer: SDK.ReserveSpendRedeemer = {
        reserve_input_index: layout.reserveInputIndex,
        payout_input_index: layout.payoutInputIndex,
        payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.payoutInput],
          Data.to(payoutSpendRedeemer, SDK.PayoutSpendRedeemer),
        )
        .collectFrom(
          [config.reserveInput],
          Data.to(reserveSpendRedeemer, SDK.ReserveSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .pay.ToAddressWithData(
          contracts.payout.spendingScriptAddress,
          { kind: "inline", value: payoutDatumCbor },
          payoutOutputAssets,
        );
      if (hasNonZeroAssetQuantity(reserveChangeAssets)) {
        tx = tx.pay.ToAddress(
          contracts.reserve.spendingScriptAddress,
          reserveChangeAssets,
        );
      }
      tx = attachIfMissing(
        tx,
        contracts.payout.spendingScript,
        refs.payoutSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.reserve.spendingScript,
        refs.reserveSpending,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "reserve funding",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveAddReserveFundsLayout({
          tx,
          payoutInput: config.payoutInput,
          reserveInput: config.reserveInput,
          payoutAddress: contracts.payout.spendingScriptAddress,
          payoutOutputAssets,
          payoutDatumCbor,
          reserveAddress: contracts.reserve.spendingScriptAddress,
          reserveChangeAssets,
          hubOracleRefInput,
        }),
      sameLayout: sameAddReserveFundsLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(`Reserve funding layout: ${formatLayout(built.layout)}`),
    ),
  );

export const buildConcludePayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: ConcludePayoutConfig,
): Effect.Effect<
  BuiltReservePayoutTx<ConcludePayoutLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const payoutDatum = decodePayoutDatum(config.payoutInput);
    const payoutAssetName = payoutAssetNameFromInput(
      config.payoutInput,
      contracts.payout.policyId,
    );
    const payoutUnit = toUnit(contracts.payout.policyId, payoutAssetName);
    const l1Assets = valueToAssets(payoutDatum.l2_value);
    const currentPayoutAssets = removeAssetUnit(
      config.payoutInput.assets,
      payoutUnit,
      1n,
    );
    if (!assetsEqual(currentPayoutAssets, l1Assets)) {
      return yield* fail(
        "Payout input value does not exactly equal the payout datum target",
        {
          payoutInput: outRefLabel(config.payoutInput),
          currentPayoutAssets,
          targetAssets: l1Assets,
        },
      );
    }
    const l1Address = addressDataToBech32(network, payoutDatum.l1_address);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "payout spending", script: contracts.payout.spendingScript },
        { name: "payout minting", script: contracts.payout.mintingScript },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.payoutInput,
      hubOracleRefInput,
      ...(refs.payoutSpending === undefined ? [] : [refs.payoutSpending]),
      ...(refs.payoutMinting === undefined ? [] : [refs.payoutMinting]),
    ]);
    const txInputs = [config.payoutInput, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      refs.payoutSpending,
      refs.payoutMinting,
    ]);
    const initialLayout = initialConcludePayoutLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      payoutInput: config.payoutInput,
      hubOracleRefInput,
    });
    const makeTx = (layout: ConcludePayoutLayout): TxBuilder => {
      const payoutSpendRedeemer: SDK.PayoutSpendRedeemer = {
        ConcludeWithdrawal: {
          payout_input_index: layout.payoutInputIndex,
          l1_output_index: layout.l1OutputIndex,
          burn_redeemer_index: layout.burnRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      const payoutBurnRedeemer: SDK.PayoutMintRedeemer = {
        BurnPayout: {
          payout_input_index: layout.payoutInputIndex,
          payout_asset_name: payoutAssetName,
          payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.payoutInput],
          Data.to(payoutSpendRedeemer, SDK.PayoutSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [payoutUnit]: -1n },
          Data.to(payoutBurnRedeemer, SDK.PayoutMintRedeemer),
        );
      tx = payToAddressWithCardanoDatum(
        tx,
        l1Address,
        payoutDatum.l1_datum,
        l1Assets,
      );
      tx = attachIfMissing(
        tx,
        contracts.payout.spendingScript,
        refs.payoutSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.payout.mintingScript,
        refs.payoutMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "payout conclusion",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveConcludePayoutLayout({
          tx,
          payoutInput: config.payoutInput,
          l1Address,
          l1Datum: payoutDatum.l1_datum,
          l1Assets,
          hubOracleRefInput,
        }),
      sameLayout: sameConcludePayoutLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(`Payout conclusion layout: ${formatLayout(built.layout)}`),
    ),
  );

export const buildRefundInvalidWithdrawalTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: RefundInvalidWithdrawalConfig,
): Effect.Effect<
  BuiltReservePayoutTx<RefundWithdrawalLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const witnessScript = SDK.buildUserEventWitnessCertificateValidator(
      config.withdrawal.assetName,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        {
          name: "withdrawal minting",
          script: contracts.withdrawal.mintingScript,
        },
        {
          name: "withdrawal spending",
          script: contracts.withdrawal.spendingScript,
        },
        { name: "withdrawal witness certificate", script: witnessScript },
        {
          name: "membership proof withdrawal",
          script: config.membershipProofWithdrawal.script,
        },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const withdrawalUnit = toUnit(
      contracts.withdrawal.policyId,
      config.withdrawal.assetName,
    );
    const refundAssets = removeAssetUnit(
      config.withdrawal.utxo.assets,
      withdrawalUnit,
      1n,
    );
    const refundAddress = addressDataToBech32(
      network,
      config.withdrawal.datum.refund_address,
    );
    const settlementDatum = settlementDatumFromInput(config.settlementRefInput);
    const overriddenWithdrawalInfo: SDK.WithdrawalInfo = {
      ...config.withdrawal.datum.event.info,
      validity: config.validityOverride,
    };
    const membershipRedeemer = encodeMembershipProofWithdrawalRedeemer(
      settlementDatum.withdrawals_root,
      Data.to(config.withdrawal.datum.event.id, SDK.OutputReference),
      Data.to(overriddenWithdrawalInfo, SDK.WithdrawalInfo),
      config.membershipProof,
    );
    const witnessRedeemer = SDK.encodeUserEventWitnessMintOrBurnRedeemer(
      contracts.withdrawal.policyId,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.withdrawal.utxo,
      config.settlementRefInput,
      hubOracleRefInput,
      ...(refs.withdrawalMinting === undefined ? [] : [refs.withdrawalMinting]),
      ...(refs.withdrawalSpending === undefined
        ? []
        : [refs.withdrawalSpending]),
      ...(refs.withdrawalWitnessCertificate === undefined
        ? []
        : [refs.withdrawalWitnessCertificate]),
      ...(refs.membershipProofWithdrawal === undefined
        ? []
        : [refs.membershipProofWithdrawal]),
    ]);
    const txInputs = [config.withdrawal.utxo, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      config.settlementRefInput,
      refs.withdrawalMinting,
      refs.withdrawalSpending,
      refs.withdrawalWitnessCertificate,
      refs.membershipProofWithdrawal,
    ]);
    const initialLayout = initialRefundWithdrawalLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      withdrawal: config.withdrawal,
      hubOracleRefInput,
      settlementRefInput: config.settlementRefInput,
    });
    const makeTx = (layout: RefundWithdrawalLayout): TxBuilder => {
      const withdrawalSpendRedeemer: SDK.WithdrawalSpendRedeemer = {
        input_index: layout.withdrawalInputIndex,
        output_index: layout.refundOutputIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
        settlement_ref_input_index: layout.settlementRefInputIndex,
        burn_redeemer_index: layout.burnRedeemerIndex,
        payout_mint_redeemer_index: 0n,
        membership_proof: config.membershipProof,
        inclusion_proof_script_withdraw_redeemer_index:
          layout.inclusionProofWithdrawalRedeemerIndex,
        purpose: {
          Refund: {
            validity_override: config.validityOverride,
          },
        },
      };
      const withdrawalBurnRedeemer: SDK.UserEventMintRedeemer = {
        BurnEventNFT: {
          nonce_asset_name: config.withdrawal.assetName,
          witness_unregistration_redeemer_index:
            layout.witnessUnregistrationRedeemerIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.withdrawal.utxo],
          Data.to(withdrawalSpendRedeemer, SDK.WithdrawalSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [withdrawalUnit]: -1n },
          Data.to(withdrawalBurnRedeemer, SDK.UserEventMintRedeemer),
        );
      tx = payToAddressWithCardanoDatum(
        tx,
        refundAddress,
        config.withdrawal.datum.refund_datum,
        refundAssets,
      );
      tx = applyEventWitnessUnregistration(
        tx,
        network,
        witnessScript,
        witnessRedeemer,
        refs.withdrawalWitnessCertificate,
      );
      tx = applyMembershipProofWithdrawal(
        tx,
        network,
        config.membershipProofWithdrawal,
        membershipRedeemer,
        refs.membershipProofWithdrawal,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.spendingScript,
        refs.withdrawalSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.mintingScript,
        refs.withdrawalMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "invalid withdrawal refund",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveRefundWithdrawalLayout({
          tx,
          withdrawal: config.withdrawal,
          refundAddress,
          refundDatum: config.withdrawal.datum.refund_datum,
          refundAssets,
          hubOracleRefInput,
          settlementRefInput: config.settlementRefInput,
        }),
      sameLayout: sameRefundWithdrawalLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(
        `Invalid withdrawal refund layout: ${formatLayout(built.layout)}`,
      ),
    ),
  );

export const __reservePayoutTest = {
  addAssets,
  assetsToValue,
  assetsEqual,
  encodeMembershipProofWithdrawalRedeemer,
  disposableFeeInputCandidates,
  initialAbsorbDepositLayout,
  initialAddReserveFundsLayout,
  initialConcludePayoutLayout,
  initialInitializePayoutLayout,
  initialRefundWithdrawalLayout,
  aikenSerialisedPlutusDataCbor,
  minPositiveAssets,
  removeAssetUnit,
  selectFeeInputProgram,
  sameAddReserveFundsLayout,
  sameConcludePayoutLayout,
  subtractAssets,
  valueToAssets,
};
