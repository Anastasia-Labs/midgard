import * as SDK from "@/reserve-payout/primitives.js";
import {
  type Assets,
  type BuildTxWithRedeemer,
  type Credential,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type OutputDatum,
  type Script,
  type TxOutput,
  scriptHashToCredential,
  toUnit,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "@/cardano-addresses.js";
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
  completeWithFinalLayoutProgram,
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
  type InitializePayoutLayout,
  type RefundWithdrawalLayout,
  settlementDatumFromInput,
} from "@/reserve-payout/layout.js";
import {
  attachIfMissing,
  mergeReferenceScripts,
  referenceInputs,
  type ReservePayoutReferenceScripts,
  resolveReferenceScriptsProgram,
} from "@/reserve-payout/references.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnRedeemerIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSinglePublishRedeemerIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@/tx-context-redeemer.js";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCbor,
  canonicalPlutusDataCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";

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
  const keyData = encodeHexBytesData(keyCbor);
  const valueData = encodeHexBytesData(valueCbor);
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

const outputHasNoDatum = (output: TxOutput): boolean =>
  output.datum == null && output.datumHash == null;

const outputDatumMatches = (
  output: TxOutput,
  datum: SDK.CardanoDatum,
): boolean => {
  if (datum === "NoDatum") {
    return outputHasNoDatum(output);
  }
  if ("DatumHash" in datum) {
    return output.datumHash === datum.DatumHash.hash && output.datum == null;
  }
  return outputDatumCborMatches(
    output,
    Data.to(datum.InlineDatum.data as any, Data.Any() as any),
  );
};

const outputDatumCborMatches = (output: TxOutput, datumCbor: string): boolean =>
  output.datum != null &&
  canonicalPlutusDataCbor(output.datum) === canonicalPlutusDataCbor(datumCbor);

const reserveOutputIndex = (
  outputs: readonly TxOutput[],
  reserveAddress: string,
  reserveAssets: Assets,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === reserveAddress &&
      outputHasNoDatum(output) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, reserveAssets),
    label,
  );

const outputWithDatumIndex = (
  outputs: readonly TxOutput[],
  address: string,
  datumCbor: string,
  assets: Assets,
  label: string,
): bigint => {
  return requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address &&
      outputDatumCborMatches(output, datumCbor) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, assets),
    label,
  );
};

const outputWithCardanoDatumIndex = (
  outputs: readonly TxOutput[],
  address: string,
  datum: SDK.CardanoDatum,
  assets: Assets,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address &&
      outputDatumMatches(output, datum) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, assets),
    label,
  );

const requireResolvedLayout = <L>(layout: L | undefined, label: string): L => {
  if (layout === undefined) {
    throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout.`);
  }
  return layout;
};

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
      config.deposit.idCbor.toString("hex"),
      config.deposit.infoCbor.toString("hex"),
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
    const membershipRewardAddress = scriptRewardAddress(
      network,
      config.membershipProofWithdrawal.script,
    );
    type AbsorbSpendLayout = Omit<
      AbsorbDepositLayout,
      "witnessUnregistrationRedeemerIndex"
    >;
    let absorbSpendLayout: AbsorbSpendLayout | undefined;
    let witnessUnregistrationRedeemerIndex: bigint | undefined;
    const depositSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, config.deposit.utxo, "deposit absorption");
      const layout: AbsorbSpendLayout = {
        depositInputIndex: requireInputIndex(
          ctx,
          config.deposit.utxo,
          "deposit absorption",
        ),
        reserveOutputIndex: reserveOutputIndex(
          ctx.outputs,
          contracts.reserve.spendingScriptAddress,
          reserveAssets,
          "reserve absorption",
        ),
        hubRefInputIndex: requireReferenceInputIndex(
          ctx,
          hubOracleRefInput,
          "deposit absorption hub oracle",
        ),
        settlementRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.settlementRefInput,
          "deposit absorption settlement",
        ),
        burnRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          contracts.deposit.policyId,
          "deposit burn",
        ),
        inclusionProofWithdrawalRedeemerIndex: requireWithdrawalRedeemerIndex(
          ctx,
          membershipRewardAddress,
          "deposit membership proof",
        ),
      };
      absorbSpendLayout = layout;
      return Data.to(
        {
          input_index: layout.depositInputIndex,
          output_index: layout.reserveOutputIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
          settlement_ref_input_index: layout.settlementRefInputIndex,
          mint_redeemer_index: layout.burnRedeemerIndex,
          membership_proof: config.membershipProof,
          inclusion_proof_script_withdraw_redeemer_index:
            layout.inclusionProofWithdrawalRedeemerIndex,
        } satisfies SDK.DepositSpendRedeemer,
        SDK.DepositSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const depositBurnRedeemer = ((ctx) => {
      requireOwnMintPurpose(ctx, contracts.deposit.policyId, "deposit burn");
      witnessUnregistrationRedeemerIndex = requireSinglePublishRedeemerIndex(
        ctx,
        "deposit witness unregistration",
      );
      return Data.to(
        {
          BurnEventNFT: {
            nonce_asset_name: config.deposit.assetName,
            witness_unregistration_redeemer_index:
              witnessUnregistrationRedeemerIndex,
          },
        } satisfies SDK.UserEventMintRedeemer,
        SDK.UserEventMintRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const makeTx = (): TxBuilder => {
      let tx = lucid.newTx().readFrom([...txReferenceInputs]);
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
      tx = attachIfMissing(tx, witnessScript, refs.depositWitnessCertificate);
      tx = attachIfMissing(
        tx,
        config.membershipProofWithdrawal.script,
        refs.membershipProofWithdrawal,
      );
      tx = tx
        .collectFrom([config.deposit.utxo], depositSpendRedeemer)
        .collectFrom([feeInput])
        .mintAssets({ [depositUnit]: -1n }, depositBurnRedeemer)
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
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "deposit absorption",
      lucid,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      resolveLayout: () => ({
        ...requireResolvedLayout(absorbSpendLayout, "deposit absorption"),
        witnessUnregistrationRedeemerIndex: requireResolvedLayout(
          witnessUnregistrationRedeemerIndex,
          "deposit witness unregistration",
        ),
      }),
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
      config.withdrawal.idCbor.toString("hex"),
      config.withdrawal.infoCbor.toString("hex"),
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
    const membershipRewardAddress = scriptRewardAddress(
      network,
      config.membershipProofWithdrawal.script,
    );
    type InitializeSpendLayout = Omit<
      InitializePayoutLayout,
      "withdrawalSpendRedeemerIndex" | "witnessUnregistrationRedeemerIndex"
    >;
    let initializeSpendLayout: InitializeSpendLayout | undefined;
    let withdrawalSpendRedeemerIndex: bigint | undefined;
    let witnessUnregistrationRedeemerIndex: bigint | undefined;
    const withdrawalSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        config.withdrawal.utxo,
        "payout initialization",
      );
      const layout: InitializeSpendLayout = {
        withdrawalInputIndex: requireInputIndex(
          ctx,
          config.withdrawal.utxo,
          "payout initialization",
        ),
        payoutOutputIndex: outputWithDatumIndex(
          ctx.outputs,
          contracts.payout.spendingScriptAddress,
          payoutDatumCbor,
          payoutAssets,
          "payout initialization",
        ),
        hubRefInputIndex: requireReferenceInputIndex(
          ctx,
          hubOracleRefInput,
          "payout initialization hub oracle",
        ),
        settlementRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.settlementRefInput,
          "payout initialization settlement",
        ),
        withdrawalBurnRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          contracts.withdrawal.policyId,
          "withdrawal burn",
        ),
        payoutMintRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          contracts.payout.policyId,
          "payout mint",
        ),
        inclusionProofWithdrawalRedeemerIndex: requireWithdrawalRedeemerIndex(
          ctx,
          membershipRewardAddress,
          "withdrawal membership proof",
        ),
      };
      initializeSpendLayout = layout;
      return Data.to(
        {
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
        } satisfies SDK.WithdrawalSpendRedeemer,
        SDK.WithdrawalSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const withdrawalBurnRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.withdrawal.policyId,
        "withdrawal burn",
      );
      witnessUnregistrationRedeemerIndex = requireSinglePublishRedeemerIndex(
        ctx,
        "withdrawal witness unregistration",
      );
      return Data.to(
        {
          BurnEventNFT: {
            nonce_asset_name: config.withdrawal.assetName,
            witness_unregistration_redeemer_index:
              witnessUnregistrationRedeemerIndex,
          },
        } satisfies SDK.UserEventMintRedeemer,
        SDK.UserEventMintRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const payoutMintRedeemer = ((ctx) => {
      requireOwnMintPurpose(ctx, contracts.payout.policyId, "payout mint");
      withdrawalSpendRedeemerIndex = requireSpendRedeemerIndex(
        ctx,
        config.withdrawal.utxo,
        "payout mint withdrawal",
      );
      return Data.to(
        {
          MintPayout: {
            withdrawal_utxo_out_ref: {
              transactionId: config.withdrawal.utxo.txHash,
              outputIndex: BigInt(config.withdrawal.utxo.outputIndex),
            },
            withdrawal_input_index: requireInputIndex(
              ctx,
              config.withdrawal.utxo,
              "payout mint withdrawal",
            ),
            withdrawal_spend_redeemer_index: withdrawalSpendRedeemerIndex,
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleRefInput,
              "payout mint hub oracle",
            ),
          },
        } satisfies SDK.PayoutMintRedeemer,
        SDK.PayoutMintRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const makeTx = (): TxBuilder => {
      let tx = lucid.newTx().readFrom([...txReferenceInputs]);
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
      tx = attachIfMissing(
        tx,
        witnessScript,
        refs.withdrawalWitnessCertificate,
      );
      tx = attachIfMissing(
        tx,
        config.membershipProofWithdrawal.script,
        refs.membershipProofWithdrawal,
      );
      tx = tx
        .collectFrom([config.withdrawal.utxo], withdrawalSpendRedeemer)
        .collectFrom([feeInput])
        .mintAssets({ [withdrawalUnit]: -1n }, withdrawalBurnRedeemer)
        .mintAssets({ [payoutUnit]: 1n }, payoutMintRedeemer)
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
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "payout initialization",
      lucid,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      resolveLayout: () => ({
        ...requireResolvedLayout(
          initializeSpendLayout,
          "payout initialization",
        ),
        withdrawalSpendRedeemerIndex: requireResolvedLayout(
          withdrawalSpendRedeemerIndex,
          "payout initialization withdrawal spend redeemer",
        ),
        witnessUnregistrationRedeemerIndex: requireResolvedLayout(
          witnessUnregistrationRedeemerIndex,
          "payout initialization witness unregistration",
        ),
      }),
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
    const reserveChangeOutputIndex = (outputs: readonly TxOutput[]) =>
      hasNonZeroAssetQuantity(reserveChangeAssets)
        ? reserveOutputIndex(
            outputs,
            contracts.reserve.spendingScriptAddress,
            reserveChangeAssets,
            "reserve change",
          )
        : null;
    let addReserveFundsLayout: AddReserveFundsLayout | undefined;
    const payoutSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, config.payoutInput, "reserve funding payout");
      const layout: AddReserveFundsLayout = {
        payoutInputIndex: requireInputIndex(
          ctx,
          config.payoutInput,
          "reserve funding payout",
        ),
        payoutOutputIndex: outputWithDatumIndex(
          ctx.outputs,
          contracts.payout.spendingScriptAddress,
          payoutDatumCbor,
          payoutOutputAssets,
          "updated payout",
        ),
        reserveInputIndex: requireInputIndex(
          ctx,
          config.reserveInput,
          "reserve funding reserve",
        ),
        reserveChangeOutputIndex: reserveChangeOutputIndex(ctx.outputs),
        reserveSpendRedeemerIndex: requireSpendRedeemerIndex(
          ctx,
          config.reserveInput,
          "reserve funding reserve",
        ),
        payoutSpendRedeemerIndex: requireOwnRedeemerIndex(
          ctx,
          "reserve funding payout",
        ),
        hubRefInputIndex: requireReferenceInputIndex(
          ctx,
          hubOracleRefInput,
          "reserve funding hub oracle",
        ),
      };
      addReserveFundsLayout = layout;
      return Data.to(
        {
          AddFunds: {
            payout_input_index: layout.payoutInputIndex,
            payout_output_index: layout.payoutOutputIndex,
            reserve_input_index: layout.reserveInputIndex,
            reserve_change_output_index: layout.reserveChangeOutputIndex,
            reserve_spend_redeemer_index: layout.reserveSpendRedeemerIndex,
            payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
            hub_ref_input_index: layout.hubRefInputIndex,
          },
        } satisfies SDK.PayoutSpendRedeemer,
        SDK.PayoutSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const reserveSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        config.reserveInput,
        "reserve funding reserve",
      );
      return Data.to(
        {
          reserve_input_index: requireInputIndex(
            ctx,
            config.reserveInput,
            "reserve funding reserve",
          ),
          payout_input_index: requireInputIndex(
            ctx,
            config.payoutInput,
            "reserve funding payout",
          ),
          payout_spend_redeemer_index: requireSpendRedeemerIndex(
            ctx,
            config.payoutInput,
            "reserve funding payout",
          ),
          hub_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleRefInput,
            "reserve funding hub oracle",
          ),
        } satisfies SDK.ReserveSpendRedeemer,
        SDK.ReserveSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const makeTx = (): TxBuilder => {
      let tx = lucid.newTx().readFrom([...txReferenceInputs]);
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
      tx = tx
        .collectFrom([config.payoutInput], payoutSpendRedeemer)
        .collectFrom([config.reserveInput], reserveSpendRedeemer)
        .collectFrom([feeInput])
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
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "reserve funding",
      lucid,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      resolveLayout: () =>
        requireResolvedLayout(addReserveFundsLayout, "reserve funding"),
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
    let concludePayoutLayout: ConcludePayoutLayout | undefined;
    const payoutSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, config.payoutInput, "payout conclusion");
      const layout: ConcludePayoutLayout = {
        payoutInputIndex: requireInputIndex(
          ctx,
          config.payoutInput,
          "payout conclusion",
        ),
        l1OutputIndex: outputWithCardanoDatumIndex(
          ctx.outputs,
          l1Address,
          payoutDatum.l1_datum,
          l1Assets,
          "payout destination",
        ),
        payoutSpendRedeemerIndex: requireOwnRedeemerIndex(
          ctx,
          "payout conclusion",
        ),
        burnRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          contracts.payout.policyId,
          "payout burn",
        ),
        hubRefInputIndex: requireReferenceInputIndex(
          ctx,
          hubOracleRefInput,
          "payout conclusion hub oracle",
        ),
      };
      concludePayoutLayout = layout;
      return Data.to(
        {
          ConcludeWithdrawal: {
            payout_input_index: layout.payoutInputIndex,
            l1_output_index: layout.l1OutputIndex,
            burn_redeemer_index: layout.burnRedeemerIndex,
            hub_ref_input_index: layout.hubRefInputIndex,
          },
        } satisfies SDK.PayoutSpendRedeemer,
        SDK.PayoutSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const payoutBurnRedeemer = ((ctx) => {
      requireOwnMintPurpose(ctx, contracts.payout.policyId, "payout burn");
      return Data.to(
        {
          BurnPayout: {
            payout_input_index: requireInputIndex(
              ctx,
              config.payoutInput,
              "payout burn",
            ),
            payout_asset_name: payoutAssetName,
            payout_spend_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              config.payoutInput,
              "payout burn",
            ),
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleRefInput,
              "payout burn hub oracle",
            ),
          },
        } satisfies SDK.PayoutMintRedeemer,
        SDK.PayoutMintRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const makeTx = (): TxBuilder => {
      let tx = lucid.newTx().readFrom([...txReferenceInputs]);
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
      tx = tx
        .collectFrom([config.payoutInput], payoutSpendRedeemer)
        .collectFrom([feeInput])
        .mintAssets({ [payoutUnit]: -1n }, payoutBurnRedeemer);
      tx = payToAddressWithCardanoDatum(
        tx,
        l1Address,
        payoutDatum.l1_datum,
        l1Assets,
      );
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "payout conclusion",
      lucid,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      resolveLayout: () =>
        requireResolvedLayout(concludePayoutLayout, "payout conclusion"),
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
    const membershipRewardAddress = scriptRewardAddress(
      network,
      config.membershipProofWithdrawal.script,
    );
    type RefundSpendLayout = Omit<
      RefundWithdrawalLayout,
      "witnessUnregistrationRedeemerIndex"
    >;
    let refundSpendLayout: RefundSpendLayout | undefined;
    let witnessUnregistrationRedeemerIndex: bigint | undefined;
    const withdrawalSpendRedeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, config.withdrawal.utxo, "withdrawal refund");
      const layout: RefundSpendLayout = {
        withdrawalInputIndex: requireInputIndex(
          ctx,
          config.withdrawal.utxo,
          "withdrawal refund",
        ),
        refundOutputIndex: outputWithCardanoDatumIndex(
          ctx.outputs,
          refundAddress,
          config.withdrawal.datum.refund_datum,
          refundAssets,
          "withdrawal refund",
        ),
        hubRefInputIndex: requireReferenceInputIndex(
          ctx,
          hubOracleRefInput,
          "withdrawal refund hub oracle",
        ),
        settlementRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.settlementRefInput,
          "withdrawal refund settlement",
        ),
        burnRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          contracts.withdrawal.policyId,
          "withdrawal refund burn",
        ),
        inclusionProofWithdrawalRedeemerIndex: requireWithdrawalRedeemerIndex(
          ctx,
          membershipRewardAddress,
          "withdrawal refund membership proof",
        ),
      };
      refundSpendLayout = layout;
      return Data.to(
        {
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
        } satisfies SDK.WithdrawalSpendRedeemer,
        SDK.WithdrawalSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const withdrawalBurnRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.withdrawal.policyId,
        "withdrawal refund burn",
      );
      witnessUnregistrationRedeemerIndex = requireSinglePublishRedeemerIndex(
        ctx,
        "withdrawal refund witness unregistration",
      );
      return Data.to(
        {
          BurnEventNFT: {
            nonce_asset_name: config.withdrawal.assetName,
            witness_unregistration_redeemer_index:
              witnessUnregistrationRedeemerIndex,
          },
        } satisfies SDK.UserEventMintRedeemer,
        SDK.UserEventMintRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const makeTx = (): TxBuilder => {
      let tx = lucid.newTx().readFrom([...txReferenceInputs]);
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
        witnessScript,
        refs.withdrawalWitnessCertificate,
      );
      tx = attachIfMissing(
        tx,
        config.membershipProofWithdrawal.script,
        refs.membershipProofWithdrawal,
      );
      tx = tx
        .collectFrom([config.withdrawal.utxo], withdrawalSpendRedeemer)
        .collectFrom([feeInput])
        .mintAssets({ [withdrawalUnit]: -1n }, withdrawalBurnRedeemer);
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
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "invalid withdrawal refund",
      lucid,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      resolveLayout: () => ({
        ...requireResolvedLayout(
          refundSpendLayout,
          "invalid withdrawal refund",
        ),
        witnessUnregistrationRedeemerIndex: requireResolvedLayout(
          witnessUnregistrationRedeemerIndex,
          "invalid withdrawal refund witness unregistration",
        ),
      }),
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
  aikenSerialisedPlutusDataCbor,
  minPositiveAssets,
  removeAssetUnit,
  selectFeeInputProgram,
  subtractAssets,
  valueToAssets,
};
