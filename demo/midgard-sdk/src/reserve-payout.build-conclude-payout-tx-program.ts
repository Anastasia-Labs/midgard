import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCbor,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  addressDataToBech32,
  cardanoDatumCborToOutputDatum,
  type ConcludePayoutConfig,
  encodeMembershipProofWithdrawalRedeemer,
  outputDatumMatches,
  outputWithCardanoDatumIndex,
  payToAddressWithCardanoDatum,
  type RefundInvalidWithdrawalConfig,
  requireNetwork,
  requireResolvedLayout,
  withdrawalPayoutDatumCbor,
} from "./reserve-payout.address-data-to-bech32.js";
import {
  decodePayoutDatum,
  payoutAssetNameFromInput,
} from "./reserve-payout.build-add-reserve-funds-to-payout-tx-program.js";
import { buildHistoryRetirementProgram } from "./reserve-payout.build-history-retirement-program.js";
import {
  addAssets,
  assetsEqual,
  assetsToValue,
  minPositiveAssets,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "./reserve-payout/assets.js";
import {
  type BuiltReservePayoutTx,
  completeWithFinalLayoutProgram,
} from "./reserve-payout/completion.js";
import { formatLayout } from "./reserve-payout/diagnostics.js";
import { fail, ReservePayoutTxError } from "./reserve-payout/errors.js";
import { fetchHubOracleReferenceProgram } from "./reserve-payout/hub-reference.js";
import {
  disposableFeeInputCandidates,
  selectFeeInputProgram,
} from "./reserve-payout/inputs.js";
import {
  type ConcludePayoutLayout,
  type RefundWithdrawalLayout,
} from "./reserve-payout/layout.js";
import * as SDK from "./reserve-payout/primitives.js";
import {
  attachIfMissing,
  mergeReferenceScripts,
  referenceInputs,
  resolveReferenceScriptsProgram,
} from "./reserve-payout/references.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnRedeemerIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
} from "./tx-context-redeemer.js";

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
      contracts.referenceScriptAuth,
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
          plutusConstrFieldCbor(config.payoutInput.datum!, [2]),
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
        plutusConstrFieldCbor(config.payoutInput.datum!, [2]),
        l1Assets,
      );
      if (config.validTo !== undefined) tx = tx.validTo(config.validTo);
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
) =>
  buildHistoryRetirementProgram(lucid, contracts, config, config.withdrawal, {
    RefundInvalidWithdrawal: { validity: config.validityOverride },
  }).pipe(
    Effect.map(({ tx, layout }) => ({
      tx,
      layout: {
        ...layout,
        withdrawalInputIndex: layout.witness.order_input_index,
        refundOutputIndex: layout.witness.funds_output_index,
      } satisfies RefundWithdrawalLayout,
    })),
  );

export const __reservePayoutTest = {
  cardanoDatumCborToOutputDatum,
  payToAddressWithCardanoDatum,
  outputDatumMatches,
  outputWithCardanoDatumIndex,
  withdrawalPayoutDatumCbor,
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
