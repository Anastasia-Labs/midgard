import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxBuilder,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type AbsorbConfirmedDepositConfig,
  type AddReserveFundsConfig,
  type InitializePayoutConfig,
  outputWithDatumIndex,
  requireResolvedLayout,
  reserveOutputIndex,
} from "./reserve-payout.address-data-to-bech32.js";
import { buildHistoryRetirementProgram } from "./reserve-payout.build-history-retirement-program.js";
import {
  addAssets,
  assertAssetsNonNegative,
  assertNoAssetExceeds,
  hasNonZeroAssetQuantity,
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
import { selectFeeInputProgram } from "./reserve-payout/inputs.js";
import {
  type AbsorbDepositLayout,
  type AddReserveFundsLayout,
  type InitializePayoutLayout,
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
  requireOwnRedeemerIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
} from "./tx-context-redeemer.js";

export const buildAbsorbConfirmedDepositToReserveTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AbsorbConfirmedDepositConfig,
) =>
  buildHistoryRetirementProgram(
    lucid,
    contracts,
    config,
    config.deposit,
    "AbsorbDeposit",
  ).pipe(
    Effect.map(({ tx, layout }) => ({
      tx,
      layout: {
        ...layout,
        depositInputIndex: layout.witness.order_input_index,
        reserveOutputIndex: layout.witness.funds_output_index,
      } satisfies AbsorbDepositLayout,
    })),
  );

export const buildInitializePayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: InitializePayoutConfig,
) =>
  buildHistoryRetirementProgram(
    lucid,
    contracts,
    config,
    config.withdrawal,
    "InitializeWithdrawalPayout",
  ).pipe(
    Effect.map(({ tx, layout }) => ({
      tx,
      layout: {
        ...layout,
        withdrawalInputIndex: layout.witness.order_input_index,
        payoutOutputIndex: layout.witness.funds_output_index,
        withdrawalBurnRedeemerIndex: layout.burnRedeemerIndex,
        payoutMintRedeemerIndex: layout.payoutMintRedeemerIndex!,
      } satisfies InitializePayoutLayout,
    })),
  );

export const decodePayoutDatum = (payoutInput: UTxO): SDK.PayoutDatum => {
  if (payoutInput.datum == null) {
    throw new Error(
      `Payout input ${outRefLabel(payoutInput)} has no inline datum`,
    );
  }
  return Data.from(payoutInput.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

export const payoutAssetNameFromInput = (
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
      contracts.referenceScriptAuth,
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const feeInput = yield* selectFeeInputProgram(
      lucid,
      config.feeInput,
      [
        config.payoutInput,
        config.reserveInput,
        hubOracleRefInput,
        ...(refs.reserveSpending === undefined ? [] : [refs.reserveSpending]),
        ...(refs.payoutSpending === undefined ? [] : [refs.payoutSpending]),
      ],
      config.walletInputs,
    );
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
      if (config.validTo !== undefined) tx = tx.validTo(config.validTo);
      return tx;
    };
    return yield* completeWithFinalLayoutProgram({
      label: "reserve funding",
      lucid,
      walletInputs: config.walletInputs,
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
