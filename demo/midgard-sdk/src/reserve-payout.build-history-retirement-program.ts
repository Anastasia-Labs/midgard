import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type BuildTxWithRedeemer,
  Constr,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "./cardano-addresses.js";
import { getLinkedListNodeViewFromUTxO } from "./linked-list.js";
import {
  addressDataToBech32,
  HISTORY_RETIREMENT_VALIDITY_RANGE_MS,
  outputHasNoDatum,
  outputWithCardanoDatumIndex,
  outputWithDatumIndex,
  payToAddressWithCardanoDatum,
  requireNetwork,
  requireResolvedLayout,
  type RetirementConfig,
  type RetirementLayout,
  withdrawalPayoutDatumCbor,
} from "./reserve-payout.address-data-to-bech32.js";
import {
  addAssets,
  assertNoAssetExceeds,
  assetsEqual,
  valueToAssets,
} from "./reserve-payout/assets.js";
import { completeWithFinalLayoutProgram } from "./reserve-payout/completion.js";
import {
  fail,
  HistoryRetirementProtectedError,
  ReservePayoutTxError,
} from "./reserve-payout/errors.js";
import { fetchHubOracleReferenceProgram } from "./reserve-payout/hub-reference.js";
import { selectFeeInputProgram } from "./reserve-payout/inputs.js";
import * as SDK from "./reserve-payout/primitives.js";
import {
  attachIfMissing,
  mergeReferenceScripts,
  referenceInputs,
  resolveReferenceScriptsProgram,
} from "./reserve-payout/references.js";
import { getConfirmedStateFromStateQueueDatum } from "./state-queue.js";
import { STATE_QUEUE_ROOT_ASSET_NAME } from "./state-queue.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "./tx-context-redeemer.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryObserve,
  EventHistoryRetirementArgs,
  eventHistoryRetirementOperation,
  type EventHistoryRetirementWitness,
} from "./user-events/history.js";
import {
  eventHistoryDeploymentFromContracts,
  requireEventHistoryContracts,
} from "./user-events/history-deployment.js";
import { historyEventFromPresence } from "./user-events/history-events.js";
import { eventHistoryMinimumOutputLovelace } from "./user-events/history-funding.js";
import {
  authenticateHistoryNodes,
  readEventHistoryOrders,
} from "./user-events/history-query.js";

/** Rebuild from current authenticated nodes. Pointer churn is allowed, immutable
 * event facts and original Value must still match the caller's settlement leaf. */
export const buildHistoryRetirementProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: RetirementConfig,
  requested: SDK.DepositUTxO | SDK.WithdrawalUTxO,
  purpose: EventHistoryRetirementWitness["purpose"],
) =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hub = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const prepared = yield* Effect.tryPromise({
      try: async () => {
        const pair = requireEventHistoryContracts(contracts);
        const history =
          requested.kind === "Deposit" ? pair.deposit : pair.withdrawal;
        const configured =
          requested.kind === "Deposit"
            ? contracts.deposit
            : contracts.withdrawal;
        if (
          history.recipe.kind !== requested.kind ||
          history.recipe.hubPolicyId !== contracts.hubOracle.policyId ||
          history.list.policyId !== configured.policyId ||
          history.list.spendingScriptAddress !==
            configured.spendingScriptAddress
        )
          throw new Error("History recipe differs from configured deployment");
        const deployment = eventHistoryDeploymentFromContracts(history);
        const nodeUtxos = await lucid.utxosAt(deployment.address);
        const retained = await lucid.utxosAt(deployment.retentionAddress);
        const orders = readEventHistoryOrders(nodeUtxos, retained, deployment);
        const presence = orders.find(
          ({ anchor }) => anchor.key === requested.assetName,
        );
        if (presence === undefined)
          throw new Error("Event is no longer an authenticated live Order");
        const event = historyEventFromPresence(presence, deployment);
        if (
          event.kind !== requested.kind ||
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            plutusConstrFieldCbor(event.utxo.datum!, [3, 0]),
          ) !==
            aikenSerialisedPlutusDataCborPreservingMapOrder(
              plutusConstrFieldCbor(requested.utxo.datum!, [3, 0]),
            ) ||
          event.history.payloadCbor !== requested.history.payloadCbor ||
          !assetsEqual(event.originalAssets, requested.originalAssets) ||
          !event.idCbor.equals(requested.idCbor) ||
          !event.infoCbor.equals(requested.infoCbor)
        ) {
          throw new Error(
            "Authenticated event immutable facts or original Value changed",
          );
        }
        const nodes = authenticateHistoryNodes(nodeUtxos, deployment);
        const predecessors = nodes.filter(
          ({ node }) => node.next === event.assetName,
        );
        if (predecessors.length !== 1)
          throw new Error("Event has no unique current predecessor");
        const predecessor = predecessors[0]!;
        const confirmedUnit =
          contracts.stateQueue.policyId + STATE_QUEUE_ROOT_ASSET_NAME;
        const confirmedCandidates =
          config.confirmedRefInput === undefined
            ? await lucid.utxosAtWithUnit(
                contracts.stateQueue.spendingScriptAddress,
                confirmedUnit,
              )
            : await lucid.utxosByOutRef([config.confirmedRefInput]);
        if (
          confirmedCandidates.length !== 1 ||
          confirmedCandidates[0]!.assets[confirmedUnit] !== 1n ||
          confirmedCandidates[0]!.address !==
            contracts.stateQueue.spendingScriptAddress ||
          confirmedCandidates[0]!.datum == null
        )
          throw new Error("Missing authenticated confirmed-state reference");
        const confirmedNode = await Effect.runPromise(
          getLinkedListNodeViewFromUTxO(confirmedCandidates[0]!),
        );
        const confirmedState = await Effect.runPromise(
          getConfirmedStateFromStateQueueDatum(confirmedNode),
        );
        if (
          event.facts.inclusion_time <= 0n ||
          event.facts.inclusion_time > confirmedState.data.endTime
        )
          throw new Error("Event eligibility interval is not confirmed");
        const settlementCandidates = await lucid.utxosByOutRef([
          config.settlementRefInput,
        ]);
        const settlement = settlementCandidates[0];
        if (
          settlementCandidates.length !== 1 ||
          settlement?.datum == null ||
          settlement.address !== contracts.settlement.spendingScriptAddress ||
          Object.entries(settlement.assets).filter(
            ([unit, quantity]) =>
              unit.startsWith(contracts.settlement.policyId) && quantity === 1n,
          ).length !== 1
        )
          throw new Error("Missing current authenticated settlement reference");
        Data.from(settlement.datum, SDK.SettlementDatum);
        const now = config.nowMs ?? Date.now();
        if (!Number.isSafeInteger(now))
          throw new Error("Retirement clock must be a safe integer timestamp");
        const protocolLower =
          predecessor.node.protected_until >
          presence.anchor.node.protected_until
            ? predecessor.node.protected_until
            : presence.anchor.node.protected_until;
        if (protocolLower > BigInt(now))
          throw new HistoryRetirementProtectedError(
            protocolLower,
            now,
            history.recipe.protectionDurationMs,
          );
        const desiredLower = Number(
          protocolLower > BigInt(now - 60_000)
            ? protocolLower
            : BigInt(now - 60_000),
        );
        // Round upward so slot conversion never backdates below protection.
        let lowerSlot = lucid.unixTimeToSlot(desiredLower);
        if (lucid.slotToUnixTime(lowerSlot) < desiredLower) lowerSlot++;
        const validFrom = lucid.slotToUnixTime(lowerSlot);
        const validTo = validFrom + HISTORY_RETIREMENT_VALIDITY_RANGE_MS;
        const upper =
          BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(validTo))) - 1n;
        if (
          upper + history.recipe.protectionDurationMs >
          EVENT_HISTORY_MAX_PROTECTION_TIME
        )
          throw new Error("History protection exceeds funded encoding width");
        const continuedCbor = replacePlutusConstrFieldCbor(
          replacePlutusConstrFieldCbor(
            predecessor.utxo.datum!,
            [1],
            Data.to(
              presence.anchor.node.next === null
                ? new Constr(1, [])
                : new Constr(0, [presence.anchor.node.next]),
            ),
          ),
          [2],
          Data.to(upper + history.recipe.protectionDurationMs),
        );
        return {
          history,
          event,
          predecessor,
          confirmed: confirmedCandidates[0]!,
          settlement,
          validFrom,
          validTo,
          continuedCbor,
        };
      },
      catch: (cause) =>
        new ReservePayoutTxError({
          message: "Failed to resolve authenticated retirement state",
          cause,
        }),
    });
    const {
      history,
      event,
      predecessor,
      confirmed,
      settlement,
      validFrom,
      validTo,
      continuedCbor,
    } = prepared;
    const initialize = purpose === "InitializeWithdrawalPayout";
    let fundsAddress: string;
    let fundsDatum = Data.to("NoDatum", SDK.CardanoDatum);
    let fundsAssets = event.originalAssets;
    if (purpose === "AbsorbDeposit")
      fundsAddress = contracts.reserve.spendingScriptAddress;
    else {
      if (event.kind !== "Withdrawal")
        return yield* fail(
          "Withdrawal retirement requires a withdrawal Order",
          event.kind,
        );
      if (initialize) {
        if (event.event.info.validity !== "WithdrawalIsValid")
          return yield* fail(
            "Payout requires valid withdrawal",
            event.event.info.validity,
          );
        const body = event.event.info.body;
        assertNoAssetExceeds(
          event.originalAssets,
          valueToAssets(body.l2_value),
          "Initial payout accumulator",
        );
        fundsAddress = contracts.payout.spendingScriptAddress;
        fundsAssets = addAssets(event.originalAssets, {
          [contracts.payout.policyId + event.assetName]: 1n,
        });
        const payoutCbor = withdrawalPayoutDatumCbor(event.history.payloadCbor);
        fundsDatum = replacePlutusConstrFieldCbor(
          Data.to({ InlineDatum: { data: 0n } }, SDK.CardanoDatum),
          [0],
          payoutCbor,
        );
      } else {
        fundsAddress = addressDataToBech32(network, event.refundAddress);
        fundsDatum = plutusConstrFieldCbor(event.history.payloadCbor, [2]);
      }
    }
    const refundAddress = credentialToAddress(network, {
      type: "Key",
      hash: event.facts.structural_refund_key,
    });
    const structuralMinimum = eventHistoryMinimumOutputLovelace(
      { lovelace: event.facts.structural_lovelace },
      "NoDatum",
    );
    const structuralRefundAssets = {
      lovelace:
        event.facts.structural_lovelace > structuralMinimum
          ? event.facts.structural_lovelace
          : structuralMinimum,
    };
    const resolved = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        {
          name:
            event.kind === "Deposit"
              ? "deposit spending"
              : "withdrawal spending",
          script: history.list.spendingScript,
        },
        {
          name:
            event.kind === "Deposit"
              ? "deposit history retirement"
              : "withdrawal history retirement",
          script: history.retirement.withdrawalScript,
        },
        ...(initialize
          ? [{ name: "payout minting", script: contracts.payout.mintingScript }]
          : []),
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(config.referenceScripts, resolved);
    const listReference =
      refs.historyList ??
      (event.kind === "Deposit"
        ? refs.depositSpending
        : refs.withdrawalSpending);
    const references = referenceInputs(hub, [
      confirmed,
      settlement,
      event.history.retainedDataUtxo,
      listReference,
      refs.historyRetirement,
      ...(initialize ? [refs.payoutMinting] : []),
    ]);
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      event.utxo,
      predecessor.utxo,
      ...references,
    ]);
    const retirementAddress = scriptRewardAddress(
      network,
      history.retirement.withdrawalScript,
    );
    const listAddress = scriptRewardAddress(
      network,
      history.list.withdrawalScript,
    );
    let layout: RetirementLayout | undefined;
    const resolve = (ctx: RedeemerContext): RetirementLayout => {
      const structuralRefundIndex =
        event.facts.structural_lovelace === 0n
          ? null
          : requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === refundAddress &&
                outputHasNoDatum(output) &&
                output.scriptRef === undefined &&
                assetsEqual(output.assets, structuralRefundAssets),
              "structural refund",
            );
      const witness: EventHistoryRetirementWitness = {
        predecessor_input_index: requireInputIndex(
          ctx,
          predecessor.utxo,
          "retirement predecessor",
        ),
        order_input_index: requireInputIndex(
          ctx,
          event.utxo,
          "retirement Order",
        ),
        predecessor_output_index: outputWithDatumIndex(
          ctx.outputs,
          predecessor.utxo.address,
          continuedCbor,
          predecessor.utxo.assets,
          "continued predecessor",
        ),
        funds_output_index: outputWithCardanoDatumIndex(
          ctx.outputs,
          fundsAddress,
          fundsDatum,
          fundsAssets,
          "retirement funds",
        ),
        structural_refund_output_index: structuralRefundIndex,
        confirmed_reference_index: requireReferenceInputIndex(
          ctx,
          confirmed,
          "confirmed state",
        ),
        settlement_reference_index: requireReferenceInputIndex(
          ctx,
          settlement,
          "settlement",
        ),
        external_reference_index:
          event.history.retainedDataUtxo === undefined
            ? null
            : requireReferenceInputIndex(
                ctx,
                event.history.retainedDataUtxo,
                "retained data",
              ),
        membership: {
          phas_root: config.membershipProof.phas_root,
          count: config.membershipProof.count,
          proof: config.membershipProof.proof,
        },
        purpose,
      };
      layout = {
        witness,
        hubRefInputIndex: requireReferenceInputIndex(ctx, hub, "hub oracle"),
        settlementRefInputIndex: witness.settlement_reference_index,
        retirementWithdrawalRedeemerIndex: requireWithdrawalRedeemerIndex(
          ctx,
          retirementAddress,
          "retirement observer",
        ),
        listWithdrawalRedeemerIndex: requireWithdrawalRedeemerIndex(
          ctx,
          listAddress,
          "list observer",
        ),
        burnRedeemerIndex: requireMintRedeemerIndex(
          ctx,
          history.list.policyId,
          "Order burn",
        ),
        payoutMintRedeemerIndex: initialize
          ? requireMintRedeemerIndex(
              ctx,
              contracts.payout.policyId,
              "payout mint",
            )
          : null,
      };
      return layout;
    };
    const spend =
      (input: UTxO): BuildTxWithRedeemer =>
      (ctx) => {
        requireOwnSpendPurpose(ctx, input, "history node");
        return Data.to(requireInputIndex(ctx, input, "history node"));
      };
    return yield* completeWithFinalLayoutProgram({
      label: "event history retirement",
      lucid,
      walletInputExclusions: [
        event.utxo,
        predecessor.utxo,
        feeInput,
        ...references,
      ],
      resolveLayout: () =>
        requireResolvedLayout(layout, "event history retirement"),
      makeTx: () => {
        let tx = lucid.newTx().readFrom([...references]);
        tx = attachIfMissing(tx, history.list.spendingScript, listReference);
        tx = attachIfMissing(
          tx,
          history.retirement.withdrawalScript,
          refs.historyRetirement,
        );
        tx = tx
          .collectFrom([predecessor.utxo], spend(predecessor.utxo))
          .collectFrom([event.utxo], spend(event.utxo))
          .collectFrom([feeInput])
          .mintAssets(
            { [history.list.policyId + event.assetName]: -1n },
            Data.void(),
          )
          .pay.ToAddressWithData(
            predecessor.utxo.address,
            { kind: "inline", value: continuedCbor },
            predecessor.utxo.assets,
          )
          .withdraw(listAddress, 0n, ((ctx) => {
            const resolved = resolve(ctx);
            return Data.to(
              {
                Apply: {
                  hub_reference_index: resolved.hubRefInputIndex,
                  operation: eventHistoryRetirementOperation(resolved.witness),
                },
              },
              EventHistoryObserve,
            );
          }) satisfies BuildTxWithRedeemer)
          .withdraw(retirementAddress, 0n, ((ctx) => {
            const resolved = resolve(ctx);
            return Data.to(
              {
                hub_reference_index: resolved.hubRefInputIndex,
                witness: resolved.witness,
              },
              EventHistoryRetirementArgs,
            );
          }) satisfies BuildTxWithRedeemer)
          .validFrom(validFrom)
          .validTo(validTo);
        tx = payToAddressWithCardanoDatum(
          tx,
          fundsAddress,
          fundsDatum,
          fundsAssets,
        );
        if (event.facts.structural_lovelace > 0n)
          tx = tx.pay.ToAddress(refundAddress, structuralRefundAssets);
        if (initialize) {
          tx = attachIfMissing(
            tx,
            contracts.payout.mintingScript,
            refs.payoutMinting,
          );
          tx = tx.mintAssets(
            { [contracts.payout.policyId + event.assetName]: 1n },
            ((ctx) => {
              requireOwnMintPurpose(
                ctx,
                contracts.payout.policyId,
                "payout mint",
              );
              const resolved = resolve(ctx);
              return Data.to(
                {
                  MintPayout: {
                    withdrawal_utxo_out_ref: {
                      transactionId: event.utxo.txHash,
                      outputIndex: BigInt(event.utxo.outputIndex),
                    },
                    withdrawal_input_index: resolved.witness.order_input_index,
                    retirement_withdraw_redeemer_index:
                      resolved.retirementWithdrawalRedeemerIndex,
                    hub_ref_input_index: resolved.hubRefInputIndex,
                  },
                },
                SDK.PayoutMintRedeemer,
              );
            }) satisfies BuildTxWithRedeemer,
          );
        }
        return tx;
      },
    });
  });
