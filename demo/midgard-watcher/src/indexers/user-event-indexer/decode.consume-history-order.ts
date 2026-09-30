import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  ConfirmedState,
  EventHistoryOperation,
  eventHistoryRetirementOperation,
  LinkedListDatum,
  PayoutMintRedeemerSchema,
  SettlementDatumSchema,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import {
  historyCardanoDatumMatches,
  historyListObservation,
  historyNodeFromOutput,
  historyOrderContinuationMatches,
  historyPayloadFromNode,
  historyRawField,
  historyRetirementObservation,
  historyWithdrawalPayoutDatum,
} from ".././authenticated-event-history.js";
import {
  type WatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from ".././user-event-reference-authority.js";
import {
  addressMatchesData,
  canonicalDatumForOutput,
} from "./decode.canonical-datum-for-output.js";
import {
  authenticReferenceDatum,
  countedRootMatches,
  outputValue,
  sameValue,
} from "./decode.counted-root-matches.js";
import {
  canonicalBody,
  dataRoundTrip,
  decodeHubAt,
  exactlyOneAsset,
  inlineDatumCbor,
  matchingRedeemer,
  mintPolicyIndex,
  referencedOutRefAt,
} from "./decode.forced-order-material-field-count.js";
import { isHex28, sha256Bytes } from "./policy.js";
import {
  type EventSchema,
  type WatcherIndexedUserEvent,
  type WatcherUserEventTerminalStatus,
} from "./types.js";

/** Classify a consumed history order using the deployed observer. A consumed
 * predecessor continues the same event; it is never a settlement by itself. */
export const consumeHistoryOrder = (
  event: WatcherIndexedUserEvent,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  references: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  inputIndex: number,
):
  | { continuation: WatcherIndexedUserEvent }
  | { terminalStatus: WatcherUserEventTerminalStatus }
  | null => {
  if (event.kind === "forced_order") return null;
  const body = canonicalBody(transaction.body.bytesHex)!;
  const observed = historyListObservation(transaction, event.policyId);
  const input = CML.TransactionOutput.from_cbor_hex(event.outputCborHex);
  const node = historyNodeFromOutput(input, event.policyId);
  if (
    observed === null ||
    !("Apply" in observed.observe) ||
    node === null ||
    node.node.payload === "RootContent" ||
    !("Order" in node.node.payload)
  ) {
    return null;
  }
  const spend = matchingRedeemer(transaction, "spend", inputIndex);
  if (
    spend === null ||
    dataRoundTrip<bigint>(
      spend.bytes.bytesHex,
      Data.Integer() as EventSchema,
    ) !== BigInt(inputIndex)
  )
    return null;
  const operation = observed.observe.Apply.operation;
  const operationFields = Object.values(operation)[0]!;
  const continuingIndex =
    "predecessor_output_index" in operationFields
      ? operationFields.predecessor_output_index
      : null;
  if (
    continuingIndex !== null &&
    continuingIndex >= 0n &&
    continuingIndex < BigInt(body.outputs().len())
  ) {
    const output = body.outputs().get(Number(continuingIndex));
    if (historyOrderContinuationMatches(input, output, event.policyId)) {
      const datum = canonicalDatumForOutput(
        transaction,
        Number(continuingIndex),
        output,
      );
      if (datum === null) return null;
      const outputCborHex = output.to_cbor_hex();
      return {
        continuation: Object.freeze({
          ...event,
          outRef: `${transaction.txHash}#${continuingIndex}`,
          transactionHash: transaction.txHash,
          outputIndex: String(continuingIndex),
          datumCborHex: datum.cborHex,
          datumDigest: datum.digest,
          outputCborHex,
          outputDigest: sha256Bytes(Buffer.from(outputCborHex, "hex")),
        }),
      };
    }
  }
  if (!("RetireOrder" in operation)) return null;
  const retirementHash =
    deployment.appliedScriptHashes[event.kind + "HistoryRetirementWithdraw"];
  if (!isHex28(retirementHash)) return null;
  const retired = historyRetirementObservation(transaction, retirementHash);
  if (retired === null) return null;
  const { witness, hub_reference_index } = retired.args;
  if (
    witness.order_input_index !== BigInt(inputIndex) ||
    hub_reference_index !== observed.observe.Apply.hub_reference_index ||
    Data.to(eventHistoryRetirementOperation(witness), EventHistoryOperation) !==
      Data.to(operation, EventHistoryOperation)
  )
    return null;
  const hub = decodeHubAt(
    references,
    transaction.txHash,
    body,
    hub_reference_index,
    deployment,
  );
  if (
    hub === null ||
    hub[event.kind] !== event.policyId ||
    !addressMatchesData(input.address(), hub[event.kind + "_addr"])
  )
    return null;
  const facts = node.node.payload.Order.facts;
  const external =
    witness.external_reference_index === null
      ? null
      : watcherUserEventReferenceOutput(
          references,
          transaction.txHash,
          referencedOutRefAt(body, witness.external_reference_index),
        );
  const opened = historyPayloadFromNode(
    node,
    event.kind,
    node.key,
    external,
    deployment.appliedScriptHashes[event.kind + "HistoryRetentionSpend"],
  );
  if (opened === null || event.historyPayloadCborHex !== opened.payloadCbor)
    return null;
  const { payload, payloadCbor } = opened;
  const confirmedOutput = watcherUserEventReferenceOutput(
    references,
    transaction.txHash,
    referencedOutRefAt(body, witness.confirmed_reference_index),
  );
  try {
    if (
      confirmedOutput === null ||
      !isHex28(hub.state_queue) ||
      !addressMatchesData(confirmedOutput.address(), hub.state_queue_addr) ||
      confirmedOutput.script_ref() !== undefined ||
      exactlyOneAsset(confirmedOutput, hub.state_queue)?.assetNameHex !==
        Buffer.from("MIDGARD_CONFIRMED_STATE").toString("hex") ||
      exactlyOneAsset(confirmedOutput, hub.state_queue)?.quantity !== 1n
    )
      return null;
    const confirmedNode = Data.from(
      inlineDatumCbor(confirmedOutput)!,
      LinkedListDatum,
    );
    if (!("Root" in confirmedNode.data)) return null;
    const confirmed = Data.castFrom(
      confirmedNode.data.Root.data,
      ConfirmedState,
    );
    if (facts.inclusion_time <= 0n || facts.inclusion_time > confirmed.endTime)
      return null;
  } catch {
    return null;
  }
  const settlement = isHex28(hub.settlement)
    ? authenticReferenceDatum(
        references,
        transaction.txHash,
        body,
        witness.settlement_reference_index,
        hub.settlement,
        asDataType<EventSchema>(SettlementDatumSchema),
      )
    : null;
  const root =
    settlement?.datum[
      event.kind === "deposit" ? "deposits_root" : "withdrawals_root"
    ];
  if (
    settlement === null ||
    !addressMatchesData(settlement.output.address(), hub.settlement_addr) ||
    settlement.output.script_ref() !== undefined ||
    !countedRootMatches(
      {
        ...witness.membership,
        domain:
          event.kind === "deposit"
            ? "DepositsRootDomain"
            : "WithdrawalsRootDomain",
        root: root as string,
        key: "",
        value: "",
      },
      event.kind === "deposit" ? "DepositsRootDomain" : "WithdrawalsRootDomain",
      root,
    )
  )
    return null;
  const mint = body.mint()?.get_assets(CML.ScriptHash.from_hex(event.policyId));
  if (
    mint?.len() !== 1 ||
    mint.get(CML.AssetName.from_hex(event.assetNameHex)) !== -1n ||
    witness.funds_output_index < 0n ||
    witness.funds_output_index >= BigInt(body.outputs().len()) ||
    witness.funds_output_index === witness.predecessor_output_index
  )
    return null;
  const output = body.outputs().get(Number(witness.funds_output_index));
  const original = new Map(outputValue(input));
  original.delete(event.policyId + event.assetNameHex);
  original.set(
    "lovelace",
    (original.get("lovelace") ?? 0n) - facts.structural_lovelace,
  );
  if (
    (original.get("lovelace") ?? -1n) < 0n ||
    output.script_ref() !== undefined
  )
    return null;
  if (facts.structural_lovelace === 0n) {
    if (witness.structural_refund_output_index !== null) return null;
  } else {
    const index = witness.structural_refund_output_index;
    if (
      index === null ||
      index < 0n ||
      index >= BigInt(body.outputs().len()) ||
      index === witness.funds_output_index ||
      index === witness.predecessor_output_index
    )
      return null;
    const refund = body.outputs().get(Number(index));
    if (
      CML.EnterpriseAddress.from_address(refund.address()) === undefined ||
      refund.address().payment_cred()?.as_pub_key()?.to_hex() !==
        facts.structural_refund_key ||
      refund.datum() !== undefined ||
      refund.script_ref() !== undefined ||
      refund.amount().has_multiassets() ||
      refund.amount().coin() < facts.structural_lovelace
    )
      return null;
  }
  if (witness.purpose === "AbsorbDeposit")
    return event.kind === "deposit" &&
      "DepositPayload" in payload &&
      addressMatchesData(output.address(), hub.reserve_addr) &&
      output.datum() === undefined &&
      sameValue(original, outputValue(output))
      ? { terminalStatus: "absorbed" }
      : null;
  if (event.kind !== "withdrawal" || !("WithdrawalPayload" in payload))
    return null;
  const withdrawal = payload.WithdrawalPayload;
  if (typeof witness.purpose === "object")
    return witness.purpose.RefundInvalidWithdrawal.validity !==
      "WithdrawalIsValid" &&
      addressMatchesData(output.address(), withdrawal.refund_address) &&
      historyCardanoDatumMatches(output, historyRawField(payloadCbor, [2])) &&
      sameValue(original, outputValue(output))
      ? { terminalStatus: "refunded" }
      : null;
  if (witness.purpose !== "InitializeWithdrawalPayout" || !isHex28(hub.payout))
    return null;
  const payoutIndex =
    body.mint() === undefined ? -1 : mintPolicyIndex(body.mint()!, hub.payout);
  const payoutRedeemer =
    payoutIndex < 0 ? null : matchingRedeemer(transaction, "mint", payoutIndex);
  const payout =
    payoutRedeemer === null
      ? null
      : dataRoundTrip<{
          MintPayout: {
            withdrawal_utxo_out_ref: {
              transactionId: string;
              outputIndex: bigint;
            };
            withdrawal_input_index: bigint;
            retirement_withdraw_redeemer_index: bigint;
            hub_ref_input_index: bigint;
          };
        }>(
          payoutRedeemer.bytes.bytesHex,
          asDataType<EventSchema>(PayoutMintRedeemerSchema),
        );
  if (
    payout === null ||
    !("MintPayout" in payout) ||
    payout.MintPayout.retirement_withdraw_redeemer_index !==
      BigInt(retired.globalIndex) ||
    payout.MintPayout.withdrawal_input_index !== BigInt(inputIndex) ||
    payout.MintPayout.hub_ref_input_index !== hub_reference_index ||
    payout.MintPayout.withdrawal_utxo_out_ref.transactionId !==
      event.transactionHash ||
    payout.MintPayout.withdrawal_utxo_out_ref.outputIndex !==
      BigInt(event.outputIndex) ||
    withdrawal.event.info.validity !== "WithdrawalIsValid" ||
    !addressMatchesData(output.address(), hub.payout_addr)
  )
    return null;
  original.set(hub.payout + event.assetNameHex, 1n);
  const payoutAssets = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(hub.payout));
  const datum = inlineDatumCbor(output);
  return payoutAssets?.len() === 1 &&
    payoutAssets.get(CML.AssetName.from_hex(event.assetNameHex)) === 1n &&
    sameValue(original, outputValue(output)) &&
    datum !== null &&
    historyRawField(datum, []) === historyWithdrawalPayoutDatum(payloadCbor)
    ? { terminalStatus: "payout_initialized" }
    : null;
};
