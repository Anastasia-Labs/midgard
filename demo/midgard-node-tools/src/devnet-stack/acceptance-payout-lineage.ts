import { assetsEqual, normalizeAssets } from "@al-ft/midgard-core/assets";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, datumToHash } from "@lucid-evolution/lucid";
import { addressDataToBech32 } from "midgard-node/commands/withdrawal-utils";

import { decodeAcceptanceTransaction } from "./acceptance-payout-transaction.js";
import {
  type AcceptanceCanonicalTransaction,
  type AcceptanceOutRef,
  acceptanceOutRefKey,
  type AcceptancePayoutConfig,
  type AcceptancePayoutInput,
  type AcceptancePayoutProof,
  type AcceptanceTransaction,
  requireAcceptance,
} from "./acceptance-payout-types.js";

const numberIndex = (index: bigint, count: number) => {
  requireAcceptance(
    index >= 0n && index < BigInt(count),
    "redeemer index outside exact canonical list",
  );
  return Number(index);
};
const redeemer = (
  tx: AcceptanceTransaction,
  tag: CML.RedeemerTag,
  index: number,
) => {
  const key = `${tag}:${index}`;
  const raw = tx.redeemers.get(key);
  requireAcceptance(raw !== undefined, "missing exact purpose/index redeemer");
  return raw!;
};
const redeemerPosition = (
  tx: AcceptanceTransaction,
  tag: CML.RedeemerTag,
  index: number,
) =>
  [...tx.redeemers.keys()]
    .sort((a, b) => {
      const [at, ai] = a.split(":").map(BigInt);
      const [bt, bi] = b.split(":").map(BigInt);
      return at! < bt!
        ? -1
        : at! > bt!
          ? 1
          : ai! < bi!
            ? -1
            : ai! > bi!
              ? 1
              : 0;
    })
    .indexOf(`${tag}:${index}`);
const inputIndex = (tx: AcceptanceTransaction, ref: AcceptanceOutRef) => {
  const index = tx.inputs.findIndex(
    (input) => acceptanceOutRefKey(input) === acceptanceOutRefKey(ref),
  );
  requireAcceptance(
    index >= 0,
    "transaction does not consume exact preceding outref",
  );
  return index;
};
const canonical = (
  evidence: AcceptanceCanonicalTransaction,
  config: AcceptancePayoutConfig,
) => {
  requireAcceptance(
    evidence.canonicalDepth >= config.confirmationDepth,
    "transaction lacks required selected-chain depth",
  );
  return decodeAcceptanceTransaction(
    evidence.observed.transactionCbor,
    evidence.observed.txHash,
    config.maxTransactionBytes,
  );
};
const onePayout = (
  tx: AcceptanceTransaction,
  unit: string,
  config: AcceptancePayoutConfig,
  fields: readonly string[],
) => {
  const indexes = tx.outputs.flatMap((output, index) =>
    output.assets[unit] === undefined ? [] : [index],
  );
  requireAcceptance(
    indexes.length === 1,
    "payout NFT has no unique exact output",
  );
  const index = indexes[0]!;
  const output = tx.outputs[index]!;
  requireAcceptance(
    output.assets[unit] === 1n &&
      output.address === config.payoutAddress &&
      output.datum !== undefined &&
      output.datumHash === undefined &&
      output.scriptRef === undefined,
    "payout NFT address/datum/quantity mismatch",
  );
  Data.from(output.datum!, SDK.PayoutDatum);
  requireAcceptance(
    fields.every(
      (field, index) => plutusConstrFieldCbor(output.datum!, [index]) === field,
    ),
    "payout datum does not preserve exact withdrawal body fields",
  );
  return { txHash: tx.txHash, outputIndex: index };
};
const policyMint = (tx: AcceptanceTransaction, policy: string) =>
  Object.fromEntries(
    Object.entries(tx.mint).filter(([unit]) => unit.startsWith(policy)),
  );

/** Pure verification. Actual inclusion/depth/current generation are supplied by the bounded source owner. */
export const verifyAcceptancePayoutLineage = (
  input: AcceptancePayoutInput,
  config: AcceptancePayoutConfig,
): AcceptancePayoutProof => {
  for (const policy of [config.withdrawalPolicyId, config.payoutPolicyId])
    requireAcceptance(
      /^[0-9a-f]{56}$/u.test(policy),
      "invalid deployment policy",
    );
  requireAcceptance(
    config.confirmationDepth > 0n,
    "invalid confirmation depth",
  );
  requireAcceptance(
    Number.isSafeInteger(config.maxLineageTransactions) &&
      config.maxLineageTransactions >= 2 &&
      input.settlements.length <= config.maxLineageTransactions,
    "lineage exceeds explicit transaction bound",
  );
  const record = input.record;
  const eventId = Data.from(record.withdrawalEventId, SDK.OutputReference);
  const eventKey = datumToHash(Data.to(eventId, SDK.OutputReference));
  const eventRef = {
    txHash: eventId.transactionId,
    outputIndex: Number(eventId.outputIndex),
  };
  requireAcceptance(
    Number.isSafeInteger(eventRef.outputIndex) && eventRef.outputIndex >= 0,
    "invalid event nonce index",
  );
  const order = canonical(input.order, config);
  requireAcceptance(
    order.txHash === record.txHash,
    "journal hash is not original Order transaction",
  );
  inputIndex(order, eventRef);
  const orderUnit = config.withdrawalPolicyId + eventKey;
  const orders = order.outputs.flatMap((output, index) =>
    output.assets[orderUnit] === undefined ? [] : [index],
  );
  requireAcceptance(orders.length === 1, "Order NFT has no unique output");
  const orderIndex = orders[0]!;
  const orderOutput = order.outputs[orderIndex]!;
  requireAcceptance(
    orderOutput.assets[orderUnit] === 1n &&
      orderOutput.address === config.withdrawalAddress &&
      orderOutput.datum !== undefined &&
      orderOutput.datumHash === undefined,
    "Order NFT address/datum/quantity mismatch",
  );
  const node = Data.from(orderOutput.datum!, SDK.EventHistoryNode);
  requireAcceptance(
    typeof node.position === "object" &&
      node.position.Key[0] === eventKey &&
      typeof node.payload === "object" &&
      "Order" in node.payload,
    "Order datum is not exact event node",
  );
  const facts = node.payload.Order.facts;
  requireAcceptance(
    facts.event_id.transactionId === eventId.transactionId &&
      facts.event_id.outputIndex === eventId.outputIndex,
    "Order facts event id mismatch",
  );
  let payloadCbor: string;
  if ("Inline" in facts.location)
    payloadCbor = plutusConstrFieldCbor(orderOutput.datum!, [3, 0, 2, 0]);
  else {
    const external = input.externalDatum;
    requireAcceptance(
      typeof external === "string" &&
        external.length / 2 <= config.maxTransactionBytes &&
        datumToHash(external) === facts.location.External.storage_datum_hash,
      "missing or unbound retained external payload",
    );
    const retained = Data.from(external!, SDK.EventHistoryData);
    requireAcceptance(
      retained.event_key === eventKey,
      "external datum event key mismatch",
    );
    payloadCbor = plutusConstrFieldCbor(external!, [1]);
  }
  const payload = Data.from(payloadCbor, SDK.EventHistoryPayload);
  requireAcceptance(
    "WithdrawalPayload" in payload,
    "Order payload is not Withdrawal",
  );
  const event = payload.WithdrawalPayload.event;
  const body = event.info.body;
  requireAcceptance(
    event.id.transactionId === eventId.transactionId &&
      event.id.outputIndex === eventId.outputIndex &&
      event.info.validity === "WithdrawalIsValid",
    "withdrawal event identity/validity mismatch",
  );
  const assets = normalizeAssets(
    Object.fromEntries(
      Object.entries(record.l2Value).map(([unit, amount]) => {
        requireAcceptance(
          /^(?:0|[1-9][0-9]*)$/u.test(amount),
          "journal value is not a natural decimal",
        );
        return [unit, BigInt(amount)];
      }),
    ),
  );
  requireAcceptance(
    assetsEqual(SDK.valueToAssets(body.l2_value), assets) &&
      addressDataToBech32(config.network, body.l1_address) ===
        record.l1Address &&
      `${body.l2_outref.transactionId}#${body.l2_outref.outputIndex}` ===
        record.l2OutRef,
    "withdrawal body does not match exact journey intent",
  );
  const payoutFields = [2, 3, 4].map((field) =>
    plutusConstrFieldCbor(payloadCbor, [0, 1, 0, field]),
  );
  const payoutUnit = config.payoutPolicyId + eventKey;
  requireAcceptance(
    assets[payoutUnit] === undefined,
    "withdrawal target contains its reserved payout NFT",
  );
  const rows = input.settlements.map((row) => {
    const tx = canonical(row, config);
    decodeAcceptanceTransaction(
      row.signedCbor,
      tx.txHash,
      config.maxTransactionBytes,
    );
    requireAcceptance(
      row.requiredOutputs.length === 1 &&
        Number.isSafeInteger(row.requiredOutputs[0]) &&
        row.requiredOutputs[0]! >= 0,
      "journal has no single exact required output",
    );
    return { row, tx };
  });
  requireAcceptance(
    new Set(rows.map(({ tx }) => tx.txHash)).size === rows.length,
    "duplicate canonical settlement transaction",
  );
  const initial = rows.filter(({ row }) => row.phase === "initialize");
  requireAcceptance(initial.length === 1, "initialize is missing or ambiguous");
  const { row: initRow, tx: init } = initial[0]!;
  const orderRef = { txHash: order.txHash, outputIndex: orderIndex };
  const orderInputIndex = inputIndex(init, orderRef);
  requireAcceptance(
    assetsEqual(policyMint(init, config.payoutPolicyId), { [payoutUnit]: 1n }),
    "initialize does not mint exact sole payout NFT",
  );
  const mintIndex = init.policies.indexOf(config.payoutPolicyId);
  const mint = Data.from(
    redeemer(init, CML.RedeemerTag.Mint, mintIndex),
    SDK.PayoutMintRedeemer,
  );
  requireAcceptance(
    "MintPayout" in mint &&
      mint.MintPayout.withdrawal_input_index === BigInt(orderInputIndex) &&
      mint.MintPayout.withdrawal_utxo_out_ref.transactionId ===
        orderRef.txHash &&
      mint.MintPayout.withdrawal_utxo_out_ref.outputIndex ===
        BigInt(orderIndex),
    "initialize mint does not bind exact Order input",
  );
  let current = onePayout(init, payoutUnit, config, payoutFields);
  requireAcceptance(
    initRow.requiredOutputs[0] === current.outputIndex,
    "initialize journal output index mismatch",
  );
  let currentOutput = init.outputs[current.outputIndex]!;
  const lineage = [
    { phase: "order", txHash: order.txHash },
    { phase: "initialize", txHash: init.txHash },
  ];
  const remaining = rows.filter(({ row }) => row.phase !== "initialize");
  while (remaining.length > 0) {
    const next = remaining.filter(({ tx }) =>
      tx.inputs.some(
        (ref) => acceptanceOutRefKey(ref) === acceptanceOutRefKey(current),
      ),
    );
    requireAcceptance(
      next.length === 1,
      "payout next spend is missing or ambiguous",
    );
    const item = next[0]!;
    remaining.splice(remaining.indexOf(item), 1);
    const { tx, row } = item;
    const index = inputIndex(tx, current);
    const spend = Data.from(
      redeemer(tx, CML.RedeemerTag.Spend, index),
      SDK.PayoutSpendRedeemer,
    );
    if (row.phase === "fund") {
      requireAcceptance("AddFunds" in spend, "fund is not AddFunds");
      const args = spend.AddFunds;
      const outputIndex = numberIndex(
        args.payout_output_index,
        tx.outputs.length,
      );
      requireAcceptance(
        args.payout_input_index === BigInt(index) &&
          args.payout_spend_redeemer_index ===
            BigInt(redeemerPosition(tx, CML.RedeemerTag.Spend, index)),
        "fund redeemer input/self index mismatch",
      );
      requireAcceptance(
        Object.keys(policyMint(tx, config.payoutPolicyId)).length === 0,
        "fund mints or burns payout policy",
      );
      const funded = onePayout(tx, payoutUnit, config, payoutFields);
      requireAcceptance(
        outputIndex === funded.outputIndex &&
          row.requiredOutputs[0] === outputIndex,
        "fund output/redeemer/journal index mismatch",
      );
      current = funded;
      currentOutput = tx.outputs[outputIndex]!;
      lineage.push({ phase: "fund", txHash: tx.txHash });
      continue;
    }
    requireAcceptance(
      row.phase === "conclude" &&
        "ConcludeWithdrawal" in spend &&
        remaining.length === 0,
      "lineage does not terminate with exact conclude",
    );
    const args = spend.ConcludeWithdrawal;
    const outputIndex = numberIndex(args.l1_output_index, tx.outputs.length);
    requireAcceptance(
      args.payout_input_index === BigInt(index) &&
        row.requiredOutputs[0] === outputIndex,
      "conclude input/output/journal index mismatch",
    );
    requireAcceptance(
      assetsEqual(currentOutput.assets, { ...assets, [payoutUnit]: 1n }),
      "conclude input is not exact funded value plus NFT",
    );
    const burnIndex = tx.policies.indexOf(config.payoutPolicyId);
    requireAcceptance(
      assetsEqual(policyMint(tx, config.payoutPolicyId), {
        [payoutUnit]: -1n,
      }) &&
        args.burn_redeemer_index ===
          BigInt(redeemerPosition(tx, CML.RedeemerTag.Mint, burnIndex)),
      "conclude does not burn exact NFT with indexed witness",
    );
    const burn = Data.from(
      redeemer(tx, CML.RedeemerTag.Mint, burnIndex),
      SDK.PayoutMintRedeemer,
    );
    requireAcceptance(
      "BurnPayout" in burn &&
        burn.BurnPayout.payout_input_index === BigInt(index) &&
        burn.BurnPayout.payout_asset_name === eventKey &&
        burn.BurnPayout.payout_spend_redeemer_index ===
          BigInt(redeemerPosition(tx, CML.RedeemerTag.Spend, index)),
      "burn does not bind exact payout spend",
    );
    const output = tx.outputs[outputIndex]!;
    requireAcceptance(
      output.address === record.l1Address &&
        assetsEqual(output.assets, assets) &&
        output.scriptRef === undefined,
      "beneficiary address/full value/reference script mismatch",
    );
    const datum = body.l1_datum;
    let expectedDatum: string | undefined;
    let expectedDatumHash: string | undefined;
    if (typeof datum === "object" && "InlineDatum" in datum)
      expectedDatum = plutusConstrFieldCbor(payloadCbor, [0, 1, 0, 4, 0]);
    if (typeof datum === "object" && "DatumHash" in datum)
      expectedDatumHash = datum.DatumHash.hash;
    requireAcceptance(
      output.datum === expectedDatum && output.datumHash === expectedDatumHash,
      "beneficiary exact datum mismatch",
    );
    lineage.push({ phase: "conclude", txHash: tx.txHash });
    return {
      eventId: record.withdrawalEventId,
      eventKey,
      order: orderRef,
      payout: current,
      beneficiary: { txHash: tx.txHash, outputIndex },
      address: record.l1Address,
      assets,
      ...(expectedDatum === undefined ? {} : { datum: expectedDatum }),
      ...(expectedDatumHash === undefined
        ? {}
        : { datumHash: expectedDatumHash }),
      lineage,
    };
  }
  throw new Error("exact payout: missing canonical conclude transaction");
};
