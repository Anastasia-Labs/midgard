import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  credentialToAddress,
  Data,
  datumToHash,
  type TxOutput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";
import { addressDataToBech32 } from "midgard-node/commands/withdrawal-utils";

import type {
  AcceptanceCanonicalTransaction,
  AcceptanceOutRef,
  AcceptancePayoutConfig,
  AcceptancePayoutInput,
  AcceptanceSettlementTransaction,
} from "../src/devnet-stack/acceptance-payout-types.js";

export const hash = (byte: string) => byte.repeat(32);
const withdrawalPolicy = "31".repeat(28);
const payoutPolicy = "32".repeat(28);
const payment = {
  paymentCredential: { PublicKeyCredential: ["41".repeat(28)] },
  stakeCredential: null,
} satisfies SDK.AddressData;
const withdrawalAddress = credentialToAddress("Custom", {
  type: "Script",
  hash: withdrawalPolicy,
});
const payoutAddress = credentialToAddress("Custom", {
  type: "Script",
  hash: payoutPolicy,
});
export const beneficiaryAddress = addressDataToBech32("Custom", payment);
const feeAddress = credentialToAddress("Custom", {
  type: "Key",
  hash: "42".repeat(28),
});
export const target = {
  lovelace: 9_007_199_254_740_993n,
  ["55".repeat(28) + "ab"]: 9_007_199_254_740_995n,
};
export const config: AcceptancePayoutConfig = {
  network: "Custom",
  withdrawalPolicyId: withdrawalPolicy,
  withdrawalAddress,
  payoutPolicyId: payoutPolicy,
  payoutAddress,
  confirmationDepth: 2160n,
  maxTransactionBytes: 16_384,
  maxLineageTransactions: 8,
};

export const transaction = (
  inputs: readonly AcceptanceOutRef[],
  outputs: readonly TxOutput[],
  mint: Record<string, bigint> = {},
  redeemers: readonly [CML.RedeemerTag, number, string][] = [],
  referenceInputs: readonly AcceptanceOutRef[] = [],
): AcceptanceCanonicalTransaction => {
  const allocated: { free(): void }[] = [];
  const own = <T extends { free(): void }>(value: T) => {
    allocated.push(value);
    return value;
  };
  try {
    const ins = own(CML.TransactionInputList.new());
    for (const input of inputs)
      ins.add(
        own(
          CML.TransactionInput.new(
            own(CML.TransactionHash.from_hex(input.txHash)),
            BigInt(input.outputIndex),
          ),
        ),
      );
    const outs = own(CML.TransactionOutputList.new());
    for (const output of outputs)
      outs.add(
        own(
          typeof output.datumHash !== "string"
            ? utxoToTransactionOutput({
                ...output,
                txHash: hash("00"),
                outputIndex: 0,
              })
            : CML.TransactionOutput.new(
                own(CML.Address.from_bech32(output.address)),
                own(assetsToValue(output.assets)),
                CML.DatumOption.new_hash(
                  own(CML.DatumHash.from_hex(output.datumHash)),
                ),
              ),
        ),
      );
    const body = own(CML.TransactionBody.new(ins, outs, 200_000n));
    if (referenceInputs.length > 0) {
      const references = own(CML.TransactionInputList.new());
      for (const input of referenceInputs)
        references.add(
          own(
            CML.TransactionInput.new(
              own(CML.TransactionHash.from_hex(input.txHash)),
              BigInt(input.outputIndex),
            ),
          ),
        );
      body.set_reference_inputs(references);
    }
    if (Object.keys(mint).length > 0) {
      const values = own(CML.Mint.new());
      for (const [unit, quantity] of Object.entries(mint))
        values.set(
          own(CML.ScriptHash.from_hex(unit.slice(0, 56))),
          own(CML.AssetName.from_hex(unit.slice(56))),
          quantity,
        );
      body.set_mint(values);
    }
    const witness = own(CML.TransactionWitnessSet.new());
    if (redeemers.length > 0) {
      const map = own(CML.MapRedeemerKeyToRedeemerVal.new());
      for (const [tag, index, raw] of redeemers)
        map.insert(
          own(CML.RedeemerKey.new(tag, BigInt(index))),
          own(
            CML.RedeemerVal.new(
              own(CML.PlutusData.from_cbor_hex(raw)),
              own(CML.ExUnits.new(1n, 1n)),
            ),
          ),
        );
      witness.set_redeemers(
        own(CML.Redeemers.new_map_redeemer_key_to_redeemer_val(map)),
      );
    }
    const tx = own(CML.Transaction.new(body, witness, true));
    const txHash = own(CML.hash_transaction(body)).to_hex();
    return {
      canonicalDepth: 2160n,
      observed: {
        txHash,
        spentInputs: inputs,
        referenceInputs,
        transactionIndex: 0,
        transactionCbor: tx.to_cbor_hex(),
        blockPoint: { headerHash: hash("77"), slot: 100, blockNo: 100 },
        selectedChainTip: { id: hash("78"), slot: 5000, height: 2259 },
      },
    };
  } finally {
    for (const item of allocated.reverse()) item.free();
  }
};
const fee = { address: feeAddress, assets: { lovelace: 2_000_000n } };
const step = (
  value: AcceptanceCanonicalTransaction,
  phase: AcceptanceSettlementTransaction["phase"],
  outputIndex: number,
): AcceptanceSettlementTransaction => ({
  ...value,
  phase,
  signedCbor: value.observed.transactionCbor!,
  requiredOutputs: [outputIndex],
});

export const payoutFixture = (
  options: {
    nonceByte?: string;
    external?: boolean;
    orderMoves?: number;
    corruptOrder?: boolean;
    corruptOrderValue?: boolean;
    wrongAddress?: boolean;
    wrongValue?: boolean;
    extraAsset?: boolean;
    wrongMintIndex?: boolean;
    wrongFundIndex?: boolean;
    wrongBurnIndex?: boolean;
    wrongEvent?: boolean;
    datum?: SDK.CardanoDatum;
  } = {},
): AcceptancePayoutInput & {
  externalPublication?: AcceptanceCanonicalTransaction;
} => {
  const nonce = { txHash: hash(options.nonceByte ?? "11"), outputIndex: 2 };
  const eventId = { transactionId: nonce.txHash, outputIndex: 2n };
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: hash("12"), outputIndex: 3n },
    l2_owner: "41".repeat(28),
    l2_value: SDK.assetsToValue(target),
    l1_address: payment,
    l1_datum: options.datum ?? "NoDatum",
  };
  const payload: SDK.EventHistoryPayload = {
    WithdrawalPayload: {
      event: {
        id: eventId,
        info: { body, signature: ["99", "aa"], validity: "WithdrawalIsValid" },
      },
      refund_address: payment,
      refund_datum: "NoDatum",
    },
  };
  const payloadCbor = Data.to(payload, SDK.EventHistoryPayload);
  const eventKey = datumToHash(Data.to(eventId, SDK.OutputReference));
  const externalDatum = Data.to(
    {
      event_key: eventKey,
      event_payload: Data.from(payloadCbor),
      reclaim_auth: { PublicKeyCredential: ["41".repeat(28)] },
    },
    SDK.EventHistoryData,
  );
  const externalPublication = options.external
    ? transaction(
        [{ txHash: hash("66"), outputIndex: Number(eventId.outputIndex) }],
        [
          {
            address: withdrawalAddress,
            assets: { lovelace: 2_000_000n },
            datum: externalDatum,
          },
        ],
      )
    : undefined;
  const node: SDK.EventHistoryNode = {
    position: { Key: [eventKey] },
    next: null,
    protected_until: 0n,
    payload: {
      Order: {
        facts: {
          event_id: eventId,
          inclusion_time: 0n,
          location: options.external
            ? { External: { storage_datum_hash: datumToHash(externalDatum) } }
            : { Inline: { payload } },
          structural_lovelace: 2_000_000n,
          structural_refund_key: "41".repeat(28),
        },
      },
    },
  };
  const order = transaction(
    [nonce],
    [
      {
        address: withdrawalAddress,
        assets: { lovelace: 2_000_000n, [withdrawalPolicy + eventKey]: 1n },
        datum: Data.to(node, SDK.EventHistoryNode),
      },
    ],
    {},
    [],
    externalPublication === undefined
      ? []
      : [{ txHash: externalPublication.observed.txHash, outputIndex: 0 }],
  );
  if (typeof node.payload !== "object" || !("Order" in node.payload))
    throw new Error("fixture Order payload missing");
  const originalFacts = node.payload.Order.facts;
  let orderRef = { txHash: order.observed.txHash, outputIndex: 0 };
  const orderSuccessors: AcceptanceCanonicalTransaction[] = [];
  for (let index = 0; index < (options.orderMoves ?? 0); index++) {
    const moved: SDK.EventHistoryNode = {
      ...node,
      next: hash((index + 20).toString(16).padStart(2, "0")),
      protected_until: BigInt(index + 1),
      ...(options.corruptOrder
        ? {
            payload: {
              Order: {
                facts: { ...originalFacts, inclusion_time: 1n },
              },
            },
          }
        : {}),
    };
    const successor = transaction(
      [orderRef, { txHash: hash("00"), outputIndex: 0 }],
      [
        fee,
        {
          address: withdrawalAddress,
          assets: {
            lovelace: options.corruptOrderValue ? 2_000_001n : 2_000_000n,
            [withdrawalPolicy + eventKey]: 1n,
          },
          datum: Data.to(moved, SDK.EventHistoryNode),
        },
      ],
    );
    orderSuccessors.push(successor);
    orderRef = { txHash: successor.observed.txHash, outputIndex: 1 };
  }
  const unit = payoutPolicy + (options.wrongEvent ? hash("98") : eventKey);
  const payoutDatum = Data.to(
    {
      l2_value: body.l2_value,
      l1_address: body.l1_address,
      l1_datum: body.l1_datum,
    },
    SDK.PayoutDatum,
  );
  const payout = (assets: Record<string, bigint>) => ({
    address: payoutAddress,
    assets: { ...assets, [unit]: 1n },
    datum: payoutDatum,
  });
  const initInput = [orderRef, { txHash: hash("00"), outputIndex: 0 }];
  const initialize = transaction(
    initInput,
    [fee, payout({ lovelace: 2_000_000n })],
    { [unit]: 1n },
    [
      [
        CML.RedeemerTag.Mint,
        0,
        Data.to(
          {
            MintPayout: {
              withdrawal_utxo_out_ref: {
                transactionId: orderRef.txHash,
                outputIndex: BigInt(orderRef.outputIndex),
              },
              withdrawal_input_index: options.wrongMintIndex ? 0n : 1n,
              retirement_withdraw_redeemer_index: 0n,
              hub_ref_input_index: 0n,
            },
          },
          SDK.PayoutMintRedeemer,
        ),
      ],
    ],
  );
  const initRef = { txHash: initialize.observed.txHash, outputIndex: 1 };
  const funds = (
    previous: AcceptanceOutRef,
    assets: Record<string, bigint>,
    wrong = false,
  ) => {
    const inputs = [previous, { txHash: hash("00"), outputIndex: 0 }];
    const value = transaction(inputs, [fee, payout(assets)], {}, [
      [
        CML.RedeemerTag.Spend,
        1,
        Data.to(
          {
            AddFunds: {
              payout_input_index: 1n,
              payout_output_index: wrong ? 0n : 1n,
              reserve_input_index: 0n,
              reserve_change_output_index: null,
              reserve_spend_redeemer_index: 0n,
              payout_spend_redeemer_index: 0n,
              hub_ref_input_index: 0n,
            },
          },
          SDK.PayoutSpendRedeemer,
        ),
      ],
    ]);
    return step(value, "fund", 1);
  };
  const fund1 = funds(
    initRef,
    { lovelace: 4_000_000n },
    options.wrongFundIndex,
  );
  const fund2 = funds(
    { txHash: fund1.observed.txHash, outputIndex: 1 },
    target,
  );
  const last = { txHash: fund2.observed.txHash, outputIndex: 1 };
  const paidAssets = {
    ...target,
    ...(options.wrongValue ? { lovelace: target.lovelace - 1n } : {}),
    ...(options.extraAsset ? { ["56".repeat(28)]: 1n } : {}),
  };
  const datum = body.l1_datum;
  const outputDatum =
    typeof datum === "object" && "InlineDatum" in datum
      ? { datum: Data.to(datum.InlineDatum.data) }
      : typeof datum === "object" && "DatumHash" in datum
        ? { datumHash: datum.DatumHash.hash }
        : {};
  const conclude = transaction(
    [last, { txHash: hash("00"), outputIndex: 0 }],
    [
      fee,
      {
        address: options.wrongAddress ? feeAddress : beneficiaryAddress,
        assets: paidAssets,
        ...outputDatum,
      },
    ],
    { [unit]: -1n },
    [
      [
        CML.RedeemerTag.Spend,
        1,
        Data.to(
          {
            ConcludeWithdrawal: {
              payout_input_index: 1n,
              l1_output_index: 1n,
              burn_redeemer_index: options.wrongBurnIndex ? 0n : 1n,
              hub_ref_input_index: 0n,
            },
          },
          SDK.PayoutSpendRedeemer,
        ),
      ],
      [
        CML.RedeemerTag.Mint,
        0,
        Data.to(
          {
            BurnPayout: {
              payout_input_index: 1n,
              payout_asset_name: eventKey,
              payout_spend_redeemer_index: 0n,
              hub_ref_input_index: 0n,
            },
          },
          SDK.PayoutMintRedeemer,
        ),
      ],
    ],
  );
  return {
    record: {
      user: "userA",
      l2OutRef: `${body.l2_outref.transactionId}#3`,
      l1Address: beneficiaryAddress,
      txHash: order.observed.txHash,
      withdrawalEventId: Data.to(eventId, SDK.OutputReference),
      l2Value: Object.fromEntries(
        Object.entries(target).map(([key, amount]) => [key, amount.toString()]),
      ),
    },
    order,
    orderSuccessors,
    settlements: [
      step(initialize, "initialize", 1),
      fund2,
      step(conclude, "conclude", 1),
      fund1,
    ],
    ...(options.external ? { externalDatum, externalPublication } : {}),
  };
};
