import {
  encodeEventHistoryData,
  EventHistoryData,
  eventHistoryDataHash,
  EventHistoryNode,
  EventHistoryObserve,
  EventHistoryPayload,
  EventHistoryRetirementArgs,
  OutputReference,
} from "@al-ft/midgard-sdk";
import { CML, Data, datumToHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  exactHistoryWithdrawal,
  historyCardanoDatumMatches,
  historyListObservation,
  historyNodeFromOutput,
  historyOrderContinuationMatches,
  historyPayloadFromNode,
  historyRawField,
  historyRetirementObservation,
  historyWithdrawalPayoutDatum,
} from "../../src/indexers/authenticated-event-history.js";
import type { WatcherNormalizedL1Block } from "../../src/l1/l1-adapter.js";

const policy = "11".repeat(28);
const retirement = "22".repeat(28);
const retention = "33".repeat(28);
const id = { transactionId: "44".repeat(32), outputIndex: 0n };
const key = datumToHash(Data.to(id, OutputReference));
const owner = "55".repeat(28);
const payload: EventHistoryPayload = {
  DepositPayload: {
    event: {
      id,
      info: {
        l2_address: {
          paymentCredential: { PublicKeyCredential: [owner] },
          stakeCredential: null,
        },
        l2_network_id: 0n,
        l2_datum: null,
      },
    },
  },
};
const node = (): EventHistoryNode => ({
  position: { Key: [key] },
  next: null,
  protected_until: 100n,
  payload: {
    Order: {
      facts: {
        event_id: id,
        inclusion_time: 60n,
        location: { Inline: { payload } },
        structural_lovelace: 2_000_000n,
        structural_refund_key: owner,
      },
    },
  },
});
const output = (datum: EventHistoryNode, coin = 12_000_000n) => {
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(policy),
    CML.AssetName.from_hex(
      datum.position === "Root" ? "" : datum.position.Key[0],
    ),
    1n,
  );
  const value = CML.Value.new(coin, assets);
  return CML.TransactionOutput.new(
    CML.Address.from_hex(`70${policy}`),
    value,
    CML.DatumOption.new_datum(
      CML.PlutusData.from_cbor_hex(Data.to(datum, EventHistoryNode)),
    ),
  );
};
type Transaction = WatcherNormalizedL1Block["transactions"][number];
// Pure decoder fixtures: these bytes deliberately confer no chain authority.
const transaction = (
  hash: string,
  cbor: string,
  amount = 0n,
  purpose = "withdrawal",
): Transaction => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(id.transactionId),
      0n,
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output(node()));
  const body = CML.TransactionBody.new(inputs, outputs, 0n);
  const withdrawals = CML.MapRewardAccountToCoin.new();
  withdrawals.insert(
    CML.RewardAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex(hash)),
    ),
    amount,
  );
  body.set_withdrawals(withdrawals);
  return {
    body: { bytesHex: body.to_cbor_hex() },
    redeemers: [{ purpose, index: "0", bytes: { bytesHex: cbor } }],
  } as unknown as Transaction;
};

describe("authenticated history transaction decoding", () => {
  it("keeps immutable order facts and original Value through repeated pointer churn", () => {
    let previous = output(node());
    for (let index = 0; index < 12; index++) {
      const changed = node();
      changed.next = "ff".repeat(32);
      changed.protected_until += BigInt(index + 1);
      const next = output(changed);
      expect(historyOrderContinuationMatches(previous, next, policy)).toBe(
        true,
      );
      previous = next;
    }
    const changedFacts = node();
    if (
      changedFacts.payload !== "RootContent" &&
      "Order" in changedFacts.payload
    )
      changedFacts.payload.Order.facts.inclusion_time += 1n;
    expect(
      historyOrderContinuationMatches(previous, output(changedFacts), policy),
    ).toBe(false);
    expect(
      historyOrderContinuationMatches(
        previous,
        output(node(), 11_000_000n),
        policy,
      ),
    ).toBe(false);
  });

  it("distinguishes root/filler nodes and exact-key promotion from order continuations", () => {
    const filler: EventHistoryNode = {
      position: { Key: [key] },
      next: null,
      protected_until: 0n,
      payload: { Filler: { refund_key: owner } },
    };
    const root: EventHistoryNode = {
      position: "Root",
      next: key,
      protected_until: 0n,
      payload: "RootContent",
    };
    expect(historyNodeFromOutput(output(root), policy)?.key).toBe("");
    expect(historyNodeFromOutput(output(filler), policy)?.node.payload).toEqual(
      filler.payload,
    );
    expect(
      historyOrderContinuationMatches(output(filler), output(node()), policy),
    ).toBe(false);
    expect(
      historyPayloadFromNode(
        { node: filler, cbor: Data.to(filler, EventHistoryNode) },
        "deposit",
        key,
        null,
        retention,
      ),
    ).toBeNull();
  });

  it("requires the exact script zero withdrawal, purpose and redeemer index", () => {
    const observe: EventHistoryObserve = {
      Apply: {
        hub_reference_index: 0n,
        operation: {
          InsertOrder: {
            predecessor_input_index: 0n,
            predecessor_output_index: 0n,
            order_output_index: 1n,
            nonce_input_index: 1n,
            external_reference_index: null,
          },
        },
      },
    };
    const cbor = Data.to(observe, EventHistoryObserve);
    const tx = transaction(policy, cbor);
    expect(historyListObservation(tx, policy)?.observe).toEqual(observe);
    expect(exactHistoryWithdrawal(tx, retirement)).toBeNull();
    expect(
      exactHistoryWithdrawal(transaction(policy, cbor, 1n), policy),
    ).toBeNull();
    expect(
      exactHistoryWithdrawal(transaction(policy, cbor, 0n, "spend"), policy),
    ).toBeNull();
    expect(
      exactHistoryWithdrawal(
        { ...tx, redeemers: [...tx.redeemers, ...tx.redeemers] },
        policy,
      ),
    ).toBeNull();
  });

  it("decodes retirement only from its exact observer, never a legacy Spend", () => {
    const args: EventHistoryRetirementArgs = {
      hub_reference_index: 0n,
      witness: {
        predecessor_input_index: 0n,
        order_input_index: 1n,
        predecessor_output_index: 0n,
        funds_output_index: 1n,
        structural_refund_output_index: 2n,
        confirmed_reference_index: 1n,
        settlement_reference_index: 2n,
        external_reference_index: null,
        membership: { phas_root: "66".repeat(32), count: 1n, proof: [] },
        purpose: "AbsorbDeposit",
      },
    };
    const cbor = Data.to(args, EventHistoryRetirementArgs);
    expect(
      historyRetirementObservation(transaction(retirement, cbor), retirement)
        ?.args,
    ).toEqual(args);
    expect(
      historyRetirementObservation(transaction(policy, cbor), retirement),
    ).toBeNull();
    expect(
      historyRetirementObservation(
        transaction(retirement, cbor, 0n, "spend"),
        retirement,
      ),
    ).toBeNull();
  });

  it("opens external payload only at its retained reference and content hash", () => {
    const stored: EventHistoryData = {
      event_key: key,
      event_payload: Data.from(Data.to(payload, EventHistoryPayload)),
      reclaim_auth: { PublicKeyCredential: [owner] },
    };
    const externalNode = node();
    if (
      externalNode.payload === "RootContent" ||
      !("Order" in externalNode.payload)
    )
      throw new Error("fixture");
    externalNode.payload.Order.facts.location = {
      External: { storage_datum_hash: eventHistoryDataHash(stored) },
    };
    const external = CML.TransactionOutput.new(
      CML.Address.from_hex(`70${retention}`),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(encodeEventHistoryData(stored)),
      ),
    );
    expect(
      historyPayloadFromNode(
        { node: externalNode, cbor: Data.to(externalNode, EventHistoryNode) },
        "deposit",
        key,
        external,
        retention,
      ),
    ).toMatchObject({ payload });
    expect(
      historyPayloadFromNode(
        { node: externalNode, cbor: Data.to(externalNode, EventHistoryNode) },
        "deposit",
        key,
        null,
        retention,
      ),
    ).toBeNull();
    expect(
      historyPayloadFromNode(
        { node: externalNode, cbor: Data.to(externalNode, EventHistoryNode) },
        "deposit",
        key,
        external,
        policy,
      ),
    ).toBeNull();
    expect(
      historyPayloadFromNode(
        { node: externalNode, cbor: Data.to(externalNode, EventHistoryNode) },
        "withdrawal",
        key,
        external,
        retention,
      ),
    ).toBeNull();
  });
});

describe("raw authenticated history maps", () => {
  const marker = "fe".repeat(40);
  const arbitrary = "a302a20102010301000102";
  const changedDuplicate = "a302a20102010901000102";
  const rawOutput = (value: EventHistoryNode, datum: string) => {
    const base = output(value);
    return CML.TransactionOutput.new(
      base.address(),
      base.amount(),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    );
  };
  it.each(["deposit", "withdrawal"] as const)(
    "preserves %s inline/retained payload and distinguishes collapsed duplicate facts",
    (kind) => {
      const value = node();
      if (value.payload === "RootContent" || !("Order" in value.payload))
        throw new Error("fixture");
      const eventPayload: EventHistoryPayload =
        kind === "deposit"
          ? {
              DepositPayload: {
                event: {
                  ...payload.DepositPayload.event,
                  info: {
                    ...payload.DepositPayload.event.info,
                    l2_datum: marker,
                  },
                },
              },
            }
          : {
              WithdrawalPayload: {
                event: {
                  id,
                  info: {
                    body: {
                      l2_outref: id,
                      l2_owner: owner,
                      l2_value: new Map(),
                      l1_address: payload.DepositPayload.event.info.l2_address,
                      l1_datum: { InlineDatum: { data: marker } },
                    },
                    signature: ["aa", "bb"],
                    validity: "WithdrawalIsValid",
                  },
                },
                refund_address: payload.DepositPayload.event.info.l2_address,
                refund_datum: { InlineDatum: { data: marker } },
              },
            };
      value.payload.Order.facts.location = {
        Inline: { payload: eventPayload },
      };
      const encoded = Data.to(value, EventHistoryNode).replaceAll(
        Data.to(marker),
        arbitrary,
      );
      const before = rawOutput(value, encoded);
      const authenticated = historyNodeFromOutput(before, policy)!;
      const opened = historyPayloadFromNode(
        authenticated,
        kind,
        key,
        null,
        retention,
      )!;
      expect(opened.payloadCbor).toContain(arbitrary);
      expect(opened.eventCbor).toContain(arbitrary);
      const altered = encoded.replaceAll(arbitrary, changedDuplicate);
      expect(
        Data.to(Data.from(encoded, EventHistoryNode), EventHistoryNode),
      ).toBe(Data.to(Data.from(altered, EventHistoryNode), EventHistoryNode));
      expect(
        historyOrderContinuationMatches(
          before,
          rawOutput(value, altered),
          policy,
        ),
      ).toBe(false);
      value.next = "ff".repeat(32);
      value.protected_until += 1n;
      const continuing = Data.to(value, EventHistoryNode).replaceAll(
        Data.to(marker),
        arbitrary,
      );
      expect(
        historyOrderContinuationMatches(
          before,
          rawOutput(value, continuing),
          policy,
        ),
      ).toBe(true);
      const stored = Data.to(
        {
          event_key: key,
          event_payload: marker,
          reclaim_auth: { PublicKeyCredential: [owner] },
        },
        EventHistoryData,
      ).replace(Data.to(marker), opened.payloadCbor);
      value.payload.Order.facts.location = {
        External: {
          storage_datum_hash: datumToHash(historyRawField(stored, [])),
        },
      };
      const externalNode = historyNodeFromOutput(output(value), policy)!;
      const external = CML.TransactionOutput.new(
        CML.Address.from_hex(`70${retention}`),
        CML.Value.from_coin(2_000_000n),
        CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(stored)),
      );
      expect(
        historyPayloadFromNode(externalNode, kind, key, external, retention)
          ?.payloadCbor,
      ).toBe(opened.payloadCbor);
      expect(
        historyPayloadFromNode(externalNode, kind, key, null, retention),
      ).toBeNull();
      const substituted = CML.TransactionOutput.new(
        external.address(),
        external.amount(),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(
            stored.replaceAll(arbitrary, changedDuplicate),
          ),
        ),
      );
      expect(
        historyPayloadFromNode(externalNode, kind, key, substituted, retention),
      ).toBeNull();
      if (kind === "withdrawal") {
        const payout = historyWithdrawalPayoutDatum(opened.payloadCbor);
        expect(historyRawField(payout, [2, 0])).toBe(arbitrary);
        const rawRefund = historyRawField(opened.payloadCbor, [2]);
        expect(
          historyCardanoDatumMatches(
            CML.TransactionOutput.new(
              before.address(),
              before.amount(),
              CML.DatumOption.new_datum(
                CML.PlutusData.from_cbor_hex(arbitrary),
              ),
            ),
            rawRefund,
          ),
        ).toBe(true);
        expect(
          historyCardanoDatumMatches(
            CML.TransactionOutput.new(
              before.address(),
              before.amount(),
              CML.DatumOption.new_datum(
                CML.PlutusData.from_cbor_hex(changedDuplicate),
              ),
            ),
            rawRefund,
          ),
        ).toBe(false);
      }
    },
  );
});
