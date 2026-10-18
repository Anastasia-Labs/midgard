import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/native-tx-field-access";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/index.js";
import "./tx-order.transaction-cbor.js";

import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core";
import {
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";
import { transactionCbor } from "./tx-order.transaction-cbor.js";

describe("V1 transaction-order datum and §8 field carriage", () => {
  it("pins the exact datum, spend, and forced-key V1 vectors", () => {
    const txOrderId: SDK.OutputReference = {
      transactionId: "33".repeat(32),
      outputIndex: 4n,
    };
    const source: SDK.ForcedTxProofSource = {
      compact_cbor: "01",
      witness_set_compact_cbor: "0203",
      field_preimage_lengths_cbor: "04",
    };
    const payload: SDK.TxOrderPayload = {
      tx_id: "44".repeat(32),
      transaction_commitment: "55".repeat(32),
      submitted_source: source,
    };
    const event: SDK.TxOrderEvent = { id: txOrderId, tx: payload };
    const datum: SDK.TxOrderDatum = {
      event,
      inclusion_time: 123n,
      witness: "66".repeat(28),
      refund_address: {
        paymentCredential: {
          PublicKeyCredential: ["77".repeat(28)],
        },
        stakeCredential: null,
      },
      refund_datum: "NoDatum",
    };
    const forced: SDK.ForcedInclusionTxV1 = {
      tx_id: payload.tx_id,
      submitted_source: source,
      verdict: {
        ForcedTxInvalid: {
          reason: {
            PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 0n },
          },
        },
      },
    };
    const spend: SDK.TxOrderSpendRedeemer = {
      input_index: 0n,
      output_index: 1n,
      hub_ref_input_index: 2n,
      settlement_ref_input_index: 3n,
      burn_redeemer_index: 4n,
      membership_proof: {
        domain: "ForcedTransactionsV1RootDomain",
        root: "00".repeat(32),
        phas_root: "11".repeat(32),
        count: 1n,
        key: Data.to(txOrderId, SDK.OutputReference),
        value: Data.to(forced, SDK.ForcedInclusionTxV1),
        proof: [],
      },
      inclusion_proof_script_withdraw_redeemer_index: 5n,
      validity_override: {
        ForcedTxInvalid: {
          reason: {
            PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 0n },
          },
        },
      },
    };

    const datumCbor = Data.to(datum, SDK.TxOrderDatum);
    expect(Data.to(txOrderId, SDK.OutputReference)).toBe(
      `d8799f5820${"33".repeat(32)}04ff`,
    );
    expect(Data.to(payload, SDK.TxOrderPayload)).toBe(
      `d8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffff`,
    );
    expect(Data.to(event, SDK.TxOrderEvent)).toBe(
      `d8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffffff`,
    );
    expect(datumCbor).toBe(
      `d8799fd8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffffff187b581c${"66".repeat(28)}d8799fd8799f581c${"77".repeat(28)}ffd87a80ffd87980ff`,
    );
    expect(Data.to(forced, SDK.ForcedInclusionTxV1)).toBe(
      `d8799f5820${"44".repeat(32)}d8799f41014202034104ffd87a9fd905229f0000ffffff`,
    );
    expect(Data.to(spend, SDK.TxOrderSpendRedeemer)).toBe(
      "d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff583cd8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87a9fd905229f0000ffffff80ff05d87a9fd905229f0000ffffff",
    );
    expect(
      SDK.decodeTxOrderDatumCbor(SDK.encodeTxOrderDatumCbor(datum)),
    ).toEqual(datum);

    // Read as the chain casts it: any encoding, the order datum's shape only.
    const read = (cbor: string) =>
      SDK.decodeTxOrderDatumCbor(Buffer.from(cbor, "hex"));
    expect(read(datumCbor.replace("187b581c", "1a0000007b581c"))).toEqual(
      datum,
    );
    expect(() => read(`d87a${datumCbor.slice(4)}`)).toThrow();
    expect(() => read(`${datumCbor.slice(0, -2)}00ff`)).toThrow();

    const pointerDatum: SDK.TxOrderDatum = {
      ...datum,
      refund_address: {
        paymentCredential: datum.refund_address.paymentCredential,
        stakeCredential: {
          Pointer: {
            slotNumber: 6n,
            transactionIndex: 7n,
            certificateIndex: 8n,
          },
        },
      },
    };
    expect(
      Data.from(Data.to(pointerDatum, SDK.TxOrderDatum), SDK.TxOrderDatum),
    ).toEqual(pointerDatum);
    expect(() =>
      Data.to(
        {
          ...pointerDatum,
          refund_address: {
            ...pointerDatum.refund_address,
            stakeCredential: {
              Pointer: [
                {
                  slotNumber: 6n,
                  transactionIndex: 7n,
                  certificateIndex: 8n,
                },
              ],
            },
          },
        } as never,
        SDK.TxOrderDatum,
      ),
    ).toThrow();
  });

  it("derives the §8 carriage of every non-empty field and nothing for the empty ones", () => {
    const cbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor: cbor,
      owner: Buffer.alloc(28, 0x44),
    });

    expect(cbor.length).toBeGreaterThan(8 * 1024);
    expect(cbor.length).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
    );
    // Only fields 0 (one spend input) and 2 (outputs) carry anything in this
    // fixture, so the carriage list is exactly those two entries: the counted
    // scheme published four per-item chunks for the outputs alone, which is the
    // whole difference the §4 reversion makes to a publisher.
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([0, 2]);
    const outputs = material.carriage[1]!;
    expect(outputs.fieldName).toBe("outputs");
    expect(outputs.preimage).toEqual(
      decodeMidgardForcedTxFullFromCanonicalCbor(cbor).body.outputsPreimageCbor,
    );
    expect(outputs.commitment).toBe(
      midgardFieldCommitment(outputs.preimage).toString("hex"),
    );
    // §8.4 is a partition, so the tier is a fact about the byte length and not a
    // choice this module makes.
    expect(outputs.plan.tier).toBe(
      selectMidgardFieldCarriageTier(outputs.preimage.length),
    );
    expect(outputs.plan.totalLength).toBe(outputs.preimage.length);
    expect(outputs.plan.commitment.toString("hex")).toBe(outputs.commitment);
    expect(outputs.plan.txId.toString("hex")).toBe(material.transactionId);
  });

  it("plans inline carriage under the order reserve and publishes what will not fit", () => {
    const cbor = transactionCbor();
    const owner = Buffer.alloc(28, 0x44);
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor: cbor,
      owner,
    });
    const [spendInputs, outputs] = material.carriage;
    expect(outputs).toBeDefined();

    // Under the default reserve both fields fit the order transaction's own
    // redeemer, so nothing has to be published first.
    const inline = SDK.planTxOrderMaterialCarriage({ material, owner });
    expect(inline.carriage.map((field) => field.plan.tier)).toEqual([
      "Inline",
      "Inline",
    ]);
    expect(inline.referenced).toEqual([]);
    expect(inline.inlineBytes).toBe(
      spendInputs!.preimage.length + outputs!.preimage.length,
    );
    expect(inline.inlineReserveBytes).toBe(
      SDK.MIDGARD_TX_ORDER_INLINE_CARRIAGE_RESERVE_BYTES,
    );

    // One byte short of the field's own length is the whole decision: there is no
    // consensus threshold here, only this transaction's budget (§8.11), so the
    // same field is demoted to tier 2 and gets one publication at the creator's
    // own wallet address.
    const published = SDK.planTxOrderMaterialCarriage({
      material,
      owner,
      inlineReserveBytes: outputs!.preimage.length - 1,
    });
    // The small spend-input field still fits the reserve the outputs missed.
    expect(published.inline.map((field) => field.fieldIndex)).toEqual([0]);
    expect(published.inlineBytes).toBe(spendInputs!.preimage.length);
    expect(published.referenced.map((field) => field.plan.tier)).toEqual([
      "RawUtxo",
    ]);
    const [demoted] = published.referenced;
    expect(demoted!.plan.inlinePreimage).toBeNull();
    expect(demoted!.plan.publications.map((entry) => entry.bytes)).toEqual([
      outputs!.preimage,
    ]);
    // Demotion changes whose budget pays and nothing about the bytes: the §4
    // commitment the door checks is the same one either way.
    expect(demoted!.plan.commitment).toEqual(outputs!.plan.commitment);
  });

  it("pins the tx-order mint redeemer wire form against its Aiken vector", () => {
    // The same three bytes-level vectors
    // `midgard/user_events/tx_order_v1.test`'s
    // `tx_order_mint_redeemer_wire_form_is_the_event_plus_the_carriage_vector`
    // pins. Both halves emit `Constr 0 [<user event redeemer>, <carriage list>]`,
    // and the carriage constructors keep §8.8's frozen tags 0/1/2.
    const event = {
      AuthenticateEvent: {
        nonce_input_index: 0n,
        event_output_index: 1n,
        hub_ref_input_index: 2n,
        witness_registration_redeemer_index: 3n,
      },
    } as const;

    expect(
      Data.to(
        { event, material_carriage: [] } satisfies SDK.TxOrderMintRedeemer,
        SDK.TxOrderMintRedeemer,
      ),
    ).toBe("d8799fd8799f00010203ff80ff");
    expect(
      Data.to(
        {
          event: {
            BurnEventNFT: {
              nonce_asset_name: "aabb",
              witness_unregistration_redeemer_index: 4n,
            },
          },
          material_carriage: [],
        } satisfies SDK.TxOrderMintRedeemer,
        SDK.TxOrderMintRedeemer,
      ),
    ).toBe("d8799fd87a9f42aabb04ff80ff");
    expect(
      Data.to(
        {
          event,
          material_carriage: [
            { Inline: { preimage: "80" } },
            { RawUtxo: { ref_input_index: 5n } },
            {
              Certified: {
                cert_ref_input_index: 6n,
                chunk_ref_input_indices: [7n, 8n],
              },
            },
          ],
        } satisfies SDK.TxOrderMintRedeemer,
        SDK.TxOrderMintRedeemer,
      ),
    ).toBe(
      "d8799fd8799f00010203ff9fd8799f4180ffd87a9f05ffd87b9f069f0708ffffffff",
    );
  });

  it("refuses material it cannot bind to a canonical transaction", () => {
    expect(() =>
      SDK.deriveTxOrderMaterial({
        submittedTxCbor: Buffer.from("00", "hex"),
        owner: Buffer.alloc(28, 0x44),
      }),
    ).toThrow();
  });
});
