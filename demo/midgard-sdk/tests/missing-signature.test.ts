import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const h32 = (byte: string) => byte.repeat(32);
const h28 = (byte: string) => byte.repeat(28);
const proof: SDK.Proof = [];
const bodyOpening = {
  BodyFieldOpening: {
    native_tx_compact_cbor: "80",
    carriage: { Inline: { preimage: "80" } },
  },
} as const;
const witnessOpening = {
  WitnessFieldOpening: {
    native_tx_compact_cbor: "80",
    witness_set: {
      addr_tx_wits_hash: h32("11"),
      script_tx_wits_hash: h32("22"),
      redeemer_tx_wits_hash: h32("33"),
    },
    carriage: { RawUtxo: { ref_input_index: 4n } },
  },
} as const;

const roundTrip = <T>(value: T, schema: Parameters<typeof Data.to>[1]): T =>
  Data.from(Data.to(value, schema), schema) as T;

describe("missing-signature v1 SDK wire twins", () => {
  it("round-trips every step state and argument in Aiken field order", () => {
    const step01 = {
      input_index: 0n,
      output_index: 1n,
      hub_ref_input_index: 2n,
      state_queue_node_ref_input_index: 3n,
      native_tx_id: h32("44"),
      l2_transaction_source_cbor: "80",
      transactions_phas_root: h32("55"),
      tx_membership_proof: proof,
      inclusion_proof_script_withdraw_redeemer_index: 0n,
    };
    expect(roundTrip(step01, SDK.MissingSignatureStep01Args)).toEqual(step01);

    const step02State = {
      verified_tx_id: h32("44"),
      verified_witness_set_hash: h32("66"),
    };
    const step02Args = {
      input_index: 0n,
      output_index: 1n,
      required_signers_opening: bodyOpening,
      bad_required_signer_hash_index: 2n,
    };
    expect(roundTrip(step02State, SDK.MissingSignatureStep02State)).toEqual(
      step02State,
    );
    expect(roundTrip(step02Args, SDK.MissingSignatureStep02Args)).toEqual(
      step02Args,
    );

    const step03State = {
      missing_required_signer_hash: h28("77"),
      ...step02State,
    };
    const step03Args = {
      input_index: 0n,
      output_index: 1n,
      missing_required_signer_vkey: h32("88"),
    };
    expect(roundTrip(step03State, SDK.MissingSignatureStep03State)).toEqual(
      step03State,
    );
    expect(roundTrip(step03Args, SDK.MissingSignatureStep03Args)).toEqual(
      step03Args,
    );

    const step04State = {
      missing_required_signer_vkey: h32("88"),
      ...step02State,
      field_walk_checkpoint_hash: "",
    };
    const step04Scan = {
      Scan: {
        input_index: 0n,
        output_index: 1n,
        addr_tx_wits_opening: witnessOpening,
        checkpoint_cbor: null,
      },
    };
    const step04Finalize = {
      Finalize: {
        input_index: 0n,
        output_index: 1n,
        fraud_proof_mint_redeemer_index: 0n,
        addr_tx_wits_opening: witnessOpening,
        checkpoint_cbor: "aa".repeat(53),
      },
    };
    expect(roundTrip(step04State, SDK.MissingSignatureStep04State)).toEqual(
      step04State,
    );
    expect(roundTrip(step04Scan, SDK.MissingSignatureStep04Args)).toEqual(
      step04Scan,
    );
    expect(roundTrip(step04Finalize, SDK.MissingSignatureStep04Args)).toEqual(
      step04Finalize,
    );
  });

  it("pins canonical field-7 checkpoints and rejects unreachable thread state", () => {
    const checkpoint = SDK.missingSignatureFieldWalkCheckpoint({
      txId: h32("22"),
      itemCount: 140,
      totalLength: 2 + 140 * 103,
      nextItemIndex: 32,
    });
    expect(checkpoint.checkpointCbor).toHaveLength(106);
    expect(checkpoint.checkpointCbor).toBe(
      `865820${h32("22")}4107430038564300008c4300002043000ce2`,
    );
    expect(checkpoint.checkpointHash).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      SDK.resolveMissingSignatureFieldWalkCheckpoint({
        txId: h32("22"),
        itemCount: 140,
        totalLength: 2 + 140 * 103,
        committedHash: checkpoint.checkpointHash,
      }),
    ).toStrictEqual(checkpoint);
    expect(() =>
      SDK.resolveMissingSignatureFieldWalkCheckpoint({
        txId: h32("22"),
        itemCount: 140,
        totalLength: 2 + 140 * 103,
        committedHash: h32("ff"),
      }),
    ).toThrow(/not reachable/u);
  });

  it("pins the vkey lift, missing-ordinal selection, and test-independent asset name", () => {
    const vkey = "99".repeat(32);
    const hash = SDK.missingSignatureVkeyHash(vkey);
    expect(hash).toMatch(/^[0-9a-f]{56}$/u);
    expect(
      SDK.findMissingRequiredSignerIndex({
        requiredSignerHashes: [h28("aa"), hash],
        addrTxWits: [{ verification_key: vkey, signature: "bb".repeat(64) }],
      }),
    ).toBe(0);
    expect(
      SDK.missingSignatureThreadTokenAssetName("0000000e", "cc".repeat(28)),
    ).toBe(`0000000e${"cc".repeat(28)}`);
  });
});
