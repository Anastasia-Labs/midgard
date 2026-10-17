import "./native-script-decoding-envelope.step-02-redeemer-envelope-chart-the-escalation-capable-check-2-3.js";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  deriveMidgardNativeTxProofSource,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
} from "@al-ft/midgard-core";
import {
  type FieldCarriage,
  NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
  NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
  NativeScriptDecodingStep03OpenSubjectSpendRedeemer,
  NativeScriptDecodingStep04SpendRedeemer,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  chunkProofToData,
  dataBytes,
  L1_ENVELOPE_BYTES,
  proofFromCbor,
  STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
} from "./native-script-decoding-envelope.step-01-redeemer-envelope-chart-both-carriages-q4.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  insertAdversarialMembershipSiblings,
  outputReferenceCbor,
} from "./support/submit-init-emulator-fixtures.js";
import { h32, makeNativeTx } from "./support/submit-init-emulator-shared.js";

describe("step-03 redeemer envelope chart (windows, frames, tier frontier)", () => {
  // Adversarial reference-script item at the §8 aggregate-field ceiling:
  // 32,768 bytes is `maxTransactionAggregateFieldBytes`, the widest a single
  // output field — and so any single item inside it — can commit to. Chunked
  // at the §8.4 stride this is 8 full windows.
  const ADVERSARIAL_ITEM_BYTES = 32_000;
  const item = buildMidgardBoundedItem({
    fieldIndex: 4,
    itemIndex: 0,
    bytes: Buffer.alloc(ADVERSARIAL_ITEM_BYTES, 0x82),
  });
  /**
   * The scan control is the canonically-encoded 15-field thread state (the
   * v1/v2 wire vectors pin it at ~120 bytes); 256 bytes is a deliberate
   * over-allowance so this chart cannot be invalidated by a field widening
   * alone. The emulator journeys re-measure it with real control bytes.
   */
  const controlCbor = Buffer.alloc(256, 0x88).toString("hex");
  /**
   * Frames are PLANNER-bounded, not adversary-bounded: the §5.2 planner emits
   * at most 16 nodes per segment (Q-policy default) and a frame tail never
   * exceeds one token's bounded width. 16 frames with 64-byte tails is
   * therefore above the worst plan the planner may legally submit; suites 3–4
   * re-measure the frames axis against real plans.
   */
  const worstFrames = Array.from({ length: 16 }, () => ({
    tail: Buffer.alloc(64, 0x83).toString("hex"),
    kind: 3n,
    child_count: 65_535n,
    remaining: 65_535n,
    valid_count: 65_535n,
    required: 65_535n,
  }));

  it("fits the worst AdvanceOrClose window and its small closing form", () => {
    const midChunk = chunkProofToData(
      buildMidgardBoundedItemChunkProof(item, 3),
    );
    const nextChunk = chunkProofToData(
      buildMidgardBoundedItemChunkProof(item, 4),
    );
    expect(dataBytes(midChunk.chunk)).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    expect(dataBytes(nextChunk.chunk)).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);

    const scanRedeemer: NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer =
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            control_cbor: controlCbor,
            chunk_proof: midChunk,
            next_chunk_proof: nextChunk,
            frames: worstFrames,
            step_budget: 16n,
          },
        ],
      };
    const scanBytes = dataBytes(
      Data.to(
        scanRedeemer,
        NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
      ),
    );
    expect(
      scanBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
      "worst two-chunk Scan window no longer fits the L1 envelope; the §5.2 planner's window geometry has no smaller legal cut to fall back to",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    const verdictRedeemer: NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer =
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            control_cbor: controlCbor,
            chunk_proof: midChunk,
            next_chunk_proof: nextChunk,
            frames: [],
            step_budget: 1n,
          },
        ],
      };
    const verdictBytes = dataBytes(
      Data.to(
        verdictRedeemer,
        NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
      ),
    );
    expect(verdictBytes).toBeLessThan(scanBytes);
    expect(verdictBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES).toBeLessThanOrEqual(
      L1_ENVELOPE_BYTES,
    );
  });

  it("charts the split opening and descriptor proofs independently", async () => {
    // Real deep ledger-trie proof for the accused outpoint key.
    const accusedKey = outputReferenceCbor({
      transactionId: h32("e1"),
      outputIndex: 0n,
    });
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(accusedKey, Buffer.from("e0", "hex"));
    await insertAdversarialMembershipSiblings({
      trie,
      targets: [{ key: accusedKey, domain: 0x0c01 }],
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    const ledgerProof = proofFromCbor(
      Buffer.from((await trie.prove(accusedKey)).toCBOR()).toString("hex"),
    );
    const firstChunk = chunkProofToData(
      buildMidgardBoundedItemChunkProof(item, 0),
    );
    const compactTx = makeNativeTx({
      spendInputCbors: [
        { transactionId: h32("e2"), outputIndex: 0n },
        { transactionId: h32("e3"), outputIndex: 1n },
      ].map(outputReferenceCbor),
      fee: 1_000_000n,
      referenceByte: "e4",
      outputByte: "e5",
      witnessByte: "e6",
    });
    const compactCborHex = Buffer.from(
      deriveMidgardNativeTxProofSource(compactTx).compactCbor,
    ).toString("hex");

    const openWithCarriage = (carriage: FieldCarriage): number => {
      const redeemer: NativeScriptDecodingStep03OpenSubjectSpendRedeemer = {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            subject_field_opening: {
              BodyFieldOpening: {
                native_tx_compact_cbor: compactCborHex,
                carriage,
              },
            },
          },
        ],
      };
      return dataBytes(
        Data.to(redeemer, NativeScriptDecodingStep03OpenSubjectSpendRedeemer),
      );
    };

    const bindRedeemer: NativeScriptDecodingStep03BindDescriptorSpendRedeemer =
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            outpoint_key_cbor: accusedKey.toString("hex"),
            descriptor_cbor: Buffer.alloc(128, 0x84).toString("hex"),
            ledger_membership_proof: ledgerProof,
            first_chunk_proof: firstChunk,
          },
        ],
      };
    const bindBytes = dataBytes(
      Data.to(
        bindRedeemer,
        NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
      ),
    );
    expect(
      bindBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
      "BindDescriptor at adversarial ledger depth no longer fits the L1 envelope",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    // Tier 2 (RawUtxo): the production-default carriage for large subject
    // fields — must fit at adversarial ledger depth with the full chunk-0
    // proof aboard.
    const tier2Bytes = openWithCarriage({
      RawUtxo: { ref_input_index: 3n },
    });
    expect(
      tier2Bytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
      "tier-2 OpenSubject no longer fits the L1 envelope",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    // Tier-1 frontier: the largest inline subject-field preimage BindOutpoint
    // can still carry. Derived, recorded, and bounded — the §5.2 planner picks
    // tier 1 only under this number; the spec-side per-field tier-1 cap is
    // 14,336 (§8.1), so the frontier can never exceed it.
    const emptyInlineBytes = openWithCarriage({ Inline: { preimage: "" } });
    const probeBytes = 4_096;
    const probeInlineBytes = openWithCarriage({
      Inline: { preimage: Buffer.alloc(probeBytes, 0x85).toString("hex") },
    });
    const perPreimageByte = (probeInlineBytes - emptyInlineBytes) / probeBytes;
    const tier1FrontierBytes = Math.min(
      14_336,
      Math.floor(
        (L1_ENVELOPE_BYTES -
          STEP_TX_OVERHEAD_ALLOWANCE_BYTES -
          emptyInlineBytes) /
          perPreimageByte,
      ),
    );
    expect(
      tier1FrontierBytes,
      "the tier-1 inline-opening frontier fell below two §8.4 chunks; the planner would lose its small-field fast path",
    ).toBeGreaterThanOrEqual(2 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
  }, 600_000);
});

describe("step-04 redeemer pin", () => {
  it("is constant-size and trivially inside the envelope", () => {
    const redeemer: NativeScriptDecodingStep04SpendRedeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 65_535n,
        },
      ],
    };
    const bytes = dataBytes(
      Data.to(redeemer, NativeScriptDecodingStep04SpendRedeemer),
    );
    expect(bytes).toBeLessThan(64);
  });
});
