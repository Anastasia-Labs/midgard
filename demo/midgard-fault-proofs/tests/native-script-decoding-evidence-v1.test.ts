/**
 * Unit coverage for the `native-script-decoding` evidence module (offchain
 * plan §4.2): the cheap deterministic pieces — outpoint trie key, wire
 * converters, planner-window proof selection, the §7.2 out-of-domain face
 * classifier, and the injected ledger-trie handle in both polarities. The
 * heavier compositions (step-02 committed-claim openings against a real
 * reconstruction) ride the emulator end-to-end suites.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  computeHash32,
  encodeMidgardNativeScript,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MidgardNativeScriptDecodingDirectionsV1,
  type MidgardNativeScriptScanFrameV1,
} from "@al-ft/midgard-core";
import {
  BoundedItemChunkProofV1,
  MIDGARD_FIELD_INDEX_V1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingChunkProofV1,
  buildNativeScriptDecodingLedgerMembershipV1,
  buildNativeScriptDecodingScanPlanV1,
  classifyNativeScriptDecodingOutOfDomainFaceV1,
  nativeScriptDecodingChunkProofDataV1,
  nativeScriptDecodingFrameDataV1,
  NativeScriptDecodingOutOfDomainFacesV1,
  nativeScriptDecodingOutpointKeyHashV1,
  nativeScriptDecodingOutpointKeyV1,
  nativeScriptDecodingScanAccusationOfV1,
  nativeScriptDecodingScanArgsEvidenceV1,
  nativeScriptDecodingSubjectFieldIndexV1,
  nativeScriptDecodingWindowProofsV1,
} from "../src/native-script-decoding/index.js";

const TX_ID_HEX = "ab".repeat(32);
const FIELD = MIDGARD_FIELD_INDEX_V1.referenceInputs;

const signerKey = Buffer.alloc(28, 0x55);
/** The versioned tag-0 item wrapping one canonical signature node. */
const sigItemBytes = (): Uint8Array => {
  const script = encodeMidgardNativeScript({
    type: "sig",
    keyHash: signerKey,
  });
  return Buffer.concat([
    Buffer.from("8200", "hex"),
    Buffer.from([0x58, script.length]),
    script,
  ]);
};

describe("native-script-decoding evidence v1", () => {
  it("derives the accused outpoint's 38-byte trie key and its hash", () => {
    const key = nativeScriptDecodingOutpointKeyV1({
      txIdHex: TX_ID_HEX,
      outputIndex: 0x0102,
    });
    expect(key.length).toBe(38);
    expect(key.toString("hex")).toBe(`825820${TX_ID_HEX}190102`);
    expect(nativeScriptDecodingOutpointKeyHashV1(key)).toBe(
      computeHash32(key).toString("hex"),
    );
    expect(() =>
      nativeScriptDecodingOutpointKeyV1({
        txIdHex: "abcd",
        outputIndex: 0,
      }),
    ).toThrow(/32 bytes of lowercase hex/);
  });

  it("maps outpoint source kinds onto the SS2.5 field indices and refuses strangers", () => {
    expect(nativeScriptDecodingSubjectFieldIndexV1(0n)).toBe(
      MIDGARD_FIELD_INDEX_V1.spendInputs,
    );
    expect(nativeScriptDecodingSubjectFieldIndexV1(1n)).toBe(
      MIDGARD_FIELD_INDEX_V1.referenceInputs,
    );
    expect(() => nativeScriptDecodingSubjectFieldIndexV1(2n)).toThrow(
      /names no/,
    );
  });

  it("converts core chunk proofs to the wire shape and round-trips through Data", () => {
    const itemBytes = Buffer.alloc(
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 + 905,
      0xab,
    );
    const coreProof = buildMidgardBoundedItemChunkProofV1(
      buildMidgardBoundedItemV1({
        fieldIndex: FIELD,
        itemIndex: 3,
        bytes: itemBytes,
      }),
      1,
    );
    const wire = nativeScriptDecodingChunkProofDataV1(coreProof);
    expect(wire.field_index).toBe(BigInt(FIELD));
    expect(wire.item_index).toBe(3n);
    expect(wire.total_length).toBe(BigInt(itemBytes.length));
    expect(wire.chunk_index).toBe(1n);
    expect(wire.chunk).toBe(
      itemBytes.subarray(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1).toString("hex"),
    );
    const cbor = Data.to(wire, BoundedItemChunkProofV1);
    expect(Data.from(cbor, BoundedItemChunkProofV1)).toStrictEqual(wire);
  });

  it("builds chunk proofs by index and refuses chunks outside the item", () => {
    const itemBytes = Buffer.alloc(100, 0x11);
    const wire = buildNativeScriptDecodingChunkProofV1({
      fieldIndex: FIELD,
      itemIndex: 0,
      itemBytes,
      chunkIndex: 0,
    });
    expect(wire.chunk_index).toBe(0n);
    expect(wire.chunk).toBe(itemBytes.toString("hex"));
    for (const chunkIndex of [1, -1]) {
      expect(() =>
        buildNativeScriptDecodingChunkProofV1({
          fieldIndex: FIELD,
          itemIndex: 0,
          itemBytes,
          chunkIndex,
        }),
      ).toThrow(/outside the item's 1 chunks/);
    }
  });

  it("selects window proofs exactly as the plan windows demand", () => {
    const single = Buffer.alloc(64, 0x22);
    const double = Buffer.alloc(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 + 1, 0x33);

    expect(
      nativeScriptDecodingWindowProofsV1({
        window: null,
        fieldIndex: FIELD,
        itemIndex: 0,
        itemBytes: single,
      }),
    ).toStrictEqual({ chunk_proof: null, next_chunk_proof: null });

    const windowed = nativeScriptDecodingWindowProofsV1({
      window: { chunkIndex: 0, needNext: false },
      fieldIndex: FIELD,
      itemIndex: 0,
      itemBytes: single,
    });
    expect(windowed.chunk_proof?.chunk_index).toBe(0n);
    expect(windowed.next_chunk_proof).toBeNull();

    const adjacent = nativeScriptDecodingWindowProofsV1({
      window: { chunkIndex: 0, needNext: true },
      fieldIndex: FIELD,
      itemIndex: 0,
      itemBytes: double,
    });
    expect(adjacent.chunk_proof?.chunk_index).toBe(0n);
    expect(adjacent.next_chunk_proof?.chunk_index).toBe(1n);

    expect(() =>
      nativeScriptDecodingWindowProofsV1({
        window: { chunkIndex: 1, needNext: true },
        fieldIndex: FIELD,
        itemIndex: 0,
        itemBytes: double,
      }),
    ).toThrow(/outside the item's 2 chunks/);
  });

  it("converts engine frame witnesses to the wire shape", () => {
    const frame: MidgardNativeScriptScanFrameV1 = {
      tail: Buffer.from("8200581cdead", "hex"),
      kind: 1,
      childCount: 3,
      remaining: 2,
      validCount: 1,
      required: 0n,
    };
    expect(nativeScriptDecodingFrameDataV1(frame)).toStrictEqual({
      tail: "8200581cdead",
      kind: 1n,
      child_count: 3n,
      remaining: 2n,
      valid_count: 1n,
      required: 0n,
    });
  });

  it("assembles Scan-redeemer evidence from a planned segment", () => {
    const itemBytes = sigItemBytes();
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes,
      direction: MidgardNativeScriptDecodingDirectionsV1.WrongfulRejection,
    });
    expect(plan.segments.length).toBeGreaterThan(0);
    const segment = plan.segments[0]!;
    const evidence = nativeScriptDecodingScanArgsEvidenceV1({
      segment,
      fieldIndex: FIELD,
      itemIndex: 0,
      itemBytes,
    });
    expect(evidence.control_cbor).toBe(segment.controlBefore.cborHex);
    expect(evidence.step_budget).toBe(BigInt(segment.stepBudget));
    expect(evidence.frames).toHaveLength(segment.frames.length);
    expect(evidence.chunk_proof?.chunk_index).toBe(
      BigInt(segment.window!.chunkIndex),
    );
    expect(evidence.next_chunk_proof).toBeNull();
  });

  it("copies each decoding accusation verbatim and refuses foreign rejection arms", () => {
    expect(
      nativeScriptDecodingScanAccusationOfV1({
        ResolvedReferenceScriptMalformed: { source_kind: 1n, input_index: 0n },
      }),
    ).toStrictEqual({
      scanReasonClass: 0n,
      outpointSourceKind: 1n,
      outpointCursor: 0n,
    });
    expect(
      nativeScriptDecodingScanAccusationOfV1({
        ResolvedReferenceScriptNodeLimit: { source_kind: 0n, input_index: 7n },
      }),
    ).toStrictEqual({
      scanReasonClass: 1n,
      outpointSourceKind: 0n,
      outpointCursor: 7n,
    });
    expect(
      nativeScriptDecodingScanAccusationOfV1({
        ResolvedReferenceScriptDepthLimit: {
          source_kind: 2n,
          input_index: -3n,
        },
      }),
    ).toStrictEqual({
      scanReasonClass: 2n,
      outpointSourceKind: 2n,
      outpointCursor: -3n,
    });
    expect(() =>
      nativeScriptDecodingScanAccusationOfV1("FeeBelowMinimum"),
    ).toThrow(/three decoding arms/);
  });

  it("classifies every SS7.2 out-of-domain face and refuses in-domain pairs", () => {
    const classify = classifyNativeScriptDecodingOutOfDomainFaceV1;
    expect(
      classify({ outpointSourceKind: 2n, outpointCursor: 0n, itemCount: null }),
    ).toBe(NativeScriptDecodingOutOfDomainFacesV1.UnknownSourceKind);
    expect(
      classify({
        outpointSourceKind: 0n,
        outpointCursor: -1n,
        itemCount: null,
      }),
    ).toBe(NativeScriptDecodingOutOfDomainFacesV1.NegativeOrdinal);
    expect(
      classify({ outpointSourceKind: 1n, outpointCursor: 5n, itemCount: 5n }),
    ).toBe(NativeScriptDecodingOutOfDomainFacesV1.CountFace);
    expect(
      classify({ outpointSourceKind: 0n, outpointCursor: 4n, itemCount: 5n }),
    ).toBeNull();
    expect(() =>
      classify({ outpointSourceKind: 0n, outpointCursor: 4n, itemCount: null }),
    ).toThrow(/authenticated item count/);
  });

  it("proves ledger membership through the injected trie handle and refuses a root mismatch", async () => {
    const key = nativeScriptDecodingOutpointKeyV1({
      txIdHex: TX_ID_HEX,
      outputIndex: 7,
    });
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(key, Buffer.from("d0", "hex"));
    await trie.insert(Buffer.alloc(38, 0x99), Buffer.from("d1", "hex"));
    const handle = {
      rootHex: (trie.hash as Buffer).toString("hex"),
      prove: async (target: Buffer) =>
        Buffer.from((await trie.prove(target)).toCBOR()),
    };

    const proof = await buildNativeScriptDecodingLedgerMembershipV1({
      trie: handle,
      outpointKey: key,
      priorLedgerRootHex: handle.rootHex.toUpperCase(),
    });
    expect(Array.isArray(proof)).toBe(true);

    await expect(
      buildNativeScriptDecodingLedgerMembershipV1({
        trie: handle,
        outpointKey: key,
        priorLedgerRootHex: "00".repeat(32),
      }),
    ).rejects.toThrow(/not the thread's prior_ledger_root/);
  });
});
