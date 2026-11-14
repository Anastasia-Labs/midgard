/**
 * The native-script scan ends an item whose length is a multiple of the
 * chunk size the way the chain does: `phase_a_native_chunk_window` admits an
 * exhausted cursor as an empty window before it reads any chunk, in either
 * mode, so the end-of-item token witness carries the real last chunk and no
 * next one. The items are the Aiken test's (`tests_phase_a_native_window`,
 * `late_aligned_end_of_item_rejects_at_the_carried_purpose_key`): an all-of
 * that declares three more children than it holds.
 */
import {
  buildMidgardBoundedItem,
  emptyMidgardValidationMerkleFrontier,
  encodeCbor,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { readValidationMachineVersionedScriptHeader } from "../src/validation-machine/native-script-frame.js";
import {
  type NativeScanPrepared,
  scanNativeScriptItem,
} from "../src/validation-machine/native-script-scan.js";
import type { ValidationMachineWorkWitness } from "../src/validation-machine/types.js";

/** Aiken's `timelock_payload(n)` behind its `82 00 59 LLLL` versioned header. */
const timelockItem = (n: number): Buffer => {
  const payload = Buffer.concat([
    Buffer.from("820199", "hex"),
    Buffer.from((n + 3).toString(16).padStart(4, "0"), "hex"),
    Buffer.from("82041818".repeat(2) + "820400".repeat(n), "hex"),
  ]);
  const length = Buffer.alloc(2);
  length.writeUInt16BE(payload.length);
  return Buffer.concat([Buffer.from("820059", "hex"), length, payload]);
};

const prepared = {
  proofSource: {
    compactCbor: Buffer.from("a0", "hex"),
    witnessSetCompactCbor: Buffer.from("a0", "hex"),
    fieldPreimageLengthsCbor: Buffer.from("80", "hex"),
  },
  contextCbor: Buffer.from("80", "hex"),
  resolutionScheduleHash: Buffer.alloc(32),
  signerFrontier: emptyMidgardValidationMerkleFrontier(),
  canonicalSignerHashes: [],
  signerLeafHashes: [],
  compactProofTransaction: {
    transactionBody: { validityIntervalStart: 100n, validityIntervalEnd: -1n },
  },
} as unknown as NativeScanPrepared;

const scanEndOfItem = (n: number, continuationCbor?: Buffer) => {
  const bytes = timelockItem(n);
  const item = buildMidgardBoundedItem({ fieldIndex: 2, itemIndex: 3, bytes });
  const pushed: ValidationMachineWorkWitness["auxiliary"][] = [];
  const scan = scanNativeScriptItem(
    prepared,
    (_phase, _cbor, auxiliary = null) => pushed.push(auxiliary),
    item,
    {
      stage: 1,
      scriptCount: 1,
      scriptSeen: 0,
      containsNonNativeScript: 0,
      itemLength: bytes.length,
      itemCommitment: item.commitment,
      cursor: readValidationMachineVersionedScriptHeader(bytes).payloadOffset,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: 0,
      result: -1,
      purposeKind: continuationCbor === undefined ? -1n : 1n,
      purposeIndex: continuationCbor === undefined ? -1n : 2n,
    },
    continuationCbor,
  );
  return { bytes, scan, last: pushed[pushed.length - 1] };
};

describe("native-script scan at a chunk-aligned end of item", () => {
  for (const [n, length] of [
    [1359, 4095],
    [2724, 8190],
  ] as const) {
    it(`rejects a ${length}-byte item at its last chunk in either mode`, () => {
      for (const [continuationCbor, reason] of [
        [
          encodeCbor([1n]),
          {
            ExecutionNativeScriptMalformed: {
              purpose_kind: 1n,
              purpose_index: 2n,
            },
          },
        ],
        [undefined, { WitnessNativeScriptMalformed: { script_index: 0n } }],
      ] as const) {
        const { bytes, scan, last } = scanEndOfItem(n, continuationCbor);
        expect(bytes.length).toBe(length);
        expect(length % MIDGARD_BOUNDED_ITEM_CHUNK_BYTES).toBe(0);
        expect(scan.control).toMatchObject({ stage: 1, cursor: length });
        expect(scan.stop).toMatchObject({ kind: "stopped", reason });
        expect(last).toMatchObject({
          kind: "nativeScriptToken",
          chunkProof: {
            chunkIndex: midgardBoundedItemChunkCount(length) - 1,
          },
          nextChunkProof: null,
        });
      }
    });
  }
});
