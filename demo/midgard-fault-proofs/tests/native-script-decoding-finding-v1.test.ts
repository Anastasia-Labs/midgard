/**
 * Unit coverage for the §3.4 finding record's boundary gate: the §3.2/3.3
 * provability classification is non-negotiable — unprovable corners are
 * refused at the proving core's API boundary regardless of policy, and a
 * structurally incoherent record never reaches a submitter.
 */
import { describe, expect, it } from "vitest";

import {
  assertNativeScriptDecodingFindingProvableV1,
  type NativeScriptDecodingFindingV1,
  NativeScriptDecodingProvabilityV1,
} from "../src/native-script-decoding/index.js";

const TX_ID_HEX = "ab".repeat(32);
const HEADER_HASH = "cd".repeat(28);

const directionANormal: NativeScriptDecodingFindingV1 = {
  direction: 0n,
  sourceKind: 0n,
  event: { kind: "l2Transaction", txId: TX_ID_HEX },
  headerHash: HEADER_HASH,
  fraudulentBlockOutRef: `${TX_ID_HEX}#0`,
  accusedOutpointSourceKind: 0n,
  accusedOutpointCursor: 0n,
  scanReasonClass: null,
  provability: NativeScriptDecodingProvabilityV1.MachineRoute,
  descriptor: {
    referenceScriptLanguage: 0,
    outputIndex: 0,
    totalLength: 64,
  },
  estimatedThreadTxCount: 7,
};

const directionBForced: NativeScriptDecodingFindingV1 = {
  ...directionANormal,
  direction: 1n,
  sourceKind: 1n,
  event: { kind: "forcedEvent", orderKeyCbor: "d8799f00ff" },
  scanReasonClass: 0n,
};

describe("native-script-decoding finding v1", () => {
  it("accepts each provable route in its coherent shape", () => {
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1(directionANormal),
    ).not.toThrow();
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1(directionBForced),
    ).not.toThrow();
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionBForced,
        provability: NativeScriptDecodingProvabilityV1.DescriptorContradiction,
        descriptor: {
          referenceScriptLanguage: 3,
          outputIndex: 0,
          totalLength: 64,
        },
      }),
    ).not.toThrow();
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionBForced,
        provability: NativeScriptDecodingProvabilityV1.OutOfDomainAccusation,
        accusedOutpointCursor: -1n,
        descriptor: null,
      }),
    ).not.toThrow();
  });

  it("refuses the unprovable classes regardless of shape", () => {
    for (const provability of [
      NativeScriptDecodingProvabilityV1.WrapperContradiction,
      NativeScriptDecodingProvabilityV1.NotAFault,
    ]) {
      expect(() =>
        assertNativeScriptDecodingFindingProvableV1({
          ...directionBForced,
          provability,
        }),
      ).toThrow(/not provable by this family/);
    }
  });

  it("refuses structurally incoherent records", () => {
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionBForced,
        sourceKind: 0n,
        event: { kind: "l2Transaction", txId: TX_ID_HEX },
      }),
    ).toThrow(/forced source/);
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionBForced,
        scanReasonClass: null,
      }),
    ).toThrow(/scan-reason class/);
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionANormal,
        provability: NativeScriptDecodingProvabilityV1.OutOfDomainAccusation,
        descriptor: null,
      }),
    ).toThrow(/direction B's alone/);
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionANormal,
        event: { kind: "forcedEvent", orderKeyCbor: "d8799f00ff" },
      }),
    ).toThrow(/committed transaction id/);
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionANormal,
        descriptor: null,
      }),
    ).toThrow(/resolved descriptor fields/);
  });

  it("refuses language tags that contradict the claimed route", () => {
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionBForced,
        provability: NativeScriptDecodingProvabilityV1.DescriptorContradiction,
      }),
    ).toThrow(/cannot name a tag-0 descriptor/);
    expect(() =>
      assertNativeScriptDecodingFindingProvableV1({
        ...directionANormal,
        descriptor: {
          referenceScriptLanguage: 3,
          outputIndex: 0,
          totalLength: 64,
        },
      }),
    ).toThrow(/tag-0 descriptors only/);
  });
});
