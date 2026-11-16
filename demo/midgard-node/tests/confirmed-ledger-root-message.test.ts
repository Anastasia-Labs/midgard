/**
 * The message the landed ledger's root worker receives and posts back
 * (`confirmed-ledger-root-message.ts`): a packed input unpacks to itself,
 * a request of any other shape is refused by name, and only a response
 * with exactly two roots, or exactly a failure, is read.
 */
import { describe, expect, it } from "vitest";

import {
  type ConfirmedLedgerRootsInput,
  packConfirmedLedgerRootsRequest,
  parseConfirmedLedgerRootsResponse,
  unpackConfirmedLedgerRootsRequest,
} from "../src/mpf/confirmed-ledger-root-message.js";

const bytes = (label: number, length: number) => Buffer.alloc(length, label);

const INPUT: ConfirmedLedgerRootsInput = {
  confirmed: [
    { outref: bytes(1, 36), output: bytes(11, 40) },
    { outref: bytes(2, 36), output: bytes(12, 41) },
  ],
  delta: [
    { kind: "spent", outref: bytes(1, 36) },
    { kind: "produced", outref: bytes(3, 36), output: bytes(13, 42) },
    { kind: "spent", outref: bytes(3, 36) },
  ],
};

const plain = (input: unknown) => JSON.parse(JSON.stringify(input)) as unknown;

const packed = () => packConfirmedLedgerRootsRequest(INPUT).request;

describe("the root worker's request", () => {
  it("unpacks to the input it packs, the deltas in order", () => {
    const unpacked = unpackConfirmedLedgerRootsRequest(packed());
    expect(plain(unpacked)).toEqual(plain(INPUT));
  });

  it("packs an empty input", () => {
    expect(
      unpackConfirmedLedgerRootsRequest(
        packConfirmedLedgerRootsRequest({ confirmed: [], delta: [] }).request,
      ),
    ).toEqual({ confirmed: [], delta: [] });
  });

  it("transfers every buffer it carries", () => {
    const { request, transfer } = packConfirmedLedgerRootsRequest(INPUT);
    expect(new Set(transfer)).toEqual(
      new Set([
        request.confirmed.outRefs,
        request.confirmed.outputs,
        request.confirmed.offsets,
        request.delta.outRefs,
        request.delta.outputs,
        request.delta.offsets,
        request.deltaKinds,
      ]),
    );
  });

  it("refuses a request of another shape", () => {
    const request = packed();
    const kinds = (values: number[]) => new Uint8Array(values).buffer;
    const refusals: [unknown, string][] = [
      [null, "is not confirmed_ledger, a delta and its kinds"],
      [[], "is not confirmed_ledger, a delta and its kinds"],
      [
        { ...request, extra: 1 },
        "is not confirmed_ledger, a delta and its kinds",
      ],
      [
        { confirmed: request.confirmed, delta: request.delta },
        "is not confirmed_ledger, a delta and its kinds",
      ],
      [
        { ...request, deltaKinds: new Uint8Array(3) },
        "is not confirmed_ledger, a delta and its kinds",
      ],
      [
        { ...request, confirmed: { ...request.confirmed, extra: 1 } },
        "confirmed_ledger is not three packed buffers",
      ],
      [
        {
          ...request,
          delta: { ...request.delta, offsets: new ArrayBuffer(15) },
        },
        "the delta is not three packed buffers",
      ],
      [
        {
          ...request,
          confirmed: { ...request.confirmed, outputs: new ArrayBuffer(40) },
        },
        "confirmed_ledger entry 1 lies outside its buffers",
      ],
      [
        { ...request, deltaKinds: kinds([0, 1]) },
        "the delta has 3 entries and 2 kinds",
      ],
      [
        { ...request, deltaKinds: kinds([0, 2, 0]) },
        "delta entry 1 is neither a spent outref nor a produced entry",
      ],
      // A spent outref carries no output.
      [
        { ...request, deltaKinds: kinds([0, 0, 0]) },
        "delta entry 1 is neither a spent outref nor a produced entry",
      ],
    ];
    for (const [value, refusal] of refusals)
      expect(unpackConfirmedLedgerRootsRequest(value)).toContain(refusal);
  });
});

describe("the root worker's response", () => {
  const frontierRoot = "a0".repeat(32);
  const tailRoot = "a1".repeat(32);

  it("reads exactly two roots", () => {
    expect(
      parseConfirmedLedgerRootsResponse({ frontierRoot, tailRoot }),
    ).toEqual({ frontierRoot, tailRoot });
  });

  it("throws a failure the worker posts", () => {
    expect(() =>
      parseConfirmedLedgerRootsResponse({ error: "out of heap" }),
    ).toThrow("confirmed_ledger root worker failed: out of heap");
  });

  it("throws on anything else", () => {
    for (const value of [
      null,
      { root: frontierRoot },
      { frontierRoot },
      { frontierRoot, tailRoot: "A1".repeat(32) },
      { frontierRoot, tailRoot: "a1".repeat(31) },
      { frontierRoot, tailRoot, extra: 1 },
      { frontierRoot, tailRoot, error: "out of heap" },
      { error: 1 },
    ])
      expect(() => parseConfirmedLedgerRootsResponse(value)).toThrow(
        "confirmed_ledger root worker sent no roots",
      );
  });
});
