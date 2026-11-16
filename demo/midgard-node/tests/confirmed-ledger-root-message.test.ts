/**
 * The messages between the verification and the landed ledger's root
 * worker (`confirmed-ledger-root-message.ts`): a packed delta unpacks to
 * itself, and so does a page of entries a leaves command carries; a request
 * or command of any other shape is refused by name; and only a reply with
 * exactly what was asked, or exactly a failure, is read.
 */
import { describe, expect, it } from "vitest";

import {
  type ConfirmedLedgerDeltaOp,
  leavesCommand,
  packConfirmedLedgerRootsRequest,
  parseConfirmedLedgerFedResponse,
  parseConfirmedLedgerLeavesResponse,
  parseConfirmedLedgerRootsResponse,
  unpackConfirmedLedgerRootsCommand,
  unpackConfirmedLedgerRootsRequest,
} from "../src/mpf/confirmed-ledger-root-message.js";

const bytes = (label: number, length: number) => Buffer.alloc(length, label);

const DELTA: readonly ConfirmedLedgerDeltaOp[] = [
  { kind: "spent", outref: bytes(1, 36) },
  { kind: "produced", outref: bytes(3, 36), output: bytes(13, 42) },
  { kind: "spent", outref: bytes(3, 36) },
];

const ENTRIES = [
  { outref: bytes(1, 36), output: bytes(11, 40) },
  { outref: bytes(2, 36), output: bytes(12, 41) },
];

const plain = (input: unknown) => JSON.parse(JSON.stringify(input)) as unknown;

const packed = () => packConfirmedLedgerRootsRequest(DELTA).request;

describe("the root worker's request", () => {
  it("unpacks to the delta it packs, in order", () => {
    expect(plain(unpackConfirmedLedgerRootsRequest(packed()))).toEqual(
      plain(DELTA),
    );
  });

  it("packs an empty delta", () => {
    expect(
      unpackConfirmedLedgerRootsRequest(
        packConfirmedLedgerRootsRequest([]).request,
      ),
    ).toEqual([]);
  });

  it("transfers every buffer it carries", () => {
    const { request, transfer } = packConfirmedLedgerRootsRequest(DELTA);
    expect(new Set(transfer)).toEqual(
      new Set([
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
      [null, "is not a delta and its kinds"],
      [[], "is not a delta and its kinds"],
      [{ ...request, extra: 1 }, "is not a delta and its kinds"],
      [{ delta: request.delta }, "is not a delta and its kinds"],
      [
        { ...request, deltaKinds: new Uint8Array(3) },
        "is not a delta and its kinds",
      ],
      [
        { ...request, delta: { ...request.delta, extra: 1 } },
        "the delta is not three packed buffers",
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
          delta: { ...request.delta, outputs: new ArrayBuffer(40) },
        },
        "the delta entry 1 lies outside its buffers",
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

describe("the root worker's commands", () => {
  it("unpacks a leaves command to the entries it packs, in order", () => {
    const { command, transfer } = leavesCommand(ENTRIES);
    expect(plain(unpackConfirmedLedgerRootsCommand(command))).toEqual(
      plain({ kind: "leaves", entries: ENTRIES }),
    );
    expect(transfer).toHaveLength(3);
  });

  it("unpacks a feed and the roots", () => {
    const leaves = new Uint8Array(64).fill(7);
    expect(
      unpackConfirmedLedgerRootsCommand({
        kind: "feed",
        leaves: leaves.buffer,
      }),
    ).toEqual({ kind: "feed", leaves });
    expect(unpackConfirmedLedgerRootsCommand({ kind: "roots" })).toEqual({
      kind: "roots",
    });
  });

  it("refuses a command of another shape", () => {
    const { command } = leavesCommand(ENTRIES);
    const refusals: [unknown, string][] = [
      [null, "the command is not leaves, a feed or the roots"],
      [{ kind: "roots", extra: 1 }, "the command is not leaves, a feed"],
      [{ kind: "feed", leaves: [1, 2] }, "the command is not leaves, a feed"],
      [{ kind: "feed", entries: {} }, "the command is not leaves, a feed"],
      [
        { kind: "leaves", entries: { outRefs: new ArrayBuffer(0) } },
        "the entries is not three packed buffers",
      ],
      [
        { ...command, kind: "hash" },
        "the command is not leaves, a feed or the roots",
      ],
    ];
    for (const [value, refusal] of refusals)
      expect(unpackConfirmedLedgerRootsCommand(value)).toContain(refusal);
  });
});

describe("the root worker's replies", () => {
  const frontierRoot = "a0".repeat(32);
  const tailRoot = "a1".repeat(32);

  it("reads exactly two roots", () => {
    expect(
      parseConfirmedLedgerRootsResponse({ frontierRoot, tailRoot }),
    ).toEqual({ frontierRoot, tailRoot });
  });

  it("reads exactly leaves, and exactly a feed's acknowledgement", () => {
    const leaves = new Uint8Array(128).fill(9);
    expect(
      parseConfirmedLedgerLeavesResponse({ leaves: leaves.buffer }),
    ).toEqual(leaves);
    expect(() => parseConfirmedLedgerFedResponse({ fed: true })).not.toThrow();
    expect(() => parseConfirmedLedgerLeavesResponse({ leaves: [1] })).toThrow(
      "confirmed_ledger root worker sent no leaves",
    );
    expect(() => parseConfirmedLedgerFedResponse({ fed: 1 })).toThrow(
      "confirmed_ledger root worker sent no feed",
    );
  });

  it("throws a failure the worker posts, whatever was asked", () => {
    for (const parse of [
      parseConfirmedLedgerRootsResponse,
      parseConfirmedLedgerLeavesResponse,
      parseConfirmedLedgerFedResponse,
    ])
      expect(() => parse({ error: "out of heap" })).toThrow(
        "confirmed_ledger root worker failed: out of heap",
      );
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
      { fed: true },
    ])
      expect(() => parseConfirmedLedgerRootsResponse(value)).toThrow(
        "confirmed_ledger root worker sent no roots",
      );
  });
});
