import { describe, expect, it } from "vitest";

import { encodeCbor } from "../src/codec/cbor.js";
import {
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  type DaLedgerSnapshotEntry,
  daLedgerSnapshotPage,
  type DaLedgerSnapshotRequest,
  type DaLedgerSnapshotResponse,
  DaRequestResponseProtocol,
  decodeDaLedgerSnapshotRequestCbor,
  decodeDaLedgerSnapshotResponseCbor,
  encodeDaLedgerSnapshotRequestCbor,
  encodeDaLedgerSnapshotResponseCbor,
} from "../src/da-transport.js";

const b = (byte: number, count: number): Buffer => Buffer.alloc(count, byte);
const h = (byte: string, count: number): string => byte.repeat(count);

const deploymentFingerprint = b(0x01, 28);
const utxosRoot = b(0x04, 32);

describe("ledger-snapshot request and response", () => {
  it("is a node protocol, not part of the public retained-DA profile", () => {
    expect(DaRequestResponseProtocol.ledgerSnapshot).toBe("ledger-snapshot");
    expect(DA_PUBLIC_RETAINED_DA_PROTOCOLS).not.toContain("ledger-snapshot");
  });

  // Each vector's bytes are written out by hand from the CBOR rules.
  it.each<[string, DaLedgerSnapshotRequest, string]>([
    [
      "the first page",
      { deploymentFingerprint, utxosRoot, after: null },
      `83581c${h("01", 28)}5820${h("04", 32)}f6`,
    ],
    [
      "a page after an out-ref",
      { deploymentFingerprint, utxosRoot, after: Buffer.from("0a0b", "hex") },
      `83581c${h("01", 28)}5820${h("04", 32)}420a0b`,
    ],
  ])("encodes and decodes the request for %s", (_label, request, hex) => {
    const encoded = encodeDaLedgerSnapshotRequestCbor(request);
    expect(encoded.toString("hex")).toBe(hex);
    expect(decodeDaLedgerSnapshotRequestCbor(encoded)).toEqual(request);
  });

  it.each<[string, DaLedgerSnapshotResponse, string]>([
    [
      "a page with entries",
      {
        status: "ok",
        entries: [
          { outRef: b(0x05, 3), output: Buffer.from("8200", "hex") },
          { outRef: b(0x06, 1), output: b(0x07, 24) },
        ],
        done: false,
      },
      `8300828243${h("05", 3)}428200824106${"5818" + h("07", 24)}f4`,
    ],
    [
      "the empty last page",
      { status: "ok", entries: [], done: true },
      "830080f5",
    ],
    ["a root the node does not hold", { status: "not_held" }, "8301f6f6"],
    ["another deployment", { status: "rejected" }, "8302f6f6"],
  ])("encodes and decodes the response for %s", (_label, response, hex) => {
    const encoded = encodeDaLedgerSnapshotResponseCbor(response);
    expect(encoded.toString("hex")).toBe(hex);
    expect(decodeDaLedgerSnapshotResponseCbor(encoded)).toEqual(response);
  });

  it.each<[string, unknown[], RegExp]>([
    [
      "a root that is not 32 bytes",
      [deploymentFingerprint, b(0x04, 31), null],
      /utxos_root must be 32 bytes/u,
    ],
    [
      "a missing field",
      [deploymentFingerprint, utxosRoot],
      /must have exactly 3 elements/u,
    ],
    [
      "an after that is not bytes",
      [deploymentFingerprint, utxosRoot, 1n],
      /after must be bytes/u,
    ],
  ])("refuses a request with %s", (_label, fields, refusal) => {
    expect(() => decodeDaLedgerSnapshotRequestCbor(encodeCbor(fields))).toThrow(
      refusal,
    );
  });

  it.each<[string, unknown[], RegExp]>([
    [
      "an unknown status",
      [3n, null, null],
      /status has unsupported enum code/u,
    ],
    ["a missing field", [0n, []], /must have exactly 3 elements/u],
    [
      "an ok page with no entry list",
      [0n, null, true],
      /entries must be a CBOR array/u,
    ],
    ["an ok page with no done flag", [0n, [], null], /done must be a boolean/u],
    [
      "a done flag that is not a boolean",
      [0n, [], 1n],
      /done must be a boolean/u,
    ],
    [
      "an out-ref that is not bytes",
      [0n, [[1n, b(0x07, 2)]], true],
      /entries\[0\]\.out_ref must be bytes/u,
    ],
    [
      "an output that is not bytes",
      [0n, [[b(0x05, 3), "output"]], true],
      /entries\[0\]\.output must be bytes/u,
    ],
    [
      "an entry that is not a pair",
      [0n, [[b(0x05, 3)]], true],
      /entries\[0\] must have exactly 2 elements/u,
    ],
    [
      "entries under a status other than ok",
      [1n, [], null],
      /carries entries only when status is ok/u,
    ],
    [
      "a done flag under a status other than ok",
      [2n, null, true],
      /carries entries only when status is ok/u,
    ],
  ])("refuses a response with %s", (_label, fields, refusal) => {
    expect(() =>
      decodeDaLedgerSnapshotResponseCbor(encodeCbor(fields)),
    ).toThrow(refusal);
  });

  it("refuses trailing bytes after a request or a response", () => {
    const request = encodeDaLedgerSnapshotRequestCbor({
      deploymentFingerprint,
      utxosRoot,
      after: null,
    });
    const response = encodeDaLedgerSnapshotResponseCbor({
      status: "not_held",
    });
    expect(() =>
      decodeDaLedgerSnapshotRequestCbor(
        Buffer.concat([request, Buffer.from([0x00])]),
      ),
    ).toThrow(/trailing bytes/u);
    expect(() =>
      decodeDaLedgerSnapshotResponseCbor(
        Buffer.concat([response, Buffer.from([0x00])]),
      ),
    ).toThrow(/trailing bytes/u);
  });
});

/** Entry `index`: a 36-byte out-ref and an output of 1 to 300 bytes, so the
 * byte-string heads of outputs take one, two and three bytes. */
const entry = (index: number): DaLedgerSnapshotEntry => {
  const outRef = b(0x5a, 36);
  outRef.writeUInt32BE(index, 0);
  return { outRef, output: b(index % 251, ((index * 37) % 300) + 1) };
};

/** An async source of `entries`, counting how many it has handed out and
 * whether it was closed. */
const counted = (entries: readonly DaLedgerSnapshotEntry[]) => {
  const state = { pulled: 0, closed: false };
  async function* source() {
    try {
      for (const item of entries) {
        state.pulled += 1;
        yield item;
      }
    } finally {
      state.closed = true;
    }
  }
  return { source: source(), state };
};

const encodedPage = (page: {
  entries: readonly DaLedgerSnapshotEntry[];
  done: boolean;
}): Buffer => encodeDaLedgerSnapshotResponseCbor({ status: "ok", ...page });

describe("daLedgerSnapshotPage", () => {
  const all = Array.from({ length: 400 }, (_, index) => entry(index));
  const bound = 6_000;

  it("fills each page up to the bound and no further, and ends with done", async () => {
    const served: DaLedgerSnapshotEntry[] = [];
    let pages = 0;
    for (;;) {
      const { source } = counted(all.slice(served.length));
      const page = await daLedgerSnapshotPage(source, bound);
      pages += 1;
      expect(page.entries.length).toBeGreaterThan(1);
      expect(encodedPage(page).length).toBeLessThanOrEqual(bound);
      served.push(...page.entries);
      if (page.done) break;
      // The next entry would not have fitted.
      const next = all[served.length]!;
      expect(
        encodedPage({ entries: [...page.entries, next], done: false }).length,
      ).toBeGreaterThan(bound);
    }
    expect(served).toEqual(all);
    // Some pages hold more than 23 entries, whose array head is two bytes.
    expect(pages).toBeLessThan(all.length / 24);
  });

  it("gives done for a source of exactly the entries that fit", async () => {
    const first = await daLedgerSnapshotPage(counted(all).source, bound);
    expect(first.done).toBe(false);
    const exact = await daLedgerSnapshotPage(
      counted(first.entries).source,
      bound,
    );
    expect(exact).toEqual({ entries: first.entries, done: true });
  });

  it("sends an entry larger than the bound alone", async () => {
    const large = { outRef: b(0x01, 36), output: b(0x02, bound + 100) };
    expect(
      await daLedgerSnapshotPage(
        counted([large, entry(1), entry(2)]).source,
        bound,
      ),
    ).toEqual({ entries: [large], done: false });
    expect(await daLedgerSnapshotPage(counted([large]).source, bound)).toEqual({
      entries: [large],
      done: true,
    });
  });

  it("pulls at most one entry past the page and then closes the source", async () => {
    const { source, state } = counted(all);
    const page = await daLedgerSnapshotPage(source, bound);
    expect(page.done).toBe(false);
    expect(state.pulled).toBe(page.entries.length + 1);
    expect(state.closed).toBe(true);
  });

  it("gives the empty last page for an empty source", async () => {
    expect(await daLedgerSnapshotPage(counted([]).source, bound)).toEqual({
      entries: [],
      done: true,
    });
  });
});
