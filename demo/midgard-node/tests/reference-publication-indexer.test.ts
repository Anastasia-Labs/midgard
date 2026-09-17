import { afterEach, expect, it, vi } from "vitest";

import { synchronizePublicationIndexer } from "./helpers/reference-publication-chain.js";

const blockHash = "ab".repeat(32);
const tip = { result: { slot: 100, id: blockHash } };
const response = (body: unknown) => new Response(JSON.stringify(body));

afterEach(() => vi.unstubAllGlobals());

it("requires the exact canonical checkpoint hash as well as its slot", async () => {
  const fetch = vi
    .fn()
    .mockResolvedValueOnce(response(tip))
    .mockResolvedValueOnce(
      response([{ slot_no: 100, header_hash: "cd".repeat(32) }]),
    )
    .mockResolvedValueOnce(
      response([{ slot_no: 100, header_hash: blockHash }]),
    );
  vi.stubGlobal("fetch", fetch);
  expect(
    await synchronizePublicationIndexer("http://node", "http://indexer"),
  ).toBe(100);
  expect(fetch).toHaveBeenCalledTimes(3);
});

it.each([
  null,
  { result: "origin" },
  { result: { slot: 100 } },
  { result: { slot: -1, id: blockHash } },
  { result: { slot: 1.5, id: blockHash } },
  { result: { slot: Number.MAX_SAFE_INTEGER + 1, id: blockHash } },
  { result: { slot: "100", id: blockHash } },
  { result: { slot: 100, id: "invalid" } },
  { ...tip, error: { code: -1 } },
])("refuses malformed canonical node responses: %j", async (body) => {
  vi.stubGlobal("fetch", vi.fn().mockResolvedValue(response(body)));
  await expect(
    synchronizePublicationIndexer("http://node", "http://indexer"),
  ).rejects.toThrow("Cannot establish canonical publication tip");
});

it.each([
  null,
  { slot_no: 100, header_hash: blockHash },
  [null],
  [{ slot_no: 100 }],
  [{ slot_no: "100", header_hash: blockHash }],
])("refuses malformed indexer checkpoint responses: %j", async (body) => {
  vi.stubGlobal(
    "fetch",
    vi
      .fn()
      .mockResolvedValueOnce(response(tip))
      .mockResolvedValueOnce(response(body)),
  );
  await expect(
    synchronizePublicationIndexer("http://node", "http://indexer"),
  ).rejects.toThrow("Cannot read publication indexer checkpoints");
});
