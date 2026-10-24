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
    .mockResolvedValueOnce(response(tip))
    .mockResolvedValueOnce(response([{ slot_no: 100, header_hash: blockHash }]))
    .mockResolvedValueOnce(response(tip));
  vi.stubGlobal("fetch", fetch);
  expect(
    await synchronizePublicationIndexer("http://node", "http://indexer"),
  ).toBe(100);
  expect(fetch).toHaveBeenCalledTimes(5);
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

it("withdraws checkpoint evidence if the node rolls back during catch-up", async () => {
  const replacement = { result: { slot: 99, id: "ef".repeat(32) } };
  vi.stubGlobal(
    "fetch",
    vi
      .fn()
      .mockResolvedValueOnce(response(tip))
      .mockResolvedValueOnce(
        response([{ slot_no: 100, header_hash: blockHash }]),
      )
      .mockResolvedValueOnce(response(replacement))
      .mockResolvedValueOnce(
        response([{ slot_no: 99, header_hash: "ef".repeat(32) }]),
      )
      .mockResolvedValueOnce(response(replacement)),
  );
  expect(
    await synchronizePublicationIndexer("http://node", "http://indexer"),
  ).toBe(99);
});

it("follows a replacement tip when the abandoned checkpoint was never indexed", async () => {
  const replacement = { result: { slot: 99, id: "ef".repeat(32) } };
  vi.stubGlobal(
    "fetch",
    vi
      .fn()
      .mockResolvedValueOnce(response(tip))
      .mockResolvedValueOnce(response([]))
      .mockResolvedValueOnce(response(replacement))
      .mockResolvedValueOnce(
        response([{ slot_no: 99, header_hash: "ef".repeat(32) }]),
      )
      .mockResolvedValueOnce(response(replacement)),
  );
  expect(
    await synchronizePublicationIndexer("http://node", "http://indexer"),
  ).toBe(99);
});

it.each([
  ["a timed-out read", new DOMException("aborted", "TimeoutError")],
  ["a dropped connection", new TypeError("fetch failed")],
])(
  "retries %s within the barrier instead of abandoning publication",
  async (_label, failure) => {
    const fetch = vi
      .fn()
      .mockRejectedValueOnce(failure)
      .mockResolvedValueOnce(response(tip))
      .mockRejectedValueOnce(failure)
      .mockResolvedValueOnce(
        response([{ slot_no: 100, header_hash: blockHash }]),
      )
      .mockResolvedValueOnce(response(tip));
    vi.stubGlobal("fetch", fetch);
    expect(
      await synchronizePublicationIndexer("http://node", "http://indexer"),
    ).toBe(100);
    expect(fetch).toHaveBeenCalledTimes(5);
  },
);

it("gives up on a persistently unreachable node at the barrier deadline", async () => {
  vi.useFakeTimers();
  try {
    vi.stubGlobal(
      "fetch",
      vi.fn().mockRejectedValue(new DOMException("aborted", "TimeoutError")),
    );
    const barrier = expect(
      synchronizePublicationIndexer("http://node", "http://indexer"),
    ).rejects.toThrow("aborted");
    await vi.advanceTimersByTimeAsync(61_000);
    await barrier;
  } finally {
    vi.useRealTimers();
  }
});
