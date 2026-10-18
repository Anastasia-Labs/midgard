import { describe, expect, it, vi } from "vitest";

import { fixture, verdict } from "./foreign-block-import.fixture.js";
import { committedBlock, fundedBy } from "./import-check.fixture.js";

/**
 * The import's own computations failing on a block they handle (its
 * payload roots, its header hash, its check bookkeeping) are `local_fault`,
 * and the same block imports once they work again. A block whose payload
 * cannot form its roots is still `invalid`.
 */

const fault = vi.hoisted(() => ({
  roots: false,
  hash: false,
  bookkeeping: false,
}));

vi.mock(
  "../src/workers/commit-block-header/da-payload.compute-da-payload-roots.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../src/workers/commit-block-header/da-payload.compute-da-payload-roots.js")
      >();
    const { Effect } = await import("effect");
    return {
      ...actual,
      computeDaPayloadRoots: (
        ...args: Parameters<typeof actual.computeDaPayloadRoots>
      ) =>
        fault.roots
          ? Effect.fail(new Error("scratch store unavailable"))
          : actual.computeDaPayloadRoots(...args),
    };
  },
);

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    hashBlockHeader: (...args: Parameters<typeof actual.hashBlockHeader>) =>
      fault.hash
        ? Effect.fail(
            new actual.HashingError({
              message: "hash unavailable",
              cause: undefined,
            }),
          )
        : actual.hashBlockHeader(...args),
  };
});

vi.mock("../src/import-check/kill.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/import-check/kill.js")>();
  return {
    ...actual,
    importCheckCommittedRunBytes: (
      ...args: Parameters<typeof actual.importCheckCommittedRunBytes>
    ) => {
      if (fault.bookkeeping) throw new Error("bookkeeping fault");
      return actual.importCheckCommittedRunBytes(...args);
    },
  };
});

describe("a foreign block import failing locally", { timeout: 60_000 }, () => {
  it("holds on a payload-root failure, then imports the block", async () => {
    const payload = await fixture();
    fault.roots = true;
    const held = await verdict(payload);
    fault.roots = false;
    expect(held).toMatchObject({
      _tag: "Left",
      left: {
        reason: "local_fault",
        detail: expect.stringContaining("payload roots failed locally"),
      },
    });
    expect((await verdict(payload))._tag).toBe("Right");
  });

  it("holds on a header-hash failure, then imports the block", async () => {
    const payload = await fixture();
    fault.hash = true;
    const held = await verdict(payload);
    fault.hash = false;
    expect(held).toMatchObject({
      _tag: "Left",
      left: {
        reason: "local_fault",
        detail: expect.stringContaining("header hashing failed locally"),
      },
    });
    expect((await verdict(payload))._tag).toBe("Right");
  });

  it("holds on a check-bookkeeping failure, then imports the block", async () => {
    const parent = [fundedBy(0x61, "accept")];
    const block = await committedBlock(parent, [
      { input: parent[0]!.outRef, body: "accept", next: "accept" },
    ]);
    fault.bookkeeping = true;
    const held = await block.imported();
    fault.bookkeeping = false;
    expect(held).toMatchObject({
      _tag: "Left",
      left: {
        reason: "local_fault",
        detail: expect.stringContaining("import check bookkeeping"),
      },
    });
    const imported = await block.imported();
    expect(imported._tag).toBe("Right");
  });

  it("refuses a payload that names one key twice, whose roots cannot form", async () => {
    const payload = await fixture();
    const traces = payload.block_body.validation_traces;
    expect(traces.length).toBeGreaterThan(0);
    const repeated = {
      ...payload,
      block_body: {
        ...payload.block_body,
        validation_traces: [...traces, traces[0]!],
      },
    };
    expect(await verdict(repeated)).toMatchObject({
      _tag: "Left",
      left: {
        reason: "invalid",
        detail: "foreign DA payload names a validation_traces key twice",
      },
    });
  });
});
