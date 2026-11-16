/**
 * The parent ledger a foreign block is replayed on is this node's own: its
 * entries (`confirmed_ledger` with the processed deltas) and the parent's
 * root (the frontier's or a landed row's) are both read from local stores.
 * Entries that do not derive that root are a local ledger divergence, a
 * `local_fault` naming its check and counted under it, never `invalid`; the
 * same block imports on entries that derive it. A block whose header names
 * another parent root is still `invalid`, and so is one whose replay on a
 * parent ledger deriving its root misses the post-state root it commits.
 * A payload fault that does not depend on the parent ledger (a key named
 * twice, a count its header or body does not commit) is judged `invalid`
 * before the parent's entries are checked, on any parent ledger.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric } from "effect";
import { describe, expect, it } from "vitest";

import type * as Ledger from "../src/database/utils/ledger.js";
import { localLedgerDivergenceCounter } from "../src/mpf/local-ledger-divergence.js";
import { fixture, rebind, verdict } from "./foreign-block-import.fixture.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

/** An entry the parent's root does not hold. */
const stray: Ledger.MinimalEntry = {
  outref: makeOutRefCbor(simDigest("divergence:stray"), 0),
  output: simOutput(2_000_000n),
};

const divergences = () =>
  Effect.runPromise(
    Metric.value(
      Metric.tagged(localLedgerDivergenceCounter, "check", "landed_parent"),
    ),
  ).then(({ count }) => count);

describe("a foreign block replayed on a parent ledger that does not derive the parent's root", () => {
  it("is a local fault naming the divergence, never invalid, and imports on the parent's ledger", async () => {
    const payload = await fixture();
    const before = await divergences();
    const result = await verdict(payload, {
      parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      parentEntries: [stray],
    });
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left).toMatchObject({
        _tag: "ForeignBlockVerificationError",
        foreignHeaderHash: payload.block_body.header_hash,
        reason: "local_fault",
      });
      expect(result.left).toMatchObject({
        detail: expect.stringContaining(
          `local ledger divergence (landed_parent): the ledger at ${payload.block_body.header.prevHeaderHash}`,
        ),
      });
    }
    expect(await divergences()).toBe(before + 1);

    // The same block on the ledger its parent's root commits.
    const imported = await verdict(payload, {
      parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      parentEntries: [],
    });
    expect(imported._tag).toBe("Right");
    expect(await divergences()).toBe(before + 1);
  });

  it("still calls a header naming another parent root invalid", async () => {
    const payload = await fixture();
    const result = await verdict(payload, {
      parentUtxosRoot: "ab".repeat(32),
      parentEntries: [stray],
    });
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "invalid",
        detail: "foreign header does not extend the verified parent",
      });
  });

  it("still calls a block committing another post-state root on a parent ledger that derives its root invalid", async () => {
    const payload = await fixture();
    const wrongPostState = await rebind({
      ...payload,
      block_body: {
        ...payload.block_body,
        header: { ...payload.block_body.header, utxosRoot: "ab".repeat(32) },
      },
    });
    const before = await divergences();
    const result = await verdict(wrongPostState, {
      parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      parentEntries: [],
    });
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "invalid",
        detail: "foreign replay UTxO root mismatch",
      });
    expect(await divergences()).toBe(before);
  });

  it("calls a payload naming a key twice invalid on a parent ledger that does not derive the parent's root", async () => {
    const payload = await fixture();
    const traces = payload.block_body.validation_traces;
    const before = await divergences();
    const result = await verdict(
      {
        ...payload,
        block_body: {
          ...payload.block_body,
          validation_traces: [...traces, traces[0]!],
        },
      },
      { parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT, parentEntries: [stray] },
    );
    expect(result).toMatchObject({
      _tag: "Left",
      left: {
        reason: "invalid",
        detail: "foreign DA payload names a validation_traces key twice",
      },
    });
    expect(await divergences()).toBe(before);
  });

  it("calls a payload whose counts its body does not commit invalid on a parent ledger that does not derive the parent's root", async () => {
    const payload = await fixture();
    const before = await divergences();
    const result = await verdict(
      {
        ...payload,
        block_body: {
          ...payload.block_body,
          counts: { ...payload.block_body.counts, depositCount: 5n },
        },
      },
      { parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT, parentEntries: [stray] },
    );
    expect(result).toMatchObject({
      _tag: "Left",
      left: {
        reason: "invalid",
        detail: "foreign DA commitment mismatch: depositCount",
      },
    });
    expect(await divergences()).toBe(before);
  });
});
