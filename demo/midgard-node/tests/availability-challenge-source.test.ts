import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  availabilityCommandCanonicalSource,
  AvailabilityHistoryUnavailableError,
  availabilityNodeLedgerSource,
} from "../src/commands/availability-challenge-source.js";
import type { ToolL1Access } from "../src/commands/l1-command-access.js";
import {
  availabilityKupmiosSource,
  kupoUnitHistory,
} from "../src/l1-external/kupmios-availability-source.js";
import type { FetchLike } from "../src/l1-external/kupmios-history.js";

const hex = (byte: string) => byte.repeat(32);

type PointStatus = "on_chain" | "not_on_chain" | "immutable";

/** A node-ledger access whose tip and point statuses the test sets. */
const ledger = (state: {
  tip: { slot: number; hash: string; blockNo: number };
  status: Map<string, PointStatus>;
}) => ({
  readTip: async () => state.tip,
  pointStatus: async (point: { slot: number; hash: string }) =>
    state.status.get(`${point.slot.toString()}:${point.hash}`) ??
    ("not_on_chain" as const),
});

describe("availability canonical source on the node ledger (a tool, before listen)", () => {
  it("anchors on the ledger tip and keeps it while the node still has it", async () => {
    const state = {
      tip: { slot: 100, hash: hex("11"), blockNo: 10 },
      status: new Map<string, PointStatus>([[`100:${hex("11")}`, "on_chain"]]),
    };
    const lucid = await Lucid(new Emulator([]), "Custom");
    const source = availabilityNodeLedgerSource({
      lucid,
      access: ledger(state),
    });
    const anchor = await source.readBoundary();
    expect(anchor).toEqual({
      pointId: `100:${hex("11")}`,
      slot: 100,
      blockNo: 10,
      blockHash: hex("11"),
    });
    await expect(
      source.assertCanonicalAncestor(anchor),
    ).resolves.toBeUndefined();
  });

  it("revokes the anchor once the node no longer has it, or it left the volatile window", async () => {
    const state = {
      tip: { slot: 100, hash: hex("11"), blockNo: 10 },
      status: new Map<string, PointStatus>([[`100:${hex("11")}`, "on_chain"]]),
    };
    const lucid = await Lucid(new Emulator([]), "Custom");
    const source = availabilityNodeLedgerSource({
      lucid,
      access: ledger(state),
    });
    const anchor = await source.readBoundary();
    state.status.set(`100:${hex("11")}`, "not_on_chain");
    await expect(source.assertCanonicalAncestor(anchor)).rejects.toThrow(
      /canonical generation changed.*no longer on the node's chain/,
    );
    state.status.set(`100:${hex("11")}`, "immutable");
    await expect(source.assertCanonicalAncestor(anchor)).rejects.toThrow(
      /canonical generation changed.*past the node's volatile window/,
    );
  });

  it("refuses every history read by name, pointing at --l1 kupmios", async () => {
    const lucid = await Lucid(new Emulator([]), "Custom");
    const source = availabilityNodeLedgerSource({
      lucid,
      access: ledger({
        tip: { slot: 1, hash: hex("11"), blockNo: 1 },
        status: new Map(),
      }),
    });
    const refusal = source.unitHistory({ policyId: "aa", assetName: "bb" });
    await expect(refusal).rejects.toBeInstanceOf(
      AvailabilityHistoryUnavailableError,
    );
    await expect(refusal).rejects.toMatchObject({
      reason: "availability_history_unavailable",
      access: "node",
    });
    await expect(refusal).rejects.toThrow(/--l1 kupmios/);
  });

  it("refuses the Blockfrost access, which has no history reader", async () => {
    const lucid = await Lucid(new Emulator([]), "Custom");
    await expect(
      availabilityCommandCanonicalSource({
        lucid,
        access: { kind: "blockfrost" } as ToolL1Access,
      }),
    ).rejects.toMatchObject({
      reason: "availability_history_unavailable",
      access: "blockfrost",
    });
  });
});

/** A fetch answering Kupo's /health, /checkpoints and Ogmios's tip. */
const kupmiosFetch = (state: {
  ogmios: { slot: number; id: string; height: number };
  kupo: { slot: number; etag: string };
  checkpoints: Map<number, { slot_no: number; header_hash: string }>;
}): FetchLike =>
  (async (url: string, init?: { body?: string }) => {
    if (url.startsWith("http://ogmios")) {
      const method = (JSON.parse(init?.body ?? "{}") as { method: string })
        .method;
      const result =
        method === "queryNetwork/tip"
          ? { slot: state.ogmios.slot, id: state.ogmios.id }
          : state.ogmios.height;
      return new Response(JSON.stringify({ result }));
    }
    const path = new URL(url).pathname;
    if (path === "/health")
      return new Response(
        `kupo_most_recent_checkpoint ${state.kupo.slot.toString()}\n`,
        { headers: { etag: `"${state.kupo.etag}"` } },
      );
    if (path.startsWith("/checkpoints/"))
      return new Response(
        JSON.stringify(
          state.checkpoints.get(Number(path.split("/").at(-1))) ?? null,
        ),
      );
    return new Response("not found", { status: 404 });
  }) as unknown as FetchLike;

describe("availability canonical source on Kupmios", () => {
  it("anchors only where Kupo and Ogmios agree on the tip", async () => {
    const state = {
      ogmios: { slot: 100, id: hex("11"), height: 10 },
      kupo: { slot: 100, etag: hex("11") },
      checkpoints: new Map([[100, { slot_no: 100, header_hash: hex("11") }]]),
    };
    const lucid = await Lucid(new Emulator([]), "Custom");
    const source = availabilityKupmiosSource({
      lucid,
      kupoUrl: "http://kupo",
      ogmiosUrl: "http://ogmios",
      fetchImpl: kupmiosFetch(state),
    });
    const anchor = await source.readBoundary();
    expect(anchor).toEqual({
      pointId: `100:${hex("11")}`,
      slot: 100,
      blockNo: 10,
      blockHash: hex("11"),
    });
    await expect(
      source.assertCanonicalAncestor(anchor),
    ).resolves.toBeUndefined();
    // A rival block at the anchor's slot revokes it.
    state.checkpoints.set(100, { slot_no: 100, header_hash: hex("22") });
    await expect(source.assertCanonicalAncestor(anchor)).rejects.toThrow(
      /canonical generation changed/,
    );
    // Kupo behind Ogmios: no boundary.
    state.kupo = { slot: 99, etag: hex("33") };
    await expect(source.readBoundary()).rejects.toThrow(
      /Kupo and Ogmios aligned/,
    );
  });

  it("reads a unit's history from Kupo, inline datums only", async () => {
    const seen: string[] = [];
    const fetchImpl = (async (url: string) => {
      seen.push(url);
      return new Response(
        JSON.stringify([
          { datum_type: "inline", datum: "d87980" },
          { datum_type: "hash", datum: "d87a80" },
          {},
        ]),
      );
    }) as unknown as FetchLike;
    await expect(
      kupoUnitHistory({ kupoUrl: "http://kupo", fetchImpl })({
        policyId: "aa",
        assetName: "bb",
      }),
    ).resolves.toEqual(["d87980", null, null]);
    expect(seen).toEqual(["http://kupo/matches/aa.bb?resolve_hashes"]);
    const broken = (async () =>
      new Response(JSON.stringify({ hint: "no" }))) as unknown as FetchLike;
    await expect(
      kupoUnitHistory({ kupoUrl: "http://kupo", fetchImpl: broken })({
        policyId: "aa",
        assetName: "bb",
      }),
    ).rejects.toThrow(/no match array/);
  });
});
