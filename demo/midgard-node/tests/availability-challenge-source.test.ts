import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { availabilityCommandCanonicalSource } from "../src/commands/availability-challenge-source.js";
import type { ToolL1Access } from "../src/commands/l1-command-access.js";
import {
  availabilityKupmiosSource,
  kupoUnitHistory,
} from "../src/l1-external/kupmios-availability-source.js";
import type { FetchLike } from "../src/l1-external/kupmios-history.js";

const hex = (byte: string) => byte.repeat(32);

describe("availability canonical source without a history reader", () => {
  for (const kind of ["node", "blockfrost"] as const)
    it(`refuses the ${kind} access, naming --l1 kupmios`, async () => {
      const lucid = await Lucid(new Emulator([]), "Custom");
      const refusal = availabilityCommandCanonicalSource({
        lucid,
        access: { kind } as ToolL1Access,
      });
      await expect(refusal).rejects.toMatchObject({
        reason: "availability_history_unavailable",
        access: kind,
      });
      await expect(refusal).rejects.toThrow(/--l1 kupmios/);
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
