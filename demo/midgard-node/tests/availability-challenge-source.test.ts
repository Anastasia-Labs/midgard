import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { availabilityCommandCanonicalSource } from "../src/commands/availability-challenge-source.js";

afterEach(() => vi.unstubAllGlobals());

describe("availability command canonical source", () => {
  it("refuses a lagging query index before actuation", async () => {
    const hash = "11".repeat(32);
    vi.stubGlobal("fetch", async (url: string) =>
      url.includes("ogmios")
        ? Response.json({ result: { id: hash, slot: 100, height: 10 } })
        : new Response("kupo_most_recent_checkpoint 99\n", {
            headers: { etag: `"${hash}"` },
          }),
    );
    const lucid = await Lucid(new Emulator([]), "Custom");
    const source = availabilityCommandCanonicalSource({
      lucid,
      kupoUrl: "http://kupo",
      ogmiosUrl: "http://ogmios",
    });
    await expect(source.readBoundary()).rejects.toThrow(
      /aligned at the same canonical tip/,
    );
  });

  it("revokes a captured generation when a recovered source reports a different canonical ancestor", async () => {
    const original = "11".repeat(32);
    let current = original;
    vi.stubGlobal("fetch", async (url: string) => {
      if (url.includes("ogmios"))
        return Response.json({
          result: { id: current, slot: 100, height: 10 },
        });
      if (url.includes("checkpoints"))
        return Response.json({ slot_no: 100, header_hash: current });
      return new Response("kupo_most_recent_checkpoint 100\n", {
        headers: { etag: `"${current}"` },
      });
    });
    const lucid = await Lucid(new Emulator([]), "Custom");
    const source = availabilityCommandCanonicalSource({
      lucid,
      kupoUrl: "http://kupo",
      ogmiosUrl: "http://ogmios",
    });
    const anchor = await source.readBoundary();
    await expect(
      source.assertCanonicalAncestor(anchor),
    ).resolves.toBeUndefined();
    current = "22".repeat(32);
    await expect(source.assertCanonicalAncestor(anchor)).rejects.toThrow(
      /canonical generation changed/,
    );
  });
});
