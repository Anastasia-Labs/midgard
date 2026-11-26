import { describe, expect, it } from "vitest";

import {
  ProviderCapabilityError,
  ProviderHttpError,
  ProviderPayloadError,
} from "../src/index.js";
import { jsonResponse, makeProvider } from "./provider.protocol-info.js";

describe("MidgardNodeProvider.blocks", () => {
  it("queries GET /blocks without options and returns blocks + nextCursor", async () => {
    const provider = await makeProvider(async (input, init) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/blocks");
      expect(url.search).toBe("");
      expect(init?.method).toBeUndefined();
      return jsonResponse({
        blocks: [
          { headerHash: "aa".repeat(28), txCount: 2 },
          { headerHash: "bb".repeat(28), txCount: 0 },
        ],
        nextCursor: "cursor-2",
      });
    });

    await expect(provider.blocks()).resolves.toEqual({
      blocks: [
        { headerHash: "aa".repeat(28), txCount: 2 },
        { headerHash: "bb".repeat(28), txCount: 0 },
      ],
      nextCursor: "cursor-2",
    });
  });

  it("passes limit and cursor query parameters", async () => {
    const provider = await makeProvider(async (input) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/blocks");
      expect(url.searchParams.get("limit")).toBe("25");
      expect(url.searchParams.get("cursor")).toBe("opaque-cursor");
      return jsonResponse({ blocks: [], nextCursor: null });
    });

    await expect(
      provider.blocks({ limit: 25, cursor: "opaque-cursor" }),
    ).resolves.toEqual({
      blocks: [],
      nextCursor: null,
    });
  });

  it("rejects invalid query options before calling fetch", async () => {
    const provider = await makeProvider(async () => {
      throw new Error("fetch should not be called");
    });

    await expect(provider.blocks({ limit: 0 })).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(provider.blocks({ limit: -5 })).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(provider.blocks({ limit: 1.5 })).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(provider.blocks({ cursor: "   " })).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("rejects malformed /blocks payloads", async () => {
    for (const payload of [
      {},
      { blocks: "not-an-array", nextCursor: null },
      { blocks: [], nextCursor: 123 },
      { blocks: ["not-an-object"], nextCursor: null },
    ]) {
      const provider = await makeProvider(async () => jsonResponse(payload));
      await expect(provider.blocks()).rejects.toBeInstanceOf(
        ProviderPayloadError,
      );
    }
  });

  it("maps 404/405/501 to ProviderCapabilityError and 5xx to retryable ProviderHttpError", async () => {
    const unavailable = await makeProvider(async () =>
      jsonResponse({ error: "not found" }, 404),
    );
    await expect(unavailable.blocks()).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );

    const serverError = await makeProvider(async () =>
      jsonResponse({ error: "unavailable" }, 503),
    );
    await expect(serverError.blocks()).rejects.toMatchObject({
      statusCode: 503,
      retryable: true,
    });
    await expect(serverError.blocks()).rejects.toBeInstanceOf(
      ProviderHttpError,
    );
  });
});
