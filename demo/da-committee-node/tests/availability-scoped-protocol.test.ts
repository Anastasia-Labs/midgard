import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { committeeScopedProtocolDigest } from "../src/l1/availability-scoped-protocol.js";

const limits = {
  requestRefusalMs: 1000,
  httpResponseBytes: 10000,
  webSocketMessageBytes: 10000,
  rawUtxos: 2,
};
const read = async (reply: unknown) => {
  const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
  try {
    return await committeeScopedProtocolDigest({
      ogmiosUrl: "ws://fixture",
      limits,
      fetchImpl: async (url, init) => {
        expect(String(url)).toBe("http://fixture/");
        expect(JSON.parse(String(init?.body))).toMatchObject({
          method: "queryLedgerState/protocolParameters",
          id: "committee-promise-protocol",
        });
        expect(init?.signal).toBeInstanceOf(AbortSignal);
        return new Response(
          typeof reply === "string" ? reply : JSON.stringify(reply),
        );
      },
    })(scope);
  } finally {
    scope.close();
  }
};
describe("fresh native protocol binding", () => {
  it("hashes the actual fresh result canonically rather than a cached Lucid configuration", async () => {
    const result = {
      maxTransactionSize: { bytes: 16384 },
      minFeeCoefficient: 44,
    };
    const reply = { jsonrpc: "2.0", id: "committee-promise-protocol", result };
    expect(await read(reply)).toBe(
      createHash("sha256")
        .update(canonicalJson(result, "fixture"))
        .digest("hex"),
    );
    expect(
      await read({ ...reply, result: { ...result, minFeeCoefficient: 45 } }),
    ).not.toBe(await read(reply));
  });
  it("binds the recorded native decimal multiplier and every changed parameter", async () => {
    const parameters = JSON.parse(
      readFileSync(
        new URL("./fixtures/native-protocol-parameters.json", import.meta.url),
        "utf8",
      ),
    );
    expect(parameters.minFeeReferenceScripts.multiplier).toBe(1.2);
    const reply = {
      jsonrpc: "2.0",
      id: "committee-promise-protocol",
      result: parameters,
    };
    const digest = await read(reply);
    expect(digest).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      await read({
        ...reply,
        result: Object.fromEntries(Object.entries(parameters).reverse()),
      }),
    ).toBe(digest);
    expect(
      await read({
        ...reply,
        result: {
          ...parameters,
          minFeeReferenceScripts: {
            ...parameters.minFeeReferenceScripts,
            multiplier: 1.3,
          },
        },
      }),
    ).not.toBe(digest);
    expect(() => canonicalJson(parameters, "consensus fixture")).toThrow(
      "safe integers",
    );
  });
  it.each(["1e309", "-1e309", "9007199254740992"])(
    "refuses non-finite or unsafe integer protocol evidence %s",
    async (value) => {
      await expect(
        read(
          `{"jsonrpc":"2.0","id":"committee-promise-protocol","result":{"value":${value}}}`,
        ),
      ).rejects.toThrow();
    },
  );
  it.each([
    { jsonrpc: "2.0", id: "other", result: { value: 1 } },
    {
      jsonrpc: "2.0",
      id: "committee-promise-protocol",
      error: { code: 1 },
      result: { value: 1 },
    },
    { jsonrpc: "2.0", id: "committee-promise-protocol", result: {} },
    { jsonrpc: "2.0", id: "committee-promise-protocol", result: [] },
  ])(
    "refuses mismatched, failed or unavailable parameter evidence",
    async (reply) => {
      await expect(read(reply)).rejects.toThrow();
    },
  );
});
