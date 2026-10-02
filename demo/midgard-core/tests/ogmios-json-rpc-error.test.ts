import { describe, expect, it } from "vitest";

import {
  decodeOgmiosJsonRpcError,
  formatOgmiosJsonRpcError,
  isTransientOgmiosJsonRpcErrorCode,
  isTransientOgmiosJsonRpcFailure,
  OgmiosJsonRpcError,
  ogmiosJsonRpcErrorCodeOf,
} from "../src/ogmios-json-rpc-error.js";

describe("Ogmios JSON-RPC error classification", () => {
  it.each([2000, 2001, 2002, 2003, -32000, -32603])(
    "treats %s as transient",
    (code) => {
      expect(isTransientOgmiosJsonRpcErrorCode(code)).toBe(true);
      expect(new OgmiosJsonRpcError("x", { code }).transient).toBe(true);
    },
  );

  it.each([
    1000, 1001, 2004, 3005, 3117, 3997, 4000, -32700, -32600, -32601, -32602,
    -32001, 0, 9999,
  ])("treats %s as a refusal", (code) => {
    expect(isTransientOgmiosJsonRpcErrorCode(code)).toBe(false);
    expect(new OgmiosJsonRpcError("x", { code }).transient).toBe(false);
  });

  it.each([
    ["a missing code", { message: "x" }],
    ["a string code", { code: "2003" }],
    ["a fractional code", { code: 2003.5 }],
    ["an unsafe bigint code", { code: 2n ** 64n }],
    ["null", null],
    ["an array", [2003]],
    ["a string", "2003"],
  ])("reads no code from %s and refuses", (_label, error) => {
    const answer = decodeOgmiosJsonRpcError(error);
    expect(answer.code).toBeUndefined();
    expect(isTransientOgmiosJsonRpcErrorCode(answer.code)).toBe(false);
  });

  it("reads a lossless-parser bigint code and the message and data", () => {
    expect(
      decodeOgmiosJsonRpcError({ code: 2003n, message: "gone", data: 1 }),
    ).toEqual({ code: 2003, message: "gone", data: 1 });
  });

  it("formats bigint members losslessly", () => {
    expect(formatOgmiosJsonRpcError({ code: 2003n, data: 2n ** 64n })).toBe(
      '{"code":"2003","data":"18446744073709551616"}',
    );
  });

  it("reads the code of this package's error and of the Kupmios provider's", () => {
    expect(
      ogmiosJsonRpcErrorCodeOf(new OgmiosJsonRpcError("x", { code: 2001 })),
    ).toBe(2001);
    const kupmiosShape = Object.assign(
      new Error("Ogmios JSON-RPC error 2003"),
      {
        name: "OgmiosJsonRpcError",
        kind: "json_rpc",
        code: 2003,
        retryable: false,
      },
    );
    expect(ogmiosJsonRpcErrorCodeOf(kupmiosShape)).toBe(2003);
    expect(isTransientOgmiosJsonRpcFailure(kupmiosShape)).toBe(true);
  });

  it("reads no code from an error that only carries a numeric code", () => {
    const other = Object.assign(new Error("x"), { code: 2003 });
    expect(ogmiosJsonRpcErrorCodeOf(other)).toBeUndefined();
    expect(isTransientOgmiosJsonRpcFailure(other)).toBe(false);
    expect(
      isTransientOgmiosJsonRpcFailure(
        Object.assign(new Error("x"), {
          name: "OgmiosJsonRpcError",
          code: 2003,
        }),
      ),
    ).toBe(false);
  });
});
