import { describe, expect, it } from "vitest";
import {
  ADMIN_ROUTE_PATHS,
  authorizeAdminRoute,
  extractSubmitTxHex,
  isAdminRoutePath,
  validateSubmitTxHex,
} from "@/commands/listen-utils.js";

describe("listen admin auth helpers", () => {
  it("detects admin route paths", () => {
    expect(isAdminRoutePath("/init")).toBe(true);
    expect(isAdminRoutePath("/commit")).toBe(true);
    expect(isAdminRoutePath("/stateQueue")).toBe(true);
    expect(isAdminRoutePath("/logBlocksDB")).toBe(true);
    expect(isAdminRoutePath("/logGlobals")).toBe(true);
    expect(isAdminRoutePath("/tx")).toBe(false);
    expect(isAdminRoutePath("/submit")).toBe(false);
    expect(ADMIN_ROUTE_PATHS.size).toBeGreaterThan(0);
  });

  it("requires admin key to be configured", () => {
    const result = authorizeAdminRoute("", undefined);
    expect(result.authorized).toBe(false);
    if (result.authorized) {
      throw new Error("expected unauthorized result");
    }
    expect(result.status).toBe(403);
  });

  it("rejects missing or invalid admin key", () => {
    const missing = authorizeAdminRoute("secret", undefined);
    expect(missing.authorized).toBe(false);
    if (missing.authorized) {
      throw new Error("expected unauthorized result");
    }
    expect(missing.status).toBe(401);

    const wrong = authorizeAdminRoute("secret", "wrong");
    expect(wrong.authorized).toBe(false);
    if (wrong.authorized) {
      throw new Error("expected unauthorized result");
    }
    expect(wrong.status).toBe(401);
  });

  it("accepts matching admin key", () => {
    const ok = authorizeAdminRoute("secret", "secret");
    expect(ok.authorized).toBe(true);
  });
});

describe("submit admission helpers", () => {
  it("extracts tx hex from canonical and camelCase keys", () => {
    expect(extractSubmitTxHex({ tx_cbor: "abcd" })).toBe("abcd");
    expect(extractSubmitTxHex({ txCbor: "1234" })).toBe("1234");
    expect(extractSubmitTxHex({ tx_cbor: 42 })).toBeUndefined();
    expect(extractSubmitTxHex(null)).toBeUndefined();
  });

  it("rejects non-hex, odd-length, and oversized tx payloads", () => {
    const invalidHex = validateSubmitTxHex("zz", 4);
    expect(invalidHex.ok).toBe(false);
    if (invalidHex.ok) {
      throw new Error("expected invalid result");
    }
    expect(invalidHex.status).toBe(400);

    const oddLength = validateSubmitTxHex("abc", 4);
    expect(oddLength.ok).toBe(false);
    if (oddLength.ok) {
      throw new Error("expected invalid result");
    }
    expect(oddLength.status).toBe(400);

    const oversized = validateSubmitTxHex("00".repeat(6), 5);
    expect(oversized.ok).toBe(false);
    if (oversized.ok) {
      throw new Error("expected invalid result");
    }
    expect(oversized.status).toBe(413);
  });

  it("accepts valid tx payload and returns byte length", () => {
    const accepted = validateSubmitTxHex("00".repeat(5), 5);
    expect(accepted.ok).toBe(true);
    if (!accepted.ok) {
      throw new Error("expected accepted result");
    }
    expect(accepted.byteLength).toBe(5);
  });
});
