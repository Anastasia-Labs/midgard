import fs from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import {
  ADMIN_ROUTE_PATHS,
  authorizeAdminRoute,
  extractSubmitTxHex,
  extractSubmitTxHexFromQueryParams,
  isAdminRoutePath,
  normalizeSubmitTxHexToNative,
  validateSubmitTxHex,
} from "@/commands/listen-utils.js";
import { cardanoTxBytesToMidgardNativeTxFullBytes } from "@/midgard-tx-codec/index.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];

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

  it("extracts tx hex from query params", () => {
    expect(
      extractSubmitTxHexFromQueryParams({
        tx_cbor: "abcd",
      }),
    ).toBe("abcd");
    expect(
      extractSubmitTxHexFromQueryParams({
        txCbor: "1234",
      }),
    ).toBe("1234");
    expect(
      extractSubmitTxHexFromQueryParams({
        tx_cbor: ["abcd"],
      }),
    ).toBeUndefined();
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

  it("normalizes Cardano tx bytes into Midgard-native bytes", () => {
    const cardanoHex = txFixtures[0].cborHex;
    const normalized = normalizeSubmitTxHexToNative(cardanoHex);
    expect(normalized.ok).toBe(true);
    if (!normalized.ok) {
      throw new Error("expected normalized tx");
    }
    expect(normalized.source).toBe("cardano-converted");
    expect(normalized.txIdHex.length).toBe(64);
    expect(normalized.txCbor.length).toBeGreaterThan(0);
    expect(normalized.txBodyHashForWitnesses?.length).toBe(32);
  });

  it("keeps native tx bytes unchanged when payload is already Midgard-native", () => {
    const cardanoBytes = Buffer.from(txFixtures[0].cborHex, "hex");
    const nativeBytes = cardanoTxBytesToMidgardNativeTxFullBytes(cardanoBytes);
    const normalized = normalizeSubmitTxHexToNative(nativeBytes.toString("hex"));
    expect(normalized.ok).toBe(true);
    if (!normalized.ok) {
      throw new Error("expected normalized tx");
    }
    expect(normalized.source).toBe("native");
    expect(normalized.txCbor.equals(nativeBytes)).toBe(true);
    expect(normalized.txBodyHashForWitnesses).toBeUndefined();
  });

  it("returns an invalid payload result for bytes that are neither native nor convertible Cardano", () => {
    const normalized = normalizeSubmitTxHexToNative("ffff");
    expect(normalized.ok).toBe(false);
    if (normalized.ok) {
      throw new Error("expected invalid payload result");
    }
    expect(normalized.error).toBe("Invalid transaction CBOR payload");
  });
});
