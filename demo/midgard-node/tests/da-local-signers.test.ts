import { describe, expect, it } from "vitest";

import {
  committeeSignerIndex,
  daLocalSigners,
  isStrictlyAscending,
  splitPackedHex,
  VERIFICATION_KEY_HEX_LENGTH,
} from "@/da/local-signers.js";

const OPERATOR_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";
const COSIGNER_SEED =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";

const BASE = {
  L1_OPERATOR_SEED_PHRASE: OPERATOR_SEED,
  NETWORK: "Preprod" as const,
};

describe("splitPackedHex", () => {
  it("splits a packed set into fixed-width lowercase chunks", () => {
    expect(
      splitPackedHex(
        ("AB".repeat(32) + "cd".repeat(32)).toString(),
        VERIFICATION_KEY_HEX_LENGTH,
        "DA committee",
      ),
    ).toEqual(["ab".repeat(32), "cd".repeat(32)]);
  });

  it("rejects non-hex, empty, and ragged input", () => {
    expect(() => splitPackedHex("zz", 64, "DA committee")).toThrow(
      /must be hex/,
    );
    expect(() => splitPackedHex("", 64, "DA committee")).toThrow(/non-empty/);
    expect(() => splitPackedHex("ab".repeat(33), 64, "DA committee")).toThrow(
      /multiple of 32 bytes/,
    );
  });
});

describe("isStrictlyAscending", () => {
  it("accepts ascending sets and rejects descending ones", () => {
    expect(isStrictlyAscending(["01", "02", "03"])).toBe(true);
    expect(isStrictlyAscending(["02", "01"])).toBe(false);
  });

  // The governor's walker uses `compare(prev, key) == Less`, so equal adjacent
  // elements are a script failure. This pins `<` rather than `<=`.
  it("rejects duplicates", () => {
    expect(isStrictlyAscending(["01", "01"])).toBe(false);
  });

  it("orders hex digits below letters, matching bytewise comparison", () => {
    expect(isStrictlyAscending(["09", "0a"])).toBe(true);
    expect(isStrictlyAscending(["0a", "09"])).toBe(false);
  });
});

describe("committeeSignerIndex", () => {
  const committee = "01".repeat(32) + "02".repeat(32) + "03".repeat(32);

  // The signer index is read off the committee rather than assumed to be zero,
  // because the committee is sorted by key and the local key may sit anywhere.
  it("returns the member's position, not zero", () => {
    expect(committeeSignerIndex(committee, "01".repeat(32))).toBe(0);
    expect(committeeSignerIndex(committee, "02".repeat(32))).toBe(1);
    expect(committeeSignerIndex(committee, "03".repeat(32))).toBe(2);
  });

  it("is case-insensitive on the looked-up key", () => {
    expect(committeeSignerIndex(committee, "02".repeat(32).toUpperCase())).toBe(
      1,
    );
  });

  it("returns null for a key outside the committee", () => {
    expect(committeeSignerIndex(committee, "ff".repeat(32))).toBeNull();
  });
});

describe("daLocalSigners", () => {
  it("yields the operator key alone when no cosigner is configured", () => {
    const signers = daLocalSigners(BASE);
    expect(signers).toHaveLength(1);
    expect(signers[0]!.role).toBe("operator");
    expect(signers[0]!.verificationKeyHex).toMatch(/^[0-9a-f]{64}$/);
    expect(signers[0]!.keyHashHex).toMatch(/^[0-9a-f]{56}$/);
  });

  it("adds a distinct cosigner key", () => {
    const signers = daLocalSigners({
      ...BASE,
      DA_COSIGNER_SEED_PHRASE: COSIGNER_SEED,
    });
    expect(signers).toHaveLength(2);
    expect(signers.map((signer) => signer.role)).toEqual([
      "operator",
      "cosigner",
    ]);
    expect(signers[0]!.verificationKeyHex).not.toBe(
      signers[1]!.verificationKeyHex,
    );
  });

  it("treats a blank cosigner seed as absent", () => {
    expect(
      daLocalSigners({ ...BASE, DA_COSIGNER_SEED_PHRASE: "   " }),
    ).toHaveLength(1);
  });

  // A cosigner seed that derives the operator's own key contributes no second
  // member and no second signature, so it must not inflate the local set.
  it("deduplicates a cosigner that repeats the operator key", () => {
    expect(
      daLocalSigners({ ...BASE, DA_COSIGNER_SEED_PHRASE: OPERATOR_SEED }),
    ).toHaveLength(1);
  });

  it("produces 64-byte Ed25519 signatures", () => {
    const [signer] = daLocalSigners(BASE);
    expect(signer!.sign(Buffer.from("midgard", "utf8"))).toMatch(
      /^[0-9a-f]{128}$/,
    );
  });
});
