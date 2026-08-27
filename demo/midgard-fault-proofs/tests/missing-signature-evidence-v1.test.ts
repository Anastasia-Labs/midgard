import type { MidgardAddressWitness } from "@al-ft/midgard-sdk";
import { missingSignatureVkeyHashV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { selectMissingSignatureAccusationV1 } from "../src/missing-signature/index.js";

const witness = (vkeyByte: string): MidgardAddressWitness => ({
  verification_key: vkeyByte.repeat(32),
  signature: "aa".repeat(64),
});

describe("missing-signature evidence v1", () => {
  it("selects the first absent signer and treats a present invalid signature as present", () => {
    const present = witness("11");
    const missingVkey = "22".repeat(32);
    expect(
      selectMissingSignatureAccusationV1({
        requiredSignerHashes: [
          missingSignatureVkeyHashV1(present.verification_key),
          missingSignatureVkeyHashV1(missingVkey),
        ],
        // Signature content is intentionally garbage: presence is by key hash.
        addrTxWits: [present],
      }),
    ).toStrictEqual({
      index: 1n,
      hash: missingSignatureVkeyHashV1(missingVkey),
    });
  });

  it("returns null for an honest witness set", () => {
    const present = witness("33");
    expect(
      selectMissingSignatureAccusationV1({
        requiredSignerHashes: [
          missingSignatureVkeyHashV1(present.verification_key),
        ],
        addrTxWits: [present],
      }),
    ).toBeNull();
  });
});
