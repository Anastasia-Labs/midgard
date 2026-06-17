import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { deriveOperatorDaParams } from "@/transactions/initialization.js";

const OPERATOR_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";

describe("DA attestation configuration", () => {
  it("builds fresh deployment DA params from configured committee and threshold", async () => {
    const committee = "01".repeat(32) + "02".repeat(32) + "03".repeat(32);
    const params = await Effect.runPromise(
      deriveOperatorDaParams({
        L1_OPERATOR_SEED_PHRASE: OPERATOR_SEED,
        NETWORK: "Preprod",
        DA_COMMITTEE_HEX: committee,
        DA_THRESHOLD: 2n,
      }),
    );
    expect(params.committee).toBe(committee);
    expect(params.da_threshold).toBe(2n);
    expect(params.committee_signers_hash).toBe(
      Buffer.from(blake2b(Buffer.from(committee, "hex"), { dkLen: 32 })).toString(
        "hex",
      ),
    );
  });

  it("rejects DA thresholds larger than the configured committee", async () => {
    await expect(
      Effect.runPromise(
        deriveOperatorDaParams({
          L1_OPERATOR_SEED_PHRASE: OPERATOR_SEED,
          NETWORK: "Preprod",
          DA_COMMITTEE_HEX: "01".repeat(32),
          DA_THRESHOLD: 2n,
        }),
      ),
    ).rejects.toThrow(/Invalid DA threshold/);
  });
});
