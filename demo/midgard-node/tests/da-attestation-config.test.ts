import * as SDK from "@al-ft/midgard-sdk";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect, Logger, LogLevel } from "effect";
import { describe, expect, it } from "vitest";

import { deriveOperatorDaParams } from "../src/transactions/initialization.js";

const OPERATOR_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";
const COSIGNER_SEED =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";

const BASE = {
  L1_OPERATOR_SEED_PHRASE: OPERATOR_SEED,
  NETWORK: "Preprod" as const,
};

/** Independently derives the payment vkey and key hash a seed contributes. */
const seedKeys = (seedPhrase: string) => {
  const wallet = walletFromSeed(seedPhrase, { network: "Preprod" });
  const publicKey = CML.PrivateKey.from_bech32(wallet.paymentKey).to_public();
  return {
    vkey: Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
    keyHash: publicKey.hash().to_hex(),
  };
};

const blake2b256Hex = (hex: string): string =>
  Buffer.from(blake2b(Buffer.from(hex, "hex"), { dkLen: 32 })).toString("hex");

/**
 * Runs a success case with the default logger replaced, so the ruling's
 * explanatory warning is measured as an emitted log line rather than inferred
 * from the absence of an error.
 */
const daParamsWithLogs = async (
  config: Parameters<typeof deriveOperatorDaParams>[0],
): Promise<{
  readonly params: SDK.DaParamsDatum;
  readonly warnings: readonly string[];
}> => {
  const warnings: string[] = [];
  const capturingLogger = Logger.make(({ logLevel, message }) => {
    if (logLevel !== LogLevel.Warning) {
      return;
    }
    warnings.push(
      Array.isArray(message)
        ? message.map((part) => String(part)).join(" ")
        : String(message),
    );
  });
  const params = await Effect.runPromise(
    deriveOperatorDaParams(config).pipe(
      Effect.provide(Logger.replace(Logger.defaultLogger, capturingLogger)),
    ),
  );
  return { params, warnings };
};

/**
 * Runs a rejection case and returns the `HashingError`'s summary plus its
 * detail, since the actionable text lives in `cause` rather than `message`.
 */
const daParamsFailure = async (
  config: Parameters<typeof deriveOperatorDaParams>[0],
): Promise<{ readonly message: string; readonly cause: string }> => {
  const result = await Effect.runPromise(
    Effect.either(deriveOperatorDaParams(config)),
  );
  if (result._tag === "Right") {
    throw new Error("expected deriveOperatorDaParams to fail closed");
  }
  return { message: result.left.message, cause: String(result.left.cause) };
};

describe("DA attestation configuration", () => {
  it("builds fresh deployment DA params from configured committee and owners", async () => {
    const committee = "01".repeat(32) + "02".repeat(32) + "03".repeat(32);
    const owners = ["11".repeat(28), "22".repeat(28)];
    const params = await Effect.runPromise(
      deriveOperatorDaParams({
        ...BASE,
        DA_COMMITTEE_HEX: committee,
        DA_OWNERS_HEX: owners.join(""),
        DA_THRESHOLD: 2n,
      }),
    );
    expect(params.committee).toBe(committee);
    expect(params.da_threshold).toBe(2n);
    expect(params.owners).toEqual(owners);
    expect(params.update_threshold).toBe(2n);
    expect(params.committee_signers_hash).toBe(blake2b256Hex(committee));
  });

  // Q63 (F04 §4): with no explicit DA_THRESHOLD the bootstrap must land on the
  // governed floor, never on the pre-Q63 default of one.
  it("defaults both thresholds to the governed floor", async () => {
    const committee =
      "01".repeat(32) + "02".repeat(32) + "03".repeat(32) + "04".repeat(32);
    const params = await Effect.runPromise(
      deriveOperatorDaParams({
        ...BASE,
        DA_COMMITTEE_HEX: committee,
        DA_OWNERS_HEX: ["11".repeat(28), "22".repeat(28), "33".repeat(28)].join(
          "",
        ),
      }),
    );
    expect(params.da_threshold).toBe(BigInt(SDK.governedThresholdFloor(4)));
    expect(params.da_threshold).toBe(3n);
    expect(params.update_threshold).toBe(BigInt(SDK.governedThresholdFloor(3)));
    expect(params.update_threshold).toBe(2n);
  });

  // The dev/emulator bootstrap shape: the harness generates a second wallet and
  // hands it over as DA_COSIGNER_SEED_PHRASE, so one process holds two distinct
  // keys and can reach a floor-compliant 2-of-2.
  it("derives a floor-compliant 2-of-2 from a locally held cosigner", async () => {
    const operator = seedKeys(OPERATOR_SEED);
    const cosigner = seedKeys(COSIGNER_SEED);
    const params = await Effect.runPromise(
      deriveOperatorDaParams({
        ...BASE,
        DA_COSIGNER_SEED_PHRASE: COSIGNER_SEED,
      }),
    );

    const expectedCommittee = [operator.vkey, cosigner.vkey].sort();
    expect(params.committee).toBe(expectedCommittee.join(""));
    expect(params.owners).toEqual([operator.keyHash, cosigner.keyHash].sort());
    expect(params.da_threshold).toBe(2n);
    expect(params.update_threshold).toBe(2n);
    expect(params.committee_signers_hash).toBe(blake2b256Hex(params.committee));
    expect(
      SDK.daParamsFloorViolations({
        committeeLength: 2,
        daThreshold: Number(params.da_threshold),
        ownerCount: params.owners.length,
        updateThreshold: Number(params.update_threshold),
      }),
    ).toEqual([]);
  });

  // Rider 3 of the 2026-08-11 owner ruling 4: "bootstrap warns instead of
  // rejecting". Both of the next two cases were `fails closed` assertions
  // before the F04 §4 amendment. They are the bootstrap half of the lift on the
  // committee side; the owner-set case below is its counterpart under the
  // 2026-08-13 ruling, and warns rather than refusing for the same reason.
  it("warns instead of rejecting when the configured committee has a single member", async () => {
    const { params, warnings } = await daParamsWithLogs({
      ...BASE,
      DA_COMMITTEE_HEX: "01".repeat(32),
      DA_OWNERS_HEX: ["11".repeat(28), "22".repeat(28)].join(""),
    });

    expect(params.committee).toBe("01".repeat(32));
    // The floor, not a hardcoded 1: a 1-of-1 committee is exactly what the
    // amended `ceil(2n/3)` admits at n = 1.
    expect(params.da_threshold).toBe(BigInt(SDK.governedThresholdFloor(1)));
    expect(params.da_threshold).toBe(1n);
    expect(
      SDK.daParamsFloorViolations({
        committeeLength: 1,
        daThreshold: Number(params.da_threshold),
        ownerCount: params.owners.length,
        updateThreshold: Number(params.update_threshold),
      }),
    ).toEqual([]);

    expect(warnings).toHaveLength(1);
    expect(warnings[0]).toMatch(/Single-key DA configuration/);
    expect(warnings[0]).toMatch(/DA_COSIGNER_SEED_PHRASE/);
  });

  // Rider 3's owner-side half, under the 2026-08-13 in-session owner ruling
  // recorded on #602: the governor's owner-set minimum dropped to one, so this
  // was a `fails closed` assertion twice over — first for the committee, then
  // for the owner set — and is now a warning on both.
  it("warns instead of rejecting when the configured owner set has a single member", async () => {
    const { params, warnings } = await daParamsWithLogs({
      ...BASE,
      DA_COMMITTEE_HEX: "01".repeat(32) + "02".repeat(32),
      DA_OWNERS_HEX: "11".repeat(28),
    });

    expect(params.owners).toEqual(["11".repeat(28)]);
    expect(params.update_threshold).toBe(BigInt(SDK.governedThresholdFloor(1)));
    expect(params.update_threshold).toBe(1n);
    expect(
      SDK.daParamsFloorViolations({
        committeeLength: 2,
        daThreshold: Number(params.da_threshold),
        ownerCount: params.owners.length,
        updateThreshold: Number(params.update_threshold),
      }),
    ).toEqual([]);

    // Only the governance warning: the committee has two members.
    expect(warnings).toHaveLength(1);
    expect(warnings[0]).toMatch(/Single-key DA governance/);
    expect(warnings[0]).toMatch(/rotate the DA committee/);
  });

  // The end-to-end shape the ruling exists for: a node holding nothing but its
  // own key bootstraps, and gets both warnings rather than either refusal.
  it("warns twice instead of rejecting when no second key is configured at all", async () => {
    const operator = seedKeys(OPERATOR_SEED);
    const { params, warnings } = await daParamsWithLogs(BASE);

    expect(params.committee).toBe(operator.vkey);
    expect(params.owners).toEqual([operator.keyHash]);
    expect(params.da_threshold).toBe(1n);
    expect(params.update_threshold).toBe(1n);
    expect(
      SDK.daParamsFloorViolations({
        committeeLength: 1,
        daThreshold: 1,
        ownerCount: 1,
        updateThreshold: 1,
      }),
    ).toEqual([]);

    expect(warnings).toHaveLength(2);
    expect(warnings[0]).toMatch(/Single-key DA configuration/);
    expect(warnings[1]).toMatch(/Single-key DA governance/);
  });

  it("emits no single-key warning for a two-member committee and owner set", async () => {
    const { params, warnings } = await daParamsWithLogs({
      ...BASE,
      DA_COMMITTEE_HEX: "01".repeat(32) + "02".repeat(32),
      DA_OWNERS_HEX: ["11".repeat(28), "22".repeat(28)].join(""),
    });

    expect(params.da_threshold).toBe(2n);
    expect(warnings).toStrictEqual([]);
  });

  // Malformed configuration still fails closed — the rulings lifted arity
  // prohibitions, not encoding validation.
  it("still fails closed on a malformed configured owner set", async () => {
    const failure = await daParamsFailure({
      ...BASE,
      DA_COMMITTEE_HEX: "01".repeat(32) + "02".repeat(32),
      DA_OWNERS_HEX: "  ,  ",
    });
    expect(failure.message).toBe("Invalid DA owner configuration");
    expect(failure.cause).toMatch(/DA_OWNERS_HEX must be/);
  });

  // Committee position is the signer index every witness and attested-signer
  // bit is keyed on, so an unsorted committee is rejected rather than repaired.
  it("fails closed on an unsorted committee", async () => {
    const failure = await daParamsFailure({
      ...BASE,
      DA_COMMITTEE_HEX: "02".repeat(32) + "01".repeat(32),
      DA_OWNERS_HEX: ["11".repeat(28), "22".repeat(28)].join(""),
    });
    expect(failure.message).toBe("Invalid DA committee configuration");
    expect(failure.cause).toMatch(/sorted ascending/);
  });

  it("rejects DA thresholds below the governed floor", async () => {
    const failure = await daParamsFailure({
      ...BASE,
      DA_COMMITTEE_HEX: "01".repeat(32) + "02".repeat(32),
      DA_OWNERS_HEX: ["11".repeat(28), "22".repeat(28)].join(""),
      DA_THRESHOLD: 1n,
    });
    expect(failure.message).toBe("Invalid DA threshold configuration");
    expect(failure.cause).toMatch(/da_threshold_below_floor/);
  });

  it("rejects DA thresholds larger than the configured committee", async () => {
    const failure = await daParamsFailure({
      ...BASE,
      DA_COMMITTEE_HEX: "01".repeat(32) + "02".repeat(32),
      DA_OWNERS_HEX: ["11".repeat(28), "22".repeat(28)].join(""),
      DA_THRESHOLD: 3n,
    });
    expect(failure.message).toBe("Invalid DA threshold configuration");
    expect(failure.cause).toMatch(/da_threshold_exceeds_committee/);
  });

  // Regression: the governed floors belong to initialization, not to config
  // load. A pre-Q63 `DA_THRESHOLD=1` left in a checkout's `.env` must not be
  // able to fail `NodeConfig` for subsystems that never touch DA — but it must
  // still be refused here, where the datum would be written.
  it("still refuses a pre-Q63 threshold of one over a well-formed committee", async () => {
    const failure = await daParamsFailure({
      ...BASE,
      DA_COMMITTEE_HEX: "01".repeat(32) + "02".repeat(32),
      DA_OWNERS_HEX: ["11".repeat(28), "22".repeat(28)].join(""),
      DA_THRESHOLD: 1n,
    });
    expect(failure.message).toBe("Invalid DA threshold configuration");
    expect(failure.cause).toMatch(/da_threshold_below_floor/);
  });
});
