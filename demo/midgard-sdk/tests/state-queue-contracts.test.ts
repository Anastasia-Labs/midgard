import { readFileSync } from "node:fs";

import { credentialToAddress, getAddressDetails } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(
      new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      "utf8",
    ),
  ),
);

const input = {
  blueprint,
  network: "Preprod" as const,
  hubOraclePolicyId: "11".repeat(28),
  correctionLockScriptHash: "22".repeat(28),
  activeOperatorsPolicyId: "33".repeat(28),
  activeOperatorsAddress: credentialToAddress("Preprod", {
    type: "Script",
    hash: "33".repeat(28),
  }),
  retiredOperatorsPolicyId: "44".repeat(28),
  schedulerPolicyId: "55".repeat(28),
  fraudProofPolicyId: "66".repeat(28),
  settlementPolicyId: "77".repeat(28),
  daAttestationPolicyId: "88".repeat(28),
  availabilityChallengePolicyId: "99".repeat(28),
  referenceScriptAuthPolicyId: "aa".repeat(28),
};

describe("queue deployment recipes", () => {
  it("binds spending addresses to the requested network without changing scripts", async () => {
    const preprod = await Effect.runPromise(
      SDK.buildStateQueueValidator(input),
    );
    const mainnet = await Effect.runPromise(
      SDK.buildStateQueueValidator({ ...input, network: "Mainnet" }),
    );
    expect(getAddressDetails(preprod.spendingScriptAddress).networkId).toBe(0);
    expect(getAddressDetails(mainnet.spendingScriptAddress).networkId).toBe(1);
    expect(mainnet).toEqual({
      ...preprod,
      spendingScriptAddress: mainnet.spendingScriptAddress,
    });
    const lockPreprod = await Effect.runPromise(
      SDK.buildCorrectionLockValidator(input),
    );
    const lockMainnet = await Effect.runPromise(
      SDK.buildCorrectionLockValidator({ ...input, network: "Mainnet" }),
    );
    expect(getAddressDetails(lockMainnet.spendingScriptAddress).networkId).toBe(
      1,
    );
    expect(getAddressDetails(lockPreprod.spendingScriptAddress).networkId).toBe(
      0,
    );
    expect(lockMainnet).toEqual({
      ...lockPreprod,
      spendingScriptAddress: lockMainnet.spendingScriptAddress,
    });
  });

  const titles = [
    ...Object.values(SDK.STATE_QUEUE_SCRIPT_TITLES),
    SDK.CORRECTION_LOCK_SCRIPT_TITLES.spend,
  ];
  const build = (title: string, changedBlueprint: SDK.FaultProofBlueprint) =>
    Effect.runPromise(
      title === SDK.CORRECTION_LOCK_SCRIPT_TITLES.spend
        ? SDK.buildCorrectionLockValidator({
            ...input,
            blueprint: changedBlueprint,
          })
        : SDK.buildStateQueueValidator({
            ...input,
            blueprint: changedBlueprint,
          }),
    );

  it.each(titles)("refuses a missing blueprint entry: %s", async (title) => {
    await expect(
      build(title, {
        validators: blueprint.validators.filter(
          (entry) => entry.title !== title,
        ),
      }),
    ).rejects.toThrow(`Validator with title "${title}" not found in blueprint`);
  });

  it.each(titles)(
    "refuses both arity changes after cache warmup: %s",
    async (title) => {
      await build(title, blueprint);
      for (const delta of [-1, 1]) {
        const changed = {
          validators: blueprint.validators.map((entry) =>
            entry.title !== title
              ? entry
              : {
                  ...entry,
                  parameters:
                    delta < 0
                      ? entry.parameters.slice(0, -1)
                      : [...entry.parameters, { title: "extra" }],
                },
          ),
        };
        await expect(build(title, changed)).rejects.toThrow(
          /declares .* parameter\(s\).* were applied/,
        );
      }
    },
  );

  it("refuses a malformed active-operators address", async () => {
    await expect(
      Effect.runPromise(
        SDK.buildStateQueueValidator({
          ...input,
          activeOperatorsAddress: "not-an-address",
        }),
      ),
    ).rejects.toThrow(/Failed to encode active-operators address/);
  });

  it.each([
    "hubOraclePolicyId",
    "activeOperatorsPolicyId",
    "retiredOperatorsPolicyId",
    "schedulerPolicyId",
    "fraudProofPolicyId",
    "settlementPolicyId",
    "daAttestationPolicyId",
    "availabilityChallengePolicyId",
    "referenceScriptAuthPolicyId",
  ] as const)("refuses malformed state-queue policy/hash %s", async (field) => {
    await expect(
      Effect.runPromise(
        SDK.buildStateQueueValidator({ ...input, [field]: "aa" }),
      ),
    ).rejects.toThrow(/28-byte hash/);
  });
  it("refuses malformed correction-lock bytes in the queue parameters", async () => {
    await expect(
      Effect.runPromise(
        SDK.buildStateQueueValidator({
          ...input,
          correctionLockScriptHash: "zz",
        }),
      ),
    ).rejects.toThrow(
      /correction_lock_script_hash.*bytestring as lowercase hexadecimal/,
    );
  });
  it("refuses a malformed correction-lock policy parameter", async () => {
    await expect(
      Effect.runPromise(
        SDK.buildCorrectionLockValidator({
          blueprint,
          network: "Preprod",
          hubOraclePolicyId: "11".repeat(28),
          availabilityChallengePolicyId: "22",
        }),
      ),
    ).rejects.toThrow(/availability_policy_id.*28-byte hash/);
  });
});
