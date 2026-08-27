import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core";
import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1 } from "../src/missing-native-script-tx/contracts-v1.js";
import {
  buildMissingNativeScriptTxChainV1,
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const EXPECTED_UNAPPLIED_SIZES_BYTES = {
  step01: 5_775,
  step02: 7_117,
  step03: 5_846,
  step04: 8_736,
  step05: 1_570,
  step06: 7_642,
} as const;

describe("missing-native-script-tx envelope and reference-script deployment", () => {
  const blueprint = readBlueprint(realBlueprintPath);

  it("pins all six nonzero unapplied sizes to the audited blueprint", () => {
    let found = 0;
    for (const [step, title] of Object.entries(
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1,
    )) {
      const validator = blueprint.validators.find(
        (candidate) => candidate.title === title,
      );
      expect(validator, title).toBeDefined();
      expect(validator!.compiledCode.length / 2).toBe(
        EXPECTED_UNAPPLIED_SIZES_BYTES[
          step as keyof typeof EXPECTED_UNAPPLIED_SIZES_BYTES
        ],
      );
      found += 1;
    }
    expect(found).toBe(6);
  });

  it("applies six distinct scripts and fits each oversized publication host", async () => {
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        credentialToAddress(network, scriptHashToCredential("22".repeat(28))),
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const steps = buildMissingNativeScriptTxChainV1({
      realBlueprint: blueprint,
      computationThreadPolicyId: "11".repeat(28),
      fraudProofPolicyId: "33".repeat(28),
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId: "44".repeat(28),
      hubOraclePolicyId: "55".repeat(28),
    });
    expect(new Set(steps.map((step) => step.spendingScriptHash)).size).toBe(6);
    for (const [index, step] of steps.entries()) {
      const appliedBytes = step.spendingScriptCBOR.length / 2;
      expect(appliedBytes).toBeGreaterThanOrEqual(
        Object.values(EXPECTED_UNAPPLIED_SIZES_BYTES)[index]!,
      );
      expect(appliedBytes + 2_048).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
      );
      expect(appliedBytes).toBeGreaterThan(0);
    }
    // Owner ruling: publication/reference is uniform even though each
    // individual validator body is below the 16,384-byte L1 envelope.
    expect(
      Math.max(...Object.values(EXPECTED_UNAPPLIED_SIZES_BYTES)),
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes);
  });
});
