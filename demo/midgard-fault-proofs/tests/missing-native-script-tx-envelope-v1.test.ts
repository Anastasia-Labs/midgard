import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES } from "../src/missing-native-script-tx/contracts-v1.js";
import { measureBlueprintValidatorBytes } from "../src/runtime.js";
import {
  buildMissingNativeScriptTxChain,
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

// Derivation: half the hex length of the blueprint body, and `parameters.length`,
// for each `fraud_proofs/missing_native_script_tx/step_NN.main.spend` entry of
// `onchain/aiken/plutus.json`, built with `aiken build --env testnet`. Both are
// read through `measureBlueprintValidatorBytes`, so this file never touches a
// blueprint body itself — the #610 bare-loader scan in
// `zz605-semantic-resolver-arity.test.ts` is a deliberately dumb text scan, and
// its own instruction is to word comments around the field name rather than
// grow the allowlist for a file that loads nothing.
const EXPECTED_UNAPPLIED_SIZES_BYTES = {
  step01: 7_872,
  step02: 7_199,
  step03: 7_935,
  step04: 8_795,
  step05: 1_579,
  step06: 9_899,
  step07: 10_292,
  step08: 9_523,
} as const;
const EXPECTED_DECLARED_ARITIES = {
  step01: 3,
  step02: 3,
  step03: 3,
  step04: 3,
  step05: 2,
  step06: 5,
  step07: 3,
  step08: 4,
} as const;

describe("missing-native-script-tx envelope and reference-script deployment", () => {
  const blueprint = readBlueprint(realBlueprintPath);

  it("pins all eight nonzero unapplied sizes to the audited blueprint", () => {
    let found = 0;
    for (const [step, title] of Object.entries(
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES,
    )) {
      expect(
        measureBlueprintValidatorBytes({
          blueprint,
          title,
          expectedDeclaredParameterCount:
            EXPECTED_DECLARED_ARITIES[
              step as keyof typeof EXPECTED_DECLARED_ARITIES
            ],
        }),
        title,
      ).toBe(
        EXPECTED_UNAPPLIED_SIZES_BYTES[
          step as keyof typeof EXPECTED_UNAPPLIED_SIZES_BYTES
        ],
      );
      found += 1;
    }
    expect(found).toBe(8);
  });

  it("applies eight distinct scripts and fits each oversized publication host", async () => {
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        credentialToAddress(network, scriptHashToCredential("22".repeat(28))),
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const steps = buildMissingNativeScriptTxChain({
      realBlueprint: blueprint,
      computationThreadPolicyId: "11".repeat(28),
      fraudProofPolicyId: "33".repeat(28),
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId: "44".repeat(28),
      hubOraclePolicyId: "55".repeat(28),
    });
    expect(new Set(steps.map((step) => step.spendingScriptHash)).size).toBe(8);
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
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes);
  });
});
