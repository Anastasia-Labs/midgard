import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  buildMissingNativeScriptTxFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES } from "../src/missing-native-script-tx/contracts.js";
import { measureBlueprintValidatorBytes } from "../src/runtime.js";
import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

// Derivation: `parameters.length` for each
// `fraud_proofs/missing_native_script_tx/step_NN.main.spend` entry of
// `onchain/aiken/plutus.json`, built with `aiken build --env testnet`. It is
// read through `measureBlueprintValidatorBytes`, so this file never touches a
// blueprint body itself; it measures and deploys nothing.
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

/**
 * Everything a reference-script publication transaction carries besides the
 * script itself — skeleton, funding input, change and collateral, the auth
 * token mint and one signature. Same allowance the native-script-decoding
 * envelope suite states, and the emulator publication journeys are the
 * binding re-measurement of it.
 */
const PUBLICATION_OVERHEAD_ALLOWANCE_BYTES = 2_048;

describe("missing-native-script-tx envelope and reference-script deployment", () => {
  const blueprint = readBlueprint(realBlueprintPath);

  it("declares all eight step validators with their audited arities", () => {
    // `measureBlueprintValidatorBytes` throws when the blueprint's declared
    // parameter list does not match the audited arity, and when the title is
    // absent or duplicated. The arity is the applied-script ABI: a step that
    // gains or loses a parameter cannot be applied by this family's builder.
    expect(
      Object.keys(MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES).sort(),
    ).toEqual(Object.keys(EXPECTED_DECLARED_ARITIES).sort());
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
      ).toBeGreaterThan(0);
    }
  });

  it("applies eight distinct scripts and fits each oversized publication host", async () => {
    const {
      missingNativeScriptTx: { steps },
    } = await Effect.runPromise(
      buildMissingNativeScriptTxFaultProofContracts({
        blueprint: parseFaultProofBlueprint(blueprint),
        network,
        hubOraclePolicyId: "55".repeat(28),
        fraudProofCataloguePolicyId: "66".repeat(28),
      }),
    );
    expect(steps).toHaveLength(8);
    expect(new Set(steps.map((step) => step.spendingScriptHash)).size).toBe(8);
    for (const step of steps) {
      // The envelope claim, stated against the consensus floor rather than a
      // transcribed size table: the applied script plus the publication
      // transaction's own overhead allowance must fit the smallest L1
      // `max_tx_size` Midgard supports.
      const appliedBytes = step.spendingScriptCBOR.length / 2;
      expect(
        appliedBytes + PUBLICATION_OVERHEAD_ALLOWANCE_BYTES,
        step.spendingScriptHash,
      ).toBeLessThanOrEqual(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes);
    }
  });
});
