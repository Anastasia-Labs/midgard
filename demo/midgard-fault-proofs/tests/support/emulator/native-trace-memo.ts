import { buildDeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { canonicalFixtureDigest, createFixtureMemo } from "./fixture-memo.js";

const spendingKeyBytes = new Map<string, Uint8Array>();

/**
 * The spending key of each distinct native-trace parameter set, drawn fresh
 * the first time this test file asks for that set and reused after, so every
 * fixture built from the same parameters signs the same transaction and its
 * honest trace can be shared. Fixtures with different parameters still get
 * independent keys.
 */
export const spendingKeyFor = (params: unknown): CML.PrivateKey => {
  const digest = canonicalFixtureDigest(params);
  let bytes = spendingKeyBytes.get(digest);
  if (bytes === undefined) {
    bytes = CML.PrivateKey.generate_ed25519().to_raw_bytes();
    spendingKeyBytes.set(digest, bytes);
  }
  return CML.PrivateKey.from_normal_bytes(bytes);
};

/**
 * The honest deterministic trace, built once per distinct replay input per
 * test file (the key covers every byte of that input) and handed to each
 * caller as its own deep copy.
 */
export const honestTraceFor = createFixtureMemo(
  (input: Parameters<typeof buildDeterministicValidationMachineTrace>[0]) =>
    Effect.runPromise(buildDeterministicValidationMachineTrace(input)),
);
