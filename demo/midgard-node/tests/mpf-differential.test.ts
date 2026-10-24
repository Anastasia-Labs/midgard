import { fileURLToPath } from "node:url";

import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { mpfReplayProgram } from "../src/commands/mpf-replay.js";
import {
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";

// The architecture_g replay spawns the native owner binary; see the helper
// for the build/skip contract (#642).
const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent) {
  warnNativeOwnerBinaryAbsent("mpf-differential");
}

describe.skipIf(!binaryPresent)("mpf differential replay", () => {
  it.effect(
    "binds the seeded adversarial MPF corpus to the TypeScript reference and Architecture G",
    () =>
      Effect.gen(function* () {
        const corpusPath = fileURLToPath(
          new URL("./fixtures/mpf-adversarial.ndjson", import.meta.url),
        );
        const summary = yield* mpfReplayProgram(corpusPath);
        expect(summary).toMatchObject({
          corpusPath,
          blocks: 1,
          runs: 4,
          proofChecks: 8,
          implementations: ["typescript_reference", "architecture_g"],
          runsByImplementation: {
            typescript_reference: 2,
            architecture_g: 2,
          },
          scratchBuilds: ["insert", "fromlist"],
          nativeOwner: {
            binaryPath: expect.stringMatching(/architecture-g-owner$/),
            binarySha256: expect.stringMatching(/^[0-9a-f]{64}$/),
          },
          adversarialCoverage: {
            emptyEvents: 1,
            deleteReinsertEvents: 1,
            collapseResplitSequences: 1,
            longestHashedPrefixNibbles: expect.any(Number),
          },
        });
        expect(
          summary.adversarialCoverage.longestHashedPrefixNibbles,
        ).toBeGreaterThanOrEqual(6);
      }),
  );
});
