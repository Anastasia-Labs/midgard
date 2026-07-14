import { fileURLToPath } from "node:url";

import { it } from "@effect/vitest";
import { Effect } from "effect";
import { expect } from "vitest";

import { mpfReplayProgram } from "@/commands/mpf-replay.js";

it.effect(
  "binds the seeded adversarial MPF corpus to legacy and Architecture G",
  () =>
    Effect.gen(function* () {
      const corpusPath = fileURLToPath(
        new URL("./fixtures/mpf-adversarial.ndjson", import.meta.url),
      );
      const summary = yield* mpfReplayProgram(corpusPath);
      expect(summary).toMatchObject({
        corpusPath,
        blocks: 1,
        runs: 6,
        proofChecks: 12,
        engines: ["legacy", "overlay", "architecture_g"],
        runsByEngine: {
          legacy: 2,
          overlay: 2,
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
