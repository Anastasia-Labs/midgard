import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { mpfReplayProgram } from "@/commands/mpf-replay.js";

// The architecture_g engine spawns the native owner binary. The plain `test`
// run builds it in tests/global-setup.ts; it can still be absent when cargo is
// unavailable (or MIDGARD_SKIP_NATIVE_BUILD=1). Skip LOUDLY rather than fail
// on a missing optional toolchain — and never pass silently (#642).
const nativeOwnerBinaryPath = fileURLToPath(
  new URL(
    "../native/mpf-event-flat-wasm/target/release/architecture-g-owner",
    import.meta.url,
  ),
);
const nativeOwnerBinaryPresent = existsSync(nativeOwnerBinaryPath);
if (!nativeOwnerBinaryPresent) {
  console.warn(
    `[mpf-differential] SKIPPING: native binary absent at ${nativeOwnerBinaryPath} — build it with \`pnpm run native:mpf-owner:build\` (requires cargo)`,
  );
}

describe.skipIf(!nativeOwnerBinaryPresent)("mpf differential replay", () => {
  it.effect(
    "binds the seeded adversarial MPF corpus to every configured engine",
    () =>
      Effect.gen(function* () {
        const corpusPath = fileURLToPath(
          new URL("./fixtures/mpf-adversarial.ndjson", import.meta.url),
        );
        const summary = yield* mpfReplayProgram(corpusPath);
        expect(summary).toMatchObject({
          corpusPath,
          blocks: 1,
          runs: 8,
          proofChecks: 16,
          engines: ["legacy", "overlay", "event_flat", "architecture_g"],
          runsByEngine: {
            legacy: 2,
            overlay: 2,
            event_flat: 2,
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
