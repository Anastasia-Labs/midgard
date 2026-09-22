/**
 * The one place that knows where the native architecture-G owner binary
 * lives and how its absence is reported (#642). The plain `test` run builds
 * it in tests/global-setup.ts; it can still be absent when cargo is
 * unavailable (or MIDGARD_SKIP_NATIVE_BUILD=1). Consumers skip LOUDLY via
 * `warnNativeOwnerBinaryAbsent` rather than fail on a missing optional
 * toolchain — and never pass silently.
 */
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";

export const nativeOwnerBinaryPath = fileURLToPath(
  new URL(
    "../../native/mpf-event-flat-wasm/target/release/architecture-g-owner",
    import.meta.url,
  ),
);

export const nativeOwnerBinaryPresent = (): boolean =>
  existsSync(nativeOwnerBinaryPath);

export const warnNativeOwnerBinaryAbsent = (tag: string): void => {
  console.warn(
    `[${tag}] SKIPPING: native binary absent at ${nativeOwnerBinaryPath} — build it with \`pnpm run native:mpf-owner:build\` (requires cargo)`,
  );
};
