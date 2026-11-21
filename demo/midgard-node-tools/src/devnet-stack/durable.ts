import { existsSync, readFileSync } from "node:fs";

import { writeTextFileAtomicSync } from "@al-ft/midgard-core/atomic-write";

/**
 * Replaces `path` atomically, so a crash leaves either the old or the new
 * file, never a torn one. Run files are private: the file gets `mode` and
 * every directory the write creates 0o700.
 */
export const writeDurableFile = (
  path: string,
  content: string | Uint8Array,
  mode = 0o600,
): void =>
  writeTextFileAtomicSync(path, content, { mode, directoryMode: 0o700 });

export const writeDurableJson = (
  path: string,
  value: unknown,
  mode = 0o600,
): void => writeDurableFile(path, `${JSON.stringify(value, null, 2)}\n`, mode);

/**
 * Writes `content` once. A later call with the same bytes is a no-op; with
 * different bytes it throws, so a run's configuration never changes under a
 * process that already holds state bound to it.
 */
export const writeOnceFile = (
  path: string,
  content: string | Uint8Array,
  mode = 0o600,
): void => {
  const bytes =
    typeof content === "string" ? Buffer.from(content) : Buffer.from(content);
  if (existsSync(path)) {
    if (!readFileSync(path).equals(bytes))
      throw new Error(
        `${path} differs from what this controller derives for the run; it is never rewritten`,
      );
    return;
  }
  writeDurableFile(path, bytes, mode);
};

export const readJsonIfPresent = <T>(path: string): T | undefined =>
  existsSync(path) ? (JSON.parse(readFileSync(path, "utf8")) as T) : undefined;

/**
 * Write-once state: the first call persists `make()`, every later call returns
 * what was persisted. Identities (keys, seeds, ports) go through this so a
 * retry can never silently replace them.
 */
export const createOnce = <T>(path: string, make: () => T): T => {
  const existing = readJsonIfPresent<T>(path);
  if (existing !== undefined) return existing;
  const value = make();
  writeDurableJson(path, value);
  return value;
};
