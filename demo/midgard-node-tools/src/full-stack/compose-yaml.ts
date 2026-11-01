import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

/** A value Compose replaces instead of merging with the base file's (`!override`). */
const OVERRIDE = Symbol("override");
type Overridden = { [OVERRIDE]: unknown };
export const override = (value: unknown): Overridden => ({
  [OVERRIDE]: value,
});

/** YAML flow style: JSON, plus the Compose merge tag JSON cannot carry. */
export function composeYaml(value: unknown): string {
  if (value !== null && typeof value === "object" && OVERRIDE in value)
    return `!override ${composeYaml((value as Overridden)[OVERRIDE])}`;
  if (Array.isArray(value)) return `[${value.map(composeYaml).join(", ")}]`;
  if (value !== null && typeof value === "object")
    return `{${Object.entries(value)
      .map(([key, item]) => `${JSON.stringify(key)}: ${composeYaml(item)}`)
      .join(", ")}}`;
  return JSON.stringify(value);
}

/**
 * Labels that carry the digest of files a service bind-mounts. Compose
 * recreates a container on `up -d` when its service definition changes, never
 * when only a mounted file's bytes do, so a service reading a generated file
 * carries its digest and a changed file restarts it.
 */
export async function mountedFilesLabels(paths: readonly string[]) {
  const hash = createHash("sha256");
  for (const path of paths)
    hash.update(`\0${path}\0`).update(
      createHash("sha256")
        .update(await readFile(path))
        .digest("hex"),
    );
  return { "io.midgard.stack.mounted-files-sha256": hash.digest("hex") };
}
