import { randomBytes } from "node:crypto";
import { existsSync } from "node:fs";
import { mkdir, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

/** The journey watcher's secret files, shared by every session of one run. */
export const journeyWatcherKeySources = (runDirectory: string) => {
  const source = (name: string) => ({
    kind: "file" as const,
    path: join(runDirectory, "secrets", name),
  });
  return {
    rollback: source("watcher-rollback.key"),
    prover: source("watcher-prover.seed"),
    availability: source("watcher-availability.seed"),
  };
};

/**
 * A fresh journey runtime creates its missing secrets. Once the runtime
 * directory exists the watcher's state is bound to them, so a missing secret
 * is refused and never regenerated.
 */
export const ensureJourneyWatcherSecrets = async (input: {
  runDirectory: string;
  runtimeDirectory: string;
  publisherSeed: string;
  availabilitySeed: string;
}) => {
  const keys = journeyWatcherKeySources(input.runDirectory);
  const fresh = !existsSync(input.runtimeDirectory);
  const secret = async (path: string, initial: () => string) => {
    if (existsSync(path)) return;
    if (!fresh)
      throw new Error(
        "Established journey watcher secret is missing; it is never regenerated",
      );
    await mkdir(dirname(path), { recursive: true, mode: 0o700 });
    await writeFile(path, initial(), { mode: 0o600, flag: "wx" });
  };
  await secret(keys.rollback.path, () => randomBytes(32).toString("hex"));
  await secret(keys.prover.path, () => input.publisherSeed);
  await secret(keys.availability.path, () => input.availabilitySeed);
  await mkdir(input.runtimeDirectory, { recursive: true, mode: 0o700 });
  return keys;
};
