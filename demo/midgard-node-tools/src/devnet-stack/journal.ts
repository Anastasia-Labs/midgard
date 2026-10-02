import { readJsonIfPresent, writeDurableJson } from "./durable.js";

type JournalFile = {
  readonly schemaVersion: "midgard-devnet-journal-v1";
  readonly entries: Record<string, unknown>;
};

/**
 * Durable step records. A step writes its intent (for example a signed
 * transaction id) before acting on it, so a rerun reconciles that exact
 * intent against the chain instead of building a second one.
 */
export class Journal {
  private readonly file: JournalFile;

  constructor(private readonly path: string) {
    this.file = readJsonIfPresent<JournalFile>(path) ?? {
      schemaVersion: "midgard-devnet-journal-v1",
      entries: {},
    };
    if (this.file.schemaVersion !== "midgard-devnet-journal-v1")
      throw new Error(`${path} has an unknown schema; preserve it`);
  }

  get<T>(key: string): T | undefined {
    return this.file.entries[key] as T | undefined;
  }

  /** Every value whose key starts with `prefix`, in insertion order. */
  withPrefix<T>(prefix: string): T[] {
    return Object.entries(this.file.entries)
      .filter(([key]) => key.startsWith(prefix))
      .map(([, value]) => value as T);
  }

  /** Writes over the file as it is now, so an instance opened earlier never
   * drops an entry another instance recorded since. */
  set(key: string, value: unknown): void {
    const current = readJsonIfPresent<JournalFile>(this.path);
    Object.assign(this.file.entries, current?.entries, { [key]: value });
    writeDurableJson(this.path, this.file);
  }
}
