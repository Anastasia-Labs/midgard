import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import type {
  OutputReferenceScriptDecodingJournal,
  OutputReferenceScriptDecodingJournalEntry,
} from "./workflow-v1.js";

const identityPattern = /^[0-9a-f]{64}$/u;

/** Family-owned, append-only crash journal with atomic file replacement. */
export class DirectoryOutputReferenceScriptDecodingJournal
  implements OutputReferenceScriptDecodingJournal
{
  readonly #directory: string;

  public constructor(directory: string) {
    this.#directory = directory;
  }

  async load(
    identity: string,
  ): Promise<readonly OutputReferenceScriptDecodingJournalEntry[]> {
    if (!identityPattern.test(identity))
      throw new Error(
        "outputReferenceScriptDecoding journal identity is invalid",
      );
    try {
      const parsed: unknown = JSON.parse(
        await readFile(join(this.#directory, `${identity}.json`), "utf8"),
      );
      if (!Array.isArray(parsed))
        throw new Error("journal payload is not an array");
      return Object.freeze(
        parsed.map((value, sequence) => {
          const entry =
            value as Partial<OutputReferenceScriptDecodingJournalEntry>;
          if (entry.identity !== identity || entry.sequence !== sequence)
            throw new Error(
              "outputReferenceScriptDecoding durable journal identity/sequence changed",
            );
          return Object.freeze(
            entry as OutputReferenceScriptDecodingJournalEntry,
          );
        }),
      );
    } catch (cause) {
      if ((cause as NodeJS.ErrnoException).code === "ENOENT")
        return Object.freeze([]);
      throw cause;
    }
  }

  async append(
    entry: OutputReferenceScriptDecodingJournalEntry,
  ): Promise<void> {
    const entries = await this.load(entry.identity);
    if (entry.sequence !== entries.length)
      throw new Error(
        "outputReferenceScriptDecoding durable journal append raced",
      );
    const path = join(this.#directory, `${entry.identity}.json`);
    const temporary = `${path}.${process.pid.toString()}.${entry.sequence.toString()}.tmp`;
    await mkdir(dirname(path), { recursive: true });
    await writeFile(temporary, `${JSON.stringify([...entries, entry])}\n`, {
      encoding: "utf8",
      flag: "wx",
    });
    await rename(temporary, path);
  }
}
