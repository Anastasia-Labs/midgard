import { randomUUID } from "node:crypto";
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import {
  type ChainSyncConsumerCursorStore,
  type ChainSyncCursor,
  getRecord,
  parsePersistedChainSyncCursor,
  type PersistedChainSyncConsumerState,
} from "./provider.parse-persisted-chain-sync-state.js";
import { samePersistedCursor } from "./provider.same-persisted-cursor.js";

export class FileChainSyncConsumerCursorStore
  implements ChainSyncConsumerCursorStore
{
  constructor(
    private readonly path: string,
    private readonly authorityFingerprint: string,
  ) {
    if (!/^[0-9a-f]{64}$/u.test(authorityFingerprint)) {
      throw new Error(
        "chain-sync consumer authority fingerprint must be lowercase sha256 hex",
      );
    }
  }

  async load(): Promise<ChainSyncCursor | undefined> {
    let raw: string;
    try {
      raw = await readFile(this.path, "utf8");
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return undefined;
      }
      throw error;
    }
    const state = parsePersistedChainSyncConsumerState(
      JSON.parse(raw) as unknown,
      this.authorityFingerprint,
    );
    return state.cursor;
  }

  async save(cursor: ChainSyncCursor): Promise<void> {
    const previous = await this.load();
    if (
      previous !== undefined &&
      (cursor.sequence < previous.sequence ||
        cursor.rollbackGeneration < previous.rollbackGeneration)
    ) {
      throw new Error("chain-sync consumer cursor cannot move backwards");
    }
    if (
      previous !== undefined &&
      cursor.sequence === previous.sequence &&
      !samePersistedCursor(previous, cursor)
    ) {
      throw new Error(
        "chain-sync consumer cursor cannot change at the same sequence",
      );
    }
    const state: PersistedChainSyncConsumerState = {
      schemaVersion: 1,
      authorityFingerprint: this.authorityFingerprint,
      cursor,
    };
    await mkdir(dirname(this.path), { recursive: true });
    const temporaryPath = `${this.path}.${randomUUID()}.tmp`;
    await writeFile(temporaryPath, `${JSON.stringify(state)}\n`, {
      encoding: "utf8",
      mode: 0o600,
    });
    await rename(temporaryPath, this.path);
  }
}

const parsePersistedChainSyncConsumerState = (
  value: unknown,
  expectedAuthorityFingerprint: string,
): PersistedChainSyncConsumerState => {
  const record = getRecord(value, "persisted chain-sync consumer state");
  if (
    Object.keys(record).some(
      (key) =>
        key !== "schemaVersion" &&
        key !== "authorityFingerprint" &&
        key !== "cursor",
    ) ||
    record.schemaVersion !== 1 ||
    typeof record.authorityFingerprint !== "string" ||
    record.cursor === undefined
  ) {
    throw new Error(
      "persisted chain-sync consumer state has an unsupported schema",
    );
  }
  if (record.authorityFingerprint !== expectedAuthorityFingerprint) {
    throw new Error(
      "persisted chain-sync consumer authority fingerprint does not match the configured local node endpoint",
    );
  }
  return {
    schemaVersion: 1,
    authorityFingerprint: record.authorityFingerprint,
    cursor: parsePersistedChainSyncCursor(record.cursor),
  };
};
