/** SQLite representation of existing watcher records. Logical archive digests
 * and authority bytes stay unchanged; roots reference shared immutable records.
 * This is a storage codec, not a second validation or checkpoint authority. */
import {
  rebuildWatcherDurableCaches,
  watcherCanonicalJson,
  type WatcherDurableRecords,
  type WatcherDurableStore,
  watcherSha256CanonicalJson,
} from "./durable-store.js";

const STORE_SCHEMA = "midgard-watcher-durable-store-v1";
const AUTHORITY_SCHEMA = "midgard-watcher-rollback-durable-authority-v1";
const PAGE_SCHEMA = "midgard-watcher-record-page-v1";
const PAGE_SIZE = 64;
const HEX_32 = /^[0-9a-f]{64}$/u;
const RECORD_FIELDS = [
  "l1Observations",
  "chainPoints",
  "protocolUtxos",
  "spentProtocolUtxos",
  "daProofInputs",
  "reconstructedStates",
  "decisions",
  "faults",
  "submissions",
  "confirmations",
  "retries",
  "deadlines",
  "correctionResults",
] as const satisfies readonly (keyof WatcherDurableRecords)[];

export type WatcherRecordEncoding = "raw" | "store" | "authority" | "event";
export type WatcherEncodedRecord = Readonly<{
  encoding: WatcherRecordEncoding;
  bytes: Uint8Array;
}>;
type PutRecord = (digest: string, record: WatcherEncodedRecord) => void;
type ReadRecord = (digest: string) => Uint8Array;
type RecordObject = Record<string, unknown>;
type Sequence = Readonly<{ count: number; pages: readonly string[] }>;

const encode = (value: unknown): Uint8Array =>
  Buffer.from(watcherCanonicalJson(value), "utf8");
const parse = (bytes: Uint8Array): unknown =>
  JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
const object = (value: unknown): RecordObject => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("watcher stored record framing is invalid");
  return value as RecordObject;
};
const digest = (value: unknown): string => {
  if (typeof value !== "string" || !HEX_32.test(value))
    throw new Error("watcher stored record reference is invalid");
  return value;
};

/** Called inside the existing SQLite transaction. Each record/page is inserted
 * once; an unchanged page or record has exactly the same content address. */
export const encodeWatcherDurableRecord = (
  bytes: Uint8Array,
  put: PutRecord,
  canonicalValue?: unknown,
  hasRecord: (digest: string) => boolean = () => false,
): WatcherEncodedRecord => {
  let value: unknown;
  try {
    value = canonicalValue ?? parse(bytes);
  } catch {
    return { encoding: "raw", bytes };
  }
  if (typeof value !== "object" || value === null || Array.isArray(value))
    return { encoding: "raw", bytes };
  const schema = (value as RecordObject).schemaVersion;
  const eventArchive =
    (typeof schema === "string" &&
      [
        "midgard-watcher-local-user-event-checkpoint-payload-v1",
        "midgard-watcher-local-user-event-block-evidence-v1",
        "midgard-watcher-local-user-event-origin-archive-v1",
      ].includes(schema)) ||
    (Object.keys(value).length === 2 &&
      "entry" in value &&
      "observation" in value);
  if (schema !== STORE_SCHEMA && schema !== AUTHORITY_SCHEMA && !eventArchive)
    return { encoding: "raw", bytes };
  // The byte backend also accepts opaque/noncanonical inputs. Only the exact
  // canonical watcher representations participate in record normalization.
  if (!Buffer.from(encode(value)).equals(bytes)) {
    if (canonicalValue !== undefined)
      throw new Error(
        "watcher canonical record hint differs from committed bytes",
      );
    return { encoding: "raw", bytes };
  }

  const saveRaw = (member: unknown): string => {
    const key = watcherSha256CanonicalJson(member);
    if (!hasRecord(key)) put(key, { encoding: "raw", bytes: encode(member) });
    return key;
  };
  const saveSequence = (members: readonly unknown[]): Sequence => {
    const pages: string[] = [];
    for (let offset = 0; offset < members.length; offset += PAGE_SIZE) {
      const records = members.slice(offset, offset + PAGE_SIZE).map(saveRaw);
      pages.push(saveRaw({ schemaVersion: PAGE_SCHEMA, records }));
    }
    return { count: members.length, pages };
  };
  const saveStore = (input: RecordObject): WatcherEncodedRecord => {
    const collections: Record<string, Sequence> = {};
    const metadata = { ...input };
    for (const field of RECORD_FIELDS) {
      if (!Array.isArray(input[field]))
        throw new Error("watcher stored record collection is invalid");
      collections[field] = saveSequence(input[field]);
      delete metadata[field];
    }
    // Positional caches shift when sorted records receive an insertion. They
    // are reproducible indexes, not another set of retained evidence.
    const caches = rebuildWatcherDurableCaches({
      deploymentMarker: input.deploymentMarker,
      ...Object.fromEntries(
        RECORD_FIELDS.map((field) => [field, input[field]]),
      ),
    } as WatcherDurableStore);
    if (watcherCanonicalJson(caches) !== watcherCanonicalJson(input.caches))
      throw new Error("watcher stored cache source digest mismatch");
    const { entries: _entries, ...cacheMetadata } = caches;
    metadata.caches = cacheMetadata;
    return {
      encoding: "store",
      bytes: encode({ metadata, collections }),
    };
  };
  const storeReferences = new Map<string, WatcherEncodedRecord>();
  const project = (member: unknown): unknown => {
    if (Array.isArray(member)) return member.map(project);
    if (typeof member !== "object" || member === null) return member;
    const fields = object(member);
    if (fields.schemaVersion === STORE_SCHEMA) {
      const key = watcherSha256CanonicalJson(fields);
      if (hasRecord(key)) return { watcherStore: key };
      let stored = storeReferences.get(key);
      if (stored === undefined) {
        stored = saveStore(fields);
        storeReferences.set(key, stored);
        put(key, stored);
      }
      return { watcherStore: key };
    }
    return Object.fromEntries(
      Object.entries(fields).map(([key, child]) => [key, project(child)]),
    );
  };
  if (eventArchive) {
    const projectEvent = (member: unknown): unknown => {
      if (Array.isArray(member)) return member.map(projectEvent);
      if (typeof member !== "object" || member === null) return member;
      const fields = object(member);
      if (fields.schemaVersion === "midgard-watcher-user-event-snapshot-v1") {
        const { activeEvents, terminalEvents, ...metadata } = fields;
        if (!Array.isArray(activeEvents) || !Array.isArray(terminalEvents))
          throw new Error("watcher stored event collections are invalid");
        return {
          watcherEventSnapshot: {
            metadata,
            activeEvents: saveSequence(activeEvents),
            terminalEvents: saveSequence(terminalEvents),
          },
        };
      }
      return Object.fromEntries(
        Object.entries(fields).map(([key, child]) => {
          if (key === "retainedEntries" && Array.isArray(child))
            return [key, { watcherEventEntries: saveSequence(child) }];
          if (key === "rawBlockCbor" && typeof child === "string")
            return [key, { watcherBlockBytes: saveRaw(child) }];
          return [key, projectEvent(child)];
        }),
      );
    };
    return { encoding: "event", bytes: encode(projectEvent(value)) };
  }
  if (schema === STORE_SCHEMA) return saveStore(object(value));
  const authority = object(value);
  if (!Array.isArray(authority.consistencyHistory))
    throw new Error("watcher stored consistency history is invalid");
  const { consistencyHistory, ...state } = authority;
  return {
    encoding: "authority",
    bytes: encode({
      state: project(state),
      consistencyHistory: saveSequence(consistencyHistory),
    }),
  };
};

/** Reassemble on cold read or when SQLite reports an external change. This
 * checks physical/logical integrity; callers authenticate saved validation. */
export const decodeWatcherDurableRecord = (
  record: WatcherEncodedRecord,
  read: ReadRecord,
): Uint8Array => {
  if (record.encoding === "raw") return record.bytes;
  const payload = object(parse(record.bytes));
  const readSequence = (input: unknown): unknown[] => {
    const sequence = object(input);
    if (
      !Number.isSafeInteger(sequence.count) ||
      (sequence.count as number) < 0 ||
      (sequence.count as number) > 1_000_000 ||
      !Array.isArray(sequence.pages) ||
      sequence.pages.length !==
        Math.ceil((sequence.count as number) / PAGE_SIZE)
    )
      throw new Error("watcher stored record sequence is invalid");
    const result: unknown[] = [];
    for (let index = 0; index < sequence.pages.length; index += 1) {
      const page = object(parse(read(digest(sequence.pages[index]))));
      const expected = Math.min(
        PAGE_SIZE,
        (sequence.count as number) - result.length,
      );
      if (
        page.schemaVersion !== PAGE_SCHEMA ||
        !Array.isArray(page.records) ||
        page.records.length !== expected
      )
        throw new Error("watcher stored record page is invalid");
      for (const key of page.records) result.push(parse(read(digest(key))));
    }
    return result;
  };
  if (record.encoding === "store") {
    const metadata = object(payload.metadata);
    const collections = object(payload.collections);
    const records = Object.fromEntries(
      RECORD_FIELDS.map((field) => [field, readSequence(collections[field])]),
    );
    const store = { ...metadata, ...records } as WatcherDurableStore;
    const caches = rebuildWatcherDurableCaches({
      deploymentMarker: store.deploymentMarker,
      ...records,
    } as WatcherDurableStore);
    const { entries: _entries, ...cacheMetadata } = caches;
    if (
      watcherCanonicalJson(cacheMetadata) !==
      watcherCanonicalJson(metadata.caches)
    )
      throw new Error("watcher stored cache source digest mismatch");
    return encode({
      ...store,
      caches,
    });
  }
  if (record.encoding === "event") {
    const expandEvent = (member: unknown): unknown => {
      if (Array.isArray(member)) return member.map(expandEvent);
      if (typeof member !== "object" || member === null) return member;
      const fields = object(member);
      if (Object.keys(fields).length === 1) {
        if ("watcherEventSnapshot" in fields) {
          const snapshot = object(fields.watcherEventSnapshot);
          return {
            ...object(snapshot.metadata),
            activeEvents: readSequence(snapshot.activeEvents),
            terminalEvents: readSequence(snapshot.terminalEvents),
          };
        }
        if ("watcherEventEntries" in fields)
          return readSequence(fields.watcherEventEntries);
        if ("watcherBlockBytes" in fields)
          return parse(read(digest(fields.watcherBlockBytes)));
      }
      return Object.fromEntries(
        Object.entries(fields).map(([key, child]) => [key, expandEvent(child)]),
      );
    };
    return encode(expandEvent(payload));
  }
  if (record.encoding !== "authority")
    throw new Error("watcher stored record encoding is invalid");
  const expand = (member: unknown): unknown => {
    if (Array.isArray(member)) return member.map(expand);
    if (typeof member !== "object" || member === null) return member;
    const fields = object(member);
    if (Object.keys(fields).length === 1 && "watcherStore" in fields)
      return parse(read(digest(fields.watcherStore)));
    return Object.fromEntries(
      Object.entries(fields).map(([key, child]) => [key, expand(child)]),
    );
  };
  return encode({
    ...object(expand(payload.state)),
    consistencyHistory: readSequence(payload.consistencyHistory),
  });
};
