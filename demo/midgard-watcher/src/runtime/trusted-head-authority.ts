import { createHash, createHmac, timingSafeEqual } from "node:crypto";
import {
  type FileHandle,
  mkdir,
  open,
  readdir,
  readFile,
  realpath,
} from "node:fs/promises";
import { createServer, type Server } from "node:http";
import { isAbsolute, join, normalize } from "node:path";

import {
  parseWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import {
  admitWatcherRollbackDurableTrustedHead,
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherRollbackDurableTrustedHead,
} from "../l1/rollback-engine.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";

export const WATCHER_TRUSTED_HEAD_AUTHORITY_SCHEMA_VERSION =
  "midgard-watcher-trusted-head-authority-v1" as const;
export const WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION =
  "midgard-watcher-trusted-head-authority-record-v1" as const;

const RECORD_FILE = /^([0-9]{20})\.json$/u;
const UINT64_MAX = 18_446_744_073_709_551_615n;
const MAX_RECORD_BYTES = 16_384;
const MAX_REQUEST_BYTES = 32_768;
const LOOPBACK_HOSTS = new Set(["127.0.0.1", "localhost", "::1", "[::1]"]);

class TrustedHeadCallerError extends Error {}

type TrustedHeadAuthorityRecord = Readonly<{
  schemaVersion: typeof WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION;
  revision: string;
  priorRecordSha256: string | null;
  head: WatcherRollbackDurableTrustedHead;
  recordAuthenticationKeyId: string;
  recordMac: string;
}>;

type TrustedHeadAuthorityRecordContent = Omit<
  TrustedHeadAuthorityRecord,
  "recordMac"
>;

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Readonly<Record<string, unknown>> | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    return null;
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  return actual.length === expected.length &&
    actual.every((key, index) => key === expected[index])
    ? record
    : null;
};

const canonicalDirectory = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error(
      "trusted-head authority requires a canonical durable directory",
    );
  }
  return value;
};

const revision = (head: WatcherRollbackDurableTrustedHead): bigint => {
  const value = BigInt(head.revision);
  if (value > UINT64_MAX) {
    throw new Error("trusted-head authority revision exceeds uint64");
  }
  return value;
};

const recordName = (value: bigint): string =>
  `${value.toString().padStart(20, "0")}.json`;

const sameHead = (
  left: WatcherRollbackDurableTrustedHead | null,
  right: WatcherRollbackDurableTrustedHead | null,
): boolean =>
  left === null || right === null
    ? left === right
    : watcherCanonicalJson(left) === watcherCanonicalJson(right);

const sameCanonical = (left: unknown, right: unknown): boolean => {
  try {
    return watcherCanonicalJson(left) === watcherCanonicalJson(right);
  } catch {
    return false;
  }
};

const sha256 = (bytes: Uint8Array | string): string =>
  createHash("sha256").update(bytes).digest("hex");

const recordKeyId = (key: Uint8Array): string => sha256(key);

const recordMac = (
  key: Uint8Array,
  content: TrustedHeadAuthorityRecordContent,
): string =>
  createHmac("sha256", key)
    .update(
      `${WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION}:${watcherCanonicalJson(content)}`,
      "utf8",
    )
    .digest("hex");

const makeAuthorityRecord = (input: {
  readonly head: WatcherRollbackDurableTrustedHead;
  readonly priorRecordSha256: string | null;
  readonly recordAuthenticationKey: Uint8Array;
}): TrustedHeadAuthorityRecord => {
  const content = Object.freeze({
    schemaVersion: WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION,
    revision: input.head.revision,
    priorRecordSha256: input.priorRecordSha256,
    head: input.head,
    recordAuthenticationKeyId: recordKeyId(input.recordAuthenticationKey),
  });
  return Object.freeze({
    ...content,
    recordMac: recordMac(input.recordAuthenticationKey, content),
  });
};

const readBounded = async (path: string): Promise<Uint8Array> => {
  const bytes = await readFile(path);
  if (bytes.byteLength === 0 || bytes.byteLength > MAX_RECORD_BYTES) {
    throw new Error("trusted-head authority record size is invalid");
  }
  return Uint8Array.from(bytes);
};

const parseJson = (bytes: Uint8Array): unknown => {
  try {
    return JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
  } catch {
    throw new Error("trusted-head authority record is malformed");
  }
};

const syncDirectory = async (directory: string): Promise<void> => {
  let handle: FileHandle | undefined;
  try {
    handle = await open(directory, "r");
    await handle.sync();
  } finally {
    await handle?.close();
  }
};

export type WatcherTrustedHeadAuthorityStore = Readonly<{
  readRecordAuthenticationKeyId(): Promise<string>;
  readCurrent(): Promise<WatcherRollbackDurableTrustedHead | null>;
  compareAndSwap(input: {
    readonly expectedTrustedHead: unknown | null;
    readonly nextTrustedHead: unknown;
  }): Promise<boolean>;
}>;

/**
 * Opens the operationally independent append-only freshness store. Every
 * startup replays the complete directory and rejects gaps, substitutions,
 * malformed/HMAC-invalid records, and non-canonical bytes.
 */
export const openWatcherTrustedHeadAuthorityStore = async (input: {
  readonly directory: string;
  readonly policy: WatcherFinalityPolicy;
  /** Independently authenticates the append-only sidecar record chain. */
  readonly recordAuthenticationKey: Uint8Array;
}): Promise<WatcherTrustedHeadAuthorityStore> => {
  const directory = canonicalDirectory(input.directory);
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null) {
    throw new Error("trusted-head authority finality policy is invalid");
  }
  const recordAuthenticationKey = Uint8Array.from(
    input.recordAuthenticationKey,
  );
  if (recordAuthenticationKey.byteLength !== 32) {
    throw new Error("trusted-head authority authentication key is invalid");
  }
  await mkdir(directory, { recursive: true, mode: 0o700 });
  if ((await realpath(directory)) !== directory) {
    throw new Error("trusted-head authority directory traverses a symlink");
  }

  const admitHead = (
    value: unknown,
    callerAuthored = false,
  ): WatcherRollbackDurableTrustedHead => {
    const head = exactRecord(value, [
      "schemaVersion",
      "policyDigest",
      "deploymentMarker",
      "authenticationKeyId",
      "revision",
      "snapshotSha256",
      "authorityDigest",
      "headMac",
    ]);
    if (
      head === null ||
      head.schemaVersion !==
        WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION ||
      head.policyDigest !== policy.policyDigest ||
      !sameCanonical(head.deploymentMarker, policy.deploymentMarker) ||
      typeof head.authenticationKeyId !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.authenticationKeyId) ||
      typeof head.revision !== "string" ||
      !/^(?:0|[1-9][0-9]*)$/u.test(head.revision) ||
      typeof head.snapshotSha256 !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.snapshotSha256) ||
      typeof head.authorityDigest !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.authorityDigest) ||
      typeof head.headMac !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.headMac)
    ) {
      const ErrorType = callerAuthored ? TrustedHeadCallerError : Error;
      throw new ErrorType("trusted-head authority record structure failed");
    }
    const admitted = Object.freeze({
      schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
      policyDigest: head.policyDigest,
      deploymentMarker: policy.deploymentMarker,
      authenticationKeyId: head.authenticationKeyId,
      revision: head.revision,
      snapshotSha256: head.snapshotSha256,
      authorityDigest: head.authorityDigest,
      headMac: head.headMac,
    }) as WatcherRollbackDurableTrustedHead;
    revision(admitted);
    return admitted;
  };

  const admitRecord = (value: unknown): TrustedHeadAuthorityRecord => {
    const record = exactRecord(value, [
      "schemaVersion",
      "revision",
      "priorRecordSha256",
      "head",
      "recordAuthenticationKeyId",
      "recordMac",
    ]);
    if (
      record === null ||
      record.schemaVersion !==
        WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION ||
      typeof record.revision !== "string" ||
      !/^(?:0|[1-9][0-9]*)$/u.test(record.revision) ||
      (record.priorRecordSha256 !== null &&
        (typeof record.priorRecordSha256 !== "string" ||
          !/^[0-9a-f]{64}$/u.test(record.priorRecordSha256))) ||
      record.recordAuthenticationKeyId !==
        recordKeyId(recordAuthenticationKey) ||
      typeof record.recordMac !== "string" ||
      !/^[0-9a-f]{64}$/u.test(record.recordMac)
    ) {
      throw new Error("trusted-head authority sidecar record is invalid");
    }
    const head = admitHead(record.head);
    if (head.revision !== record.revision) {
      throw new Error(
        "trusted-head authority record revision differs from head",
      );
    }
    const content = Object.freeze({
      schemaVersion: WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION,
      revision: record.revision,
      priorRecordSha256: record.priorRecordSha256 as string | null,
      head,
      recordAuthenticationKeyId: record.recordAuthenticationKeyId as string,
    });
    const expectedMac = recordMac(recordAuthenticationKey, content);
    if (
      !timingSafeEqual(
        Buffer.from(expectedMac, "hex"),
        Buffer.from(record.recordMac, "hex"),
      )
    ) {
      throw new Error("trusted-head authority sidecar record MAC is invalid");
    }
    return Object.freeze({
      ...content,
      recordMac: record.recordMac,
    }) as TrustedHeadAuthorityRecord;
  };

  const scan = async (): Promise<Readonly<{
    head: WatcherRollbackDurableTrustedHead;
    recordSha256: string;
  }> | null> => {
    const entries = await readdir(directory, { withFileTypes: true });
    const names = entries.map((entry) => {
      if (!entry.isFile() || !RECORD_FILE.test(entry.name)) {
        throw new Error(
          "trusted-head authority directory has an unknown entry",
        );
      }
      return entry.name;
    });
    names.sort();
    let previous: Readonly<{
      head: WatcherRollbackDurableTrustedHead;
      recordSha256: string;
    }> | null = null;
    for (let index = 0; index < names.length; index += 1) {
      const name = names[index]!;
      const expectedRevision = BigInt(index);
      if (name !== recordName(expectedRevision)) {
        throw new Error("trusted-head authority revision chain has a gap");
      }
      const bytes = await readBounded(join(directory, name));
      const sidecarRecord = admitRecord(parseJson(bytes));
      const expectedPriorRecordSha256 = previous?.recordSha256 ?? null;
      if (
        revision(sidecarRecord.head) !== expectedRevision ||
        sidecarRecord.priorRecordSha256 !== expectedPriorRecordSha256 ||
        new TextDecoder().decode(bytes) !== watcherCanonicalJson(sidecarRecord)
      ) {
        throw new Error("trusted-head authority record is non-canonical");
      }
      if (
        previous !== null &&
        revision(sidecarRecord.head) !== revision(previous.head) + 1n
      ) {
        throw new Error(
          "trusted-head authority revision chain is discontinuous",
        );
      }
      previous = Object.freeze({
        head: sidecarRecord.head,
        recordSha256: sha256(bytes),
      });
    }
    return previous;
  };

  await scan();

  return Object.freeze({
    readRecordAuthenticationKeyId: async () =>
      recordKeyId(recordAuthenticationKey),
    readCurrent: async () => (await scan())?.head ?? null,
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      const expected =
        expectedTrustedHead === null
          ? null
          : admitHead(expectedTrustedHead, true);
      const next = admitHead(nextTrustedHead, true);
      const nextRevision = revision(next);
      if (
        (expected === null && nextRevision !== 0n) ||
        (expected !== null && nextRevision !== revision(expected) + 1n)
      ) {
        return false;
      }
      const current = await scan();
      if (!sameHead(current?.head ?? null, expected)) return false;
      const sidecarRecord = makeAuthorityRecord({
        head: next,
        priorRecordSha256: current?.recordSha256 ?? null,
        recordAuthenticationKey,
      });

      const path = join(directory, recordName(nextRevision));
      let handle: FileHandle | undefined;
      try {
        handle = await open(path, "wx", 0o600);
        await handle.writeFile(watcherCanonicalJson(sidecarRecord), {
          encoding: "utf8",
        });
        await handle.sync();
      } catch (error) {
        if ((error as NodeJS.ErrnoException).code === "EEXIST") return false;
        throw error;
      } finally {
        await handle?.close();
      }
      await syncDirectory(directory);
      return sameHead((await scan())?.head ?? null, next);
    },
  });
};

export type WatcherTrustedHeadAuthorityClient = Readonly<{
  readRecordAuthenticationKeyId(): Promise<string>;
  readCurrent(): Promise<WatcherRollbackDurableTrustedHead | null>;
  compareAndSwap(input: {
    readonly expectedTrustedHead: WatcherRollbackDurableTrustedHead | null;
    readonly nextTrustedHead: WatcherRollbackDurableTrustedHead;
  }): Promise<boolean>;
}>;

const endpointUrl = (value: unknown): URL => {
  let url: URL;
  try {
    url = new URL(String(value));
  } catch {
    throw new Error("trusted-head authority endpoint is invalid");
  }
  if (
    url.protocol !== "http:" ||
    !LOOPBACK_HOSTS.has(url.hostname.toLowerCase()) ||
    url.username !== "" ||
    url.password !== "" ||
    url.search !== "" ||
    url.hash !== "" ||
    (url.pathname !== "/" && url.pathname !== "")
  ) {
    throw new Error("trusted-head authority endpoint must be loopback HTTP");
  }
  return url;
};

const secret = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    value.length < 32 ||
    value.length > 256
  ) {
    throw new Error("trusted-head authority HTTP secret is invalid");
  }
  return value;
};

const authorized = (header: string | undefined, expected: string): boolean => {
  const actual = header?.startsWith("Bearer ") ? header.slice(7) : "";
  const left = createHash("sha256").update(actual, "utf8").digest();
  const right = createHash("sha256").update(expected, "utf8").digest();
  return timingSafeEqual(left, right);
};

const readRequestBody = async (
  request: AsyncIterable<Uint8Array>,
): Promise<unknown> => {
  const chunks: Uint8Array[] = [];
  let length = 0;
  for await (const chunk of request) {
    length += chunk.byteLength;
    if (length > MAX_REQUEST_BYTES) {
      throw new TrustedHeadCallerError(
        "trusted-head authority request is too large",
      );
    }
    chunks.push(Uint8Array.from(chunk));
  }
  try {
    return parseJson(Buffer.concat(chunks));
  } catch {
    throw new TrustedHeadCallerError(
      "trusted-head authority request is malformed",
    );
  }
};

const replyJson = (
  response: import("node:http").ServerResponse,
  status: number,
  value: unknown,
): void => {
  response.writeHead(status, {
    "content-type": "application/json",
    "cache-control": "no-store",
  });
  response.end(watcherCanonicalJson(value));
};

export type WatcherTrustedHeadAuthorityServer = Readonly<{
  endpoint: string;
  close(): Promise<void>;
}>;

export const startWatcherTrustedHeadAuthorityServer = async (input: {
  readonly endpoint: string;
  readonly httpSecret: string;
  readonly store: WatcherTrustedHeadAuthorityStore;
  readonly unsafeAllowEphemeralPortForTest?: true;
}): Promise<WatcherTrustedHeadAuthorityServer> => {
  const endpoint = endpointUrl(input.endpoint);
  const httpSecret = secret(input.httpSecret);
  const port = Number(endpoint.port || "80");
  if (port === 0 && input.unsafeAllowEphemeralPortForTest !== true) {
    throw new Error(
      "trusted-head authority production port cannot be ephemeral",
    );
  }
  // The request body catches failures and always writes a bounded response.
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  const server: Server = createServer(async (request, response) => {
    try {
      if (!authorized(request.headers.authorization, httpSecret)) {
        replyJson(response, 401, { error: "unauthorized" });
        return;
      }
      if (request.method === "GET" && request.url === "/v1/trusted-head") {
        replyJson(response, 200, { head: await input.store.readCurrent() });
        return;
      }
      if (request.method === "GET" && request.url === "/v1/identity") {
        replyJson(response, 200, {
          recordAuthenticationKeyId:
            await input.store.readRecordAuthenticationKeyId(),
        });
        return;
      }
      if (request.method === "POST" && request.url === "/v1/trusted-head/cas") {
        const body = exactRecord(await readRequestBody(request), [
          "expectedTrustedHead",
          "nextTrustedHead",
        ]);
        if (body === null) {
          replyJson(response, 400, { error: "invalid_request" });
          return;
        }
        const committed = await input.store.compareAndSwap({
          expectedTrustedHead: body.expectedTrustedHead,
          nextTrustedHead: body.nextTrustedHead,
        });
        replyJson(response, committed ? 200 : 409, {
          committed,
          head: await input.store.readCurrent(),
        });
        return;
      }
      replyJson(response, 404, { error: "not_found" });
    } catch (error) {
      if (error instanceof TrustedHeadCallerError) {
        replyJson(response, 400, { error: "invalid_request" });
      } else {
        replyJson(response, 500, { error: "persistence_failure" });
      }
    }
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(port, endpoint.hostname, () => {
      server.off("error", reject);
      resolve();
    });
  });
  const address = server.address();
  if (address === null || typeof address === "string") {
    server.close();
    throw new Error("trusted-head authority did not bind TCP");
  }
  const publishedEndpoint = `http://${
    address.address.includes(":") ? `[${address.address}]` : address.address
  }:${address.port.toString()}`;
  return Object.freeze({
    endpoint: publishedEndpoint,
    close: async () =>
      await new Promise<void>((resolve, reject) =>
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        ),
      ),
  });
};

export const createWatcherTrustedHeadAuthorityClient = (input: {
  readonly endpoint: string;
  readonly httpSecret: string;
  readonly policy: WatcherFinalityPolicy;
  readonly authenticationKey: Uint8Array;
  readonly requestTimeoutMs: number;
}): WatcherTrustedHeadAuthorityClient => {
  const endpoint = endpointUrl(input.endpoint).toString().replace(/\/$/u, "");
  const httpSecret = secret(input.httpSecret);
  const admit = (value: unknown): WatcherRollbackDurableTrustedHead => {
    const head = admitWatcherRollbackDurableTrustedHead({
      head: value,
      policy: input.policy,
      authenticationKey: input.authenticationKey,
    });
    if (head === null)
      throw new Error("trusted-head authority returned an invalid head");
    return head;
  };
  const call = async (path: string, init?: RequestInit): Promise<unknown> => {
    const response = await fetch(`${endpoint}${path}`, {
      ...init,
      headers: {
        authorization: `Bearer ${httpSecret}`,
        ...(init?.body === undefined
          ? {}
          : { "content-type": "application/json" }),
      },
      signal: AbortSignal.timeout(input.requestTimeoutMs),
    });
    const value = (await response.json()) as unknown;
    if (!response.ok && response.status !== 409) {
      throw new Error(
        `trusted-head authority request failed with ${response.status.toString()}`,
      );
    }
    return value;
  };
  return Object.freeze({
    readRecordAuthenticationKeyId: async () => {
      const body = exactRecord(await call("/v1/identity"), [
        "recordAuthenticationKeyId",
      ]);
      if (
        body === null ||
        typeof body.recordAuthenticationKeyId !== "string" ||
        !/^[0-9a-f]{64}$/u.test(body.recordAuthenticationKeyId)
      ) {
        throw new Error("trusted-head authority identity response is invalid");
      }
      return body.recordAuthenticationKeyId;
    },
    readCurrent: async () => {
      const body = exactRecord(await call("/v1/trusted-head"), ["head"]);
      if (body === null)
        throw new Error("trusted-head authority response is invalid");
      return body.head === null ? null : admit(body.head);
    },
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      const body = exactRecord(
        await call("/v1/trusted-head/cas", {
          method: "POST",
          body: watcherCanonicalJson({
            expectedTrustedHead,
            nextTrustedHead,
          }),
        }),
        ["committed", "head"],
      );
      if (body === null || typeof body.committed !== "boolean") {
        throw new Error("trusted-head authority CAS response is invalid");
      }
      const head = body.head === null ? null : admit(body.head);
      if (body.committed && !sameHead(head, nextTrustedHead)) {
        throw new Error(
          "trusted-head authority CAS read-back differs from publication",
        );
      }
      return body.committed;
    },
  });
};
