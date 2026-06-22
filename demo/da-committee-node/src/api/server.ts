import {
  createServer,
  type IncomingMessage,
  type ServerResponse,
} from "node:http";
import type { AddressInfo } from "node:net";

import type { DaSignatureRecord } from "../domain.js";
import { jsonBigIntStringReplacer } from "../json.js";
import { verifyPeerRequestAuth } from "../peer/auth.js";
import { validateDaSignatureRecord } from "../peer/signatures.js";
import type { DaCommitteeValidation } from "../signer.js";
import type { WatcherStore } from "../store.js";

export type WatcherApiServer = {
  readonly listen: (port: number, host: string) => Promise<void>;
  readonly address: () => AddressInfo | string | null;
  readonly close: () => Promise<void>;
};

export const createWatcherApiServer = ({
  deploymentFingerprint,
  signerIndex,
  signerValidation,
  store,
  ready,
  manifest = {},
  peerReplayWindowMs = 300_000,
  peerMaxBodyBytes = 1_048_576,
  peerRateLimitWindowMs = 60_000,
  peerRateLimitMaxRequests = 120,
}: {
  readonly deploymentFingerprint: string;
  readonly signerIndex?: number;
  readonly signerValidation?: DaCommitteeValidation;
  readonly store: WatcherStore;
  readonly ready: () => boolean;
  readonly manifest?: Record<string, unknown>;
  readonly peerReplayWindowMs?: number;
  readonly peerMaxBodyBytes?: number;
  readonly peerRateLimitWindowMs?: number;
  readonly peerRateLimitMaxRequests?: number;
}): WatcherApiServer => {
  const rateLimiter = new WindowRateLimiter(
    peerRateLimitWindowMs,
    peerRateLimitMaxRequests,
  );
  const server = createServer(async (request, response) => {
    try {
      await routeRequest({
        request,
        response,
        deploymentFingerprint,
        signerIndex,
        signerValidation,
        store,
        ready,
        manifest,
        peerReplayWindowMs,
        peerMaxBodyBytes,
        rateLimiter,
      });
    } catch (error) {
      json(response, 500, {
        error: error instanceof Error ? error.message : String(error),
      });
    }
  });
  return {
    listen: (port, host) =>
      new Promise((resolve) => {
        server.listen(port, host, resolve);
      }),
    address: () => server.address(),
    close: () =>
      new Promise((resolve, reject) => {
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        );
      }),
  };
};

const routeRequest = async ({
  request,
  response,
  deploymentFingerprint,
  signerIndex,
  signerValidation,
  store,
  ready,
  manifest,
  peerReplayWindowMs,
  peerMaxBodyBytes,
  rateLimiter,
}: {
  readonly request: IncomingMessage;
  readonly response: ServerResponse;
  readonly deploymentFingerprint: string;
  readonly signerIndex?: number;
  readonly signerValidation?: DaCommitteeValidation;
  readonly store: WatcherStore;
  readonly ready: () => boolean;
  readonly manifest: Record<string, unknown>;
  readonly peerReplayWindowMs: number;
  readonly peerMaxBodyBytes: number;
  readonly rateLimiter: WindowRateLimiter;
}): Promise<void> => {
  const method = request.method ?? "GET";
  const url = new URL(request.url ?? "/", "http://watcher.local");
  if (method === "GET" && url.pathname === "/healthz") {
    json(response, 200, { ok: true });
    return;
  }
  if (method === "GET" && url.pathname === "/readyz") {
    json(response, ready() ? 200 : 503, { ready: ready() });
    return;
  }
  if (method === "GET" && url.pathname === "/v1/manifest") {
    json(response, 200, manifest);
    return;
  }

  const route = parseHeaderRoute(url.pathname);
  if (route === undefined) {
    json(response, 404, { error: "not found" });
    return;
  }
  if (route.deploymentFingerprint !== deploymentFingerprint) {
    json(response, 404, { error: "unknown deployment" });
    return;
  }

  switch (route.resource) {
    case "signature":
      await handleLegacySignatureRoute({
        method,
        url,
        request,
        response,
        deploymentFingerprint,
        signerIndex,
        signerValidation,
        headerHash: route.headerHash,
        store,
        peerMaxBodyBytes,
      });
      return;
    case "signatures":
      await handleSignaturesRoute({
        method,
        url,
        request,
        response,
        deploymentFingerprint,
        signerIndex,
        signerValidation,
        headerHash: route.headerHash,
        store,
        peerReplayWindowMs,
        peerMaxBodyBytes,
        rateLimiter,
      });
      return;
    case "payload":
      await handlePayloadRoute({
        method,
        response,
        headerHash: route.headerHash,
        store,
      });
      return;
    case "payload/metadata":
      await handlePayloadMetadataRoute({
        method,
        response,
        headerHash: route.headerHash,
        store,
      });
      return;
    case "status":
      await handleStatusRoute({
        method,
        response,
        headerHash: route.headerHash,
        store,
      });
      return;
  }
};

const handleLegacySignatureRoute = async ({
  method,
  url,
  request,
  response,
  deploymentFingerprint,
  signerIndex,
  signerValidation,
  headerHash,
  store,
  peerMaxBodyBytes,
}: {
  readonly method: string;
  readonly url: URL;
  readonly request: IncomingMessage;
  readonly response: ServerResponse;
  readonly deploymentFingerprint: string;
  readonly signerIndex?: number;
  readonly signerValidation?: DaCommitteeValidation;
  readonly headerHash: string;
  readonly store: WatcherStore;
  readonly peerMaxBodyBytes: number;
}): Promise<void> => {
  if (method === "GET") {
    let requestedSignerIndex: number;
    try {
      requestedSignerIndex = parseRequestedSignerIndex(
        url.searchParams.get("signer_index"),
        signerIndex,
      );
    } catch (error) {
      json(response, 400, {
        error: error instanceof Error ? error.message : String(error),
      });
      return;
    }
    const signature = await store.getDaSignature({
      headerHash,
      signerIndex: requestedSignerIndex,
    });
    if (signature === undefined) {
      json(response, 404, { error: "signature not found" });
      return;
    }
    json(response, 200, signature);
    return;
  }
  if (method === "POST") {
    const body = await readJsonOrReject(request, response, peerMaxBodyBytes);
    if (body === undefined) {
      return;
    }
    const validationError = validateDaSignatureRecord({
      body: body as Partial<DaSignatureRecord>,
      headerHash,
      deploymentFingerprint,
      localSignerIndex: signerIndex,
      signerValidation,
    });
    if (validationError !== undefined) {
      json(response, 400, { error: validationError });
      return;
    }
    const now = new Date().toISOString();
    const record = {
      ...(body as DaSignatureRecord),
      broadcastStatus: "posted" as const,
      source: "legacy" as const,
      receivedAt: now,
      verifiedAt: now,
    };
    await store.saveDaSignature(record);
    json(response, 202, { accepted: true, signerIndex: record.signerIndex });
    return;
  }
  json(response, 405, { error: "method not allowed" });
};

const handleSignaturesRoute = async ({
  method,
  url,
  request,
  response,
  deploymentFingerprint,
  signerIndex,
  signerValidation,
  headerHash,
  store,
  peerReplayWindowMs,
  peerMaxBodyBytes,
  rateLimiter,
}: {
  readonly method: string;
  readonly url: URL;
  readonly request: IncomingMessage;
  readonly response: ServerResponse;
  readonly deploymentFingerprint: string;
  readonly signerIndex?: number;
  readonly signerValidation?: DaCommitteeValidation;
  readonly headerHash: string;
  readonly store: WatcherStore;
  readonly peerReplayWindowMs: number;
  readonly peerMaxBodyBytes: number;
  readonly rateLimiter: WindowRateLimiter;
}): Promise<void> => {
  if (method === "GET") {
    const signatures = await store.listDaSignatures(headerHash);
    json(response, 200, { signatures });
    return;
  }
  if (method !== "POST") {
    json(response, 405, { error: "method not allowed" });
    return;
  }
  if (signerValidation === undefined) {
    json(response, 403, {
      error: "peer signatures require committee validation",
    });
    return;
  }
  const rawBody = await readRawOrReject(request, response, peerMaxBodyBytes);
  if (rawBody === undefined) {
    return;
  }
  const pathAndSearch = `${url.pathname}${url.search}`;
  const auth = verifyPeerRequestAuth({
    headers: request.headers,
    signerValidation,
    deploymentFingerprint,
    method,
    pathAndSearch,
    body: rawBody,
    replayWindowMs: peerReplayWindowMs,
  });
  if (!auth.ok) {
    json(response, 401, { error: auth.error });
    return;
  }
  const rateKey = `${auth.fields.signerIndex.toString()}:${
    request.socket.remoteAddress ?? "unknown"
  }`;
  if (!rateLimiter.allow(rateKey)) {
    json(response, 429, { error: "peer write rate limit exceeded" });
    return;
  }
  const acceptedNonce = await store.recordPeerNonce({
    deploymentFingerprint,
    signerIndex: auth.fields.signerIndex,
    nonce: auth.fields.nonce,
    timestampMs: auth.fields.timestampMs,
    receivedAt: new Date().toISOString(),
  });
  if (!acceptedNonce) {
    json(response, 409, { error: "peer request nonce replayed" });
    return;
  }
  let body: unknown;
  try {
    body = JSON.parse(rawBody.toString("utf8")) as unknown;
  } catch {
    json(response, 400, { error: "invalid JSON body" });
    return;
  }
  if (
    typeof body !== "object" ||
    body === null ||
    (body as Partial<DaSignatureRecord>).signerIndex !== auth.fields.signerIndex
  ) {
    json(response, 400, {
      error: "peer auth signer index does not match body",
    });
    return;
  }
  const verifiedPayload = await store.getDaPayload(headerHash);
  if (verifiedPayload === undefined) {
    json(response, 409, { error: "local payload is not available" });
    return;
  }
  const validationError = validateDaSignatureRecord({
    body: body as Partial<DaSignatureRecord>,
    headerHash,
    deploymentFingerprint,
    localSignerIndex: signerIndex,
    signerValidation,
    verifiedPayload,
  });
  if (validationError !== undefined) {
    json(response, 400, { error: validationError });
    return;
  }
  const now = new Date().toISOString();
  const record: DaSignatureRecord = {
    ...(body as DaSignatureRecord),
    broadcastStatus: "posted",
    source: "peer",
    sourcePeer: request.socket.remoteAddress ?? "unknown",
    receivedAt: now,
    verifiedAt: now,
  };
  await store.saveDaSignature(record);
  json(response, 202, { accepted: true, signerIndex: record.signerIndex });
};

const handlePayloadRoute = async ({
  method,
  response,
  headerHash,
  store,
}: {
  readonly method: string;
  readonly response: ServerResponse;
  readonly headerHash: string;
  readonly store: WatcherStore;
}): Promise<void> => {
  if (method !== "GET") {
    json(response, 405, { error: "method not allowed" });
    return;
  }
  const payload = await store.getDaPayload(headerHash);
  if (
    payload === undefined ||
    payload.validationStatus !== "verified" ||
    payload.payloadCborHex.length === 0
  ) {
    json(response, 404, { error: "verified payload not found" });
    return;
  }
  response.writeHead(200, { "content-type": "application/cbor" });
  response.end(Buffer.from(payload.payloadCborHex, "hex"));
};

const handlePayloadMetadataRoute = async ({
  method,
  response,
  headerHash,
  store,
}: {
  readonly method: string;
  readonly response: ServerResponse;
  readonly headerHash: string;
  readonly store: WatcherStore;
}): Promise<void> => {
  if (method !== "GET") {
    json(response, 405, { error: "method not allowed" });
    return;
  }
  const payload = await store.getDaPayload(headerHash);
  if (payload === undefined) {
    json(response, 404, { error: "payload metadata not found" });
    return;
  }
  json(response, 200, payload);
};

const handleStatusRoute = async ({
  method,
  response,
  headerHash,
  store,
}: {
  readonly method: string;
  readonly response: ServerResponse;
  readonly headerHash: string;
  readonly store: WatcherStore;
}): Promise<void> => {
  if (method !== "GET") {
    json(response, 405, { error: "method not allowed" });
    return;
  }
  const [
    header,
    payload,
    signatures,
    candidates,
    submissions,
    broadcasts,
    health,
  ] = await Promise.all([
    store.getStateQueueHeader(headerHash),
    store.getDaPayload(headerHash),
    store.listDaSignatures(headerHash),
    store.listDaAttestationCandidates(headerHash),
    store.listL1Submissions(),
    store.listPeerBroadcasts(headerHash),
    store.listPeerHealth(),
  ]);
  json(response, 200, {
    headerHash,
    header,
    payload,
    signatures,
    candidates,
    l1Submissions: submissions.filter(
      (entry) => entry.headerHash === headerHash,
    ),
    peerBroadcasts: broadcasts,
    peerHealth: health,
  });
};

type HeaderRoute = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly resource:
    | "signature"
    | "signatures"
    | "status"
    | "payload"
    | "payload/metadata";
};

const parseHeaderRoute = (pathname: string): HeaderRoute | undefined => {
  const match = pathname.match(
    /^\/v1\/deployments\/([^/]+)\/headers\/([0-9a-fA-F]{56})\/(.+)$/,
  );
  if (match === null) {
    return undefined;
  }
  const resource = match[3]!;
  if (
    resource !== "signature" &&
    resource !== "signatures" &&
    resource !== "status" &&
    resource !== "payload" &&
    resource !== "payload/metadata"
  ) {
    return undefined;
  }
  return {
    deploymentFingerprint: match[1]!,
    headerHash: match[2]!.toLowerCase(),
    resource,
  };
};

const parseRequestedSignerIndex = (
  value: string | null,
  defaultSignerIndex: number | undefined,
): number => {
  if (value === null) {
    if (defaultSignerIndex === undefined) {
      throw new Error("signer_index is required");
    }
    return defaultSignerIndex;
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 0 || parsed > 255) {
    throw new Error("signer_index must fit in one byte");
  }
  return parsed;
};

const readJsonOrReject = async (
  request: IncomingMessage,
  response: ServerResponse,
  maxBytes: number,
): Promise<unknown | undefined> => {
  const raw = await readRawOrReject(request, response, maxBytes);
  if (raw === undefined) {
    return undefined;
  }
  try {
    return JSON.parse(raw.toString("utf8")) as unknown;
  } catch {
    json(response, 400, { error: "invalid JSON body" });
    return undefined;
  }
};

const readRawOrReject = async (
  request: IncomingMessage,
  response: ServerResponse,
  maxBytes: number,
): Promise<Buffer | undefined> => {
  try {
    return await readRaw(request, maxBytes);
  } catch (error) {
    json(response, error instanceof BodyTooLargeError ? 413 : 400, {
      error: error instanceof Error ? error.message : String(error),
    });
    return undefined;
  }
};

const readRaw = async (
  request: IncomingMessage,
  maxBytes: number,
): Promise<Buffer> => {
  const chunks: Buffer[] = [];
  let byteLength = 0;
  for await (const chunk of request) {
    const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
    byteLength += buffer.length;
    if (byteLength > maxBytes) {
      throw new BodyTooLargeError("request body exceeds configured limit");
    }
    chunks.push(buffer);
  }
  return Buffer.concat(chunks);
};

class BodyTooLargeError extends Error {}

class WindowRateLimiter {
  private readonly windowMs: number;
  private readonly maxRequests: number;
  private readonly buckets = new Map<
    string,
    { count: number; resetAt: number }
  >();

  constructor(windowMs: number, maxRequests: number) {
    this.windowMs = windowMs;
    this.maxRequests = maxRequests;
  }

  allow(key: string, now = Date.now()): boolean {
    const bucket = this.buckets.get(key);
    if (bucket === undefined || bucket.resetAt <= now) {
      this.buckets.set(key, { count: 1, resetAt: now + this.windowMs });
      return true;
    }
    if (bucket.count >= this.maxRequests) {
      return false;
    }
    bucket.count += 1;
    return true;
  }
}

const json = (
  response: ServerResponse,
  statusCode: number,
  body: unknown,
): void => {
  if (response.writableEnded || response.destroyed) {
    return;
  }
  const payload = `${JSON.stringify(body, jsonBigIntStringReplacer)}\n`;
  if (response.headersSent) {
    response.end();
    return;
  }
  response.writeHead(statusCode, { "content-type": "application/json" });
  response.end(payload);
};
