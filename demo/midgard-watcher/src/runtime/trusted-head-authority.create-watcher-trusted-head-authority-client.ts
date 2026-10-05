import { createHash, timingSafeEqual } from "node:crypto";
import { createServer, type Server } from "node:http";
import { setTimeout as retryDelay } from "node:timers/promises";

import { type WatcherFinalityPolicy } from "../l1/finality-engine.js";
import {
  admitWatcherRollbackDurableTrustedHead,
  type WatcherRollbackDurableTrustedHead,
} from "../l1/rollback-engine.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  exactRecord,
  MAX_REQUEST_BYTES,
  parseJson,
  sameHead,
  TrustedHeadCallerError,
  type WatcherTrustedHeadAuthorityStore,
} from "./trusted-head-authority.exact-record.js";
import {
  endpointUrl,
  type WatcherTrustedHeadAuthorityClient,
} from "./trusted-head-authority.open-watcher-trusted-head-authority-store.js";

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
    // Classification can block the caller beyond this server's idle timeout.
    // Do not leave a pooled socket for the next, non-retriable CAS to reuse.
    connection: "close",
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
        const result = await input.store.compareAndSwap({
          expectedTrustedHead: body.expectedTrustedHead,
          nextTrustedHead: body.nextTrustedHead,
        });
        replyJson(response, result.committed ? 200 : 409, result);
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

/** Connection failures before the request reached the authority. */
const CONNECT_FAILURE_CODES: ReadonlySet<string> = new Set([
  "ECONNREFUSED",
  "ENOTFOUND",
  "EAI_AGAIN",
  "ETIMEDOUT",
  "UND_ERR_CONNECT_TIMEOUT",
]);

/** Transport loss after the request may have reached the authority. */
const IN_FLIGHT_LOSS_CODES: ReadonlySet<string> = new Set([
  "UND_ERR_SOCKET",
  "ECONNRESET",
  "EPIPE",
]);

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
    // Only the two idempotent reads may retry transport loss. One deadline
    // includes every attempt, response body and pause; CAS is never retried.
    const signal = AbortSignal.timeout(input.requestTimeoutMs);
    for (let attempt = 0; ; attempt += 1) {
      let response: Response | undefined;
      try {
        response = await fetch(`${endpoint}${path}`, {
          ...init,
          headers: {
            authorization: `Bearer ${httpSecret}`,
            ...(init?.body === undefined
              ? {}
              : { "content-type": "application/json" }),
          },
          signal,
        });
        const value = (await response.json()) as unknown;
        if (!response.ok && response.status !== 409) {
          throw new Error(
            `trusted-head authority request failed with ${response.status.toString()}`,
          );
        }
        return value;
      } catch (error) {
        const cause = error instanceof TypeError ? error.cause : undefined;
        const code =
          cause instanceof Error &&
          "code" in cause &&
          typeof cause.code === "string"
            ? cause.code
            : undefined;
        // A listener that is down (a restarting authority) never saw the
        // request, so it is waited for until the deadline. A connection lost
        // in flight keeps its three-attempt bound.
        const transient =
          code !== undefined &&
          (CONNECT_FAILURE_CODES.has(code) ||
            (attempt < 2 && IN_FLIGHT_LOSS_CODES.has(code)));
        if (
          init !== undefined ||
          signal.aborted ||
          (response !== undefined && !response.ok) ||
          !transient
        )
          throw error;
        await retryDelay(Math.min(100 * 2 ** attempt, 1_000), undefined, {
          signal,
        }).catch(() => {
          throw error;
        });
      }
    }
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
