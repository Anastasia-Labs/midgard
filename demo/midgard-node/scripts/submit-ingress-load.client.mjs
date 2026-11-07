// The load client: sends corpus requests to `/submit` one tx per request, at
// a fixed offered rate (open loop), mostly on new connections, from many
// client addresses.
//
// Client addresses, two ways:
// - Straight to the node: each request carries a synthetic address in the
//   trusted-proxy header (`x-midgard-client-ip`), which the node reads only
//   from a socket peer listed in HTTP_TRUSTED_PROXY_PEERS. Without it every
//   request is one client and the per-client limits cap the run.
// - Through the nginx edge, which overwrites that header with the socket
//   address: `loopbackSources` binds each connection to one of that many
//   127.x.y.z source addresses, which an edge listening on the host's
//   loopback sees as distinct clients. A port Docker publishes through its
//   userland proxy hides the source address, so the edge must run on the host
//   network for this to work.
import http from "node:http";
import https from "node:https";

export const CLIENT_IP_HEADER = "x-midgard-client-ip";
export const SUBMIT_CONTENT_TYPE = "application/vnd.midgard.v1+cbor";

/** The `index`-th synthetic client address, in 10.0.0.0/8. */
export const syntheticClientIp = (index) => {
  const value = (index % 16_777_214) + 1;
  return `10.${(value >>> 16) & 0xff}.${(value >>> 8) & 0xff}.${value & 0xff}`;
};

/**
 * The `index`-th loopback source address, from 127.1.0.1 up. Linux routes all
 * of 127.0.0.0/8 to the loopback interface, so a client can bind any of them
 * as its source without configuration, and a server on loopback (an nginx on
 * the host network) sees each as a different client address.
 */
export const loopbackSourceAddress = (index) => {
  const value = 65_537 + (index % 16_646_142);
  return `127.${(value >>> 16) & 0xff}.${(value >>> 8) & 0xff}.${value & 0xff}`;
};

/** The `q` quantile (0..1) of an ascending array, or null when empty. */
export const quantile = (sorted, q) => {
  if (sorted.length === 0) return null;
  const index = Math.min(
    sorted.length - 1,
    Math.max(0, Math.ceil(q * sorted.length) - 1),
  );
  return sorted[index];
};

/**
 * The outcome label of one response: status and, for a JSON body, its
 * `status` (admissions) or `error` (refusals) field.
 */
export const outcomeLabel = (statusCode, bodyText) => {
  let detail = "";
  try {
    const body = JSON.parse(bodyText);
    if (typeof body?.error === "string") detail = body.error;
    else if (typeof body?.status === "string") detail = body.status;
  } catch {
    detail = "";
  }
  // Private profiles return free-form diagnostics; keep labels bounded.
  if (detail.length > 64) detail = `${detail.slice(0, 61)}...`;
  return detail === "" ? String(statusCode) : `${statusCode} ${detail}`;
};

const sendOne = ({ target, body, agent, clientIp, localAddress, timeoutMs }) =>
  new Promise((resolve) => {
    const startedAt = process.hrtime.bigint();
    const finish = (outcome, statusCode) =>
      resolve({
        outcome,
        statusCode,
        latencyMs: Number(process.hrtime.bigint() - startedAt) / 1e6,
      });
    const headers = {
      "content-type": SUBMIT_CONTENT_TYPE,
      "content-length": String(body.length),
      ...(agent === false ? { connection: "close" } : {}),
      ...(clientIp === undefined ? {} : { [CLIENT_IP_HEADER]: clientIp }),
    };
    const transport = target.url.protocol === "https:" ? https : http;
    const request = transport.request(
      {
        ...(target.socketPath === undefined
          ? {
              hostname: target.url.hostname,
              port: target.url.port === "" ? undefined : target.url.port,
            }
          : { socketPath: target.socketPath }),
        path: `${target.url.pathname.replace(/\/+$/u, "")}/submit`,
        method: "POST",
        headers,
        agent,
        ...(localAddress === undefined ? {} : { localAddress }),
        timeout: timeoutMs,
        ...(target.insecure ? { rejectUnauthorized: false } : {}),
      },
      (response) => {
        const chunks = [];
        response.on("data", (chunk) => chunks.push(chunk));
        response.on("end", () =>
          finish(
            outcomeLabel(
              response.statusCode ?? 0,
              Buffer.concat(chunks).toString("utf8"),
            ),
            response.statusCode ?? 0,
          ),
        );
        response.on("error", (error) =>
          finish(`error ${error.code ?? error.message}`, 0),
        );
      },
    );
    request.on("timeout", () => request.destroy(new Error("timeout")));
    request.on("error", (error) =>
      finish(`error ${error.code ?? error.message}`, 0),
    );
    request.end(body);
  });

const bump = (record, key) => {
  record[key] = (record[key] ?? 0) + 1;
};

/**
 * Offers `requests` (corpus records `{kind, body}`) at `ratePerSecond` for
 * `durationMs` (or until the corpus runs out), never more than `maxInFlight`
 * at once. A request that is due while `maxInFlight` are open waits; the
 * report counts how far the client fell behind, so a saturated client is
 * never mistaken for a saturated node.
 */
export const runLoad = async ({
  requests,
  target,
  ratePerSecond,
  durationMs,
  maxInFlight = 512,
  keepAliveShare = 0.1,
  clientIpCount = 4_096,
  useClientIpHeader = true,
  loopbackSources = 0,
  timeoutMs = 10_000,
  onProgress = () => {},
}) => {
  if (!(ratePerSecond > 0)) throw new Error("ratePerSecond must be positive");
  const keepAliveAgent = new (
    target.url.protocol === "https:" ? https : http
  ).Agent({ keepAlive: true, maxSockets: maxInFlight });
  const results = [];
  const admittedLatencies = [];
  const byKind = {};
  const outcomes = {};
  let sent = 0;
  let inFlight = 0;
  let maxLagRequests = 0;
  const startedAt = performance.now();
  const endsAt = startedAt + durationMs;
  const inFlightDone = new Set();
  let nextProgressAt = startedAt + 1_000;

  while (true) {
    const now = performance.now();
    if (now >= endsAt || sent >= requests.length) break;
    // Request k is due at k / rate seconds, the first at once.
    const due = Math.min(
      requests.length,
      Math.floor(((now - startedAt) / 1_000) * ratePerSecond) + 1,
    );
    maxLagRequests = Math.max(maxLagRequests, due - sent);
    while (sent < due && inFlight < maxInFlight) {
      const index = sent;
      const request = requests[index];
      sent += 1;
      inFlight += 1;
      const promise = sendOne({
        target,
        body: request.body,
        // Spread keep-alive requests evenly through the run.
        agent:
          Math.floor((index + 1) * keepAliveShare) >
          Math.floor(index * keepAliveShare)
            ? keepAliveAgent
            : false,
        clientIp: useClientIpHeader
          ? syntheticClientIp(index % clientIpCount)
          : undefined,
        localAddress:
          loopbackSources > 0
            ? loopbackSourceAddress(index % loopbackSources)
            : undefined,
        timeoutMs,
      }).then((result) => {
        inFlight -= 1;
        inFlightDone.delete(promise);
        results.push(result.latencyMs);
        bump(outcomes, result.outcome);
        byKind[request.kind] ??= {};
        bump(byKind[request.kind], result.outcome);
        if (result.statusCode === 202) admittedLatencies.push(result.latencyMs);
      });
      inFlightDone.add(promise);
    }
    if (now >= nextProgressAt) {
      onProgress({ elapsedMs: now - startedAt, sent, inFlight });
      nextProgressAt += 1_000;
    }
    await new Promise((resolve) => setTimeout(resolve, 1));
  }
  const offeredMs = performance.now() - startedAt;
  // Requests due in [0, durationMs), or up to the moment the corpus ran out.
  const dueAtEnd = Math.min(
    requests.length,
    offeredMs >= durationMs
      ? Math.ceil((durationMs / 1_000) * ratePerSecond)
      : Math.floor((offeredMs / 1_000) * ratePerSecond) + 1,
  );
  await Promise.all([...inFlightDone]);
  const wallMs = performance.now() - startedAt;
  keepAliveAgent.destroy();

  const latencies = [...results].sort((a, b) => a - b);
  const admitted = [...admittedLatencies].sort((a, b) => a - b);
  const admittedCount = byOutcomePrefix(outcomes, "202");
  return {
    offeredRatePerSecond: ratePerSecond,
    offeredMs,
    wallMs,
    sent,
    notSentWhenDue: Math.max(0, dueAtEnd - sent),
    maxLagRequests,
    completed: latencies.length,
    admitted: admittedCount,
    admittedPerSecond: admittedCount / (offeredMs / 1_000),
    outcomes,
    byKind,
    latencyMs: summarize(latencies),
    admittedLatencyMs: summarize(admitted),
  };
};

const byOutcomePrefix = (outcomes, prefix) =>
  Object.entries(outcomes)
    .filter(([label]) => label === prefix || label.startsWith(`${prefix} `))
    .reduce((sum, [, count]) => sum + count, 0);

const summarize = (sorted) => ({
  count: sorted.length,
  p50: quantile(sorted, 0.5),
  p90: quantile(sorted, 0.9),
  p99: quantile(sorted, 0.99),
  max: sorted.length === 0 ? null : sorted[sorted.length - 1],
});
