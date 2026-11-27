#!/usr/bin/env node

/**
 * Single-shot Preprod health, pipeline-stall, and wallet-balance monitor.
 * Intended to be run from cron or a systemd timer; prints JSON to stdout,
 * posts to ALERT_WEBHOOK_URL (Discord or Slack) on failure, and exits 0/1.
 *
 * Env:
 *   MIDGARD_NODE_URL            Required (e.g. https://preprod.midgard.example)
 *   MIDGARD_WATCHER_HEALTH_URL  Optional watcher health URL
 *   ALERT_WEBHOOK_URL           Optional Discord or Slack webhook URL
 *   STALL_THRESHOLD_MS          Optional max pending block age (default 900000 = 15m)
 *   REQUEST_TIMEOUT_MS          Optional per-request timeout (default 10000)
 *   WALLET_CHECK_ADDRESSES      Optional comma-separated label=addr_test1... list
 *   WALLET_MIN_LOVELACE         Optional minimum lovelace per wallet (default 50000000)
 *   BLOCKFROST_URL              Optional (default https://cardano-preprod.blockfrost.io/api/v0)
 *   BLOCKFROST_PROJECT_ID       Optional Blockfrost project_id header
 */

import { realpathSync } from "node:fs";
import { fileURLToPath } from "node:url";

const DEFAULT_STALL_MS = 900_000;
const DEFAULT_TIMEOUT_MS = 10_000;
const DEFAULT_MIN_LOVELACE = 50_000_000n;
const DEFAULT_BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";
const DISCORD_WEBHOOK =
  /^https:\/\/(?:[^/]+\.)?discord(?:app)?\.com\/api\/(?:v\d+\/)?webhooks\//u;

const trimUrl = (raw) => raw?.trim().replace(/\/+$/u, "") || undefined;

const fetchJson = async (
  url,
  { timeoutMs, fetchImpl = fetch, headers = {} },
) => {
  const res = await fetchImpl(url, {
    headers: { accept: "application/json", ...headers },
    signal: AbortSignal.timeout(timeoutMs),
  });
  const text = await res.text();
  try {
    return { status: res.status, data: text.trim() ? JSON.parse(text) : null };
  } catch {
    return { status: res.status, data: null };
  }
};

const safeCheck = async (id, fn) => {
  try {
    return await fn();
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return { id, ok: false, message };
  }
};

export const parseWallets = (raw) =>
  (raw ?? "")
    .split(",")
    .map((entry) => entry.trim())
    .filter(Boolean)
    .map((entry) => {
      const i = entry.indexOf("=");
      if (i <= 0 || i === entry.length - 1)
        throw new Error(`Invalid WALLET_CHECK_ADDRESSES entry: ${entry}`);
      return {
        label: entry.slice(0, i).trim(),
        address: entry.slice(i + 1).trim(),
      };
    });

export const formatWebhookPayload = (webhookUrl, alerts) => {
  const summary = `Midgard Preprod alert (${alerts.length}):\n${alerts.map((a) => `- [${a.id}] ${a.message}`).join("\n")}`;
  return DISCORD_WEBHOOK.test(webhookUrl)
    ? { content: summary }
    : { text: summary, alerts };
};

export const runChecks = async (env, fetchImpl = fetch) => {
  const nodeUrl = trimUrl(env.MIDGARD_NODE_URL);
  if (!nodeUrl) throw new Error("MIDGARD_NODE_URL is required");
  const watcherUrl = trimUrl(env.MIDGARD_WATCHER_HEALTH_URL);
  const blockfrostUrl = trimUrl(env.BLOCKFROST_URL) ?? DEFAULT_BLOCKFROST_URL;
  const projectId = env.BLOCKFROST_PROJECT_ID?.trim() || undefined;
  const stallMs = Number(env.STALL_THRESHOLD_MS || DEFAULT_STALL_MS);
  const timeoutMs = Number(env.REQUEST_TIMEOUT_MS || DEFAULT_TIMEOUT_MS);
  const minLovelace = BigInt(env.WALLET_MIN_LOVELACE || DEFAULT_MIN_LOVELACE);
  const wallets = parseWallets(env.WALLET_CHECK_ADDRESSES);
  const opts = { timeoutMs, fetchImpl };

  const tasks = [
    safeCheck("node:readyz", async () => {
      const { status, data } = await fetchJson(`${nodeUrl}/readyz`, opts);
      if (status === 200 && data?.ready === true)
        return { id: "node:readyz", ok: true, message: "ready" };
      const reasons = Array.isArray(data?.reasons)
        ? ` (${data.reasons.join(", ")})`
        : "";
      return {
        id: "node:readyz",
        ok: false,
        message: `HTTP ${status}, ready=${data?.ready ?? null}${reasons}`,
      };
    }),
    safeCheck("node:pipeline", async () => {
      const { status, data } = await fetchJson(
        `${nodeUrl}/pipeline-status`,
        opts,
      );
      if (status !== 200 || !data || typeof data !== "object")
        return { id: "node:pipeline", ok: false, message: `HTTP ${status}` };
      const oldestAge =
        data.pendingBlockFinalizations?.oldestActive?.ageMs ?? 0;
      const unconfirmedAge =
        data.stateQueue?.unconfirmedSubmittedBlockAgeMs ?? 0;
      if (oldestAge >= stallMs || unconfirmedAge >= stallMs) {
        return {
          id: "node:pipeline",
          ok: false,
          message: `stalled (oldestActiveAgeMs=${oldestAge}, unconfirmedAgeMs=${unconfirmedAge}, limit=${stallMs})`,
        };
      }
      return { id: "node:pipeline", ok: true, message: "ok" };
    }),
  ];

  if (watcherUrl) {
    tasks.push(
      safeCheck("watcher:health", async () => {
        const { status } = await fetchJson(watcherUrl, opts);
        return status === 200
          ? { id: "watcher:health", ok: true, message: "ok" }
          : { id: "watcher:health", ok: false, message: `HTTP ${status}` };
      }),
    );
  }

  for (const { label, address } of wallets) {
    tasks.push(
      safeCheck(`wallet:${label}`, async () => {
        const headers = projectId ? { project_id: projectId } : {};
        const { status, data } = await fetchJson(
          `${blockfrostUrl}/addresses/${encodeURIComponent(address)}`,
          { ...opts, headers },
        );
        const lovelace =
          status === 404
            ? 0n
            : status === 200 && Array.isArray(data?.amount)
              ? BigInt(
                  data.amount.find((a) => a.unit === "lovelace")?.quantity ??
                    "0",
                )
              : null;
        if (lovelace === null)
          return {
            id: `wallet:${label}`,
            ok: false,
            message: `HTTP ${status}`,
          };
        const ada = (Number(lovelace) / 1_000_000).toFixed(2);
        return lovelace >= minLovelace
          ? { id: `wallet:${label}`, ok: true, message: `${ada} tADA` }
          : {
              id: `wallet:${label}`,
              ok: false,
              message: `low balance ${ada} tADA (< ${(Number(minLovelace) / 1_000_000).toFixed(2)} tADA)`,
            };
      }),
    );
  }

  const checks = await Promise.all(tasks);
  const alerts = checks.filter((check) => !check.ok);
  const webhookUrl = trimUrl(env.ALERT_WEBHOOK_URL);
  if (alerts.length > 0 && webhookUrl) {
    await fetchImpl(webhookUrl, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(formatWebhookPayload(webhookUrl, alerts)),
      signal: AbortSignal.timeout(timeoutMs),
    });
  }
  return { ok: alerts.length === 0, checks };
};

const isMain = () =>
  process.argv[1] !== undefined &&
  realpathSync(fileURLToPath(import.meta.url)) ===
    realpathSync(process.argv[1]);

if (isMain()) {
  runChecks(process.env)
    .then((result) => {
      process.stdout.write(`${JSON.stringify(result)}\n`);
      process.exitCode = result.ok ? 0 : 1;
    })
    .catch((error) => {
      process.stderr.write(
        `preprod-monitor: ${error instanceof Error ? error.message : String(error)}\n`,
      );
      process.exitCode = 1;
    });
}
