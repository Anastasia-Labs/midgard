import { createHash, randomUUID } from "node:crypto";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmdirSync,
  unlinkSync,
} from "node:fs";
import { join } from "node:path";

import { isPlainRecord } from "@al-ft/midgard-core/narrowing";

import { codeStamp, runtimeDistTargets } from "./dist-freshness.js";
import { writeDurableJson } from "./durable.js";
import {
  HISTORY_CHILD_ROLES,
  type HistoryChildRole,
} from "./history-child-evidence.js";
import {
  type HistoryDaemonScope,
  queryHistoryDaemon,
  startHistoryDaemonQuery,
} from "./history-daemon-query.js";
import { historyProofRemaining } from "./history-proof-deadline.js";
import { historyPublicFile } from "./history-public-file.js";
import type { createHistoryRoleRegistry } from "./history-role-registry.js";
import { historyAdmissionPublicPaths } from "./history-signed-admission.js";
import { type Layout, readRunEnv } from "./layout.js";
import { lockOwner, processStartTime } from "./lock.js";
import {
  recoveryScope,
  recoveryScopeMatches,
  specsDigest,
} from "./service-recovery-scope.js";
import type { ServiceReport } from "./stack.js";
import type { ServiceSpec, SupervisorPaths } from "./supervisor.js";

type Registry = ReturnType<typeof createHistoryRoleRegistry>;
export type HistoryDaemonChild = Readonly<{
  role: HistoryChildRole;
  serviceName: string;
  pid: number;
  attemptId: string;
}>;
type Descriptor = Readonly<{
  schema: "history-daemon-discovery-v1";
  scope: HistoryDaemonScope;
  socketPath: string | null;
  children: readonly HistoryDaemonChild[];
}>;
const tuple = (children: readonly HistoryDaemonChild[]) =>
  JSON.stringify(children);
const sameScope = (a: HistoryDaemonScope, b: HistoryDaemonScope) =>
  JSON.stringify(a) === JSON.stringify(b);
const hex = (v: unknown): v is string =>
  typeof v === "string" && /^[0-9a-f]{64}$/u.test(v);
const uuid = (v: unknown): v is string =>
  typeof v === "string" &&
  /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/u.test(v);
const role = (v: unknown): v is HistoryChildRole =>
  HISTORY_CHILD_ROLES.some((r) => r === v);
const keys = (value: Record<string, unknown>, names: readonly string[]) =>
  Object.keys(value).sort().join() === [...names].sort().join();
const parseDescriptor = (value: unknown): Descriptor | undefined => {
  if (
    !isPlainRecord(value) ||
    !keys(value, ["schema", "scope", "socketPath", "children"]) ||
    value.schema !== "history-daemon-discovery-v1" ||
    !isPlainRecord(value.scope) ||
    !Array.isArray(value.children) ||
    value.children.length !== 4 ||
    typeof value.socketPath !== "string"
  )
    return undefined;
  const s = value.scope;
  if (
    !keys(s, [
      "runId",
      "daemonPid",
      "codeStamp",
      "serviceSpecsDigest",
      "incarnation",
    ]) ||
    typeof s.runId !== "string" ||
    s.runId.length === 0 ||
    s.runId.length > 200 ||
    typeof s.daemonPid !== "number" ||
    !Number.isSafeInteger(s.daemonPid) ||
    s.daemonPid <= 0 ||
    !hex(s.codeStamp) ||
    !hex(s.serviceSpecsDigest) ||
    !uuid(s.incarnation)
  )
    return undefined;
  const children: HistoryDaemonChild[] = [];
  for (const child of value.children) {
    if (
      !isPlainRecord(child) ||
      !keys(child, ["role", "serviceName", "pid", "attemptId"]) ||
      !role(child.role) ||
      typeof child.serviceName !== "string" ||
      child.serviceName.length === 0 ||
      child.serviceName.length > 200 ||
      typeof child.pid !== "number" ||
      !Number.isSafeInteger(child.pid) ||
      child.pid <= 0 ||
      !uuid(child.attemptId)
    )
      return undefined;
    children.push({
      role: child.role,
      serviceName: child.serviceName,
      pid: child.pid,
      attemptId: child.attemptId,
    });
  }
  if (
    children.some(
      (child, index) => child.role !== HISTORY_CHILD_ROLES[index],
    ) ||
    new Set(children.map((child) => child.serviceName)).size !== 4
  )
    return undefined;
  return {
    schema: "history-daemon-discovery-v1",
    scope: {
      runId: s.runId,
      daemonPid: s.daemonPid,
      codeStamp: s.codeStamp,
      serviceSpecsDigest: s.serviceSpecsDigest,
      incarnation: s.incarnation,
    },
    socketPath: value.socketPath,
    children,
  };
};

/** Discovery only. A cohort change revokes the old server before any async work. */
export const historyDaemonDiscovery = (
  paths: SupervisorPaths,
  registry: Registry,
  record: (event: Record<string, unknown>) => void,
) => {
  const binding = paths.historyDaemon;
  if (binding === undefined)
    return { changed: () => undefined, close: async () => undefined };
  const started = recoveryScope(paths);
  if (started === undefined || lockOwner(binding.supervisorPid) !== process.pid)
    throw new Error("History query requires the current supervisor lease");
  let incarnation = randomUUID();
  let children = registry.cohort();
  let socketPath: string | null = null;
  let descriptorBytes: string | undefined;
  let descriptorIdentity: string | undefined;
  let closed = false;
  let closing: Promise<void> | undefined;
  let dirty = false;
  let refreshing: Promise<void> | undefined;
  let server: { close(): Promise<void> } | undefined;
  let directory: string | undefined;
  const scope = (): HistoryDaemonScope | undefined =>
    !closed &&
    recoveryScopeMatches(started, recoveryScope(paths)) &&
    lockOwner(binding.supervisorPid) === process.pid
      ? {
          runId: binding.runId,
          daemonPid: process.pid,
          ...started,
          incarnation,
        }
      : undefined;
  const publish = () => {
    const current = scope();
    if (current === undefined) return;
    const descriptor: Descriptor = {
      schema: "history-daemon-discovery-v1",
      scope: current,
      socketPath,
      children,
    };
    descriptorBytes = `${JSON.stringify(descriptor, null, 2)}\n`;
    writeDurableJson(binding.descriptorPath, descriptor);
    descriptorIdentity = historyPublicFile(binding.descriptorPath).identity;
  };
  const stopServer = async () => {
    const previous = server;
    server = undefined;
    await previous?.close();
    if (directory !== undefined) {
      rmdirSync(directory);
      directory = undefined;
    }
  };
  const refresh = async () => {
    while (dirty && !closed) {
      dirty = false;
      await stopServer();
      const current = scope();
      const captured = children;
      if (current === undefined || captured.length !== 4) continue;
      directory = mkdtempSync("/tmp/midgard-history-query-");
      const target = join(directory, "query.sock");
      let proof: Awaited<ReturnType<Registry["prove"]>>;
      const live = () => {
        const now = scope();
        return now !== undefined &&
          sameScope(current, now) &&
          tuple(captured) === tuple(registry.cohort())
          ? now
          : undefined;
      };
      const candidate = await startHistoryDaemonQuery({
        socketPath: target,
        scope: () => {
          const now = live();
          if (now === undefined) return undefined;
          if (proof !== undefined) {
            const checking = proof;
            proof = undefined;
            if (!checking.current()) return undefined;
          }
          return now;
        },
        check: async (name, remainingMs, signal) => {
          proof = undefined;
          const service = paths.serviceSpecs?.find(
            (item) =>
              item.historyReadiness?.role === name &&
              captured.some(
                (child) =>
                  child.role === name && child.serviceName === item.name,
              ),
          );
          if (service === undefined || live() === undefined) return false;
          proof = await registry.prove(service, remainingMs, signal);
          return proof !== undefined;
        },
      });
      server = candidate;
      if (live() === undefined || closed) {
        await stopServer();
        continue;
      }
      socketPath = target;
      publish();
    }
  };
  const changed = () => {
    const next = registry.cohort();
    if (tuple(next) === tuple(children) || closed) return;
    children = next;
    incarnation = randomUUID();
    socketPath = null;
    publish();
    dirty = true;
    pump();
  };
  const pump = () => {
    if (refreshing !== undefined || closed) return;
    refreshing = refresh()
      .catch((error: unknown) => {
        record({
          event: "history-query-held",
          reason:
            error instanceof Error ? error.message.slice(0, 300) : "unknown",
        });
      })
      .finally(() => {
        refreshing = undefined;
        if (dirty && !closed) pump();
      });
  };
  publish();
  return {
    changed,
    close: () => {
      if (closing !== undefined) return closing;
      closing = (async () => {
        closed = true;
        dirty = false;
        await refreshing;
        await stopServer();
        if (
          descriptorBytes !== undefined &&
          existsSync(binding.descriptorPath)
        ) {
          try {
            const file = historyPublicFile(binding.descriptorPath);
            if (
              file.identity === descriptorIdentity &&
              file.text === descriptorBytes
            )
              unlinkSync(binding.descriptorPath);
          } catch (error) {
            if (
              !(
                error instanceof Error &&
                "code" in error &&
                error.code === "ENOENT"
              )
            )
              throw error;
          }
        }
      })();
      return closing;
    },
  };
};

/** One command/poll owns one complete query. The result is never persisted. */
export const historyDaemonReports = (
  layout: Layout,
  services: readonly ServiceSpec[],
  deadline: number | null,
) => {
  const configured = services.filter(
    (service) => service.historyReadiness !== undefined,
  );
  let refused =
    deadline === null ||
    configured.length !== 4 ||
    new Set(configured.map((service) => service.name)).size !== 4 ||
    HISTORY_CHILD_ROLES.some(
      (name) =>
        !configured.some((service) => service.historyReadiness?.role === name),
    );
  let descriptor: Descriptor | undefined;
  let initial: string | undefined;
  let used = false;
  const key = () => {
    if (deadline === null || historyProofRemaining(deadline) === 0)
      throw new Error("History query deadline elapsed");
    const run = readRunEnv(layout);
    const code = codeStamp(runtimeDistTargets(layout));
    const digest = specsDigest(services, code);
    const file = historyPublicFile(layout.historyDaemonDescriptor, deadline);
    if (file.bytes.length > 4096)
      throw new Error("History query descriptor oversized");
    const parsed: unknown = JSON.parse(file.text);
    const current = parseDescriptor(parsed);
    if (
      current === undefined ||
      current.children.some(
        (child) =>
          !configured.some(
            (service) =>
              service.name === child.serviceName &&
              service.historyReadiness?.role === child.role,
          ),
      ) ||
      current.scope.runId !== run.runId ||
      current.scope.daemonPid !== lockOwner(layout.supervisorPid) ||
      current.scope.codeStamp !== code ||
      current.scope.serviceSpecsDigest !== digest ||
      readFileSync(layout.supervisorSpecs, "utf8") !== digest ||
      realpathSync(layout.runDir) !== layout.runDir ||
      realpathSync(join(layout.watcherRoot, "dist/index.js")) !==
        join(layout.watcherRoot, "dist/index.js")
    )
      throw new Error("History query scope is unknown");
    const hash = createHash("sha256");
    const field = (bytes: Buffer) => {
      hash.update(`${bytes.length}:`);
      hash.update(bytes);
    };
    field(
      Buffer.from(
        JSON.stringify([run.runId, run.networkMagic, run.portOffset, current]),
      ),
    );
    for (const path of [
      layout.historyDaemonDescriptor,
      ...historyAdmissionPublicPaths(layout),
      ...current.children.map((child) =>
        join(layout.state, "services", `${child.serviceName}.json`),
      ),
    ]) {
      const input = historyPublicFile(path, deadline);
      field(Buffer.from(path));
      field(Buffer.from(input.identity));
      field(input.bytes);
      if (path.startsWith(join(layout.state, "services") + "/")) {
        const pidRecord: unknown = JSON.parse(input.text);
        const actor = current.children.find(
          (child) =>
            path ===
            join(layout.state, "services", `${child.serviceName}.json`),
        );
        if (
          actor === undefined ||
          !isPlainRecord(pidRecord) ||
          pidRecord.pid !== actor.pid
        )
          throw new Error("History query child record changed");
        process.kill(actor.pid, 0);
        const started = processStartTime(actor.pid);
        if (started === undefined)
          throw new Error("History query child kernel identity unavailable");
        field(Buffer.from(started));
      }
    }
    if (historyProofRemaining(deadline) === 0)
      throw new Error("History query deadline elapsed");
    descriptor ??= current;
    return hash.digest("hex");
  };
  const current = () => {
    if (refused) return false;
    try {
      if (key() !== initial) refused = true;
    } catch {
      refused = true;
    }
    return !refused;
  };
  if (!refused)
    try {
      initial = key();
    } catch {
      refused = true;
    }
  const ready = (async () => {
    if (
      refused ||
      descriptor === undefined ||
      descriptor.socketPath === null ||
      deadline === null
    )
      return false;
    const result = await queryHistoryDaemon({
      socketPath: descriptor.socketPath,
      expectedScope: descriptor.scope,
      scope: () => (current() ? descriptor?.scope : undefined),
      serviceName: "history-recorder",
      timeoutMs: historyProofRemaining(deadline),
    });
    return result === "ready" && current();
  })();
  return {
    ready,
    current,
    map: (
      reports: readonly ServiceReport[],
      proven: boolean,
    ): readonly ServiceReport[] => {
      const unchanged = current();
      const accepted = !used && proven && unchanged;
      used = true;
      return reports.map((report) => {
        if (!configured.some((service) => service.name === report.name))
          return report;
        const child = descriptor?.children.find(
          (actor) => actor.serviceName === report.name,
        );
        const ready =
          accepted &&
          child?.pid === report.pid &&
          report.alive &&
          report.refusal === undefined;
        return {
          ...report,
          ready,
          ...(ready
            ? { reasons: undefined }
            : {
                reasons:
                  report.refusal === undefined
                    ? ["history_proof_unknown"]
                    : report.reasons,
              }),
        };
      });
    },
  };
};
