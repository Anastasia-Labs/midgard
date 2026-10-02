import type { L1Service } from "./chaos-restore.js";

export type Json = Record<string, unknown>;

/** A node HTTP endpoint a targeted kill watches. */
export type TriggerEndpoint = "/pipeline-status" | "/readyz";

/** Holds at the moment a targeted kill should fire. */
export type Trigger = {
  readonly endpoint: TriggerEndpoint;
  readonly description: string;
  readonly holds: (body: Json) => boolean;
};

export type Drill =
  | {
      /** SIGKILL a supervised service's whole process group. */
      readonly kind: "kill";
      readonly name: string;
      readonly service: string;
      /** When present, the kill waits (bounded) for this to hold. */
      readonly trigger?: Trigger;
    }
  | {
      /** `stop` then `start`, or `pause` then `unpause`, after `outageMs`. */
      readonly kind: "outage";
      readonly name: string;
      readonly service: L1Service;
      readonly mode: "stop" | "pause";
      readonly outageMs: number;
    }
  | {
      readonly kind: "restart";
      readonly name: string;
      readonly service: L1Service;
    };

export const section = (body: Json, key: string) => (body[key] ?? {}) as Json;

/**
 * Settlement details the node reports while a journaled settlement
 * transaction is submitted and not yet confirmed (settlement.build-job.ts and
 * settlement.reconcile-attempt.ts).
 */
const SETTLEMENT_IN_FLIGHT =
  /^(submitted exact journaled settlement transaction|waiting for authenticated confirmation depth|waiting for confirmation block identity)$/u;

export const TRIGGERS = {
  admission: {
    endpoint: "/pipeline-status",
    description: "durable admission backlog > 0",
    holds: (body) => {
      const { backlog } = section(body, "durableAdmission");
      return typeof backlog === "string" && /^[1-9]\d*$/u.test(backlog);
    },
  },
  blockSubmitted: {
    endpoint: "/pipeline-status",
    description: "a submitted block awaits L1 confirmation",
    holds: (body) =>
      typeof section(body, "stateQueue").unconfirmedSubmittedBlockTxHash ===
      "string",
  },
  settlement: {
    endpoint: "/readyz",
    description: "a journaled settlement transaction is in flight",
    holds: (body) => {
      const { state, detail } = section(body, "settlement");
      return (
        state === "waiting" &&
        typeof detail === "string" &&
        SETTLEMENT_IN_FLIGHT.test(detail)
      );
    },
  },
} as const satisfies Record<string, Trigger>;

/** The supervised service a `kill-watcher` drill targets, when it exists. */
export const WATCHER_SERVICE = "watcher";

/**
 * Every drill this run supports. Service kills are offered only for services
 * the supervisor actually runs (`serviceSpecs` names).
 */
export const drillCatalogue = (
  serviceNames: readonly string[],
  options: { outageMs?: number; pauseMs?: number } = {},
): Drill[] => {
  const outageMs = options.outageMs ?? 60_000;
  const pauseMs = options.pauseMs ?? 30_000;
  const has = (name: string) => serviceNames.includes(name);
  const drills: Drill[] = [];
  if (has("node"))
    drills.push(
      { kind: "kill", name: "kill-node", service: "node" },
      // kill-node-on-admission, -block-submitted, -settlement.
      ...Object.entries(TRIGGERS).map(
        ([moment, trigger]): Drill => ({
          kind: "kill",
          name: `kill-node-on-${moment.replace(/[A-Z]/gu, (c) => `-${c.toLowerCase()}`)}`,
          service: "node",
          trigger,
        }),
      ),
    );
  for (const name of serviceNames) {
    const member = /^da-committee-(\d+)$/u.exec(name);
    if (member !== null)
      drills.push({
        kind: "kill",
        name: `kill-da-member-${member[1]}`,
        service: name,
      });
  }
  if (has("public-retained-da"))
    drills.push({
      kind: "kill",
      name: "kill-public-retained-da",
      service: "public-retained-da",
    });
  if (has(WATCHER_SERVICE))
    drills.push({
      kind: "kill",
      name: "kill-watcher",
      service: WATCHER_SERVICE,
    });
  drills.push(
    {
      kind: "outage",
      name: "stop-kupo",
      service: "kupo",
      mode: "stop",
      outageMs,
    },
    {
      kind: "outage",
      name: "stop-ogmios",
      service: "ogmios",
      mode: "stop",
      outageMs,
    },
    {
      kind: "outage",
      name: "pause-postgres",
      service: "postgres",
      mode: "pause",
      outageMs: pauseMs,
    },
    { kind: "restart", name: "restart-cardano-node", service: "cardano-node" },
  );
  return drills;
};

/** The named drills, in the order given; an unknown name is an error. */
export const selectDrills = (
  catalogue: readonly Drill[],
  names: readonly string[],
) =>
  names.map((name) => {
    const drill = catalogue.find((candidate) => candidate.name === name);
    if (drill === undefined)
      throw new Error(
        `unknown drill ${name}; this run offers ${catalogue.map((d) => d.name).join(", ")}`,
      );
    return drill;
  });
