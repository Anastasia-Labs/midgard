import { randomUUID } from "node:crypto";

import { assertAcceptanceActive } from "./acceptance-process.js";
import {
  type ChaosDeps,
  type Drill,
  type DrillRecord,
  type Json,
  runDrills,
  type RunDrillsOptions,
  selectDrills,
} from "./chaos.js";
import type { DeployContext } from "./deploy.js";
import type { TestAsset } from "./funding.js";
import type { UserRole } from "./identities.js";
import { Journey, type JourneyOptions } from "./journey.js";
import type { HubOracleOneShot } from "./node-env.js";

export const ACCEPTANCE_PHASE_DRILLS = {
  "transfers-concurrent": "kill-node-on-admission",
  "transfers-sequential": "kill-node-on-block-submitted",
  "withdrawals-concurrent": "kill-node-on-settlement",
} as const;

export type AcceptanceObservation = {
  readonly nonce: string;
  readonly phase: string;
  readonly drill: string;
  readonly kind: "armed" | "injection";
  readonly at: string;
  readonly body?: Json;
  readonly pid?: number;
};

export const requireInjectedDrills = (
  expected: readonly Drill[],
  records: readonly DrillRecord[],
) => {
  if (records.length !== expected.length)
    throw new Error(
      `acceptance expected ${expected.length} drills, got ${records.length}`,
    );
  for (const [index, drill] of expected.entries()) {
    const record = records[index]!;
    if (
      record.drill !== drill.name ||
      record.target !== drill.service ||
      !record.ok ||
      record.skipped === true ||
      record.injectedAt === null ||
      record.recoveredAt === null
    )
      throw new Error(
        `acceptance drill ${drill.name} incomplete: ${record.detail}`,
      );
  }
};

export type AcceptanceGateOptions = {
  readonly drills: readonly Drill[];
  readonly chaos: Omit<
    RunDrillsOptions,
    "drills" | "signal" | "shouldStop" | "rounds"
  >;
  readonly abort: AbortController;
  readonly drain: () => Promise<void>;
  readonly observe: (receipt: AcceptanceObservation) => void;
};

/** Only this opt-in runner gates the existing supported scenario's phases. */
export class AcceptanceJourney extends Journey {
  readonly drillRecords: DrillRecord[] = [];

  constructor(
    context: DeployContext,
    oneShot: HubOracleOneShot,
    assets: readonly TestAsset[],
    readonly gate: AcceptanceGateOptions,
    options: JourneyOptions = {},
  ) {
    super(context, oneShot, assets, options);
  }

  protected override async attempt(
    user: UserRole | undefined,
    args: readonly string[],
    label: string,
    submissionId?: string,
  ) {
    assertAcceptanceActive(this.gate.abort.signal);
    try {
      return await super.attempt(user, args, label, submissionId);
    } finally {
      assertAcceptanceActive(this.gate.abort.signal);
    }
  }

  override async until<T>(
    what: string,
    timeoutMs: number,
    read: () => Promise<T | undefined>,
  ): Promise<T> {
    assertAcceptanceActive(this.gate.abort.signal);
    return super.until(what, timeoutMs, async () => {
      assertAcceptanceActive(this.gate.abort.signal);
      try {
        return await read();
      } finally {
        assertAcceptanceActive(this.gate.abort.signal);
      }
    });
  }

  override async phase(name: string, run: () => Promise<void>) {
    assertAcceptanceActive(this.gate.abort.signal);
    const guardedRun = async () => {
      await run();
      // The superclass publishes phase:done only after this callback resolves.
      assertAcceptanceActive(this.gate.abort.signal);
    };
    const drillName =
      ACCEPTANCE_PHASE_DRILLS[name as keyof typeof ACCEPTANCE_PHASE_DRILLS];
    if (drillName === undefined || this.journal.get(`phase:${name}`) === "done")
      return super.phase(name, guardedRun);
    const [drill] = selectDrills(this.gate.drills, [drillName]);
    if (drill?.kind !== "kill" || drill.trigger === undefined)
      throw new Error(`acceptance phase ${name} has no targeted drill`);
    const trigger = drill.trigger;
    const { abort, chaos, observe } = this.gate;
    const nonce = randomUUID();
    let armed = false;
    let release!: () => void;
    const barrier = new Promise<void>((resolve) => {
      release = resolve;
    });
    let triggerBody: Json | undefined;
    let observationError: unknown;
    const deps: ChaosDeps = {
      ...chaos.deps,
      fetchNode: (path) => {
        const original = chaos.deps.fetchNode(path);
        // Observe without replacing, delaying, or changing the returned promise.
        void original.then(
          (body) => {
            triggerBody = body;
          },
          () => {},
        );
        if (!armed) {
          armed = true;
          try {
            observe({
              nonce,
              phase: name,
              drill: drillName,
              kind: "armed",
              at: new Date().toISOString(),
            });
            release();
          } catch (error) {
            observationError = error;
            abort.abort();
          }
        }
        return original;
      },
      signalGroup: (pid, signal) => {
        assertAcceptanceActive(abort.signal);
        if (triggerBody === undefined || !trigger.holds(triggerBody))
          throw new Error(
            `acceptance ${drillName} lacks its actual trigger body`,
          );
        observe({
          nonce,
          phase: name,
          drill: drillName,
          kind: "injection",
          at: new Date().toISOString(),
          body: triggerBody,
          pid,
        });
        chaos.deps.signalGroup(pid, signal);
      },
    };
    const drillTask = runDrills({
      ...chaos,
      deps,
      drills: [drill],
      rounds: 1,
      signal: abort.signal,
    })
      .then((summary) => {
        if (observationError !== undefined)
          throw new Error("acceptance armed observation failed", {
            cause: observationError,
          });
        requireInjectedDrills([drill], summary.records);
        this.drillRecords.push(...summary.records);
      })
      .catch((error: unknown) => {
        abort.abort();
        throw error;
      });
    const phaseTask = (async () => {
      // A finished task without a first trigger read never releases a workload.
      await Promise.race([
        barrier,
        drillTask.then(() => {
          throw new Error(`acceptance ${drillName} ended before arming`);
        }),
      ]);
      assertAcceptanceActive(abort.signal);
      await super.phase(name, guardedRun);
    })().catch((error: unknown) => {
      abort.abort();
      throw error;
    });
    const outcomes = await Promise.allSettled([phaseTask, drillTask]);
    await this.gate.drain();
    const failures = outcomes.filter(
      (outcome) => outcome.status === "rejected",
    );
    if (failures.length > 0)
      throw new AggregateError(
        failures.map((failure) => failure.reason),
        failures
          .map((failure) =>
            failure.reason instanceof Error
              ? failure.reason.message
              : String(failure.reason),
          )
          .join("; "),
      );
    assertAcceptanceActive(abort.signal);
  }
}
