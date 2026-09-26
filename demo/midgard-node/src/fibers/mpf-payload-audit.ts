import { Effect, Schedule } from "effect";

import { type MpfAuditResult, runMpfAudit } from "../commands/mpf-audit.js";
import { MpfEngineStateDB } from "../database/index.js";
import {
  Database,
  Globals,
  NodeConfig,
  type NodeConfigDep,
} from "../services/index.js";

export const shouldRunMpfPayloadAudit = (
  payloadRootCheck: NodeConfigDep["MPF_PAYLOAD_ROOT_CHECK"],
): boolean => payloadRootCheck !== "off";

/**
 * The running node's ledger audit: under Architecture G the persisted root is
 * the native owner's durable root, read lazily so the audit reads it under its
 * leases, after the no-active-submission check, at the same ledger point it
 * recomputes.
 */
export const runLedgerPayloadAudit: Effect.Effect<
  MpfAuditResult,
  unknown,
  Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  return yield* runMpfAudit({
    readNativeDurableRoot: Effect.gen(function* () {
      const owner = yield* globals.NATIVE_MPF_OWNER;
      if (owner === undefined) {
        return yield* Effect.fail(
          new Error(
            "Architecture G native MPF owner is not open; the ledger audit cannot read its durable root",
          ),
        );
      }
      return (yield* Effect.tryPromise(() => owner.diagnostics())).durableRoot;
    }),
  });
});

export const mpfPayloadAuditFiber: Effect.Effect<
  number,
  never,
  Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  if (!shouldRunMpfPayloadAudit(config.MPF_PAYLOAD_ROOT_CHECK)) return 0;
  const runWhenDue = MpfEngineStateDB.ledgerAuditIsDue({
    intervalBlocks: config.MPF_PAYLOAD_AUDIT_INTERVAL_BLOCKS,
    intervalMs: config.MPF_PAYLOAD_AUDIT_INTERVAL_MS,
  }).pipe(
    Effect.flatMap((due) => (due ? runLedgerPayloadAudit : Effect.void)),
    Effect.catchAllCause((cause) =>
      Effect.logError(`MPF payload audit failed: ${String(cause)}`),
    ),
  );
  return yield* runWhenDue.pipe(
    Effect.repeat(
      Schedule.spaced(
        `${Math.min(
          config.MPF_PAYLOAD_AUDIT_INTERVAL_MS,
          config.WAIT_BETWEEN_BLOCK_COMMITMENT,
        )} millis`,
      ),
    ),
  );
});
