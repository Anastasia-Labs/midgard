import { Effect, Ref } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import type { NodeConfigDep } from "./config.js";
import { Globals } from "./globals.js";
import { type Decision } from "./history-expired-intent-release.signed-commit-node.js";
import { failure } from "./history-expired-intent-release.table.js";
import type {
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "./mpf-native-owner/index.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.js";

/** With this intent's own replacement plan retained, a sticky deferral would
 * leave the plan retained while the correction path waits for it. The plan
 * binds only the replaced journal, so it is resumed instead. */
export const effective = (
  decision: Decision,
  retainedPlan: boolean,
): Decision =>
  retainedPlan && decision.kind === "defer" && decision.sticky
    ? {
        kind: "replace",
        cause: `${decision.reason}; its retained replacement plan is resumed`,
      }
    : decision;

/** The node's native owner, opening only its retained native bytes when none
 * is open: never a genesis bootstrap or a journal replay. Create validates the
 * durable marker. */
export const openRetainedNativeOwner = (
  globals: Globals,
  config: NodeConfigDep,
) =>
  Effect.gen(function* () {
    const open = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    if (open !== undefined) return open;
    return yield* Effect.uninterruptible(
      Effect.gen(function* () {
        const opened = yield* Effect.tryPromise({
          try: () =>
            ProductionNativeMpfOwnerService.create({
              levelPath: config.LEDGER_MPF_DB_PATH,
              binaryPath: config.MPF_NATIVE_OWNER_BINARY_PATH,
              binarySha256: config.MPF_NATIVE_OWNER_BINARY_SHA256,
              maxFrameBytes: config.MPF_NATIVE_OWNER_MAX_FRAME_BYTES,
              maxChunkBytes: config.MPF_NATIVE_OWNER_MAX_CHUNK_BYTES,
              requestTimeoutMs: config.MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS,
              restartLimit: config.MPF_NATIVE_OWNER_RESTART_LIMIT,
              sidecarPath: config.MPF_NATIVE_OWNER_SIDECAR_PATH,
            }),
          catch: (cause) =>
            failure("Retained native owner could not open", cause),
        });
        yield* Ref.set(globals.NATIVE_MPF_OWNER, opened);
        return opened as NativeMpfOwnerService;
      }),
    );
  });

export const persistedReplay = (
  replay: NonNullable<Pending.Record["nativeMpfReplay"]>,
): PersistedNativeMpfReplay => ({
  schema: 1,
  ownerBinarySha256: replay.ownerBinarySha256.toString("hex"),
  baseRoot: replay.baseRoot.toString("hex"),
  candidateRoot: replay.candidateRoot.toString("hex"),
  eventLog: replay.eventLog,
  eventLogDigest: replay.eventLogDigest.toString("hex"),
  eventRoots: replay.eventRoots,
  eventCount: replay.eventCount,
});
