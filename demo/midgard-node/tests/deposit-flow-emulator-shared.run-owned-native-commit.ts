import type * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";

import {
  promoteOrRecoverNativeMpf,
  publishCommitMempoolLedgerMutation,
} from "../src/fibers/block-commitment.js";
import {
  FollowerWrite,
  runAtFollowerView,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.js";
import { Lucid, MidgardContracts, NodeConfig } from "../src/services/index.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.js";
import { recoverNativeMpfForLocalFinalization } from "../src/services/native-mpf-local-finalization.js";
import type { WorkerInput as CommitWorkerInput } from "../src/workers/utils/commit-block-header.js";
import type {
  commitWorkerProgram,
  OwnedCommitFixture,
} from "./deposit-flow-emulator-shared.commit-worker-program.js";
import type { makeLucidRuntimeService } from "./deposit-flow-emulator-shared.js";

/** Follow the production parent: build as a producer at the follower
 * driver's applied view (`runAtFollowerView`). */
export const runOwnedNativeCommit = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  production: OwnedCommitFixture,
  input: CommitWorkerInput,
  run: (input: CommitWorkerInput) => ReturnType<typeof commitWorkerProgram>,
) =>
  runAtFollowerView(
    Effect.gen(function* () {
      const permit = yield* FollowerWrite;
      const startedAtMs = Date.now();
      const native = yield* Ref.get(production.globals.NATIVE_MPF_OWNER);
      if (
        native !== undefined &&
        input.data.localFinalizationPending &&
        input.data.availableLocalFinalizationBlock !== ""
      ) {
        yield* recoverNativeMpfForLocalFinalization(
          native,
          input.data.availableLocalFinalizationBlock,
        );
      }
      const nativeMpf =
        native === undefined
          ? undefined
          : {
              port: native.createWorkerPort(),
              durableRoot: (yield* Effect.promise(() => native.diagnostics()))
                .durableRoot,
              ownerBinarySha256:
                production.nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
            };
      const output = yield* run({
        ...input,
        history: permit,
        nativeMpf,
      }).pipe(
        Effect.provideService(MempoolLedgerCache, production.cache),
        Effect.ensuring(Effect.sync(() => nativeMpf?.port.close())),
      );
      if (
        "nativeMpfPromotion" in output &&
        output.nativeMpfPromotion !== undefined
      ) {
        if (native === undefined)
          return yield* Effect.die("Missing native owner for promotion");
        yield* promoteOrRecoverNativeMpf({
          owner: native,
          handle: output.nativeMpfPromotion.handle,
        });
      }
      yield* publishCommitMempoolLedgerMutation(
        production.globals,
        output,
        production.nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
      );
      yield* Effect.sync(() =>
        production.onCommitAttempt?.({
          permit,
          startedAtMs,
          finishedAtMs: Date.now(),
          output,
        }),
      );
      return output;
    }),
  ).pipe(
    Effect.provideService(Globals, production.globals),
    Effect.provideService(Lucid, lucidService as never),
    Effect.provideService(MidgardContracts, contracts as never),
    Effect.provideService(NodeConfig, production.nodeConfig),
  );
