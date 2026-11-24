import type * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Ref } from "effect";

import { promoteOrRecoverNativeMpf } from "../src/fibers/block-commitment.promote-or-recover-native-mpf.js";
import { publishCommitMempoolLedgerMutation } from "../src/fibers/block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import { NodeConfig } from "../src/services/config.make-config.js";
import {
  FollowerWrite,
  runAtFollowerView,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.globals.js";
import { Lucid } from "../src/services/lucid.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.mempool-ledger-cache-service.js";
import { MidgardContracts } from "../src/services/midgard-contracts.make-midgard-contract-runtime.js";
import type { NotifyCommitWorkerParent } from "../src/workers/commit-block-header.commit-explicit-block-header-program.js";
import { takeCommitWorkerOutput } from "../src/workers/commit-worker-message.js";
import type { WorkerInput as CommitWorkerInput } from "../src/workers/utils/commit-block-header.js";
import type {
  commitWorkerProgram,
  OwnedCommitFixture,
} from "./deposit-flow-emulator-shared.commit-worker-program.js";
import type { makeLucidRuntimeService } from "./deposit-flow-emulator-shared.js";

/**
 * The parent's part for a notice the commit program posts while it runs: a
 * commit-stage revert marks the outrefs it names stale in the validation
 * cache, through the parent's dispatcher. The other notices feed readiness,
 * which this fixture does not follow.
 */
export const relayLedgerRevert =
  (globals: Globals): NotifyCommitWorkerParent =>
  (message) =>
    Effect.sync(() => {
      if (message.type === "MempoolLedgerRevertedNotice")
        takeCommitWorkerOutput(globals, message);
    });

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
      yield* publishCommitMempoolLedgerMutation(production.globals, output);
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
