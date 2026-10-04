import * as SDK from "@al-ft/midgard-sdk";
import { Context, Effect, Option, Ref } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import {
  applyForeignBaseVerificationOutcome,
  beginForeignBaseVerification,
  type ForeignBaseVerificationScope,
} from "../services/foreign-base-verification.js";
import { requestForeignNativeAdoption } from "../services/foreign-native-adoption-request.js";
import { Globals } from "../services/globals.js";
import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import { verifyForeignCommitBase } from "../workers/commit-block-header.verify-foreign-base.js";
import {
  deserializeStateQueueUTxO,
  type SerializedStateQueueUTxO,
  type WorkerOutput,
} from "../workers/utils/commit-block-header.js";

export const prepareForeignBaseForCommitment = ({
  localFinalizationPending,
  availableConfirmedBlock,
  owner,
  globals,
  scope,
}: {
  readonly localFinalizationPending: boolean;
  readonly availableConfirmedBlock: "" | SerializedStateQueueUTxO;
  readonly owner: NativeMpfOwnerService;
  readonly globals: Context.Tag.Service<typeof Globals>;
  readonly scope: ForeignBaseVerificationScope | undefined;
}) =>
  Effect.gen(function* () {
    let adoptionRequested = false;
    if (!localFinalizationPending && availableConfirmedBlock !== "") {
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );
      if (latestBlock.datum.key === "Empty") return undefined;
      const header = yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum);
      const headerHash = yield* SDK.hashBlockHeader(header);
      const local = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(headerHash, "hex"),
      );
      if (Option.isNone(local)) {
        const verification = yield* verifyForeignCommitBase(latestBlock).pipe(
          Effect.map((base) => ({ _tag: "Verified" as const, base })),
          Effect.catchTag("ForeignBlockVerificationError", (error) =>
            Effect.succeed({ _tag: "Unavailable" as const, error }),
          ),
        );
        const outcome =
          verification._tag === "Unavailable"
            ? {
                status:
                  verification.error.reason === "invalid"
                    ? ("refused" as const)
                    : ("missing" as const),
                foreignHeaderHash: verification.error.foreignHeaderHash,
                reason: verification.error.detail,
              }
            : undefined;
        if (verification._tag === "Verified") {
          adoptionRequested = yield* requestForeignNativeAdoption({
            base: verification.base,
            owner: owner,
          });
        }
        if (outcome !== undefined || adoptionRequested) {
          const held = outcome ?? {
            status: "missing" as const,
            foreignHeaderHash: headerHash,
            reason:
              "Verified foreign ledger adoption is pending source-owner recovery",
          };
          if (scope !== undefined) {
            const currentScope = scope;
            yield* Ref.update(globals.FOREIGN_BASE_VERIFICATION, (current) =>
              applyForeignBaseVerificationOutcome(current, currentScope, held),
            );
          }
          return {
            adoptionRequested,
            output: {
              type: "AwaitingForeignDaOutput",
              foreignHeaderHash: held.foreignHeaderHash,
              reason: held.reason,
              foreignBaseVerification: held,
            } satisfies WorkerOutput,
          };
        }
      }
    }
    return undefined;
  });

export const beginCommitForeignVerification = (
  globals: Context.Tag.Service<typeof Globals>,
  scope: ForeignBaseVerificationScope,
) =>
  Ref.update(globals.FOREIGN_BASE_VERIFICATION, (current) =>
    beginForeignBaseVerification(current, scope),
  ).pipe(Effect.as(scope));

export const applyCommitForeignVerification = (
  globals: Context.Tag.Service<typeof Globals>,
  scope: ForeignBaseVerificationScope | undefined,
  output: WorkerOutput,
) =>
  scope === undefined || output.foreignBaseVerification === undefined
    ? Effect.void
    : Ref.update(globals.FOREIGN_BASE_VERIFICATION, (current) =>
        applyForeignBaseVerificationOutcome(
          current,
          scope,
          output.foreignBaseVerification!,
        ),
      );

export const notifyForeignNativeAdoptionRequested = (requested: boolean) =>
  !requested
    ? Effect.void
    : Effect.gen(function* () {
        const globals = yield* Globals;
        const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
        if (owner === undefined)
          return yield* Effect.fail(
            new Error("Foreign adoption requires the active history owner"),
          );
        // This runs after the producer has left, before the owner drains its lifetime.
        yield* owner.requestReconciliation(
          "Verified foreign ledger adoption requested",
        );
      });
