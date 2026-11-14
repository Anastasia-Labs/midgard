import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { CommitteeAvailabilityReads } from "../l1/follower/availability-reads.js";
import type { availabilityResponderOperations } from "./factory.availability-responder-operations.js";
import { discoverAvailabilityResponderChallenges } from "./factory.discover-availability-responder-challenges.js";
import type { PromiseCapacityPoint } from "./promise-capacity-evidence.js";
import {
  currentPromiseScheduling,
  promiseCurrentSchedulingDigest,
  type PromiseSchedulingLiability,
} from "./promise-current-scheduling.js";

/** Keeps only one outstanding source certificate. Concurrent replacement
 * invalidates an older permit rather than letting it use another candidate's view.
 * Every canonical point is read from the committee follower under the
 * certificate's boundary; `generation` is the follower view's (plan §8.1). */
export const promiseSchedulingSource = (
  args: Readonly<{
    lucid: LucidEvolution;
    deployment: SDK.DaAvailabilityDeployment;
    openWindowMs: number;
    reads: Pick<CommitteeAvailabilityReads, "canonicalPoint">;
    readBoundary: ReturnType<
      typeof availabilityResponderOperations
    >["readBoundary"];
    assertActuationCurrent: ReturnType<
      typeof availabilityResponderOperations
    >["assertActuationCurrent"];
  }>,
) => {
  let captureGeneration = 0;
  let certificate:
    | Readonly<{
        digest: string;
        liabilities: readonly PromiseSchedulingLiability[];
        point: PromiseCapacityPoint;
        generation: number;
      }>
    | undefined;
  const capture = async (
    input: Readonly<{
      liabilities: readonly PromiseSchedulingLiability[];
      rawSnapshot?: SDK.DaAvailabilitySnapshotUtxos;
      complete: boolean;
      point: PromiseCapacityPoint;
      generation: number;
      scope?: SDK.DaAvailabilityReadScope;
    }>,
  ) => {
    const { scope } = input;
    if (!scope || !input.rawSnapshot)
      throw new Error("Scoped complete scheduling evidence is unavailable");
    const assertCurrent = async () => {
      await args.assertActuationCurrent(scope);
      const boundary = await args.readBoundary(scope);
      if (
        boundary.slot !== input.point.slot ||
        boundary.blockHash !== input.point.blockHash ||
        boundary.blockNo !== input.point.blockNo ||
        boundary.generation !== input.generation
      )
        throw new Error(
          "Scheduling canonical boundary or consumed generation changed",
        );
      scope.assertCurrent();
    };
    const current = await currentPromiseScheduling({
      deployment: args.deployment,
      boundary: input.point,
      canonicalTimeMs: args.lucid.slotToUnixTime(input.point.slot),
      openWindowMs: args.openWindowMs,
      rawSnapshot: input.rawSnapshot,
      complete: input.complete,
      liabilities: input.liabilities,
      readCanonicalPoint: (point) =>
        scope.read(() =>
          args.reads.canonicalPoint(point, {
            ...input.point,
            generation: input.generation,
          }),
        ),
      assertCurrent,
    });
    return {
      current,
      digest: promiseCurrentSchedulingDigest({
        boundary: input.point,
        rollbackGeneration: input.generation,
        current,
        liabilities: input.liabilities,
        rawSnapshot: input.rawSnapshot,
      }),
    };
  };
  return {
    capture: async (input: Parameters<typeof capture>[0]) => {
      const generation = ++captureGeneration;
      certificate = undefined;
      const result = await capture(input);
      if (generation !== captureGeneration)
        throw new Error("Current scheduling capture was superseded");
      certificate = {
        digest: result.digest,
        liabilities: input.liabilities,
        point: input.point,
        generation: input.generation,
      };
      return result;
    },
    assertCurrent: async (
      digest: string | undefined,
      scope?: SDK.DaAvailabilityReadScope,
    ) => {
      const prior = certificate;
      if (!digest || !prior || digest !== prior.digest || !scope)
        throw new Error(
          "Current scheduling certificate is unavailable or superseded",
        );
      let complete = true,
        rawSnapshot: SDK.DaAvailabilitySnapshotUtxos | undefined;
      await discoverAvailabilityResponderChallenges(
        args.lucid,
        args.deployment,
        () => {
          complete = false;
        },
        {
          scope,
          onSnapshot: (snapshot) => {
            rawSnapshot = snapshot;
          },
        },
      );
      const boundary = await args.readBoundary(scope);
      const fresh = await capture({
        liabilities: prior.liabilities,
        rawSnapshot,
        complete,
        point: {
          slot: boundary.slot,
          blockHash: boundary.blockHash,
          blockNo: boundary.blockNo,
        },
        generation: boundary.generation,
        scope,
      });
      if (
        certificate !== prior ||
        prior.generation !== boundary.generation ||
        fresh.digest !== prior.digest
      )
        throw new Error(
          "Current scheduling raw evidence changed before signing",
        );
    },
  };
};
