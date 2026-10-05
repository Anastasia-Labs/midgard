import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { promiseCanonicalPointReader } from "../l1/promise-capacity-point.js";
import type { ChainSyncCursor } from "../l1/provider.js";
import type { availabilityResponderOperations } from "./factory.availability-responder-operations.js";
import { discoverAvailabilityResponderChallenges } from "./factory.discover-availability-responder-challenges.js";
import type { PromiseCapacityPoint } from "./promise-capacity-evidence.js";
import {
  currentPromiseScheduling,
  promiseCurrentSchedulingDigest,
  type PromiseSchedulingLiability,
} from "./promise-current-scheduling.js";
import {
  committeeScopedWebSocketFactory,
  type CommitteeSourceReadLimits,
} from "./scoped-transports.js";

/** Keeps only one outstanding source certificate. Concurrent replacement
 * invalidates an older permit rather than letting it use another candidate's view. */
export const promiseSchedulingSource = (
  args: Readonly<{
    lucid: LucidEvolution;
    deployment: SDK.DaAvailabilityDeployment;
    ogmiosUrl: string;
    openWindowMs: number;
    limits?: CommitteeSourceReadLimits;
    readUtxos?: (
      address: string,
      scope: SDK.DaAvailabilityReadScope,
    ) => Promise<UTxO[]>;
    currentCursor: (
      scope?: SDK.DaAvailabilityReadScope,
    ) => Promise<ChainSyncCursor>;
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
        sequence: number;
        rollbackGeneration: number;
      }>
    | undefined;
  const capture = async (
    input: Readonly<{
      liabilities: readonly PromiseSchedulingLiability[];
      rawSnapshot?: SDK.DaAvailabilitySnapshotUtxos;
      complete: boolean;
      point: PromiseCapacityPoint;
      cursor: ChainSyncCursor;
      scope?: SDK.DaAvailabilityReadScope;
    }>,
  ) => {
    const { scope } = input;
    if (!scope || !args.limits || !args.readUtxos || !input.rawSnapshot)
      throw new Error("Scoped complete scheduling transport is unavailable");
    const assertCurrent = async () => {
      await args.assertActuationCurrent(scope);
      const boundary = await args.readBoundary(scope);
      const cursor = await scope.read(() => args.currentCursor(scope));
      if (
        boundary.slot !== input.point.slot ||
        boundary.blockHash !== input.point.blockHash ||
        boundary.blockNo !== input.point.blockNo ||
        cursor.sequence !== input.cursor.sequence ||
        cursor.rollbackGeneration !== input.cursor.rollbackGeneration
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
          promiseCanonicalPointReader(
            args.ogmiosUrl,
            committeeScopedWebSocketFactory(scope, args.limits!),
            input.point,
          )(point),
        ),
      assertCurrent,
    });
    return {
      current,
      digest: promiseCurrentSchedulingDigest({
        boundary: input.point,
        rollbackGeneration: input.cursor.rollbackGeneration,
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
        sequence: input.cursor.sequence,
        rollbackGeneration: input.cursor.rollbackGeneration,
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
          readUtxos: args.readUtxos,
          onSnapshot: (snapshot) => {
            rawSnapshot = snapshot;
          },
        },
      );
      const boundary = await args.readBoundary(scope);
      const cursor = await scope.read(() => args.currentCursor(scope));
      const fresh = await capture({
        liabilities: prior.liabilities,
        rawSnapshot,
        complete,
        point: {
          slot: boundary.slot,
          blockHash: boundary.blockHash,
          blockNo: boundary.blockNo,
        },
        cursor,
        scope,
      });
      if (
        certificate !== prior ||
        prior.sequence !== cursor.sequence ||
        prior.rollbackGeneration !== cursor.rollbackGeneration ||
        fresh.digest !== prior.digest
      )
        throw new Error(
          "Current scheduling raw evidence changed before signing",
        );
    },
  };
};
