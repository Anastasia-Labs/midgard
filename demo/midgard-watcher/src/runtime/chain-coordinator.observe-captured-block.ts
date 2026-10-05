import { isWatcherL1BlockAttestedBy } from "../l1/l1-adapter.js";
import {
  guardWatcherLocalKupmiosNativeObservation,
  type WatcherLocalKupmiosNativeObservation,
  type WatcherLocalKupmiosNativeObservationRuntime,
} from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import type { WatcherNativeChainSyncEvent } from "../l1/native-chain-sync.js";
import {
  depthAtTip,
  pointKey,
} from "./chain-coordinator.canonical-path-from-history.js";
import { WatcherCoordinatorIntegrityHeld } from "./chain-coordinator.integrity-hold.js";

export type CapturedWatcherObservation = {
  first: WatcherLocalKupmiosNativeObservation;
  firstDepth: string;
  latest: WatcherLocalKupmiosNativeObservation;
  latestDepth: string;
  persistedDepth: string | null;
};

export const observeCapturedBlock = (input: {
  readonly captured: Map<string, CapturedWatcherObservation>;
  readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
  readonly generation: () => number;
  readonly stopped: () => boolean;
}) => {
  return async (
    block: WatcherNativeBlockAdmission,
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<
    WatcherLocalKupmiosNativeObservation &
      Readonly<{ assertCurrent?: () => void }>
  > => {
    const capturedEpoch = input.generation();
    const assertCurrent = () => {
      if (input.stopped() || input.generation() !== capturedEpoch)
        throw new WatcherCoordinatorIntegrityHeld(
          "native_generation_changed",
          "native observation generation changed before persistence",
        );
    };
    const key = pointKey(block.blockHash, block.slot);
    const depth = depthAtTip(block, event);
    const prior = input.captured.get(key);
    if (prior?.latestDepth === depth)
      return guardWatcherLocalKupmiosNativeObservation({
        observation: prior.latest,
        nativeBlock: block,
        assertCurrent,
      });
    const capturedObservation = await input.observation.observe({
      block,
      depth,
    });
    assertCurrent();
    const assertLive = () => {
      assertCurrent();
      if (
        capturedObservation.transportAttestations.length > 0 &&
        !capturedObservation.observations.every((candidate) =>
          capturedObservation.transportAttestations.some((context) =>
            isWatcherL1BlockAttestedBy(candidate, context),
          ),
        )
      )
        throw new WatcherCoordinatorIntegrityHeld(
          "native_generation_changed",
          "native source attestation expired before persistence",
        );
    };
    const observation = guardWatcherLocalKupmiosNativeObservation({
      observation: capturedObservation,
      nativeBlock: block,
      assertCurrent: assertLive,
    });
    if (prior === undefined) {
      input.captured.set(key, {
        first: observation,
        firstDepth: depth,
        latest: observation,
        latestDepth: depth,
        persistedDepth: null,
      });
    } else {
      prior.latest = observation;
      prior.latestDepth = depth;
    }
    return observation;
  };
};
