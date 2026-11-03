import type { OutRef } from "@al-ft/midgard-l1-follower";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { createWatcherReplayRetention } from "../../src/l1-follower/replay-retention.js";
import { createWatcherFollowerReplaySource } from "../../src/l1-follower/replay-source.js";
import {
  initTx,
  SIM_HUB_ORACLE_ONE_SHOT,
} from "./l1-follower-state-queue-traffic.js";
import {
  AUTHORITY,
  D,
  openStore,
  RELEASE_DEPTH,
} from "./l1-follower-store-reset.js";

/** An outref as the deployment manifest spells it: `<tx hash>#<index>`. */
export const outRefLabel = (outRef: OutRef) =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

/** Replay retention whose admission clock starts at `zeroSlot`. */
export const replayRetention = (zeroSlot = 0) =>
  createWatcherReplayRetention({
    slotConfig: { zeroTime: 0, zeroSlot, slotLength: 1000 },
    eventWaitMs: 1n,
    maximumAdmissionRangeMs: 1n,
    maximumProtectionWaitMs: 0n,
  });

/** The follower's replay port over `store`, for its queue reads only. */
export const replaySource = (
  store: ReturnType<typeof openStore>,
  minimumCheckpointDepth = RELEASE_DEPTH,
  retention = replayRetention(),
) =>
  createWatcherFollowerReplaySource({
    store,
    identity: {
      deploymentManifestId: AUTHORITY.deploymentFingerprint,
      blueprintHash: "00".repeat(32),
      network: "Preprod",
    },
    scripts: {
      depositPolicyId: "01".repeat(28),
      withdrawalPolicyId: "02".repeat(28),
      forcedOrderPolicyId: "03".repeat(28),
      forcedOrderAddressHex: "70" + "04".repeat(28),
    },
    userEvents: {
      deploymentManifestId: AUTHORITY.deploymentFingerprint,
      blueprintHash: "00".repeat(32),
      eventAuthority: async () => {
        throw new Error("checkpoint read does not read events");
      },
      headerFence: async () => {
        throw new Error("checkpoint read does not capture events");
      },
      close: () => undefined,
    },
    minimumCheckpointDepth,
    observationAuthority: {
      ...AUTHORITY,
      hubOracleOneShotOutRef: outRefLabel(SIM_HUB_ORACLE_ONE_SHOT),
    },
    retention,
  });

export const GENESIS = SDK.makeGenesisConfirmedState(1000n);

/** The protocol init whose root carries the genesis ConfirmedState. */
export const genesisInitTx = (
  oneShot: OutRef = SIM_HUB_ORACLE_ONE_SHOT,
): SimTx => {
  const initial = initTx(D, undefined, oneShot);
  return {
    ...initial,
    outputs: initial.outputs.map((output, index) =>
      index === 2
        ? {
            ...output,
            datum: Buffer.from(
              Data.to(
                SDK.nodeViewToLinkedListDatum({
                  key: "Empty",
                  next: "Empty",
                  data: Data.from(Data.to(GENESIS, SDK.ConfirmedState)),
                }),
                SDK.LinkedListDatum,
              ),
              "hex",
            ),
          }
        : output,
    ),
  };
};
