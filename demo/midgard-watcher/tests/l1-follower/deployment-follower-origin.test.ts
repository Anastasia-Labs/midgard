/**
 * The watcher follower's origin against the deployment's recorded origin T
 * (the manifest's `l1Origin`): the follower starts at `l1.origin` when set,
 * else at T, and a store whose origin is after T is unready R3
 * (`origin_after_protocol_init`) while the follower keeps running.
 */
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it } from "vitest";

import { openWatcherDeploymentFollower } from "../../src/l1-follower/deployment-follower.js";
import {
  verifyWatcherDeploymentIdentity,
  watcherDeploymentProtocolScriptAuthority,
} from "../../src/runtime/deployment-identity.js";
import {
  canonicalIdentity,
  withManifestId,
} from "../runtime/deployment-identity.canonical-identity.js";
import { makeFixture } from "../runtime/deployment-identity.make-fixture.js";
import { SIM_HUB_ORACLE_ONE_SHOT } from "../support/l1-follower-state-queue-traffic.js";
import {
  AUTHORITY,
  chainEvents,
  RECOVERY_DEPTH,
  scriptedTransport,
  until,
} from "../support/l1-follower-store-reset.js";

const T: L1Origin = { slot: 4_200, blockHash: "ab".repeat(32) };

/** The simulated chain's origin, in the manifest's form. */
const SIM: L1Origin = {
  slot: SIM_ORIGIN.point.slot,
  blockHash: SIM_ORIGIN.point.hash.toString("hex"),
};

describe("the deployment's recorded origin in the watcher's verified identity", () => {
  it("carries a manifest's l1Origin into the deployment follower's authority, and nothing for a manifest without one", () => {
    const authorityOf = (manifest: Record<string, unknown>) => {
      const fixture = makeFixture();
      fixture.signedIdentity.manifest = manifest;
      fixture.resign();
      return watcherDeploymentProtocolScriptAuthority(
        verifyWatcherDeploymentIdentity({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: makeDeploymentMarker(String(manifest.manifestId)),
        }),
      );
    };
    expect(
      authorityOf(withManifestId({ ...canonicalIdentity(), l1Origin: T }))
        .recordedL1Origin,
    ).toEqual(T);
    expect(
      Object.keys(authorityOf(withManifestId(canonicalIdentity()))),
    ).not.toContain("recordedL1Origin");
  });
});

describe("where the watcher's deployment follower starts", () => {
  const opened: (() => Promise<void>)[] = [];
  afterEach(async () => {
    await Promise.all(opened.splice(0).map((close) => close()));
  });

  const open = (
    origin: L1Origin | undefined,
    recordedL1Origin: L1Origin | undefined,
  ) => {
    const follower = openWatcherDeploymentFollower({
      authority: {
        network: "Preprod",
        protocolScriptHashes: AUTHORITY.protocolScriptHashes,
        hubOracleOneShotOutRef: `${SIM_HUB_ORACLE_ONE_SHOT.txHash.toString("hex")}#${SIM_HUB_ORACLE_ONE_SHOT.index.toString()}`,
        ...(recordedL1Origin === undefined ? {} : { recordedL1Origin }),
      },
      origin,
      followedScripts: [],
      storePath: ":memory:",
      automaticRecoveryMaxDepth: RECOVERY_DEPTH,
      node: {
        binaryPath: "unused",
        socketPath: "unused",
        networkMagic: 42,
        requestTimeoutMs: 1_000,
      },
      walletAddresses: [],
      unsafeTransportForTest: scriptedTransport(chainEvents(3), { acked: 0 }),
    });
    opened.push(() => follower.close());
    return follower;
  };
  const reasons = async (follower: ReturnType<typeof open>) =>
    (await follower.readiness()).map(({ reason }) => reason);
  const atTip = (follower: ReturnType<typeof open>) =>
    until(
      "the follower to reach the tip",
      () => follower.status()?.atTip === true,
    );

  it("C-a: with no l1.origin, follows from the manifest's recorded origin, which precedes the protocol init", async () => {
    const follower = open(undefined, SIM);
    await atTip(follower);
    expect((await follower.store.cursor())?.origin).toEqual(SIM_ORIGIN.point);
    expect(follower.status()?.protocolInit).toBe("seen");
    expect(await reasons(follower)).not.toContain("l1_origin_not_configured");
  });

  it("C-b: an l1.origin after the recorded origin is R3, though the protocol-init spend is seen, and the follower keeps running", async () => {
    const follower = open(SIM, {
      slot: SIM.slot - 1,
      blockHash: "9f".repeat(32),
    });
    await atTip(follower);
    expect((await follower.store.cursor())?.origin).toEqual(SIM_ORIGIN.point);
    expect(await reasons(follower)).toContain("origin_after_protocol_init");
    expect(follower.status()?.state).not.toBe("stopped");
  });

  it("C-b: neither l1.origin nor a recorded origin leaves the follower unconfigured", async () => {
    expect(await reasons(open(undefined, undefined))).toEqual([
      "l1_origin_not_configured",
    ]);
  });
});
