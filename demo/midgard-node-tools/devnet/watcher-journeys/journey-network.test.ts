import {
  DEPLOYMENT_PROFILE_DIGESTS,
  DEPLOYMENT_PROFILES,
  type DeploymentProfileName,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import {
  assertJourneyNetwork,
  journeyNetwork,
  JourneyNetworkMismatchError,
  journeyNetworkOf,
} from "./journey-network.js";

const profileNames = Object.keys(
  DEPLOYMENT_PROFILES,
) as DeploymentProfileName[];
const journeyProfileNames = profileNames.filter(
  (name) => DEPLOYMENT_PROFILES[name].network !== "Mainnet",
);

/** What a deployment of the compiled profile, on its own node, presents. */
const compiledDeployment = () => {
  const expected = journeyNetwork();
  return {
    manifestNetwork: expected.network,
    manifestProfileDigest: expected.profileDigest,
    handshakeMagic: expected.magic,
  };
};

describe("journey network", () => {
  it("is the compiled deployment profile's network, read once and frozen", () => {
    const network = journeyNetwork();
    expect(network).toBe(journeyNetwork());
    expect(Object.isFrozen(network)).toBe(true);
    expect(network.network).toBe(SELECTED_DEPLOYMENT_PROFILE.network);
    expect(network.profileDigest).toBe(SELECTED_DEPLOYMENT_PROFILE_DIGEST);
  });

  it("gives the devnet and Preprod profiles distinct node magics and refuses Mainnet", () => {
    const networks = journeyProfileNames.map((name) =>
      journeyNetworkOf(
        DEPLOYMENT_PROFILES[name],
        DEPLOYMENT_PROFILE_DIGESTS[name],
      ),
    );
    expect(new Set(networks.map(({ network }) => network))).toEqual(
      new Set(["Custom", "Preprod"]),
    );
    const magicByNetwork = new Map(
      networks.map(({ network, magic }) => [network, magic]),
    );
    expect(magicByNetwork.size).toBe(2);
    expect(new Set(magicByNetwork.values()).size).toBe(2);
    for (const name of profileNames.filter(
      (candidate) => !journeyProfileNames.includes(candidate),
    ))
      expect(() =>
        journeyNetworkOf(
          DEPLOYMENT_PROFILES[name],
          DEPLOYMENT_PROFILE_DIGESTS[name],
        ),
      ).toThrow(JourneyNetworkMismatchError);
  });

  it("opens a deployment of the compiled profile on its own node", () => {
    expect(() =>
      assertJourneyNetwork(journeyNetwork(), compiledDeployment()),
    ).not.toThrow();
  });

  it("refuses a manifest deployed under any other profile", () => {
    const others = profileNames.filter(
      (name) =>
        DEPLOYMENT_PROFILE_DIGESTS[name] !== journeyNetwork().profileDigest,
    );
    expect(others.length).toBeGreaterThan(0);
    for (const name of others)
      expect(() =>
        assertJourneyNetwork(journeyNetwork(), {
          ...compiledDeployment(),
          manifestNetwork: DEPLOYMENT_PROFILES[name].network,
          manifestProfileDigest: DEPLOYMENT_PROFILE_DIGESTS[name],
        }),
      ).toThrow(JourneyNetworkMismatchError);
  });

  it("refuses a manifest whose network is not the profile's", () => {
    const otherNetwork =
      journeyNetwork().network === "Custom" ? "Preprod" : "Custom";
    expect(() =>
      assertJourneyNetwork(journeyNetwork(), {
        ...compiledDeployment(),
        manifestNetwork: otherNetwork,
      }),
    ).toThrow(/manifest network/u);
  });

  it("refuses a node that handshakes with another network's magic", () => {
    const otherMagic = journeyProfileNames
      .map((name) =>
        journeyNetworkOf(
          DEPLOYMENT_PROFILES[name],
          DEPLOYMENT_PROFILE_DIGESTS[name],
        ),
      )
      .find(({ network }) => network !== journeyNetwork().network)!.magic;
    expect(() =>
      assertJourneyNetwork(journeyNetwork(), {
        ...compiledDeployment(),
        handshakeMagic: otherMagic,
      }),
    ).toThrow(/node magic/u);
  });
});
