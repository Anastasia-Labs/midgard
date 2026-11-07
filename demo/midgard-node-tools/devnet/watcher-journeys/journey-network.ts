import {
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";

/**
 * The L1 networks a watcher journey runs on, each with the magic its local
 * node handshakes with: the isolated devnet (`Custom`, generated with magic
 * 424242) and Preprod.
 */
const JOURNEY_NETWORK_MAGIC = { Custom: 424_242, Preprod: 1 } as const;

/** The compiled deployment profile's L1 network, as a journey uses it. */
export type JourneyNetwork = Readonly<{
  network: keyof typeof JOURNEY_NETWORK_MAGIC;
  magic: number;
  profileDigest: string;
}>;

/** A deployment or node a journey is about to drive is not its profile's. */
export class JourneyNetworkMismatchError extends Error {
  override readonly name = "JourneyNetworkMismatchError";
}

/** The journey network of a deployment profile; refuses any other network. */
export const journeyNetworkOf = (
  profile: Readonly<{ network: string }>,
  profileDigest: string,
): JourneyNetwork => {
  if (!Object.hasOwn(JOURNEY_NETWORK_MAGIC, profile.network))
    throw new JourneyNetworkMismatchError(
      `Watcher journeys do not run on the ${profile.network} network`,
    );
  const network = profile.network as JourneyNetwork["network"];
  return Object.freeze({
    network,
    magic: JOURNEY_NETWORK_MAGIC[network],
    profileDigest,
  });
};

let selected: JourneyNetwork | undefined;

/**
 * The one network every journey site uses: the compiled
 * `SELECTED_DEPLOYMENT_PROFILE`'s, read once. A journey runs from a build of
 * its deployment's profile (`deployment:build local-devnet-testing` on the
 * devnet, `deployment:build preprod-rehearsal` on Preprod).
 */
export const journeyNetwork = (): JourneyNetwork =>
  (selected ??= journeyNetworkOf(
    SELECTED_DEPLOYMENT_PROFILE,
    SELECTED_DEPLOYMENT_PROFILE_DIGEST,
  ));

/**
 * The DA signer config of a devnet journey, which signs as the operator and
 * as the committee's cosigner on the journey's network.
 */
export const journeyDaSignerConfig = (
  accounts: Readonly<
    Record<"operator" | "cosigner", Readonly<{ seedPhrase: string }>>
  >,
) => ({
  NETWORK: journeyNetwork().network,
  L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
  DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
});

/**
 * Refuses to open a journey context unless the deployment and node agree with
 * the compiled profile: the manifest's verified network and profile digest,
 * and the magic the journey's node transport handshakes with (the node's own
 * genesis magic).
 */
export const assertJourneyNetwork = (
  expected: JourneyNetwork,
  observed: Readonly<{
    manifestNetwork: string;
    manifestProfileDigest: string;
    handshakeMagic: number;
  }>,
): void => {
  const mismatches = [
    observed.manifestNetwork !== expected.network &&
      `manifest network ${observed.manifestNetwork} is not ${expected.network}`,
    observed.manifestProfileDigest !== expected.profileDigest &&
      `manifest profile digest ${observed.manifestProfileDigest} is not ${expected.profileDigest}`,
    observed.handshakeMagic !== expected.magic &&
      `node magic ${observed.handshakeMagic} is not ${expected.magic}`,
  ].filter((mismatch) => mismatch !== false);
  if (mismatches.length > 0)
    throw new JourneyNetworkMismatchError(
      `Journey deployment differs from the compiled profile: ${mismatches.join("; ")}`,
    );
};
