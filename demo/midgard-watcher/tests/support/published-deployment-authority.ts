import {
  createHash,
  createPrivateKey,
  createPublicKey,
  generateKeyPairSync,
  type KeyObject,
} from "node:crypto";
import { existsSync } from "node:fs";
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "midgard-node/files/atomic-write";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";

import type { WatcherWorkflowFundingProfileBody } from "../../src/funding/workflow-funding-profile-overlay.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import { authorWatcherDeploymentRelease } from "../../src/runtime/deployment-release-authoring.js";

type PublishedDeployment = Awaited<
  ReturnType<typeof publishWorkflowDeploymentOnChain>
>;

export type WatcherTestTrustRootKey = Readonly<{
  privateKey: KeyObject;
  publicKeySpkiDerHex: string;
  trustRootId: string;
}>;

const trustRootKeyOf = (privateKey: KeyObject): WatcherTestTrustRootKey => {
  const publicKeySpkiDerHex = createPublicKey(privateKey)
    .export({ format: "der", type: "spki" })
    .toString("hex");
  return {
    privateKey,
    publicKeySpkiDerHex,
    trustRootId: createHash("sha256")
      .update(Buffer.from(publicKeySpkiDerHex, "hex"))
      .digest("hex"),
  };
};

/**
 * Loads the ed25519 trust-root signing key saved at `path`, creating it on
 * first use. Journeys that share one watcher runtime must attest their
 * deployment under one trust root: the watcher's user-event policy binds the
 * attesting trust root id, so a re-keyed authority makes the runtime refuse
 * its saved history as a changed dependency.
 */
export const loadOrCreateWatcherTestTrustRootKey = async (
  path: string,
): Promise<WatcherTestTrustRootKey> => {
  if (existsSync(path)) {
    const privateKey = createPrivateKey(await readFile(path, "utf8"));
    if (privateKey.asymmetricKeyType !== "ed25519")
      throw new Error(`Saved trust-root key ${path} is not an ed25519 key`);
    return trustRootKeyOf(privateKey);
  }
  const { privateKey } = generateKeyPairSync("ed25519");
  await writeFile(path, privateKey.export({ format: "pem", type: "pkcs8" }), {
    mode: 0o600,
    flag: "wx",
  });
  return trustRootKeyOf(privateKey);
};

/**
 * Signs the exact published manifest once through the production release
 * authoring, under a saved trust-root key. The key defaults to one kept in
 * `directory`, so reopening a directory reuses its authority; with
 * `trustRootKeyPath` every directory signed against that path publishes the
 * same trust root, and a saved authority under another root is set aside and
 * re-signed.
 */
export const createPublishedWatcherDeploymentAuthority = async ({
  deployment,
  programCommitments,
  fundingProfiles,
  directory,
  trustRootKeyPath = join(directory, "deployment-trust-root.pem"),
}: {
  readonly deployment: PublishedDeployment;
  readonly programCommitments: Readonly<Record<string, string>>;
  readonly fundingProfiles: readonly WatcherWorkflowFundingProfileBody[];
  readonly directory: string;
  readonly trustRootKeyPath?: string;
}) => {
  const manifest = deployment.manifest;
  if (manifest.network !== "Preprod" && manifest.network !== "Custom") {
    throw new Error(
      "Published watcher scenario requires a Preprod or Custom deployment",
    );
  }
  const paths = {
    authority: join(directory, "deployment-authority.json"),
    rules: join(directory, "rules.json"),
    manifest: join(directory, "deployment-manifest.json"),
    blueprint: join(directory, "plutus.json"),
    deploymentInfo: join(directory, "contract-deployment-info.json"),
    funding: join(directory, "funding-profiles.json"),
  };
  const { authority, deploymentAuthority, fundingProfileOverlay } =
    await authorWatcherDeploymentRelease({
      manifest,
      blueprintJson: deployment.blueprintJson,
      programCommitments,
      fundingProfiles,
      fundingPaymentKeyHash: paymentCredentialOf(
        await deployment.publisherLucid.wallet().address(),
      ).hash,
      signingKey: (await loadOrCreateWatcherTestTrustRootKey(trustRootKeyPath))
        .privateKey,
      paths,
      existingAuthority: "replace",
      writer: {
        replace: (path, contents) => writeTextFileAtomic(path, contents),
        create: (path, contents) =>
          writeTextFileAtomicNoReplace(path, contents),
      },
    });
  return {
    nativeDeployment: {
      signedIdentity: authority.signedIdentity,
      policy: authority.policy,
      trustRoots: authority.trustRoots,
      result: deploymentAuthority.deploymentIdentity,
      marker: makeDeploymentMarker(manifest.manifestId),
      contracts: manifest.contracts,
    },
    deploymentAuthority,
    fundingProfileOverlay,
    reopen: () =>
      loadWatcherVerifiedDeploymentAuthority({
        path: paths.authority,
        ruleBundlePath: paths.rules,
      }),
    authorityPath: paths.authority,
    ruleBundlePath: paths.rules,
    manifestPath: paths.manifest,
    blueprintPath: paths.blueprint,
    deploymentInfoPath: paths.deploymentInfo,
    fundingProfileBundlePath: paths.funding,
  };
};
