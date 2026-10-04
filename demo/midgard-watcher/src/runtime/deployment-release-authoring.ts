import { createHash, createPublicKey, type KeyObject, sign } from "node:crypto";
import { readFile, rename } from "node:fs/promises";
import { dirname, join } from "node:path";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  makeDeploymentMarker,
  parseDeploymentManifestEconomics,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { computeFraudProofReleaseEconomicsPolicyDigest } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import {
  createWatcherWorkflowFundingProfileBundle,
  loadWatcherWorkflowFundingProfileOverlay,
  type WatcherWorkflowFundingProfileBody,
} from "../funding/workflow-funding-profile-overlay.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../verification/rule-bundle.js";
import { loadWatcherVerifiedDeploymentAuthority } from "./deployment-authority.js";
import {
  makeWatcherDeploymentIdentitySignaturePayload,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicy,
} from "./deployment-identity.js";

/** Where a release's artifacts live; the watcher process config names them. */
export type WatcherDeploymentReleasePaths = Readonly<{
  authority: string;
  rules: string;
  funding: string;
  manifest: string;
  blueprint: string;
  /** Contract deployment info: the finalized manifest, as the node writes it. */
  deploymentInfo: string;
}>;

/** Durable file writes; `create` fails with EEXIST instead of replacing. */
export type WatcherDeploymentReleaseWriter = Readonly<{
  replace: (path: string, contents: string | Uint8Array) => Promise<void>;
  create: (path: string, contents: string | Uint8Array) => Promise<void>;
}>;

/**
 * What to do with an authority already saved at `paths.authority` under a
 * trust root other than the signing key's: `refuse` fails, `replace` sets it
 * aside beside itself and signs a new one. An authority under the signing
 * key's own root is reused when it attests exactly this release, and refused
 * otherwise, under either policy.
 */
export type WatcherExistingAuthorityPolicy = "refuse" | "replace";

type SavedAuthority = Readonly<{
  signedIdentity: unknown;
  policy: WatcherDeploymentIdentityPolicy;
  trustRoots: readonly Readonly<{
    trustRootId: string;
    publicKeySpkiDerHex: string;
  }>[];
}>;

const json = (value: unknown) => `${JSON.stringify(value, null, 2)}\n`;

const readSavedAuthority = async (
  path: string,
): Promise<SavedAuthority | undefined> => {
  try {
    return JSON.parse(await readFile(path, "utf8")) as SavedAuthority;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ENOENT") return undefined;
    throw error;
  }
};

/**
 * Signs one watcher release for a finalized deployment manifest: the rule
 * bundle, the measured funding profiles and the release bindings, under the
 * supplied ed25519 trust-root key. Every bound artifact is written durably
 * before the authority that attests it, and the authority is only ever
 * created, never overwritten. Measurements that do not belong to this
 * deployment or funding wallet are refused, never substituted.
 */
export const authorWatcherDeploymentRelease = async ({
  manifest: manifestValue,
  blueprintJson,
  programCommitments,
  fundingProfiles,
  fundingPaymentKeyHash,
  signingKey,
  paths,
  existingAuthority,
  writer,
}: {
  readonly manifest: unknown;
  readonly blueprintJson: string | Uint8Array;
  readonly programCommitments: Readonly<Record<string, string>>;
  readonly fundingProfiles: readonly WatcherWorkflowFundingProfileBody[];
  readonly fundingPaymentKeyHash: string;
  readonly signingKey: KeyObject;
  readonly paths: WatcherDeploymentReleasePaths;
  readonly existingAuthority: WatcherExistingAuthorityPolicy;
  readonly writer: WatcherDeploymentReleaseWriter;
}) => {
  const manifest = verifyFinalizedDeploymentManifest(manifestValue);
  const blueprintHash = createHash("sha256")
    .update(blueprintJson)
    .digest("hex");
  if (blueprintHash !== manifest.artifacts.blueprintHash)
    throw new Error("Blueprint differs from the deployed release");
  const economics = parseDeploymentManifestEconomics(manifest.economics);
  const economicsPolicyDigest = computeFraudProofReleaseEconomicsPolicyDigest({
    profile: economics.profile,
    requiredBondLovelace: economics.requiredBondLovelace.toString(),
    slashingPenaltyLovelace: economics.slashingPenaltyLovelace.toString(),
    fraudProverRewardLovelace: economics.fraudProverRewardLovelace.toString(),
    inactivitySlashingPenaltyLovelace:
      economics.inactivitySlashingPenaltyLovelace.toString(),
    proverCollateralFloorLovelace:
      economics.proverCollateralFloorLovelace.toString(),
  });
  const publishedScriptHashes = new Set(
    Object.values(manifest.referenceScripts).map(
      (reference) => reference!.scriptHash,
    ),
  );
  for (const profile of fundingProfiles) {
    if (
      profile.blueprintSha256 !== blueprintHash ||
      profile.protocolParametersDigest !==
        manifest.cardanoProtocolParameters.digest ||
      profile.economicsPolicyDigest !== economicsPolicyDigest ||
      profile.fundingPaymentKeyHash !== fundingPaymentKeyHash ||
      profile.actions.some((action) =>
        action.referenceInputs.some(
          (reference) =>
            reference.scriptHash !== null &&
            !publishedScriptHashes.has(reference.scriptHash),
        ),
      )
    )
      throw new Error(
        "Measured funding profiles differ from this deployment or funding wallet",
      );
  }
  const funding = createWatcherWorkflowFundingProfileBundle({
    profiles: fundingProfiles,
  });
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined)
    throw new Error("Deployment manifest has no fraud-proof catalogue");
  const rules = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: manifest.manifestId,
      network: manifest.network,
      blueprintHash,
      programCommitments,
    },
    targetParameterSnapshot: manifest.cardanoProtocolParameters.snapshot,
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(rules);
  const publicKeySpkiDerHex = createPublicKey(signingKey)
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = createHash("sha256")
    .update(Buffer.from(publicKeySpkiDerHex, "hex"))
    .digest("hex");

  const reopen = async () => {
    const deploymentAuthority = await loadWatcherVerifiedDeploymentAuthority({
      path: paths.authority,
      ruleBundlePath: paths.rules,
    });
    const identity = deploymentAuthority.deploymentIdentity;
    if (
      identity.manifestId !== manifest.manifestId ||
      identity.blueprintHash !== blueprintHash ||
      identity.ruleBundleCommitment !== ruleBundleCommitment ||
      identity.fundingProfileBundleDigest !==
        funding.fundingProfileBundleDigest ||
      identity.trustRootId !== trustRootId
    )
      throw new Error(
        "Saved watcher authority differs from the requested deployment or release",
      );
    return {
      authority: (await readSavedAuthority(paths.authority))!,
      deploymentAuthority,
      fundingProfileOverlay: await loadWatcherWorkflowFundingProfileOverlay({
        bundlePath: paths.funding,
        deploymentIdentity: identity,
      }),
    };
  };

  const saved = await readSavedAuthority(paths.authority);
  if (saved !== undefined) {
    const savedRoot = saved.trustRoots[0]?.trustRootId;
    if (savedRoot === trustRootId) return reopen();
    if (existingAuthority === "refuse")
      throw new Error(
        `Saved watcher authority is signed by trust root ${savedRoot ?? "unknown"}, not the configured signing key`,
      );
    await rename(
      paths.authority,
      join(
        dirname(paths.authority),
        `deployment-authority.superseded-${savedRoot ?? "unknown"}.json`,
      ),
    );
  }

  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    ruleBundleCommitment,
    programCommitments,
    fundingProfileBundleDigest: funding.fundingProfileBundleDigest,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(manifest.da),
    },
    artifacts: { blueprintHash },
  };
  const signedIdentity = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId,
      signature: sign(
        null,
        makeWatcherDeploymentIdentitySignaturePayload(
          manifest.manifestId,
          releaseBindings,
        ),
        signingKey,
      ).toString("hex"),
    },
  };
  const policy: WatcherDeploymentIdentityPolicy = {
    network: manifest.network,
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    appliedScriptHashes: Object.fromEntries(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((name) => [
        name,
        manifest.contracts[name]!.scriptHash,
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        (role) => {
          const reference = manifest.referenceScripts[role]!;
          return [
            role,
            { scriptHash: reference.scriptHash, outRef: reference.outRef },
          ];
        },
      ),
    ),
    fraudProofCatalogue: {
      root: catalogue.root,
      categories: Object.fromEntries(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => {
          const entry = catalogue.categories[category]!;
          return [
            category,
            { categoryId: entry.categoryId, scriptHash: entry.scriptHash },
          ];
        }),
      ) as WatcherDeploymentIdentityPolicy["fraudProofCatalogue"]["categories"],
    },
    ruleBundleCommitment,
    programCommitments,
    fundingProfileBundleDigest: funding.fundingProfileBundleDigest,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    blueprintHash,
  };
  await writer.replace(paths.rules, json(rules));
  await writer.replace(paths.manifest, json(manifest));
  // The node's contract-deployment-info.json is the finalized manifest.
  await writer.replace(paths.deploymentInfo, json(manifest));
  await writer.replace(paths.blueprint, blueprintJson);
  // Funding bundles require canonical bytes, including no trailing newline.
  await writer.replace(paths.funding, funding.fundingProfileBundleBytes);
  // Publish the signed authority only after every bound artifact is durable.
  await writer.create(
    paths.authority,
    json({
      signedIdentity,
      policy,
      trustRoots: [{ trustRootId, publicKeySpkiDerHex }],
      durableMarker: makeDeploymentMarker(manifest.manifestId),
    }),
  );
  return reopen();
};
