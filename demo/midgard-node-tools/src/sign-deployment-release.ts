import { mkdir, readdir, readFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";

import { verifySignedDeploymentRelease } from "@al-ft/midgard-core/deployment-release";
import { writeTextFileAtomicNoReplace } from "midgard-node/files/atomic-write";
import { authorWatcherDeploymentRelease } from "midgard-watcher";

import { readReleaseInput } from "./full-stack/release.js";

/** File names in the output directory; the operator mounts each where its reader expects it. */
export const SIGNED_RELEASE_FILES = {
  authority: "deployment-authority.json",
  trustRoots: "release-trust-roots.json",
  rules: "rule-bundle.json",
  funding: "funding-profiles.json",
  manifest: "deployment-manifest.json",
  deploymentInfo: "contract-deployment-info.json",
  blueprint: "plutus.json",
  provenance: "release-provenance.json",
  parameterRegistry: "deployment-parameters.json",
  classification: "blueprint-classification.json",
} as const;

const json = (value: unknown) => `${JSON.stringify(value, null, 2)}\n`;

const absolute = (label: string, path: string) => {
  if (!isAbsolute(path)) throw new Error(`${label} must be an absolute path`);
  return path;
};

/** The unused-validator reasons the signed registry carries, as the node's classification file. */
const classificationOf = (registry: unknown) => {
  const validators = (
    registry as {
      deployment: {
        validators: readonly {
          title: string;
          disposition: string;
          reason?: string;
        }[];
      };
    }
  ).deployment.validators;
  return Object.fromEntries(
    validators
      .filter((validator) => validator.disposition === "not_deployed")
      .map((validator) => [validator.title, validator.reason]),
  );
};

/**
 * Signs one deployment release into a new directory with the release key the
 * release input names: the signed authority, its trust roots, the watcher's
 * rule and funding bundles, and the manifest, blueprint, provenance and
 * parameter registry they bind. The watcher's own release authoring writes and
 * reopens the bundle; the result is then checked with the node's shared
 * release verifier against the written trust roots.
 */
export const signDeploymentRelease = async (options: {
  readonly releaseInput: string;
  readonly deploymentInfo: string;
  readonly blueprint: string;
  readonly output: string;
  readonly fundingPaymentKeyHash?: string;
}) => {
  const output = absolute("--output", options.output);
  const { input, key, releaseProvenance, deploymentParameterRegistry } =
    await readReleaseInput(absolute("--release-input", options.releaseInput));
  if (
    input.fundingProfiles.length > 0 &&
    !/^[0-9a-f]{56}$/u.test(options.fundingPaymentKeyHash ?? "")
  )
    throw new Error(
      "Funding profiles require --funding-payment-key-hash of the prover funding wallet",
    );
  const manifest: unknown = JSON.parse(
    await readFile(
      absolute("--deployment-info", options.deploymentInfo),
      "utf8",
    ),
  );
  const blueprintJson = await readFile(
    absolute("--blueprint", options.blueprint),
  );
  // A release is written once, into a directory that did not exist.
  await mkdir(output);
  const path = (name: keyof typeof SIGNED_RELEASE_FILES) =>
    join(output, SIGNED_RELEASE_FILES[name]);
  const write = (target: string, contents: string | Uint8Array) =>
    writeTextFileAtomicNoReplace(target, contents, { mode: 0o644 });
  const { authority } = await authorWatcherDeploymentRelease({
    manifest,
    blueprintJson,
    programCommitments: input.programCommitments,
    ...(releaseProvenance === undefined ? {} : { releaseProvenance }),
    ...(deploymentParameterRegistry === undefined
      ? {}
      : { deploymentParameterRegistry }),
    fundingProfiles: input.fundingProfiles,
    fundingPaymentKeyHash: options.fundingPaymentKeyHash ?? "",
    signingKey: key,
    paths: {
      authority: path("authority"),
      rules: path("rules"),
      funding: path("funding"),
      manifest: path("manifest"),
      blueprint: path("blueprint"),
      deploymentInfo: path("deploymentInfo"),
      ...(releaseProvenance === undefined
        ? {}
        : { provenance: path("provenance") }),
      ...(deploymentParameterRegistry === undefined
        ? {}
        : { parameterRegistry: path("parameterRegistry") }),
    },
    existingAuthority: "refuse",
    writer: { replace: write, create: write },
  });
  await write(path("trustRoots"), json(authority.trustRoots));
  if (deploymentParameterRegistry !== undefined)
    await write(
      path("classification"),
      json(
        classificationOf(
          JSON.parse(await readFile(path("parameterRegistry"), "utf8")),
        ),
      ),
    );
  const release = verifySignedDeploymentRelease({
    signedIdentity: authority.signedIdentity,
    trustRoots: JSON.parse(
      await readFile(path("trustRoots"), "utf8"),
    ) as Parameters<typeof verifySignedDeploymentRelease>[0]["trustRoots"],
  });
  return {
    manifestId: release.manifest.manifestId,
    trustRootId: release.trustRootId,
    deploymentProfile: release.manifest.deploymentProfile.name,
    output,
    files: (await readdir(output)).sort(),
  };
};
