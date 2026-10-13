import { generateKeyPairSync } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeDeploymentManifestJsonDigest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "midgard-node/files/atomic-write";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";
import { beforeAll, expect, it } from "vitest";

import { authorWatcherDeploymentRelease } from "../../src/runtime/deployment-release-authoring.js";
import { verifyDeploymentReleaseParameterRegistry } from "../../src/runtime/deployment-release-parameter-registry.js";

let fixture: Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;
beforeAll(async () => {
  fixture = await makeGenuineDeploymentRegistryForTest();
}, 120_000);
it("accepts every real deployed script with its canonical published or null role", () => {
  verifyFinalizedDeploymentManifest(fixture.manifest);
  expect(
    verifyDeploymentReleaseParameterRegistry(
      fixture.retained,
      fixture.blueprintJson,
      fixture.manifest,
    ),
  ).toEqual(fixture.retained);
});
it("refuses changed deployed contract bytes and retained reference outpoints", () => {
  const changed = {
    ...fixture.manifest,
    contracts: {
      ...fixture.manifest.contracts,
      hubOracleMint: {
        ...fixture.manifest.contracts.hubOracleMint,
        contract: {
          ...fixture.manifest.contracts.hubOracleMint.contract,
          cborHex: fixture.manifest.contracts.txOrderMint.contract.cborHex,
        },
      },
    },
  };
  expect(() =>
    verifyDeploymentReleaseParameterRegistry(
      fixture.retained,
      fixture.blueprintJson,
      changed,
    ),
  ).toThrow("Parameter registry differs from deployed contract hubOracleMint");
  const published = Object.entries(fixture.retained.referenceScripts).find(
    ([, reference]) => reference?.outRef != null,
  );
  if (published === undefined)
    throw new Error("Genuine fixture has no published reference outpoint");
  const [role, original] = published;
  const outRef = original?.outRef;
  if (outRef == null)
    throw new Error("Genuine fixture has no published reference outpoint");
  const [txHash, outputIndex] = outRef.split("#");
  const retained = {
    ...fixture.retained,
    referenceScripts: {
      ...fixture.retained.referenceScripts,
      [role]: {
        ...original,
        outRef: `${txHash}#${(Number(outputIndex) + 1).toString()}`,
      },
    },
  };
  expect(() =>
    verifyDeploymentReleaseParameterRegistry(
      retained,
      fixture.blueprintJson,
      fixture.manifest,
    ),
  ).toThrow("reference records differ from deployment manifest");
});
it("the author reconstructs the genuine registry and enforces public measurement coverage before publishing", async () => {
  const directory = await mkdtemp(
    join(tmpdir(), "midgard-genuine-registry-author-"),
  );
  const paths = {
    authority: join(directory, "authority.json"),
    rules: join(directory, "rules.json"),
    funding: join(directory, "funding.json"),
    manifest: join(directory, "manifest.json"),
    blueprint: join(directory, "plutus.json"),
    deploymentInfo: join(directory, "deployment-info.json"),
    parameterRegistry: join(directory, "parameters.json"),
    provenance: join(directory, "provenance.json"),
  };
  const publicProfile =
    fixture.manifest.deploymentProfile.name === "preprod-public" ||
    fixture.manifest.deploymentProfile.name === "mainnet";
  // These metadata-only coordinates are syntax fixtures. The public control
  // must refuse missing measurements before any artifact or authority writes;
  // it does not attest images, independent rebuilds or deployed references.
  const releaseProvenance = publicProfile
    ? {
        schemaVersion: "midgard-deployment-release-provenance-v1",
        sourceRevision: "ab".repeat(20),
        deploymentProfileName: fixture.manifest.deploymentProfile.name,
        deploymentProfileDigest: fixture.manifest.deploymentProfileDigest,
        blueprintHash: fixture.manifest.artifacts.blueprintHash,
        aikenSourceHash: "11".repeat(32),
        compiler: {
          version: "test-only-metadata",
          forkRepository: "https://example.com/test-only-compiler",
          forkRevision: "22".repeat(20),
          cargoLockHash: "33".repeat(32),
        },
        roleImages: { node: `test-only/node@sha256:${"44".repeat(32)}` },
        evidence: {
          independentBlueprintRebuildDigest: "55".repeat(32),
          frozenBuildDigest: "66".repeat(32),
          parameterRegistryDigest: computeDeploymentManifestJsonDigest(
            fixture.retained,
          ),
        },
      }
    : undefined;
  let writes = 0;
  try {
    const authored = authorWatcherDeploymentRelease({
      manifest: fixture.manifest,
      blueprintJson: fixture.blueprintJson,
      deploymentParameterRegistry: fixture.retained,
      ...(releaseProvenance === undefined ? {} : { releaseProvenance }),
      programCommitments: { "computation-thread-policy-v1": "cd".repeat(32) },
      fundingProfiles: [],
      fundingPaymentKeyHash: "ee".repeat(28),
      signingKey: generateKeyPairSync("ed25519").privateKey,
      paths,
      existingAuthority: "refuse",
      writer: {
        replace: (path, contents) => {
          writes += 1;
          return writeTextFileAtomic(path, contents, { mode: 0o600 });
        },
        create: (path, contents) => {
          writes += 1;
          return writeTextFileAtomicNoReplace(path, contents, { mode: 0o600 });
        },
      },
    });
    if (publicProfile) {
      await expect(authored).rejects.toThrow(
        "Public release requires measured funding profiles for every installed catalogue category",
      );
      expect(writes).toBe(0);
    } else {
      const result = await authored;
      expect(result.deploymentAuthority.deploymentIdentity.manifestId).toBe(
        fixture.manifest.manifestId,
      );
    }
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
}, 120_000);
