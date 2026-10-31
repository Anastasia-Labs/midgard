import { generateKeyPairSync } from "node:crypto";
import { mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { stackPaths } from "../src/full-stack/deployment.js";
import {
  checkFreshReleaseInput,
  completedProvenancePath,
  completePublicReleaseInput,
  prepareStackRelease,
  proverFundingKeyHash,
  releasePaths,
  stackBlueprintPath,
  stackRegistryPath,
} from "../src/full-stack/release.js";
import {
  signDeploymentRelease,
  SIGNED_RELEASE_FILES,
} from "../src/sign-deployment-release.js";
import {
  createPublicReleaseFixture,
  exists,
  publicStack,
  removePublicReleaseFixture,
} from "./full-stack-public-release.fixtures.js";

// Runs in the `preprod-public-release` project, which compiles the
// preprod-public profile against a blueprint built for it.

let fixture: Awaited<ReturnType<typeof createPublicReleaseFixture>>;
beforeAll(async () => {
  fixture = await createPublicReleaseFixture();
}, 300_000);
afterAll(removePublicReleaseFixture);

describe("public-release step", () => {
  it("signs the public release once; a resume reopens it and never signs again", async () => {
    const stack = await publicStack("sign-once", null);
    expect(stack.release.id).toBe("public-release");
    expect(stack.configuration.id).toBe("runtime-configuration");
    await expect(stack.release.reconcile(undefined)).resolves.toEqual({
      status: "retry",
    });
    const signed = (await stack.release.execute(undefined)) as {
      manifestId: string;
    };
    expect(signed.manifestId).toBe(fixture.manifest.manifestId);
    expect((await readdir(stack.releaseDirectory)).sort()).toEqual(
      Object.values(SIGNED_RELEASE_FILES).sort(),
    );
    const authority = join(
      stack.releaseDirectory,
      SIGNED_RELEASE_FILES.authority,
    );
    const before = await readFile(authority, "utf8");
    // A key the trust roots do not list: signing again could not verify.
    const input = JSON.parse(
      await readFile(stack.processes.config.watcher.releaseInput!, "utf8"),
    );
    await writeFile(
      input.signingKeyFile,
      generateKeyPairSync("ed25519").privateKey.export({
        format: "pem",
        type: "pkcs8",
      }),
    );
    await expect(stack.release.reconcile(undefined)).resolves.toEqual({
      status: "complete",
      data: signed,
    });
    await expect(stack.release.execute(undefined)).resolves.toEqual(signed);
    expect(await readFile(authority, "utf8")).toBe(before);
  });

  it("passes a fresh stack's release input before its deployment writes the registry", async () => {
    const stack = await publicStack("fresh", null);
    await rm(stackRegistryPath(stack.processes.config));
    await expect(
      checkFreshReleaseInput(stack.processes),
    ).resolves.toBeUndefined();
    // A provenance for another blueprint is refused before anything is spent.
    await stack.writeProvenance({
      ...stack.provenance,
      blueprintHash: "88".repeat(32),
    });
    await expect(checkFreshReleaseInput(stack.processes)).rejects.toThrow(
      `has blueprintHash ${"88".repeat(32)}, but the blueprint this stack built`,
    );
  });

  it("checks a public provenance's blueprint hash on every run, not only before the first release", async () => {
    const stack = await publicStack("blueprint-every-run", null);
    await stack.release.execute(undefined);
    await prepareStackRelease(stack.processes);
    await expect(
      checkFreshReleaseInput(stack.processes),
    ).resolves.toBeUndefined();
    // A resumed run whose provenance names another blueprint is refused before it builds on it.
    await stack.writeProvenance({
      ...stack.provenance,
      blueprintHash: "88".repeat(32),
    });
    await expect(checkFreshReleaseInput(stack.processes)).rejects.toThrow(
      `has blueprintHash ${"88".repeat(32)}, but the blueprint this stack built`,
    );
  });

  it("resumes a run stopped between completing the provenance and signing with the same provenance bytes", async () => {
    const stack = await publicStack("crash-before-signing", null);
    // The step completed the provenance, then the run stopped before it signed.
    await completePublicReleaseInput(stack.processes);
    const path = completedProvenancePath(stack.processes.config);
    const before = await readFile(path, "utf8");
    expect(await exists(stack.releaseDirectory)).toBe(false);
    // The operator's file is rewritten with the same fields in another order.
    const { evidence, ...rest } = stack.provenance;
    await stack.writeProvenance({ evidence, ...rest });
    await expect(stack.release.execute(undefined)).resolves.toMatchObject({
      manifestId: fixture.manifest.manifestId,
    });
    expect(await readFile(path, "utf8")).toBe(before);
    expect(
      await readFile(
        join(stack.releaseDirectory, SIGNED_RELEASE_FILES.provenance),
        "utf8",
      ),
    ).toBe(before);
  });

  /** Signs the stack's deployment out of band, as on a separate signing machine, into its release directory. */
  async function signOutOfBand(
    stack: Awaited<ReturnType<typeof publicStack>>,
    provenance: unknown,
  ) {
    const { config } = stack.processes;
    const input = JSON.parse(
      await readFile(config.watcher.releaseInput!, "utf8"),
    ) as { signingKeyFile: string; provenanceFile: string };
    const provenanceFile = join(config.runDirectory, "out-of-band.json");
    await mkdir(config.runDirectory, { recursive: true });
    await writeFile(provenanceFile, JSON.stringify(provenance));
    const releaseInput = join(config.runDirectory, "out-of-band-input.json");
    await writeFile(
      releaseInput,
      JSON.stringify({
        signingKeyFile: input.signingKeyFile,
        provenanceFile,
        parameterRegistryFile: stackRegistryPath(config),
        programCommitments: {},
        fundingProfiles: [],
      }),
    );
    await signDeploymentRelease({
      releaseInput,
      deploymentInfo: stackPaths(stack.processes).manifest,
      blueprint: stackBlueprintPath(config),
      output: stack.releaseDirectory,
      fundingPaymentKeyHash: proverFundingKeyHash(stack.processes),
    });
  }

  it("uses a release signed out of band from the operator's provenance with the deployed registry's digest", async () => {
    const stack = await publicStack("out-of-band", null);
    // The operator adds the deployed registry's digest to the provenance the stack's release input names.
    const provenance = {
      ...stack.provenance,
      evidence: {
        ...stack.provenance.evidence,
        parameterRegistryDigest: computeDeploymentManifestJsonDigest(
          fixture.retained,
        ),
      },
    };
    await stack.writeProvenance(provenance);
    await signOutOfBand(stack, provenance);
    await expect(stack.release.execute(undefined)).resolves.toMatchObject({
      manifestId: fixture.manifest.manifestId,
    });
    await expect(stack.release.reconcile(undefined)).resolves.toMatchObject({
      status: "complete",
    });
  });

  it("refuses a release signed out of band with another provenance than the stack completes", async () => {
    const stack = await publicStack("out-of-band-other", null);
    const digest = computeDeploymentManifestJsonDigest(fixture.retained);
    // Trusted, for this deployment and its images, but not the operator's provenance.
    await signOutOfBand(stack, {
      ...stack.provenance,
      evidence: {
        ...stack.provenance.evidence,
        frozenBuildDigest: "66".repeat(32),
        parameterRegistryDigest: digest,
      },
    });
    const refusal = `public.releaseDirectory ${stack.releaseDirectory} holds a release this deployment cannot use (its ${SIGNED_RELEASE_FILES.provenance} differs from the provenance this stack completed`;
    await expect(stack.release.execute(undefined)).rejects.toThrow(refusal);
    await expect(stack.release.reconcile(undefined)).rejects.toThrow(refusal);
  });

  it("completes the provenance with the deployed registry's digest once; a resume reuses it", async () => {
    const stack = await publicStack("completed", null);
    await stack.release.execute(undefined);
    const path = completedProvenancePath(stack.processes.config);
    // The operator's fields, and the digest of the registry the deployment wrote.
    const digest = computeDeploymentManifestJsonDigest(fixture.retained);
    const completed = JSON.parse(await readFile(path, "utf8")) as unknown;
    expect(completed).toEqual({
      ...stack.provenance,
      evidence: {
        ...stack.provenance.evidence,
        parameterRegistryDigest: digest,
      },
    });
    // The signed release carries exactly the completed provenance.
    expect(
      JSON.parse(
        await readFile(
          join(stack.releaseDirectory, SIGNED_RELEASE_FILES.provenance),
          "utf8",
        ),
      ),
    ).toEqual(completed);
    // A resume, and the watcher release signed after it, reuse the same file.
    const before = await readFile(path, "utf8");
    await expect(stack.release.execute(undefined)).resolves.toBeDefined();
    await prepareStackRelease(stack.processes);
    expect(await readFile(path, "utf8")).toBe(before);
    // Both sign the same release: the same key over the same completed inputs.
    const watcher = await releasePaths(stack.processes);
    expect(JSON.parse(await readFile(watcher.provenance, "utf8"))).toEqual(
      completed,
    );
    expect(await readFile(watcher.authority, "utf8")).toBe(
      await readFile(
        join(stack.releaseDirectory, SIGNED_RELEASE_FILES.authority),
        "utf8",
      ),
    );
    // Completed from a provenance the operator then edited, it is never rewritten.
    await stack.writeProvenance({
      ...stack.provenance,
      aikenSourceHash: "99".repeat(32),
    });
    await expect(completePublicReleaseInput(stack.processes)).rejects.toThrow(
      `${path} holds the provenance this stack completed earlier`,
    );
    expect(await readFile(path, "utf8")).toBe(before);
  });

  it("keeps a supplied registry digest that matches the deployment and refuses one that differs", async () => {
    const stack = await publicStack("supplied-digest", null);
    const withDigest = (parameterRegistryDigest: string) =>
      stack.writeProvenance({
        ...stack.provenance,
        evidence: { ...stack.provenance.evidence, parameterRegistryDigest },
      });
    await withDigest("77".repeat(32));
    await expect(stack.release.execute(undefined)).rejects.toThrow(
      `has evidence.parameterRegistryDigest ${"77".repeat(32)}, but this deployment's parameter registry`,
    );
    expect(await exists(stack.releaseDirectory)).toBe(false);
    await withDigest(computeDeploymentManifestJsonDigest(fixture.retained));
    await expect(stack.release.execute(undefined)).resolves.toMatchObject({
      manifestId: fixture.manifest.manifestId,
    });
  });

  it("refuses a partly written release directory, naming it", async () => {
    const stack = await publicStack("partial", null);
    await stack.release.execute(undefined);
    await rm(join(stack.releaseDirectory, SIGNED_RELEASE_FILES.provenance));
    await expect(stack.release.reconcile(undefined)).rejects.toThrow(
      `public.releaseDirectory ${stack.releaseDirectory} holds a release this deployment cannot use`,
    );
  });

  it("refuses to sign a release whose classification differs from the deployed one", async () => {
    const stack = await publicStack("classification", null);
    const classification = JSON.parse(
      await readFile(stack.classification, "utf8"),
    ) as Record<string, string>;
    const [title] = Object.keys(classification);
    await writeFile(
      stack.classification,
      JSON.stringify({ ...classification, [title!]: "Edited reason" }),
    );
    await expect(stack.release.execute(undefined)).rejects.toThrow(
      "classifies the unused validators differently from public.blueprintClassificationFile",
    );
    expect(await exists(stack.releaseDirectory)).toBe(false);
  });
});
