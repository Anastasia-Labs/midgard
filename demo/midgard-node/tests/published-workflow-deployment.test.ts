import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { inspect } from "node:util";

import {
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  bindFraudProofWorkflowDeployment,
  requireManifestBoundReferenceScriptUtxo,
} from "@al-ft/midgard-fault-proofs";
import {
  FraudProofComputationThreadStepDatum,
  NetworkIdStep02Datum,
} from "@al-ft/midgard-sdk";
import { Lucid, paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, beforeAll, expect, it } from "vitest";

import { readFinalizedDeploymentIdentity } from "../src/commands/contract-deployment-info.js";
import { verifyNodeRuntimeReferenceScriptsProgram } from "../src/transactions/reference-scripts.js";
import { publishWorkflowDeployment } from "./helpers/published-workflow-deployment.js";

let installed: Awaited<ReturnType<typeof publishWorkflowDeployment>>;
let directory: string | undefined;
let manifestPath: string;
let blueprintPath: string;
const expectedCount = Object.keys(
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
).length;

beforeAll(async () => {
  installed = await publishWorkflowDeployment({
    publicationMaxTargetsPerBatch: 8,
  }).catch((cause: unknown) => {
    throw new Error(inspect(cause, { depth: 12 }));
  });
  directory = await mkdtemp(join(tmpdir(), "midgard-published-workflow-"));
  manifestPath = join(directory, "deployment.json");
  blueprintPath = join(directory, "blueprint.json");
  await writeFile(manifestPath, JSON.stringify(installed.manifest));
  await writeFile(blueprintPath, installed.blueprintJson);
}, 420_000);

afterAll(async () => {
  if (directory !== undefined)
    await rm(directory, { recursive: true, force: true });
});

const bind = (manifest: unknown, blueprintJson: string) =>
  bindFraudProofWorkflowDeployment({
    manifest,
    blueprintJson,
    deploymentInfo: manifest,
    category: "networkId",
    headerHash: installed.manifest.genesis.headerHash,
    proverCredential: paymentCredentialOf(
      installed.manifest.referenceScriptDeployAddress,
    ).hash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      NetworkIdStep02Datum,
    ],
  });

it("publishes every role, initializes genesis and reopens the finalized deployment identity", async () => {
  const restored = readFinalizedDeploymentIdentity(manifestPath);
  expect(() =>
    verifyFinalizedDeploymentManifest(restored.manifest),
  ).not.toThrow();
  expect(restored.manifestId).toBe(installed.manifest.manifestId);
  expect(installed.receipts).toHaveLength(expectedCount);
  expect(
    installed.publicationMetrics.peakUnconfirmedTransactions,
  ).toBeLessThanOrEqual(8);
  expect(installed.publicationMetrics.peakUnconfirmedBytes).toBeLessThanOrEqual(
    100_000,
  );
  expect(installed.publicationMetrics.rejectedTransactions).toBe(0);
  expect(
    new Set(installed.receipts.map(({ outRef }) => outRef.txHash)).size,
  ).toBeLessThan(expectedCount);
  expect(installed.availabilityRegistrations).toHaveLength(5);
  expect(installed.initialization).toMatchObject({
    nonceConsumed: true,
    genesisConfirmed: true,
  });
  // Resolve current L1 outputs with a fresh observer after reading disk identity.
  const observer = await Lucid(installed.emulator, "Custom");
  const references = await Effect.runPromise(
    verifyNodeRuntimeReferenceScriptsProgram(
      observer,
      restored.manifest.referenceScriptDeployAddress,
      installed.contracts,
      installed.contracts.referenceScriptAuth,
    ),
  );
  expect(references).toHaveLength(expectedCount);
  for (const { name, utxo } of references) {
    expect(restored.manifest.referenceScripts[name]?.outRef, name).toBe(
      `${utxo.txHash}#${utxo.outputIndex}`,
    );
  }
  if (process.env.MIDGARD_PUBLISHED_WORKFLOW_RECEIPT_PATH !== undefined) {
    const sourceUrls = [
      new URL("./helpers/published-workflow-deployment.ts", import.meta.url),
      new URL("./published-workflow-deployment.test.ts", import.meta.url),
    ];
    const sourceHashes = await Promise.all(
      sourceUrls.map(async (url) => ({
        path: url.pathname,
        sha256: createHash("sha256")
          .update(await readFile(url))
          .digest("hex"),
      })),
    );
    await writeFile(
      process.env.MIDGARD_PUBLISHED_WORKFLOW_RECEIPT_PATH,
      JSON.stringify(
        {
          schemaVersion: "midgard-published-workflow-deployment-v1",
          scope:
            "emulator deployment, persisted finalized identity reload and reference authentication; no proof journey",
          blueprintSha256: installed.manifest.artifacts.blueprintHash,
          manifestId: installed.manifest.manifestId,
          manifestFileSha256: restored.contractDeploymentInfoSha256,
          sourceHashes,
          roleCount: expectedCount,
          publications: installed.receipts,
          publicationMetrics: installed.publicationMetrics,
          publicationJournalPath: installed.publicationJournalPath,
          initialization: installed.initialization,
          availabilityRegistrations: installed.availabilityRegistrations,
          publicationAuthorityExpired: true,
          allRolesUnique: true,
          persistedIdentityReloaded: true,
          authenticatedReferences: references.length,
          productionBinding:
            "separate acceptance test; not certified by this prerequisite receipt",
        },
        null,
        2,
      ) + "\n",
    );
  }
});

it("rejects a workflow blueprint that differs from the deployment manifest", async () => {
  const identityInput = Object.fromEntries(
    Object.entries({
      ...installed.manifest,
      artifacts: { blueprintHash: "00".repeat(32) },
    }).filter(([key]) => key !== "manifestId"),
  );
  const unsealed = {
    ...identityInput,
    manifestId: computeDeploymentManifestId(identityInput),
  };
  await expect(bind(unsealed, installed.blueprintJson)).rejects.toThrow(
    /blueprint SHA-256 does not match/u,
  );
});

// Binding depends on actual deployment identity and published references.
it("binds the installed workflow and resumes its identity from persisted files", async () => {
  const original = await bind(installed.manifest, installed.blueprintJson);
  const observer = await Lucid(installed.emulator, "Custom");
  const restored = await bind(
    JSON.parse(await readFile(manifestPath, "utf8")) as unknown,
    await readFile(blueprintPath, "utf8"),
  );
  expect(restored.deploymentFingerprint).toBe(original.deploymentFingerprint);
  expect(restored.blueprintHash).toBe(original.blueprintHash);
  expect(restored.definition).toEqual(original.definition);
  expect(Object.keys(restored.referenceScriptsByContract)).toHaveLength(
    expectedCount,
  );
  for (const [contractName, identity] of Object.entries(
    restored.referenceScriptsByContract,
  )) {
    const [txHash, outputIndex] = identity.outRef.split("#");
    const [utxo] = await observer.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    expect(utxo, contractName).toBeDefined();
    expect(
      requireManifestBoundReferenceScriptUtxo({
        binding: restored,
        contractName,
        utxo: utxo!,
      }),
    ).toEqual(utxo);
  }
});
