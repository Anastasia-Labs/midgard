/**
 * Deployment resolution of the Plutus execution dispute against a published
 * deployment: the curated catalogue carries validationTraceDispute, so the
 * real workflow publication records and publishes every one of its steps.
 * Resolution checks each recorded step against the reapplied chain, refuses a
 * later step whose recorded hash is another script's, and refuses a deployment
 * that omits a later step, so a missing reference script fails at startup
 * rather than mid-dispute.
 */
import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { VALIDATION_TRACE_DISPUTE_STEP_COUNT } from "@al-ft/midgard-sdk";
import { publishWorkflowDeployment } from "midgard-node/tests/helpers/published-workflow-deployment";
import { beforeAll, describe, expect, it } from "vitest";

import { resolveFaultProofDeploymentContracts } from "../src/index.js";

type RecordedContracts = Record<
  string,
  { scriptHash: string; refScriptUTxO?: unknown }
>;

const STEP_NAMES = deploymentFamilyStepNames("validationTraceDispute");
/** A step deep in the chain and the last one. */
const LATER_STEPS = [67, STEP_NAMES.length - 1] as const;
const stepLabel = (index: number) =>
  `step-${(index + 1).toString().padStart(2, "0")}`;

describe("validation-trace dispute deployment resolution", () => {
  let installed: Awaited<ReturnType<typeof publishWorkflowDeployment>>;
  let blueprint: unknown;
  let contracts: RecordedContracts;

  beforeAll(async () => {
    installed = await publishWorkflowDeployment({
      publicationMaxTargetsPerBatch: 8,
    });
    blueprint = JSON.parse(installed.blueprintJson);
    contracts = installed.manifest.contracts as unknown as RecordedContracts;
  }, 600_000);

  const resolve = (deploymentInfo: unknown) =>
    resolveFaultProofDeploymentContracts({
      blueprint,
      deploymentInfo,
      network: installed.manifest.network,
      categoryName: "validationTraceDispute",
      requireStateQueueMint: true,
      requireFraudProofSpend: true,
    });

  /** The published manifest with one step's entry changed or removed. */
  const withStep = (
    index: number,
    change: (contracts: RecordedContracts, name: string) => void,
  ) => {
    const manifest = structuredClone(installed.manifest) as unknown as {
      contracts: RecordedContracts;
    };
    change(manifest.contracts, STEP_NAMES[index]!);
    return manifest;
  };

  it("resolves every recorded step of the published deployment", async () => {
    expect(SELECTED_DEPLOYMENT_PROFILE.excluded_fraud_proof_categories).toEqual(
      [],
    );
    expect(STEP_NAMES).toHaveLength(VALIDATION_TRACE_DISPUTE_STEP_COUNT);
    for (const name of STEP_NAMES) {
      expect(contracts[name]?.scriptHash, name).toMatch(/^[0-9a-f]{56}$/u);
      expect(contracts[name]?.refScriptUTxO, name).toBeTruthy();
    }

    const resolved = await resolve(installed.manifest);
    const chain = resolved.contracts.validationTraceDispute!;
    expect(chain.steps.map((step) => step.spendingScriptHash)).toEqual(
      STEP_NAMES.map((name) => contracts[name]!.scriptHash),
    );
  }, 120_000);

  it.each(LATER_STEPS)(
    "refuses a deployment whose chain step %i records another script",
    async (index) => {
      const name = STEP_NAMES[index]!;
      const other = STEP_NAMES[index - 1]!;
      await expect(
        resolve(
          withStep(index, (recorded) => {
            recorded[name] = {
              ...recorded[name]!,
              scriptHash: recorded[other]!.scriptHash,
            };
          }),
        ),
      ).rejects.toThrow(`${name} ${stepLabel(index)} script`);
    },
    120_000,
  );

  it.each(LATER_STEPS)(
    "refuses a deployment that omits chain step %i",
    async (index) => {
      const name = STEP_NAMES[index]!;
      await expect(
        resolve(
          withStep(index, (recorded) => {
            delete recorded[name];
          }),
        ),
      ).rejects.toThrow(
        `Deployment info is missing "${name}" for its ${stepLabel(index)} script`,
      );
    },
    120_000,
  );
});
