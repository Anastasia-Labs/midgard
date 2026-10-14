import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";

import { submitResolvedInit } from "../../src/submit-init.js";
import { captureLocallyEvaluatedTransaction } from "../../src/workflow/transaction-boundary.js";
import { commitInitializedProvedFixtureHeader } from "./submit-init-emulator-fixtures.commit-initialized-proved-fixture-header.js";
import { buildProvedFixtureDeploymentContext } from "./submit-init-emulator-fixtures.deployment-context.js";
import { EMULATOR_HEADER_CLOCK_HEADROOM_MS } from "./submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";

/** Actual SDK publication and Init; no synthetic deployment authority. */
export const makeGenuineFundingInitCaptureForTest = async () => {
  const fixture = await buildProvedFixtureDeploymentContext(0n, true);
  const {
    canonical,
    contracts,
    proverSigner,
    proverLucid,
    funderLucid,
    emulator,
  } = fixture;
  if (canonical === undefined)
    throw new Error("actual deployment context was not built");
  const headerStart =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const setup = await commitInitializedProvedFixtureHeader({
    lucid: funderLucid,
    contracts,
    header: {
      ...canonical.header,
      operatorVkey: paymentCredentialOf(await funderLucid.wallet().address())
        .hash,
      startTime: canonical.genesisEndTime,
      endTime: BigInt(headerStart) + BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
    },
    schedulerStartTime: BigInt(headerStart),
  });
  const { retained } = await makeGenuineDeploymentRegistryForTest({
    contracts,
    manifest: canonical.manifest,
  });
  const record = JSON.parse(
    readFileSync(`${realBlueprintPath}.deployment.json`, "utf8"),
  );
  const bytes = readFileSync(realBlueprintPath);
  const contextInput = {
    environment: "emulator" as const,
    manifest: canonical.manifest,
    blueprintBytes: bytes,
    deploymentParameterRegistry: retained,
    buildRecord: record,
    expectedCompilerVersion: record.compiler,
    // The fixture recipe identity is deliberately test-scoped; contributor
    // receipts separately retain the full actual source and compiled pins.
    sourceSnapshotSha256: createHash("sha256")
      .update(readFileSync(new URL(import.meta.url)))
      .digest("hex"),
    fundingPaymentKeyHash: proverSigner.paymentKeyHash,
  };
  const transaction = await captureLocallyEvaluatedTransaction(
    (preSubmitBoundary) =>
      submitResolvedInit({
        lucid: proverLucid,
        blueprint: fixture.realBlueprint,
        network: "Preprod",
        label: "funding producer Init",
        contracts: {
          steps: [contracts.fraudProofContracts.doubleSpend.steps[0]!] as const,
          computationThread: contracts.computationThread,
          hubOraclePolicyId: contracts.hubOracle.policyId,
          stateQueuePolicyId: contracts.stateQueue.policyId,
        },
        category: fixture.catalogue.categories.doubleSpend,
        catalogue: {
          policyId: contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            contracts.fraudProofCatalogue.spendingScriptAddress,
          root: fixture.catalogue.root,
        },
        signer: proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: canonical.witnessReferenceScripts,
        preSubmitBoundary,
      }),
  );
  return { fixture, contextInput, transaction };
};
