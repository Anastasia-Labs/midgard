import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import * as artifactAdmission from "../src/value-not-preserved/artifact.js";
import { CONSERVATION_POSITIONS } from "../src/value-not-preserved/contracts.js";
import * as fieldPrerequisite from "../src/value-not-preserved/field-prerequisite.js";
import { conservationDatum } from "../src/value-not-preserved/submit-union.js";
import {
  createManifestBoundValueConservationWorkflow,
  type ManifestBoundValueConservationWorkflowConfig,
} from "../src/value-not-preserved/workflow.js";
import * as deployment from "../src/workflow/deployment-manifest-binding.js";
import * as observations from "../src/workflow/family-l1-observation.js";
import * as fieldCarriage from "../src/workflow/field-carriage-prerequisite.js";
import * as proofChunks from "../src/workflow/proof-chunk-prerequisite.js";
import * as derivation from "../src/workflow/raw-l1-family-derivation.js";
import * as rawAdmission from "../src/workflow/raw-l1-snapshot.js";

afterEach(() => vi.restoreAllMocks());

it("derives the conservation cursor and datum from one snapshot while the pending fold is included", async () => {
  const headerHash = "11".repeat(28);
  const owner = "22".repeat(28);
  const address = (byte: string) =>
    credentialToAddress("Preprod", { type: "Script", hash: byte.repeat(28) });
  const entryAddress = address("33");
  const nextAddress = address("44");
  const script = { type: "PlutusV3", script: "49480100002221200101" } as const;
  const reference = { scriptRef: script };
  const chain = {
    steps: Array.from({ length: 4 }, () => ({
      spendingScriptAddress: entryAddress,
    })),
    ...Object.fromEntries(
      CONSERVATION_POSITIONS.map((position) => [
        position,
        { spendingScriptAddress: nextAddress },
      ]),
    ),
  };
  // Isolate the production factory's observation scheduling. Authority admission
  // and artifact replay have their own suites; real CML outputs and conservation
  // datums below exercise the cursor matching after these admitted boundaries.
  vi.spyOn(deployment, "bindFraudProofWorkflowDeployment").mockResolvedValue({
    network: "Preprod",
    definition: { computationThread: { steps: [] } },
    resolvedContracts: {
      contracts: {
        valueNotPreserved: chain,
        computationThread: {},
        fraudProof: {},
      },
      stateQueuePolicyId: "55".repeat(28),
    },
    cardanoProtocolParameters: { maxTxSize: 16_384 },
    releaseFinality: {},
    releaseEconomics: {},
  } as unknown as Awaited<
    ReturnType<typeof deployment.bindFraudProofWorkflowDeployment>
  >);
  vi.spyOn(deployment, "assertManifestBoundWorkflowSigner").mockImplementation(
    () => undefined,
  );
  vi.spyOn(
    deployment,
    "requireManifestBoundReferenceScriptUtxo",
  ).mockImplementation(({ utxo }) => utxo);
  vi.spyOn(
    deployment,
    "releaseFinalityAuthorityFromDeploymentBinding",
  ).mockReturnValue({} as never);
  vi.spyOn(artifactAdmission, "admitValueConservationArtifact").mockReturnValue(
    {
      headerHash,
      actions: [
        { position: "entry", inputState: null },
        { position: "unionForcedSource", inputState: null },
      ],
    } as unknown as ReturnType<
      typeof artifactAdmission.admitValueConservationArtifact
    >,
  );
  const request = {} as rawAdmission.FraudProofRawL1SnapshotRequest;
  vi.spyOn(
    derivation,
    "fraudProofRawL1SnapshotRequestForFamily",
  ).mockReturnValue(request);
  vi.spyOn(rawAdmission, "admitFraudProofRawL1Snapshot").mockImplementation(
    ({ value }) => value as rawAdmission.FraudProofRawL1Snapshot,
  );
  const datum = conservationDatum(owner, null);
  const snapshot = (byte: string, outputAddress: string) => ({
    scopes: [
      {
        utxos: [
          {
            outRef: `${byte.repeat(32)}#0`,
            outputCbor: CML.TransactionOutput.new(
              CML.Address.from_bech32(outputAddress),
              CML.Value.from_coin(2_000_000n),
              CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
            ).to_canonical_cbor_hex(),
          },
        ],
      },
    ],
  });
  const before = snapshot("66", entryAddress);
  const after = snapshot("77", nextAddress);
  const capture = vi
    .fn()
    .mockResolvedValueOnce(before)
    .mockResolvedValue(after);
  const stage = (value: typeof before) => ({
    kind: "step" as const,
    step: 1 as const,
    threadOutRef: value.scopes[0]!.utxos[0]!.outRef,
    stateQueueBlockOutRef: `${"88".repeat(32)}#0`,
  });
  const derive = vi
    .spyOn(derivation, "deriveFraudProofRawL1FamilyStage")
    .mockImplementation(async ({ snapshot: value }) =>
      stage(value as unknown as typeof before),
    );
  const observe = vi.fn(async () => ({ stage: stage(await capture()) }));
  vi.spyOn(
    observations,
    "createFraudProofFamilyLocalKupmiosL1ObservationPort",
  ).mockReturnValue({
    observe,
    rawL1: { capture },
  } as unknown as ReturnType<
    typeof observations.createFraudProofFamilyLocalKupmiosL1ObservationPort
  >);
  vi.spyOn(
    observations,
    "createFraudProofFamilyAuthenticatedL1TerminalVerifier",
  ).mockReturnValue({} as never);
  vi.spyOn(
    fieldPrerequisite,
    "createValueConservationFieldPrerequisite",
  ).mockReturnValue({} as never);
  vi.spyOn(
    proofChunks,
    "createAuthenticatedProofChunkPrerequisitePort",
  ).mockReturnValue({} as never);
  vi.spyOn(fieldCarriage, "withFieldCarriagePrerequisite").mockImplementation(
    ({ base }) => base,
  );
  vi.spyOn(proofChunks, "withProofChunkPrerequisite").mockImplementation(
    ({ base }) => base,
  );
  const workflow = await createManifestBoundValueConservationWorkflow({
    manifest: {},
    blueprintJson: "{}",
    deploymentInfo: {},
    headerHash,
    lucid: {},
    signer: { paymentKeyHash: owner },
    source: {},
    stateQueueMutationLeaseCoordinator: {},
    referenceScripts: {
      steps: Array.from({ length: 4 }, () => reference),
      union: Object.fromEntries(
        CONSERVATION_POSITIONS.map((position) => [position, reference]),
      ),
      removal: {},
      witnesses: {},
      fieldPreimageCertificateMint: reference,
    },
  } as unknown as ManifestBoundValueConservationWorkflowConfig);
  const context: Parameters<typeof workflow.adapter.observe>[0] = {
    identity: {
      schemaVersion: "midgard-fraud-proof-workflow-identity-v1" as const,
      deploymentFingerprint: "99".repeat(32),
      category: "valueNotPreserved",
      target: { kind: "state_queue_header" as const, headerHash },
    },
    workflowId: "aa".repeat(32),
    artifact: {},
    entries: [],
  };
  await expect(workflow.adapter.observe(context)).resolves.toMatchObject({
    kind: "action_required",
    action: { actionId: `fold:0:${"66".repeat(32)}#0`, input: { index: 0 } },
  });
  expect(capture).toHaveBeenCalledTimes(1);
  await expect(workflow.adapter.observe(context)).resolves.toMatchObject({
    kind: "action_required",
    action: { actionId: `fold:1:${"77".repeat(32)}#0`, input: { index: 1 } },
  });
  expect(capture).toHaveBeenCalledTimes(2);
  expect(derive.mock.calls.map(([input]) => input.snapshot)).toEqual([
    before,
    after,
  ]);
  expect(observe).not.toHaveBeenCalled();
});
