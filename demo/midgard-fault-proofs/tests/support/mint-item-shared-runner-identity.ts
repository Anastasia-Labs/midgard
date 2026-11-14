import {
  acceptedVerdictSubject,
  getProtocolParameters,
} from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import {
  createManifestBoundMintItemNonCanonicalSubmission,
  createMintItemNonCanonicalRawL1StageResolver,
  type ManifestBoundMintItemNonCanonicalConfig,
  MINT_ITEM_NON_CANONICAL_WORKFLOW,
} from "../../src/mint-item-non-canonical/index.js";
import { MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION } from "../../src/mint-item-non-canonical/workflow.mint-item-non-canonical-family-definition.js";
import { loadStateQueueTopology } from "../../src/remove-fraudulent-block.load-state-queue-topology.js";
import * as deployment from "../../src/workflow/deployment-manifest-binding.js";
import * as observations from "../../src/workflow/family-l1-observation.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
} from "../../src/workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../../src/workflow/manifest-bound-family-assembly.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../../src/workflow/raw-l1-publication-observation.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../../src/workflow/raw-l1-snapshot.js";
import {
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
  workflowPreflightTransaction,
} from "../../src/workflow/transaction-boundary.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { makeHeader } from "./emulator/header-fixtures.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import {
  committedInclusions,
  evidenceOf,
  registeredContracts,
  witnessSetCborOf,
} from "./mint-item-non-canonical-lifecycle.js";
import { mintField } from "./mint-item-vectors.js";
import { fixtureLeaseCoordinator } from "./script-decode-installed-host.js";
import {
  countedTransactionsRoot,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  publishRemovalReferenceScripts,
} from "./submit-init-emulator-shared.js";

const category = "mintItemNonCanonical" as const;
const provenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "mint-item-shared-runner-identity",
  grade: "security",
} as const;
const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * Thirty-three canonical policies walk the envelope in two checkpoints; the
 * last policy's 460-entry map ends in a zero quantity, so the item scan takes
 * several windows and the field (over 15,148 bytes) needs certified carriage.
 */
const identityField = () => {
  const items = Array.from({ length: 33 }, (_, index) => {
    const key = Buffer.alloc(28);
    key.writeUInt16BE(index, 26);
    return Buffer.concat([
      Buffer.from("82581c", "hex"),
      key,
      Buffer.from("a14001", "hex"),
    ]);
  });
  const entries = Array.from({ length: 460 }, (_, index) => {
    const name = Buffer.alloc(32);
    name.writeUInt16BE(index, 30);
    return Buffer.concat([Buffer.from([0x58, 32]), name, Buffer.from([1])]);
  });
  const faulty = Buffer.concat([
    Buffer.from(`82581c${"ff".repeat(28)}b901cc`, "hex"),
    ...entries,
  ]);
  faulty[faulty.length - 1] = 0;
  return { items: [...items, faulty], itemIndex: items.length };
};

/** Two envelope checkpoints, several scan windows and one peel, so every
 * self-loop and both removal shapes are compared. */
const covering = <Row extends { point: string; leased: boolean }>(
  rows: readonly Row[],
) => {
  const count = (stage: string) =>
    rows.filter(({ point }) => point.startsWith(`${stage} `)).length;
  for (const stage of ["init", "step_01", "certify_field_carriage", "step_04"])
    expect(count(stage), stage).toBe(1);
  expect(count("publish_field_carriage")).toBe(2);
  expect(count("step_02")).toBe(2);
  expect(count("step_03")).toBeGreaterThan(1);
  expect(
    rows
      .filter(({ point }) => point.startsWith("remove "))
      .map(({ leased }) => leased),
  ).toEqual([true, false]);
  return rows;
};

/** The bespoke submission action that builds the same physical transaction. */
const bespokeAction = (stage: unknown) => {
  const actions: Record<string, string> = {
    init: "submitInit",
    step_01: "submitStep01",
    step_02: "submitStep02",
    publish_field_carriage: "submitStep02",
    certify_field_carriage: "submitStep02",
    step_03: "submitStep03",
    step_04: "submitStep04",
    remove: "removeDescendants",
  };
  const action = actions[String(stage)];
  if (action === undefined)
    throw new Error(`unexpected stage ${String(stage)}`);
  return action as "submitInit";
};

/**
 * Drives one mint-item proof and its removal (with one descendant) through
 * the shared cursor runner, and at every point captures the bespoke
 * submission's transaction at the same chain state. Returns one row per
 * physical transaction.
 */
export const mintItemSharedRunnerIdentity = async () => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMintItemNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const registered = await registeredContracts(h);
  const { items, itemIndex } = identityField();
  const tx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    mintPreimageCbor: mintField(...items),
  });
  const committed = await committedInclusions([tx]);
  const inclusion = committed.inclusions[0]!;
  const evidence = evidenceOf(
    acceptedVerdictSubject(inclusion.nativeTxId),
    tx,
    itemIndex,
  );
  expect(evidence.carriage).toBe("Certified");
  const setup = await setupFraudulentBlock({
    funderLucid: h.funderLucid,
    emulator: h.emulator,
    contracts: h.contracts,
    catalogue: registered.catalogue,
    fixture: {
      transactionsRoot: committed.transactionsRoot,
      l2TransactionCount: 1n,
    },
  });
  // One descendant, so removal peels the successor before the target.
  const ahead =
    Number(setup.header.endTime - 60_000n) - h.emulator.now() + 1_000;
  if (ahead > 0) h.emulator.awaitSlot(Math.ceil(ahead / 1_000));
  await submitSuccessorBlockTx({
    lucid: h.funderLucid,
    emulator: h.emulator,
    contracts: h.contracts,
    anchorBlockUnit: setup.stateQueueBlockUnit,
    header: {
      ...makeHeader(
        setup.header.operatorVkey,
        emulatorSuccessorHeaderStart({
          predecessorEndTime: setup.header.endTime,
          emulator: h.emulator,
        }),
        await countedTransactionsRoot(committed.transactionsRoot, 1n),
        1n,
      ),
      prevHeaderHash: setup.headerHash,
      prevUtxosRoot: setup.header.utxosRoot,
      utxosRoot: setup.header.utxosRoot,
    },
    hubOracle: setup.hubOracle,
    scheduler: setup.scheduler,
    activeOperatorNode: setup.activeOperatorNode,
    activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
  });
  const certificateReference = await registered.publishReferences();
  const removalReferences = await publishRemovalReferenceScripts({
    lucid: h.proverLucid,
    contracts: h.contracts,
  });
  const headerHash = setup.headerHash;
  const binding = {
    deploymentFingerprint: "aa".repeat(32),
    network: "Custom",
    blueprint: h.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
      removalReferenceScripts: removalReferences.published,
    }),
    definition: { category, headerHash },
    resolvedContracts: { category: registered.category },
    catalogue: {
      policyId: h.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        h.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: h.catalogue.root,
    },
    fieldPreimageCertificate: {
      policyId: h.contracts.fieldPreimageCertificate.policyId,
      mintingScript: h.contracts.fieldPreimageCertificate.mintingScript,
    },
    releaseEconomics: {
      policy: {
        fraudProverRewardLovelace:
          getProtocolParameters("Preprod").fraud_prover_reward.toString(),
      },
    },
  };
  const [step01, step02, step03, step04] = registered.references;
  const referenceScripts = {
    step01: step01!,
    step02: step02!,
    step03: step03!,
    step04: step04!,
    fieldPreimageCertificateMint: certificateReference,
    witnesses: h.witnessReferenceScripts,
  };
  const config = {
    schemaVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
    lucid: h.proverLucid,
    signer: h.proverSigner,
    binding,
    contracts: registered.contracts,
    referenceScripts,
  } as unknown as ManifestBoundMintItemNonCanonicalConfig;
  const holding = async (address: string, policyId: string) =>
    (await h.proverLucid.utxosAt(address)).filter((utxo) =>
      Object.keys(utxo.assets).some((unit) => unit.startsWith(policyId)),
    );
  // The current chain as the raw-L1 port derives it: queue topology, our
  // proof token, else our thread at whichever step address holds it.
  const observe = async () => {
    const topology = await loadStateQueueTopology({
      lucid: h.proverLucid,
      stateQueueAddress: h.contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: h.contracts.stateQueue.policyId,
    });
    const target = topology.nodeByHeaderHash.get(headerHash);
    if (target === undefined) throw new Error("fraudulent block is removed");
    const stateQueueBlockOutRef = outRef(target.utxo);
    const [proof] = await holding(
      h.contracts.fraudProof.spendingScriptAddress,
      h.contracts.fraudProof.policyId,
    );
    if (proof !== undefined) {
      const successor = topology.successorByHeaderHash.get(headerHash);
      return {
        provenance,
        stage: {
          kind: "proof_token" as const,
          fraudProofOutRef: outRef(proof),
          stateQueueBlockOutRef,
          nextRemovalOutRef:
            successor === undefined
              ? stateQueueBlockOutRef
              : outRef(successor.utxo),
        },
      };
    }
    for (const [index, step] of registered.contracts.steps.entries()) {
      const [thread] = await holding(
        step.spendingScriptAddress,
        h.contracts.computationThread.policyId,
      );
      if (thread !== undefined)
        return {
          provenance,
          stage: {
            kind: "step" as const,
            step: index + 1,
            threadOutRef: outRef(thread),
            stateQueueBlockOutRef,
          },
        };
    }
    return {
      provenance,
      stage: { kind: "not_started" as const, stateQueueBlockOutRef },
    };
  };
  const unexpected = async (): Promise<never> => {
    throw new Error("unexpected L1 read");
  };
  const l1 = {
    portVersion: observations.FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    observe,
    observeHeader: unexpected,
    observeSignedTransaction: unexpected,
    transactionConfirmed: async () => true,
    rawL1: { authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY },
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async ({ expectedOutRef }: { expectedOutRef: string }) => {
        const [txHash, index] = expectedOutRef.split("#");
        const [utxo] = await h.proverLucid.utxosByOutRef([
          { txHash: txHash!, outputIndex: Number(index) },
        ]);
        return utxo === undefined
          ? { kind: "not_found" as const }
          : { kind: "confirmed" as const, outRef: expectedOutRef };
      },
    },
  };
  const stateQueueMutationLeaseCoordinator = fixtureLeaseCoordinator();
  const spies = [
    vi
      .spyOn(deployment, "bindFraudProofWorkflowDeployment")
      .mockResolvedValue(binding as never),
    vi
      .spyOn(deployment, "requireManifestBoundReferenceScriptUtxo")
      .mockImplementation(({ utxo }) => utxo),
    vi
      .spyOn(observations, "createFraudProofFamilyL1ObservationPort")
      .mockReturnValue(l1 as never),
  ];
  const bound = await bindManifestBoundFamilyWorkflow(
    MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION,
    {
      manifest: {},
      blueprintJson: "{}",
      deploymentInfo: {},
      headerHash,
      lucid: h.proverLucid,
      signer: h.proverSigner,
      referenceScripts: {
        steps: [step01!, step02!, step03!, step04!],
        witnesses: h.witnessReferenceScripts,
        fieldPreimageCertificateMint: certificateReference,
      },
      l1Source: {},
      stateQueueMutationLeaseCoordinator,
    } as never,
  ).finally(() => {
    for (const spy of spies) spy.mockRestore();
  });
  const source = {
    nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
    witnessSetCompactCbor: witnessSetCborOf(tx),
    acceptedInclusion: inclusion,
  };
  const material = {
    category,
    headerHash,
    detectionId: `mint-item-non-canonical:0:${inclusion.nativeTxId}:${itemIndex.toString()}`,
    evidence,
    source,
  };
  const { adapter } = assembleBoundManifestBoundFamilyWorkflow(
    MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION,
    bound,
    {
      config,
      material: {
        prepare: unexpected,
        validatePreparedArtifact: unexpected,
        require: () => material,
      },
    } as never,
  );
  // The bespoke claims persist across its aborted captures, as its own
  // journal did, so its later steps reuse the carriage the runner published.
  const ownOutputs = createOwnOutputJournal();
  const bespoke = async (stage: unknown) =>
    await captureLocallyEvaluatedTransaction(
      async (preSubmitBoundary: FraudProofPreSubmitBoundary) => {
        await createManifestBoundMintItemNonCanonicalSubmission({
          config,
          observe: unexpected,
          resolveStage: createMintItemNonCanonicalRawL1StageResolver({
            config,
            l1: l1 as never,
            source,
          }),
          stateQueueMutationLeaseCoordinator,
          ownOutputs,
          preSubmitBoundary,
        }).submit(bespokeAction(stage), evidence);
      },
    );
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
    deploymentFingerprint: binding.deploymentFingerprint,
    category,
    target: { kind: "state_queue_header", headerHash },
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const append = (event: FraudProofWorkflowJournalEvent) =>
    entries.push({
      schemaVersion: "midgard-fraud-proof-workflow-journal-entry-v1",
      identity,
      workflowId,
      sequence: entries.length,
      recordedAt: new Date(h.emulator.now()).toISOString(),
      event,
    } as FraudProofWorkflowJournalEntry);
  const rows: {
    point: string;
    txHash: string;
    equal: boolean;
    leased: boolean;
  }[] = [];
  // Validity windows read the clock; both builders read the emulator's.
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => h.emulator.now());
  try {
    for (;;) {
      const context = { identity, workflowId, artifact: {}, entries };
      const observed = await adapter.observe(context);
      if (observed.kind !== "action_required")
        throw new Error(`runner stopped: ${JSON.stringify(observed)}`);
      const { action } = observed;
      const preflight = await adapter.preflight({ ...context, action });
      const shared = workflowPreflightTransaction(preflight)!;
      const own = await bespoke(action.input.stage);
      const count = rows.filter(({ point }) =>
        point.startsWith(String(action.input.stage)),
      ).length;
      rows.push({
        point: `${String(action.input.stage)} #${(count + 1).toString()}`,
        txHash: preflight.txHash,
        equal:
          own.signed.toTransaction().to_cbor_hex() ===
          shared.toTransaction().to_cbor_hex(),
        leased: action.input.requiresMutationLease === true,
      });
      append({
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        ...(preflight.durableRecovery === undefined
          ? {}
          : { durableRecovery: preflight.durableRecovery }),
        attempt: 1,
        txHash: preflight.txHash,
      });
      const submitted = await adapter.submit({
        ...context,
        action,
        preflight,
      });
      if (submitted.kind !== "submitted")
        throw new Error(`runner refused submission: ${submitted.kind}`);
      await h.proverLucid.awaitTx(submitted.txHash);
      append({
        kind: "confirmed",
        actionId: action.actionId,
        txHash: submitted.txHash,
      });
      if (
        action.input.stage === "remove" &&
        action.input.nextRemovalOutRef === action.input.stateQueueBlockOutRef
      )
        return covering(rows);
    }
  } finally {
    clock.mockRestore();
  }
};
