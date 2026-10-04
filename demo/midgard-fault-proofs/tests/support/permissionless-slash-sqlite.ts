import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import { outRefLabel } from "@al-ft/midgard-core";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type Header } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { readFraudSlashFundingAuthority } from "../../src/remove-fraudulent-block.js";
import { createWorkflowActuationPermitController } from "../../src/workflow/actuation-permit.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "../../src/workflow/complete-replay.js";
import {
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  confirmWorkflowFundingReservationTransaction,
  createWorkflowFundingReservationPermit,
  prepareWorkflowFundingReservationTransaction,
  readWorkflowFundingRecovery,
  type WorkflowFundingSubmissionHandoff,
} from "../../src/workflow/funding-reservation-permit.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
} from "../../src/workflow/journal.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import { WORKFLOW_RUNNER_FACTORIES } from "../../src/workflow/runtime.js";
import {
  createWorkflowRuntimeFundingPolicy,
  readWorkflowRuntimeFundingPolicy,
} from "../../src/workflow/runtime-funding-policy.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  workflowTransactionCollateralInputOutRefs,
} from "../../src/workflow/transaction-boundary.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import {
  assertPermissionlessSlashRemovedHeader,
  createPermissionlessSlashSqlitePort,
} from "./permissionless-slash-sqlite-port.js";
import type { bindCanonicalFixtureHeader } from "./submit-init-emulator-fixtures.deployment-context.js";

type Evidence = Awaited<ReturnType<typeof bindCanonicalFixtureHeader>>;
type Boundary = LocallyEvaluatedTransaction;

/** Only the existing test opener's deployment-plan admission is bypassed. */
export const preparePermissionlessSlashSqlite = async ({
  evidence,
  removedHeader,
  callerLucid,
  callerKeyHash,
  boundary,
}: {
  readonly evidence: Evidence;
  readonly removedHeader: Header;
  readonly callerLucid: LucidEvolution;
  readonly callerKeyHash: string;
  readonly boundary: Boundary;
}) => {
  const { openStore } = await import(
    "midgard-watcher/tests/funding/sqlite-prover-funding-reservation-store.signed-transition"
  );
  const { WATCHER_PROVER_FUNDING_RESERVATION_PLAN } = await import(
    "midgard-watcher"
  );
  const authority = readFraudSlashFundingAuthority(boundary.signed);
  if (authority === null)
    throw new Error("Canonical builder did not mint its live slash authority");
  const { manifest } = evidence;
  expect(authority.headerHash).toBe(evidence.headerHash);
  expect(authority.deploymentFingerprint).toBe(manifest.manifestId);
  await assertPermissionlessSlashRemovedHeader({
    authority,
    manifest,
    removedHeader,
    callerLucid,
  });
  const observation = authenticatedHeaderObservation(evidence);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: manifest.manifestId,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => ({
        schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: manifest.manifestId,
        blueprintHash: manifest.artifacts.blueprintHash,
        policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
          manifest.l1Finality,
        ),
        policy: manifest.l1Finality,
      }),
    },
  });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: manifest.l1Finality.confirmationDepth,
      }),
    sources: [
      {
        sourceId: "permissionless-retained-canonical-pair",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "permissionless-retained-canonical-pair",
            grade: "security" as const,
          },
          sourceId: "permissionless-retained-canonical-pair",
          sourcePeerId: "fixture",
          payloadEnvelopeCbor: evidence.payloadEnvelopeCbor,
          attempts: [],
        }),
      },
    ],
  });
  if (
    decision.decision !== "fault_detected" ||
    decision.category !== "doubleSpend"
  )
    throw new Error("Actual canonical pair was not admitted as doubleSpend");
  expect(decision.headerHash).toBe(evidence.headerHash);
  const controller = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "0",
  });
  const runner = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
    throw new Error("Boundary control does not start a duplicate workflow");
  });
  const released = manifest.economics;
  const economics = {
    profile: released.profile,
    requiredBondLovelace: released.requiredBondLovelace.toString(),
    slashingPenaltyLovelace: released.slashingPenaltyLovelace.toString(),
    fraudProverRewardLovelace: released.fraudProverRewardLovelace.toString(),
    inactivitySlashingPenaltyLovelace:
      released.inactivitySlashingPenaltyLovelace.toString(),
    proverCollateralFloorLovelace:
      released.proverCollateralFloorLovelace.toString(),
  };
  const uniqueContracts = new Map<
    string,
    {
      address: string;
      scriptHash: string;
      role: "protocol_state" | "correction_lock";
    }
  >();
  for (const [name, entry] of Object.entries(manifest.contracts)) {
    if (!name.endsWith("Spend")) continue;
    const address = credentialToAddress(manifest.network, {
      type: "Script",
      hash: entry.scriptHash,
    });
    uniqueContracts.set(address, {
      address,
      scriptHash: entry.scriptHash,
      role:
        name === "correctionLockSpend" ? "correction_lock" : "protocol_state",
    });
  }
  const policy = createWorkflowRuntimeFundingPolicy({
    category: "doubleSpend",
    runner,
    deploymentFingerprint: manifest.manifestId,
    fundingPaymentKeyHash: callerKeyHash,
    protocolParameters: manifest.cardanoProtocolParameters.snapshot,
    economics: {
      schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: manifest.manifestId,
      blueprintHash: manifest.artifacts.blueprintHash,
      policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(economics),
      policy: economics,
    },
    contracts: [...uniqueContracts.values()],
    referenceScripts: Object.values(manifest.contracts).flatMap((entry) =>
      entry.refScriptUTxO === null
        ? []
        : [
            {
              outRef: outRefLabel(entry.refScriptUTxO),
              scriptHash: entry.scriptHash,
            },
          ],
    ),
  });
  const collateral = workflowTransactionCollateralInputOutRefs(boundary.signed);
  const walletAddress = await callerLucid.wallet().address();
  expect(authority.rewardAddress).not.toBe(walletAddress);
  const signedOutputs = boundary.signed.toTransaction().body().outputs();
  const rewardOutputs = Array.from(
    { length: signedOutputs.len() },
    (_, index) => signedOutputs.get(index),
  ).filter(
    (output) => output.address().to_bech32() === authority.rewardAddress,
  );
  expect(rewardOutputs).toHaveLength(1);
  expect(rewardOutputs[0]!.amount().coin().toString()).toBe(
    authority.rewardLovelace,
  );
  expect(
    Array.from({ length: signedOutputs.len() }, (_, index) =>
      signedOutputs.get(index),
    ).some((output) => output.address().to_bech32() === walletAddress),
  ).toBe(false);
  const inputs = (await callerLucid.wallet().getUtxos())
    .map((utxo) => {
      expect(utxo.address).toBe(walletAddress);
      expect(Object.keys(utxo.assets)).toEqual(["lovelace"]);
      expect(utxo.datum).toBeUndefined();
      expect(utxo.scriptRef).toBeUndefined();
      return {
        outRef: outRefLabel(utxo),
        role: collateral.includes(outRefLabel(utxo))
          ? ("collateral" as const)
          : ("funding" as const),
        lovelace: utxo.assets.lovelace!.toString(),
        assets: [],
      };
    })
    .sort((a, b) => a.outRef.localeCompare(b.outRef, "en"));
  expect(inputs.some((input) => input.role === "funding")).toBe(true);
  expect(
    inputs
      .filter((input) => input.role === "collateral")
      .map((input) => input.outRef),
  ).toEqual([...collateral].sort());
  const basis = {
    walletAddress,
    callerKeyHash,
    inputs,
    decisionDigest: decision.decisionDigest,
  };
  const plan = {
    schemaVersion: WATCHER_PROVER_FUNDING_RESERVATION_PLAN,
    deploymentFingerprint: manifest.manifestId,
    decisionDigest: decision.decisionDigest,
    policyDigest: readWorkflowRuntimeFundingPolicy(policy).policyDigest,
    reservationBasisDigest: computeDeploymentManifestJsonDigest(basis),
    fundingPaymentKeyHash: callerKeyHash,
    walletAddress,
    inputs,
    fundingLovelace: inputs
      .filter((input) => input.role === "funding")
      .reduce((sum, input) => sum + BigInt(input.lovelace), 0n)
      .toString(),
    collateralLovelace: inputs
      .filter((input) => input.role === "collateral")
      .reduce((sum, input) => sum + BigInt(input.lovelace), 0n)
      .toString(),
    assets: [],
    reservationId: computeDeploymentManifestJsonDigest({
      basis,
      manifestId: manifest.manifestId,
    }),
  };
  const directory = await mkdtemp(
    join(
      process.env.MIDGARD_PERMISSIONLESS_SQLITE_TEST_DIRECTORY ?? process.cwd(),
      ".permissionless-slash-",
    ),
  );
  const path = join(directory, "watcher.sqlite");
  let opened: Awaited<ReturnType<typeof openStore>> | undefined;
  try {
    opened = await openStore(path);
    let store = opened.runtime.store;
    await store.reserve(plan);
    const { port, record, resolveCallCount } =
      await createPermissionlessSlashSqlitePort({
        readStore: () => store,
        plan,
        callerLucid,
        contracts: uniqueContracts,
      });
    const bind = async () =>
      bindWorkflowFundingReservationJournal({
        journal: {},
        permit: await createWorkflowFundingReservationPermit({
          category: "doubleSpend",
          runner,
          policy,
          actuationPermit: controller.permit,
          rollbackGeneration: "0",
          port,
        }),
      });
    let journal = await bind();
    const action = {
      actionId: "remove-current-target",
      input: {
        stage: "remove",
        category: "doubleSpend",
        nextRemovalOutRef: authority.removedStateQueueOutRef,
        fraudProofOutRef: authority.fraudProofOutRef,
      },
    };
    const preflight = bindWorkflowPreflightTransaction(
      boundary,
      boundary.signed,
    );
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: manifest.manifestId,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: evidence.headerHash },
      decisionDigest: decision.decisionDigest,
    };
    const handoff: WorkflowFundingSubmissionHandoff = {
      workflowId: computeFraudProofWorkflowId(identity),
      identity,
      preparedArtifactDigest: createHash("sha256")
        .update(evidence.payloadEnvelopeCbor)
        .digest("hex"),
      expectedJournalSequence: 2,
      preflight: {
        kind: "preflight_passed",
        actionId: action.actionId,
        txHash: boundary.signed.toHash(),
        localEvaluator: LOCAL_UPLC_EVALUATOR,
        referenceScripts: boundary.referenceScripts,
      },
      submissionIntent: {
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        txHash: boundary.signed.toHash(),
        attempt: 1,
      },
    };
    await beginWorkflowFundingReservationAction({ journal, action });
    await prepareWorkflowFundingReservationTransaction({
      journal,
      action,
      preflight,
      handoff,
    });
    const pending = await record();
    expect(pending.activeInputs).toEqual(inputs);
    expect(pending.pendingTransition?.consumedOutRefs).toEqual([]);
    expect(pending.pendingTransition?.producedInputs).toEqual([]);
    expect(pending.pendingTransition?.signedTransactionCborHex).toBe(
      boundary.signed.toTransaction().to_cbor_hex(),
    );
    expect(pending.pendingTransition?.transactionBodySha256).toBe(
      createHash("sha256")
        .update(
          Buffer.from(
            boundary.signed.toTransaction().body().to_cbor_hex(),
            "hex",
          ),
        )
        .digest("hex"),
    );
    expect(
      boundary.signed.toTransaction().body().outputs().len(),
    ).toBeGreaterThan(0);
    const reopen = async () => {
      opened!.runtime.close();
      opened = undefined;
      opened = await openStore(path);
      store = opened.runtime.store;
      expect(opened.path).toBe(path);
      journal = await bind();
    };
    await reopen();
    const callsBeforeRecovery = resolveCallCount();
    const recovered = await readWorkflowFundingRecovery(journal);
    expect(resolveCallCount()).toBe(callsBeforeRecovery);
    expect(recovered.transition?.signedTransactionCborHex).toBe(
      pending.pendingTransition?.signedTransactionCborHex,
    );
    expect(recovered.transition?.transactionBodySha256).toBe(
      pending.pendingTransition?.transactionBodySha256,
    );
    expect(recovered.submissionHandoff).toEqual(handoff);
    await assertWorkflowFundingReservationReadyToSubmit({
      journal,
      transactionHash: boundary.signed.toHash(),
    });
    return {
      confirm: async () => {
        await confirmWorkflowFundingReservationTransaction({
          journal,
          transactionHash: boundary.signed.toHash(),
        });
        const confirmed = await record();
        expect(confirmed.pendingTransition).toBeNull();
        expect(confirmed.activeInputs).toEqual(inputs);
        expect(confirmed.lastConfirmedTransitionDigest).toBe(
          pending.pendingTransition?.transitionDigest,
        );
        const outputs = boundary.signed.toTransaction().body().outputs();
        const rewardAddress = authority.rewardAddress;
        const rewardIndex = Array.from(
          { length: outputs.len() },
          (_, index) => index,
        ).find(
          (index) => outputs.get(index).address().to_bech32() === rewardAddress,
        );
        if (rewardIndex === undefined)
          throw new Error("Authenticated reward output disappeared");
        const rewardOutRef = `${boundary.signed.toHash()}#${rewardIndex}`;
        const lineage = await store.readConfirmedInput({
          reservationId: plan.reservationId,
          outRef: rewardOutRef,
        });
        expect(lineage).toEqual({
          sourceActionKind: "remove",
          sourceOutputIndex: rewardIndex,
          outRef: rewardOutRef,
          resolvedOutputCborHex: outputs
            .get(rewardIndex)
            .to_canonical_cbor_hex(),
        });
        await reopen();
        expect(await record()).toEqual(confirmed);
        await confirmWorkflowFundingReservationTransaction({
          journal,
          transactionHash: boundary.signed.toHash(),
        });
        expect(await record()).toEqual(confirmed);
        expect(
          (await readWorkflowFundingRecovery(journal)).transition,
        ).toBeNull();
        expect((await record()).activeInputs).toEqual(inputs);
        expect(
          await store.readConfirmedInput({
            reservationId: plan.reservationId,
            outRef: rewardOutRef,
          }),
        ).toEqual(lineage);
        expect(
          (
            await callerLucid.utxosByOutRef(
              inputs.map((input) => {
                const [txHash, index] = input.outRef.split("#");
                return { txHash: txHash!, outputIndex: Number(index) };
              }),
            )
          )
            .map(outRefLabel)
            .sort(),
        ).toEqual(inputs.map((input) => input.outRef));
        await expect(
          store.reserve({
            ...plan,
            reservationId: computeDeploymentManifestJsonDigest({
              collision: plan.reservationId,
            }),
            reservationBasisDigest: computeDeploymentManifestJsonDigest({
              collision: basis,
            }),
          }),
        ).rejects.toThrow("prover funding output is already reserved");
      },
      close: async () => {
        opened?.runtime.close();
        opened = undefined;
        await rm(directory, { recursive: true, force: true });
      },
    };
  } catch (error) {
    opened?.runtime.close();
    await rm(directory, { recursive: true, force: true });
    throw error;
  }
};
