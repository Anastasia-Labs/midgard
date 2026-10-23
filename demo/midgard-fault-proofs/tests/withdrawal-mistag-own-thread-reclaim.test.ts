/** Another prover convicts a withdrawal-mistag header and removes it while
 * a computation thread of ours still stands on it. Our run, over the real
 * emulator chain, cancels that thread back to our wallet (the thread's
 * lovelace less the cancel's fee), waits out rollback recovery, and only
 * then ends resolved by the other record: no conviction of ours, no
 * terminal. A restart between the cancel's submission and its confirmation
 * resumes the same cancel without building or sending another. */
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { resolveProverSigner } from "../src/index.js";
import type { ResolvedProverSigner } from "../src/runtime.js";
import {
  createWithdrawalMistagTransactionPort,
  withdrawalMistagEvidenceRequirement,
} from "../src/withdrawal-mistag/workflow.js";
import { WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { WITHDRAWAL_MISTAG_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../src/workflow/family-l1-observation.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
} from "../src/workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflow,
} from "../src/workflow/orchestrator.js";
import { createLinearCancelReclaimPort } from "../src/workflow/own-thread-reclaim.linear-cancel-port.js";
import { withOwnThreadReclaim } from "../src/workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  withRawDatumPreimagePrerequisite,
} from "../src/workflow/raw-datum-preimage-prerequisite.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  observeEmulatorSubmissions,
  recordCrossBlockRawEmulator,
} from "./support/cross-block-raw-emulator.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/emulator/setup-tx.js";
import { network } from "./support/submit-init-emulator-shared.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";
import {
  makeWithdrawalMistagEmulatorHarness,
  publishWithdrawalMistagScripts,
} from "./support/withdrawal-mistag-emulator.js";
import { withdrawalMistagRetainedFixture } from "./support/withdrawal-mistag-retained.js";
import {
  DEPLOYMENT_FINGERPRINT,
  releaseFinalityAuthority,
  terminalVerifier,
} from "./workflow.make-adapter.js";

const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const FINAL_DEPTH = finalityPolicy.automaticRecoveryMaxDepth + 2;
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;
const ROLES = [
  "computation_thread_step_01",
  "computation_thread_step_02",
  "computation_thread_step_03",
  "computation_thread_step_04",
  "computation_thread_step_05",
] as const;
const DATUM_SCHEMAS = [
  SDK.FraudProofComputationThreadStepDatum,
  SDK.WithdrawalMistagStep02Datum,
  SDK.WithdrawalMistagStep03Datum,
  SDK.WithdrawalMistagStep04Datum,
  SDK.WithdrawalMistagStep05Datum,
];

const kinds = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.map(({ event }) => event.kind);

const walletLovelace = async (lucid: LucidEvolution) =>
  (await lucid.wallet().getUtxos()).reduce(
    (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
    0n,
  );

/**
 * A withdrawal-mistag header on the emulator, its scripts published, and a
 * workflow runner per prover over the recorded raw L1. `reclaiming` puts
 * the reclaim of our own threads in front of that prover's family adapter,
 * as a deployment wires it.
 */
const scene = async () => {
  const recorder = recordCrossBlockRawEmulator();
  const h = await makeWithdrawalMistagEmulatorHarness();
  const operator = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const fixture = await withdrawalMistagRetainedFixture({
    direction: "valid-marked-invalid",
    operatorVkey: operator.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120_000,
      ) - 1,
  });
  await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: fixture.predecessor.header,
  });
  const committed = await submitSecondHeaderTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    header: fixture.evidence.header,
  });
  const headerHash = committed.headerHash;
  const steps = (await publishWithdrawalMistagScripts({ harness: h })).refs;
  const removal = await publishRemovalReferenceScripts({
    lucid: h.proverLucid,
    contracts: h.contracts,
  });
  // Another prover with its own wallet and key, funded from ours.
  const theirSigner = resolveProverSigner({
    network,
    walletSeedPhrase: generateEmulatorAccount({ lovelace: 0n }).seedPhrase,
  });
  expect(theirSigner.paymentKeyHash).not.toBe(h.proverSigner.paymentKeyHash);
  const funding = await h.proverLucid
    .newTx()
    .pay.ToAddress(theirSigner.address, { lovelace: 4_000_000_000n })
    .pay.ToAddress(theirSigner.address, { lovelace: 1_000_000_000n })
    .complete({ localUPLCEval: true });
  await h.proverLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const theirLucid = await Lucid(h.emulator, "Custom");
  theirSigner.selectWallet(theirLucid);
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => h.emulator.now());
  h.emulator.awaitBlock(DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth);
  const binding = {
    definition: { headerHash },
    blueprint: h.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
      removalReferenceScripts: removal.published,
    }),
    network: "Custom",
    resolvedContracts: { category: h.category },
    catalogue: {
      policyId: h.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        h.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: h.catalogue.root,
    },
    releaseFinality: {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
      policy: finalityPolicy,
    },
    releaseEconomics: {
      schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
      policy: economicsPolicy,
    },
  };
  const leaseCoordinator = {
    acquire: async () => ({
      token: "withdrawal-mistag-lease",
      source: "emulator",
      renew: async () => {},
      release: async () => {},
      fail: async () => {},
    }),
  };
  const decision = await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
    fixture.evidence,
    fixture.context,
  );
  const l1For = (signer: ResolvedProverSigner) =>
    createFraudProofFamilyRawL1ObservationPort({
      authority: recorder.authority,
      releaseFinality: binding.releaseFinality,
      releaseEconomics: binding.releaseEconomics,
      definition: {
        category: "withdrawalMistag",
        categoryId: h.category.categoryId,
        headerHash,
        proverCredential: signer.paymentKeyHash,
        stateQueue: {
          policyId: h.contracts.stateQueue.policyId,
          address: h.contracts.stateQueue.spendingScriptAddress,
        },
        computationThread: {
          policyId: h.withdrawalMistag.computationThread.policyId,
          steps: h.withdrawalMistag.steps.map((step, index) => ({
            role: ROLES[index]!,
            address: step.spendingScriptAddress,
            datumSchema: DATUM_SCHEMAS[index]!,
          })),
        },
        proofToken: {
          policyId: h.withdrawalMistag.fraudProof.policyId,
          address: h.withdrawalMistag.fraudProof.spendingScriptAddress,
        },
        operatorDirectory: {
          activePolicyId: h.contracts.activeOperators.policyId,
          activeAddress: h.contracts.activeOperators.spendingScriptAddress,
          retiredPolicyId: h.contracts.retiredOperators.policyId,
          retiredAddress: h.contracts.retiredOperators.spendingScriptAddress,
        },
        schedulerAddress: h.contracts.scheduler.spendingScriptAddress,
      },
    });
  /** One prover's process: a fresh adapter per run over the journal in
   * `directory`, which a restarted process passes back in. */
  const runner = async ({
    lucid,
    signer,
    reclaiming,
    journalDirectory,
  }: {
    readonly lucid: LucidEvolution;
    readonly signer: ResolvedProverSigner;
    readonly reclaiming: boolean;
    readonly journalDirectory?: string;
  }) => {
    const directory =
      journalDirectory ??
      (await mkdtemp(join(tmpdir(), "withdrawal-mistag-reclaim-journal-")));
    const l1 = l1For(signer);
    const run = async (): Promise<FraudProofWorkflowRunResult> => {
      const prerequisite = () =>
        createAuthenticatedRawDatumPreimagePrerequisitePort({
          category: "withdrawalMistag",
          lucid,
          network: "Custom",
          signer,
          publications: l1.publications,
          requirementForAction: withdrawalMistagEvidenceRequirement,
        });
      const base = withRawDatumPreimagePrerequisite({
        category: "withdrawalMistag",
        prerequisite: prerequisite(),
        base: createCursorFamilyWorkflowAdapter({
          spec: WITHDRAWAL_MISTAG_CURSOR_SPEC,
          l1,
          transactions: createWithdrawalMistagTransactionPort({
            evidencePrerequisite: prerequisite(),
            binding: binding as never,
            lucid,
            signer,
            contracts: h.withdrawalMistag,
            references: {
              steps,
              witnesses: h.witnessReferenceScripts as Required<
                typeof h.witnessReferenceScripts
              >,
            },
            replayContext: fixture.context,
            stateQueueMutationLeaseCoordinator: leaseCoordinator,
          }),
          stateQueueMutationLeaseCoordinator: leaseCoordinator,
        }),
      });
      const adapter = reclaiming
        ? withOwnThreadReclaim({
            category: "withdrawalMistag",
            base,
            releaseFinality: binding.releaseFinality,
            reclaim: createLinearCancelReclaimPort({
              family: "withdrawalMistag",
              lucid,
              signer,
              categoryId: h.category.categoryId,
              contracts: () => h.withdrawalMistag,
              stepReferences: steps,
              witnessReferenceScripts: h.witnessReferenceScripts,
              l1: {
                observeOwnThreads: async (input) =>
                  await l1.observeOwnThreads!(input),
                observeSignedTransaction: async () => {
                  throw new Error(
                    "no signed bytes are recorded without a funding reservation",
                  );
                },
              },
            }),
          })
        : base;
      return await runFraudProofWorkflow({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        evidence: fixture.evidence,
        detections: decision.detections,
        replayContext: fixture.context,
        registry: createFraudProofWorkflowRegistry({
          adapters: [adapter],
          launchScope: ["withdrawalMistag"],
        }),
        journal: new DirectoryFraudProofWorkflowJournalStore(directory),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
        maxActions: 1,
      });
    };
    return { l1, run, directory };
  };
  return {
    h,
    headerHash,
    theirLucid,
    theirSigner,
    runner,
    restore: () => {
      clock.mockRestore();
      recorder.restore();
    },
  };
};

/** Our run, until a thread of ours stands on the header. */
const openOurThread = async (
  s: Awaited<ReturnType<typeof scene>>,
  ours: Awaited<ReturnType<Awaited<ReturnType<typeof scene>>["runner"]>>,
) => {
  for (let pass = 0; pass < 12; pass += 1) {
    const threads = await ours.l1.observeOwnThreads!({
      headerHash: s.headerHash,
    });
    if (threads.standing !== undefined) return threads.standing;
    expect((await ours.run()).kind).toBe("pending");
    s.h.emulator.awaitBlock();
  }
  throw new Error("our run never opened a thread");
};

/** The other prover's run, until its record removed the header. */
const theyConvictAndRemove = async (
  s: Awaited<ReturnType<typeof scene>>,
  theirs: Awaited<ReturnType<Awaited<ReturnType<typeof scene>>["runner"]>>,
) => {
  for (let pass = 0; pass < 40; pass += 1) {
    if (
      (await theirs.l1.observe({ headerHash: s.headerHash })).stage.kind ===
      "removed"
    )
      return;
    expect((await theirs.run()).kind).toBe("pending");
    s.h.emulator.awaitBlock();
  }
  throw new Error("the other prover never removed the header");
};

const expectResolvedWithReclaim = (
  result: FraudProofWorkflowRunResult,
  thread: { readonly threadOutRef: string; readonly lovelace: string },
  reclaimTxHash: string,
) => {
  expect(result).toMatchObject({
    kind: "resolved_by_another_record",
    reclaimed: [
      {
        txHash: reclaimTxHash,
        threadOutRef: thread.threadOutRef,
        lovelace: thread.lovelace,
      },
    ],
  });
  if (result.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  // Never our conviction: no terminal, no completion, no removal of ours.
  expect("terminal" in result).toBe(false);
  expect(kinds(result.entries)).not.toContain("completed");
  expect(kinds(result.entries)).not.toContain("terminal_included");
};

describe("withdrawal-mistag: our thread on a header another prover's record removed", () => {
  it.each([{ restart: false }, { restart: true }])(
    "cancels our thread to our wallet, then resolves by the other record (restart between submit and confirm: $restart)",
    async ({ restart }) => {
      const s = await scene();
      const submissions: { txHash: string; cbor: string }[] = [];
      const observer = observeEmulatorSubmissions((txHash, cbor) =>
        submissions.push({ txHash, cbor }),
      );
      try {
        let ours = await s.runner({
          lucid: s.h.proverLucid,
          signer: s.h.proverSigner,
          reclaiming: true,
        });
        const theirs = await s.runner({
          lucid: s.theirLucid,
          signer: s.theirSigner,
          reclaiming: false,
        });
        const thread = await openOurThread(s, ours);
        await theyConvictAndRemove(s, theirs);
        const before = await walletLovelace(s.h.proverLucid);
        const submitted = submissions.length;

        // Our run reconciles what it left, then builds our thread's cancel.
        let result = await ours.run();
        for (
          let pass = 0;
          pass < 4 &&
          submissions.length === submitted &&
          result.kind === "pending";
          pass += 1
        ) {
          s.h.emulator.awaitBlock();
          result = await ours.run();
        }
        expect(result.kind).toBe("pending");
        const sent = submissions.slice(submitted);
        expect(sent).toHaveLength(1);
        const reclaim = sent[0]!;
        const body = CML.Transaction.from_cbor_hex(reclaim.cbor).body();
        const fee = body.fee();
        if (restart) {
          // A fresh process before the cancel lands: it waits on the same
          // cancel and sends nothing new.
          ours = await s.runner({
            lucid: s.h.proverLucid,
            signer: s.h.proverSigner,
            reclaiming: true,
            journalDirectory: ours.directory,
          });
          expect((await ours.run()).kind).toBe("pending");
          expect(submissions.slice(submitted)).toHaveLength(1);
        }
        s.h.emulator.awaitBlock();
        // Landed: our wallet holds the thread's lovelace less the fee.
        expect(await walletLovelace(s.h.proverLucid)).toBe(
          before + BigInt(thread.lovelace) - fee,
        );
        expect(
          (await ours.l1.observeOwnThreads!({ headerHash: s.headerHash }))
            .standing,
        ).toBeUndefined();
        // Within rollback recovery: once the cancel is reconciled (one
        // action per run), the run waits on its finality.
        result = await ours.run();
        const awaitingOurCancel = `reclaim ${reclaim.txHash} of our thread ${thread.threadOutRef} awaits release finality`;
        if (!JSON.stringify(result).includes(awaitingOurCancel))
          result = await ours.run();
        expect(result).toMatchObject({
          kind: "pending",
          reason: expect.stringContaining(awaitingOurCancel),
        });
        s.h.emulator.awaitBlock(FINAL_DEPTH);
        result = await ours.run();
        expectResolvedWithReclaim(result, thread, reclaim.txHash);
        expect(submissions.slice(submitted)).toHaveLength(1);
      } finally {
        observer.restore();
        s.restore();
      }
    },
    900_000,
  );
});
