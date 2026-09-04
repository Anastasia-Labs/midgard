import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { basename, dirname, join } from "node:path";

import {
  DA_ATTESTATION_TIMEOUT_MS,
  fetchCorrectionLockUTxOProgram,
  fetchSortedStateQueueUTxOsProgram,
  getStateQueueNodeFromStateQueueDatum,
  HUB_ORACLE_ASSET_NAME,
  incompletePruneTimedOutBlockDescendantTxProgram,
  incompleteRemoveUnattestedHeadAfterTimeoutTxProgram,
  NO_DA_ATTESTATION,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
} from "./inspect-contracts.js";
import {
  createHttpStateQueueMutationLeaseCoordinator,
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  type StateQueueMutationLeaseCoordinator,
} from "./remove-fraudulent-block.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  makeLucidForSubmit,
  outRefLabel,
  type ProverSignerConfig,
  readJsonFile,
  requireDeploymentReferenceScript,
  requireDeploymentScriptHash,
  requireMatchingScriptHash,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { selectFeeInput } from "./submit-step-01.js";

const TIMEOUT_CORRECTION_LEASE_HOLDER = "attestation_timeout_removal";

export type TimeoutCorrectionTxKind = "prune-descendant" | "remove-head";
export type TimeoutCorrectionTxStatus =
  | "prepared"
  | "submitted"
  | "confirmed"
  | "superseded";

export type TimeoutCorrectionJournalStep = {
  readonly kind: TimeoutCorrectionTxKind;
  readonly removedHeaderHash: string;
  readonly inputOutRefs: readonly string[];
  readonly txHash: string;
  readonly status: TimeoutCorrectionTxStatus;
};

export type TimeoutCorrectionJournal = {
  readonly version: 1;
  readonly targetHeaderHash: string;
  readonly targetDeadlineMs: string;
  readonly steps: readonly TimeoutCorrectionJournalStep[];
  readonly completed: boolean;
};

export interface TimeoutCorrectionJournalStore {
  readonly load: () => Promise<TimeoutCorrectionJournal | undefined>;
  readonly save: (journal: TimeoutCorrectionJournal) => Promise<void>;
}

const HEADER_HASH_PATTERN = /^[0-9a-f]{56}$/;
const TX_HASH_PATTERN = /^[0-9a-f]{64}$/;
const OUT_REF_PATTERN = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/;
const DECIMAL_NATURAL_PATTERN = /^(?:0|[1-9][0-9]*)$/;

const hasExactKeys = (
  value: object,
  expectedKeys: readonly string[],
): boolean => {
  const actualKeys = Object.keys(value).sort();
  const canonicalExpectedKeys = [...expectedKeys].sort();
  return (
    actualKeys.length === canonicalExpectedKeys.length &&
    actualKeys.every((key, index) => key === canonicalExpectedKeys[index])
  );
};

export const parseTimeoutCorrectionJournal = (
  value: unknown,
): TimeoutCorrectionJournal => {
  if (
    typeof value !== "object" ||
    value === null ||
    !hasExactKeys(value, [
      "version",
      "targetHeaderHash",
      "targetDeadlineMs",
      "steps",
      "completed",
    ]) ||
    (value as { version?: unknown }).version !== 1 ||
    typeof (value as { targetHeaderHash?: unknown }).targetHeaderHash !==
      "string" ||
    typeof (value as { targetDeadlineMs?: unknown }).targetDeadlineMs !==
      "string" ||
    !Array.isArray((value as { steps?: unknown }).steps) ||
    typeof (value as { completed?: unknown }).completed !== "boolean"
  ) {
    throw new Error("Invalid attestation-timeout correction journal V1.");
  }
  const candidate = value as {
    readonly targetHeaderHash: string;
    readonly targetDeadlineMs: string;
    readonly steps: readonly unknown[];
    readonly completed: boolean;
  };
  if (
    !HEADER_HASH_PATTERN.test(candidate.targetHeaderHash) ||
    !DECIMAL_NATURAL_PATTERN.test(candidate.targetDeadlineMs)
  ) {
    throw new Error(
      "Timeout-correction journal target hash/deadline is non-canonical.",
    );
  }
  const seenTxHashes = new Set<string>();
  const steps = candidate.steps.map((rawStep, index) => {
    if (
      typeof rawStep !== "object" ||
      rawStep === null ||
      !hasExactKeys(rawStep, [
        "kind",
        "removedHeaderHash",
        "inputOutRefs",
        "txHash",
        "status",
      ])
    ) {
      throw new Error(`Timeout-correction journal step ${index} is invalid.`);
    }
    const step = rawStep as Partial<TimeoutCorrectionJournalStep>;
    if (
      (step.kind !== "prune-descendant" && step.kind !== "remove-head") ||
      !HEADER_HASH_PATTERN.test(step.removedHeaderHash ?? "") ||
      !TX_HASH_PATTERN.test(step.txHash ?? "") ||
      (step.status !== "prepared" &&
        step.status !== "submitted" &&
        step.status !== "confirmed" &&
        step.status !== "superseded") ||
      !Array.isArray(step.inputOutRefs) ||
      step.inputOutRefs.length !== 3 ||
      step.inputOutRefs.some(
        (outRef) => typeof outRef !== "string" || !OUT_REF_PATTERN.test(outRef),
      ) ||
      new Set(step.inputOutRefs).size !== step.inputOutRefs.length
    ) {
      throw new Error(
        `Timeout-correction journal step ${index} has non-canonical fields.`,
      );
    }
    if (seenTxHashes.has(step.txHash!)) {
      throw new Error(
        `Timeout-correction journal repeats transaction hash ${step.txHash}.`,
      );
    }
    seenTxHashes.add(step.txHash!);
    if (
      (step.kind === "remove-head") !==
      (step.removedHeaderHash === candidate.targetHeaderHash)
    ) {
      throw new Error(
        `Timeout-correction journal step ${index} does not match the target-removal topology.`,
      );
    }
    if (
      index < candidate.steps.length - 1 &&
      (step.status === "prepared" || step.status === "submitted")
    ) {
      throw new Error(
        "Only the final timeout-correction journal step may be non-terminal.",
      );
    }
    return step as TimeoutCorrectionJournalStep;
  });
  if (
    candidate.completed &&
    steps.some(
      (step) => step.status === "prepared" || step.status === "submitted",
    )
  ) {
    throw new Error(
      "Completed timeout-correction journal has a non-terminal transaction.",
    );
  }
  return {
    version: 1,
    targetHeaderHash: candidate.targetHeaderHash,
    targetDeadlineMs: candidate.targetDeadlineMs,
    steps,
    completed: candidate.completed,
  };
};

export type TimeoutCorrectionTransactionStatus =
  | "pending"
  | "confirmed"
  | "failed"
  | "not_found";

export type TimeoutCorrectionStepReconciliation = {
  readonly disposition: "none" | "pending" | "confirmed" | "superseded";
  readonly journal: TimeoutCorrectionJournal;
};

/** Durable single-file journal. Rename makes each state transition atomic. */
export const createFileTimeoutCorrectionJournalStore = (
  journalPath: string,
): TimeoutCorrectionJournalStore => ({
  load: async () => {
    try {
      return parseTimeoutCorrectionJournal(
        JSON.parse(await readFile(journalPath, "utf8")),
      );
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return undefined;
      }
      throw error;
    }
  },
  save: async (journal) => {
    const directory = dirname(journalPath);
    await mkdir(directory, { recursive: true });
    const temporaryPath = join(
      directory,
      `.${basename(journalPath)}.${process.pid.toString()}.tmp`,
    );
    await writeFile(temporaryPath, `${JSON.stringify(journal, null, 2)}\n`, {
      encoding: "utf8",
      mode: 0o600,
    });
    await rename(temporaryPath, journalPath);
  },
});

const headerHashOf = (node: StateQueueUTxO): string => {
  if (node.datum.key === "Empty") {
    throw new Error("Confirmed-state root does not have a block header hash.");
  }
  const headerHash = node.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (node.datum.key.Key.key !== headerHash) {
    throw new Error("State-queue node key does not match its NFT asset name.");
  }
  return headerHash;
};

const outRefsOf = (nodes: readonly StateQueueUTxO[]): readonly string[] =>
  nodes.map((node) => outRefLabel(node.utxo)).sort();

const replaceLastJournalStepStatus = (
  journal: TimeoutCorrectionJournal,
  status: "confirmed" | "superseded",
): TimeoutCorrectionJournal => {
  const lastStepIndex = journal.steps.length - 1;
  return {
    ...journal,
    steps: journal.steps.map(
      (step, index): TimeoutCorrectionJournalStep =>
        index === lastStepIndex ? { ...step, status } : step,
    ),
  };
};

/**
 * Reconcile the sole recoverable transaction intent against both its
 * provider-authenticated status and the freshly authenticated queue. A
 * confirmed transaction is not journal-confirmed until its exact spent
 * outrefs and removed header have disappeared from the canonical queue.
 */
export const reconcileLastTimeoutCorrectionStep = (
  journal: TimeoutCorrectionJournal,
  queue: readonly StateQueueUTxO[],
  transactionStatus: TimeoutCorrectionTransactionStatus,
): TimeoutCorrectionStepReconciliation => {
  const lastStep = journal.steps[journal.steps.length - 1];
  if (
    lastStep === undefined ||
    (lastStep.status !== "prepared" && lastStep.status !== "submitted")
  ) {
    return { disposition: "none", journal };
  }
  if (transactionStatus === "pending") {
    return { disposition: "pending", journal };
  }
  if (transactionStatus === "failed" || transactionStatus === "not_found") {
    return {
      disposition: "superseded",
      journal: replaceLastJournalStepStatus(journal, "superseded"),
    };
  }

  const currentOutRefs = new Set(outRefsOf(queue));
  const recordedInputsAreSpent = lastStep.inputOutRefs.every(
    (outRef) => !currentOutRefs.has(outRef),
  );
  const removedHeaderIsAbsent = !queue.some(
    (node, index) =>
      index > 0 && headerHashOf(node) === lastStep.removedHeaderHash,
  );
  if (!recordedInputsAreSpent || !removedHeaderIsAbsent) {
    return { disposition: "pending", journal };
  }
  return {
    disposition: "confirmed",
    journal: replaceLastJournalStepStatus(journal, "confirmed"),
  };
};

export type TimeoutCorrectionPlan = {
  readonly kind: TimeoutCorrectionTxKind;
  readonly root: StateQueueUTxO;
  readonly head: StateQueueUTxO;
  readonly removed: StateQueueUTxO;
  readonly inputOutRefs: readonly string[];
};

/** Pure topology decision used both by the command and stale/race tests. */
export const planNextTimeoutCorrection = (
  queue: readonly StateQueueUTxO[],
  targetHeaderHash: string,
): TimeoutCorrectionPlan | undefined => {
  const root = queue[0];
  const head = queue[1];
  if (root === undefined || root.datum.key !== "Empty") {
    throw new Error(
      "Canonical state queue is missing its confirmed-state root.",
    );
  }
  if (head === undefined || headerHashOf(head) !== targetHeaderHash) {
    return undefined;
  }
  const descendant = queue[2];
  if (descendant !== undefined) {
    return {
      kind: "prune-descendant",
      root,
      head,
      removed: descendant,
      inputOutRefs: outRefsOf([head, descendant]),
    };
  }
  return {
    kind: "remove-head",
    root,
    head,
    removed: head,
    inputOutRefs: outRefsOf([root, head]),
  };
};

/**
 * A completed journal is never completion authority. Reconcile it against the
 * authenticated queue: reopen on rollback, rotate only after target absence,
 * and fail if a restored target is no longer the head it originally was.
 */
export const reconcileCompletedTimeoutCorrectionJournal = (
  journal: TimeoutCorrectionJournal,
  queue: readonly StateQueueUTxO[],
): TimeoutCorrectionJournal | undefined => {
  if (!journal.completed) {
    return journal;
  }
  const targetIndex = queue.findIndex(
    (node, index) =>
      index > 0 && headerHashOf(node) === journal.targetHeaderHash,
  );
  if (targetIndex === 1) {
    return {
      ...journal,
      completed: false,
      steps: journal.steps.map((step) => ({
        ...step,
        status: "superseded" as const,
      })),
    };
  }
  if (targetIndex > 1) {
    throw new Error(
      `Completed timeout-correction target ${journal.targetHeaderHash} reappeared outside the canonical head position.`,
    );
  }
  return queue[1] === undefined ? journal : undefined;
};

export const releaseTimeoutCorrectionLeaseBeforeYield = async (
  lease: { readonly release: () => Promise<void> } | undefined,
): Promise<boolean> => {
  if (lease === undefined) {
    return false;
  }
  await lease.release();
  return true;
};

const requireDeploymentScript = (
  deploymentInfo: ContractDeploymentInfo,
  name:
    | "correctionLockSpend"
    | "stateQueueSpend"
    | "stateQueueMint"
    | "stateQueueUnattestedTimeoutWithdraw",
): Script => {
  const entry = deploymentInfo[name];
  if (entry?.contract === undefined) {
    throw new Error(
      `Deployment info entry "${name}" is missing contract CBOR.`,
    );
  }
  const script = {
    type: entry.contract.type,
    script: entry.contract.cborHex,
  } as Script;
  requireMatchingScriptHash({
    label: `${name} script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(script),
  });
  return script;
};

export type SubmitUnattestedTimeoutCorrectionParams = {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly journalStore: TimeoutCorrectionJournalStore;
  readonly awaitConfirmation?: boolean;
  readonly nowMs?: () => number;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
};

export type SubmitUnattestedTimeoutCorrectionResult = {
  readonly status: "empty" | "not-ready" | "pending" | "complete";
  readonly targetHeaderHash: string | null;
  readonly deadlineMs: string | null;
  readonly pendingTxHash: string | null;
  readonly submittedTxHashes: readonly string[];
  readonly removedHeaderHashes: readonly string[];
};

export const submitUnattestedTimeoutCorrection = async ({
  lucid,
  deploymentInfo: rawDeploymentInfo,
  network,
  signer,
  journalStore,
  awaitConfirmation = true,
  nowMs = Date.now,
  stateQueueMutationLeaseCoordinator,
}: SubmitUnattestedTimeoutCorrectionParams): Promise<SubmitUnattestedTimeoutCorrectionResult> => {
  signer.selectWallet(lucid);
  const deploymentInfo = parseContractDeploymentInfo(rawDeploymentInfo);
  const stateQueuePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "stateQueueMint",
  );
  const stateQueueSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueSpend",
  );
  const stateQueueMintingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueMint",
  );
  const stateQueueUnattestedTimeoutWithdrawalScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueUnattestedTimeoutWithdraw",
  );
  const correctionLockSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "correctionLockSpend",
  );
  const hubOraclePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "hubOracleMint",
  );
  const stateQueueAddress = validatorToAddress(
    network,
    stateQueueSpendingScript,
  );
  const stateQueueConfig = { stateQueueAddress, stateQueuePolicyId };
  const [
    correctionLockSpendRef,
    stateQueueSpendRef,
    stateQueueMintRef,
    stateQueueUnattestedTimeoutWithdrawRef,
  ] = await Promise.all([
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "correctionLockSpend",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "stateQueueSpend",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "stateQueueMint",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "stateQueueUnattestedTimeoutWithdraw",
    }),
  ]);
  const referenceScripts = {
    correctionLockSpend: correctionLockSpendRef,
    stateQueueSpend: stateQueueSpendRef,
    stateQueueMint: stateQueueMintRef,
  };
  const hubOracleRefInput = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      network,
      scriptHashToCredential(hubOraclePolicyId),
    ),
    unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "hub oracle",
  });
  const loadCorrectionLock = () =>
    Effect.runPromise(
      fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: validatorToAddress(
          network,
          correctionLockSpendingScript,
        ),
        hubOraclePolicyId,
      }),
    );
  const loadQueue = () =>
    Effect.runPromise(
      fetchSortedStateQueueUTxOsProgram(lucid, stateQueueConfig),
    );

  let queue = await loadQueue();
  const initialHead = queue[1];
  let journal = await journalStore.load();
  if (journal?.completed === true) {
    journal = reconcileCompletedTimeoutCorrectionJournal(journal, queue);
    if (journal?.completed === true) {
      const correctionLock = await loadCorrectionLock();
      if (correctionLock.datum !== "Idle") {
        throw new Error(
          "Completed timeout-correction journal is inconsistent with a locked on-chain correction singleton.",
        );
      }
      return {
        status: "complete",
        targetHeaderHash: journal.targetHeaderHash,
        deadlineMs: journal.targetDeadlineMs,
        pendingTxHash: null,
        submittedTxHashes: journal.steps.map((step) => step.txHash),
        removedHeaderHashes: journal.steps
          .filter((step) => step.status === "confirmed")
          .map((step) => step.removedHeaderHash),
      };
    }
  }
  if (journal === undefined) {
    if (initialHead === undefined) {
      return {
        status: "empty",
        targetHeaderHash: null,
        deadlineMs: null,
        pendingTxHash: null,
        submittedTxHashes: [],
        removedHeaderHashes: [],
      };
    }
    const headNode = await Effect.runPromise(
      getStateQueueNodeFromStateQueueDatum(initialHead.datum),
    );
    if (headNode.da_attestation !== NO_DA_ATTESTATION) {
      return {
        status: "empty",
        targetHeaderHash: null,
        deadlineMs: null,
        pendingTxHash: null,
        submittedTxHashes: [],
        removedHeaderHashes: [],
      };
    }
    const deadline = headNode.header.endTime + DA_ATTESTATION_TIMEOUT_MS;
    if (BigInt(nowMs()) < deadline) {
      return {
        status: "not-ready",
        targetHeaderHash: headerHashOf(initialHead),
        deadlineMs: deadline.toString(),
        pendingTxHash: null,
        submittedTxHashes: [],
        removedHeaderHashes: [],
      };
    }
    journal = {
      version: 1,
      targetHeaderHash: headerHashOf(initialHead),
      targetDeadlineMs: deadline.toString(),
      steps: [],
      completed: false,
    };
    await journalStore.save(journal);
  }
  if (journal === undefined) {
    throw new Error("Failed to initialize timeout-correction journal.");
  }

  const lease = await stateQueueMutationLeaseCoordinator?.acquire();
  let leaseReleased = false;
  try {
    while (true) {
      queue = await loadQueue();
      const lastStep = journal.steps[journal.steps.length - 1];
      if (
        lastStep !== undefined &&
        (lastStep.status === "prepared" || lastStep.status === "submitted")
      ) {
        const txStatus = await lucid.transactionStatus(lastStep.txHash);
        const reconciliation = reconcileLastTimeoutCorrectionStep(
          journal,
          queue,
          txStatus.status,
        );
        journal = reconciliation.journal;
        if (reconciliation.disposition === "pending") {
          if (!awaitConfirmation) {
            leaseReleased =
              await releaseTimeoutCorrectionLeaseBeforeYield(lease);
            return {
              status: "pending",
              targetHeaderHash: journal.targetHeaderHash,
              deadlineMs: journal.targetDeadlineMs,
              pendingTxHash: lastStep.txHash,
              submittedTxHashes: journal.steps.map((step) => step.txHash),
              removedHeaderHashes: journal.steps
                .filter((step) => step.status === "confirmed")
                .map((step) => step.removedHeaderHash),
            };
          }
          if (txStatus.status === "pending") {
            await lucid.awaitTx(lastStep.txHash, DEFAULT_CONFIRMATION_POLL_MS);
          } else {
            // Transaction status can lead the provider's UTxO view. Do not
            // journal confirmation until the authenticated queue catches up.
            await new Promise((resolve) =>
              setTimeout(resolve, DEFAULT_CONFIRMATION_POLL_MS),
            );
          }
          continue;
        }
        await journalStore.save(journal);
      }

      const plan = planNextTimeoutCorrection(queue, journal.targetHeaderHash);
      if (plan === undefined) {
        const correctionLock = await loadCorrectionLock();
        if (correctionLock.datum !== "Idle") {
          throw new Error(
            "Timeout correction cannot complete while the on-chain correction singleton remains locked.",
          );
        }
        journal = { ...journal, completed: true };
        await journalStore.save(journal);
        await lease?.release();
        leaseReleased = true;
        return {
          status: "complete",
          targetHeaderHash: journal.targetHeaderHash,
          deadlineMs: journal.targetDeadlineMs,
          pendingTxHash: null,
          submittedTxHashes: journal.steps.map((step) => step.txHash),
          removedHeaderHashes: journal.steps
            .filter((step) => step.status === "confirmed")
            .map((step) => step.removedHeaderHash),
        };
      }

      const deadline = BigInt(journal.targetDeadlineMs);
      const backdated =
        BigInt(nowMs()) - STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;
      const validFrom = backdated > deadline ? backdated : deadline;
      const validTo = validFrom + STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS;
      const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
      const correctionLockInput = await loadCorrectionLock();
      const common = {
        timedOutHeadUTxO: plan.head,
        additionalInputs: [feeInput],
        hubOracleRefInput,
        correctionLockInput,
        correctionLockSpendingScript,
        validFrom,
        validTo,
        stateQueueSpendingScript,
        stateQueueMintingScript,
        referenceScripts,
        yieldWitness: {
          referenceInput: stateQueueUnattestedTimeoutWithdrawRef,
          script: stateQueueUnattestedTimeoutWithdrawalScript,
        },
      } as const;
      const tx =
        plan.kind === "prune-descendant"
          ? incompletePruneTimedOutBlockDescendantTxProgram(
              lucid,
              stateQueueConfig,
              {
                ...common,
                confirmedStateRefInput: plan.root,
                removedDescendantUTxO: plan.removed,
              },
            )
          : incompleteRemoveUnattestedHeadAfterTimeoutTxProgram(
              lucid,
              stateQueueConfig,
              {
                ...common,
                confirmedStateUTxO: plan.root,
              },
            );
      const unsigned = await tx
        .addSignerKey(signer.paymentKeyHash)
        .complete({ localUPLCEval: true });
      const signed = await unsigned.sign.withWallet().complete();
      const txHash = signed.toHash();
      const step: TimeoutCorrectionJournalStep = {
        kind: plan.kind,
        removedHeaderHash: headerHashOf(plan.removed),
        inputOutRefs: [
          ...plan.inputOutRefs,
          outRefLabel(correctionLockInput.utxo),
        ],
        txHash,
        status: "prepared",
      };
      const sameTxIndex = journal.steps.findIndex(
        (entry) => entry.txHash === txHash,
      );
      if (
        sameTxIndex >= 0 &&
        journal.steps[sameTxIndex]?.status !== "superseded"
      ) {
        throw new Error(
          `Timeout-correction transaction hash ${txHash} conflicts with non-superseded journal state.`,
        );
      }
      journal = {
        ...journal,
        steps:
          sameTxIndex < 0
            ? [...journal.steps, step]
            : [
                ...journal.steps.filter((_, index) => index !== sameTxIndex),
                step,
              ],
      };
      await journalStore.save(journal);
      const submittedTxHash = await signed.submit();
      if (submittedTxHash !== txHash) {
        throw new Error(
          `Provider returned transaction hash ${submittedTxHash}, expected ${txHash}.`,
        );
      }
      const submittedStepIndex: number = journal.steps.length - 1;
      journal = {
        ...journal,
        steps: journal.steps.map(
          (entry, index): TimeoutCorrectionJournalStep =>
            index === submittedStepIndex
              ? { ...entry, status: "submitted" }
              : entry,
        ),
      };
      await journalStore.save(journal);
      await lease?.renew();
      if (!awaitConfirmation) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return {
          status: "pending",
          targetHeaderHash: journal.targetHeaderHash,
          deadlineMs: journal.targetDeadlineMs,
          pendingTxHash: txHash,
          submittedTxHashes: journal.steps.map((entry) => entry.txHash),
          removedHeaderHashes: journal.steps
            .filter((entry) => entry.status === "confirmed")
            .map((entry) => entry.removedHeaderHash),
        };
      }
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    }
  } catch (error) {
    if (lease !== undefined && !leaseReleased) {
      await lease.fail(error instanceof Error ? error.message : String(error));
    }
    throw error;
  }
};

export type RemoveUnattestedBlockCliConfig = SubmitProviderConfig &
  ProverSignerConfig & {
    readonly deploymentInfoPath: string;
    readonly journalPath: string;
    readonly awaitConfirmation?: boolean;
    readonly midgardNodeUrl?: string;
    readonly midgardNodeAdminKey?: string;
    readonly stateQueueLeaseTtlMs?: number;
  };

export const submitUnattestedTimeoutCorrectionFromFiles = async (
  config: RemoveUnattestedBlockCliConfig,
): Promise<SubmitUnattestedTimeoutCorrectionResult> => {
  const [lucid, deploymentInfo] = await Promise.all([
    makeLucidForSubmit(config),
    readJsonFile(config.deploymentInfoPath),
  ]);
  const stateQueueMutationLeaseCoordinator =
    config.midgardNodeUrl === undefined
      ? undefined
      : createHttpStateQueueMutationLeaseCoordinator({
          midgardNodeUrl: config.midgardNodeUrl,
          adminKey:
            config.midgardNodeAdminKey ??
            (() => {
              throw new Error(
                "midgardNodeAdminKey is required when midgardNodeUrl is configured.",
              );
            })(),
          ttlMs: config.stateQueueLeaseTtlMs,
          holder: TIMEOUT_CORRECTION_LEASE_HOLDER,
        });
  return submitUnattestedTimeoutCorrection({
    lucid,
    deploymentInfo,
    network: config.network,
    signer: resolveProverSigner(config),
    journalStore: createFileTimeoutCorrectionJournalStore(config.journalPath),
    awaitConfirmation: config.awaitConfirmation,
    stateQueueMutationLeaseCoordinator,
  });
};
