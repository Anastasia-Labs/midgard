import { createHash } from "node:crypto";
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { basename, dirname, join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type CorrectionLockDatum,
  DA_ATTESTATION_TIMEOUT_MS,
  fetchCorrectionLockUTxOProgram,
  fetchSortedStateQueueUTxOsProgram,
  getStateQueueNodeFromStateQueueDatum,
  HUB_ORACLE_ASSET_NAME,
  incompletePruneUnattestedBlockDescendantTxProgram,
  incompleteRemoveLastUnattestedBlockTxProgram,
  NO_DA_ATTESTATION,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  CML,
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
import {
  createLocalKupmiosHttpOgmiosRawSource,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
} from "./workflow/local-kupmios-http-ogmios-source.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type ReleaseL1FinalityPolicy,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./workflow/release-finality-policy.js";
import {
  inspectSignedWorkflowTransaction,
  reconcileSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./workflow/signed-transaction-reconciliation.js";

const TIMEOUT_CORRECTION_LEASE_HOLDER = "attestation_timeout_removal";

export type TimeoutCorrectionTxKind = "prune-descendant" | "remove-block";
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
  readonly signedCbor: string;
  readonly validFromSlot: string;
  readonly validToSlot: string;
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
  readonly archive?: (journal: TimeoutCorrectionJournal) => Promise<void>;
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

const transactionInputOutRefs = (inputs: CML.TransactionInputList): string[] =>
  Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  }).sort();

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
        "signedCbor",
        "validFromSlot",
        "validToSlot",
        "status",
      ])
    ) {
      throw new Error(`Timeout-correction journal step ${index} is invalid.`);
    }
    const step = rawStep as Partial<TimeoutCorrectionJournalStep>;
    if (
      (step.kind !== "prune-descendant" && step.kind !== "remove-block") ||
      !HEADER_HASH_PATTERN.test(step.removedHeaderHash ?? "") ||
      !TX_HASH_PATTERN.test(step.txHash ?? "") ||
      typeof step.signedCbor !== "string" ||
      !DECIMAL_NATURAL_PATTERN.test(step.validFromSlot ?? "") ||
      !DECIMAL_NATURAL_PATTERN.test(step.validToSlot ?? "") ||
      (step.status !== "prepared" &&
        step.status !== "submitted" &&
        step.status !== "confirmed" &&
        step.status !== "superseded") ||
      !Array.isArray(step.inputOutRefs) ||
      step.inputOutRefs.length < 3 ||
      step.inputOutRefs.some(
        (outRef) => typeof outRef !== "string" || !OUT_REF_PATTERN.test(outRef),
      ) ||
      new Set(step.inputOutRefs).size !== step.inputOutRefs.length
    ) {
      throw new Error(
        `Timeout-correction journal step ${index} has non-canonical fields.`,
      );
    }
    const inspected = inspectSignedWorkflowTransaction({
      transactionHash: step.txHash!,
      signedTransactionCborHex: step.signedCbor!,
    });
    const actualInputs = transactionInputOutRefs(inspected.body.inputs());
    if (
      inspected.validFromSlot?.toString() !== step.validFromSlot ||
      inspected.expiresAtSlot?.toString() !== step.validToSlot ||
      BigInt(step.validFromSlot!) >= BigInt(step.validToSlot!) ||
      actualInputs.join(",") !== [...step.inputOutRefs].sort().join(",")
    )
      throw new Error(
        "Timeout-correction signed bytes disagree with journal inputs or validity.",
      );
    if (seenTxHashes.has(step.txHash!)) {
      throw new Error(
        `Timeout-correction journal repeats transaction hash ${step.txHash}.`,
      );
    }
    seenTxHashes.add(step.txHash!);
    if (
      (step.kind === "remove-block") !==
      (step.removedHeaderHash === candidate.targetHeaderHash)
    ) {
      throw new Error(
        `Timeout-correction journal step ${index} does not match the target-removal topology.`,
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
  | "not_found"
  | "expired"
  | "invalidated"
  | "unknown";

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
  archive: async (journal) => {
    const bytes = `${JSON.stringify(journal, null, 2)}\n`;
    const digest = createHash("sha256").update(bytes).digest("hex");
    const archivePath = `${journalPath}.archive-${digest}.json`;
    await mkdir(dirname(journalPath), { recursive: true });
    try {
      await writeFile(archivePath, bytes, {
        encoding: "utf8",
        mode: 0o600,
        flag: "wx",
      });
    } catch (error) {
      if (
        !(
          typeof error === "object" &&
          error !== null &&
          "code" in error &&
          error.code === "EEXIST"
        )
      )
        throw error;
      if ((await readFile(archivePath, "utf8")) !== bytes)
        throw new Error(
          "Timeout correction archive content does not match its identity.",
        );
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

const replaceJournalStepStatus = (
  journal: TimeoutCorrectionJournal,
  stepIndex: number,
  status: "confirmed" | "superseded",
): TimeoutCorrectionJournal => {
  return {
    ...journal,
    steps: journal.steps.map(
      (step, index): TimeoutCorrectionJournalStep =>
        index === stepIndex ? { ...step, status } : step,
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
  const stepIndex = journal.steps.findIndex(
    (step) => step.status === "prepared" || step.status === "submitted",
  );
  const lastStep = journal.steps[stepIndex];
  if (
    lastStep === undefined ||
    (lastStep.status !== "prepared" && lastStep.status !== "submitted")
  ) {
    return { disposition: "none", journal };
  }
  if (
    transactionStatus === "pending" ||
    transactionStatus === "unknown" ||
    transactionStatus === "not_found" ||
    transactionStatus === "failed"
  ) {
    return { disposition: "pending", journal };
  }
  if (transactionStatus === "expired" || transactionStatus === "invalidated") {
    return {
      disposition: "superseded",
      journal: replaceJournalStepStatus(journal, stepIndex, "superseded"),
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
    journal: replaceJournalStepStatus(journal, stepIndex, "confirmed"),
  };
};

export type TimeoutCorrectionPlan = {
  readonly kind: TimeoutCorrectionTxKind;
  readonly predecessor: StateQueueUTxO;
  readonly target: StateQueueUTxO;
  readonly removed: StateQueueUTxO;
  readonly inputOutRefs: readonly string[];
};

/** Locate the authenticated target without discarding its unaffected prefix. */
export const planNextTimeoutCorrection = (
  queue: readonly StateQueueUTxO[],
  targetHeaderHash: string,
): TimeoutCorrectionPlan | undefined => {
  if (queue[0]?.datum.key !== "Empty")
    throw new Error(
      "Canonical state queue is missing its confirmed-state root.",
    );
  const index = queue.findIndex(
    (node, i) => i > 0 && headerHashOf(node) === targetHeaderHash,
  );
  if (index < 0) return undefined;
  const predecessor = queue[index - 1]!;
  const target = queue[index]!;
  if (
    predecessor.datum.next === "Empty" ||
    predecessor.datum.next.Key.key !== targetHeaderHash
  )
    throw new Error(
      "Timeout target is not linked from its authenticated predecessor.",
    );
  const descendant = queue[index + 1];
  if (descendant !== undefined) {
    if (
      target.datum.next === "Empty" ||
      target.datum.next.Key.key !== headerHashOf(descendant)
    )
      throw new Error(
        "Timeout target does not link to its immediate descendant.",
      );
    return {
      kind: "prune-descendant",
      predecessor,
      target,
      removed: descendant,
      inputOutRefs: outRefsOf([target, descendant]),
    };
  }
  if (target.datum.next !== "Empty")
    throw new Error("Terminal timeout target retains a descendant link.");
  return {
    kind: "remove-block",
    predecessor,
    target,
    removed: target,
    inputOutRefs: outRefsOf([predecessor, target]),
  };
};

/** Resume the on-chain correction before considering a different expired block. */
export const selectTimeoutCorrectionTarget = async (
  queue: readonly StateQueueUTxO[],
  nowMs: bigint,
  lock: CorrectionLockDatum,
): Promise<{ target: StateQueueUTxO; deadline: bigint } | undefined> => {
  const lockedTarget =
    lock === "Idle" ? undefined : lock.Locked.target_header_hash;
  if (
    lock !== "Idle" &&
    lock.Locked.correction_identity !== "AttestationTimeout"
  )
    throw new Error(
      "State-queue correction lock is owned by another correction kind.",
    );
  let waiting: { target: StateQueueUTxO; deadline: bigint } | undefined;
  for (const target of queue.slice(1)) {
    if (lockedTarget !== undefined && headerHashOf(target) !== lockedTarget)
      continue;
    const node = await Effect.runPromise(
      getStateQueueNodeFromStateQueueDatum(target.datum),
    );
    if (node.da_attestation !== NO_DA_ATTESTATION) {
      if (lockedTarget !== undefined)
        throw new Error("Locked timeout target is already attested.");
      continue;
    }
    const deadline = node.header.endTime + DA_ATTESTATION_TIMEOUT_MS;
    if (deadline <= nowMs) return { target, deadline };
    if (lockedTarget !== undefined)
      throw new Error("Locked timeout target has not reached its deadline.");
    waiting ??= { target, deadline };
  }
  if (lockedTarget !== undefined)
    throw new Error(
      "Locked timeout target is absent from the canonical queue.",
    );
  return waiting;
};

/** Reopen reverted effects without replacing their retained signed attempts. */
export const reopenRolledBackTimeoutCorrectionSteps = (
  journal: TimeoutCorrectionJournal,
  queue: readonly StateQueueUTxO[],
): TimeoutCorrectionJournal => {
  const liveInputs = new Set(outRefsOf(queue));
  const liveHeaders = new Set(queue.slice(1).map(headerHashOf));
  let reopened = false;
  const steps = journal.steps.map((step) => {
    if (
      step.status !== "confirmed" ||
      (!liveHeaders.has(step.removedHeaderHash) &&
        !step.inputOutRefs.some((input) => liveInputs.has(input)))
    )
      return step;
    reopened = true;
    return { ...step, status: "prepared" as const };
  });
  return reopened ? { ...journal, completed: false, steps } : journal;
};

/** A rollback reopens the objective; it does not retire previously signed attempts. */
export const reconcileCompletedTimeoutCorrectionJournal = (
  journal: TimeoutCorrectionJournal,
  queue: readonly StateQueueUTxO[],
): TimeoutCorrectionJournal | undefined => {
  const reconciled = reopenRolledBackTimeoutCorrectionSteps(journal, queue);
  if (!reconciled.completed) return reconciled;
  if (
    queue.some(
      (node, index) =>
        index > 0 && headerHashOf(node) === journal.targetHeaderHash,
    )
  )
    return { ...reconciled, completed: false };
  return queue.length === 1 ? reconciled : undefined;
};

const hasCompetingCorrection = (
  lock: CorrectionLockDatum,
  target?: string,
): boolean =>
  lock !== "Idle" &&
  (lock.Locked.correction_identity !== "AttestationTimeout" ||
    (target !== undefined && lock.Locked.target_header_hash !== target));

const pendingTimeoutCorrection = (
  journal?: TimeoutCorrectionJournal,
): SubmitUnattestedTimeoutCorrectionResult => ({
  status: "pending",
  targetHeaderHash: journal?.targetHeaderHash ?? null,
  deadlineMs: journal?.targetDeadlineMs ?? null,
  pendingTxHash:
    journal?.steps.find(
      (step) => step.status === "prepared" || step.status === "submitted",
    )?.txHash ?? null,
  submittedTxHashes: journal?.steps.map((step) => step.txHash) ?? [],
  removedHeaderHashes:
    journal?.steps
      .filter((step) => step.status === "confirmed")
      .map((step) => step.removedHeaderHash) ?? [],
});

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

export type TimeoutCorrectionRecovery = Readonly<{
  observeSignedTransaction(
    input: SignedWorkflowTransaction,
  ): Promise<SignedTransactionRecoveryObservation>;
  rebroadcastSignedTransaction(
    input: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ): Promise<string>;
}>;

/** Same admitted, canonical signed-attempt recovery used by proof workflows. */
export const createLocalKupmiosTimeoutCorrectionRecovery = (input: {
  readonly deploymentManifest: unknown;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly network: Network;
}): TimeoutCorrectionRecovery => {
  const manifest = verifyFinalizedDeploymentManifest(input.deploymentManifest);
  if (manifest.network !== input.network)
    throw new Error(
      "Timeout recovery network differs from its finalized deployment.",
    );
  const policy = manifest.l1Finality as ReleaseL1FinalityPolicy;
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifest.manifestId as string,
    blueprintHash: (manifest.artifacts as { blueprintHash: string })
      .blueprintHash,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  });
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: `attestation-timeout:${releaseFinality.deploymentIdentityDigest}`,
    kupoHttpUrl: input.kupoUrl,
    ogmiosUrl: input.ogmiosUrl,
    releaseFinality,
    observationDepth: "inclusion",
  });
  return {
    observeSignedTransaction: (signed) =>
      readAdmittedLocalKupmiosSignedTransactionRecovery({ source, ...signed }),
    rebroadcastSignedTransaction: (signed) =>
      rebroadcastAdmittedLocalKupmiosSignedTransaction({ source, ...signed }),
  };
};

/** Bare not_found/failed is ambiguous; only admitted canonical observations retire attempts. */
export const recoverTimeoutCorrectionAttempt = async (input: {
  readonly journal: TimeoutCorrectionJournal;
  readonly queue: readonly StateQueueUTxO[];
  readonly transactionStatus: TimeoutCorrectionTransactionStatus;
  readonly allowRebroadcast?: boolean;
  readonly recovery?: TimeoutCorrectionRecovery;
  readonly authorizeResubmission: (
    signed: SignedWorkflowTransaction,
  ) => Promise<void>;
}): Promise<TimeoutCorrectionStepReconciliation> => {
  const step = input.journal.steps.find(
    (entry) => entry.status === "prepared" || entry.status === "submitted",
  );
  if (step === undefined)
    return { disposition: "none", journal: input.journal };
  let canonical: SignedTransactionRecoveryObservation | undefined;
  const result = await reconcileSignedWorkflowTransaction({
    transactionHash: step.txHash,
    signedTransactionCborHex: step.signedCbor,
    observe:
      input.recovery === undefined
        ? undefined
        : async (signed) => {
            canonical = await input.recovery!.observeSignedTransaction(signed);
            return canonical;
          },
    rebroadcast:
      input.allowRebroadcast === false
        ? undefined
        : input.recovery?.rebroadcastSignedTransaction,
    authorizeResubmission: input.authorizeResubmission,
  });
  if (result.kind === "conflict")
    throw new Error(`Timeout correction canonical conflict: ${result.reason}`);
  const status =
    canonical?.status === "included"
      ? "confirmed"
      : result.kind === "not_found" &&
          (canonical?.status === "expired" ||
            canonical?.status === "invalidated")
        ? canonical.status
        : input.recovery === undefined &&
            input.transactionStatus === "confirmed"
          ? "confirmed"
          : "unknown";
  return reconcileLastTimeoutCorrectionStep(input.journal, input.queue, status);
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
  readonly recovery?: TimeoutCorrectionRecovery;
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
  recovery,
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
  const initialLock = await loadCorrectionLock();
  let journal = await journalStore.load();
  if (hasCompetingCorrection(initialLock.datum, journal?.targetHeaderHash)) {
    // Another timeout actor may have stopped after acquiring the on-chain lock.
    // Retire or confirm our old attempts before adopting that authenticated target.
    if (
      initialLock.datum === "Idle" ||
      initialLock.datum.Locked.correction_identity !== "AttestationTimeout" ||
      journal === undefined
    )
      return pendingTimeoutCorrection(journal);
    journal = reopenRolledBackTimeoutCorrectionSteps(journal, queue);
    await journalStore.save(journal);
    while (
      journal.steps.some(
        (step) => step.status === "prepared" || step.status === "submitted",
      )
    ) {
      const reconciled = await recoverTimeoutCorrectionAttempt({
        journal,
        queue,
        transactionStatus: "unknown",
        recovery,
        allowRebroadcast: false,
        authorizeResubmission: async () => {
          throw new Error("A competing correction owns the lock.");
        },
      });
      if (reconciled.disposition === "pending")
        return pendingTimeoutCorrection(journal);
      journal = reconciled.journal;
      await journalStore.save(journal);
      queue = await loadQueue();
    }
    if (journalStore.archive === undefined)
      return pendingTimeoutCorrection(journal);
    await journalStore.archive(journal);
    journal = undefined;
  }
  if (journal?.completed === true) {
    journal = reconcileCompletedTimeoutCorrectionJournal(journal, queue);
    if (journal?.completed === true) {
      if (initialLock.datum !== "Idle")
        throw new Error(
          "Completed timeout journal conflicts with an active correction lock.",
        );
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
    if (journal !== undefined) await journalStore.save(journal);
  }
  if (journal === undefined) {
    const selected = await selectTimeoutCorrectionTarget(
      queue,
      BigInt(nowMs()),
      initialLock.datum,
    );
    if (selected === undefined)
      return {
        status: "empty",
        targetHeaderHash: null,
        deadlineMs: null,
        pendingTxHash: null,
        submittedTxHashes: [],
        removedHeaderHashes: [],
      };
    if (BigInt(nowMs()) < selected.deadline)
      return {
        status: "not-ready",
        targetHeaderHash: headerHashOf(selected.target),
        deadlineMs: selected.deadline.toString(),
        pendingTxHash: null,
        submittedTxHashes: [],
        removedHeaderHashes: [],
      };
    journal = {
      version: 1,
      targetHeaderHash: headerHashOf(selected.target),
      targetDeadlineMs: selected.deadline.toString(),
      steps: [],
      completed: false,
    };
    await journalStore.save(journal);
  }

  const lease = await stateQueueMutationLeaseCoordinator?.acquire();
  let leaseReleased = false;
  try {
    while (true) {
      queue = await loadQueue();
      const reopened = reopenRolledBackTimeoutCorrectionSteps(journal, queue);
      if (reopened !== journal) {
        journal = reopened;
        await journalStore.save(journal);
      }
      if (
        hasCompetingCorrection(
          (await loadCorrectionLock()).datum,
          journal.targetHeaderHash,
        )
      ) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return pendingTimeoutCorrection(journal);
      }
      const lastStep = journal.steps.find(
        (step) => step.status === "prepared" || step.status === "submitted",
      );
      if (lastStep !== undefined) {
        const txStatus = await lucid
          .transactionStatus(lastStep.txHash)
          .catch(() => ({ status: "not_found" as const }));
        const activeTargetHeaderHash = journal.targetHeaderHash;
        const reconciliation = await recoverTimeoutCorrectionAttempt({
          journal,
          queue,
          transactionStatus: txStatus.status,
          recovery,
          authorizeResubmission: async (signed) => {
            const retained = await journalStore.load();
            const pending = retained?.steps.find(
              (step) =>
                step.status === "prepared" || step.status === "submitted",
            );
            if (
              retained?.targetHeaderHash !== activeTargetHeaderHash ||
              pending?.txHash !== signed.transactionHash ||
              pending.signedCbor !== signed.signedTransactionCborHex
            )
              throw new Error(
                "Timeout rebroadcast no longer matches the retained active attempt.",
              );
            const currentQueue = await loadQueue();
            const currentPlan = planNextTimeoutCorrection(
              currentQueue,
              activeTargetHeaderHash,
            );
            const lock = await loadCorrectionLock();
            if (
              currentPlan === undefined ||
              currentPlan.removed.datum.key === "Empty" ||
              headerHashOf(currentPlan.removed) !== pending.removedHeaderHash ||
              (lock.datum !== "Idle" &&
                (lock.datum.Locked.correction_identity !==
                  "AttestationTimeout" ||
                  lock.datum.Locked.target_header_hash !==
                    activeTargetHeaderHash))
            )
              throw new Error(
                "Timeout rebroadcast requires the same live target, descendant and correction owner.",
              );
          },
        });
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
          await new Promise((resolve) =>
            setTimeout(resolve, DEFAULT_CONFIRMATION_POLL_MS),
          );
          continue;
        }
        await journalStore.save(journal);
        // A rollback can reopen several prior signed attempts. Reconcile every
        // one before creating any replacement transaction.
        if (
          journal.steps.some(
            (step) => step.status === "prepared" || step.status === "submitted",
          )
        )
          continue;
        queue = await loadQueue();
      }

      const plan = planNextTimeoutCorrection(queue, journal.targetHeaderHash);
      if (plan === undefined) {
        const correctionLock = await loadCorrectionLock();
        if (correctionLock.datum !== "Idle") {
          leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
          return pendingTimeoutCorrection(journal);
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
      const targetNode = await Effect.runPromise(
        getStateQueueNodeFromStateQueueDatum(plan.target.datum),
      );
      if (
        targetNode.da_attestation !== NO_DA_ATTESTATION ||
        targetNode.header.endTime + DA_ATTESTATION_TIMEOUT_MS !== deadline
      )
        throw new Error(
          "Timeout target attestation or immutable deadline changed before signing.",
        );
      lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
      const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
      const correctionLockInput = await loadCorrectionLock();
      if (
        hasCompetingCorrection(
          correctionLockInput.datum,
          journal.targetHeaderHash,
        )
      ) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return pendingTimeoutCorrection(journal);
      }
      const common = {
        timedOutBlockUTxO: plan.target,
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
          ? incompletePruneUnattestedBlockDescendantTxProgram(
              lucid,
              stateQueueConfig,
              {
                ...common,
                predecessorRefInput: plan.predecessor,
                removedDescendantUTxO: plan.removed,
              },
            )
          : incompleteRemoveLastUnattestedBlockTxProgram(
              lucid,
              stateQueueConfig,
              {
                ...common,
                predecessorUTxO: plan.predecessor,
              },
            );
      const unsigned = await tx
        .addSignerKey(signer.paymentKeyHash)
        .complete({ localUPLCEval: true });
      const signed = await unsigned.sign.withWallet().complete();
      const txHash = signed.toHash();
      const signedCbor = signed.toCBOR();
      const inspected = inspectSignedWorkflowTransaction({
        transactionHash: txHash,
        signedTransactionCborHex: signedCbor,
      });
      if (
        inspected.validFromSlot === undefined ||
        inspected.expiresAtSlot === undefined
      )
        throw new Error("Timeout correction requires bounded signed validity.");
      const step: TimeoutCorrectionJournalStep = {
        kind: plan.kind,
        removedHeaderHash: headerHashOf(plan.removed),
        inputOutRefs: transactionInputOutRefs(inspected.body.inputs()),
        txHash,
        signedCbor,
        validFromSlot: inspected.validFromSlot.toString(),
        validToSlot: inspected.expiresAtSlot.toString(),
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
      let submittedTxHash: string;
      try {
        submittedTxHash = await signed.submit();
      } catch {
        // Submission may have reached the node. The next iteration observes
        // these exact retained bytes before authorizing any replacement.
        if (awaitConfirmation) continue;
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
      // Canonical signed-byte recovery and queue observation confirm on the next iteration.
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
  const kupoUrl = config.kupoUrl ?? process.env.L1_KUPO_KEY;
  const ogmiosUrl = config.ogmiosUrl ?? process.env.L1_OGMIOS_KEY;
  if (
    (config.provider ?? process.env.L1_PROVIDER) !== "Kupmios" ||
    kupoUrl === undefined ||
    ogmiosUrl === undefined
  )
    throw new Error(
      "Timeout-correction recovery requires configured local Kupmios.",
    );
  const recovery = createLocalKupmiosTimeoutCorrectionRecovery({
    deploymentManifest: deploymentInfo,
    kupoUrl,
    ogmiosUrl,
    network: config.network,
  });
  return submitUnattestedTimeoutCorrection({
    lucid,
    deploymentInfo,
    recovery,
    network: config.network,
    signer: resolveProverSigner(config),
    journalStore: createFileTimeoutCorrectionJournalStore(config.journalPath),
    awaitConfirmation: config.awaitConfirmation,
    stateQueueMutationLeaseCoordinator,
  });
};
