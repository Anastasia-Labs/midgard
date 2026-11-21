/**
 * The cancel of one of our computation threads with no header in hand: one
 * operation per registered family, derived from the family application
 * registry's roster. A thread is an output at one of its family's step
 * scripts holding one token `policy ++ categoryId ++ headerHash`; its
 * step's Cancel arm needs only that output, the burn and our signature, so
 * the thread's out-ref and the deployment's published scripts are all the
 * cancel reads. The registry names the category's scripts by role; the
 * manifest's script hashes say which role the thread's address is.
 */
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk/core";
import {
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { buildLinearFaultCancel } from "../linear-fault-cancel.js";
import { parseOutRef, type ResolvedProverSigner } from "../runtime.js";
import { PAST_THE_OPENER } from "../validation-trace-dispute/own-thread-reclaim.js";
import { VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS } from "../validation-trace-dispute/workflow.js";
import type { FamilyReferenceScriptResolver } from "./family-application.apply-family-application-record.js";
import { FAMILY_APPLICATION_REGISTRY } from "./family-application-registry.family-application-registry.js";
import type { InstalledFraudProofCategory } from "./installed-categories.js";

/** The roster role of the computation-thread minting policy. */
const COMPUTATION_THREAD_MINT_ROLE = "computationThreadMint";

/**
 * The roles of a family whose step scripts are not all cancellable: only
 * these roles' scripts have a Cancel arm, and `otherwise` says why a thread
 * at any other role is not ours to cancel. A family absent here cancels at
 * every role.
 */
const CANCELLABLE_ROLES: Readonly<
  Partial<
    Record<
      InstalledFraudProofCategory,
      Readonly<{ roles: readonly string[]; otherwise: string }>
    >
  >
> = Object.freeze({
  validationTraceDispute: Object.freeze({
    roles: Object.freeze([
      "opener",
    ] satisfies readonly (keyof typeof VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS)[]),
    otherwise: PAST_THE_OPENER,
  }),
});

/** What the deployment offers a header-free cancel. */
export type OwnThreadCancelDeployment = Readonly<{
  /** The verified manifest contracts' script hashes, by contract name. */
  contractScriptHashes: Readonly<Record<string, string>>;
  resolveReferenceScript: FamilyReferenceScriptResolver;
}>;

/** A thread of ours as the chain holds it, before any cancel is built. */
export type OwnThreadCancelSubject = Readonly<{
  threadOutRef: string;
  /** The four-byte catalogue id the thread token names. */
  categoryId: string;
  /** The payment script hash of the thread's address. */
  paymentScriptHash: string;
}>;

/**
 * How a thread of ours can be cancelled: at `role` of `category`'s roster,
 * not at all (`uncancellable`, the chain offers no cancel there), or not by
 * this deployment (`unavailable`, nothing here can name its script).
 */
export type OwnThreadCancelPlan =
  | Readonly<{
      kind: "cancellable";
      category: InstalledFraudProofCategory;
      role: string;
      contractName: string;
    }>
  | Readonly<{
      kind: "uncancellable";
      category: InstalledFraudProofCategory;
      reason: string;
    }>
  | Readonly<{ kind: "unavailable"; reason: string }>;

const REGISTERED_CATEGORIES = Object.keys(
  FAMILY_APPLICATION_REGISTRY,
) as InstalledFraudProofCategory[];

const CATEGORY_BY_ID: ReadonlyMap<string, InstalledFraudProofCategory> =
  new Map(
    REGISTERED_CATEGORIES.map((category) => [
      FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category],
      category,
    ]),
  );

/** The registered family a thread token's catalogue id names. */
export const ownThreadCategory = (
  categoryId: string,
): InstalledFraudProofCategory | undefined => CATEGORY_BY_ID.get(categoryId);

/**
 * Which role of its family's roster a thread of ours stands at, and whether
 * that role's script cancels it. Reads nothing: the roster, the manifest's
 * script hashes and the thread's address decide.
 */
export const planOwnThreadCancel = (
  subject: Omit<OwnThreadCancelSubject, "threadOutRef">,
  contractScriptHashes: OwnThreadCancelDeployment["contractScriptHashes"],
): OwnThreadCancelPlan => {
  const category = ownThreadCategory(subject.categoryId);
  if (category === undefined)
    return Object.freeze({
      kind: "unavailable",
      reason: `no registered family has catalogue id ${subject.categoryId}`,
    });
  const roster: Readonly<Record<string, string>> =
    FAMILY_APPLICATION_REGISTRY[category].roster;
  const at = Object.entries(roster).find(
    ([role, contractName]) =>
      role !== COMPUTATION_THREAD_MINT_ROLE &&
      contractScriptHashes[contractName] === subject.paymentScriptHash,
  );
  if (at === undefined)
    return Object.freeze({
      kind: "unavailable",
      reason: `script ${subject.paymentScriptHash} is no ${category} roster script`,
    });
  const [role, contractName] = at;
  const restricted = CANCELLABLE_ROLES[category];
  if (restricted !== undefined && !restricted.roles.includes(role))
    return Object.freeze({
      kind: "uncancellable",
      category,
      reason: restricted.otherwise,
    });
  if (roster[COMPUTATION_THREAD_MINT_ROLE] === undefined)
    return Object.freeze({
      kind: "unavailable",
      reason: `the ${category} roster names no computation-thread minting script`,
    });
  return Object.freeze({ kind: "cancellable", category, role, contractName });
};

const referenceScriptOf = (utxo: UTxO, label: string): Script => {
  if (utxo.scriptRef == null)
    throw new Error(`${label} reference carries no script`);
  return utxo.scriptRef;
};

/**
 * Builds, evaluates and signs the cancel `plan` names for the thread at
 * `threadOutRef`, without submitting it: the thread's value back to the
 * signer's wallet, less the fee, which `signer`'s wallet pays. The step and
 * minting scripts come from the deployment's published references; the
 * thread's own token fixes the minting policy, so a reference that is not
 * the thread's policy refuses.
 */
export const buildOwnThreadCancel = async ({
  lucid,
  signer,
  deployment,
  plan,
  subject,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: OwnThreadCancelDeployment;
  readonly plan: Extract<OwnThreadCancelPlan, { kind: "cancellable" }>;
  readonly subject: OwnThreadCancelSubject;
  /** The cancel's upper validity bound (POSIX ms). */
  readonly validTo: number;
}): Promise<Awaited<ReturnType<typeof buildLinearFaultCancel>>> => {
  const { category, role, contractName } = plan;
  const roster: Readonly<Record<string, string>> =
    FAMILY_APPLICATION_REGISTRY[category].roster;
  const [stepReference, mintReference] = await Promise.all([
    deployment.resolveReferenceScript({ category, role, contractName }),
    deployment.resolveReferenceScript({
      category,
      role: COMPUTATION_THREAD_MINT_ROLE,
      contractName: roster[COMPUTATION_THREAD_MINT_ROLE]!,
    }),
  ]);
  const spendingScript = referenceScriptOf(
    stepReference,
    `${category} ${role}`,
  );
  const mintingScript = referenceScriptOf(
    mintReference,
    `${category} computation-thread mint`,
  );
  const threadUtxo = (
    await lucid.utxosByOutRef([parseOutRef(subject.threadOutRef, "thread")])
  )[0];
  if (threadUtxo === undefined)
    throw new Error(`${category} thread ${subject.threadOutRef} is spent`);
  return await buildLinearFaultCancel({
    lucid,
    family: category,
    steps: [
      {
        spendingScript,
        spendingScriptHash: subject.paymentScriptHash,
        spendingScriptAddress: threadUtxo.address,
      },
    ],
    computationThread: {
      policyId: validatorToScriptHash(mintingScript),
      mintingScript,
    },
    categoryId: subject.categoryId,
    signer,
    threadOutRef: subject.threadOutRef,
    referenceScriptUtxo: stepReference,
    witnessReferenceScripts: { computationThreadMint: mintReference },
    validTo,
  });
};

/** A cancel built and signed, or why none is. */
export type OwnThreadCancelBuild =
  | Readonly<{
      kind: "built";
      cancel: Awaited<ReturnType<typeof buildOwnThreadCancel>>;
    }>
  | Exclude<OwnThreadCancelPlan, { kind: "cancellable" }>;

/** One family's header-free cancel of a thread of ours. */
export type CancelOwnThread = (
  input: Omit<Parameters<typeof buildOwnThreadCancel>[0], "plan">,
) => Promise<OwnThreadCancelBuild>;

const cancelOwnThreadOf =
  (category: InstalledFraudProofCategory): CancelOwnThread =>
  async (input) => {
    const plan = planOwnThreadCancel(
      input.subject,
      input.deployment.contractScriptHashes,
    );
    if (plan.kind !== "cancellable") return plan;
    if (plan.category !== category)
      return Object.freeze({
        kind: "unavailable",
        reason: `catalogue id ${input.subject.categoryId} names ${plan.category}, not ${category}`,
      });
    return Object.freeze({
      kind: "built",
      cancel: await buildOwnThreadCancel({ ...input, plan }),
    });
  };

/**
 * One header-free cancel per registered family, keyed by catalogue
 * category: each plans the thread against its own roster and builds the
 * cancel that plan names.
 */
export const FAMILY_OWN_THREAD_CANCELS: Readonly<
  Record<InstalledFraudProofCategory, CancelOwnThread>
> = Object.freeze(
  Object.fromEntries(
    REGISTERED_CATEGORIES.map((category) => [
      category,
      cancelOwnThreadOf(category),
    ]),
  ) as Record<InstalledFraudProofCategory, CancelOwnThread>,
);
