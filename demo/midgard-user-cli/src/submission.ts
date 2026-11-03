/**
 * The part of `deposit` and `withdraw` after the request is known: check the
 * record agrees, check Kupo indexes what the run reads, open the user's view,
 * read the minting reference script by its exact outref, write the intent,
 * then hand the run to the SDK with the file store.
 */
import { join } from "node:path";

import {
  eventHistoryProtectionWaitBoundMs,
  USER_SUBMISSION_VALIDITY_DURATION_MS,
  type UserHistoryContracts,
  type UserL1View,
  type UserSubmissionError,
  type UserSubmissionOptions,
  type UserSubmissionOutcome,
  type UserSubmissionPending,
} from "@al-ft/midgard-sdk";
import { type OutRef, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import { type UserCliDeps } from "./deps.js";
import { type L1Access, requireKupoCoverage } from "./l1-view.js";
import { type UserDeployment } from "./manifest.js";
import { parsePositiveMs, refuse } from "./options.js";
import {
  createIntent,
  fileSubmissionStore,
  type IntentRecord,
  readIntent,
  recordDirectory,
  requireSameIntent,
  type SubmissionKind,
} from "./record-store.js";

/** Options every command takes. */
export type CommonOptions = {
  readonly submissionId: string;
  readonly manifest?: string;
  readonly l1?: string;
  readonly kupoUrl?: string;
  readonly ogmiosUrl?: string;
  readonly blockfrostUrl?: string;
  readonly blockfrostProjectId?: string;
  readonly recordDir?: string;
  readonly json?: boolean;
};

export type SubmitOptions = CommonOptions & {
  readonly walletSeedPhraseEnv?: string;
  readonly walletSeedPhraseFile?: string;
  readonly timeoutMs?: string;
};

export const manifestPathOf = (
  options: CommonOptions,
  deps: UserCliDeps,
): string =>
  options.manifest ??
  deps.env.MIDGARD_DEPLOYMENT_MANIFEST ??
  refuse("--manifest (or MIDGARD_DEPLOYMENT_MANIFEST) is required.");

export const recordRootOf = (
  options: CommonOptions,
  deps: UserCliDeps,
): string =>
  options.recordDir ??
  deps.env.MIDGARD_USER_RECORD_DIR ??
  join(deps.homeDir, ".midgard", "user-submissions");

export type PreparedRequest<Config> = {
  readonly kind: SubmissionKind;
  readonly submissionId: string;
  readonly deployment: UserDeployment;
  readonly access: L1Access;
  readonly seedPhrase: string;
  readonly walletAddress: string;
  readonly intentHash: string;
  readonly intent: Readonly<Record<string, unknown>>;
  readonly recordDirectory: string;
  /** The SDK config, given the minting reference script. */
  readonly config: (referenceScript: UTxO) => Config;
};

export const recordDirectoryOf = (
  options: CommonOptions,
  deps: UserCliDeps,
  manifestId: string,
  kind: SubmissionKind,
  submissionId: string,
): string =>
  recordDirectory(recordRootOf(options, deps), manifestId, kind, submissionId);

/** The intent already on disk for this id, refused unless it is this one. */
export const existingIntent = async (
  directory: string,
  wanted: Omit<IntentRecord, "version" | "hubPolicyId" | "intent">,
): Promise<IntentRecord | undefined> => {
  const existing = await readIntent(directory);
  if (existing !== undefined) requireSameIntent(existing, wanted);
  return existing;
};

const readReferenceScript = async (
  view: UserL1View,
  outRef: OutRef,
  role: string,
): Promise<UTxO> => {
  const [utxo] = await view.lucid.utxosByOutRef([outRef]);
  return utxo !== undefined && utxo.scriptRef != null
    ? utxo
    : refuse(
        `The ${role} reference script ${outRef.txHash}#${outRef.outputIndex.toString()} is not an unspent output with a script on L1.`,
      );
};

export type RunSubmission<Config> = (
  view: UserL1View,
  contracts: UserHistoryContracts,
  config: Config,
  options: UserSubmissionOptions,
) => Effect.Effect<
  UserSubmissionOutcome,
  UserSubmissionError | UserSubmissionPending
>;

export const runSubmission = async <Config>(
  options: SubmitOptions,
  deps: UserCliDeps,
  request: PreparedRequest<Config>,
  program: RunSubmission<Config>,
): Promise<UserSubmissionOutcome> => {
  const { deployment, kind } = request;
  const history =
    kind === "Deposit"
      ? deployment.contracts.deposit
      : deployment.contracts.withdrawal;
  const pair =
    deployment.contracts.eventHistory ??
    refuse("The deployment manifest has no event-history contracts.");
  const recipe =
    kind === "Deposit" ? pair.deposit.recipe : pair.withdrawal.recipe;
  const timeoutMs =
    options.timeoutMs === undefined
      ? 2 * USER_SUBMISSION_VALIDITY_DURATION_MS +
        eventHistoryProtectionWaitBoundMs(recipe)
      : parsePositiveMs(options.timeoutMs, "--timeout-ms");
  if (request.access.kind === "kupmios")
    await requireKupoCoverage(deps.fetch, request.access.kupoUrl, [
      { role: "wallet address", address: request.walletAddress },
      {
        role: `${kind.toLowerCase()} list address`,
        address: history.spendingScriptAddress,
      },
    ]);
  const view = await deps.openL1View({
    access: request.access,
    network: deployment.network,
    slotConfig: deployment.slotConfig,
    seedPhrase: request.seedPhrase,
  });
  const referenceScript = await readReferenceScript(
    view,
    kind === "Deposit"
      ? deployment.referenceScripts.depositMinting
      : deployment.referenceScripts.withdrawalMinting,
    `${kind.toLowerCase()} minting`,
  );
  const wanted = {
    version: 1 as const,
    kind,
    submissionId: request.submissionId,
    manifestId: deployment.manifestId,
    hubPolicyId: deployment.contracts.hubOracle.policyId,
    walletAddress: request.walletAddress,
    intentHash: request.intentHash,
    intent: request.intent,
  };
  // A concurrent first run may have won the create; it must be this request.
  requireSameIntent(
    await createIntent(request.recordDirectory, wanted),
    wanted,
  );
  const result = await Effect.runPromise(
    Effect.either(
      program(view, deployment.contracts, request.config(referenceScript), {
        store: fileSubmissionStore(request.recordDirectory),
        deadlineMs: deps.now() + timeoutMs,
        now: deps.now,
        sleep: deps.sleep,
        log: (line) => deps.stderr(`${line}\n`),
      }),
    ),
  );
  if (Either.isLeft(result)) throw result.left;
  return result.right;
};
