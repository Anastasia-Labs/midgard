/** `midgard-user deposit`: one deposit, landed or resumed by submission id. */
import {
  type SubmitDepositConfig,
  submitDepositProgram,
  userSubmissionIntentHash,
  type UserSubmissionOutcome,
} from "@al-ft/midgard-sdk";

import { type UserCliDeps } from "./deps.js";
import { resolveL1Access } from "./l1-view.js";
import { loadUserDeployment } from "./manifest.js";
import {
  parseAddress,
  parseAssets,
  parseLovelace,
  parseOptionalDataHex,
  parseOptionalLovelace,
  parseSubmissionId,
} from "./options.js";
import { resolveSeedPhrase, walletOf } from "./seed.js";
import {
  existingIntent,
  manifestPathOf,
  recordDirectoryOf,
  runSubmission,
  type SubmitOptions,
} from "./submission.js";

export type DepositOptions = SubmitOptions & {
  readonly l2Address: string;
  readonly lovelace: string;
  readonly asset: readonly string[];
  readonly l2Datum?: string;
  readonly structuralLovelace?: string;
};

export const runDeposit = async (
  options: DepositOptions,
  deps: UserCliDeps,
): Promise<UserSubmissionOutcome> => {
  const submissionId = parseSubmissionId(options.submissionId);
  const lovelace = parseLovelace(
    options.lovelace,
    "--lovelace must be greater than zero.",
  );
  const additionalAssets = parseAssets(options.asset);
  const l2Datum = parseOptionalDataHex(options.l2Datum, "--l2-datum");
  const structuralLovelace = parseOptionalLovelace(
    options.structuralLovelace,
    "--structural-lovelace",
  );
  const seedPhrase = resolveSeedPhrase(
    {
      env: options.walletSeedPhraseEnv,
      file: options.walletSeedPhraseFile,
    },
    deps.env,
  );
  const deployment = await loadUserDeployment(manifestPathOf(options, deps));
  const l2Address = parseAddress(
    options.l2Address,
    "--l2-address",
    deployment.network,
  );
  const access = resolveL1Access(options, deps.env, deployment.network);
  const walletAddress = walletOf(seedPhrase, deployment.network).address;
  const intent = {
    l2Address,
    l2Datum,
    lovelace,
    additionalAssets,
    ...(structuralLovelace === undefined ? {} : { structuralLovelace }),
  };
  const intentHash = userSubmissionIntentHash("Deposit", intent);
  const recordDirectory = recordDirectoryOf(
    options,
    deps,
    deployment.manifestId,
    "Deposit",
    submissionId,
  );
  await existingIntent(recordDirectory, {
    kind: "Deposit",
    submissionId,
    manifestId: deployment.manifestId,
    walletAddress,
    intentHash,
  });
  return runSubmission(
    options,
    deps,
    {
      kind: "Deposit",
      submissionId,
      deployment,
      access,
      seedPhrase,
      walletAddress,
      intentHash,
      intent: {
        l2Address,
        l2Datum,
        lovelace: lovelace.toString(),
        assets: Object.fromEntries(
          Object.entries(additionalAssets).map(([unit, quantity]) => [
            unit,
            quantity.toString(),
          ]),
        ),
        structuralLovelace: structuralLovelace?.toString() ?? null,
      },
      recordDirectory,
      config: (depositMinting): SubmitDepositConfig => ({
        ...intent,
        referenceScripts: { depositMinting },
      }),
    },
    submitDepositProgram,
  );
};
