import { realpathSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { type ParsedArgs, usage } from "./bin.parsed-args.js";
import { parseNetwork } from "./inspect-contracts.js";
import { stringifyJson } from "./json-file.js";

/**
 * `--midgard-node-url` is still parsed for the `prepare-*` diagnostics, so a
 * removal command must refuse it explicitly rather than ignore it: removal is
 * coordinated locally and never talks to a Midgard node. A lost race against a
 * competing commit or merge fails the run, which the operator re-runs; the
 * watcher's workflow orchestrator does that retry itself.
 */
const refuseRemovalMidgardNodeUrl = (args: ParsedArgs, command: string) => {
  if (args.midgardNodeUrl !== undefined) {
    throw new Error(
      `${command} does not accept --midgard-node-url: state-queue removal is coordinated locally and never contacts a Midgard node. A removal that loses a race to a competing commit or merge fails and must be re-run.\n${usage}`,
    );
  }
};

export const buildRemoveFraudulentBlockCliConfig = (args: ParsedArgs) => {
  if (args.blueprintPath === undefined) {
    throw new Error(`Missing required --blueprint <path>.\n${usage}`);
  }
  if (args.deploymentInfoPath === undefined) {
    throw new Error(`Missing required --deployment-info <path>.\n${usage}`);
  }
  if (args.fraudulentHeaderHash === undefined) {
    throw new Error(
      `Missing required --fraudulent-header-hash <hex>.\n${usage}`,
    );
  }
  refuseRemovalMidgardNodeUrl(args, "remove-fraudulent-block");
  return {
    blueprintPath: args.blueprintPath,
    deploymentInfoPath: args.deploymentInfoPath,
    network: parseNetwork(args.network),
    provider: args.provider,
    blockfrostApiUrl: args.blockfrostApiUrl,
    blockfrostKey: args.blockfrostKey,
    kupoUrl: args.kupoUrl,
    ogmiosUrl: args.ogmiosUrl,
    walletSeedPhrase: args.walletSeedPhrase,
    walletSeedPhraseEnv: args.walletSeedPhraseEnv,
    walletPrivateKey: args.walletPrivateKey,
    walletPrivateKeyEnv: args.walletPrivateKeyEnv,
    fraudCategory: args.fraudCategory,
    fraudulentHeaderHash: args.fraudulentHeaderHash,
    awaitConfirmation: args.awaitConfirmation,
  };
};

export const buildRemoveUnattestedBlockCliConfig = (args: ParsedArgs) => {
  if (args.deploymentInfoPath === undefined) {
    throw new Error(`Missing required --deployment-info <path>.\n${usage}`);
  }
  if (args.correctionJournalPath === undefined) {
    throw new Error(`Missing required --correction-journal <path>.\n${usage}`);
  }
  refuseRemovalMidgardNodeUrl(args, "remove-unattested-block");
  return {
    deploymentInfoPath: args.deploymentInfoPath,
    journalPath: args.correctionJournalPath,
    network: parseNetwork(args.network),
    provider: args.provider,
    blockfrostApiUrl: args.blockfrostApiUrl,
    blockfrostKey: args.blockfrostKey,
    kupoUrl: args.kupoUrl,
    ogmiosUrl: args.ogmiosUrl,
    walletSeedPhrase: args.walletSeedPhrase,
    walletSeedPhraseEnv: args.walletSeedPhraseEnv,
    walletPrivateKey: args.walletPrivateKey,
    walletPrivateKeyEnv: args.walletPrivateKeyEnv,
    awaitConfirmation: args.awaitConfirmation,
  };
};

export const writeJson = (value: unknown): void => {
  process.stdout.write(stringifyJson(value));
};

export const requireValidationOneStepCliArguments = (
  args: ParsedArgs,
  semanticResolution: boolean,
) => {
  if (args.validationTransitionCborPath === undefined) {
    throw new Error(
      `Missing required --validation-transition-cbor <path>.\n${usage}`,
    );
  }
  if (args.validationAuxiliaryCborPath === undefined) {
    throw new Error(
      `Missing required --validation-auxiliary-cbor <path>.\n${usage}`,
    );
  }
  if (
    args.validationResolverIndex === undefined ||
    !/^(?:0|[1-9][0-9]*)$/u.test(args.validationResolverIndex)
  ) {
    throw new Error(
      `Missing or invalid --validation-resolver-index <n>.\n${usage}`,
    );
  }
  const resolverIndex = Number(args.validationResolverIndex);
  const semanticText = args.validationSemanticResolverIndex;
  if (
    semanticText === undefined ||
    !/^(?:0|[1-9][0-9]*)$/u.test(semanticText)
  ) {
    throw new Error(
      `Missing or invalid --validation-semantic-resolver-index <n>.\n${usage}`,
    );
  }
  if (
    !semanticResolution &&
    (args.validationCekSinglePublicationOutRef !== undefined ||
      args.validationCekMinimumMultiOutputOutRefs.length > 0)
  ) {
    throw new Error(
      "CEK publication outrefs are permitted only for submit-validation-dispute-semantic-resolution",
    );
  }
  return {
    validationTransitionCborPath: args.validationTransitionCborPath,
    validationAuxiliaryCborPath: args.validationAuxiliaryCborPath,
    validationResolverIndex: resolverIndex,
    validationSemanticResolverIndex: Number(semanticText),
    validationCekEnvelopeCborPath: args.validationCekEnvelopeCborPath,
    validationCekProgramMaterialSidecarCborPath:
      args.validationCekProgramMaterialSidecarCborPath,
    validationCekIncrementalNecessityReceiptSetPath:
      args.validationCekIncrementalNecessityReceiptSetPath,
    ...(semanticResolution
      ? {
          validationCekSinglePublicationOutRef:
            args.validationCekSinglePublicationOutRef,
          validationCekMinimumMultiOutputOutRefs:
            args.validationCekMinimumMultiOutputOutRefs.length === 0
              ? undefined
              : args.validationCekMinimumMultiOutputOutRefs,
        }
      : {}),
  };
};

export const isCliEntrypoint = ({
  moduleUrl,
  argvPath,
}: {
  readonly moduleUrl: string;
  readonly argvPath: string | undefined;
}): boolean => {
  if (argvPath === undefined) {
    return false;
  }
  try {
    return realpathSync(fileURLToPath(moduleUrl)) === realpathSync(argvPath);
  } catch {
    return false;
  }
};
