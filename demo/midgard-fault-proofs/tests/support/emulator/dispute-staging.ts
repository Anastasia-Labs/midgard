/**
 * Staging helpers shared by the two validation-dispute journey harnesses
 * (`dispute-scenario.ts` and `route-freedom-journey.ts`): the party/emulator
 * ledger both start from, and the explicit maxTxSize pinning for
 * reference-script publications within the shared strict L1 envelope.
 */
import {
  createReferenceScriptAuthPolicy,
  type MidgardValidators,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";

import {
  resolveProverSigner,
  validationDisputeValidityRange,
} from "../../../src/index.js";
import { network } from "./blueprints.js";
import { seedDualAddressPartyAccounts } from "./emulator-context.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";
import { type ReferenceScriptPublisher } from "./reference-script-publisher.js";
import {
  publishAuthenticatedValidationDisputeControl,
  publishValidationDisputeReferenceScript,
  type ValidationDisputeControlPublicationTarget,
  validationDisputeControlPublicationTargets,
} from "./reference-scripts.js";

/**
 * The emulator ledger every dispute journey starts from: an operator and a
 * challenger, each holding one main balance plus twelve 100-Ada fee UTxOs,
 * with a Lucid, a prover signer, and the dispute validity-range helper wired
 * to the same emulator clock for each party.
 *
 * Both parties are seeded at both addresses their seed phrase resolves to; see
 * `seedDualAddressPartyAccounts`.
 */
export const createValidationDisputeParties = async () => {
  const operator = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const challenger = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const feeUtxoCount = 12;
  // The setup transaction registers a stake credential for every dispute
  // yield (each with a 2-Ada deposit); the shared LOP family brought the
  // registration count past what a 100-Ada fee UTxO can fund.
  const feeUtxoLovelace = 200_000_000n;
  const emulator = new Emulator(
    [
      ...seedDualAddressPartyAccounts({
        account: operator,
        feeUtxoCount,
        feeUtxoLovelace,
      }),
      ...seedDualAddressPartyAccounts({
        account: challenger,
        feeUtxoCount,
        feeUtxoLovelace,
      }),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const operatorLucid = await Lucid(emulator, "Custom");
  const challengerLucid = await Lucid(emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  challengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const operatorSigner = resolveProverSigner({
    network,
    walletSeedPhrase: operator.seedPhrase,
  });
  const challengerSigner = resolveProverSigner({
    network,
    walletSeedPhrase: challenger.seedPhrase,
  });
  const validityRange = () => validationDisputeValidityRange(emulator.now());
  return {
    emulator,
    operator,
    challenger,
    operatorLucid,
    challengerLucid,
    operatorSigner,
    challengerSigner,
    validityRange,
  };
};

/**
 * Runs `operation` with the emulator's `maxTxSize` explicitly pinned to the
 * 16,384-byte L1 envelope, restoring the strict source parameters afterwards.
 * Reference-script publications and target Lucids share that envelope.
 */
export const withRealL1MaxTxSize = async <T>(
  emulator: Emulator,
  operation: () => Promise<T>,
): Promise<T> => {
  const preOperationProtocolParameters = emulator.protocolParameters;
  emulator.protocolParameters = {
    ...preOperationProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  try {
    return await operation();
  } finally {
    emulator.protocolParameters = preOperationProtocolParameters;
  }
};

type ValidationDisputePublication = Awaited<
  ReturnType<typeof publishValidationDisputeReferenceScript>
>;
type ValidationDisputeControlPublications = Record<
  ValidationDisputeControlPublicationTarget["control"],
  ValidationDisputePublication
>;

/**
 * Publishes the authenticated validation-dispute reference script on a
 * dedicated publisher Lucid, both constructed and publishing under the real
 * L1 `maxTxSize` pin, through the caller's lifecycle-stage runner. Returns
 * the publisher Lucid too — the removal reference-script publications reuse
 * it later under their own pin.
 */
export const stageAuthenticatedValidationDisputePublication = async ({
  emulator,
  operatorLucid,
  operatorSeedPhrase,
  contracts,
  authPolicy: suppliedAuthPolicy,
  publisher,
  runStage,
}: {
  readonly emulator: Emulator;
  readonly operatorLucid: LucidEvolution;
  readonly operatorSeedPhrase: string;
  readonly contracts: MidgardValidators;
  readonly publisher?: ReferenceScriptPublisher;
  readonly authPolicy?: Awaited<
    ReturnType<typeof createReferenceScriptAuthPolicy>
  >;
  readonly runStage: <T>(
    label: string,
    operation: () => Promise<T>,
  ) => Promise<T>;
}): Promise<{
  readonly referenceScriptPublisherLucid: LucidEvolution;
  readonly validationDisputePublication: ValidationDisputePublication;
  readonly validationDisputeControlPublications: ValidationDisputeControlPublications;
}> => {
  const publicationSlotConfig = operatorLucid.config().slotConfig;
  if (publicationSlotConfig === undefined) {
    throw new Error(
      "Expected reference-script publisher Lucid to expose its Custom slot config",
    );
  }
  return withRealL1MaxTxSize(emulator, async () => {
    const referenceScriptPublisherLucid = await Lucid(emulator, "Custom", {
      slotConfig: publicationSlotConfig,
    });
    referenceScriptPublisherLucid.selectWallet.fromSeed(operatorSeedPhrase);
    const validationDisputeControlPublications = await runStage(
      "reference-script.publish-authenticated",
      async () => {
        const authPolicy =
          suppliedAuthPolicy ??
          (await createReferenceScriptAuthPolicy(
            referenceScriptPublisherLucid,
            emulator.now(),
          ));
        const publications = {} as ValidationDisputeControlPublications;
        for (const target of validationDisputeControlPublicationTargets(
          contracts,
        )) {
          publications[target.control] =
            await publishAuthenticatedValidationDisputeControl({
              lucid: referenceScriptPublisherLucid,
              target,
              authPolicy,
              publisher,
            });
        }
        return publications;
      },
    );
    return {
      referenceScriptPublisherLucid,
      validationDisputePublication:
        validationDisputeControlPublications.dispute,
      validationDisputeControlPublications,
    };
  });
};

/**
 * Constructs the functional operator/challenger Lucids under the real L1
 * `maxTxSize`, so every dispute transaction they build must fit the actual
 * 16,384-byte envelope. Leaves the emulator pinned for subsequent publications.
 */
export const createRealL1TargetLucids = async ({
  emulator,
  sourceLucid,
  operatorSeedPhrase,
  challengerSeedPhrase,
}: {
  readonly emulator: Emulator;
  readonly sourceLucid: LucidEvolution;
  readonly operatorSeedPhrase: string;
  readonly challengerSeedPhrase: string;
}) => {
  const functionalProtocolParameters = emulator.protocolParameters;
  const functionalSlotConfig = sourceLucid.config().slotConfig;
  if (functionalSlotConfig === undefined) {
    throw new Error(
      "Expected functional emulator Lucid to expose its Custom slot config",
    );
  }
  emulator.protocolParameters = {
    ...functionalProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const targetOperatorLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  const targetChallengerLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  targetOperatorLucid.selectWallet.fromSeed(operatorSeedPhrase);
  targetChallengerLucid.selectWallet.fromSeed(challengerSeedPhrase);
  return {
    targetOperatorLucid,
    targetChallengerLucid,
  };
};
