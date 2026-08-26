/**
 * Staging helpers shared by the two validation-dispute journey harnesses
 * (`dispute-scenario.ts` and `route-freedom-journey.ts`): the party/emulator
 * ledger both start from, and the maxTxSize pinning that keeps
 * reference-script publications honest against the real L1 envelope while the
 * emulator itself runs at the raised deployment-time limit.
 */

import { type MidgardValidators } from "@al-ft/midgard-sdk";
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
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";
import { publishValidationDisputeReferenceScript } from "./reference-scripts.js";

/**
 * The emulator ledger every dispute journey starts from: an operator and a
 * challenger, each holding one main balance plus twelve 100-Ada fee UTxOs,
 * with a Lucid, a prover signer, and the dispute validity-range helper wired
 * to the same emulator clock for each party.
 */
export const createValidationDisputeParties = async () => {
  const operator = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const challenger = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const feeUtxoCount = 12;
  const feeUtxoLovelace = 100_000_000n;
  const emulator = new Emulator(
    [
      {
        ...operator,
        assets: {
          lovelace:
            operator.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...operator,
        assets: { lovelace: feeUtxoLovelace },
      })),
      {
        ...challenger,
        assets: {
          lovelace:
            challenger.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...challenger,
        assets: { lovelace: feeUtxoLovelace },
      })),
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
 * Runs `operation` with the emulator's `maxTxSize` pinned down from the
 * raised deployment-time limit ({@link EMULATOR_PROTOCOL_PARAMETERS}) to the
 * real 16,384-byte L1 envelope, restoring the previous parameters afterwards.
 * Reference-script publications run under this pin so a publication that
 * claims to fit L1 is actually built against the real limit.
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
  runStage,
}: {
  readonly emulator: Emulator;
  readonly operatorLucid: LucidEvolution;
  readonly operatorSeedPhrase: string;
  readonly contracts: MidgardValidators;
  readonly runStage: (
    label: string,
    operation: () => Promise<ValidationDisputePublication>,
  ) => Promise<ValidationDisputePublication>;
}): Promise<{
  readonly referenceScriptPublisherLucid: LucidEvolution;
  readonly validationDisputePublication: ValidationDisputePublication;
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
    const validationDisputePublication = await runStage(
      "reference-script.publish-authenticated",
      () =>
        publishValidationDisputeReferenceScript({
          lucid: referenceScriptPublisherLucid,
          contracts,
          now: emulator.now(),
        }),
    );
    return { referenceScriptPublisherLucid, validationDisputePublication };
  });
};

/**
 * Constructs the functional operator/challenger Lucids under the real L1
 * `maxTxSize`, so every dispute transaction they build must fit the actual
 * 16,384-byte envelope. Deliberately leaves the emulator pinned — the
 * returned pre-pin `functionalProtocolParameters` are what an oversized
 * publication temporarily restores to get a >16 KiB reference script hosted.
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
    functionalProtocolParameters,
    functionalSlotConfig,
    targetOperatorLucid,
    targetChallengerLucid,
  };
};
