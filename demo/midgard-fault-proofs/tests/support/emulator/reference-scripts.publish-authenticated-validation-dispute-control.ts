import { outRefLabel } from "@al-ft/midgard-core";
import {
  completeReferenceScriptPublicationTxProgram,
  createReferenceScriptAuthPolicy,
  type MidgardValidators,
  referenceScriptAuthPolicyDeploymentInfo,
  type ReferenceScriptAuthTokenTarget,
  referenceScriptAuthUnit,
  type ReferenceScriptPublication,
  referenceScriptPublicationFundingTarget,
  selectReferenceScriptFundingUtxos,
} from "@al-ft/midgard-sdk";
import { Lucid, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { VAN_ROSSEM_PUBLICATION_TARGET_BYTES } from "../../../src/proof-fit/van-rossem-fit-ledger.js";
import {
  type CompleteSignedTransactionMeasurement,
  measureCompleteSignedTransaction,
} from "./measurement.js";
import {
  type ReferenceScriptPublisher,
  type ReferenceScriptPublishingContracts,
} from "./reference-script-publisher.js";

export const VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE =
  "V1 validation-trace dispute";

export const validationDisputeControlPublicationTargets = (
  contracts: MidgardValidators,
) =>
  [
    {
      control: "dispute",
      name: VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE,
      script: contracts.fraudProofs.validationTraceDispute.spendingScript,
    },
    {
      control: "source",
      name: "V1 validation-trace source",
      script:
        contracts.fraudProofs.validationTraceDispute.source.spendingScript,
    },
    {
      control: "game",
      name: "V1 validation-trace game",
      script: contracts.fraudProofs.validationTraceDispute.game.spendingScript,
    },
    {
      control: "boundary",
      name: "V1 validation-trace boundary",
      script:
        contracts.fraudProofs.validationTraceDispute.boundary.spendingScript,
    },
    {
      control: "timeout",
      name: "V1 validation-trace timeout",
      script:
        contracts.fraudProofs.validationTraceDispute.timeout.spendingScript,
    },
    {
      control: "award",
      name: "V1 validation-trace award",
      script: contracts.fraudProofs.validationTraceDispute.award.spendingScript,
    },
  ] as const;

export type ValidationDisputeControlPublicationTarget = ReturnType<
  typeof validationDisputeControlPublicationTargets
>[number];

export const publishAuthenticatedValidationDisputeControl = async ({
  lucid,
  target,
  authPolicy,
  publisher,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly target: {
    readonly control: string;
    readonly name: ReferenceScriptAuthTokenTarget;
    readonly script: Script;
  };
  readonly publisher?: ReferenceScriptPublisher;
  readonly authPolicy: Awaited<
    ReturnType<typeof createReferenceScriptAuthPolicy>
  >;
}) => {
  // A deployment supplies its funding authority explicitly. Standalone
  // publication tests use their caller wallet; neither path changes a wallet.
  const publicationLucid = publisher?.lucid ?? lucid;
  const reserved = new Set(publisher?.reservedInputs.map(outRefLabel));
  const selectedFundingInputs = selectReferenceScriptFundingUtxos(
    (await publicationLucid.wallet().getUtxos()).filter(
      (utxo) => !reserved.has(outRefLabel(utxo)),
    ),
    referenceScriptPublicationFundingTarget(1),
  );
  if (selectedFundingInputs.length === 0) {
    throw new Error(
      `Expected a plain-Ada input for authenticated validation-dispute ${target.control} reference-script publication`,
    );
  }
  const referenceScriptsAddress = await publicationLucid.wallet().address();
  const { tx, layout } = await Effect.runPromise(
    completeReferenceScriptPublicationTxProgram({
      lucid: publicationLucid,
      selectedFundingInputs,
      walletAddress: referenceScriptsAddress,
      referenceScriptsAddress,
      missingTargets: [target],
      authPolicy,
    }),
  );
  const localOutput = layout.localReferenceOutputs.get(target.name);
  if (localOutput === undefined) {
    throw new Error(
      `Authenticated publication transaction omitted the validation-dispute ${target.control} reference-script output`,
    );
  }
  const signed = await tx.sign.withWallet().complete();
  const publicationMeasurement = measureCompleteSignedTransaction(
    signed.toCBOR(),
  );
  if (
    publicationMeasurement.completeSignedBytes >
    VAN_ROSSEM_PUBLICATION_TARGET_BYTES
  ) {
    throw new Error(
      `Authenticated validation-dispute ${target.control} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and exceeds the 15,872-byte publication target`,
    );
  }
  const txHash = await signed.submit();
  await publicationLucid.awaitTx(txHash);
  const outRef = {
    txHash,
    outputIndex: localOutput.outputIndex,
  };
  const published = await publicationLucid.utxosByOutRef([outRef]);
  if (published.length !== 1) {
    throw new Error(
      `Expected one live validation-dispute ${target.control} reference-script UTxO at ${txHash}#${localOutput.outputIndex.toString()}, found ${published.length.toString()}`,
    );
  }
  return {
    authPolicyDeploymentInfo:
      referenceScriptAuthPolicyDeploymentInfo(authPolicy),
    publicationMeasurement,
    utxo: published[0]!,
  };
};

export const publishValidationDisputeReferenceScript = async ({
  lucid,
  contracts,
  now,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: ReferenceScriptPublishingContracts;
  readonly now: number;
}) => {
  const target = validationDisputeControlPublicationTargets(contracts)[0];
  return publishAuthenticatedValidationDisputeControl({
    lucid,
    target,
    authPolicy: await createReferenceScriptAuthPolicy(lucid, now),
  });
};

export const publishStateQueueYieldReferenceScript = async ({
  lucid,
  contracts,
  arm,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: ReferenceScriptPublishingContracts;
  readonly arm: keyof MidgardValidators["stateQueue"]["yields"];
}) => {
  const targetByArm = {
    commit: {
      control: "commit",
      name: "state-queue commit withdrawal",
      script: contracts.stateQueue.yields.commit.withdrawalScript,
    },
    unattestedTimeout: {
      control: "unattested-timeout",
      name: "state-queue unattested-timeout withdrawal",
      script: contracts.stateQueue.yields.unattestedTimeout.withdrawalScript,
    },
    unavailableTimeout: {
      control: "unavailable-timeout",
      name: "state-queue unavailable-timeout withdrawal",
      script: contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
    },
    fraudRemoval: {
      control: "fraud-removal",
      name: "state-queue fraud-removal withdrawal",
      script: contracts.stateQueue.yields.fraudRemoval.withdrawalScript,
    },
    merge: {
      control: "merge",
      name: "state-queue merge withdrawal",
      script: contracts.stateQueue.yields.merge.withdrawalScript,
    },
  } as const;
  return publishAuthenticatedValidationDisputeControl({
    lucid,
    target: targetByArm[arm],
    publisher: contracts.referenceScriptPublisher,
    authPolicy: contracts.referenceScriptAuth as Awaited<
      ReturnType<typeof createReferenceScriptAuthPolicy>
    >,
  });
};

export const findStateQueueYieldReferenceScript = async ({
  lucid,
  contracts,
  arm,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: ReferenceScriptPublishingContracts;
  readonly arm: keyof MidgardValidators["stateQueue"]["yields"];
}): Promise<UTxO> => {
  const roleByArm = {
    commit: "state-queue commit withdrawal",
    unattestedTimeout: "state-queue unattested-timeout withdrawal",
    unavailableTimeout: "state-queue unavailable-timeout withdrawal",
    fraudRemoval: "state-queue fraud-removal withdrawal",
    merge: "state-queue merge withdrawal",
  } as const;
  const unit = referenceScriptAuthUnit(
    contracts.referenceScriptAuth.policyId,
    roleByArm[arm],
  );
  // The publishing authority and consuming operator/prover are different
  // wallets. Resolve the unique deployment role token across the provider.
  const reference = await lucid.utxoByUnit(unit);
  if (reference === undefined) {
    throw new Error(
      `Expected an authenticated ${roleByArm[arm]} reference script`,
    );
  }
  if (reference.scriptRef == null) {
    throw new Error(
      `Authenticated ${roleByArm[arm]} reference script token sits at ${outRefLabel(reference)} without a reference script`,
    );
  }
  return reference;
};

export type MinAdaYieldReferenceScripts = Readonly<{
  tx: ReferenceScriptPublication & {
    readonly publicationMeasurement: CompleteSignedTransactionMeasurement;
  };
  utxo: ReferenceScriptPublication & {
    readonly publicationMeasurement: CompleteSignedTransactionMeasurement;
  };
}>;

/** Publishes the two authenticated rewarding validators delegated to by min-Ada step 02. */
export const publishMinAdaYieldReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: ReferenceScriptPublishingContracts;
}): Promise<MinAdaYieldReferenceScripts> => {
  const targets = [
    {
      key: "tx" as const,
      control: "min-ada step-02 tx yield",
      name: "V1 fraud-proof min-ada step-02 tx yield" as const,
      script: contracts.fraudProofContracts.minAda.yields.tx.withdrawalScript,
    },
    {
      key: "utxo" as const,
      control: "min-ada step-02 UTxO yield",
      name: "V1 fraud-proof min-ada step-02 UTxO yield" as const,
      script: contracts.fraudProofContracts.minAda.yields.utxo.withdrawalScript,
    },
  ];
  const publications = {} as Record<
    (typeof targets)[number]["key"],
    MinAdaYieldReferenceScripts[(typeof targets)[number]["key"]]
  >;
  for (const target of targets) {
    const published = await publishAuthenticatedValidationDisputeControl({
      lucid,
      target,
      publisher: contracts.referenceScriptPublisher,
      authPolicy: contracts.referenceScriptAuth as Awaited<
        ReturnType<typeof createReferenceScriptAuthPolicy>
      >,
    });
    publications[target.key] = {
      name: target.name,
      utxo: published.utxo,
      publicationMeasurement: published.publicationMeasurement,
    };
  }
  return publications;
};
