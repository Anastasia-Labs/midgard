import { Lucid, type Script, type UTxO } from "@lucid-evolution/lucid";

import type { CrossBlockDuplicateEventContracts } from "../../../src/cross-block-duplicate-event/index.js";
import { chunkedVerifyWithdrawalScript } from "../../../src/proof-chunk-carriage.js";
import { getCompiledScript } from "../../../src/runtime.js";
import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "../../../src/step-support.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../../../src/step-support.js";
import { type FaultProofWitnessReferenceScripts } from "../../../src/witness-reference-scripts.js";
import { type CompleteSignedTransactionMeasurement } from "./measurement.js";
import { type ReferenceScriptPublishingContracts } from "./reference-script-publisher.js";
import { publishStateQueueYieldReferenceScript } from "./reference-scripts.publish-authenticated-validation-dispute-control.js";
import { publishPlainReferenceScriptUtxo } from "./reference-scripts.publish-operator-lifecycle-reference-scripts.js";

/**
 * Publishes the shared witness scripts (owner ruling 2026-08-26: fault proofs
 * and their supporting scripts deploy as reference scripts) once per emulator
 * scenario: the computation-thread and fraud-proof minting policies plus the
 * `phas.membership.withdraw` verifier always, and the chunked-verify /
 * `pexcludes.exclusion.withdraw` verifiers when the scenario's transactions
 * execute them. Submitters hash-check every returned UTxO against the exact
 * script they would otherwise inline-attach.
 */
export const publishFaultProofWitnessReferenceScripts = async ({
  lucid,
  realBlueprint,
  computationThreadMintingScript,
  fraudProofMintingScript,
  includeChunkedVerify = false,
  includePexcludes = false,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly realBlueprint: unknown;
  readonly computationThreadMintingScript?: Script;
  readonly fraudProofMintingScript?: Script;
  readonly includeChunkedVerify?: boolean;
  readonly includePexcludes?: boolean;
}): Promise<FaultProofWitnessReferenceScripts> => {
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const roster: readonly (readonly [
    keyof FaultProofWitnessReferenceScripts,
    Script | undefined,
  ])[] = [
    ["computationThreadMint", computationThreadMintingScript],
    ["fraudProofMint", fraudProofMintingScript],
    ["phasMembershipWithdraw", phasMembershipScript],
    [
      "chunkedVerifyWithdraw",
      includeChunkedVerify
        ? chunkedVerifyWithdrawalScript(realBlueprint)
        : undefined,
    ],
    [
      "pexcludesWithdraw",
      includePexcludes
        ? {
            type: "PlutusV3",
            script: getCompiledScript(
              realBlueprint,
              PEXCLUDES_EXCLUSION_WITHDRAW_TITLE,
            ),
          }
        : undefined,
    ],
  ];
  const published: Partial<
    Record<keyof FaultProofWitnessReferenceScripts, UTxO>
  > = {};
  // Sequential: each publication consumes wallet UTxOs the next one selects
  // from.
  for (const [name, script] of roster) {
    if (script === undefined) {
      continue;
    }
    const publication = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `fault-proof witness ${name}`,
    });
    published[name] = publication.utxo;
  }
  return published;
};

export const publishCrossBlockDuplicateEventReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: CrossBlockDuplicateEventContracts;
}): Promise<readonly [UTxO, UTxO]> => {
  const first = await publishPlainReferenceScriptUtxo({
    lucid,
    script: contracts.steps[0].spendingScript,
    label: "cross-block-duplicate-event step-01",
  });
  const second = await publishPlainReferenceScriptUtxo({
    lucid,
    script: contracts.steps[1].spendingScript,
    label: "cross-block-duplicate-event step-02",
  });
  return [first.utxo, second.utxo];
};

// The validators `remove-fraudulent-block` needs, in the same roster order as
// `REFERENCE_SCRIPT_NAMES` in `src/remove-fraudulent-block.ts`. Every one of
// these is also a production reference-script publication target (see
// `midgard-node/src/transactions/reference-scripts.ts`), so sourcing them from
// reference inputs is the deployed shape, not a test-only shortcut.
export type RemovalReferenceScriptName =
  | "correctionLockSpend"
  | "stateQueueSpend"
  | "stateQueueMint"
  | "stateQueueFraudRemovalWithdraw"
  | "activeOperatorsSpend"
  | "activeOperatorsMint"
  | "retiredOperatorsSpend"
  | "retiredOperatorsMint"
  | "schedulerSpend";

export type RemovalReferenceScriptPublications = Readonly<
  Record<RemovalReferenceScriptName, UTxO>
>;

export type RemovalReferenceScriptMeasurements = Readonly<
  Record<RemovalReferenceScriptName, CompleteSignedTransactionMeasurement>
>;

export const publishRemovalReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: ReferenceScriptPublishingContracts;
}): Promise<{
  readonly published: RemovalReferenceScriptPublications;
  readonly measurements: RemovalReferenceScriptMeasurements;
}> => {
  const roster: readonly (readonly [RemovalReferenceScriptName, Script])[] = [
    ["correctionLockSpend", contracts.correctionLock.spendingScript],
    ["stateQueueSpend", contracts.stateQueue.spendingScript],
    ["stateQueueMint", contracts.stateQueue.mintingScript],
    ["activeOperatorsSpend", contracts.activeOperators.spendingScript],
    ["activeOperatorsMint", contracts.activeOperators.mintingScript],
    ["retiredOperatorsSpend", contracts.retiredOperators.spendingScript],
    ["retiredOperatorsMint", contracts.retiredOperators.mintingScript],
    ["schedulerSpend", contracts.scheduler.spendingScript],
  ];
  const published: Partial<Record<RemovalReferenceScriptName, UTxO>> = {};
  const measurements: Partial<
    Record<RemovalReferenceScriptName, CompleteSignedTransactionMeasurement>
  > = {};
  // Sequential: each publication consumes wallet UTxOs the next one selects
  // from.
  for (const [name, script] of roster) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `state-queue removal ${name}`,
    });
    published[name] = publication.utxo;
    measurements[name] = publication.publicationMeasurement;
  }
  const fraudRemovalYield = await publishStateQueueYieldReferenceScript({
    lucid,
    contracts,
    arm: "fraudRemoval",
  });
  published.stateQueueFraudRemovalWithdraw = fraudRemovalYield.utxo;
  measurements.stateQueueFraudRemovalWithdraw =
    fraudRemovalYield.publicationMeasurement;
  return {
    published: published as RemovalReferenceScriptPublications,
    measurements: measurements as RemovalReferenceScriptMeasurements,
  };
};
