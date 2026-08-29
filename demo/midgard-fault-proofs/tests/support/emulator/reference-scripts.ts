import {
  completeReferenceScriptPublicationTxProgram,
  createReferenceScriptAuthPolicy,
  type MidgardValidators,
  referenceScriptAuthPolicyDeploymentInfo,
  referenceScriptPublicationFundingTarget,
  selectReferenceScriptFundingUtxos,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Lucid,
  type Script,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CrossBlockDuplicateEventContractsV1 } from "../../../src/cross-block-duplicate-event/index.js";
import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "../../../src/ne-submit-step-03.js";
import { chunkedVerifyWithdrawalScript } from "../../../src/proof-chunk-carriage.js";
import {
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  getCompiledScript,
} from "../../../src/runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../../../src/submit-step-01.js";
import { type FaultProofWitnessReferenceScriptsV1 } from "../../../src/witness-reference-scripts-v1.js";
import { network } from "./blueprints.js";
import {
  type CompleteSignedTransactionMeasurement,
  measureCompleteSignedTransaction,
} from "./measurement.js";

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
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly target: ValidationDisputeControlPublicationTarget;
  readonly authPolicy: ReturnType<typeof createReferenceScriptAuthPolicy>;
}) => {
  const selectedFundingInputs = selectReferenceScriptFundingUtxos(
    await lucid.wallet().getUtxos(),
    referenceScriptPublicationFundingTarget(1),
  );
  if (selectedFundingInputs.length === 0) {
    throw new Error(
      `Expected a plain-Ada input for authenticated validation-dispute ${target.control} reference-script publication`,
    );
  }
  const referenceScriptsAddress = await lucid.wallet().address();
  const { tx, layout } = await Effect.runPromise(
    completeReferenceScriptPublicationTxProgram({
      lucid,
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
  if (publicationMeasurement.l1ByteMargin <= 0) {
    throw new Error(
      `Authenticated validation-dispute ${target.control} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and does not fit the 16,384-byte L1 envelope`,
    );
  }
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const outRef = {
    txHash,
    outputIndex: localOutput.outputIndex,
  };
  const published = await lucid.utxosByOutRef([outRef]);
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
  readonly contracts: MidgardValidators;
  readonly now: number;
}) => {
  const target = validationDisputeControlPublicationTargets(contracts)[0];
  return publishAuthenticatedValidationDisputeControl({
    lucid,
    target,
    authPolicy: createReferenceScriptAuthPolicy(lucid, now),
  });
};

// Publishes a deployed validator as a plain reference-script UTxO at the
// publisher wallet address, following the hash-checked deployment
// consumption pattern (`requireDeploymentReferenceScript`); the consuming
// submit path re-derives the applied script hash and requires the published
// scriptRef to match it exactly.
export const publishPlainReferenceScriptUtxo = async ({
  lucid,
  script,
  label,
  oversized = false,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly script: Script;
  readonly label: string;
  /**
   * The applied CEK execution-selection / context-step / core-step semantic
   * resolvers (R5 item 1) exceed the 16,384-byte L1 proof envelope, so their
   * deployment-time publication cannot fit it: the emulator must host them
   * under a raised `maxTxSize`, the output must reach the script-ref min-Ada
   * for a ~45–94 KiB reference script, and the measurement is returned
   * unasserted so callers pin the honest publication size while the consuming
   * semantic-resolution transaction stays inside the envelope via `readFrom`.
   */
  readonly oversized?: boolean;
}): Promise<{
  readonly utxo: UTxO;
  readonly publicationMeasurement: CompleteSignedTransactionMeasurement;
}> => {
  // Park the reference script at an unspendable script credential so no
  // later wallet coin selection can consume the published UTxO mid-flow.
  const parkAddress = credentialToAddress(
    network,
    scriptHashToCredential("2f".repeat(28)),
  );
  const lovelace = oversized
    ? BigInt(script.script.length / 2) * 8_620n + 100_000_000n
    : 20_000_000n;
  const unsigned = await lucid
    .newTx()
    .pay.ToAddressWithData(parkAddress, undefined, { lovelace }, script)
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const publicationMeasurement = measureCompleteSignedTransaction(signedCbor);
  if (!oversized && publicationMeasurement.l1ByteMargin <= 0) {
    throw new Error(
      `${label} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and does not fit the 16,384-byte L1 envelope`,
    );
  }
  const outputs = CML.Transaction.from_cbor_hex(signedCbor).body().outputs();
  let scriptRefOutputIndex = -1;
  for (let index = 0; index < outputs.len(); index += 1) {
    if (outputs.get(index).script_ref() !== undefined) {
      scriptRefOutputIndex = index;
      break;
    }
  }
  if (scriptRefOutputIndex < 0) {
    throw new Error(`${label} publication omitted its script-ref output`);
  }
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const published = await lucid.utxosByOutRef([
    { txHash, outputIndex: scriptRefOutputIndex },
  ]);
  if (published.length !== 1 || published[0]!.scriptRef == null) {
    throw new Error(
      `Expected one live ${label} reference-script UTxO at ${txHash}#${scriptRefOutputIndex.toString()}`,
    );
  }
  return { utxo: published[0]!, publicationMeasurement };
};

/** Publishes a canonical fraud-proof chain under its production entry names. */
export const publishFraudProofChainReferenceScripts = async ({
  lucid,
  steps,
  entryNames,
  familyLabel,
  oversizedEntryNames = new Set(),
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly steps: readonly {
    readonly spendingScript: Script;
    readonly spendingScriptHash: string;
  }[];
  readonly entryNames: readonly string[];
  readonly familyLabel: string;
  /**
   * Explicit emulator-only publication hosts for scripts whose production
   * CBOR exceeds one L1 transaction. Their consuming transactions still use
   * the real reference inputs and remain under the normal envelope.
   */
  readonly oversizedEntryNames?: ReadonlySet<string>;
}): Promise<
  Readonly<Record<string, { readonly scriptHash: string; readonly utxo: UTxO }>>
> => {
  if (steps.length !== entryNames.length) {
    throw new Error(
      `${familyLabel} reference-script fixture has ${steps.length.toString()} steps for ${entryNames.length.toString()} production entries`,
    );
  }
  const publications: Record<
    string,
    { readonly scriptHash: string; readonly utxo: UTxO }
  > = {};
  for (const [index, entryName] of entryNames.entries()) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid,
      script: steps[index]!.spendingScript,
      label: `${familyLabel} ${entryName}`,
      oversized: oversizedEntryNames.has(entryName),
    });
    publications[entryName] = {
      scriptHash: steps[index]!.spendingScriptHash,
      utxo: publication.utxo,
    };
  }
  return publications;
};

/**
 * Publishes every distinct fault-proof step script in a harness once, then
 * maps all production deployment-entry names sharing that hash to the same
 * immutable UTxO. Most non-focused chains use the same tiny emulator script,
 * so hash de-duplication keeps the scenario preamble bounded.
 */
export const publishHarnessFaultProofReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
}): Promise<
  Readonly<Record<string, { readonly scriptHash: string; readonly utxo: UTxO }>>
> => {
  const publicationByHash = new Map<string, UTxO>();
  const publications: Record<
    string,
    { readonly scriptHash: string; readonly utxo: UTxO }
  > = {};
  for (const [category, entryNames] of Object.entries(
    FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  )) {
    const steps =
      contracts.fraudProofContracts[
        category as keyof MidgardValidators["fraudProofContracts"]
      ].steps;
    for (const [stepIndex, step] of steps.entries()) {
      const entryName =
        entryNames[stepIndex] ??
        `${entryNames[0]}Step${(stepIndex + 1).toString().padStart(2, "0")}`;
      let utxo = publicationByHash.get(step.spendingScriptHash);
      if (utxo === undefined) {
        utxo = (
          await publishPlainReferenceScriptUtxo({
            lucid,
            script: step.spendingScript,
            label: `fault-proof step ${entryName}`,
            oversized: step.spendingScript.script.length / 2 > 14_000,
          })
        ).utxo;
        publicationByHash.set(step.spendingScriptHash, utxo);
      }
      publications[entryName] = {
        scriptHash: step.spendingScriptHash,
        utxo,
      };
    }
  }
  return publications;
};

/**
 * Publishes the shared witness scripts (owner ruling 2026-08-26: fault proofs
 * and their supporting scripts deploy as reference scripts) once per emulator
 * scenario: the computation-thread and fraud-proof minting policies plus the
 * `phas.membership.withdraw` verifier always, and the chunked-verify /
 * `pexcludes.exclusion.withdraw` verifiers when the scenario's transactions
 * execute them. Submitters hash-check every returned UTxO against the exact
 * script they would otherwise inline-attach.
 */
export const publishFaultProofWitnessReferenceScriptsV1 = async ({
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
}): Promise<FaultProofWitnessReferenceScriptsV1> => {
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const roster: readonly (readonly [
    keyof FaultProofWitnessReferenceScriptsV1,
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
    Record<keyof FaultProofWitnessReferenceScriptsV1, UTxO>
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

export const TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES = new Set([
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
]);

export const publishCrossBlockDuplicateEventReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: CrossBlockDuplicateEventContractsV1;
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
  | "stateQueueSpend"
  | "stateQueueMint"
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
  readonly contracts: MidgardValidators;
}): Promise<{
  readonly published: RemovalReferenceScriptPublications;
  readonly measurements: RemovalReferenceScriptMeasurements;
}> => {
  const roster: readonly (readonly [RemovalReferenceScriptName, Script])[] = [
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
  return {
    published: published as RemovalReferenceScriptPublications,
    measurements: measurements as RemovalReferenceScriptMeasurements,
  };
};
