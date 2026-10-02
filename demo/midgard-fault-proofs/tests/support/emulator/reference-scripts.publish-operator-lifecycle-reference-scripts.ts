import {
  type MidgardValidators,
  type ReferenceScriptPublication,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Lucid,
  type Script,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";

import { VAN_ROSSEM_PUBLICATION_TARGET_BYTES } from "../../../src/proof-fit/van-rossem-fit-ledger.js";
import { FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY } from "../../../src/runtime.js";
import { network } from "./blueprints.js";
import {
  type CompleteSignedTransactionMeasurement,
  measureCompleteSignedTransaction,
} from "./measurement.js";
import { type ReferenceScriptPublishingContracts } from "./reference-script-publisher.js";
import { publishStateQueueYieldReferenceScript } from "./reference-scripts.publish-authenticated-validation-dispute-control.js";
import {
  TRACED_REFUSALS,
  withTracedPublicationEnvelope,
} from "./traced-refusals.js";

// Publishes a deployed validator as a plain reference-script UTxO at the
// publisher wallet address, following the hash-checked deployment
// consumption pattern (`requireDeploymentReferenceScript`); the consuming
// submit path re-derives the applied script hash and requires the published
// scriptRef to match it exactly.
export const publishPlainReferenceScriptUtxo = async ({
  lucid,
  script,
  label,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly script: Script;
  readonly label: string;
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
  const lovelace = 20_000_000n;
  const unsigned = await withTracedPublicationEnvelope(lucid, () =>
    lucid
      .newTx()
      .pay.ToAddressWithData(parkAddress, undefined, { lovelace }, script)
      .complete(),
  ).catch((cause: unknown) => {
    throw new Error(
      `${label} reference-script publication failed (applied script CBOR ${(script.script.length / 2).toString()} bytes): ${String(cause)}`,
    );
  });
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const publicationMeasurement = measureCompleteSignedTransaction(signedCbor);
  if (
    !TRACED_REFUSALS &&
    publicationMeasurement.completeSignedBytes >
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES
  ) {
    throw new Error(
      `${label} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and exceeds the 15,872-byte publication target`,
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

export type OperatorLifecycleReferenceScripts = {
  readonly registered: readonly ReferenceScriptPublication[];
  readonly active: readonly ReferenceScriptPublication[];
  readonly initial: readonly ReferenceScriptPublication[];
};

/**
 * Publish the nine distinct reference scripts consumed by genesis plus the
 * genuine register-then-activate setup lifecycle and the authenticated commit
 * reference consumed by the operator. Each script is deliberately
 * placed in its own bounded transaction so a shared publication cannot hide
 * an individually unpublishable validator.
 */
export const publishOperatorLifecycleReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: ReferenceScriptPublishingContracts;
}): Promise<OperatorLifecycleReferenceScripts> => {
  const roster = [
    {
      group: "registered" as const,
      name: "registered-operators spending",
      script: contracts.registeredOperators.spendingScript,
    },
    {
      group: "registered" as const,
      name: "registered-operators minting",
      script: contracts.registeredOperators.mintingScript,
    },
    {
      group: "active" as const,
      name: "active-operators spending",
      script: contracts.activeOperators.spendingScript,
    },
    {
      group: "active" as const,
      name: "active-operators minting",
      script: contracts.activeOperators.mintingScript,
    },
  ] as const;
  const registered: ReferenceScriptPublication[] = [];
  const active: ReferenceScriptPublication[] = [];
  for (const entry of roster) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid,
      script: entry.script,
      label: `setup ${entry.name}`,
    });
    (entry.group === "registered" ? registered : active).push({
      name: entry.name,
      utxo: publication.utxo,
    });
  }
  if (registered.length !== 2 || active.length !== 2) {
    throw new Error(
      "Operator lifecycle reference publication omitted or duplicated a canonical role",
    );
  }
  const initial: ReferenceScriptPublication[] = [
    ...registered.filter(({ name }) => name.endsWith(" minting")),
    ...active.filter(({ name }) => name.endsWith(" minting")),
  ];
  for (const entry of [
    {
      name: "hub-oracle minting",
      script: contracts.hubOracle.mintingScript,
    },
    {
      name: "fraud-proof-catalogue minting",
      script: contracts.fraudProofCatalogue.mintingScript,
    },
    { name: "scheduler minting", script: contracts.scheduler.mintingScript },
    {
      name: "state-queue minting",
      script: contracts.stateQueue.mintingScript,
    },
    {
      name: "retired-operators minting",
      script: contracts.retiredOperators.mintingScript,
    },
  ] as const) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid,
      script: entry.script,
      label: `setup ${entry.name}`,
    });
    initial.push({ name: entry.name, utxo: publication.utxo });
  }
  if (initial.length !== 7) {
    throw new Error("Initial setup reference publication omitted a role");
  }
  // Publish through the explicit deployment funding authority before the
  // header clock is sampled; operator setup consumes this ready reference.
  await publishStateQueueYieldReferenceScript({
    lucid,
    contracts,
    arm: "commit",
  });
  return { registered, active, initial };
};

/** Publishes a canonical fraud-proof chain under its production entry names. */
export const publishFraudProofChainReferenceScripts = async ({
  lucid,
  steps,
  entryNames,
  familyLabel,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly steps: readonly {
    readonly spendingScript: Script;
    readonly spendingScriptHash: string;
  }[];
  readonly entryNames: readonly string[];
  readonly familyLabel: string;
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
export const publishHarnessFaultProofReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: ReferenceScriptPublishingContracts;
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
