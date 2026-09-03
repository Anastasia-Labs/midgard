/**
 * `value-not-preserved` proving core (offchain plan §4): the full thread
 * lifecycle, one L1 transaction per step — init, bind-and-claim, one fold
 * per spend input, finish, the outputs/mint/fee completion, and the
 * finalization mint.
 *
 * The orchestrator is deliberately lean: every step submitter already makes
 * its own fail-closed local checks, and each step is an independent,
 * resumable L1 transaction (plan §4.1), so an interrupted run continues by
 * calling the remaining submitters against the thread out-ref the last
 * confirmed step returned. The prover never cancels on its own — an abort
 * surfaces as a thrown error with the live thread out-ref in hand.
 */
import type {
  MidgardMintPolicyItem,
  MidgardTxOutput,
  MidgardValue,
} from "@al-ft/midgard-core";
import type { MidgardTxInput } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { ValueNotPreservedContracts } from "./contracts-v1.js";
import {
  buildSpentInputValueWitness,
  spendInputsOpening as spendInputsOpeningV1,
  type ValueNotPreservedLedgerTrieHandle,
} from "./evidence-v1.js";
import type { ClaimedAsset, ClaimedImbalanceDirection } from "./schemas-v1.js";
import type { ValueNotPreservedCatalogueCategory } from "./submit-common-v1.js";
import { submitValueNotPreservedInit } from "./submit-value-not-preserved-init-v1.js";
import { submitValueNotPreservedStep01 } from "./submit-value-not-preserved-step-01-v1.js";
import {
  submitValueNotPreservedStep02Finish,
  submitValueNotPreservedStep02Fold,
} from "./submit-value-not-preserved-step-02-v1.js";
import { submitValueNotPreservedStep03 } from "./submit-value-not-preserved-step-03-v1.js";
import {
  submitValueNotPreservedStep04,
  type SubmitValueNotPreservedStep04Result,
} from "./submit-value-not-preserved-step-04-v1.js";

/** One spend input of the challenged transaction, with its pre-state facts. */
export type ValueNotPreservedSpentInput = {
  /** The out-ref, in the SDK's wire spelling. */
  readonly input: MidgardTxInput;
  /** The committed `LedgerOutputCommitmentV1` bytes, hex. */
  readonly descriptorCbor: string;
  /** The spent output's value — source of the token asset-leaf walk. */
  readonly spentValue: MidgardValue;
};

export type ProveValueNotPreservedResult = {
  readonly initTxHash: string;
  readonly stepTxHashes: readonly string[];
  readonly finalization: SubmitValueNotPreservedStep04Result;
};

export const proveValueNotPreserved = async ({
  lucid,
  blueprint,
  network,
  contracts,
  category,
  catalogue,
  signer,
  fraudulentBlockOutRef,
  txInclusion,
  claimedAsset,
  claimedDirection,
  prevUtxosRoot,
  spentInputs,
  ledgerTrie,
  spendInputsPreimageCbor,
  outputs,
  mintItems,
  referenceScriptUtxos,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: ValueNotPreservedContracts;
  readonly category: ValueNotPreservedCatalogueCategory;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  /** The challenged block's state-queue UTxO. */
  readonly fraudulentBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly claimedAsset: ClaimedAsset;
  readonly claimedDirection: ClaimedImbalanceDirection;
  /** The challenged header's `prev_utxos_root`, hex. */
  readonly prevUtxosRoot: string;
  /** Field-0 order: entry `k` is the transaction's spend input `k`. */
  readonly spentInputs: readonly ValueNotPreservedSpentInput[];
  /** The pre-state ledger MPF, rooted at `prevUtxosRoot`. */
  readonly ledgerTrie: ValueNotPreservedLedgerTrieHandle;
  /** The transaction's committed field-0 preimage bytes. */
  readonly spendInputsPreimageCbor: Buffer;
  /** The transaction's committed outputs (field 2). */
  readonly outputs: readonly MidgardTxOutput[];
  /** The committed mint items (field 5); null exactly for an ADA claim. */
  readonly mintItems: readonly MidgardMintPolicyItem[] | null;
  /** Published per-step reference scripts (production: always present). */
  readonly referenceScriptUtxos?: {
    readonly step01?: UTxO;
    readonly step02?: UTxO;
    readonly step03?: UTxO;
    readonly step04?: UTxO;
  };
}): Promise<ProveValueNotPreservedResult> => {
  const stepTxHashes: string[] = [];
  const categoryId = category.categoryId;

  const init = await submitValueNotPreservedInit({
    lucid,
    blueprint,
    network,
    contracts,
    category,
    catalogue,
    signer,
    fraudulentBlockOutRef,
  });

  const step01 = await submitValueNotPreservedStep01({
    lucid,
    blueprint,
    contracts,
    categoryId,
    network,
    signer,
    threadOutRef: init.nextThreadOutRef,
    stateQueueBlockOutRef: fraudulentBlockOutRef,
    txInclusion,
    claimedAsset,
    claimedDirection,
    prevUtxosRoot,
    referenceScriptUtxo: referenceScriptUtxos?.step01,
  });
  stepTxHashes.push(step01.txHash);

  const spendInputsOpening = spendInputsOpeningV1({
    nativeTxCompactCbor: txInclusion.nativeTxCompactCbor,
    spendInputsPreimageCbor,
  });

  let threadOutRef = step01.nextThreadOutRef;
  for (const spentInput of spentInputs) {
    const valueWitness = await buildSpentInputValueWitness({
      claim: claimedAsset,
      descriptorCbor: spentInput.descriptorCbor,
      spentValue: spentInput.spentValue,
      trie: ledgerTrie,
      input: spentInput.input,
      prevUtxosRootHex: prevUtxosRoot,
    });
    const fold = await submitValueNotPreservedStep02Fold({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      spendInputsOpening,
      valueWitness,
      referenceScriptUtxo: referenceScriptUtxos?.step02,
    });
    stepTxHashes.push(fold.txHash);
    threadOutRef = fold.nextThreadOutRef;
  }

  const finish = await submitValueNotPreservedStep02Finish({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    spendInputsOpening,
    spendInputCount: BigInt(spentInputs.length),
    referenceScriptUtxo: referenceScriptUtxos?.step02,
  });
  stepTxHashes.push(finish.txHash);

  const step03 = await submitValueNotPreservedStep03({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef: finish.nextThreadOutRef,
    nativeTxCompactCbor: txInclusion.nativeTxCompactCbor,
    outputs,
    mintItems,
    referenceScriptUtxo: referenceScriptUtxos?.step03,
  });
  stepTxHashes.push(step03.txHash);

  const finalization = await submitValueNotPreservedStep04({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef: step03.nextThreadOutRef,
    referenceScriptUtxo: referenceScriptUtxos?.step04,
  });
  stepTxHashes.push(finalization.txHash);

  return { initTxHash: init.txHash, stepTxHashes, finalization };
};
