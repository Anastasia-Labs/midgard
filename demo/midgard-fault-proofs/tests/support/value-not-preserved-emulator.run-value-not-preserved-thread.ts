import { type Script, type UTxO } from "@lucid-evolution/lucid";

import { submitResolvedInit } from "../../src/submit-init.js";
import {
  VALUE_NOT_PRESERVED_CATEGORY_LABEL,
  type ValueNotPreservedContracts,
} from "../../src/value-not-preserved/contracts.js";
import {
  buildSpentInputValueWitness,
  spendInputsOpening as spendInputsOpeningV1,
} from "../../src/value-not-preserved/evidence.js";
import {
  type ClaimedAsset,
  type ClaimedImbalanceDirection,
} from "../../src/value-not-preserved/schemas.js";
import { submitValueNotPreservedStep01 } from "../../src/value-not-preserved/submit-value-not-preserved-step-01.js";
import {
  submitValueNotPreservedStep02Finish,
  submitValueNotPreservedStep02Fold,
} from "../../src/value-not-preserved/submit-value-not-preserved-step-02.js";
import { submitValueNotPreservedStep03 } from "../../src/value-not-preserved/submit-value-not-preserved-step-03.js";
import { submitValueNotPreservedStep04 } from "../../src/value-not-preserved/submit-value-not-preserved-step-04.js";
import { type FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { publishFaultProofWitnessReferenceScripts } from "./emulator/reference-scripts.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
} from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  funderPaymentKeyHash,
  makeHeader,
  network as emulatorNetwork,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import { type ValueNotPreservedFixture } from "./value-not-preserved-emulator.build-value-not-preserved-fixture.js";
import {
  commitHeaderAfterAnchorBlock,
  type ValueNotPreservedHarness,
} from "./value-not-preserved-emulator.commit-header-after-anchor-block.js";

/**
 * Commits the fraudulent header as the SECOND block after an anchor block
 * carrying the fixture ledger's root as `utxos_root`.
 *
 * The state-queue commit validator chains `prev_utxos_root` exactly: the
 * first block after genesis must carry the genesis sentinel's (empty)
 * `utxo_root`, so a committed header whose `prev_utxos_root` is the
 * fixture's pre-state ledger root — the commitment every step-02 value
 * witness authenticates against — can only exist as a successor of a block
 * whose `utxos_root` IS that root. The setup therefore commits:
 *
 * - anchor block A: empty L2 material, `utxos_root = fixture.ledger.rootHex`;
 * - fraudulent block B: the fixture's counted `transactions_root`,
 *   `prev_utxos_root = fixture.ledger.rootHex`, chained to A by hash and
 *   time exactly as `commit_block_header_carries_previous_block_v1`
 *   demands.
 */
export const setupValueNotPreservedScenario = async ({
  harness,
  fixture,
}: {
  readonly harness: ValueNotPreservedHarness;
  readonly fixture: ValueNotPreservedFixture;
}) => {
  const {
    emulator,
    funderLucid,
    proverLucid,
    realBlueprint,
    contracts,
    family,
    catalogue,
    nonceUtxo,
  } = harness;
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: proverLucid,
      realBlueprint,
      computationThreadMintingScript: family.computationThread.mintingScript,
      fraudProofMintingScript: family.fraudProof.mintingScript,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const baseAnchorHeader = makeHeader(funderKeyHash, headerStartTime);
  const anchorHeader = {
    ...baseAnchorHeader,
    endTime:
      baseAnchorHeader.startTime + BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
    utxosRoot: fixture.ledger.rootHex,
  };
  const anchorSetup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: anchorHeader,
  });
  const successorStart = emulatorSuccessorHeaderStart({
    predecessorEndTime: anchorHeader.endTime,
    emulator,
  });
  const header = {
    ...makeHeader(
      funderKeyHash,
      successorStart,
      await countedTransactionsRoot(
        fixture.transactionsRoot,
        fixture.l2TransactionCount,
      ),
      fixture.l2TransactionCount,
    ),
    prevHeaderHash: anchorSetup.headerHash,
    prevUtxosRoot: fixture.ledger.rootHex,
  };
  const commit = await commitHeaderAfterAnchorBlock({
    harness,
    anchorBlockOutRef: anchorSetup.fraudulentBlockOutRef,
    header,
  });
  const setup = {
    ...anchorSetup,
    fraudulentBlockOutRef: commit.blockOutRef,
    headerHash: commit.headerHash,
    stateQueueBlockUnit: commit.stateQueueBlockUnit,
    anchorHeaderHash: anchorSetup.headerHash,
    anchorBlockOutRef: anchorSetup.fraudulentBlockOutRef,
    anchorBlockUnit: anchorSetup.stateQueueBlockUnit,
    witnessReferenceScripts,
  };
  return { header, anchorHeader, setup };
};

/**
 * Publishes all four step validators as reference scripts (production
 * deployment shape per the standing reference-script ruling). Every emulator
 * transaction that spends a step sources its witness from these.
 */
export const publishValueNotPreservedReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: ValueNotPreservedContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `value-not-preserved step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};

// ---------------------------------------------------------------------------
// The honest thread, one call: init → bind → fold* → finish [→ 03 [→ 04]]
// ---------------------------------------------------------------------------

/**
 * Runs the honest submitters over a committed scenario, capturing per-step
 * emulator measurements. `through` picks the stopping point so adversarial
 * suites can park the thread mid-chain and attack from there.
 */
export const runValueNotPreservedThread = async ({
  harness,
  fixture,
  setup,
  refs,
  claimedAsset,
  claimedDirection,
  through = "step04",
}: {
  readonly harness: ValueNotPreservedHarness;
  readonly fixture: ValueNotPreservedFixture;
  readonly setup: {
    readonly fraudulentBlockOutRef: string;
    readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  };
  readonly refs: readonly [UTxO, UTxO, UTxO, UTxO];
  readonly claimedAsset: ClaimedAsset;
  readonly claimedDirection: ClaimedImbalanceDirection;
  readonly through?: "finish" | "step03" | "step04";
}) => {
  const { emulator, proverLucid, proverSigner, family, category } = harness;
  const measurements: Record<string, CompleteSignedTransactionMeasurement> = {};
  const catalogue = {
    policyId: harness.contracts.fraudProofCatalogue.policyId,
    spendingScriptAddress:
      harness.contracts.fraudProofCatalogue.spendingScriptAddress,
    root: harness.catalogue.root,
  };

  const initCapture = await captureEmulatorSubmission(emulator, async () =>
    submitResolvedInit({
      label: VALUE_NOT_PRESERVED_CATEGORY_LABEL,
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network: emulatorNetwork,
      contracts: family,
      category,
      catalogue,
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    }),
  );
  measurements["init"] = initCapture.measurement;
  const init = initCapture.result;

  const step01Capture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep01({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      contracts: family,
      categoryId: category.categoryId,
      network: emulatorNetwork,
      signer: proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      claimedAsset,
      claimedDirection,
      prevUtxosRoot: fixture.ledger.rootHex,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    }),
  );
  measurements["step-01"] = step01Capture.measurement;
  const step01 = step01Capture.result;

  const spendInputsOpening = spendInputsOpeningV1({
    nativeTxCompactCbor: fixture.nativeTxCompactCbor,
    spendInputsPreimageCbor: fixture.spendInputsPreimageCbor,
  });
  let threadOutRef = step01.nextThreadOutRef;
  for (const [index, spent] of fixture.ledger.spentInputs.entries()) {
    const valueWitness = await buildSpentInputValueWitness({
      claim: claimedAsset,
      descriptorCbor: spent.descriptorCbor,
      spentValue: spent.spentValue,
      trie: fixture.ledger.trie,
      input: spent.input,
      prevUtxosRootHex: fixture.ledger.rootHex,
    });
    const foldCapture = await captureEmulatorSubmission(emulator, async () =>
      submitValueNotPreservedStep02Fold({
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef,
        spendInputsOpening,
        valueWitness,
        referenceScriptUtxo: refs[1],
      }),
    );
    measurements[`step-02-fold-${index.toString()}`] = foldCapture.measurement;
    threadOutRef = foldCapture.result.nextThreadOutRef;
  }

  const finishCapture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep02Finish({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef,
      spendInputsOpening,
      spendInputCount: BigInt(fixture.ledger.spentInputs.length),
      referenceScriptUtxo: refs[1],
    }),
  );
  measurements["step-02-finish"] = finishCapture.measurement;
  const finish = finishCapture.result;
  if (through === "finish") {
    return { init, step01, finish, measurements };
  }

  const step03Capture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep03({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: finish.nextThreadOutRef,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      outputs: fixture.outputs,
      mintItems: claimedAsset === "AdaAsset" ? null : fixture.mintItems,
      referenceScriptUtxo: refs[2],
    }),
  );
  measurements["step-03"] = step03Capture.measurement;
  const step03 = step03Capture.result;
  if (through === "step03") {
    return { init, step01, finish, step03, measurements };
  }

  const step04Capture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep04({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: refs[3],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    }),
  );
  measurements["step-04"] = step04Capture.measurement;
  return {
    init,
    step01,
    finish,
    step03,
    step04: step04Capture.result,
    measurements,
  };
};
