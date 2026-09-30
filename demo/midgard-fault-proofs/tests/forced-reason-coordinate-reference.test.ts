import {
  computeHash28,
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitNoReferenceInputForcedStep } from "../src/no-reference-input/submit.js";
import {
  noReferenceInputForcedSourceMaterial,
  type PreparedNoReferenceInputWrongfulRejection,
} from "../src/no-reference-input/wrongful-rejection.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { submitInit } from "../src/submit-init.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { makeReferenceNativeTx } from "./support/no-reference-input-native.js";
import { submitRawNoReferenceInputForcedStep } from "./support/no-reference-input-raw-step.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

/**
 * A forced InputNotFound reason names a reference input by source kind 1 and
 * its field position, and noReferenceInput reopens exactly that input. The
 * block's ledger holds reference 0 and not reference 1, and the verdict is the
 * one the node's classifier writes over that ledger, so the suite fails if the
 * writer and the proof disagree on the source kind or on how references are
 * counted: one position early names a reference the ledger holds and
 * convicts; the written position is refused on chain.
 */

const spent = encodeMidgardSpendInputItem({
  txId: Buffer.alloc(32, 0x56),
  outputIndex: 0,
});
const references = [0, 1].map((outputIndex) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, 0x55), outputIndex }),
);
const heldReference = references[0]!;
const heldOutput = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);
const native = materializeMidgardForcedTxFromCanonical(
  makeReferenceNativeTx({
    spendInputCbors: [spent],
    referenceInputCbors: references,
    fee: 0n,
  }),
);

const writtenReason = async () => {
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(native),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(native),
    ledger: [[heldReference, heldOutput]],
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: { InputNotFound: { source_kind: 1n, input_index: 1n } },
    },
  });
  return { source_kind: 1n, input_index: 1n };
};

const membership = async <K, V>(
  domain: SDK.RootDomain,
  key: K,
  value: V,
  keyCbor: string,
  valueCbor: string,
): Promise<SDK.RootMembershipProof<K, V>> => {
  const keyBytes = Buffer.from(keyCbor, "hex");
  const valueBytes = Buffer.from(valueCbor, "hex");
  const root = await buildCountedRoot(domain, [
    { key: keyBytes, value: valueBytes },
  ]);
  return {
    domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: 1n,
    key,
    value,
    proof: await keyValuePhasProof(
      { ...root, root: root.phasRoot },
      keyBytes,
      valueBytes,
    ),
  };
};

/**
 * One block whose single forced leaf rejects the transaction with
 * `InputNotFound { 1, inputIndex }` over a ledger holding reference 0, and
 * the step runners a noReferenceInput proof drives against it.
 */
const setupScenario = async (inputIndex: bigint) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNoReferenceInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = {
    steps: harness.contracts.fraudProofContracts.noReferenceInput.steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
  };
  const catalogue = await buildCatalogueDeploymentInfo(
    harness.contracts.fraudProofs,
  );
  const categoryId = catalogue.categories.noReferenceInput.categoryId;
  const steps: UTxO[] = [];
  for (const [stepIndex, step] of contracts.steps.entries())
    steps.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: step.spendingScript,
          label: `no-reference-input step ${stepIndex}`,
        })
      ).utxo,
    );
  const referenceScripts = {
    steps,
    computationThreadMint:
      harness.witnessReferenceScripts.computationThreadMint!,
    fraudProofMint: harness.witnessReferenceScripts.fraudProofMint!,
  };
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("operator absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const source = deriveMidgardForcedTxProofSource(native);
  const leaf = {
    tx_id: computeMidgardNativeTxId(native).toString("hex"),
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: {
      ForcedTxInvalid: {
        reason: { InputNotFound: { source_kind: 1n, input_index: inputIndex } },
      },
    },
  } as const;
  const eventKey = base.eventKey;
  const forcedKey = eventKey.ForcedTransactionEventKey.tx_order_id;
  const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: heldReference,
    outputCbor: heldOutput,
  }).descriptorCbor;
  const ledger = await keyValuePhasRootWithCount([
    { key: heldReference, value: descriptor },
  ]);
  const forcedMembership = await membership(
    SDK.ROOT_DOMAINS.forcedTransactionsV1,
    forcedKey,
    leaf,
    Data.to(forcedKey, SDK.OutputReference),
    Data.to(leaf, SDK.ForcedInclusionTxV1),
  );
  const eventValue = { step_index: 0n, phase: "ForcedTransaction" as const };
  const eventMembership = await membership(
    SDK.ROOT_DOMAINS.eventToStep,
    eventKey,
    eventValue,
    Data.to(eventKey, SDK.EventKey),
    Data.to(eventValue, SDK.EventToStepValue),
  );
  const transition: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction",
    pre_utxos_root: ledger.root,
    post_utxos_root: ledger.root,
  };
  const transitionMembership = await membership(
    SDK.ROOT_DOMAINS.transitionTrace,
    0n,
    transition,
    Data.to(0n),
    Data.to(transition, SDK.TransitionStep),
  );
  const header = {
    ...base.header,
    blockSlot: 10n,
    forcedTransactionsRoot: forcedMembership.root,
    eventToStepRoot: eventMembership.root,
    transitionTraceRoot: transitionMembership.root,
    utxosRoot: ledger.root,
  };
  const forcedSource = { header, membership: forcedMembership, direction: 1n };
  const fullTransactionCbor =
    encodeMidgardForcedTxCanonical(native).toString("hex");
  const prepared: PreparedNoReferenceInputWrongfulRejection = {
    headerHash: computeHash28(SDK.encodeHeaderCbor(header)).toString("hex"),
    forcedSource,
    fullTransactionCbor,
    ...noReferenceInputForcedSourceMaterial(forcedSource, fullTransactionCbor),
    eventMembership,
    transitionMembership,
    // The held reference's genuine membership, whichever input is named.
    ledgerMembership: {
      value: descriptor.toString("hex"),
      proof: await keyValuePhasProof(ledger, heldReference, descriptor),
    },
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId,
    signer: harness.proverSigner,
    prepared,
  };
  const init = async () => {
    const result = await submitInit({
      lucid: harness.proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(harness.contracts, catalogue),
      network: "Custom",
      signer: harness.proverSigner,
      fraudCategory: "noReferenceInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    return `${result.txHash}#${result.firstStepOutputIndex}`;
  };
  const step = (threadOutRef: string, stepIndex: 0 | 1 | 2 | 3) =>
    submitNoReferenceInputForcedStep({
      ...common,
      threadOutRef,
      stepIndex,
      referenceScripts,
      carriageUtxos: [],
      certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
    });
  const rawStep = (threadOutRef: string, stepIndex: 0 | 1 | 2 | 3) =>
    submitRawNoReferenceInputForcedStep({
      ...common,
      threadOutRef,
      stepIndex,
      references: referenceScripts,
    });
  const remove = async () => {
    const removal = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    return submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(harness.contracts, catalogue, {
        removalReferenceScripts: removal.published,
      }),
      network: "Custom",
      signer: harness.proverSigner,
      fraudCategory: "noReferenceInput",
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "no-reference-input-lease",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
  };
  return { prepared, init, step, rawStep, remove };
};

describe("forced reference InputNotFound coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the ledger holds the reference", async () => {
    const written = await writtenReason();
    const s = await setupScenario(written.input_index - 1n);
    expect(s.prepared.selectedInput).not.toBeNull();
    let thread = await s.init();
    for (const stepIndex of [0, 1, 2] as const)
      thread = (await s.step(thread, stepIndex)).nextThreadOutRef!;
    expect((await s.step(thread, 3)).fraudProofUnit).toBeTruthy();
    await s.remove();
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const s = await setupScenario((await writtenReason()).input_index);
    let thread = await s.init();
    for (const stepIndex of [0, 1, 2] as const)
      thread = await s.rawStep(thread, stepIndex);
    // The only membership the ledger can prove is reference 0's. The step
    // authenticates its inputs and the absence rule returns false.
    await expectOnchainRefusal(() => s.rawStep(thread, 3), {
      refusedBy: "fraud_proofs/no_reference_input/step_04",
      check: /^Validator returned false$/u,
    });
  }, 600_000);
});
