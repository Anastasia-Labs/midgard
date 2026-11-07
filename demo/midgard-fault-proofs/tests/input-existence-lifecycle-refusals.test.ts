/** The refusals of the registered input-existence lifecycle, each pinned to
 * the step validator and the check that refuses it: the terminal verdict for
 * a present input under a missing claim and for an absent input under an
 * honest rejection, and the pre-state ledger state for an absence carried
 * against the block-initial ledger. */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import type { Script, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  type PreparedInputExistence,
  prepareInputExistence,
} from "../src/input-existence/prepare.js";
import { submitInputExistenceStep } from "../src/input-existence/submit.js";
import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import { submitInit } from "../src/submit-init.js";
import { keyValuePhasNonMembershipProof } from "../src/transition-trace/phas.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import type { InputChronology } from "./helpers/input-existence-chronology-fixture.js";
import {
  expectOnchainRefusal,
  type RefusalPin,
} from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  checkStepFit,
  startChronologyJourney,
} from "./support/input-existence-chronology-journey.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import { registerPexcludesExclusionRewardAccount } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const TIMEOUT = 600_000;
const FIELDS = [0, 1] as const;
/** The physical step validator that holds the refusing check. */
const refusedAt = (field: 0 | 1, step: 3 | 4, check: RegExp): RefusalPin =>
  field === 0
    ? step === 3
      ? { refusedBy: "fraud_proofs/no_input/step_03", check }
      : { refusedBy: "fraud_proofs/no_input/step_04", check }
    : step === 3
      ? { refusedBy: "fraud_proofs/no_reference_input/step_03", check }
      : { refusedBy: "fraud_proofs/no_reference_input/step_04", check };
/** Checks, as their verbose traces name them. Both are the boolean result of
 * the step: the pre-state step's output ledger state, and the terminal
 * step's verdict. */
const CHECK = {
  ledgerState: /^Validator returned false$/u,
  verdict: /^Validator returned false$/u,
};

/** A forced rejection of an input that no ledger holds: one forced
 * transaction over the empty prior ledger, rejected with InputNotFound for
 * its only input of `field`. */
const startHonestRejectionJourney = async (field: 0 | 1) => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNonExistentInput: field === 0,
      realNoReferenceInput: field === 1,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerPexcludesExclusionRewardAccount,
  });
  const category = field === 0 ? "nonExistentInput" : "noReferenceInput";
  const family = h.contracts.fraudProofContracts[category];
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x66),
    outputIndex: 0,
  });
  const nativeTx = makeNativeTx({
    spendInputCbors: field === 0 ? [key] : [],
    ...(field === 1 ? { referenceByte: "66" } : {}),
    fee: 0n,
  });
  const reason: SDK.RejectionReason = {
    InputNotFound: { source_kind: BigInt(field), input_index: 0n },
  };
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(h.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120_000,
      ) - 1,
    ),
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject: {
      kind: "forced",
      nativeTx,
      orderKey: { transactionId: "77".repeat(32), outputIndex: 0n },
      verdict: { ForcedTxInvalid: { reason } },
    },
  });
  const orderKey = block.forcedOrderKey;
  if (orderKey === undefined || orderKey === null)
    throw new Error("forced fixture has no authenticated order key");
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "emulator/input-existence",
      grade: "security",
    },
  });
  const setup = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: evidence.header,
  });
  expect(setup.headerHash).toBe(evidence.headerHash);
  const prepared = await prepareInputExistence({
    block: evidence,
    eventKey: {
      ForcedTransactionEventKey: { tx_order_id: orderKey },
    },
    field,
    inputIndex: undefined,
    predecessor: await checkLedgerAgainstRoot({
      entries: [],
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      label: "registered prior-block ledger",
    }),
    owner: h.proverSigner.paymentKeyHash,
  });
  expect(prepared.source.subject.direction).toBe(1n);
  expect(prepared.ledgerEvidence).toHaveProperty("Absent");
  const steps: UTxO[] = [];
  for (const [index, step] of family.steps.entries())
    steps.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script: step.spendingScript,
          label: `input existence ${category} step${index + 1}`,
        })
      ).utxo,
    );
  const publishMint = async (script: Script, label: string) =>
    (
      await publishPlainReferenceScriptUtxo({
        lucid: h.funderLucid,
        script,
        label,
      })
    ).utxo;
  const referenceScripts = {
    steps,
    computationThreadMint: await publishMint(
      h.contracts.computationThread.mintingScript,
      "input existence CT mint",
    ),
    fraudProofMint: await publishMint(
      h.contracts.fraudProof.mintingScript,
      "input existence proof mint",
    ),
  };
  const init = await captureEmulatorSubmission(h.emulator, () =>
    submitInit({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
      network,
      signer: h.proverSigner,
      fraudCategory: category,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: h.witnessReferenceScripts,
    }),
  );
  checkStepFit(init.measurement);
  let threadOutRef = `${init.result.txHash}#${init.result.firstStepOutputIndex}`;
  const send = async (stepIndex: 0 | 1 | 2 | 3) => {
    const submitted = await captureEmulatorSubmission(h.emulator, () =>
      submitInputExistenceStep({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        network,
        contracts: {
          hubOraclePolicyId: h.contracts.hubOracle.policyId,
          steps: family.steps,
          computationThread: h.contracts.computationThread,
          fraudProof: h.contracts.fraudProof,
        },
        categoryId: h.catalogue.categories[category].categoryId,
        signer: h.proverSigner,
        threadOutRef,
        prepared,
        stepIndex,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        referenceScripts,
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
    checkStepFit(submitted.measurement);
    threadOutRef = submitted.result.nextThreadOutRef;
    return submitted.result;
  };
  const expectThreadKept = async () => {
    const [txHash, index] = threadOutRef.split("#");
    expect(
      await h.proverLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(index) },
      ]),
    ).toHaveLength(1);
  };
  return { send, expectThreadKept };
};

const withAbsence = async (
  prepared: PreparedInputExistence,
  ledger: Parameters<typeof keyValuePhasNonMembershipProof>[0],
  key: Buffer,
): Promise<PreparedInputExistence> => ({
  ...prepared,
  ledgerEvidence: {
    Absent: { proof: await keyValuePhasNonMembershipProof(ledger, key) },
  },
});

const presentUnderMissingClaim = [
  "forced_creator",
  "normal_creator",
  "prior_deposit",
  "forced_prior_deposit",
] as const satisfies readonly InputChronology[];
const byField = <T extends string>(rows: readonly T[]) =>
  FIELDS.flatMap((field) => rows.map((kind) => ({ field, kind })));

describe("input-existence lifecycle refusals, by the check that refuses", () => {
  it.each(byField(presentUnderMissingClaim))(
    "field $field: refuses a missing claim for a present input at the verdict for $kind",
    async ({ field, kind }) => {
      const j = await startChronologyJourney({ field, scenario: kind });
      expect(j.prepared.source.subject.direction).toBe(0n);
      expect(j.prepared.ledgerEvidence).toHaveProperty("Present");
      for (const stepIndex of [0, 1, 2] as const) await j.send(stepIndex);
      await expectOnchainRefusal(
        () => j.send(3),
        refusedAt(field, 4, CHECK.verdict),
      );
      await j.expectThreadKept();
    },
    TIMEOUT,
  );

  it.each(FIELDS)(
    "field %s: refuses an absence under an honest rejection of an input absent at its pre-root",
    async (field) => {
      const j = await startChronologyJourney({
        field,
        scenario: "absent_input_rejection",
      });
      expect(j.prepared.source.subject.direction).toBe(1n);
      expect(j.prepared.ledgerEvidence).toHaveProperty("Absent");
      for (const stepIndex of [0, 1, 2] as const) await j.send(stepIndex);
      await expectOnchainRefusal(
        () => j.send(3),
        refusedAt(field, 4, CHECK.verdict),
      );
      await j.expectThreadKept();
    },
    TIMEOUT,
  );

  it.each(FIELDS)(
    "field %s: refuses an absence under an honest rejection of an input no ledger holds",
    async (field) => {
      const j = await startHonestRejectionJourney(field);
      for (const stepIndex of [0, 1, 2] as const) await j.send(stepIndex);
      await expectOnchainRefusal(
        () => j.send(3),
        refusedAt(field, 4, CHECK.verdict),
      );
      await j.expectThreadKept();
    },
    TIMEOUT,
  );

  // An earlier event of the block creates the input, so the block-initial
  // ledger lacks it; the pre-state step binds the target event's own
  // pre-root instead.
  it.each(byField(["forced_creator", "normal_creator"] as const))(
    "field $field: refuses an absence against the block-initial ledger at the pre-state step for $kind",
    async ({ field, kind }) => {
      const j = await startChronologyJourney({ field, scenario: kind });
      const { history } = j;
      expect(history.targetPreRoot).not.toBe(history.predecessor.utxosRoot);
      expect(history.predecessor.utxosRoot).toBe(
        history.evidence.header.prevUtxosRoot,
      );
      expect(
        history.predecessor.trie.entries.some(({ key }) =>
          key.equals(history.selected),
        ),
      ).toBe(false);
      const carriage = {
        raw: true,
        prepared: await withAbsence(
          j.prepared,
          history.predecessor.trie,
          history.selected,
        ),
        claimedPreUtxosRoot: history.evidence.header.prevUtxosRoot,
      };
      await j.send(0, carriage);
      await j.send(1, carriage);
      await expectOnchainRefusal(
        () => j.send(2, carriage),
        refusedAt(field, 3, CHECK.ledgerState),
      );
      await j.expectThreadKept();
    },
    TIMEOUT,
  );
});
