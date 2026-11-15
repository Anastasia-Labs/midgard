/**
 * Forced rejections redeemerCanonicity answers by field 8's authenticated
 * count rather than an item: a `RedeemerMalformed` naming a redeemer the
 * field does not have (a negative coordinate, or one past the last), and a
 * `PlutusExecutionFailed` of a transaction with no redeemers, which runs no
 * Plutus script. Step 02 writes the canonical verdict straight to the
 * terminal, which convicts. A claim that a held redeemer is absent, or that a
 * transaction with redeemers has none, is refused on chain; the absent
 * redeemer route also convicts at the field's byte cap through the field
 * grammar certificate.
 */
import {
  decodeMidgardForcedTxCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  MIDGARD_CONSENSUS_LIMITS,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { FIELD_GRAMMAR_WALKED_COUNT_LIMIT } from "../src/field-grammar-certificate.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  prepareRedeemerCanonicityEvidence,
  REDEEMER_CANONICITY_EXECUTION_FAILED_INDEX,
  type RedeemerCanonicityEvidence,
  submitRedeemerCanonicityStep01Forced,
  submitRedeemerCanonicityStep02,
  submitRedeemerCanonicityStep03,
} from "../src/redeemer-canonicity/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  familyContracts,
  initThread,
  publishFamilyReferences,
} from "./redeemer-canonicity-lifecycle.init-thread.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { deployHarnessFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;

/** A Spend redeemer at `index` with the smallest data. */
const redeemerItem = (index: number) =>
  encodeMidgardRedeemerWitnessItem({
    purpose: "Spend",
    index: BigInt(index),
    redeemerCbor: Buffer.from("00", "hex"),
    executionUnits: { memory: 1n, steps: 2n },
  });

const redeemersOf = (count: number) =>
  Array.from({ length: count }, (_, index) => redeemerItem(index));

/** The largest redeemer count whose field 8 fits the aggregate field cap. */
const maximumRedeemerCount = (): number => {
  let low = 1;
  let high = FIELD_CAP;
  while (low < high) {
    const middle = Math.ceil((low + high) / 2);
    if (encodeMidgardFieldPreimage(redeemersOf(middle)).length <= FIELD_CAP)
      low = middle;
    else high = middle - 1;
  }
  return low;
};

type ForcedShape = {
  /** Field 8 of the forced transaction. */
  readonly redeemers: readonly Buffer[];
  /** Rejects for `RedeemerMalformed` at this index; a Plutus execution failure when absent. */
  readonly redeemerMalformedIndex?: number;
};

/**
 * A block whose forced leaf rejects a transaction of `shape`, with a thread
 * bound through step 01 to the rejection's redeemer coordinate.
 */
const boundForcedThread = async (
  shape: ForcedShape,
  {
    withFieldGrammar = false,
    claimAbsent = false,
  }: {
    readonly withFieldGrammar?: boolean;
    /** Binds a claim that the coordinate is absent, whatever field 8 holds. */
    readonly claimAbsent?: boolean;
  } = {},
) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realRedeemerCanonicity: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = familyContracts(harness);
  const category = harness.catalogue.categories.redeemerCanonicity;
  if (category === undefined) throw new Error("redeemer category absent");
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("missing funder key");
  const forced = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    redeemers: shape.redeemers,
    ...(shape.redeemerMalformedIndex === undefined
      ? {}
      : { redeemerMalformedIndex: shape.redeemerMalformedIndex }),
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: forced.header,
  });
  const references = await publishFamilyReferences(harness);
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: "redeemer-canonicity-certificate",
    })
  ).utxo;
  const grammar = withFieldGrammar
    ? await deployHarnessFieldGrammarCertificate(harness, "redeemerCanonicity")
    : undefined;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  const rejectionReason =
    shape.redeemerMalformedIndex === undefined
      ? ({
          PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 0n },
        } as const)
      : ({
          RedeemerMalformed: {
            redeemer_index: BigInt(shape.redeemerMalformedIndex),
          },
        } as const);
  const fieldPreimage =
    forced.forcedNativeTx.witnessSet.redeemerTxWitsPreimageCbor;
  const finding = {
    subject: forcedVerdictSubject({
      transactionId: forced.forcedTransaction.tx_id,
      sourceKey: membership.key,
      rejectionReason,
    }),
    redeemerIndex:
      shape.redeemerMalformedIndex ??
      REDEEMER_CANONICITY_EXECUTION_FAILED_INDEX,
  };
  const prepare = () =>
    prepareRedeemerCanonicityEvidence({
      finding,
      fieldPreimage,
      committedFieldHashHex:
        midgardFieldCommitment(fieldPreimage).toString("hex"),
    });
  const evidence: RedeemerCanonicityEvidence = claimAbsent
    ? Object.freeze({
        ...finding,
        fieldPreimageHex: fieldPreimage.toString("hex"),
        fieldCommitmentHex:
          midgardFieldCommitment(fieldPreimage).toString("hex"),
        itemHex: "",
        itemCount: shape.redeemers.length,
        canonical: true,
        dataPlan: null,
        carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
        absent: true,
      })
    : prepare();
  const step = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };
  const init = await initThread({
    harness,
    contracts,
    category,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
  });
  const bound = await submitRedeemerCanonicityStep01Forced({
    ...step,
    threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
    finding: evidence,
    forcedSource: { header: forced.header, membership },
    witnessSetHash: Buffer.from(
      decodeMidgardForcedTxCompact(
        Buffer.from(
          forced.forcedTransaction.submitted_source.compact_cbor,
          "hex",
        ),
      ).transactionWitnessSetHash,
    ).toString("hex"),
    referenceScriptUtxo: references[0],
  });
  /** Step 02 from the bound thread for `claimed`. */
  const authenticate = async (claimed: RedeemerCanonicityEvidence) =>
    await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep02({
        ...step,
        threadOutRef: bound.nextThreadOutRef,
        evidence: claimed,
        nativeTxCompactCbor:
          forced.forcedTransaction.submitted_source.compact_cbor,
        witnessSetCompactCbor:
          forced.forcedTransaction.submitted_source.witness_set_compact_cbor,
        referenceScriptUtxo: references[1],
        certificateReferenceScriptUtxo: certificateReference,
        ...(grammar === undefined
          ? {}
          : { fieldGrammar: grammar.fieldGrammar }),
      }),
    );
  const mint = async (threadOutRef: string) =>
    await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep03({
        ...step,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[3],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  return { evidence, prepare, authenticate, mint };
};

const refusedAtStep02 = {
  refusedBy: "fraud_proofs/redeemer_canonicity/step_02",
  check: /^Validator returned false$/u,
};

describe("redeemer-canonicity forced rejection of an absent coordinate", () => {
  it.each([
    { coordinate: "past the last", redeemerMalformedIndex: 1 },
    { coordinate: "negative", redeemerMalformedIndex: -1 },
  ])(
    "convicts a RedeemerMalformed naming a $coordinate redeemer",
    async ({ redeemerMalformedIndex }) => {
      const thread = await boundForcedThread({
        redeemers: redeemersOf(1),
        redeemerMalformedIndex,
      });
      expect(thread.evidence.absent).toBe(true);
      const authenticated = await thread.authenticate(thread.evidence);
      expect(authenticated.result.nextStage).toBe("step03");
      const minted = await thread.mint(authenticated.result.nextThreadOutRef);
      expect(minted.result.fraudProofUnit).toBeTruthy();
    },
    600_000,
  );

  it("refuses on chain a claim that a held redeemer is absent", async () => {
    const thread = await boundForcedThread(
      { redeemers: redeemersOf(1), redeemerMalformedIndex: 0 },
      { claimAbsent: true },
    );
    expect(thread.prepare().absent).toBe(false);
    await expectOnchainRefusal(
      async () => await thread.authenticate(thread.evidence),
      refusedAtStep02,
    );
  }, 600_000);

  it("convicts a Plutus execution failure of a transaction with no redeemers", async () => {
    const thread = await boundForcedThread({ redeemers: [] });
    expect(thread.evidence.absent).toBe(true);
    expect(thread.evidence.redeemerIndex).toBe(
      REDEEMER_CANONICITY_EXECUTION_FAILED_INDEX,
    );
    const authenticated = await thread.authenticate(thread.evidence);
    expect(authenticated.result.nextStage).toBe("step03");
    const minted = await thread.mint(authenticated.result.nextThreadOutRef);
    expect(minted.result.fraudProofUnit).toBeTruthy();
  }, 600_000);

  it("refuses on chain a Plutus execution failure claim against a transaction with redeemers", async () => {
    const thread = await boundForcedThread(
      { redeemers: redeemersOf(1) },
      { claimAbsent: true },
    );
    // A transaction with redeemers can fail a Plutus execution, so the
    // off-chain proof refuses to build the claim.
    expect(thread.prepare).toThrow(/field 8 carries redeemers/u);
    await expectOnchainRefusal(
      async () => await thread.authenticate(thread.evidence),
      refusedAtStep02,
    );
  }, 600_000);

  it.each([
    { coordinate: "past the last", negative: false },
    { coordinate: "negative", negative: true },
  ])(
    "convicts a $coordinate RedeemerMalformed over a field 8 at its byte cap through the field grammar certificate",
    async ({ negative }) => {
      const count = maximumRedeemerCount();
      expect(count).toBeGreaterThan(FIELD_GRAMMAR_WALKED_COUNT_LIMIT);
      const redeemerMalformedIndex = negative ? -1 : count;
      const thread = await boundForcedThread(
        { redeemers: redeemersOf(count), redeemerMalformedIndex },
        { withFieldGrammar: true },
      );
      expect(thread.evidence.absent).toBe(true);
      expect(thread.evidence.carriage).toBe("Certified");
      const authenticated = await thread.authenticate(thread.evidence);
      expect(authenticated.result.nextStage).toBe("step03");
      const minted = await thread.mint(authenticated.result.nextThreadOutRef);
      expect(minted.result.fraudProofUnit).toBeTruthy();
      for (const { measurement } of [authenticated, minted]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent redeemer (${count.toString()} redeemers, coordinate ${redeemerMalformedIndex.toString()}): step-02 mem=${authenticated.measurement.executionMemory.toString()} cpu=${authenticated.measurement.executionSteps.toString()}; step-03 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
      );
    },
    1_800_000,
  );
});
