import {
  computeMidgardNativeTxId,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  type FieldOpening,
  forcedVerdictSubject,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  type FieldItemWidthEvidence,
  prepareFieldItemWidthEvidence,
} from "../src/field-item-width-illegal/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { makeHarness } from "./field-item-width-illegal-lifecycle.make-harness.js";
import {
  coverage,
  REASON_ARM,
  record,
} from "./field-item-width-illegal-lifecycle.record.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildAcceptedWidthInclusions,
  buildWidthForcedFixture,
  type WidthShape,
} from "./support/field-item-width-illegal-shapes.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";

export const widthReason = (
  fieldIndex: number,
  itemIndex: number,
): RejectionReason =>
  ({
    FieldItemWidthIllegal: {
      field_index: BigInt(fieldIndex),
      item_index: BigInt(itemIndex),
    },
  }) as const;

export const acceptedEvidence = (shape: WidthShape): FieldItemWidthEvidence =>
  prepareFieldItemWidthEvidence({
    finding: {
      subject: acceptedVerdictSubject(
        computeMidgardNativeTxId(shape.nativeTx).toString("hex"),
      ),
      fieldIndex: shape.fieldIndex,
      itemIndex: shape.itemIndex,
    },
    fieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });

type CertifiedCarriage = {
  cert_ref_input_index: bigint;
  chunk_ref_input_indices: bigint[];
};

export const mutateCertifiedCarriage = (
  opening: FieldOpening,
  patch: (carriage: CertifiedCarriage) => CertifiedCarriage,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("Certified" in carriage))
    throw new Error("certified carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        Certified: patch({
          cert_ref_input_index: carriage.Certified.cert_ref_input_index,
          chunk_ref_input_indices: [
            ...carriage.Certified.chunk_ref_input_indices,
          ],
        }),
      },
    },
  };
};

type Harness = Awaited<ReturnType<typeof makeHarness>>;

/** The chunk publications, the certificate, then the step: recorded apart. */
export const recordCarriage = (
  prefix: string,
  shape: string,
  captured: Awaited<ReturnType<Harness["step02"]>>,
  expectedChunks: number,
): void => {
  const all = captured.measurements;
  if (expectedChunks === 0) {
    expect(all).toHaveLength(1);
  } else {
    expect(all).toHaveLength(expectedChunks + 2);
    for (let index = 0; index < expectedChunks; index += 1)
      record(
        `${prefix}-carriage-chunk0${(index + 1).toString()}`,
        shape,
        all[index]!,
      );
    record(`${prefix}-carriage-certificate`, shape, all[expectedChunks]!);
  }
  record(`${prefix}-step02`, shape, captured.measurement);
};

export const acceptedBlock = async (h: Harness, transactions: WidthShape[]) => {
  const block = await buildAcceptedWidthInclusions(
    transactions.map((shape) => shape.nativeTx),
  );
  const setup = await setupFraudulentBlock({
    funderLucid: h.harness.funderLucid,
    emulator: h.harness.emulator,
    contracts: h.harness.contracts,
    catalogue: h.catalogue,
    fixture: {
      transactionsRoot: block.transactionsRoot,
      l2TransactionCount: block.l2TransactionCount,
    },
  });
  await h.publishReferences();
  return { setup, inclusions: block.inclusions };
};

const forcedBlock = async (h: Harness, shape: WidthShape) => {
  const credential = getAddressDetails(
    await h.harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("missing funder key");
  const forced = await buildWidthForcedFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.harness.funderLucid,
        h.harness.emulator.now() + 120_000,
      ) - 1,
    nativeTx: shape.nativeTx,
    rejectionReason: widthReason(shape.fieldIndex, shape.itemIndex),
  });
  const setup = await submitSetupTx({
    lucid: h.harness.funderLucid,
    contracts: h.harness.contracts,
    nonceUtxo: h.harness.nonceUtxo,
    catalogue: h.catalogue,
    header: forced.header,
  });
  await h.publishReferences();
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  const evidence = prepareFieldItemWidthEvidence({
    finding: {
      subject: forcedVerdictSubject({
        transactionId: forced.transaction.tx_id,
        sourceKey: membership.key,
        rejectionReason: forced.rejectionReason,
      }),
      fieldIndex: shape.fieldIndex,
      itemIndex: shape.itemIndex,
    },
    fieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });
  expect(evidence.decisiveFaultHolds).toBe(shape.illegal);
  const source = { header: forced.header, membership, direction: 1n };
  return { forced, setup, membership, evidence, source };
};

/** Init → forced step 01 → step 02 → proof mint → removal, all recorded. */
export const forcedSuccess = async (
  prefix: string,
  shape: WidthShape,
  expectedChunks: number,
  beforeRemoval: (
    context: Harness & Awaited<ReturnType<typeof forcedBlock>>,
  ) => Promise<void> = async () => {},
) => {
  const h = await makeHarness();
  const block = await forcedBlock(h, shape);
  const { setup, evidence, source } = block;
  const initialized = await h.init(setup.fraudulentBlockOutRef);
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Forced(
    h.threadOf(initialized.result),
    evidence,
    source,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const authenticated = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
  );
  recordCarriage(prefix, shape.label, authenticated, expectedChunks);
  const proven = await h.step03(
    authenticated.result.nextThreadOutRef,
    evidence,
  );
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step03-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "forced_rejection_wrong");
  coverage.scenario("wrongful_forced_rejection_success");
  await beforeRemoval({ ...h, ...block });
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};

/** Init → forced step 01 → step 02, then the terminal step must refuse. */
export const forcedHonestRefusal = async (shape: WidthShape) => {
  const h = await makeHarness();
  const { setup, evidence, source } = await forcedBlock(h, shape);
  const bound = await h.step01Forced(
    h.threadOf((await h.init(setup.fraudulentBlockOutRef)).result),
    evidence,
    source,
  );
  const authenticated = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
  );
  await expectOnchainRefusal(() =>
    h.step03Raw(authenticated.result.nextThreadOutRef),
  );
  coverage.scenario("honest_forced_rejection_refusal");
};
