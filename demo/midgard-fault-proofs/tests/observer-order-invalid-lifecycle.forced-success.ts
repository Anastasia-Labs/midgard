import { midgardFieldCommitment } from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  type ObserverOrderInvalidEvidence,
  observerOrderInvalidEvidenceCloses,
  type ObserverOrderInvalidFinding,
  prepareObserverOrderInvalidEvidence,
} from "../src/observer-order-invalid/family.js";
import {
  type ObserverOrderInvalidStagedPlan,
  planObserverOrderInvalidStagedWalk,
} from "../src/observer-order-invalid/staged-plan.js";
import {
  coverage,
  LAST_ORDINAL,
  MAXIMUM_FIELD_BYTES,
  MAXIMUM_OBSERVERS,
  REASON_ARM,
  record,
} from "./observer-order-invalid-lifecycle.authentication-seams.js";
import { makeHarness } from "./observer-order-invalid-lifecycle.make-harness.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  ascendingObservers,
  buildAcceptedObserverInclusions,
  buildForcedObserverLeaf,
  type ForcedObserverLeaf,
  observerAt,
  type ObserverFieldShape,
  observerFieldShape,
  transactionIdOf,
} from "./support/observer-order-invalid-raw.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";

// ---------------------------------------------------------------------------
// Shapes: every ordering polarity at first, middle, last and duplicate ordinals
// ---------------------------------------------------------------------------

/** Ascending, with the last adjacent pair swapped: the offence is ordinal 1091. */
export const maximumLastViolationShape = () => {
  const observers = ascendingObservers(MAXIMUM_OBSERVERS);
  observers[LAST_ORDINAL - 1] = observerAt(LAST_ORDINAL);
  observers[LAST_ORDINAL] = observerAt(LAST_ORDINAL - 1);
  return observerFieldShape({
    label: `${MAXIMUM_OBSERVERS.toString()} observers, last pair descending (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observers,
  });
};

export const maximumOrderedShape = () =>
  observerFieldShape({
    label: `${MAXIMUM_OBSERVERS.toString()} strictly ascending observers (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observers: ascendingObservers(MAXIMUM_OBSERVERS),
  });

export const firstPairDescendingShape = () =>
  observerFieldShape({
    label: "2 observers, first pair descending (published inline field)",
    observers: [observerAt(1), observerAt(0)],
    fee: 11n,
  });

export const middleDuplicateShape = () =>
  observerFieldShape({
    label: "5 observers, duplicate at ordinal 2 (published inline field)",
    observers: [
      observerAt(0),
      observerAt(1),
      observerAt(1),
      observerAt(2),
      observerAt(3),
    ],
    fee: 13n,
  });

export const smallOrderedShape = () =>
  observerFieldShape({
    label: "5 strictly ascending observers (published inline field)",
    observers: ascendingObservers(5),
    fee: 17n,
  });

export const twoOrderedShape = () =>
  observerFieldShape({
    label: "2 strictly ascending observers (published inline field)",
    observers: ascendingObservers(2),
  });

export const emptyShape = () =>
  observerFieldShape({
    label: "0 observers (published inline field)",
    observers: [],
  });

export const singleShape = () =>
  observerFieldShape({
    label: "1 observer (published inline field)",
    observers: [observerAt(3)],
  });

export const duplicateFirstShape = () =>
  observerFieldShape({
    label: "3 observers, duplicate at ordinal 1 (published inline field)",
    observers: [observerAt(0), observerAt(0), observerAt(1)],
  });

export const earlierViolationShape = () =>
  observerFieldShape({
    label: "3 observers, descending at ordinal 1, ascending at ordinal 2",
    observers: [observerAt(1), observerAt(0), observerAt(2)],
  });

export const reasonAt = (observerIndex: number): RejectionReason => ({
  ObserverOrderInvalid: { observer_index: BigInt(observerIndex) },
});

export const acceptedFinding = (
  shape: ObserverFieldShape,
  observerIndex: number,
): ObserverOrderInvalidFinding => ({
  subject: acceptedVerdictSubject(transactionIdOf(shape)),
  observerIndex,
});

export const evidenceOf = (
  shape: ObserverFieldShape,
  finding: ObserverOrderInvalidFinding,
): ObserverOrderInvalidEvidence =>
  prepareObserverOrderInvalidEvidence({
    finding,
    fieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });

export const stagedOf = (
  shape: ObserverFieldShape,
  observerIndex: number,
): ObserverOrderInvalidStagedPlan =>
  planObserverOrderInvalidStagedWalk({
    transactionId: transactionIdOf(shape),
    fieldPreimageCbor: Buffer.from(shape.fieldPreimage).toString("hex"),
    observerIndex,
  });

export const forcedFinding = (
  leaf: ForcedObserverLeaf,
  sourceKey: { transactionId: string; outputIndex: bigint },
  observerIndex: number,
  rejectionReason: RejectionReason = reasonAt(observerIndex),
): ObserverOrderInvalidFinding => ({
  subject: forcedVerdictSubject({
    transactionId: leaf.transactionId,
    sourceKey,
    rejectionReason,
  }),
  observerIndex,
});

type Harness = Awaited<ReturnType<typeof makeHarness>>;

export const acceptedBlock = async (
  h: Harness,
  shapes: readonly ObserverFieldShape[],
) => {
  const block = await buildAcceptedObserverInclusions(
    shapes.map((shape) => shape.nativeTx),
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

/**
 * One rejected forced leaf typed with `rejectionReason` under a header the
 * committed block carries; the prover's finding claims this family's reason
 * at `observerIndex`.
 */
export const forcedBlock = async (
  h: Harness,
  shape: ObserverFieldShape,
  observerIndex: number,
  rejectionReason: RejectionReason = reasonAt(observerIndex),
) => {
  const credential = getAddressDetails(
    await h.harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("forced funder key absent");
  const baseFixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.harness.funderLucid,
        h.harness.emulator.now() + 120_000,
      ) - 1,
  });
  const sourceKey = baseFixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const leaf = await buildForcedObserverLeaf({
    shape,
    sourceKey,
    rejectionReason,
  });
  const header = {
    ...baseFixture.header,
    forcedTransactionsRoot: leaf.root.root,
  };
  const setup = await submitSetupTx({
    lucid: h.harness.funderLucid,
    contracts: h.harness.contracts,
    nonceUtxo: h.harness.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  await h.publishReferences();
  const finding = forcedFinding(leaf, sourceKey, observerIndex);
  const source = { header, membership: leaf.membership, direction: 1n };
  return { setup, header, leaf, sourceKey, finding, source };
};

type ForcedContext = Harness &
  Awaited<ReturnType<typeof forcedBlock>> & {
    readonly shape: ObserverFieldShape;
    readonly evidence: ObserverOrderInvalidEvidence;
    readonly staged: ObserverOrderInvalidStagedPlan;
  };

/**
 * Init -> forced step 01 -> step 02 -> every scan -> step 04 proof mint ->
 * removal, every transaction recorded under `prefix`.
 */
export const forcedSuccess = async (
  prefix: string,
  shape: ObserverFieldShape,
  observerIndex: number,
  beforeRemoval: (context: ForcedContext) => Promise<void> = async () => {},
) => {
  const h = await makeHarness();
  const block = await forcedBlock(h, shape, observerIndex);
  const { setup, finding, source } = block;
  const evidence = evidenceOf(shape, finding);
  const staged = stagedOf(shape, observerIndex);
  expect(evidence.violation).toBe(false);
  expect(observerOrderInvalidEvidenceCloses(evidence)).toBe(true);
  const initialized = await h.init(
    setup.fraudulentBlockOutRef,
    setup.headerHash,
  );
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Forced(
    h.threadOf(initialized.result),
    finding,
    source,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const published = await h.publishField(shape);
  if (published.tier === "Certified")
    h.recordCarriage(prefix, shape, published);
  const opened = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
  );
  record(`${prefix}-step02`, shape.label, opened.measurement);
  const decided = await h.scanAll(
    opened.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
    prefix,
  );
  const proven = await h.step04(decided, evidence);
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step04-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "forced_rejection_wrong");
  coverage.scenario("wrongful_forced_rejection_success");
  await beforeRemoval({ ...h, ...block, shape, evidence, staged });
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};
