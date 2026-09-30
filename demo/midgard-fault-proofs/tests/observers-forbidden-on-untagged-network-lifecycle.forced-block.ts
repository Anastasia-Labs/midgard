import { midgardFieldCommitment } from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  type ObserversForbiddenEvidence,
  observersForbiddenEvidenceCloses,
  type ObserversForbiddenFinding,
  prepareObserversForbiddenEvidence,
} from "../src/observers-forbidden-on-untagged-network/family.js";
import { makeHarness } from "./observers-forbidden-on-untagged-network-lifecycle.make-harness.js";
import {
  ABSENT_HASH,
  coverage,
  MAXIMUM_FIELD_BYTES,
  MAXIMUM_OBSERVERS,
  PRESENT_HASH,
  REASON_ARM,
  record,
} from "./observers-forbidden-on-untagged-network-lifecycle.record.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildAcceptedObserverInclusions,
  buildForcedObserverLeaf,
  type ForcedObserverLeaf,
  type ObserverShape,
  observerShape,
  transactionIdOf,
} from "./support/observers-forbidden-on-untagged-network-raw.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";

// ---------------------------------------------------------------------------
// Shapes: every polarity of observer count, network scalar, integrity hash
// ---------------------------------------------------------------------------

export const maximumUntaggedShape = () =>
  observerShape({
    label: `${MAXIMUM_OBSERVERS.toString()} observers on scalar 255 under a present integrity hash (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observerCount: MAXIMUM_OBSERVERS,
    networkId: 255,
    scriptIntegrityHash: PRESENT_HASH,
  });

/** Accepted by the machine: no Plutus evaluation, so the arm is unreachable. */
export const nativeOnlyShape = (fee = 11n) =>
  observerShape({
    label:
      "1 observer on scalar 255 under the absent integrity hash (native-only, inline field)",
    observerCount: 1,
    networkId: 255,
    scriptIntegrityHash: ABSENT_HASH,
    fee,
  });

export const emptyUntaggedShape = (fee = 13n) =>
  observerShape({
    label:
      "0 observers on scalar 255 under a present integrity hash (inline field)",
    observerCount: 0,
    networkId: 255,
    scriptIntegrityHash: PRESENT_HASH,
    fee,
  });

export const maximumTaggedShape = () =>
  observerShape({
    label: `${MAXIMUM_OBSERVERS.toString()} observers on scalar 1 under a present integrity hash (${MAXIMUM_FIELD_BYTES.toString()}-byte certified field)`,
    observerCount: MAXIMUM_OBSERVERS,
    networkId: 1,
    scriptIntegrityHash: PRESENT_HASH,
  });

export const honestForcedShape = () =>
  observerShape({
    label:
      "1 observer on scalar 255 under a present integrity hash (inline field)",
    observerCount: 1,
    networkId: 255,
    scriptIntegrityHash: PRESENT_HASH,
  });

export const acceptedFinding = (
  shape: ObserverShape,
  patch: Partial<ObserversForbiddenFinding> = {},
): ObserversForbiddenFinding => ({
  subject: acceptedVerdictSubject(transactionIdOf(shape)),
  networkId: shape.networkId,
  scriptIntegrityHash: shape.scriptIntegrityHash,
  ...patch,
});

const evidenceOf = (
  shape: ObserverShape,
  finding: ObserversForbiddenFinding,
): ObserversForbiddenEvidence =>
  prepareObserversForbiddenEvidence({
    finding,
    observerFieldPreimage: shape.fieldPreimage,
    committedFieldHashHex: midgardFieldCommitment(shape.fieldPreimage).toString(
      "hex",
    ),
  });

export const acceptedEvidence = (shape: ObserverShape) =>
  evidenceOf(shape, acceptedFinding(shape));

export const forcedFinding = (
  shape: ObserverShape,
  leaf: ForcedObserverLeaf,
  sourceKey: { transactionId: string; outputIndex: bigint },
  rejectionReason: RejectionReason = REASON_ARM,
): ObserversForbiddenFinding => ({
  subject: forcedVerdictSubject({
    transactionId: leaf.transactionId,
    sourceKey,
    rejectionReason,
  }),
  networkId: shape.networkId,
  scriptIntegrityHash: shape.scriptIntegrityHash,
});

type Harness = Awaited<ReturnType<typeof makeHarness>>;

export const acceptedBlock = async (
  h: Harness,
  shapes: readonly ObserverShape[],
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
 * committed block carries; the prover's finding claims this family's reason.
 */
export const forcedBlock = async (
  h: Harness,
  shape: ObserverShape,
  rejectionReason: RejectionReason = REASON_ARM,
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
  const finding = forcedFinding(shape, leaf, sourceKey);
  const evidence = evidenceOf(shape, finding);
  const source = { header, membership: leaf.membership, direction: 1n };
  return { setup, header, leaf, sourceKey, finding, evidence, source };
};

/** Init -> forced step 01 -> step 02 proof mint, every step recorded. */
export const forcedSuccess = async (
  prefix: string,
  shape: ObserverShape,
  beforeRemoval: (
    context: Harness & Awaited<ReturnType<typeof forcedBlock>>,
  ) => Promise<void> = async () => {},
) => {
  const h = await makeHarness();
  const block = await forcedBlock(h, shape);
  const { setup, evidence, source } = block;
  expect(observersForbiddenEvidenceCloses(evidence)).toBe(true);
  const initialized = await h.init(
    setup.fraudulentBlockOutRef,
    setup.headerHash,
  );
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Forced(
    h.threadOf(initialized.result),
    evidence,
    source,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const published = await h.publishField(shape, 1n);
  if (published.tier === "Certified")
    h.recordCarriage(prefix, shape, published);
  const proven = await h.step02(bound.result.nextThreadOutRef, evidence, shape);
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step02-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "forced_rejection_wrong");
  coverage.scenario("wrongful_forced_rejection_success");
  await beforeRemoval({ ...h, ...block });
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};
