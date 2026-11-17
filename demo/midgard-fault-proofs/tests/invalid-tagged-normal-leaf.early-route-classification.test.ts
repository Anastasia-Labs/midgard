/** The early raw routes over a normal leaf committed `TxIsInvalid`. A normal
 * leaf is an acceptance verdict, so a leaf committed with a non-zero validity
 * code is itself an `l2TxMistag` fault, read from its committed compact. The
 * canonical-decodability and field-length steps open a normal leaf only at
 * code 0, so a broken envelope or a wrong field-length vector on such a leaf
 * is never what decides the block. */
import {
  decodeMidgardNativeTxCanonical,
  decodeSingleCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core/codec";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { fetchFraudProofEvidence } from "../src/evidence/fraud-proof-evidence.js";
import {
  CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
  createCompleteCanonicalReplayUnion,
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";
import { prepareL2TxMistagRawArtifact } from "../src/workflow/l2-tx-mistag.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type FixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { retainedDaSource } from "./helpers/header-classifier.retained-da-source.js";

const DEPLOYMENT_FINGERPRINT = "d3".repeat(32);
const RELEASE_FINALITY_POLICY = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

const finalityAuthority = (): FraudProofReleaseFinalityAuthority => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
    blueprintHash: "e3".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

const withSource = (
  transaction: Omit<FixtureTransaction, "source" | "sourceValueBytes">,
  source: SDK.L2TransactionSource,
): FixtureTransaction => ({
  ...transaction,
  source,
  sourceValueBytes: Buffer.from(
    Data.to(source as never, SDK.L2TransactionSourceSchema as never),
    "hex",
  ),
});

/** A leaf whose committed outputs field is a scalar, not a §5.1 list
 * envelope; its committed source is the exact fault-evidence derivation. */
const brokenEnvelopeLeaf = (
  validity: SDK.MidgardTxValidity,
  spentTxIdByte: number,
): FixtureTransaction => {
  const valid = buildFixtureTransaction({
    validity,
    spendInputs: [outRefCbor(spentTxIdByte, 0n)],
    fee: 1n,
  });
  const outer = decodeSingleCbor(valid.canonicalCbor);
  if (!Array.isArray(outer) || !Array.isArray(outer[1]))
    throw new Error("fixture native transaction changed outer wire");
  const body = outer[1];
  const canonicalCbor = encodeCbor([
    outer[0],
    [...body.slice(0, 2), Buffer.from([0]), ...body.slice(3)],
    outer[2],
    outer[3],
  ]);
  expect(() => decodeMidgardNativeTxCanonical(canonicalCbor)).toThrow(
    "outputs is not a canonical §5.1 field preimage",
  );
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(canonicalCbor);
  const txId = material.transactionId.toString("hex");
  return withSource(
    {
      txId,
      canonicalCbor,
      compactCbor: material.proofSource.compactCbor,
    },
    {
      tx_id: txId,
      source: {
        compact_cbor: material.proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
    },
  );
};

/** A leaf whose committed field-length vector overstates its spend inputs. */
const wrongFieldLengthLeaf = (
  validity: SDK.MidgardTxValidity,
  spentTxIdByte?: number,
): FixtureTransaction => {
  const canonical = buildFixtureTransaction({
    validity,
    spendInputs:
      spentTxIdByte === undefined ? [] : [outRefCbor(spentTxIdByte, 0n)],
    fee: 1n,
  });
  const lengths = [
    ...decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(canonical.source.source.field_preimage_lengths_cbor, "hex"),
    ),
  ];
  lengths[0] = lengths[0]! + 1;
  return withSource(canonical, {
    ...canonical.source,
    source: {
      ...canonical.source.source,
      field_preimage_lengths_cbor:
        encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
    },
  });
};

const mistagLeaf = (): FixtureTransaction =>
  buildFixtureTransaction({
    validity: "TxIsInvalid",
    spendInputs: [outRefCbor(0x61, 0n)],
    fee: 1n,
  });

/** The first leaf, over spent outputs 0x40 onwards, whose id satisfies
 * `placed`: it fixes where the leaf sits in the payload's key order. */
const leafOrdered = (
  build: (spentTxIdByte: number) => FixtureTransaction,
  placed: (txId: string) => boolean,
): FixtureTransaction => {
  for (let spent = 0x40; spent < 0x80; spent += 1) {
    const leaf = build(spent);
    if (placed(leaf.txId)) return leaf;
  }
  throw new Error("no fixture leaf id falls on the requested side");
};

const classify = async (transactions: readonly FixtureTransaction[]) => {
  const fixture = await buildCanonicalBlockFixture({ transactions });
  const observation = authenticatedHeaderObservation(fixture);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    replayer: createCompleteCanonicalReplayUnion([
      CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
      L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
      FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
    ]),
    releaseFinalityAuthority: finalityAuthority(),
  });
  const sources = [
    retainedDaSource({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      fetched: { count: 0 },
    }),
  ];
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth:
          DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      }),
    sources,
  });
  return { fixture, observation, sources, decision };
};

/** The routed raw evidence builds an l2TxMistag artifact that admits, so the
 * decision is one the proof workflow can carry. */
const expectProvableMistag = async (
  classified: Awaited<ReturnType<typeof classify>>,
  transaction: FixtureTransaction,
  transactionIndex: number,
) => {
  expect(classified.decision).toMatchObject({
    decision: "fault_detected",
    headerHash: classified.fixture.headerHash,
    category: "l2TxMistag",
    violationId: "l2-tx-mistag",
    detectionId: `l2-tx-mistag:${transactionIndex.toString()}:${transaction.txId}:1`,
    position: transactionIndex.toString(),
  });
  const routed = await fetchFraudProofEvidence({
    observation: classified.observation,
    sources: classified.sources,
  });
  if (routed.kind !== "l2_tx_mistag")
    throw new Error(`routed ${routed.kind}, not the raw l2TxMistag route`);
  const artifact = await prepareL2TxMistagRawArtifact(routed);
  expect(artifact).toMatchObject({
    headerHash: classified.fixture.headerHash,
    transactionIndex,
    nativeTxId: transaction.txId,
    nativeTxCompactCbor: transaction.source.source.compact_cbor,
  });
};

describe("early raw routes over a normal leaf committed TxIsInvalid", () => {
  it("decides an invalid-tagged leaf with a broken field envelope as l2TxMistag", async () => {
    const leaf = brokenEnvelopeLeaf("TxIsInvalid", 0x52);
    await expectProvableMistag(await classify([leaf]), leaf, 0);
  });

  it("decides an invalid-tagged leaf with a wrong field-length vector as l2TxMistag", async () => {
    const leaf = wrongFieldLengthLeaf("TxIsInvalid");
    await expectProvableMistag(await classify([leaf]), leaf, 0);
  });

  it("still decides a valid-tagged leaf with a broken field envelope as canonicalDecodability", async () => {
    const leaf = brokenEnvelopeLeaf("TxIsValid", 0x52);
    const { decision } = await classify([leaf]);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "canonicalDecodability",
      position: "0",
      detectionId: expect.stringMatching(
        new RegExp(`^canonical-decodability:0:${leaf.txId}:`, "u"),
      ),
    });
  });

  // The payload lists leaves in key order. With no trace step naming either
  // leaf, both events follow the trace in that order, so the earlier leaf's
  // fault decides the block.
  it("decides a mistag that precedes a valid-tagged broken envelope as l2TxMistag", async () => {
    const mistag = mistagLeaf();
    const broken = leafOrdered(
      (spent) => brokenEnvelopeLeaf("TxIsValid", spent),
      (txId) => txId > mistag.txId,
    );
    const classified = await classify([broken, mistag]);
    await expectProvableMistag(classified, mistag, 0);
  });

  it("decides a valid-tagged broken envelope that precedes a mistag as canonicalDecodability", async () => {
    const mistag = mistagLeaf();
    const broken = leafOrdered(
      (spent) => brokenEnvelopeLeaf("TxIsValid", spent),
      (txId) => txId < mistag.txId,
    );
    const { decision } = await classify([mistag, broken]);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "canonicalDecodability",
      position: "0",
    });
  });

  it("decides a mistag that precedes a valid-tagged field-length mismatch as l2TxMistag", async () => {
    const mistag = mistagLeaf();
    const wrong = leafOrdered(
      (spent) => wrongFieldLengthLeaf("TxIsValid", spent),
      (txId) => txId > mistag.txId,
    );
    const classified = await classify([wrong, mistag]);
    await expectProvableMistag(classified, mistag, 0);
  });

  it("decides a valid-tagged field-length mismatch that precedes a mistag as fieldPreimageLengthMismatch", async () => {
    const mistag = mistagLeaf();
    const wrong = leafOrdered(
      (spent) => wrongFieldLengthLeaf("TxIsValid", spent),
      (txId) => txId < mistag.txId,
    );
    const { decision } = await classify([mistag, wrong]);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "fieldPreimageLengthMismatch",
      position: "0",
    });
  });
});
