import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import {
  authenticateTransactionsInclusionRoots,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/index.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  type FixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";

const absentKeyHash = Buffer.alloc(28, 0x44);

export const nativeScript = {
  language: "NativeCardano",
  scriptBytes: Buffer.concat([Buffer.from("8200581c", "hex"), absentKeyHash]),
  nativeScript: { type: "sig", keyHash: absentKeyHash },
} satisfies MidgardVersionedScript;

export const nativeTx = ({
  spendInputs = [],
  scripts = [],
}: {
  readonly spendInputs?: readonly Buffer[];
  readonly scripts?: readonly MidgardVersionedScript[];
}) =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputs]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: encodeCbor(
        scripts.map(encodeMidgardVersionedScript),
      ),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export const fixtureTransaction = (
  tx: ReturnType<typeof nativeTx>,
): FixtureTransaction => {
  const canonicalCbor = encodeMidgardNativeTxCanonical(tx);
  const proof =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  const source: SDK.L2TransactionSource = {
    tx_id: txId,
    source: {
      compact_cbor: proof.compactCbor.toString("hex"),
      witness_set_compact_cbor: proof.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proof.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId,
    canonicalCbor,
    compactCbor: proof.compactCbor,
    source,
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceSchema),
  };
};

export const canonicalEvidence = async (tx: ReturnType<typeof nativeTx>) => {
  const transaction = fixtureTransaction(tx);
  const nativeFixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
  });
  const payloadFixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(payloadFixture),
    payloadEnvelopeCbor: payloadFixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/native-script-family-test",
      grade: "security",
    },
  });
  return {
    ...evidence,
    observation: authenticatedHeaderObservation(nativeFixture),
    headerHash: nativeFixture.headerHash,
    header: nativeFixture.header,
    inclusionRootAuthentication: await authenticateTransactionsInclusionRoots({
      header: nativeFixture.header,
      reconstruction: evidence.reconstruction,
      transactions: evidence.transactions,
    }),
  };
};

export const evidenceFromFixture = async (
  fixture: Awaited<ReturnType<typeof buildCanonicalBlockFixture>>,
) =>
  await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/native-script-family-test",
      grade: "security",
    },
  });
