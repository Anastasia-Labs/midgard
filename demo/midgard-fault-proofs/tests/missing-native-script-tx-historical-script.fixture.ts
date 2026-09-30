import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { missingNativeScriptTxVersionedScriptHash } from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  keyHashToCredential,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
  HISTORICAL_NATIVE_SCRIPT_SOURCE,
  type HistoricalNativeScriptSource,
  unsafeCreateHistoricalNativeScriptSourceRosterForTest,
} from "../src/missing-native-script-tx/historical-script.js";
import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
} from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../src/workflow/release-finality-policy.js";

export const DEPLOYMENT = "11".repeat(32);

const RELEASE = "22".repeat(32);

export const APPLICATION_OVERLAY = "23".repeat(32);

const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

export const point = (
  slot: string,
  blockNo: string,
  blockHash: string,
): FraudProofRawL1Point => ({
  slot,
  blockNo,
  blockHash,
  pointId: computeFraudProofRawL1PointId({ slot, blockNo, blockHash }),
});

export const inclusionPoint = point("100", "10", "31".repeat(32));

export const throughPoint = point("200", "39", "32".repeat(32));

export const fixture = () => {
  const native = CML.NativeScript.new_script_all(CML.NativeScriptList.new());
  const scriptBytesHex = native.to_canonical_cbor_hex();
  const expectedScriptHash = missingNativeScriptTxVersionedScriptHash(
    Buffer.from(scriptBytesHex, "hex"),
  );
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(
      credentialToAddress("Preview", keyHashToCredential("41".repeat(28))),
    ),
    CML.Value.from_coin(3_000_000n),
    undefined,
    CML.Script.new_native(native),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    170_000n,
  );
  const txHash = CML.hash_transaction(body).to_hex();
  const response = {
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT,
    blueprintHash: RELEASE,
    finalityPolicyDigest: releaseFinality.policyDigest,
    expectedScriptHash,
    sourceMode: "local_node" as const,
    sourceId: "watcher-local-kupmios-history",
    operatorIdentitySha256: null,
    scriptBytesHex,
    publicationOutRef: `${txHash}#0`,
    publicationOutputCbor: output.to_canonical_cbor_hex(),
    publicationTransactionBodyCbor: body.to_canonical_cbor_hex(),
    publicationTransactionIndex: 0,
    inclusionBlockTransactionIds: [txHash],
    inclusionPoint,
    throughPoint,
  };
  return { expectedScriptHash, response };
};

export const source = ({
  sourceMode = "local_node",
  sourceId = "watcher-local-kupmios-history",
  operatorIdentitySha256 = null,
  response = fixture().response,
}: {
  readonly sourceMode?: "local_node" | "external_providers";
  readonly sourceId?: string;
  readonly operatorIdentitySha256?: string | null;
  readonly response?: Readonly<Record<string, unknown>>;
} = {}): HistoricalNativeScriptSource => ({
  sourceVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE,
  sourceMode,
  sourceId,
  operatorIdentitySha256,
  resolveReferenceScriptPublication: vi.fn(async (request) => ({
    ...response,
    sourceMode,
    sourceId,
    operatorIdentitySha256,
    deploymentIdentityDigest: request.deploymentIdentityDigest,
    blueprintHash: request.blueprintHash,
    finalityPolicyDigest: request.finalityPolicyDigest,
    expectedScriptHash: request.expectedScriptHash,
    throughPoint: request.throughPoint,
  })),
  confirmCanonicalHistory: vi.fn(
    async ({
      inclusionPoint: confirmedInclusion,
      throughPoint: confirmedThrough,
    }) => ({
      canonical: true,
      inclusionPoint: confirmedInclusion,
      throughPoint: confirmedThrough,
    }),
  ),
});

export const roster = (
  sourceMode: "local_node" | "external_providers",
  sources: readonly HistoricalNativeScriptSource[],
) =>
  unsafeCreateHistoricalNativeScriptSourceRosterForTest({
    sourceMode,
    sources,
    applicationOverlayDigest: APPLICATION_OVERLAY,
    releaseFinality,
  });
