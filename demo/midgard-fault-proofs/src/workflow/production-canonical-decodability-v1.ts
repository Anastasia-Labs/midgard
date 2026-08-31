import {
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  canonicalDecodabilityEvidenceFromCommittedFieldV1,
  CanonicalDecodabilityStep02Datum,
  FraudProofComputationThreadStepDatum,
  type L2TransactionSourceV1,
  L2TransactionSourceV1 as L2TransactionSourceV1Codec,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { CanonicalDecodabilityContractsV1 } from "../canonical-decodability/contracts-v1.js";
import { submitCanonicalDecodabilityInit } from "../canonical-decodability/submit-canonical-decodability-init.js";
import { submitCanonicalDecodabilityStep01 } from "../canonical-decodability/submit-canonical-decodability-step-01.js";
import { submitCanonicalDecodabilityStep02 } from "../canonical-decodability/submit-canonical-decodability-step-02.js";
import {
  canonicalDecodabilityArtifactFromRawEvidenceV1,
  type CanonicalDecodabilityRawBlockEvidenceV1,
  PRODUCTION_CANONICAL_DECODABILITY_ARTIFACT_V1,
  type ProductionCanonicalDecodabilityRawArtifactV1,
} from "../evidence/canonical-decodability-raw-evidence-v1.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatchV1,
} from "../prepare-double-spend.js";
import { resolvePublishedProofChunksV1 } from "../publish-proof-chunks.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import { CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStoreV1,
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  createProductionRawCommittedFieldCarriagePlanV1,
  type ProductionFieldCarriagePrerequisitePortV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  withProductionProofChunkPrerequisiteV1,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export { PRODUCTION_CANONICAL_DECODABILITY_ARTIFACT_V1 };

export type ProductionCanonicalDecodabilityArtifactV1 = JournalJsonObjectV1 &
  ProductionCanonicalDecodabilityRawArtifactV1;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const natural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} is not a non-negative safe integer`);
  }
  return value as number;
};

const parseArtifact = (
  value: unknown,
): ProductionCanonicalDecodabilityArtifactV1 => {
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "committedTransactionsRoot",
      "l2TransactionCount",
      "transactionsPhasRoot",
      "selectedTransactionIndex",
      "selectedFieldIndex",
      "selectedVerdict",
      "txMembershipProofCbor",
      "transactions",
    ],
    "canonical-decodability artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_CANONICAL_DECODABILITY_ARTIFACT_V1 ||
    !Array.isArray(parsed.transactions) ||
    parsed.transactions.length === 0
  ) {
    throw new Error(
      "canonical-decodability artifact version or leaves changed",
    );
  }
  const transactions = Object.freeze(
    parsed.transactions.map((value, index) => {
      const transaction = exact(
        value,
        ["nodeTxId", "txCbor", "l2TransactionSourceCbor"],
        `canonical-decodability transaction ${index.toString()}`,
      );
      return Object.freeze({
        nodeTxId: canonicalHex(
          transaction.nodeTxId,
          HEX_32,
          `transaction ${index.toString()} id`,
        ),
        txCbor: canonicalHex(
          transaction.txCbor,
          EVEN_HEX,
          `transaction ${index.toString()} CBOR`,
        ),
        l2TransactionSourceCbor: canonicalHex(
          transaction.l2TransactionSourceCbor,
          EVEN_HEX,
          `transaction ${index.toString()} source CBOR`,
        ),
      });
    }),
  );
  const l2TransactionCount = natural(
    parsed.l2TransactionCount,
    "canonical-decodability transaction count",
  );
  if (transactions.length !== l2TransactionCount) {
    throw new Error("canonical-decodability transaction count changed");
  }
  return Object.freeze({
    schemaVersion: PRODUCTION_CANONICAL_DECODABILITY_ARTIFACT_V1,
    headerHash: canonicalHex(parsed.headerHash, HEX_28, "header hash"),
    committedTransactionsRoot: canonicalHex(
      parsed.committedTransactionsRoot,
      HEX_32,
      "committed transactions root",
    ),
    l2TransactionCount,
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "transactions PHAS root",
    ),
    selectedTransactionIndex: natural(
      parsed.selectedTransactionIndex,
      "selected transaction index",
    ),
    selectedFieldIndex: natural(
      parsed.selectedFieldIndex,
      "selected field index",
    ),
    selectedVerdict: natural(parsed.selectedVerdict, "selected verdict"),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "transaction membership proof",
    ),
    transactions,
  });
};

type AdmittedCanonicalDecodabilityArtifactV1 = Readonly<{
  artifact: ProductionCanonicalDecodabilityArtifactV1;
  committedPreimage: Buffer;
  witnessSet?: NativeTxWitnessSetCompact;
  witnessSetCompactCbor?: string;
  txInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}>;

export const admitProductionCanonicalDecodabilityArtifactV1 = async (
  value: unknown,
): Promise<AdmittedCanonicalDecodabilityArtifactV1> => {
  const artifact = parseArtifact(value);
  const transactions = artifact.transactions.map((transaction, index) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(transaction.txCbor, "hex"),
    );
    let source: L2TransactionSourceV1;
    try {
      source = Data.from(
        transaction.l2TransactionSourceCbor,
        L2TransactionSourceV1Codec,
      );
    } catch (cause) {
      throw new Error(
        `canonical-decodability transaction ${index.toString()} source does not decode: ${String(cause)}`,
      );
    }
    const expected: L2TransactionSourceV1 = {
      tx_id: material.transactionId.toString("hex"),
      source: {
        compact_cbor: material.proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    if (
      expected.tx_id !== transaction.nodeTxId ||
      Data.to(source, L2TransactionSourceV1Codec) !==
        transaction.l2TransactionSourceCbor ||
      Data.to(source, L2TransactionSourceV1Codec) !==
        Data.to(expected, L2TransactionSourceV1Codec)
    ) {
      throw new Error(
        `canonical-decodability transaction ${index.toString()} changed its committed source identity`,
      );
    }
    return Object.freeze({ transaction, material });
  });
  const trie = await buildTrieView(
    transactions.map(({ transaction }) => ({
      key: Buffer.from(transaction.nodeTxId, "hex"),
      value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
    })),
  );
  if (trie.root !== artifact.transactionsPhasRoot) {
    throw new Error("canonical-decodability transactions PHAS root changed");
  }
  await requireTransactionsRootMatchV1({
    sourceRoot: trie.root,
    expectedTransactionsRoot: artifact.committedTransactionsRoot,
    count: BigInt(artifact.l2TransactionCount),
  });
  const selected = transactions[artifact.selectedTransactionIndex];
  if (selected === undefined) {
    throw new Error("canonical-decodability selected transaction is absent");
  }
  const proof = requireProof(
    trie,
    Buffer.from(selected.transaction.nodeTxId, "hex"),
    "canonical-decodability transaction",
  );
  if (proof !== artifact.txMembershipProofCbor) {
    throw new Error("canonical-decodability transaction proof changed");
  }
  const committedPreimage =
    selected.material.fieldPreimages[artifact.selectedFieldIndex];
  if (committedPreimage === undefined) {
    throw new Error("canonical-decodability selected field is absent");
  }
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompactV1(
    selected.material.canonical.witnessSet,
  );
  const witnessSet: NativeTxWitnessSetCompact = {
    addr_tx_wits_hash: witnessCompact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: witnessCompact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: witnessCompact.redeemerTxWitsHash.toString("hex"),
  };
  const fieldEvidence = canonicalDecodabilityEvidenceFromCommittedFieldV1({
    badTxId: selected.transaction.nodeTxId,
    fieldIndex: artifact.selectedFieldIndex,
    committedPreimage,
  });
  if (
    !fieldEvidence.isViolation ||
    fieldEvidence.verdict !== artifact.selectedVerdict
  ) {
    throw new Error("canonical-decodability selected verdict changed");
  }
  return Object.freeze({
    artifact,
    committedPreimage,
    ...(artifact.selectedFieldIndex < MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1
      ? {}
      : {
          witnessSet,
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1({
            addrTxWitsHash: witnessCompact.addrTxWitsHash,
            scriptTxWitsHash: witnessCompact.scriptTxWitsHash,
            redeemerTxWitsHash: witnessCompact.redeemerTxWitsHash,
          }).toString("hex"),
        }),
    txInclusion: parseSubmitStep01TxInclusion({
      nativeTxId: selected.transaction.nodeTxId,
      nativeTx: nativeTxFromCoreCompact(selected.material.compact),
      nativeTxCompactCbor:
        selected.material.proofSource.compactCbor.toString("hex"),
      l2TransactionSourceCbor: selected.transaction.l2TransactionSourceCbor,
      transactionsPhasRoot: trie.root,
      txMembershipProofCbor: proof,
    }),
  });
};

export const prepareProductionCanonicalDecodabilityArtifactV1 = async (
  evidence: CanonicalDecodabilityRawBlockEvidenceV1,
): Promise<ProductionCanonicalDecodabilityArtifactV1> => {
  const artifact = normalizeJournalJsonV1({
    ...canonicalDecodabilityArtifactFromRawEvidenceV1(evidence),
  }) as ProductionCanonicalDecodabilityArtifactV1;
  await admitProductionCanonicalDecodabilityArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type CanonicalDecodabilityWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
}>;

type BoundConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"canonicalDecodability">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: CanonicalDecodabilityContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"canonicalDecodability">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"canonicalDecodability">["catalogue"];
  referenceScripts: CanonicalDecodabilityWorkflowReferenceScriptsV1;
  fieldCarriage: ProductionFieldCarriagePrerequisitePortV1<"canonicalDecodability">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "canonical-decodability workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "canonicalDecodability" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("canonical-decodability workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  key: string,
): string => {
  const value = input[key];
  if (typeof value !== "string") {
    throw new Error(`canonical-decodability workflow action omitted ${key}`);
  }
  return value;
};

const createTransactionPortV1 = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"canonicalDecodability"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "canonicalDecodability",
  prepare: async () => {
    throw new Error(
      "canonical-decodability requires the authenticated raw committed-field evidence route",
    );
  },
  capture: async ({ action, artifact }) => {
    const admitted =
      await admitProductionCanonicalDecodabilityArtifactV1(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "canonical-decodability artifact changed header identity",
      );
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitCanonicalDecodabilityInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const [proofChunks, field] = await Promise.all([
        resolvePublishedProofChunksV1({
          lucid: config.lucid,
          address: config.signer.address,
          proofCbor: admitted.artifact.txMembershipProofCbor,
        }),
        config.fieldCarriage.resolveAuthenticated({
          headerHash: config.headerHash,
          action,
          artifact,
        }),
      ]);
      if (proofChunks === undefined || field.requirement === null) {
        throw new Error(
          "canonical-decodability prerequisites disappeared before step-01",
        );
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitCanonicalDecodabilityStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.txInclusion,
              fieldIndex: admitted.artifact.selectedFieldIndex,
              committedPreimage: admitted.committedPreimage,
              ...(admitted.witnessSet === undefined
                ? {}
                : { witnessSet: admitted.witnessSet }),
              publishedProofChunks: proofChunks,
              publishedFieldCarriageUtxos: field.publications,
              ...(field.certificate === undefined
                ? {}
                : {
                    fieldCertificateUtxo: field.certificate,
                    fieldCertificatePolicyId:
                      config.contracts.fieldPreimageCertificatePolicyId,
                  }),
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitCanonicalDecodabilityStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
      const fraudProofOutRef = stringField(input, "fraudProofOutRef");
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (boundary) => {
          await submitRemoveFraudulentBlock({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "canonicalDecodability",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (built) => {
              if (
                !workflowTransactionInputOutRefsV1(built.signed).includes(
                  nextRemovalOutRef,
                ) ||
                !workflowTransactionReferenceInputOutRefsV1(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "canonical-decodability removal changed queue/proof identity",
                );
              }
              await boundary(built);
            },
          });
        },
      );
      return Object.freeze({
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
    }
    throw new Error(
      `canonical-decodability workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundCanonicalDecodabilityWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: CanonicalDecodabilityWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundCanonicalDecodabilityWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"canonicalDecodability">;
  l1: FraudProofFamilyL1ObservationPortV1<"canonicalDecodability">;
  transactions: ProductionLinearFamilyTransactionPortV1<"canonicalDecodability">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundCanonicalDecodabilityWorkflowV1 = async (
  config: ManifestBoundCanonicalDecodabilityWorkflowConfigV1,
): Promise<ManifestBoundCanonicalDecodabilityWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "canonicalDecodability",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      CanonicalDecodabilityStep02Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.canonicalDecodability;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "canonical-decodability manifest omitted its proof/certificate contracts",
    );
  }
  const references: CanonicalDecodabilityWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps: Object.freeze([
        requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "fraudProofCanonicalDecodability",
          utxo: config.referenceScripts.steps[0],
        }),
        requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "fraudProofCanonicalDecodabilityStep02",
          utxo: config.referenceScripts.steps[1],
        }),
      ] as const),
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
      witnesses: Object.freeze({
        computationThreadMint: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "computationThreadMint",
          utxo: config.referenceScripts.witnesses.computationThreadMint,
        }),
        fraudProofMint: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "fraudProofMint",
          utxo: config.referenceScripts.witnesses.fraudProofMint,
        }),
        phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "phasMembershipWithdraw",
          utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
        }),
        chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "chunkedVerifyWithdraw",
          utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
        }),
      }),
    });
  const contracts: CanonicalDecodabilityContractsV1 = Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    claimRegistry: binding.claimRegistry,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const fieldCarriage = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "canonicalDecodability",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: async ({ action, artifact }) => {
      if (action.input.stage !== "step_01") return null;
      const admitted =
        await admitProductionCanonicalDecodabilityArtifactV1(artifact);
      return Object.freeze({
        planned: createProductionRawCommittedFieldCarriagePlanV1({
          owner: config.signer.paymentKeyHash,
          nativeTxId: admitted.txInclusion.nativeTxId,
          fieldIndex: admitted.artifact.selectedFieldIndex,
          preimage: admitted.committedPreimage,
        }),
        compactCbor: admitted.txInclusion.nativeTxCompactCbor,
        ...(admitted.witnessSetCompactCbor === undefined
          ? {}
          : { witnessSetCompactCbor: admitted.witnessSetCompactCbor }),
        certificate: Object.freeze({
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: references.fieldPreimageCertificateMint,
        }),
      });
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  const transactions = createTransactionPortV1({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    headerHash: binding.definition.headerHash,
    contracts,
    category: binding.resolvedContracts.category,
    catalogue: binding.catalogue,
    referenceScripts: references,
    fieldCarriage,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  const linear = createProductionLinearFamilyWorkflowAdapterV1({
    category: "canonicalDecodability",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const proofChunks = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "canonicalDecodability",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) =>
      action.input.stage === "step_01"
        ? parseArtifact(artifact).txMembershipProofCbor
        : null,
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter: withProductionFieldCarriagePrerequisiteV1({
      category: "canonicalDecodability",
      base: withProductionProofChunkPrerequisiteV1({
        category: "canonicalDecodability",
        base: linear,
        prerequisite: proofChunks,
      }),
      prerequisite: fieldCarriage,
    }),
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

export const runOrResumeManifestBoundCanonicalDecodabilityWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundCanonicalDecodabilityWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["canonicalDecodability"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
