import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  canonicalDecodabilityEvidenceFromCommittedField,
  CanonicalDecodabilityStep02Datum,
  FraudProofComputationThreadStepDatum,
  type L2TransactionSource,
  L2TransactionSource as L2TransactionSourceCodec,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { CanonicalDecodabilityContracts } from "../canonical-decodability/contracts-v1.js";
import { submitCanonicalDecodabilityInit } from "../canonical-decodability/submit-canonical-decodability-init.js";
import { submitCanonicalDecodabilityStep01 } from "../canonical-decodability/submit-canonical-decodability-step-01.js";
import { submitCanonicalDecodabilityStep02 } from "../canonical-decodability/submit-canonical-decodability-step-02.js";
import {
  CANONICAL_DECODABILITY_ARTIFACT,
  canonicalDecodabilityArtifactFromRawEvidence,
  type CanonicalDecodabilityRawArtifact,
  type CanonicalDecodabilityRawBlockEvidence,
} from "../evidence/canonical-decodability-raw-evidence-v1.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
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
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import { CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  createRawCommittedFieldCarriagePlan,
  type FieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./production-linear-family-adapter-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary-v1.js";

export { CANONICAL_DECODABILITY_ARTIFACT };

export type CanonicalDecodabilityArtifact = JournalJsonObject &
  CanonicalDecodabilityRawArtifact;

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

const parseArtifact = (value: unknown): CanonicalDecodabilityArtifact => {
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
    parsed.schemaVersion !== CANONICAL_DECODABILITY_ARTIFACT ||
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
    schemaVersion: CANONICAL_DECODABILITY_ARTIFACT,
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

type AdmittedCanonicalDecodabilityArtifact = Readonly<{
  artifact: CanonicalDecodabilityArtifact;
  committedPreimage: Buffer;
  witnessSet?: NativeTxWitnessSetCompact;
  witnessSetCompactCbor?: string;
  txInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}>;

export const admitCanonicalDecodabilityArtifact = async (
  value: unknown,
): Promise<AdmittedCanonicalDecodabilityArtifact> => {
  const artifact = parseArtifact(value);
  const transactions = artifact.transactions.map((transaction, index) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    let source: L2TransactionSource;
    try {
      source = Data.from(
        transaction.l2TransactionSourceCbor,
        L2TransactionSourceCodec,
      );
    } catch (cause) {
      throw new Error(
        `canonical-decodability transaction ${index.toString()} source does not decode: ${String(cause)}`,
      );
    }
    const expected: L2TransactionSource = {
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
      Data.to(source, L2TransactionSourceCodec) !==
        transaction.l2TransactionSourceCbor ||
      Data.to(source, L2TransactionSourceCodec) !==
        Data.to(expected, L2TransactionSourceCodec)
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
  await requireTransactionsRootMatch({
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
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompact(
    selected.material.canonical.witnessSet,
  );
  const witnessSet: NativeTxWitnessSetCompact = {
    addr_tx_wits_hash: witnessCompact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: witnessCompact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: witnessCompact.redeemerTxWitsHash.toString("hex"),
  };
  const fieldEvidence = canonicalDecodabilityEvidenceFromCommittedField({
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
    ...(artifact.selectedFieldIndex < MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX
      ? {}
      : {
          witnessSet,
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact({
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

export const prepareCanonicalDecodabilityArtifact = async (
  evidence: CanonicalDecodabilityRawBlockEvidence,
): Promise<CanonicalDecodabilityArtifact> => {
  const artifact = normalizeJournalJson({
    ...canonicalDecodabilityArtifactFromRawEvidence(evidence),
  }) as CanonicalDecodabilityArtifact;
  await admitCanonicalDecodabilityArtifact(artifact);
  return Object.freeze(artifact);
};

export type CanonicalDecodabilityWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
}>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"canonicalDecodability">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: CanonicalDecodabilityContracts;
  category: FraudProofWorkflowDeploymentBinding<"canonicalDecodability">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"canonicalDecodability">["catalogue"];
  referenceScripts: CanonicalDecodabilityWorkflowReferenceScripts;
  fieldCarriage: FieldCarriagePrerequisitePort<"canonicalDecodability">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
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

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"canonicalDecodability"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "canonicalDecodability",
  prepare: async () => {
    throw new Error(
      "canonical-decodability requires the authenticated raw committed-field evidence route",
    );
  },
  capture: async ({ action, artifact }) => {
    const admitted = await admitCanonicalDecodabilityArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "canonical-decodability artifact changed header identity",
      );
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
        resolvePublishedProofChunks({
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
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
                !workflowTransactionInputOutRefs(built.signed).includes(
                  nextRemovalOutRef,
                ) ||
                !workflowTransactionReferenceInputOutRefs(
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

export type ManifestBoundCanonicalDecodabilityWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: CanonicalDecodabilityWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundCanonicalDecodabilityWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"canonicalDecodability">;
  l1: FraudProofFamilyL1ObservationPort<"canonicalDecodability">;
  transactions: LinearFamilyTransactionPort<"canonicalDecodability">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundCanonicalDecodabilityWorkflow = async (
  config: ManifestBoundCanonicalDecodabilityWorkflowConfig,
): Promise<ManifestBoundCanonicalDecodabilityWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
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
  const references: CanonicalDecodabilityWorkflowReferenceScripts =
    Object.freeze({
      steps: Object.freeze([
        requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "fraudProofCanonicalDecodability",
          utxo: config.referenceScripts.steps[0],
        }),
        requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "fraudProofCanonicalDecodabilityStep02",
          utxo: config.referenceScripts.steps[1],
        }),
      ] as const),
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
      witnesses: Object.freeze({
        computationThreadMint: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "computationThreadMint",
          utxo: config.referenceScripts.witnesses.computationThreadMint,
        }),
        fraudProofMint: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "fraudProofMint",
          utxo: config.referenceScripts.witnesses.fraudProofMint,
        }),
        phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "phasMembershipWithdraw",
          utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
        }),
        chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "chunkedVerifyWithdraw",
          utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
        }),
      }),
    });
  const contracts: CanonicalDecodabilityContracts = Object.freeze({
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
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const fieldCarriage = createAuthenticatedFieldCarriagePrerequisitePort({
    category: "canonicalDecodability",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: async ({ action, artifact }) => {
      if (action.input.stage !== "step_01") return null;
      const admitted = await admitCanonicalDecodabilityArtifact(artifact);
      return Object.freeze({
        planned: createRawCommittedFieldCarriagePlan({
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
  const transactions = createTransactionPort({
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
  const linear = createLinearFamilyWorkflowAdapter({
    category: "canonicalDecodability",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const proofChunks = createAuthenticatedProofChunkPrerequisitePort({
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
    adapter: withFieldCarriagePrerequisite({
      category: "canonicalDecodability",
      base: withProofChunkPrerequisite({
        category: "canonicalDecodability",
        base: linear,
        prerequisite: proofChunks,
      }),
      prerequisite: fieldCarriage,
    }),
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const runOrResumeManifestBoundCanonicalDecodabilityWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundCanonicalDecodabilityWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["canonicalDecodability"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
