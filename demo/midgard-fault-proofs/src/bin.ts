#!/usr/bin/env node

import { realpathSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { formatUnknownError } from "@al-ft/midgard-core";
import { assertSecurityGradeEvidenceV1 } from "@al-ft/midgard-sdk";

import {
  diagnosticEvidenceBannerV1,
  LOCAL_FILE_DIAGNOSTIC_PROVENANCE_V1,
  MIDGARD_NODE_URL_DIAGNOSTIC_PROVENANCE_V1,
  SAMPLE_EVIDENCE_DIAGNOSTIC_PROVENANCE_V1,
} from "./evidence/diagnostic-evidence-v1.js";
import {
  generateFraudProofFamilyScaffoldV1,
  writeFraudProofFamilyScaffoldV1,
} from "./family-scaffold/generate-v1.js";
import {
  inspectContractsFromFiles,
  parseNetwork,
} from "./inspect-contracts.js";
import { readJsonFile, stringifyJson } from "./json-file.js";
import { rejectRetiredUnauthenticatedSubmissionRouteV1 } from "./legacy-submission-boundary-v1.js";
import { neSubmitStep01FromFiles } from "./ne-submit-step-01.js";
import { neSubmitStep02FromFiles } from "./ne-submit-step-02.js";
import { neSubmitStep03FromFiles } from "./ne-submit-step-03.js";
import { neSubmitStep04FromFiles } from "./ne-submit-step-04.js";
import { submitRemoveFraudulentBlockFromFiles } from "./remove-fraudulent-block.js";
import { type ProviderKind } from "./runtime.js";
import { submitDaHashPreimageStep01FromFiles } from "./submit-da-hash-preimage-step-01.js";
import { submitDaHashPreimageStep02FromFiles } from "./submit-da-hash-preimage-step-02.js";
import {
  type SubmitInitFraudCategory,
  submitInitFromFiles,
} from "./submit-init.js";
import { submitInputNoIdxStep01FromFiles } from "./submit-input-no-idx-step-01.js";
import {
  submitInputNoIdxStep02FromFiles,
  submitInputNoIdxStep02UntilTerminalFromFiles,
} from "./submit-input-no-idx-step-02.js";
import { submitInputNoIdxStep03FromFiles } from "./submit-input-no-idx-step-03.js";
import { submitInputNoIdxStep04FromFiles } from "./submit-input-no-idx-step-04.js";
import { submitInvalidRangeStep01FromFiles } from "./submit-invalid-range-step-01.js";
import { submitInvalidRangeStep02FromFiles } from "./submit-invalid-range-step-02.js";
import { submitInvalidSignatureStep01FromFiles } from "./submit-invalid-signature-step-01.js";
import { submitInvalidSignatureStep02FromFiles } from "./submit-invalid-signature-step-02.js";
import { submitNoReferenceInputStep01FromFiles } from "./submit-no-reference-input-step-01.js";
import { submitNoReferenceInputStep02FromFiles } from "./submit-no-reference-input-step-02.js";
import { submitNoReferenceInputStep03FromFiles } from "./submit-no-reference-input-step-03.js";
import { submitNoReferenceInputStep04FromFiles } from "./submit-no-reference-input-step-04.js";
import { submitReferenceInputNoIdxStep01FromFiles } from "./submit-reference-input-no-idx-step-01.js";
import { submitReferenceInputNoIdxStep02FromFiles } from "./submit-reference-input-no-idx-step-02.js";
import { submitReferenceInputNoIdxStep03FromFiles } from "./submit-reference-input-no-idx-step-03.js";
import { submitReferenceInputNoIdxStep04FromFiles } from "./submit-reference-input-no-idx-step-04.js";
import { submitStep01FromFiles } from "./submit-step-01.js";
import { submitStep02FromFiles } from "./submit-step-02.js";
import { submitStep03FromFiles } from "./submit-step-03.js";
import { submitStep04FromFiles } from "./submit-step-04.js";
import { submitZeroInputStep01FromFiles } from "./submit-zero-input-step-01.js";
import { submitZeroInputStep02FromFiles } from "./submit-zero-input-step-02.js";
import {
  submitValidationDisputeAwardFromFiles,
  submitValidationDisputeDirectResolutionFromFiles,
  submitValidationDisputeEnterResolutionFromFiles,
  submitValidationDisputeEnterTimeoutFromFiles,
  submitValidationDisputeOpenFromFiles,
  submitValidationDisputePrepareResolutionFromFiles,
  submitValidationDisputePrepareSelectedFromFiles,
  submitValidationDisputeRevealFromFiles,
  submitValidationDisputeSemanticResolutionFromFiles,
  submitValidationDisputeTimeoutFromFiles,
  submitValidationDisputeVerifySourceFromFiles,
} from "./validation-dispute/from-files.js";

export type ParsedArgs = {
  readonly command: string | undefined;
  readonly blueprintPath: string | undefined;
  readonly deploymentInfoPath: string | undefined;
  readonly network: string | undefined;
  readonly provider: ProviderKind | undefined;
  readonly blockfrostApiUrl: string | undefined;
  readonly blockfrostKey: string | undefined;
  readonly kupoUrl: string | undefined;
  readonly ogmiosUrl: string | undefined;
  readonly walletSeedPhrase: string | undefined;
  readonly walletSeedPhraseEnv: string | undefined;
  readonly walletPrivateKey: string | undefined;
  readonly walletPrivateKeyEnv: string | undefined;
  readonly fraudulentBlockOutRef: string | undefined;
  readonly fraudulentHeaderHash: string | undefined;
  readonly threadOutRef: string | undefined;
  readonly stateQueueBlockOutRef: string | undefined;
  readonly txInclusionPath: string | undefined;
  readonly tx1InputsPath: string | undefined;
  readonly tx2InputsPath: string | undefined;
  readonly doubleSpentInputIndex: string | undefined;
  readonly midgardNodeUrl: string | undefined;
  readonly midgardNodeAdminKey: string | undefined;
  readonly midgardNodeAdminKeyEnv: string | undefined;
  readonly stateQueueLeaseTtlMs: string | undefined;
  readonly transactionsPath: string | undefined;
  readonly sampleDoubleSpend: boolean;
  readonly headerHash: string | undefined;
  readonly expectedTransactionsRoot: string | undefined;
  readonly tx1Id: string | undefined;
  readonly tx2Id: string | undefined;
  readonly outputDir: string | undefined;
  readonly awaitConfirmation: boolean;
  readonly fraudCategory: SubmitInitFraudCategory | undefined;
  readonly blockValidFrom: string | undefined;
  readonly blockValidTo: string | undefined;
  readonly txId: string | undefined;
  readonly badTxId: string | undefined;
  readonly badInputIndex: string | undefined;
  readonly prevUtxosRoot: string | undefined;
  readonly prevBlockPayloadPath: string | undefined;
  readonly inputsPreimagePath: string | undefined;
  readonly outputsPreimagePath: string | undefined;
  readonly ledgerNonMembershipProofPath: string | undefined;
  readonly txsNonMembershipProofPath: string | undefined;
  readonly referenceInputsPreimagePath: string | undefined;
  readonly badReferenceInputIndex: string | undefined;
  readonly witnessSetCompactPath: string | undefined;
  readonly addrTxWitsPreimagePath: string | undefined;
  readonly badAddrTxWitIndex: string | undefined;
  readonly validationClaimCborPath: string | undefined;
  readonly challengerDescriptorCborPath: string | undefined;
  readonly validationTraceProofCborPath: string | undefined;
  readonly validationBoundaryEvidenceCborPath: string | undefined;
  readonly validationTransitionCborPath: string | undefined;
  readonly validationAuxiliaryCborPath: string | undefined;
  readonly validationResolverIndex: string | undefined;
  readonly validationSemanticResolverIndex: string | undefined;
  readonly validationDisputeRole: "operator" | "challenger" | undefined;
  readonly validationCekEnvelopeCborPath: string | undefined;
  readonly validationCekProgramMaterialSidecarCborPath: string | undefined;
  readonly validationCekIncrementalNecessityReceiptSetPath: string | undefined;
  readonly validationCekSinglePublicationOutRef: string | undefined;
  readonly validationCekMinimumMultiOutputOutRefs: readonly string[];
  readonly scaffoldSpecPath: string | undefined;
  readonly repoRoot: string | undefined;
  readonly dryRun: boolean;
};

const usage = `Usage:
  prepare-* security-grade execution consumes CanonicalBlockEvidenceV1 through executeCanonicalPrepareCommandV1.
  --midgard-node-url, --transactions-file, and --sample-double-spend are labelled diagnostics only and are rejected before proof construction.
  midgard-fault-proofs scaffold-family --scaffold-spec <familyScaffoldSpecV1.json> [--repo-root <path>] [--dry-run]
  midgard-fault-proofs inspect-contracts --blueprint <path> --deployment-info <path> [--network <Mainnet|Preview|Preprod>]
  midgard-fault-proofs submit-init --blueprint <path> --deployment-info <path> --fraudulent-block-out-ref <txHash#outputIndex> [--fraud-category <doubleSpend|invalidRange|transitionTrace|nonExistentInput|nonExistentInputNoIndex|zeroInput|validationTraceDispute|daHashPreimage|noReferenceInput|referenceInputNoIdx|invalidSignature>] [--fraudulent-header-hash <hex>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx1-inputs <raw-input-cbor-list.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx2-inputs <raw-input-cbor-list.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-range-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-range-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs-preimage <path> --bad-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --ledger-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --txs-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-open --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --validation-claim-cbor <path> --challenger-descriptor-cbor <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-verify-source --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-reveal --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-dispute-role <operator|challenger> --validation-trace-proof-cbor <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-enter-resolution --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-prepare-resolution --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-boundary-evidence-cbor <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-prepare-selected --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-transition-cbor <path> --validation-auxiliary-cbor <path> --validation-resolver-index <0..10|13> --validation-semantic-resolver-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [wallet options]
  midgard-fault-proofs submit-validation-dispute-semantic-resolution --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-transition-cbor <path> --validation-auxiliary-cbor <path> --validation-resolver-index <0..10|13> --validation-semantic-resolver-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [wallet options]
  midgard-fault-proofs submit-validation-dispute-award --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [wallet options]
  midgard-fault-proofs submit-validation-dispute-direct-resolution --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-transition-cbor <path> --validation-auxiliary-cbor <path> --validation-resolver-index <11..12> [--validation-cek-envelope-cbor <path> --validation-cek-program-material-sidecar-cbor <path>] [--validation-cek-incremental-necessity-receipt-set <path>] [--validation-cek-single-publication-out-ref <txHash#outputIndex>] [--validation-cek-multi-output-out-ref <txHash#outputIndex> ...] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [wallet options]
  midgard-fault-proofs submit-validation-dispute-enter-timeout --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-timeout --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-zero-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-zero-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-da-hash-preimage-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-da-hash-preimage-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <bad-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs-preimage <inputs-preimage.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-fold --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs-preimage <inputs-preimage.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <producing-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --outputs-preimage <outputs-preimage.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <nri-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --reference-inputs-preimage <nri-reference-inputs-preimage.json> --bad-reference-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --ledger-non-membership-proof <nri-ledger-non-membership.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --txs-non-membership-proof <nri-txs-non-membership.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <bad-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --reference-inputs-preimage <reference-inputs-preimage.json> [--bad-reference-input-index <n>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <producing-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --outputs-preimage <outputs-preimage.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-signature-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> --witness-set-compact <invalid-signature-witness-set-compact.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-signature-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --addr-tx-wits-preimage <invalid-signature-addr-tx-wits-preimage.json> --bad-addr-tx-wit-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs remove-fraudulent-block --blueprint <path> --deployment-info <path> --fraudulent-header-hash <hex> [--fraud-category <doubleSpend|invalidRange|transitionTrace|nonExistentInput|nonExistentInputNoIndex|zeroInput|validationTraceDispute|daHashPreimage|noReferenceInput|referenceInputNoIdx|invalidSignature>] [--midgard-node-url <url> --midgard-node-admin-key <key> | --midgard-node-admin-key-env <envVar>] [--state-queue-lease-ttl-ms <n>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
`;

export const parseFraudCategory = (
  value: string | undefined,
): SubmitInitFraudCategory | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (
    value === "doubleSpend" ||
    value === "invalidRange" ||
    value === "transitionTrace" ||
    value === "nonExistentInput" ||
    value === "nonExistentInputNoIndex" ||
    value === "zeroInput" ||
    value === "validationTraceDispute" ||
    value === "daHashPreimage" ||
    value === "noReferenceInput" ||
    value === "referenceInputNoIdx" ||
    value === "invalidSignature"
  ) {
    return value;
  }
  throw new Error(
    '--fraud-category must be one of "doubleSpend", "invalidRange", "transitionTrace", "nonExistentInput", "nonExistentInputNoIndex", "zeroInput", "validationTraceDispute", "daHashPreimage", "noReferenceInput", "referenceInputNoIdx", or "invalidSignature".',
  );
};

export const parseArgs = (argv: readonly string[]): ParsedArgs => {
  const [, , command, ...rest] = argv;
  if (command === "--help" || command === "-h") {
    console.log(usage);
    process.exit(0);
  }
  let blueprintPath: string | undefined;
  let deploymentInfoPath: string | undefined;
  let network: string | undefined;
  let provider: ProviderKind | undefined;
  let blockfrostApiUrl: string | undefined;
  let blockfrostKey: string | undefined;
  let kupoUrl: string | undefined;
  let ogmiosUrl: string | undefined;
  let walletSeedPhrase: string | undefined;
  let walletSeedPhraseEnv: string | undefined;
  let walletPrivateKey: string | undefined;
  let walletPrivateKeyEnv: string | undefined;
  let fraudulentBlockOutRef: string | undefined;
  let fraudulentHeaderHash: string | undefined;
  let threadOutRef: string | undefined;
  let stateQueueBlockOutRef: string | undefined;
  let txInclusionPath: string | undefined;
  let tx1InputsPath: string | undefined;
  let tx2InputsPath: string | undefined;
  let doubleSpentInputIndex: string | undefined;
  let midgardNodeUrl: string | undefined;
  let midgardNodeAdminKey: string | undefined;
  let midgardNodeAdminKeyEnv: string | undefined;
  let stateQueueLeaseTtlMs: string | undefined;
  let transactionsPath: string | undefined;
  let sampleDoubleSpend = false;
  let headerHash: string | undefined;
  let expectedTransactionsRoot: string | undefined;
  let tx1Id: string | undefined;
  let tx2Id: string | undefined;
  let outputDir: string | undefined;
  let awaitConfirmation = true;
  let fraudCategory: SubmitInitFraudCategory | undefined;
  let blockValidFrom: string | undefined;
  let blockValidTo: string | undefined;
  let txId: string | undefined;
  let badTxId: string | undefined;
  let badInputIndex: string | undefined;
  let prevUtxosRoot: string | undefined;
  let prevBlockPayloadPath: string | undefined;
  let inputsPreimagePath: string | undefined;
  let outputsPreimagePath: string | undefined;
  let ledgerNonMembershipProofPath: string | undefined;
  let txsNonMembershipProofPath: string | undefined;
  let referenceInputsPreimagePath: string | undefined;
  let badReferenceInputIndex: string | undefined;
  let witnessSetCompactPath: string | undefined;
  let addrTxWitsPreimagePath: string | undefined;
  let badAddrTxWitIndex: string | undefined;
  let validationClaimCborPath: string | undefined;
  let challengerDescriptorCborPath: string | undefined;
  let validationTraceProofCborPath: string | undefined;
  let validationBoundaryEvidenceCborPath: string | undefined;
  let validationTransitionCborPath: string | undefined;
  let validationAuxiliaryCborPath: string | undefined;
  let validationResolverIndex: string | undefined;
  let validationSemanticResolverIndex: string | undefined;
  let validationDisputeRole: "operator" | "challenger" | undefined;
  let validationCekEnvelopeCborPath: string | undefined;
  let validationCekProgramMaterialSidecarCborPath: string | undefined;
  let validationCekIncrementalNecessityReceiptSetPath: string | undefined;
  let validationCekSinglePublicationOutRef: string | undefined;
  const validationCekMinimumMultiOutputOutRefs: string[] = [];
  let scaffoldSpecPath: string | undefined;
  let repoRoot: string | undefined;
  let dryRun = false;

  for (let index = 0; index < rest.length; index += 1) {
    const arg = rest[index];
    switch (arg) {
      case "--blueprint":
        blueprintPath = rest[++index];
        break;
      case "--deployment-info":
        deploymentInfoPath = rest[++index];
        break;
      case "--network":
        network = rest[++index];
        break;
      case "--provider": {
        const value = rest[++index];
        if (value !== "Blockfrost" && value !== "Kupmios") {
          throw new Error(
            '--provider must be either "Blockfrost" or "Kupmios".',
          );
        }
        provider = value;
        break;
      }
      case "--blockfrost-api-url":
        blockfrostApiUrl = rest[++index];
        break;
      case "--blockfrost-key":
        blockfrostKey = rest[++index];
        break;
      case "--kupo-url":
        kupoUrl = rest[++index];
        break;
      case "--ogmios-url":
        ogmiosUrl = rest[++index];
        break;
      case "--wallet-seed-phrase":
        walletSeedPhrase = rest[++index];
        break;
      case "--wallet-seed-phrase-env":
        walletSeedPhraseEnv = rest[++index];
        break;
      case "--wallet-private-key":
        walletPrivateKey = rest[++index];
        break;
      case "--wallet-private-key-env":
        walletPrivateKeyEnv = rest[++index];
        break;
      case "--fraudulent-block-out-ref":
        fraudulentBlockOutRef = rest[++index];
        break;
      case "--fraudulent-header-hash":
        fraudulentHeaderHash = rest[++index];
        break;
      case "--thread-out-ref":
        threadOutRef = rest[++index];
        break;
      case "--state-queue-block-out-ref":
        stateQueueBlockOutRef = rest[++index];
        break;
      case "--tx-inclusion":
        txInclusionPath = rest[++index];
        break;
      case "--tx1-inputs":
        tx1InputsPath = rest[++index];
        break;
      case "--tx2-inputs":
        tx2InputsPath = rest[++index];
        break;
      case "--double-spent-input-index":
        doubleSpentInputIndex = rest[++index];
        break;
      case "--midgard-node-url":
        midgardNodeUrl = rest[++index];
        break;
      case "--midgard-node-admin-key":
        midgardNodeAdminKey = rest[++index];
        break;
      case "--midgard-node-admin-key-env":
        midgardNodeAdminKeyEnv = rest[++index];
        break;
      case "--state-queue-lease-ttl-ms":
        stateQueueLeaseTtlMs = rest[++index];
        break;
      case "--transactions-file":
        transactionsPath = rest[++index];
        break;
      case "--sample-double-spend":
        sampleDoubleSpend = true;
        break;
      case "--header-hash":
        headerHash = rest[++index];
        break;
      case "--expected-transactions-root":
        expectedTransactionsRoot = rest[++index];
        break;
      case "--tx1-id":
        tx1Id = rest[++index];
        break;
      case "--tx2-id":
        tx2Id = rest[++index];
        break;
      case "--output-dir":
        outputDir = rest[++index];
        break;
      case "--fraud-category":
        fraudCategory = parseFraudCategory(rest[++index]);
        break;
      case "--block-valid-from":
        blockValidFrom = rest[++index];
        break;
      case "--block-valid-to":
        blockValidTo = rest[++index];
        break;
      case "--tx-id":
        txId = rest[++index];
        break;
      case "--bad-tx-id":
        badTxId = rest[++index];
        break;
      case "--bad-input-index":
        badInputIndex = rest[++index];
        break;
      case "--prev-utxos-root":
        prevUtxosRoot = rest[++index];
        break;
      case "--prev-block-payload-file":
        prevBlockPayloadPath = rest[++index];
        break;
      case "--inputs-preimage":
        inputsPreimagePath = rest[++index];
        break;
      case "--outputs-preimage":
        outputsPreimagePath = rest[++index];
        break;
      case "--ledger-non-membership-proof":
        ledgerNonMembershipProofPath = rest[++index];
        break;
      case "--txs-non-membership-proof":
        txsNonMembershipProofPath = rest[++index];
        break;
      case "--reference-inputs-preimage":
        referenceInputsPreimagePath = rest[++index];
        break;
      case "--bad-reference-input-index":
        badReferenceInputIndex = rest[++index];
        break;
      case "--witness-set-compact":
        witnessSetCompactPath = rest[++index];
        break;
      case "--addr-tx-wits-preimage":
        addrTxWitsPreimagePath = rest[++index];
        break;
      case "--bad-addr-tx-wit-index":
        badAddrTxWitIndex = rest[++index];
        break;
      case "--validation-claim-cbor":
        validationClaimCborPath = rest[++index];
        break;
      case "--challenger-descriptor-cbor":
        challengerDescriptorCborPath = rest[++index];
        break;
      case "--validation-trace-proof-cbor":
        validationTraceProofCborPath = rest[++index];
        break;
      case "--validation-boundary-evidence-cbor":
        validationBoundaryEvidenceCborPath = rest[++index];
        break;
      case "--validation-transition-cbor":
        validationTransitionCborPath = rest[++index];
        break;
      case "--validation-auxiliary-cbor":
        validationAuxiliaryCborPath = rest[++index];
        break;
      case "--validation-resolver-index":
        validationResolverIndex = rest[++index];
        break;
      case "--validation-semantic-resolver-index":
        validationSemanticResolverIndex = rest[++index];
        break;
      case "--validation-cek-envelope-cbor":
        validationCekEnvelopeCborPath = rest[++index];
        break;
      case "--validation-cek-program-material-sidecar-cbor":
        validationCekProgramMaterialSidecarCborPath = rest[++index];
        break;
      case "--validation-cek-incremental-necessity-receipt-set":
        validationCekIncrementalNecessityReceiptSetPath = rest[++index];
        break;
      case "--validation-cek-single-publication-out-ref":
        validationCekSinglePublicationOutRef = rest[++index];
        break;
      case "--validation-cek-multi-output-out-ref": {
        const outRef = rest[++index];
        if (outRef === undefined) {
          throw new Error(
            "--validation-cek-multi-output-out-ref requires a txHash#outputIndex value",
          );
        }
        validationCekMinimumMultiOutputOutRefs.push(outRef);
        break;
      }
      case "--validation-dispute-role": {
        const role = rest[++index];
        if (role !== "operator" && role !== "challenger") {
          throw new Error(
            '--validation-dispute-role must be either "operator" or "challenger".',
          );
        }
        validationDisputeRole = role;
        break;
      }
      case "--no-await-confirmation":
        awaitConfirmation = false;
        break;
      case "--scaffold-spec":
        scaffoldSpecPath = rest[++index];
        break;
      case "--repo-root":
        repoRoot = rest[++index];
        break;
      case "--dry-run":
        dryRun = true;
        break;
      case "--help":
      case "-h":
        console.log(usage);
        process.exit(0);
      default:
        throw new Error(`Unknown argument: ${arg}`);
    }
  }

  return {
    command,
    blueprintPath,
    deploymentInfoPath,
    network,
    provider,
    blockfrostApiUrl,
    blockfrostKey,
    kupoUrl,
    ogmiosUrl,
    walletSeedPhrase,
    walletSeedPhraseEnv,
    walletPrivateKey,
    walletPrivateKeyEnv,
    fraudulentBlockOutRef,
    fraudulentHeaderHash,
    threadOutRef,
    stateQueueBlockOutRef,
    txInclusionPath,
    tx1InputsPath,
    tx2InputsPath,
    doubleSpentInputIndex,
    midgardNodeUrl,
    midgardNodeAdminKey,
    midgardNodeAdminKeyEnv,
    stateQueueLeaseTtlMs,
    transactionsPath,
    sampleDoubleSpend,
    headerHash,
    expectedTransactionsRoot,
    tx1Id,
    tx2Id,
    outputDir,
    awaitConfirmation,
    fraudCategory,
    blockValidFrom,
    blockValidTo,
    txId,
    badTxId,
    badInputIndex,
    prevUtxosRoot,
    prevBlockPayloadPath,
    inputsPreimagePath,
    outputsPreimagePath,
    ledgerNonMembershipProofPath,
    txsNonMembershipProofPath,
    referenceInputsPreimagePath,
    badReferenceInputIndex,
    witnessSetCompactPath,
    addrTxWitsPreimagePath,
    badAddrTxWitIndex,
    validationClaimCborPath,
    challengerDescriptorCborPath,
    validationTraceProofCborPath,
    validationBoundaryEvidenceCborPath,
    validationTransitionCborPath,
    validationAuxiliaryCborPath,
    validationResolverIndex,
    validationSemanticResolverIndex,
    validationDisputeRole,
    validationCekEnvelopeCborPath,
    validationCekProgramMaterialSidecarCborPath,
    validationCekIncrementalNecessityReceiptSetPath,
    validationCekSinglePublicationOutRef,
    validationCekMinimumMultiOutputOutRefs,
    scaffoldSpecPath,
    repoRoot,
    dryRun,
  };
};

export const buildRemoveFraudulentBlockCliConfig = (args: ParsedArgs) => {
  if (args.blueprintPath === undefined) {
    throw new Error(`Missing required --blueprint <path>.\n${usage}`);
  }
  if (args.deploymentInfoPath === undefined) {
    throw new Error(`Missing required --deployment-info <path>.\n${usage}`);
  }
  if (args.fraudulentHeaderHash === undefined) {
    throw new Error(
      `Missing required --fraudulent-header-hash <hex>.\n${usage}`,
    );
  }
  return {
    blueprintPath: args.blueprintPath,
    deploymentInfoPath: args.deploymentInfoPath,
    network: parseNetwork(args.network),
    provider: args.provider,
    blockfrostApiUrl: args.blockfrostApiUrl,
    blockfrostKey: args.blockfrostKey,
    kupoUrl: args.kupoUrl,
    ogmiosUrl: args.ogmiosUrl,
    walletSeedPhrase: args.walletSeedPhrase,
    walletSeedPhraseEnv: args.walletSeedPhraseEnv,
    walletPrivateKey: args.walletPrivateKey,
    walletPrivateKeyEnv: args.walletPrivateKeyEnv,
    fraudCategory: args.fraudCategory,
    fraudulentHeaderHash: args.fraudulentHeaderHash,
    awaitConfirmation: args.awaitConfirmation,
    midgardNodeUrl: args.midgardNodeUrl,
    midgardNodeAdminKey: args.midgardNodeAdminKey,
    midgardNodeAdminKeyEnv: args.midgardNodeAdminKeyEnv,
    stateQueueLeaseTtlMs:
      args.stateQueueLeaseTtlMs === undefined
        ? undefined
        : Number(args.stateQueueLeaseTtlMs),
  };
};

const writeJson = (value: unknown): void => {
  process.stdout.write(stringifyJson(value));
};

const requireValidationOneStepCliArguments = (
  args: ParsedArgs,
  staged: boolean,
) => {
  if (args.validationTransitionCborPath === undefined) {
    throw new Error(
      `Missing required --validation-transition-cbor <path>.\n${usage}`,
    );
  }
  if (args.validationAuxiliaryCborPath === undefined) {
    throw new Error(
      `Missing required --validation-auxiliary-cbor <path>.\n${usage}`,
    );
  }
  if (
    args.validationResolverIndex === undefined ||
    !/^(?:0|[1-9][0-9]*)$/u.test(args.validationResolverIndex)
  ) {
    throw new Error(
      `Missing or invalid --validation-resolver-index <n>.\n${usage}`,
    );
  }
  const resolverIndex = Number(args.validationResolverIndex);
  const semanticText = args.validationSemanticResolverIndex;
  if (
    staged &&
    (semanticText === undefined || !/^(?:0|[1-9][0-9]*)$/u.test(semanticText))
  ) {
    throw new Error(
      `Missing or invalid --validation-semantic-resolver-index <n>.\n${usage}`,
    );
  }
  if (!staged && semanticText !== undefined) {
    throw new Error(
      "Direct validation resolution must not select a semantic resolver",
    );
  }
  if (
    staged &&
    (args.validationCekEnvelopeCborPath !== undefined ||
      args.validationCekProgramMaterialSidecarCborPath !== undefined ||
      args.validationCekIncrementalNecessityReceiptSetPath !== undefined ||
      args.validationCekSinglePublicationOutRef !== undefined ||
      args.validationCekMinimumMultiOutputOutRefs.length > 0)
  ) {
    throw new Error(
      "CEK route material, necessity receipts, and publication outrefs are permitted only for submit-validation-dispute-direct-resolution",
    );
  }
  return {
    validationTransitionCborPath: args.validationTransitionCborPath,
    validationAuxiliaryCborPath: args.validationAuxiliaryCborPath,
    validationResolverIndex: resolverIndex,
    validationSemanticResolverIndex:
      semanticText === undefined ? null : Number(semanticText),
    ...(staged
      ? {}
      : {
          validationCekEnvelopeCborPath: args.validationCekEnvelopeCborPath,
          validationCekProgramMaterialSidecarCborPath:
            args.validationCekProgramMaterialSidecarCborPath,
          validationCekIncrementalNecessityReceiptSetPath:
            args.validationCekIncrementalNecessityReceiptSetPath,
          validationCekSinglePublicationOutRef:
            args.validationCekSinglePublicationOutRef,
          validationCekMinimumMultiOutputOutRefs:
            args.validationCekMinimumMultiOutputOutRefs.length === 0
              ? undefined
              : args.validationCekMinimumMultiOutputOutRefs,
        }),
  };
};

export const isCliEntrypoint = ({
  moduleUrl,
  argvPath,
}: {
  readonly moduleUrl: string;
  readonly argvPath: string | undefined;
}): boolean => {
  if (argvPath === undefined) {
    return false;
  }
  try {
    return realpathSync(fileURLToPath(moduleUrl)) === realpathSync(argvPath);
  } catch {
    return false;
  }
};

export const main = async (): Promise<void> => {
  const args = parseArgs(process.argv);

  // Q02: the family scaffold generator writes source skeletons, so it takes no
  // network, wallet, or evidence input and is handled before every chain path.
  if (args.command === "scaffold-family") {
    if (args.scaffoldSpecPath === undefined) {
      throw new Error(`Missing required --scaffold-spec <path>.\n${usage}`);
    }
    const plan = generateFraudProofFamilyScaffoldV1({
      spec: await readJsonFile(args.scaffoldSpecPath),
    });
    const result = await writeFraudProofFamilyScaffoldV1({
      plan,
      repoRoot: args.repoRoot ?? process.cwd(),
      dryRun: args.dryRun,
    });
    console.log(
      stringifyJson({
        generator: plan.generator,
        family: plan.spec.family,
        taskId: plan.spec.taskId,
        dryRun: result.dryRun,
        repoRoot: result.repoRoot,
        written: result.written,
      }),
    );
    return;
  }

  if (
    args.command !== "prepare-double-spend" &&
    args.command !== "prepare-invalid-range" &&
    args.command !== "prepare-non-existent-input" &&
    args.command !== "prepare-zero-input" &&
    args.command !== "prepare-input-no-idx" &&
    args.command !== "inspect-contracts" &&
    args.command !== "submit-init" &&
    args.command !== "submit-step-01" &&
    args.command !== "submit-step-02" &&
    args.command !== "submit-step-03" &&
    args.command !== "submit-step-04" &&
    args.command !== "submit-invalid-range-step-01" &&
    args.command !== "submit-invalid-range-step-02" &&
    args.command !== "submit-non-existent-input-step-01" &&
    args.command !== "submit-non-existent-input-step-02" &&
    args.command !== "submit-non-existent-input-step-03" &&
    args.command !== "submit-non-existent-input-step-04" &&
    args.command !== "submit-zero-input-step-01" &&
    args.command !== "submit-zero-input-step-02" &&
    args.command !== "submit-da-hash-preimage-step-01" &&
    args.command !== "submit-da-hash-preimage-step-02" &&
    args.command !== "submit-input-no-idx-step-01" &&
    args.command !== "submit-input-no-idx-step-02" &&
    args.command !== "submit-input-no-idx-fold" &&
    args.command !== "submit-input-no-idx-step-03" &&
    args.command !== "submit-input-no-idx-step-04" &&
    args.command !== "submit-no-reference-input-step-01" &&
    args.command !== "submit-no-reference-input-step-02" &&
    args.command !== "submit-no-reference-input-step-03" &&
    args.command !== "submit-no-reference-input-step-04" &&
    args.command !== "submit-reference-input-no-idx-step-01" &&
    args.command !== "submit-reference-input-no-idx-step-02" &&
    args.command !== "submit-reference-input-no-idx-step-03" &&
    args.command !== "submit-reference-input-no-idx-step-04" &&
    args.command !== "submit-invalid-signature-step-01" &&
    args.command !== "submit-invalid-signature-step-02" &&
    args.command !== "submit-validation-dispute-open" &&
    args.command !== "submit-validation-dispute-verify-source" &&
    args.command !== "submit-validation-dispute-reveal" &&
    args.command !== "submit-validation-dispute-enter-resolution" &&
    args.command !== "submit-validation-dispute-prepare-resolution" &&
    args.command !== "submit-validation-dispute-prepare-selected" &&
    args.command !== "submit-validation-dispute-semantic-resolution" &&
    args.command !== "submit-validation-dispute-award" &&
    args.command !== "submit-validation-dispute-direct-resolution" &&
    args.command !== "submit-validation-dispute-enter-timeout" &&
    args.command !== "submit-validation-dispute-timeout" &&
    args.command !== "remove-fraudulent-block"
  ) {
    throw new Error(
      `Expected a supported prepare, inspect, submit, validation-dispute, or removal command.\n${usage}`,
    );
  }

  // RF-043: reject every legacy diagnostic submission route before any
  // blueprint/deployment read or provider/wallet construction.  Only
  // canonical evidence submitters may cross this boundary in the future.
  rejectRetiredUnauthenticatedSubmissionRouteV1({
    command: args.command,
    fraudCategory: args.fraudCategory,
  });

  // Q03: the executable CLI has no operator-private compatibility route. The
  // current REST/file/sample flags are retained solely as clearly labelled
  // diagnostic imports and are rejected by the same security-grade gate every
  // canonical builder uses. Security-grade callers route all four verbs through
  // `executeCanonicalPrepareCommandV1`, supplying verified DA/L1 evidence from
  // the watcher/public transport rather than claiming a local file is trusted.
  if (args.command.startsWith("prepare-")) {
    if (
      args.command === "prepare-zero-input" &&
      args.expectedTransactionsRoot === undefined
    ) {
      throw new Error(
        `Missing required --expected-transactions-root <hex>.\n${usage}`,
      );
    }
    const provenance =
      args.midgardNodeUrl !== undefined
        ? MIDGARD_NODE_URL_DIAGNOSTIC_PROVENANCE_V1
        : args.transactionsPath !== undefined
          ? LOCAL_FILE_DIAGNOSTIC_PROVENANCE_V1
          : SAMPLE_EVIDENCE_DIAGNOSTIC_PROVENANCE_V1;
    process.stderr.write(`${diagnosticEvidenceBannerV1(provenance)}\n`);
    assertSecurityGradeEvidenceV1(provenance);
    // `assertSecurityGradeEvidenceV1` always throws for these prohibited
    // diagnostic trust classes. This return documents that no prepare command
    // can fall through to blueprint/wallet/submission paths.
    return;
  }

  if (args.blueprintPath === undefined) {
    throw new Error(`Missing required --blueprint <path>.\n${usage}`);
  }
  if (args.deploymentInfoPath === undefined) {
    throw new Error(`Missing required --deployment-info <path>.\n${usage}`);
  }

  if (args.command === "submit-init") {
    if (args.fraudulentBlockOutRef === undefined) {
      throw new Error(
        `Missing required --fraudulent-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitInitFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      fraudCategory: args.fraudCategory,
      fraudulentBlockOutRef: args.fraudulentBlockOutRef,
      fraudulentHeaderHash: args.fraudulentHeaderHash,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-open") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.validationClaimCborPath === undefined) {
      throw new Error(
        `Missing required --validation-claim-cbor <path>.\n${usage}`,
      );
    }
    if (args.challengerDescriptorCborPath === undefined) {
      throw new Error(
        `Missing required --challenger-descriptor-cbor <path>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeOpenFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      claimCborPath: args.validationClaimCborPath,
      challengerDescriptorCborPath: args.challengerDescriptorCborPath,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-verify-source") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeVerifySourceFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-reveal") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.validationDisputeRole === undefined) {
      throw new Error(
        `Missing required --validation-dispute-role <operator|challenger>.\n${usage}`,
      );
    }
    if (args.validationTraceProofCborPath === undefined) {
      throw new Error(
        `Missing required --validation-trace-proof-cbor <path>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeRevealFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      role: args.validationDisputeRole,
      proofCborPath: args.validationTraceProofCborPath,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-enter-resolution") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeEnterResolutionFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-prepare-resolution") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.validationBoundaryEvidenceCborPath === undefined) {
      throw new Error(
        `Missing required --validation-boundary-evidence-cbor <path>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputePrepareResolutionFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      boundaryEvidenceCborPath: args.validationBoundaryEvidenceCborPath,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (
    args.command === "submit-validation-dispute-prepare-selected" ||
    args.command === "submit-validation-dispute-semantic-resolution" ||
    args.command === "submit-validation-dispute-direct-resolution"
  ) {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const staged =
      args.command !== "submit-validation-dispute-direct-resolution";
    const oneStep = requireValidationOneStepCliArguments(args, staged);
    const config = {
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
      ...oneStep,
    };
    const output =
      args.command === "submit-validation-dispute-prepare-selected"
        ? await submitValidationDisputePrepareSelectedFromFiles(config)
        : args.command === "submit-validation-dispute-semantic-resolution"
          ? await submitValidationDisputeSemanticResolutionFromFiles(config)
          : await submitValidationDisputeDirectResolutionFromFiles(config);
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-award") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeAwardFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-enter-timeout") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeEnterTimeoutFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-timeout") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeTimeoutFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-range-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitInvalidRangeStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-range-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitInvalidRangeStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-zero-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitZeroInputStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-zero-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitZeroInputStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitInputNoIdxStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.inputsPreimagePath === undefined) {
      throw new Error(`Missing required --inputs-preimage <path>.\n${usage}`);
    }
    const output = await submitInputNoIdxStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      inputsPreimagePath: args.inputsPreimagePath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-fold") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.inputsPreimagePath === undefined) {
      throw new Error(`Missing required --inputs-preimage <path>.\n${usage}`);
    }
    const output = await submitInputNoIdxStep02UntilTerminalFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      inputsPreimagePath: args.inputsPreimagePath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitInputNoIdxStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.outputsPreimagePath === undefined) {
      throw new Error(`Missing required --outputs-preimage <path>.\n${usage}`);
    }
    const output = await submitInputNoIdxStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      outputsPreimagePath: args.outputsPreimagePath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitNoReferenceInputStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.referenceInputsPreimagePath === undefined) {
      throw new Error(
        `Missing required --reference-inputs-preimage <path>.\n${usage}`,
      );
    }
    if (args.badReferenceInputIndex === undefined) {
      throw new Error(
        `Missing required --bad-reference-input-index <n>.\n${usage}`,
      );
    }
    const output = await submitNoReferenceInputStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      referenceInputsPreimagePath: args.referenceInputsPreimagePath,
      badReferenceInputIndex: args.badReferenceInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.ledgerNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --ledger-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await submitNoReferenceInputStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      ledgerNonMembershipProofPath: args.ledgerNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txsNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --txs-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await submitNoReferenceInputStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      txsNonMembershipProofPath: args.txsNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitReferenceInputNoIdxStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.referenceInputsPreimagePath === undefined) {
      throw new Error(
        `Missing required --reference-inputs-preimage <path>.\n${usage}`,
      );
    }
    const output = await submitReferenceInputNoIdxStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      referenceInputsPreimagePath: args.referenceInputsPreimagePath,
      badReferenceInputIndex: args.badReferenceInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitReferenceInputNoIdxStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.outputsPreimagePath === undefined) {
      throw new Error(`Missing required --outputs-preimage <path>.\n${usage}`);
    }
    const output = await submitReferenceInputNoIdxStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      outputsPreimagePath: args.outputsPreimagePath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-signature-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    if (args.witnessSetCompactPath === undefined) {
      throw new Error(
        `Missing required --witness-set-compact <path>.\n${usage}`,
      );
    }
    const output = await submitInvalidSignatureStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      witnessSetCompactPath: args.witnessSetCompactPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-signature-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.addrTxWitsPreimagePath === undefined) {
      throw new Error(
        `Missing required --addr-tx-wits-preimage <path>.\n${usage}`,
      );
    }
    if (args.badAddrTxWitIndex === undefined) {
      throw new Error(
        `Missing required --bad-addr-tx-wit-index <n>.\n${usage}`,
      );
    }
    const output = await submitInvalidSignatureStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      addrTxWitsPreimagePath: args.addrTxWitsPreimagePath,
      badAddrTxWitIndex: args.badAddrTxWitIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-da-hash-preimage-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitDaHashPreimageStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-da-hash-preimage-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitDaHashPreimageStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.tx1InputsPath === undefined) {
      throw new Error(`Missing required --tx1-inputs <path>.\n${usage}`);
    }
    if (args.doubleSpentInputIndex === undefined) {
      throw new Error(
        `Missing required --double-spent-input-index <n>.\n${usage}`,
      );
    }
    const output = await submitStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      tx1InputsPath: args.tx1InputsPath,
      doubleSpentInputIndex: args.doubleSpentInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.tx2InputsPath === undefined) {
      throw new Error(`Missing required --tx2-inputs <path>.\n${usage}`);
    }
    if (args.doubleSpentInputIndex === undefined) {
      throw new Error(
        `Missing required --double-spent-input-index <n>.\n${usage}`,
      );
    }
    const output = await submitStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      tx2InputsPath: args.tx2InputsPath,
      doubleSpentInputIndex: args.doubleSpentInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await neSubmitStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.inputsPreimagePath === undefined) {
      throw new Error(`Missing required --inputs-preimage <path>.\n${usage}`);
    }
    if (args.badInputIndex === undefined) {
      throw new Error(`Missing required --bad-input-index <n>.\n${usage}`);
    }
    const output = await neSubmitStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      inputsPreimagePath: args.inputsPreimagePath,
      badInputIndex: args.badInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.ledgerNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --ledger-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await neSubmitStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      ledgerNonMembershipProofPath: args.ledgerNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-non-existent-input-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txsNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --txs-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await neSubmitStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      txsNonMembershipProofPath: args.txsNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "remove-fraudulent-block") {
    const output = await submitRemoveFraudulentBlockFromFiles(
      buildRemoveFraudulentBlockCliConfig(args),
    );

    writeJson(output);
    return;
  }

  const output = await inspectContractsFromFiles({
    blueprintPath: args.blueprintPath,
    deploymentInfoPath: args.deploymentInfoPath,
    network: parseNetwork(args.network),
  });

  writeJson(output);
};

if (
  isCliEntrypoint({ moduleUrl: import.meta.url, argvPath: process.argv[1] })
) {
  main().catch((error: unknown) => {
    process.stderr.write(
      `midgard-fault-proofs: ${formatUnknownError(error)}\n`,
    );
    process.exitCode = 1;
  });
}
