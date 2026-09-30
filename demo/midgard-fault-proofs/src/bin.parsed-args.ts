import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import { type ProviderKind } from "./runtime.js";
import { type SubmitInitFraudCategory } from "./submit-init.js";

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
  readonly transactionsPath: string | undefined;
  readonly sampleDoubleSpend: boolean;
  readonly headerHash: string | undefined;
  readonly expectedTransactionsRoot: string | undefined;
  readonly tx1Id: string | undefined;
  readonly tx2Id: string | undefined;
  readonly outputDir: string | undefined;
  readonly daPayloadEnvelopePath: string | undefined;
  readonly transitionFaultProofPath: string | undefined;
  readonly referenceInputOutRefs: readonly string[];
  readonly awaitConfirmation: boolean;
  readonly fraudCategory: SubmitInitFraudCategory | undefined;
  readonly blockSlot: string | undefined;
  readonly txId: string | undefined;
  readonly badTxId: string | undefined;
  readonly badInputIndex: string | undefined;
  readonly prevUtxosRoot: string | undefined;
  readonly prevBlockPayloadPath: string | undefined;
  readonly inputsPreimagePath: string | undefined;
  readonly depositInclusionPath: string | undefined;
  readonly withdrawalInclusionPath: string | undefined;
  readonly eventOutRef: string | undefined;
  readonly authenticContentPath: string | undefined;
  readonly outputsPreimagePath: string | undefined;
  readonly ledgerNonMembershipProofPath: string | undefined;
  readonly txsNonMembershipProofPath: string | undefined;
  readonly referenceInputsPreimagePath: string | undefined;
  readonly badReferenceInputIndex: string | undefined;
  readonly witnessSetCompactPath: string | undefined;
  readonly nativeTxCompactPath: string | undefined;
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
  readonly workflowJournalDir: string | undefined;
  readonly workflowRuntimeConfigPath: string | undefined;
  readonly deploymentFingerprint: string | undefined;
  readonly correctionJournalPath: string | undefined;
};

const fraudCategoryUsage = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.join("|");

export const usage = `Usage:
  prepare-* security-grade execution consumes CanonicalBlockEvidenceV1 through executeCanonicalPrepareCommandV1.
  --midgard-node-url, --transactions-file, and --sample-double-spend are labelled diagnostics only and are rejected before proof construction.
  remove-* coordinate state-queue mutation locally and never contact a Midgard node. A non-tail removal that loses a race to a competing commit or merge fails and must be re-run; the watcher's workflow orchestrator retries it until confirmed.
  midgard-fault-proofs workflow-readiness [--fraud-category <${fraudCategoryUsage}>]
  midgard-fault-proofs run-workflow --fraud-category <${fraudCategoryUsage}> --deployment-fingerprint <32-byte hex> --header-hash <28-byte hex> --workflow-journal-dir <directory> --workflow-runtime-config <versioned-infrastructure-config.json>
  midgard-fault-proofs resume-workflow --fraud-category <${fraudCategoryUsage}> --deployment-fingerprint <32-byte hex> --header-hash <28-byte hex> --workflow-journal-dir <directory> --workflow-runtime-config <versioned-infrastructure-config.json>
  midgard-fault-proofs inspect-contracts --blueprint <path> --deployment-info <path> [--network <Mainnet|Preview|Preprod>]
  midgard-fault-proofs prepare-transition-trace --da-payload-envelope <retained-da-envelope.cbor(.json)> --header-hash <committed 28-byte header hash, hex> [--output-dir <dir>]
  midgard-fault-proofs submit-transition-trace-proof --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --transition-fault-proof <proof.cbor(.json)> [--reference-input <txHash#outputIndex> ...] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-init --blueprint <path> --deployment-info <path> --fraudulent-block-out-ref <txHash#outputIndex> [--fraud-category <${fraudCategoryUsage}>] [--fraudulent-header-hash <hex>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx1-inputs <raw-input-cbor-list.json> --native-tx-compact <tx1-compact.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --tx2-inputs <raw-input-cbor-list.json> --native-tx-compact <tx2-compact.json> --double-spent-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-range-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-range-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-fabricated-deposit-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --deposit-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-fabricated-deposit-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--event-out-ref <txHash#outputIndex>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-fabricated-deposit-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--authentic-content <path>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-fabricated-deposit-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-fabricated-withdrawal-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --withdrawal-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-fabricated-withdrawal-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--event-out-ref <txHash#outputIndex>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-fabricated-withdrawal-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--authentic-content <path>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-fabricated-withdrawal-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation]
  midgard-fault-proofs submit-non-existent-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs-preimage <path> --native-tx-compact <native-tx-compact.json> --bad-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --ledger-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-non-existent-input-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --txs-non-membership-proof <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-open --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --validation-claim-cbor <path> --challenger-descriptor-cbor <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-verify-source --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-reveal --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-dispute-role <operator|challenger> --validation-trace-proof-cbor <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-enter-resolution --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-prepare-resolution --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-boundary-evidence-cbor <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-prepare-selected --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-transition-cbor <path> --validation-auxiliary-cbor <path> --validation-resolver-index <0..13> --validation-semantic-resolver-index <n> [--validation-cek-envelope-cbor <path> --validation-cek-program-material-sidecar-cbor <path>] [--validation-cek-incremental-necessity-receipt-set <path>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [wallet options]
  midgard-fault-proofs submit-validation-dispute-semantic-resolution --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --validation-transition-cbor <path> --validation-auxiliary-cbor <path> --validation-resolver-index <0..13> --validation-semantic-resolver-index <n> [--validation-cek-envelope-cbor <path> --validation-cek-program-material-sidecar-cbor <path>] [--validation-cek-incremental-necessity-receipt-set <path>] [--validation-cek-single-publication-out-ref <txHash#outputIndex>] [--validation-cek-multi-output-out-ref <txHash#outputIndex> ...] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [wallet options]
  midgard-fault-proofs submit-validation-dispute-award --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [wallet options]
  midgard-fault-proofs submit-validation-dispute-enter-timeout --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-validation-dispute-timeout --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-zero-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-zero-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --native-tx-compact <native-tx-compact.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-da-hash-preimage-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-da-hash-preimage-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <bad-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --inputs-preimage <inputs-preimage.json> --native-tx-compact <native-tx-compact.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-fold  (RETIRED by #604 — step 02 has one route; use submit-input-no-idx-step-02)
  midgard-fault-proofs submit-input-no-idx-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <producing-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-input-no-idx-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --outputs-preimage <outputs-preimage.json> --native-tx-compact <producing-tx-compact.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <nri-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --reference-inputs-preimage <nri-reference-inputs-preimage.json> --native-tx-compact <native-tx-compact.json> --bad-reference-input-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --ledger-non-membership-proof <nri-ledger-non-membership.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-no-reference-input-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --txs-non-membership-proof <nri-txs-non-membership.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <bad-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --reference-inputs-preimage <reference-inputs-preimage.json> --native-tx-compact <native-tx-compact.json> [--bad-reference-input-index <n>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-03 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <producing-tx-inclusion.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-reference-input-no-idx-step-04 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --outputs-preimage <outputs-preimage.json> --native-tx-compact <producing-tx-compact.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-signature-step-01 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --state-queue-block-out-ref <txHash#outputIndex> --tx-inclusion <path> --witness-set-compact <invalid-signature-witness-set-compact.json> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs submit-invalid-signature-step-02 --blueprint <path> --deployment-info <path> --thread-out-ref <txHash#outputIndex> --addr-tx-wits-preimage <invalid-signature-addr-tx-wits-preimage.json> --native-tx-compact <native-tx-compact.json> --witness-set-compact <invalid-signature-witness-set-compact.json> --bad-addr-tx-wit-index <n> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs remove-fraudulent-block --blueprint <path> --deployment-info <path> --fraudulent-header-hash <hex> [--fraud-category <${fraudCategoryUsage}>] [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--wallet-seed-phrase <phrase> | --wallet-seed-phrase-env <envVar> | --wallet-private-key <bech32> | --wallet-private-key-env <envVar>]
  midgard-fault-proofs remove-unattested-block --deployment-info <path> --correction-journal <path> [--network <Mainnet|Preview|Preprod>] [--provider <Blockfrost|Kupmios>] [--no-await-confirmation] [wallet options]
`;

export const parseFraudCategory = (
  value: string | undefined,
): SubmitInitFraudCategory | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const category = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.find(
    (candidate) => candidate === value,
  );
  if (category !== undefined) {
    return category;
  }
  throw new Error(
    `--fraud-category must be one of ${FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((candidate) => `"${candidate}"`).join(", ")}.`,
  );
};
