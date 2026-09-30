import {
  type ParsedArgs,
  parseFraudCategory,
  usage,
} from "./bin.parsed-args.js";
import { type ProviderKind } from "./runtime.js";
import { type SubmitInitFraudCategory } from "./submit-init.js";

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
  let transactionsPath: string | undefined;
  let sampleDoubleSpend = false;
  let headerHash: string | undefined;
  let expectedTransactionsRoot: string | undefined;
  let tx1Id: string | undefined;
  let tx2Id: string | undefined;
  let outputDir: string | undefined;
  let daPayloadEnvelopePath: string | undefined;
  let transitionFaultProofPath: string | undefined;
  const referenceInputOutRefs: string[] = [];
  let awaitConfirmation = true;
  let fraudCategory: SubmitInitFraudCategory | undefined;
  let blockSlot: string | undefined;
  let txId: string | undefined;
  let badTxId: string | undefined;
  let badInputIndex: string | undefined;
  let prevUtxosRoot: string | undefined;
  let prevBlockPayloadPath: string | undefined;
  let inputsPreimagePath: string | undefined;
  let depositInclusionPath: string | undefined;
  let withdrawalInclusionPath: string | undefined;
  let eventOutRef: string | undefined;
  let authenticContentPath: string | undefined;
  let outputsPreimagePath: string | undefined;
  let ledgerNonMembershipProofPath: string | undefined;
  let txsNonMembershipProofPath: string | undefined;
  let referenceInputsPreimagePath: string | undefined;
  let badReferenceInputIndex: string | undefined;
  let witnessSetCompactPath: string | undefined;
  let nativeTxCompactPath: string | undefined;
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
  let workflowJournalDir: string | undefined;
  let workflowRuntimeConfigPath: string | undefined;
  let deploymentFingerprint: string | undefined;
  let correctionJournalPath: string | undefined;

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
      case "--da-payload-envelope":
        daPayloadEnvelopePath = rest[++index];
        break;
      case "--transition-fault-proof":
        transitionFaultProofPath = rest[++index];
        break;
      case "--reference-input": {
        const outRef = rest[++index];
        if (outRef === undefined) {
          throw new Error(
            "--reference-input requires a txHash#outputIndex value",
          );
        }
        referenceInputOutRefs.push(outRef);
        break;
      }
      case "--fraud-category":
        fraudCategory = parseFraudCategory(rest[++index]);
        break;
      case "--block-slot":
        blockSlot = rest[++index];
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
      case "--deposit-inclusion":
        depositInclusionPath = rest[++index];
        break;
      case "--withdrawal-inclusion":
        withdrawalInclusionPath = rest[++index];
        break;
      case "--event-out-ref":
        eventOutRef = rest[++index];
        break;
      case "--authentic-content":
        authenticContentPath = rest[++index];
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
      case "--native-tx-compact":
        nativeTxCompactPath = rest[++index];
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
      case "--workflow-journal-dir":
        workflowJournalDir = rest[++index];
        break;
      case "--workflow-runtime-config":
        workflowRuntimeConfigPath = rest[++index];
        break;
      case "--deployment-fingerprint":
        deploymentFingerprint = rest[++index];
        break;
      case "--correction-journal":
        correctionJournalPath = rest[++index];
        break;
      case "--help":
      case "-h":
        console.log(usage);
        process.exit(0);
      // process.exit is typed never, but ESLint does not use that fact here.

      // eslint-disable-next-line no-fallthrough
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
    transactionsPath,
    sampleDoubleSpend,
    headerHash,
    expectedTransactionsRoot,
    tx1Id,
    tx2Id,
    outputDir,
    daPayloadEnvelopePath,
    transitionFaultProofPath,
    referenceInputOutRefs,
    awaitConfirmation,
    fraudCategory,
    blockSlot,
    txId,
    badTxId,
    badInputIndex,
    prevUtxosRoot,
    prevBlockPayloadPath,
    inputsPreimagePath,
    depositInclusionPath,
    withdrawalInclusionPath,
    eventOutRef,
    authenticContentPath,
    outputsPreimagePath,
    ledgerNonMembershipProofPath,
    txsNonMembershipProofPath,
    referenceInputsPreimagePath,
    badReferenceInputIndex,
    witnessSetCompactPath,
    nativeTxCompactPath,
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
    workflowJournalDir,
    workflowRuntimeConfigPath,
    deploymentFingerprint,
    correctionJournalPath,
  };
};
