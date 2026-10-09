import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, UTxO } from "@lucid-evolution/lucid";

import {
  type AuthenticatedValidator,
  type AvailabilityChallengeValidator,
  type MintingValidator,
  type SpendingValidator,
  type StateQueueValidator,
  type ValidationTraceDisputeValidators,
  type WithdrawalValidator,
} from "./common.utxos-at-by-nftpolicy-id.js";
import type { FaultProofContractChains } from "./fraud-proof/contracts/index.js";
import type { EventHistoryContractPair } from "./user-events/history-deployment.js";

export type FraudProofs = {
  doubleSpend: SpendingValidator;
  nonExistentInput: SpendingValidator;
  nonExistentInputNoIndex: SpendingValidator;
  invalidRange: SpendingValidator;
  transitionTrace: SpendingValidator;
  /**
   * V1 stateful dispute game for a transaction-validation trace.
   */
  validationTraceDispute: ValidationTraceDisputeValidators;
  zeroInput: SpendingValidator;
  /**
   * Q44 `da-hash-preimage`: a committed `transactions_root` leaf whose key is
   * not the canonical native-V1 transaction id of its own value.
   */
  daHashPreimage: SpendingValidator;
  /**
   * Q18 `no-reference-input`: a committed transaction reads an input that
   * never existed in the block's prev ledger and was not produced in-block.
   */
  noReferenceInput: SpendingValidator;
  /**
   * Q31 `reference-input-no-idx`: a committed transaction reads an output
   * index its in-block producing transaction never created.
   */
  referenceInputNoIdx: SpendingValidator;
  /**
   * Q15 `invalid-signature`: a committed transaction's address-witness set
   * does not authorize one of the inputs it spends.
   */
  invalidSignature: SpendingValidator;
  fabricatedDeposit: SpendingValidator;
  fabricatedWithdrawal: SpendingValidator;
  nativeScriptDecoding: SpendingValidator;
  missingSignature: SpendingValidator;
  withdrawnReferenceInput: SpendingValidator;
  canonicalDecodability: SpendingValidator;
  committedFieldShape: SpendingValidator;
  minFee: SpendingValidator;
  withdrawalMistag: SpendingValidator;
  doubleWithdraw: SpendingValidator;
  crossBlockDuplicateEvent: SpendingValidator;
  l2TxMistag: SpendingValidator;
  withdrawnInput: SpendingValidator;
  /**
   * `value-not-preserved`: a committed ACCEPTED transaction creates strictly
   * more of some asset than it consumes and mints.
   */
  valueNotPreserved: SpendingValidator;
  /**
   * `input-set-uniqueness`: a committed ACCEPTED transaction's intra-tx input
   * sets violate uniqueness/disjointness.
   */
  inputSetUniqueness: SpendingValidator;
  /**
   * `mint-authorization`: a committed ACCEPTED transaction mints or burns
   * under a policy that never authorized it.
   */
  mintAuthorization: SpendingValidator;
  /**
   * Q35 `network-id`: a committed accepted native transaction or one of its
   * protected output addresses targets a network other than the deployment.
   */
  networkId: SpendingValidator;
  /** Q34: an authenticated native script evaluates false in tx context. */
  nativeScriptInvalid: SpendingValidator;
  /** Q27: an accepted output or newly introduced UTxO is below min-Ada. */
  minAda: SpendingValidator;
  /** A committed field preimage disagrees with its authenticated byte length. */
  fieldPreimageLengthMismatch: SpendingValidator;
  /** A committed field item violates the field-specific width rule. */
  fieldItemWidthIllegal: SpendingValidator;
  /** A field-6 native-script witness fails canonical structural decoding. */
  witnessScriptDecoding: SpendingValidator;
  /** The script-integrity hash is absent despite an effectful script purpose. */
  scriptIntegrityHashMissing: SpendingValidator;
  /** A committed transaction output is not a canonical ledger-output encoding. */
  transactionOutputNonCanonical: SpendingValidator;
  mintItemNonCanonical: SpendingValidator;
  /** A spent input resolves to a non-canonical prior ledger output. */
  resolvedOutputNonCanonical: SpendingValidator;
  /** A mint policy declares more assets than the protocol limit. */
  mintDeclaredAssetLimit: SpendingValidator;
  /** A spent input is not authorized by any valid matching key witness. */
  spendInputSignerMissing: SpendingValidator;
  /** A protected output credential has no valid matching key witness. */
  protectedOutputSignerMissing: SpendingValidator;
  /** An untagged-network transaction carries a non-empty observer set. */
  observersForbiddenOnUntaggedNetwork: SpendingValidator;
  /** The observer field is not in canonical order. */
  observerOrderInvalid: SpendingValidator;
  /** A redeemer item is not canonically encoded Plutus Data. */
  redeemerCanonicity: SpendingValidator;
  /** A canonical output carries a malformed or over-limit reference script. */
  outputReferenceScriptDecoding: SpendingValidator;
  /** A selected native execution source is malformed or over structural limits. */
  executionSourceScriptDecoding: SpendingValidator;
  /** A selected, well-formed native execution source evaluates false. */
  executionNativeScriptInvalid: SpendingValidator;
  /** A receive purpose selects forbidden Plutus V3 execution. */
  receivePurposeLanguage: SpendingValidator;
  /** A committed witness script is not selected by any canonical purpose. */
  unusedScriptWitness: SpendingValidator;
  /** A canonical execution purpose has no matching script source. */
  missingScriptSource: SpendingValidator;
  /** A canonical Plutus purpose has no matching redeemer pointer. */
  missingRedeemer: SpendingValidator;
  /** A committed redeemer pointer is not selected by any execution purpose. */
  unusedRedeemer: SpendingValidator;
  /** The committed script-integrity hash differs from the canonical language views. */
  scriptIntegrityHashMismatch: SpendingValidator;
  /** ValueAndMint crosses the consensus distinct-asset accumulator limit. */
  distinctAssetAccumulationLimit: SpendingValidator;
};

export type MidgardValidators = {
  referenceScriptAuth: MintingValidator;
  hubOracle: AuthenticatedValidator;
  daParamsGovernor: AuthenticatedValidator;
  daAttestation: AuthenticatedValidator;
  /**
   * The pooled DA committee bond (the return type of
   * `buildDaBondPoolValidator`): one mint + spend script holding the pool NFT.
   * Apply reads it as a reference input; Timeout slashes it.
   */
  daBondPool: AuthenticatedValidator;
  /** Commitment-bound availability challenge, tranche and carrier authority. */
  availabilityChallenge: AvailabilityChallengeValidator;
  /** Deployment-bound singleton which serializes state correction. */
  correctionLock: SpendingValidator;
  stateQueue: StateQueueValidator;
  scheduler: AuthenticatedValidator;
  registeredOperators: AuthenticatedValidator;
  activeOperators: AuthenticatedValidator;
  retiredOperators: AuthenticatedValidator;
  escapeHatch: AuthenticatedValidator;
  fraudProofCatalogue: AuthenticatedValidator;
  /** Canonical computation-thread policy shared by every fraud-proof family. */
  computationThread: MintingValidator;
  fraudProof: AuthenticatedValidator;
  /** Shared published verifier for staged MPF proof chunks. */
  chunkedVerify: WithdrawalValidator;
  /** Shared published verifier for direct MPF non-membership claims. */
  pexcludes: WithdrawalValidator;
  /** Null only in the explicit always-succeeds scaffold, which cannot bootstrap history. */
  eventHistory: EventHistoryContractPair | null;
  deposit: AuthenticatedValidator;
  withdrawal: AuthenticatedValidator;
  txOrder: AuthenticatedValidator;
  /**
   * The §8.6 field-preimage certificate. #594 retired the receipt family and
   * replaced it with this policy: the tx-order mint and every field-opening
   * step take its policy id as a parameter, and the field-access door checks
   * tier-3 chunk digests against a certificate minted under it. #579 gave it
   * a deployment role so that one deployed policy serves every reader rather
   * than each load site rederiving it. The validator takes no parameters, so
   * its policy id is a pure function of the blueprint.
   */
  fieldPreimageCertificate: SpendingValidator & MintingValidator;
  /**
   * Permissionless append-only L1 availability for content-addressed V1
   * CEK material. Its validator has no successful spending path.
   */
  cekProgramMaterial: SpendingValidator;
  settlement: AuthenticatedValidator;
  reserve: SpendingValidator & WithdrawalValidator;
  payout: AuthenticatedValidator;
  /** Full ordered validator chains for every registered fraud category. */
  fraudProofContracts: FaultProofContractChains;
  /** First-step validators only, used to construct the catalogue MPF. */
  fraudProofs: FraudProofs;
};

export const OutputReferenceSchema = Data.Object({
  transactionId: Data.Bytes({ minLength: 32, maxLength: 32 }),
  outputIndex: Data.Integer(),
});

export type OutputReference = Data.Static<typeof OutputReferenceSchema>;

export const OutputReference = asDataType<OutputReference>(
  OutputReferenceSchema,
);

export const outputReferenceFromUTxO = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): OutputReference => ({
  transactionId: utxo.txHash,
  outputIndex: BigInt(utxo.outputIndex),
});

export const AssetsSchema = Data.Object({
  policyId: Data.Bytes(),
  assetName: Data.Bytes(),
});

export type Assets = Data.Static<typeof AssetsSchema>;

export const Assets = asDataType<Assets>(AssetsSchema);

export const ValueSchema = Data.Map(
  Data.Bytes(),
  Data.Map(Data.Bytes(), Data.Integer()),
);

export type Value = Data.Static<typeof ValueSchema>;

export const Value = asDataType<Value>(ValueSchema);

export const POSIXTimeSchema = Data.Integer();

export type POSIXTime = Data.Static<typeof POSIXTimeSchema>;

export const POSIXTime = asDataType<POSIXTime>(POSIXTimeSchema);

export const PosixTimeDurationSchema = Data.Integer();

export type PosixTimeDuration = Data.Static<typeof PosixTimeDurationSchema>;

export const PosixTimeDuration = asDataType<PosixTimeDuration>(
  PosixTimeDurationSchema,
);

export const VerificationKeyHashSchema = Data.Bytes({
  minLength: 28,
  maxLength: 28,
});

export const PubKeyHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });

export const ScriptHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });

export const MerkleRootSchema = Data.Bytes({ minLength: 32, maxLength: 32 });

export type MerkleRoot = Data.Static<typeof MerkleRootSchema>;

export const MerkleRoot = asDataType<MerkleRoot>(MerkleRootSchema);

export const CredentialSchema = Data.Enum([
  Data.Object({
    PublicKeyCredential: Data.Tuple([PubKeyHashSchema]),
  }),
  Data.Object({
    ScriptCredential: Data.Tuple([ScriptHashSchema]),
  }),
]);

export type CredentialD = Data.Static<typeof CredentialSchema>;

export const CredentialD = asDataType<CredentialD>(CredentialSchema);

export const AddressSchema = Data.Object({
  paymentCredential: CredentialSchema,
  stakeCredential: Data.Nullable(
    Data.Enum([
      Data.Object({ Inline: Data.Tuple([CredentialSchema]) }),
      Data.Object({
        Pointer: Data.Tuple([
          Data.Object({
            slotNumber: Data.Integer(),
            transactionIndex: Data.Integer(),
            certificateIndex: Data.Integer(),
          }),
        ]),
      }),
    ]),
  ),
});

export type AddressData = Data.Static<typeof AddressSchema>;

export const AddressData = asDataType<AddressData>(AddressSchema);

export const NeighborSchema = Data.Object({
  nibble: Data.Integer(),
  prefix: Data.Bytes(),
  root: Data.Bytes(),
});

export type Neighbor = Data.Static<typeof NeighborSchema>;

export const Neighbor = asDataType<Neighbor>(NeighborSchema);

export const ProofStepSchema = Data.Enum([
  Data.Object({
    Branch: Data.Object({
      skip: Data.Integer(),
      neighbors: Data.Bytes(),
    }),
  }),
  Data.Object({
    Fork: Data.Object({
      skip: Data.Integer(),
      neighbor: NeighborSchema,
    }),
  }),
  Data.Object({
    Leaf: Data.Object({
      skip: Data.Integer(),
      key: Data.Bytes(),
      value: Data.Bytes(),
    }),
  }),
]);

export type ProofStep = Data.Static<typeof ProofStepSchema>;

export const ProofStep = asDataType<ProofStep>(ProofStepSchema);

export const ProofSchema = Data.Array(ProofStepSchema);

export type Proof = Data.Static<typeof ProofSchema>;

export const Proof = asDataType<Proof>(ProofSchema);
