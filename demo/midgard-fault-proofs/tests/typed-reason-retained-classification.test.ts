import {
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengths,
  decodeMidgardVersionedScript,
  encodeCbor,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  EventKeySchema,
  type FraudProofCatalogueCategoryName,
  GENESIS_HEADER_HASH,
  L2TransactionSource,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { computeTransitionTraceL1EventEvidenceDigest } from "../src/transition-trace/replay-authority.js";
import {
  admitValidationTraceChallengeFromReplayContext,
  readValidationTraceReplaySelection,
} from "../src/validation-dispute/replay.js";
import {
  requireValidationTraceChallenge,
  W25_CHALLENGE_COORDINATE,
} from "../src/workflow/challenge-authority.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  admitValidationTraceReplayContext,
  type CompleteCanonicalReplay,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY,
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
  INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  MIN_FEE_COMPLETE_CANONICAL_REPLAY,
  MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
  VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import type { TypedReasonArm } from "../src/workflow/reason-disposition.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type FixtureTransactionInput,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { buildWidthForcedFixture } from "./support/field-item-width-illegal-shapes.js";
import {
  honestAddressWitness,
  invalidAddressWitness,
} from "./support/invalid-signature-emulator.js";
import { buildMissingRedeemerFixture } from "./support/missing-redeemer-emulator.js";
import { buildMissingScriptSourceFixture } from "./support/missing-script-source-emulator.js";
import {
  outputWithNativeScript,
  outputWithRawNativePayload,
} from "./support/output-reference-script-decoding-emulator.js";
import {
  buildRetainedPlutusIdentityFixture,
  buildRetainedPlutusUnboundVariableFixture,
  buildRetainedValidationBlockFixture,
  captureRetainedPlutusIdentityOrigins,
  classifyRetainedReasonFixture,
  retainRejectedValidationTrace,
} from "./support/retained-reason-classifier.js";
import { buildUnusedScriptWitnessFixture } from "./support/unused-script-witness-emulator.js";

const deploymentFingerprint = "d1".repeat(32);
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinalityAuthority = {
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  }),
};
const base: FixtureTransactionInput = {
  spendInputs: [outRefCbor(71, 0n)],
  fee: 7n,
  networkId: 0n,
};
type ReasonCase = {
  readonly arm: TypedReasonArm;
  readonly category: FraudProofCatalogueCategoryName;
  readonly replayer: CompleteCanonicalReplay;
  readonly reason: RejectionReason;
  readonly accepted: Partial<FixtureTransactionInput>;
  readonly wrongful: Partial<FixtureTransactionInput>;
  readonly mismatchFieldIndex?: number;
  readonly minFeeB?: bigint;
  readonly acceptedWitness?: "valid" | "invalid";
  readonly wrongfulWitness?: "valid" | "invalid";
  readonly priorLedger?: true;
  readonly priorOutput?: Buffer;
  readonly acceptedApplicability?:
    | "unreachable_under_consensus_bounds"
    | "not_exercised";
};
const canonicalRedeemer = encodeMidgardRedeemerWitnessItem({
  purpose: "Spend",
  index: 0n,
  redeemerCbor: Buffer.from("00", "hex"),
  executionUnits: { memory: 1n, steps: 2n },
});
const nativeTrueScript = Buffer.from("820043820180", "hex");
const nativeFalseScript = Buffer.from("820043820280", "hex");
const nativeMint = (
  scriptWitness: Buffer,
): Partial<FixtureTransactionInput> => ({
  scriptWitnesses: [scriptWitness],
  mintPolicyItems: [
    encodeCbor([
      Buffer.from(
        hashMidgardVersionedScript(decodeMidgardVersionedScript(scriptWitness)),
        "hex",
      ),
      new Map([[Buffer.alloc(0), 1n]]),
    ]),
  ],
});
const nativeSignatureScript = Buffer.concat([
  Buffer.from("820058208200581c", "hex"),
  Buffer.alloc(28, 0x99),
]);
const witnessSeed = Buffer.alloc(32);
witnessSeed.writeUInt32BE(1, 28);
const signerHash = Buffer.from(
  CML.PrivateKey.from_normal_bytes(witnessSeed)
    .to_public()
    .hash()
    .to_raw_bytes(),
);
const output = (lovelace = 2_000_000n, protectedOwner = false) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([protectedOwner ? 0x68 : 0x60]),
      signerHash,
    ]),
    value: { lovelace, assets: new Map() },
  });
const cases: readonly ReasonCase[] = [
  {
    arm: "ValueNotPreserved",
    category: "valueNotPreserved",
    replayer: VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
    reason: "ValueNotPreserved",
    accepted: {},
    wrongful: { outputs: [output(1_999_993n)] },
    priorLedger: true,
  },
  {
    arm: "ExecutionNativeScriptFalse",
    category: "executionNativeScriptInvalid",
    replayer: EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
    reason: { ExecutionNativeScriptFalse: { execution_index: 0n } },
    accepted: nativeMint(nativeFalseScript),
    wrongful: nativeMint(nativeTrueScript),
    priorLedger: true,
  },
  {
    arm: "SpendInputSignerMissing",
    category: "spendInputSignerMissing",
    replayer: SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
    reason: { SpendInputSignerMissing: { input_index: 0n } },
    accepted: {},
    wrongful: {},
    wrongfulWitness: "valid",
    priorLedger: true,
  },
  {
    arm: "InputSpentOutputNonCanonical",
    category: "resolvedOutputNonCanonical",
    replayer: RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
    reason: {
      InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 0n },
    },
    accepted: {},
    wrongful: {},
    priorLedger: true,
    acceptedApplicability: "not_exercised",
  },
  ...(
    [
      "ResolvedReferenceScriptMalformed",
      "ResolvedReferenceScriptNodeLimit",
      "ResolvedReferenceScriptDepthLimit",
    ] as const
  ).map(
    (arm): ReasonCase => ({
      arm,
      category: "nativeScriptDecoding",
      replayer: NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      reason: {
        [arm]: { source_kind: 0n, input_index: 0n },
      } as RejectionReason,
      accepted: {},
      wrongful: {},
      priorLedger: true,
      priorOutput: outputWithNativeScript({ type: "all", scripts: [] }),
      acceptedApplicability: "not_exercised",
    }),
  ),
  {
    arm: "InputNotFound",
    category: "nonExistentInput",
    replayer: NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
    reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
    accepted: { spendInputs: [outRefCbor(72, 0n)] },
    wrongful: {},
    priorLedger: true,
  },
  {
    arm: "InputNotFound",
    category: "noReferenceInput",
    replayer: NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
    reason: { InputNotFound: { source_kind: 1n, input_index: 0n } },
    accepted: { referenceInputs: [outRefCbor(72, 0n)] },
    wrongful: { referenceInputs: [outRefCbor(71, 0n)] },
    priorLedger: true,
  },
  {
    arm: "WitnessNativeScriptNodeLimit",
    category: "witnessScriptDecoding",
    replayer: WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { WitnessNativeScriptNodeLimit: { script_index: 0n } },
    accepted: {},
    wrongful: { scriptWitnesses: [nativeTrueScript] },
    acceptedApplicability: "unreachable_under_consensus_bounds",
  },
  {
    arm: "WitnessNativeScriptDepthLimit",
    category: "witnessScriptDecoding",
    replayer: WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { WitnessNativeScriptDepthLimit: { script_index: 0n } },
    accepted: {},
    wrongful: { scriptWitnesses: [nativeTrueScript] },
    acceptedApplicability: "unreachable_under_consensus_bounds",
  },
  {
    arm: "OutputReferenceScriptNodeLimit",
    category: "outputReferenceScriptDecoding",
    replayer: OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { OutputReferenceScriptNodeLimit: { output_index: 0n } },
    accepted: {},
    wrongful: {
      outputs: [outputWithNativeScript({ type: "all", scripts: [] })],
    },
    acceptedApplicability: "unreachable_under_consensus_bounds",
  },
  {
    arm: "OutputReferenceScriptDepthLimit",
    category: "outputReferenceScriptDecoding",
    replayer: OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { OutputReferenceScriptDepthLimit: { output_index: 0n } },
    accepted: {},
    wrongful: {
      outputs: [outputWithNativeScript({ type: "all", scripts: [] })],
    },
    acceptedApplicability: "unreachable_under_consensus_bounds",
  },
  {
    arm: "AddressWitnessSignatureInvalid",
    category: "invalidSignature",
    replayer: INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
    reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
    accepted: {},
    wrongful: {},
    acceptedWitness: "invalid",
    wrongfulWitness: "valid",
  },
  {
    arm: "RequiredSignerUnsigned",
    category: "missingSignature",
    replayer: MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
    reason: { RequiredSignerUnsigned: { signer_index: 0n } },
    accepted: { requiredSigners: [signerHash] },
    wrongful: { requiredSigners: [signerHash] },
    wrongfulWitness: "valid",
  },
  {
    arm: "OutputNonCanonical",
    category: "transactionOutputNonCanonical",
    replayer: TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
    reason: { OutputNonCanonical: { output_index: 0n } },
    accepted: { outputs: [Buffer.from("a0", "hex")] },
    wrongful: { outputs: [output()] },
  },
  {
    arm: "ProtectedOutputSignerMissing",
    category: "protectedOutputSignerMissing",
    replayer: PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
    reason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    accepted: { outputs: [output(2_000_000n, true)] },
    wrongful: { outputs: [output(2_000_000n, true)] },
    wrongfulWitness: "valid",
  },
  {
    arm: "MintDeclaredAssetLimit",
    category: "mintDeclaredAssetLimit",
    replayer: MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
    reason: { MintDeclaredAssetLimit: { policy_index: 0n } },
    accepted: {
      mintPolicyItems: [
        Buffer.concat([
          Buffer.from("82581c", "hex"),
          Buffer.alloc(28, 0x31),
          Buffer.from("b9400100", "hex"),
        ]),
      ],
    },
    wrongful: {
      mintPolicyItems: [
        encodeCbor([Buffer.alloc(28, 0x31), new Map([[Buffer.alloc(0), 1n]])]),
      ],
    },
  },
  {
    arm: "OutputReferenceScriptMalformed",
    category: "outputReferenceScriptDecoding",
    replayer: OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { OutputReferenceScriptMalformed: { output_index: 0n } },
    accepted: {
      outputs: [outputWithRawNativePayload(Buffer.from("820700", "hex"))],
    },
    wrongful: {
      outputs: [outputWithNativeScript({ type: "all", scripts: [] })],
    },
  },
  {
    arm: "OutputBelowMinAda",
    category: "minAda",
    replayer: MIN_ADA_COMPLETE_CANONICAL_REPLAY,
    reason: { OutputBelowMinAda: { output_index: 0n } },
    accepted: { outputs: [output(1n)] },
    wrongful: { outputs: [output(2_000_000n)] },
  },
  {
    arm: "FieldPreimageLengthMismatch",
    category: "fieldPreimageLengthMismatch",
    replayer: FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
    reason: { FieldPreimageLengthMismatch: { field_index: 0n } },
    accepted: {},
    wrongful: {},
    mismatchFieldIndex: 0,
  },
  {
    arm: "FeeBelowMinimum",
    category: "minFee",
    replayer: MIN_FEE_COMPLETE_CANONICAL_REPLAY,
    reason: "FeeBelowMinimum",
    accepted: {},
    wrongful: {},
    minFeeB: 8n,
  },
  {
    arm: "WitnessScriptHeaderMalformed",
    category: "witnessScriptDecoding",
    replayer: WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { WitnessScriptHeaderMalformed: { script_index: 0n } },
    accepted: { scriptWitnesses: [Buffer.from("8201410a", "hex")] },
    wrongful: { scriptWitnesses: [nativeTrueScript] },
  },
  {
    arm: "WitnessNativeScriptMalformed",
    category: "witnessScriptDecoding",
    replayer: WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { WitnessNativeScriptMalformed: { script_index: 0n } },
    accepted: { scriptWitnesses: [Buffer.from("820043820700", "hex")] },
    wrongful: { scriptWitnesses: [nativeTrueScript] },
  },
  {
    arm: "WitnessNativeScriptFalse",
    category: "nativeScriptInvalid",
    replayer: NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
    reason: { WitnessNativeScriptFalse: { script_index: 0n } },
    accepted: { scriptWitnesses: [nativeSignatureScript] },
    wrongful: { scriptWitnesses: [nativeTrueScript] },
  },
  {
    arm: "ScriptIntegrityHashMissing",
    category: "scriptIntegrityHashMissing",
    replayer: SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
    reason: "ScriptIntegrityHashMissing",
    accepted: { scriptWitnesses: [Buffer.from("82034401020304", "hex")] },
    wrongful: {
      scriptWitnesses: [Buffer.from("82034401020304", "hex")],
      scriptIntegrityHash: Buffer.alloc(32, 1),
    },
  },
  {
    arm: "RedeemerMalformed",
    category: "redeemerCanonicity",
    replayer: REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
    reason: { RedeemerMalformed: { redeemer_index: 0n } },
    accepted: { redeemerWitnesses: [Buffer.from("ff", "hex")] },
    wrongful: { redeemerWitnesses: [canonicalRedeemer] },
  },
  {
    arm: "FieldItemWidthIllegal",
    category: "fieldItemWidthIllegal",
    replayer: FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
    reason: { FieldItemWidthIllegal: { field_index: 5n, item_index: 0n } },
    accepted: { mintPolicyItems: [Buffer.alloc(0)] },
    wrongful: { mintPolicyItems: [Buffer.alloc(29, 0xe6)] },
  },
  {
    arm: "EmptyInputs",
    category: "zeroInput",
    replayer: ZERO_INPUT_COMPLETE_CANONICAL_REPLAY,
    reason: "EmptyInputs",
    accepted: { spendInputs: [] },
    wrongful: {},
  },
  {
    arm: "DuplicateInput",
    category: "inputSetUniqueness",
    replayer: INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY,
    reason: {
      DuplicateInput: {
        first_field_index: 0n,
        first_item_index: 0n,
        second_field_index: 0n,
        second_item_index: 1n,
      },
    },
    accepted: { spendInputs: [outRefCbor(71, 0n), outRefCbor(71, 0n)] },
    wrongful: { spendInputs: [outRefCbor(71, 0n), outRefCbor(72, 0n)] },
  },
  {
    arm: "ValidityIntervalMalformed",
    category: "invalidRange",
    replayer: INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
    reason: "ValidityIntervalMalformed",
    accepted: { validityIntervalStart: 30n, validityIntervalEnd: 20n },
    wrongful: {},
  },
  {
    arm: "ValidityIntervalExcludesBlockSlot",
    category: "invalidRange",
    replayer: INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
    reason: "ValidityIntervalExcludesBlockSlot",
    accepted: { validityIntervalStart: 30n, validityIntervalEnd: 40n },
    wrongful: {},
  },
  {
    arm: "NetworkIdMismatch",
    category: "networkId",
    replayer: NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
    reason: "NetworkIdMismatch",
    accepted: { networkId: 1n },
    wrongful: {},
  },
  {
    arm: "ObserversForbiddenOnUntaggedNetwork",
    category: "observersForbiddenOnUntaggedNetwork",
    replayer: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
    reason: "ObserversForbiddenOnUntaggedNetwork",
    accepted: {
      scriptIntegrityHash: Buffer.alloc(32, 1),
      networkId: 255n,
      requiredObservers: [Buffer.alloc(28, 1)],
    },
    wrongful: { requiredObservers: [Buffer.alloc(28, 1)] },
  },
  {
    arm: "ObserverOrderInvalid",
    category: "observerOrderInvalid",
    replayer: OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY,
    reason: { ObserverOrderInvalid: { observer_index: 1n } },
    accepted: { requiredObservers: [Buffer.alloc(28, 2), Buffer.alloc(28, 1)] },
    wrongful: { requiredObservers: [Buffer.alloc(28, 1), Buffer.alloc(28, 2)] },
  },
];

const ordinaryMachineCases = [
  {
    arm: "ExecutionNativeScriptMalformed",
    category: "executionSourceScriptDecoding",
    replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { ExecutionNativeScriptMalformed: { execution_index: 0n } },
  },
  {
    arm: "ExecutionNativeScriptNodeLimit",
    category: "executionSourceScriptDecoding",
    replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { ExecutionNativeScriptNodeLimit: { execution_index: 0n } },
  },
  {
    arm: "ExecutionNativeScriptDepthLimit",
    category: "executionSourceScriptDecoding",
    replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { ExecutionNativeScriptDepthLimit: { execution_index: 0n } },
  },
  {
    arm: "ScriptIntegrityHashMismatch",
    category: "scriptIntegrityHashMismatch",
    replayer: SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
    reason: "ScriptIntegrityHashMismatch",
  },
  {
    arm: "ReceivePurposePlutusV3Forbidden",
    category: "receivePurposeLanguage",
    replayer: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
    reason: { ReceivePurposePlutusV3Forbidden: { execution_index: 0n } },
  },
  {
    arm: "OutputAssetAccumulationLimit",
    category: "distinctAssetAccumulationLimit",
    replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    reason: {
      OutputAssetAccumulationLimit: { output_index: 1n, asset_index: 0n },
    },
  },
  {
    arm: "MintAssetAccumulationLimit",
    category: "distinctAssetAccumulationLimit",
    replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    reason: { MintAssetAccumulationLimit: { mint_index: 0n } },
  },
] as const satisfies readonly Pick<
  ReasonCase,
  "arm" | "category" | "replayer" | "reason"
>[];

describe("typed reasons classified from committed retained DA", () => {
  it("InputAssetAccumulationLimit: wrongful operator verdict with one input asset", async () => {
    const assetOutput = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), signerHash]),
      value: {
        lovelace: 2_000_000n,
        assets: new Map([["a1".repeat(28), new Map([["01", 1n]])]]),
      },
    });
    const input = outRefCbor(71, 0n);
    const unsigned = buildFixtureTransaction({
      spendInputs: [input],
      outputs: [assetOutput],
      fee: 0n,
    });
    const transaction = buildFixtureTransaction({
      spendInputs: [input],
      outputs: [assetOutput],
      fee: 0n,
      addressWitnesses: [
        honestAddressWitness({ index: 0, txId: unsigned.txId }),
      ],
    });
    const operations = [
      { type: "delete" as const, key: input },
      buildValidationMachineLedgerInsertOp({
        key: encodeMidgardSpendInputItem({
          txId: Buffer.from(transaction.txId, "hex"),
          outputIndex: 0,
        }),
        outputCbor: assetOutput,
      }),
    ];
    const ledgerWitnessEntries = [{ outRef: input, output: assetOutput }];
    const mutations = await buildValidationMachineLedgerMutationSteps({
      initialEntries: ledgerWitnessEntries,
      operations,
    });
    const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };
    const eventKey = {
      ForcedTransactionEventKey: { tx_order_id: orderKey },
    } as const;
    const reason = {
      InputAssetAccumulationLimit: { input_index: 0n, asset_index: 0n },
    } as const;
    const priorLedgerRoot = mutations[0]!.preRoot.toString("hex");
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        eventKeyCbor: Buffer.from(
          Data.to(eventKey, asLucidSchema(EventKeySchema)),
          "hex",
        ),
        sourceKind: "forced",

        blockEndTimeMs: 1_800_000_000_000,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactionId: Buffer.from(transaction.txId, "hex"),
        canonicalTransactionCbor: forcedTraceBytes(
          forcedTraceView(
            decodeMidgardNativeTxFullFromCanonicalCbor(
              transaction.canonicalCbor,
            ),
          ),
        ),
        priorUtxosRoot: priorLedgerRoot,
        postUtxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries,
        expectedLedgerOps: operations,
        ledgerMutationSteps: mutations,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );
    const fixture = await buildRetainedValidationBlockFixture({
      subject: {
        kind: "forced",
        nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
          transaction.canonicalCbor,
        ),
        orderKey,
        verdict: { ForcedTxInvalid: { reason } },
      },
      priorLedgerRoot,
      ...retainRejectedValidationTrace({ trace, eventKey, reason }),
      blockEndTimeMs: 1_800_000_000_000,
    });
    const { decision } = await classifyRetainedReasonFixture({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      deploymentFingerprint,
      releaseFinalityAuthority,
      replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    });
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "distinctAssetAccumulationLimit",
      headerHash: fixture.headerHash,
    });
  });
  for (const scenario of ordinaryMachineCases) {
    it(`${scenario.arm}: wrongful operator verdict from an ordinary complete machine trace`, async () => {
      const retained = await buildUnusedScriptWitnessFixture({
        direction: "forced",
        claimedVerdict: "rejected",
        accusedUnused: false,
        sourceCount: 4,
        allPurposeKinds: true,
        inputByte: 0x71,
        operatorVkey: "b1".repeat(28),
        startTime: 1_749_999_941_000n,
      });
      let reason: RejectionReason = scenario.reason;
      if (scenario.arm === "ReceivePurposePlutusV3Forbidden") {
        const receive = retained.trace.witnesses.find(
          ({ auxiliary }) =>
            auxiliary?.kind === "nativeExecutionDescriptor" &&
            auxiliary.purpose.purposeKind === 3,
        )?.auxiliary;
        if (receive?.kind !== "nativeExecutionDescriptor")
          throw new Error(
            "ordinary fixture omitted its native receive execution",
          );
        reason = {
          ReceivePurposePlutusV3Forbidden: {
            execution_index: BigInt(receive.executionIndex),
          },
        };
      }
      const fixture = await buildRetainedValidationBlockFixture({
        subject: {
          kind: "forced",
          nativeTx: retained.transaction.tx,
          orderKey: retained.orderKey,
          verdict: { ForcedTxInvalid: { reason } },
        },
        priorLedgerRoot:
          retained.block.reconstruction.traceByStepIndex.get(0n)!.value
            .pre_utxos_root,
        ...retainRejectedValidationTrace({
          trace: retained.trace,
          eventKey: retained.eventKey,
          reason,
        }),
        blockEndTimeMs: 1_750_000_001_000,
        blockSlot: 0n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: scenario.replayer,
      });
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: scenario.category,
        headerHash: fixture.headerHash,
      });
    });
  }
  it.each(["accepted", "wrongful", "honest"] as const)(
    "UnusedScriptWitness: %s operator verdict under unusedScriptWitness",
    async (direction) => {
      const retained = await buildUnusedScriptWitnessFixture({
        direction: direction === "accepted" ? "accepted" : "forced",
        claimedVerdict: direction === "accepted" ? "accepted" : "rejected",
        accusedUnused: direction !== "wrongful",
        sourceCount: 2,
        inputByte: 0x71,
        operatorVkey: "b1".repeat(28),
        startTime: 1_749_999_941_000n,
      });
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: retained.transaction.tx }
            : {
                kind: "forced",
                nativeTx: retained.transaction.tx,
                orderKey: retained.orderKey,
                verdict: {
                  ForcedTxInvalid: {
                    reason: {
                      UnusedScriptWitness: {
                        script_index: BigInt(retained.scriptIndex),
                      },
                    },
                  },
                },
              },
        priorLedgerRoot:
          retained.block.reconstruction.traceByStepIndex.get(0n)!.value
            .pre_utxos_root,
        descriptorEntries:
          retained.retainedEntries.authenticatedValidationTraceEntries,
        retainedEntries:
          retained.retainedEntries.retainedValidationWitnessEntries,
        blockEndTimeMs: 1_750_000_001_000,
        blockSlot: 0n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "unusedScriptWitness",
              headerHash: fixture.headerHash,
            },
      );
    },
  );
  it.each(["accepted", "wrongful", "honest"] as const)(
    "RedeemerMissing: %s operator verdict under missingRedeemer",
    async (direction) => {
      const retained = await buildMissingRedeemerFixture({
        direction: direction === "accepted" ? "accepted" : "forced",
        purposeKind: 0,
        sourceLocation: "inline",
        targetRedeemerPresent: direction === "wrongful",
      });
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: retained.transaction.tx }
            : {
                kind: "forced",
                nativeTx: retained.transaction.tx,
                orderKey: retained.orderKey,
                verdict: {
                  ForcedTxInvalid: { reason: retained.rejectionReason },
                },
              },
        priorLedgerRoot: "00".repeat(32),
        descriptorEntries: retained.descriptorEntries,
        retainedEntries: retained.retainedEntries,
        blockEndTimeMs: 1_800_000_000_000,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "missingRedeemer",
              headerHash: fixture.headerHash,
            },
      );
    },
  );
  it.each(["accepted", "wrongful", "honest"] as const)(
    "ScriptSourceMissing: %s operator verdict under missingScriptSource",
    async (direction) => {
      const retained = await buildMissingScriptSourceFixture({
        direction: direction === "accepted" ? "accepted" : "forced",
        purposeKind: 0,
        presentAt: direction === "wrongful" ? "inline" : "absent",
        inlineDecoys: direction === "wrongful" ? 1 : 0,
        referenceDecoys: 0,
      });
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: retained.transaction.tx }
            : {
                kind: "forced",
                nativeTx: retained.transaction.tx,
                orderKey: retained.orderKey,
                verdict: { ForcedTxInvalid: { reason: retained.reason } },
              },
        priorLedgerRoot: retained.priorLedgerRoot,
        descriptorEntries: retained.descriptorEntries,
        retainedEntries: retained.retainedEntries,
        blockEndTimeMs: 1_750_000_000_000,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "missingScriptSource",
              headerHash: fixture.headerHash,
            },
      );
    },
  );
  it("proves admitted script bytes cannot reach the node/depth rejection limits", () => {
    // Every native-script node uses at least three CBOR bytes; depth also
    // requires at least one distinct node per level, before outer wrappers.
    for (const scanLimit of [
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
    ]) {
      expect((scanLimit + 1) * 3).toBeGreaterThan(
        MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes,
      );
      expect((scanLimit + 1) * 3).toBeGreaterThan(
        MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
      );
    }
  });
  for (const scenario of cases) {
    it.each(
      scenario.acceptedApplicability !== undefined
        ? (["wrongful"] as const)
        : scenario.mismatchFieldIndex !== undefined ||
            scenario.minFeeB !== undefined
          ? (["accepted", "wrongful"] as const)
          : (["accepted", "wrongful", "honest"] as const),
    )(
      `${scenario.arm}: %s operator verdict under ${scenario.category}`,
      async (direction) => {
        let transaction = buildFixtureTransaction({
          ...base,
          ...scenario[direction === "wrongful" ? "wrongful" : "accepted"],
        });
        const witness =
          direction === "wrongful"
            ? scenario.wrongfulWitness
            : scenario.acceptedWitness;
        if (witness !== undefined)
          transaction = buildFixtureTransaction({
            ...base,
            ...scenario[direction === "wrongful" ? "wrongful" : "accepted"],
            addressWitnesses: [
              witness === "valid"
                ? honestAddressWitness({ index: 0, txId: transaction.txId })
                : invalidAddressWitness(0),
            ],
          });
        if (
          direction === "accepted" &&
          scenario.mismatchFieldIndex !== undefined
        ) {
          const lengths = [
            ...decodeMidgardNativeTxProofFieldLengths(
              Buffer.from(
                transaction.source.source.field_preimage_lengths_cbor,
                "hex",
              ),
            ),
          ];
          lengths[scenario.mismatchFieldIndex] =
            lengths[scenario.mismatchFieldIndex]! + 1;
          const source = {
            ...transaction.source,
            source: {
              ...transaction.source.source,
              field_preimage_lengths_cbor:
                encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
            },
          };
          transaction = {
            ...transaction,
            source,
            sourceValueBytes: Buffer.from(
              Data.to(source, L2TransactionSource),
              "hex",
            ),
          };
        }
        const predecessor = scenario.priorLedger
          ? await buildCanonicalBlockFixture({
              transactions: [],
              prevHeaderHash: GENESIS_HEADER_HASH,
              utxos: [
                {
                  key: outRefCbor(71, 0n),
                  value: scenario.priorOutput ?? output(),
                },
              ],
            })
          : undefined;
        const fixture =
          direction === "accepted" && scenario.arm === "ValueNotPreserved"
            ? await buildRetainedValidationBlockFixture({
                subject: {
                  kind: "normal",
                  nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
                    transaction.canonicalCbor,
                  ),
                },
                priorLedgerRoot: predecessor!.header.utxosRoot,
                prevHeaderHash: predecessor!.headerHash,
                blockEndTimeMs: 1_900_000_000_000,
                blockSlot: 0n,
              })
            : direction === "accepted"
              ? await buildCanonicalBlockFixture({
                  transactions: [transaction],
                  minFeeB: scenario.minFeeB,
                  prevHeaderHash: predecessor?.headerHash,
                  prevUtxosRoot: predecessor?.header.utxosRoot,
                })
              : await buildWidthForcedFixture({
                  operatorVkey: "b1".repeat(28),
                  now: 1_900_000_000_000,
                  nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
                    transaction.canonicalCbor,
                  ),
                  rejectionReason: scenario.reason,
                  finalOutputCbor: output(),
                  priorUtxosRoot: predecessor?.header.utxosRoot,
                  prevHeaderHash: predecessor?.headerHash,
                });
        const { decision } = await classifyRetainedReasonFixture({
          observation: authenticatedHeaderObservation(fixture),
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          deploymentFingerprint,
          releaseFinalityAuthority,
          replayer: scenario.replayer,
          ...(predecessor === undefined
            ? {}
            : {
                predecessor: {
                  observation: authenticatedHeaderObservation(predecessor),
                  payloadEnvelopeCbor: predecessor.payloadEnvelopeCbor,
                },
              }),
        });
        expect(decision).toMatchObject(
          direction === "honest"
            ? { decision: "healthy", headerHash: fixture.headerHash }
            : {
                decision: "fault_detected",
                category: scenario.category,
                headerHash: fixture.headerHash,
              },
        );
        expect(decision.decisionDigest).toMatch(/^[0-9a-f]{64}$/u);
      },
    );
  }
});

const admitRetainedPlutus = async (
  claim: Parameters<typeof buildRetainedPlutusIdentityFixture>[0],
  options: Readonly<{
    sourceKind?: "normal" | "forced";
    program?: "identity" | "unboundVariable";
    omitEvent?: boolean;
    omitOriginAuthority?: boolean;
  }> = {},
) => {
  const fixture =
    options.program === "unboundVariable"
      ? await buildRetainedPlutusUnboundVariableFixture(claim)
      : await buildRetainedPlutusIdentityFixture(claim, options);
  const observation = authenticatedHeaderObservation(fixture.block);
  const daProvenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "retained-fixture/emulator",
    grade: "security",
  } as const;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
    daProvenance,
    minimumConfirmationDepth: policy.confirmationDepth,
  });
  const predecessor = await admitCompleteCanonicalReplayPredecessor({
    value: {
      observation: authenticatedHeaderObservation(fixture.predecessor),
      payloadEnvelopeCborHex:
        fixture.predecessor.payloadEnvelopeCbor.toString("hex"),
      daProvenance,
    },
    currentEvidence: evidence,
    minimumConfirmationDepth: policy.confirmationDepth,
  });
  const transitionTraceEvents =
    options.sourceKind === "forced" && !options.omitOriginAuthority
      ? await captureRetainedPlutusIdentityOrigins(fixture, options)
      : undefined;
  const validationTraceReplay = await admitValidationTraceReplayContext({
    evidence,
    predecessor,
    transitionTraceEvents,
  });
  return {
    fixture,
    evidence,
    observation,
    context: { predecessor, validationTraceReplay, transitionTraceEvents },
  };
};

describe("ordinary retained Plutus replay admission", () => {
  it("admits an empty first block against the protocol genesis ledger root", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: GENESIS_HEADER_HASH,
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-fixture/emulator",
        grade: "security",
      },
      minimumConfirmationDepth: policy.confirmationDepth,
    });
    const validationTraceReplay = await admitValidationTraceReplayContext({
      evidence,
    });
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        { validationTraceReplay },
      );
    expect(result.detections).toEqual([]);
  });

  it("reconstructs the existing identity program from retained sidecars and classifies exact agreement", async () => {
    const { fixture, evidence, observation, context } =
      await admitRetainedPlutus({ verdict: "accepted" });
    expect(fixture.replay.trace.verdict).toBe("accepted");
    expect(
      fixture.replay.trace.witnesses.some(
        ({ auxiliary }) => auxiliary?.kind === "cekCoreStep",
      ),
    ).toBe(true);
    expect(
      evidence.reconstruction.payload.block_body.cek_program_material.length,
    ).toBeGreaterThan(0);
    const { decision } = await classifyRetainedReasonFixture({
      observation,
      payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
      deploymentFingerprint,
      releaseFinalityAuthority,
      replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
      replayContext: context,
    });
    expect(decision).toMatchObject({
      decision: "healthy",
      headerHash: fixture.block.headerHash,
    });
    const replay =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(replay.context?.validationTraceReplayDigest).toBe(
      context.validationTraceReplay.replayDigest,
    );
  });

  it("does not turn the shared Plutus/receive-language rejection hash into typed authority", async () => {
    const plutus = await admitRetainedPlutus({
      verdict: "rejected",
      reason: { PlutusExecutionFailed: { execution_index: 0n } },
    });
    const receive = await buildRetainedPlutusIdentityFixture({
      verdict: "rejected",
      reason: { ReceivePurposePlutusV3Forbidden: { execution_index: 0n } },
    });
    expect(receive.block.headerHash).toBe(plutus.fixture.block.headerHash);
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        plutus.evidence,
        plutus.context,
      );
    expect(result.detections).toEqual([]);
  });

  it("refuses missing, copied and cross-header replay authority", async () => {
    const { evidence, context } = await admitRetainedPlutus({
      verdict: "accepted",
    });
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence),
    ).rejects.toThrow(/admitted context/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        ...context,
        validationTraceReplay: { ...context.validationTraceReplay },
      }),
    ).rejects.toThrow(/not admitted/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        {
          ...evidence,
          headerHash: "e2".repeat(28),
        },
        context,
      ),
    ).rejects.toThrow(/challenged header|not admitted/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        validationTraceReplay: context.validationTraceReplay,
      }),
    ).rejects.toThrow(/not admitted/u);
    await expect(
      admitValidationTraceReplayContext({ evidence }),
    ).rejects.toThrow(/admitted predecessor/u);
  });

  it("re-admits the exact retained envelope instead of trusting a supplied reconstruction", async () => {
    const { evidence, context } = await admitRetainedPlutus({
      verdict: "accepted",
    });
    const readmitted = await admitValidationTraceReplayContext({
      evidence: {
        ...evidence,
        transactions: [],
        reconstruction: {
          ...evidence.reconstruction,
          transactions: [],
          sourceEvents: [],
        },
      },
      predecessor: context.predecessor,
    });
    expect(readmitted.replayDigest).toBe(
      context.validationTraceReplay.replayDigest,
    );
  });
});

describe("forced retained Plutus origin admission", () => {
  it("derives healthy execution from the retained program and admitted original bytes", async () => {
    const { evidence, context } = await admitRetainedPlutus(
      { verdict: "accepted" },
      { sourceKind: "forced" },
    );
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(result.detections).toEqual([]);
    expect(result.context?.validationTraceEventEvidenceDigest).toBe(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: context.transitionTraceEvents!,
      }),
    );
  });

  it("keeps replay identity stable across fresh captures of the same event evidence", async () => {
    const { fixture, evidence, context } = await admitRetainedPlutus(
      { verdict: "accepted" },
      { sourceKind: "forced" },
    );
    const transitionTraceEvents = await captureRetainedPlutusIdentityOrigins(
      fixture,
      { advanceCapture: true },
    );
    expect(transitionTraceEvents.snapshotDigest).not.toBe(
      context.transitionTraceEvents!.snapshotDigest,
    );
    expect(transitionTraceEvents.evidenceDigest).toBe(
      context.transitionTraceEvents!.evidenceDigest,
    );
    const validationTraceReplay = await admitValidationTraceReplayContext({
      evidence,
      predecessor: context.predecessor,
      transitionTraceEvents,
    });
    expect(validationTraceReplay.eventSnapshotDigest).toBe(
      transitionTraceEvents.snapshotDigest,
    );
    expect(validationTraceReplay.replayDigest).toBe(
      context.validationTraceReplay.replayDigest,
    );
    const previous =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    const refreshed =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        { ...context, transitionTraceEvents, validationTraceReplay },
      );
    expect(refreshed).toEqual(previous);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        ...context,
        transitionTraceEvents,
      }),
    ).rejects.toThrow(/not admitted/u);
  });

  it("selects the exact typed wrongful Plutus rejection and admits its private-material challenge", async () => {
    const { evidence, context } = await admitRetainedPlutus(
      {
        verdict: "rejected",
        reason: { PlutusExecutionFailed: { execution_index: 0n } },
      },
      { sourceKind: "forced" },
    );
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(result.detections).toHaveLength(1);
    const input = {
      evidence,
      context: context.validationTraceReplay,
      predecessor: context.predecessor,
      transitionTraceEvents: context.transitionTraceEvents,
      detectionId: result.detections[0]!.detectionId,
    };
    const selected = readValidationTraceReplaySelection(input);
    expect(selected.coordinate).toEqual({
      domain: "transition_step",
      index: "0",
    });
    expect(Object.keys(selected).sort()).toEqual([
      "coordinate",
      "detectionId",
      "eventKeyCbor",
      "headerHash",
      "payloadEnvelopeSha256",
      "payloadSha256",
    ]);
    // This tests owner admission only. The production adapter independently
    // admits a real transcript before constructing these workflow coordinates.
    const coordinate = {
      schemaVersion: W25_CHALLENGE_COORDINATE,
      deploymentFingerprint,
      stateQueueObservationDigest: "91".repeat(32),
      headerHash: selected.headerHash,
      payloadEnvelopeSha256: selected.payloadEnvelopeSha256,
      payloadSha256: selected.payloadSha256,
      transcriptDigest: "92".repeat(32),
      blockReplayResultDigest: "93".repeat(32),
      coordinate: { ...selected.coordinate },
    };
    await expect(
      admitValidationTraceChallengeFromReplayContext({
        ...input,
        coordinate: {
          ...coordinate,
          coordinate: { ...coordinate.coordinate, index: "1" },
        },
      }),
    ).rejects.toThrow(/changed selected event/u);
    const pending = admitValidationTraceChallengeFromReplayContext({
      ...input,
      coordinate,
    });
    coordinate.coordinate.index = "9";
    coordinate.transcriptDigest = "94".repeat(32);
    const challenge = await pending;
    expect(requireValidationTraceChallenge(challenge)).toBe(challenge);
    expect(challenge.coordinate.coordinate.index).toBe("0");
    expect(challenge.coordinate.transcriptDigest).toBe("92".repeat(32));
    expect(challenge.exactL1ReferenceOutRefs).toHaveLength(2);
    expect(() =>
      readValidationTraceReplaySelection({
        ...input,
        detectionId: "unadmitted",
      }),
    ).toThrow(/not an admitted interactive/u);
  });

  it("refuses the non-Plutus typed reason even when its code hash is shared", async () => {
    const { evidence, context } = await admitRetainedPlutus(
      {
        verdict: "rejected",
        reason: { ReceivePurposePlutusV3Forbidden: { execution_index: 0n } },
      },
      { sourceKind: "forced" },
    );
    const result =
      await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      );
    expect(result.detections).toEqual([]);
  });

  it("refuses missing origins and copied origin authority", async () => {
    await expect(
      admitRetainedPlutus(
        { verdict: "accepted" },
        { sourceKind: "forced", omitOriginAuthority: true },
      ),
    ).rejects.toThrow(/origin|events/u);
    await expect(
      admitRetainedPlutus(
        { verdict: "accepted" },
        { sourceKind: "forced", omitEvent: true },
      ),
    ).rejects.toThrow(/origin|event/u);
    const { evidence, context } = await admitRetainedPlutus(
      { verdict: "accepted" },
      { sourceKind: "forced" },
    );
    await expect(
      admitValidationTraceReplayContext({
        evidence,
        predecessor: context.predecessor,
        transitionTraceEvents: { ...context.transitionTraceEvents! },
      }),
    ).rejects.toThrow(/freshly admitted/u);
    await expect(
      VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        ...context,
        transitionTraceEvents: { ...context.transitionTraceEvents! },
      }),
    ).rejects.toThrow(/freshly admitted/u);
  });
});

describe("existing bounded CEK refusal through canonical retained replay", () => {
  it.each(["accepted", "rejected"] as const)(
    "classifies the %s operator claim from the actual canonical CEK boundary",
    async (verdict) => {
      const claim =
        verdict === "accepted"
          ? { verdict }
          : {
              verdict,
              reason: { PlutusExecutionFailed: { execution_index: 0n } },
            };
      const { fixture, evidence, observation, context } =
        await admitRetainedPlutus(claim, { program: "unboundVariable" });
      expect(fixture.replay.trace.verdict).toBe("rejected");
      expect(fixture.replay.trace.rejectionCode).toBe(
        "E_PLUTUS_SCRIPT_INVALID",
      );
      expect(fixture.replay.trace.witnesses.at(-2)?.auxiliary?.kind).toBe(
        "cekCoreStep",
      );
      const result =
        await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        );
      expect(result.detections).toHaveLength(verdict === "accepted" ? 1 : 0);
      const { decision } = await classifyRetainedReasonFixture({
        observation,
        payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
        replayContext: context,
      });
      expect(decision).toMatchObject(
        verdict === "accepted"
          ? { decision: "fault_detected", category: "validationTraceDispute" }
          : { decision: "healthy" },
      );
    },
  );
});
