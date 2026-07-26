import {
  canOpenMidgardValidationDisputeBeforeMaturity,
  computeHash32,
  type MidgardValidationTraceProofV1,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
  timeoutMidgardValidationDispute,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  hashBlockHeaderV1,
  HUB_ORACLE_ASSET_NAME,
  PendingValidationClaimDatumV1,
  type PendingValidationClaimDatumV1 as PendingValidationClaimDatumV1Data,
  PreparedValidationResolutionDatumV1,
  type PreparedValidationResolutionDatumV1 as PreparedValidationResolutionDatumV1Data,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ValidationAwardSpendRedeemerV1,
  ValidationBoundarySpendRedeemerV1,
  type ValidationClaimWitnessV1,
  ValidationDirectResolveSpendRedeemerV1,
  validationDisputeCoreFromData,
  validationDisputeDataFromCore,
  ValidationDisputeDatumV1,
  type ValidationDisputeDatumV1 as ValidationDisputeDatumV1Data,
  ValidationDisputeOpenSpendRedeemerV1,
  ValidationGameSpendRedeemerV1,
  type ValidationMachineStateV1,
  ValidationOneStepEvidenceV1,
  ValidationOneStepWitnessV1,
  ValidationPrepareSelectedSpendRedeemerV1,
  ValidationResolutionDatumV1,
  type ValidationResolutionDatumV1 as ValidationResolutionDatumV1Data,
  ValidationSourceSpendRedeemerV1,
  ValidationTimeoutSpendRedeemerV1,
  validationTraceDescriptorCoreFromData,
  validationTraceDescriptorDataFromCore,
  type ValidationTraceDescriptorV1,
  validationTraceProofCoreFromData,
  validationTraceProofDataFromCore,
  type ValidationTraceProofV1,
  WinningValidationResolutionDatumV1,
  type WinningValidationResolutionDatumV1 as WinningValidationResolutionDatumV1Data,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Constr,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "../submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../tx-layout.js";

export const VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS = 60_000;
export const VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS = 60_000;
export const MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES = 16 * 1024;

export type ValidationDisputeValidityRange = {
  readonly validFrom: number;
  readonly validTo: number;
};

const safeUnsignedNumber = (value: bigint, field: string): number => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return number;
};

export const validationDisputeValidityRange = (
  now: number,
): ValidationDisputeValidityRange => {
  if (
    !Number.isSafeInteger(now) ||
    now < VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS
  ) {
    throw new Error(
      "Validation-dispute current time must be a safe POSIX time",
    );
  }
  return {
    validFrom: now - VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS,
    validTo: now + VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS,
  };
};

export const validationDisputeTimeoutValidityRange = (
  now: number,
  responseDeadline: number,
): ValidationDisputeValidityRange => {
  const ordinary = validationDisputeValidityRange(now);
  if (!Number.isSafeInteger(responseDeadline) || responseDeadline < 0) {
    throw new Error(
      "Validation-dispute response deadline must be a non-negative safe integer",
    );
  }
  if (now <= responseDeadline) {
    throw new Error("Validation-dispute response deadline has not passed");
  }
  return requireValidityRange({
    validFrom: Math.max(ordinary.validFrom, responseDeadline + 1),
    validTo: ordinary.validTo,
  });
};

const requireValidityRange = (
  range: ValidationDisputeValidityRange,
): ValidationDisputeValidityRange => {
  if (
    !Number.isSafeInteger(range.validFrom) ||
    !Number.isSafeInteger(range.validTo) ||
    range.validFrom < 0 ||
    range.validTo < range.validFrom ||
    range.validTo - range.validFrom >
      VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS +
        VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS
  ) {
    throw new Error(
      "Validation-dispute validity range must be a non-negative closed range no longer than 120 seconds",
    );
  }
  return range;
};

const threadAssets = (threadUtxo: UTxO, threadUnit: string) => ({
  lovelace: threadUtxo.assets.lovelace ?? 0n,
  [threadUnit]: 1n,
});

const requireL1ProofEnvelope = (
  transactionCbor: string,
  label: string,
): void => {
  const bytes = transactionCbor.length / 2;
  if (
    transactionCbor.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(transactionCbor) ||
    bytes > MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `${label} transaction is ${bytes.toString()} bytes; the complete signed L1 proof transaction must be no larger than ${MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES.toString()} bytes`,
    );
  }
};

const VALIDATION_ONE_STEP_EVIDENCE_DOMAIN_V1 = Buffer.from(
  "MidgardValidationOneStepEvidenceV1",
  "ascii",
);

type PlutusDataValue = Data;

export type ValidationOneStepSubmissionArgumentV1 = {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number | null;
  readonly transitionCbor: Uint8Array;
  readonly auxiliaryCbor: Uint8Array;
};

const exactPlutusDataFromCbor = (
  value: Uint8Array,
  label: string,
): PlutusDataValue => {
  const bytes = Buffer.from(value);
  if (
    bytes.length === 0 ||
    bytes.length >= MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `${label} must be non-empty and strictly below the L1 proof envelope`,
    );
  }
  const decoded = Data.from(bytes.toString("hex"));
  const encoded = Buffer.from(Data.to(decoded), "hex");
  if (!encoded.equals(bytes)) {
    throw new Error(`${label} is not exact canonical V1 Plutus Data`);
  }
  return decoded;
};

const requireConstr = ({
  value,
  index,
  fields,
  label,
}: {
  readonly value: PlutusDataValue;
  readonly index: number;
  readonly fields: number;
  readonly label: string;
}): Constr<PlutusDataValue> => {
  if (
    !(value instanceof Constr) ||
    value.index !== index ||
    value.fields.length !== fields
  ) {
    throw new Error(
      `${label} must be constructor ${index.toString()} with ${fields.toString()} fields`,
    );
  }
  return value;
};

const validationOneStepEvidenceHashFromDataV1 = (
  transition: PlutusDataValue,
  auxiliary: PlutusDataValue,
): string => {
  const evidencePayload = Buffer.from(
    Data.to([transition, auxiliary]),
    "hex",
  );
  return computeHash32(
    Buffer.concat([
      VALIDATION_ONE_STEP_EVIDENCE_DOMAIN_V1,
      evidencePayload,
    ]),
  ).toString("hex");
};

export const validationOneStepEvidenceHashV1 = ({
  transitionCbor,
  auxiliaryCbor,
}: Pick<
  ValidationOneStepSubmissionArgumentV1,
  "transitionCbor" | "auxiliaryCbor"
>): string =>
  validationOneStepEvidenceHashFromDataV1(
    exactPlutusDataFromCbor(
      transitionCbor,
      "validation transition",
    ),
    exactPlutusDataFromCbor(
      auxiliaryCbor,
      "validation auxiliary witness",
    ),
  );

const VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1 = [
  2, 1, 1, 2, 4, 14, 2, 6, 20, 3, 4, 0, 0, 8,
] as const;
const VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1 = [
  0, 2, 3, 4, 6, 10, 24, 26, 32, 52, 55, -1, -1, 59,
] as const;

const auxiliaryShapeV1 = ({
  resolverIndex,
  semanticResolverIndex,
  auxiliary,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly auxiliary: PlutusDataValue;
}): Constr<PlutusDataValue> => {
  if (resolverIndex === 13) {
    const expected =
      semanticResolverIndex === 2 ||
      semanticResolverIndex === 4 ||
      semanticResolverIndex === 6 ||
      semanticResolverIndex === 7
        ? [0, 0]
        : semanticResolverIndex === 0
          ? [40, 4]
          : semanticResolverIndex === 1
            ? [32, 4]
            : semanticResolverIndex === 3
              ? [33, 3]
              : [39, 2];
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation LedgerDelta auxiliary witness",
    });
  }
  if (resolverIndex === 7) {
    const expected =
      semanticResolverIndex === 0 || semanticResolverIndex === 1
        ? [0, 0]
        : semanticResolverIndex === 2
          ? [9, 6]
          : semanticResolverIndex === 3
            ? [37, 1]
            : semanticResolverIndex === 4
              ? [38, 2]
              : [10, 4];
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation ResolveInputs auxiliary witness",
    });
  }
  if (resolverIndex === 8) {
    const outputExpected =
      semanticResolverIndex === 0
        ? null
        : semanticResolverIndex === 1
          ? [36, 4]
          : semanticResolverIndex === 2
            ? [37, 1]
            : semanticResolverIndex === 3
              ? [38, 2]
              : semanticResolverIndex === 5
                ? [2, 2]
                : semanticResolverIndex === 7
                  ? [41, 2]
                  : semanticResolverIndex >= 10 &&
                      semanticResolverIndex <= 12
                    ? [14, 8]
                    : semanticResolverIndex === 17
                      ? [14, 8]
                      : semanticResolverIndex === 19
                        ? [15, 3]
                    : semanticResolverIndex === 15
                      ? [34, 2]
                    : [0, 0];
    if (!(auxiliary instanceof Constr)) {
      throw new Error(
        "validation auxiliary witness must be a constructor",
      );
    }
    const outputAuxiliary = auxiliary;
    if (
      outputExpected !== null &&
      (
        outputAuxiliary.index !== outputExpected[0] ||
        outputAuxiliary.fields.length !== outputExpected[1]
      )
    ) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources proof family",
      );
    }
    return outputAuxiliary;
  }
  if (resolverIndex === 9) {
    const expected =
      semanticResolverIndex === 0 ? [0, 0] : [42, 17];
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation NativeScripts auxiliary witness",
    });
  }
  const expected =
    resolverIndex === 0
      ? semanticResolverIndex === 0
        ? [0, 0]
        : [2, 2]
      : resolverIndex === 1 ||
          resolverIndex === 2 ||
          resolverIndex === 10
        ? [0, 0]
        : resolverIndex === 3
          ? semanticResolverIndex === 0
            ? [0, 0]
            : [2, 2]
          : resolverIndex === 4
            ? semanticResolverIndex === 0 ||
              semanticResolverIndex === 3
              ? [0, 0]
              : semanticResolverIndex === 1
                ? [2, 2]
                : [3, 3]
            : resolverIndex === 5
              ? semanticResolverIndex === 0
                ? [0, 0]
                : semanticResolverIndex === 1
                  ? [2, 2]
                  : semanticResolverIndex === 13
                    ? [5, 1]
                    : [4, 3]
              : resolverIndex === 6
                ? semanticResolverIndex === 0
                  ? [0, 0]
                  : [2, 2]
                : null;
  if (expected === null) {
    throw new Error(
      `Validation resolver ${resolverIndex.toString()} has no staged semantic proof family`,
    );
  }
  return requireConstr({
    value: auxiliary,
    index: expected[0],
    fields: expected[1],
    label: "validation auxiliary witness",
  });
};

const requireStagedOneStepArgumentV1 = (
  argument: ValidationOneStepSubmissionArgumentV1,
): {
  readonly transition: ValidationOneStepWitnessV1;
  readonly transitionData: PlutusDataValue;
  readonly auxiliaryData: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly evidenceHash: string;
} => {
  if (
    !Number.isSafeInteger(argument.resolverIndex) ||
    argument.resolverIndex < 0 ||
    argument.resolverIndex >=
      VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1.length
  ) {
    throw new Error(
      "Staged validation one-step argument must select a prepare resolver",
    );
  }
  const semanticResolverIndex = argument.semanticResolverIndex;
  const semanticResolverCount =
    VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1[argument.resolverIndex]!;
  if (
    semanticResolverIndex === null ||
    !Number.isSafeInteger(semanticResolverIndex) ||
    semanticResolverIndex < 0 ||
    semanticResolverIndex >= semanticResolverCount
  ) {
    throw new Error(
      "Validation one-step argument selects an unavailable semantic resolver",
    );
  }
  const transitionData = exactPlutusDataFromCbor(
    argument.transitionCbor,
    "validation transition",
  );
  const auxiliaryData = exactPlutusDataFromCbor(
    argument.auxiliaryCbor,
    "validation auxiliary witness",
  );
  const transition = Data.from(
    Buffer.from(argument.transitionCbor).toString("hex"),
    ValidationOneStepWitnessV1,
  );
  const auxiliary = auxiliaryShapeV1({
    resolverIndex: argument.resolverIndex,
    semanticResolverIndex,
    auxiliary: auxiliaryData,
  });
  return {
    transition,
    transitionData,
    auxiliaryData,
    auxiliary,
    semanticResolverIndex,
    semanticResolverGlobalIndex:
      VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1[
        argument.resolverIndex
      ]! + semanticResolverIndex,
    evidenceHash: validationOneStepEvidenceHashFromDataV1(
      transitionData,
      auxiliaryData,
    ),
  };
};

const requireDirectOneStepArgumentV1 = (
  argument: ValidationOneStepSubmissionArgumentV1,
): {
  readonly evidence: ValidationOneStepEvidenceV1;
} => {
  if (
    !Number.isSafeInteger(argument.resolverIndex) ||
    argument.resolverIndex < 11 ||
    argument.resolverIndex > 12 ||
    argument.semanticResolverIndex !== null
  ) {
    throw new Error(
      "Direct validation one-step argument must select resolver 11 or 12",
    );
  }
  const transitionData = exactPlutusDataFromCbor(
    argument.transitionCbor,
    "validation transition",
  );
  const auxiliaryData = exactPlutusDataFromCbor(
    argument.auxiliaryCbor,
    "validation auxiliary witness",
  );
  Data.from(
    Buffer.from(argument.transitionCbor).toString("hex"),
    ValidationOneStepWitnessV1,
  );
  const evidenceData = new Constr(0, [
    transitionData,
    auxiliaryData,
  ]);
  const evidenceCbor = Data.to(evidenceData);
  return {
    evidence: Data.from(evidenceCbor, ValidationOneStepEvidenceV1),
  };
};

type ContinueLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

type OpenLayout = ContinueLayout & {
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

const makeOpenRedeemer = ({
  threadUtxo,
  hubOracleUtxo,
  stateQueueBlockUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  claim,
  challengerDescriptor,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly hubOracleUtxo: UTxO;
  readonly stateQueueBlockUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly claim: ValidationClaimWitnessV1;
  readonly challengerDescriptor: ValidationTraceDescriptorV1;
  readonly onLayout: (layout: OpenLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute open");
    const layout: OpenLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "validation dispute open"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute open",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "validation dispute open hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "validation dispute open state-queue block",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            Open: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              hub_ref_input_index: layout.hubOracleRefInputIndex,
              state_queue_node_ref_input_index:
                layout.stateQueueNodeRefInputIndex,
              claim,
              challenger_descriptor: challengerDescriptor,
            },
          },
        ],
      },
      ValidationDisputeOpenSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

const makeVerifySourceRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute verify source",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute verify source",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute verify source",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            VerifySource: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          },
        ],
      },
      ValidationSourceSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

const makeRevealRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  role,
  proof,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly role: "operator" | "challenger";
  readonly proof: ValidationTraceProofV1;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `validation dispute reveal ${role}`,
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        `validation dispute reveal ${role}`,
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        `validation dispute reveal ${role}`,
      ),
    };
    onLayout(layout);
    const action =
      role === "operator"
        ? {
            RevealOperator: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              proof,
            },
          }
        : {
            RevealChallenger: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              proof,
            },
          };
    return Data.to({ Continue: [action] }, ValidationGameSpendRedeemerV1);
  }) satisfies BuildTxWithRedeemer;

const makeGameHandoffRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  destination,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly destination: "resolution" | "challengerTimeout";
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    const label = `validation dispute enter ${destination}`;
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        label,
      ),
    };
    onLayout(layout);
    const action =
      destination === "resolution"
        ? {
            EnterResolution: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          }
        : {
            EnterChallengerTimeout: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          };
    return Data.to(
      { Continue: [action] },
      ValidationGameSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

export type SubmitValidationDisputeOpenResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly responseDeadline: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeOpen = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  claim,
  challengerDescriptor,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly claim: ValidationClaimWitnessV1;
  readonly challengerDescriptor: ValidationTraceDescriptorV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeOpenResult> => {
  const range = requireValidityRange(validityRange);
  const resolved = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { validationTraceDisputeCategory, hubOraclePolicyId, contracts } =
    resolved;
  const stateQueuePolicyId = resolved.stateQueuePolicyId!;
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
      label: "validation-dispute computation-thread UTxO",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(hubOraclePolicyId),
      ),
      unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "hub oracle",
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: "validation-dispute state-queue block UTxO",
    }),
  ]);
  const disputeContract = contracts.validationTraceDispute.firstStep;
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const fraudulentHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (fraudulentHeaderHash !== token.fraudulentHeaderHash) {
    throw new Error(
      `State-queue block header hash ${fraudulentHeaderHash} does not match computation-thread header hash ${token.fraudulentHeaderHash}`,
    );
  }
  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(stateQueueNodeView),
  );
  const computedHeaderHash = await Effect.runPromise(hashBlockHeaderV1(header));
  if (computedHeaderHash !== fraudulentHeaderHash) {
    throw new Error(
      `State-queue datum header hashes to ${computedHeaderHash}, expected ${fraudulentHeaderHash}`,
    );
  }
  const operatorDescriptor = validationTraceDescriptorCoreFromData(
    claim.descriptor_membership.value,
  );
  const challengerDescriptorCore =
    validationTraceDescriptorCoreFromData(challengerDescriptor);
  const dispute = openMidgardValidationDispute({
    operatorDescriptor,
    challengerDescriptor: challengerDescriptorCore,
    currentTime: range.validTo,
  });
  if (
    !canOpenMidgardValidationDisputeBeforeMaturity({
      currentTimeUpper: range.validTo,
      challengedBlockEndTime: safeUnsignedNumber(
        header.endTime,
        "header.endTime",
      ),
      maturityDuration: MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs,
    })
  ) {
    throw new Error(
      "Validation dispute cannot complete before the challenged block matures",
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        challenged_header_hash: fraudulentHeaderHash,
        challenged_header: header,
        claim,
        challenger_descriptor: challengerDescriptor,
        open_time_upper: BigInt(range.validTo),
      },
    },
    PendingValidationClaimDatumV1,
  );
  let layout: OpenLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeOpenRedeemer({
        threadUtxo,
        hubOracleUtxo,
        stateQueueBlockUtxo,
        outputAddress:
          contracts.validationTraceDispute.source.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        claim,
        challengerDescriptor,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .readFrom([hubOracleUtxo, stateQueueBlockUtxo])
    .pay.ToContract(
      contracts.validationTraceDispute.source.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(disputeContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute open layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "Validation-dispute open");
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudulentHeaderHash,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
    responseDeadline: dispute.responseDeadline,
    awaitedConfirmation: awaitConfirmation,
  };
};

const requirePendingClaimDatum = (
  threadUtxo: UTxO,
): PendingValidationClaimDatumV1Data & {
  readonly data: NonNullable<PendingValidationClaimDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation-dispute source UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, PendingValidationClaimDatumV1);
  if (datum.data === null) {
    throw new Error(
      "Validation-dispute source verification requires pending claim state",
    );
  }
  return datum as PendingValidationClaimDatumV1Data & {
    readonly data: NonNullable<PendingValidationClaimDatumV1Data["data"]>;
  };
};

export type SubmitValidationDisputeVerifySourceResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly responseDeadline: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeVerifySource = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeVerifySourceResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute source-verification UTxO",
  });
  const sourceContract = contracts.validationTraceDispute.source;
  if (threadUtxo.address !== sourceContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute source validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requirePendingClaimDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute source verification requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const operatorDescriptor = validationTraceDescriptorCoreFromData(
    inputDatum.data.claim.descriptor_membership.value,
  );
  const challengerDescriptor = validationTraceDescriptorCoreFromData(
    inputDatum.data.challenger_descriptor,
  );
  const dispute = openMidgardValidationDispute({
    operatorDescriptor,
    challengerDescriptor,
    currentTime: safeUnsignedNumber(
      inputDatum.data.open_time_upper,
      "pending.open_time_upper",
    ),
  });
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        challenged_header_hash: inputDatum.data.challenged_header_hash,
        operator_vkey: inputDatum.data.challenged_header.operatorVkey,
        dispute: validationDisputeDataFromCore(dispute),
      },
    },
    ValidationDisputeDatumV1,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeVerifySourceRedeemer({
        threadUtxo,
        outputAddress: contracts.validationTraceDispute.game.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.game.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(sourceContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute source layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute source verification",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    responseDeadline: dispute.responseDeadline,
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeRevealResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly role: "operator" | "challenger";
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly responseDeadline: number;
  readonly awaitedConfirmation: boolean;
};

type FinalizeLayout = ContinueLayout & {
  readonly fraudProofMintRedeemerIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
};

const makeTimeoutSpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly onLayout: (
    layout: Omit<FinalizeLayout, "computationThreadMintRedeemerIndex">,
  ) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute timeout");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute timeout",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: fraudProofAddress,
          datum: fraudProofDatum,
          unit: fraudProofUnit,
        }),
        "validation dispute timeout fraud proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "validation dispute timeout fraud-proof mint",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            ChallengerTimeout: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              fraud_proof_mint_redeemer_index:
                layout.fraudProofMintRedeemerIndex,
            },
          },
        ],
      },
      ValidationTimeoutSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "validation dispute fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "validation dispute computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "validation dispute computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const requireDisputeDatum = (
  threadUtxo: UTxO,
): ValidationDisputeDatumV1Data & {
  readonly data: NonNullable<ValidationDisputeDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation-dispute thread UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, ValidationDisputeDatumV1);
  if (datum.data === null) {
    throw new Error("Validation-dispute reveal requires initialized state");
  }
  return datum as ValidationDisputeDatumV1Data & {
    readonly data: NonNullable<ValidationDisputeDatumV1Data["data"]>;
  };
};

export const submitValidationDisputeReveal = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  role,
  proof,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly role: "operator" | "challenger";
  readonly proof: MidgardValidationTraceProofV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeRevealResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute computation-thread UTxO",
  });
  const disputeContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const expectedSigner =
    role === "operator"
      ? inputDatum.data.operator_vkey
      : inputDatum.fraud_prover;
  if (signer.paymentKeyHash !== expectedSigner) {
    throw new Error(
      `Validation-dispute ${role} reveal requires signer ${expectedSigner}, got ${signer.paymentKeyHash}`,
    );
  }
  const inputDispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  const nextDispute =
    role === "operator"
      ? revealMidgardValidationOperatorMidpoint({
          dispute: inputDispute,
          proof,
          currentTime: range.validTo,
        })
      : revealMidgardValidationChallengerMidpoint({
          dispute: inputDispute,
          proof,
          currentTime: range.validTo,
        });
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        challenged_header_hash: inputDatum.data.challenged_header_hash,
        operator_vkey: inputDatum.data.operator_vkey,
        dispute: validationDisputeDataFromCore(nextDispute),
      },
    },
    ValidationDisputeDatumV1,
  );
  const proofData = validationTraceProofDataFromCore(proof);
  // Round-trip before construction so non-canonical or out-of-range proof
  // fields fail before wallet selection and never reach balancing.
  validationTraceProofCoreFromData(proofData);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeRevealRedeemer({
        threadUtxo,
        outputAddress: disputeContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        role,
        proof: proofData,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      disputeContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(disputeContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      `BuildTxWithRedeemer did not resolve validation-dispute ${role} reveal layout`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), `Validation-dispute ${role} reveal`);
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    role,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    responseDeadline: nextDispute.responseDeadline,
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeEnterTimeoutResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeEnterTimeout = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange,
  now = Date.now(),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly now?: number;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeEnterTimeoutResult> => {
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute game UTxO",
  });
  const gameContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== gameContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute game validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute timeout handoff requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  const range = requireValidityRange(
    validityRange ??
      validationDisputeTimeoutValidityRange(now, dispute.responseDeadline),
  );
  if (
    timeoutMidgardValidationDispute({
      dispute,
      currentTime: range.validFrom,
    }) !== "challenger"
  ) {
    throw new Error(
      "Validation-dispute timeout does not award the fraud proof to the challenger",
    );
  }
  const outputDatum = Data.to(inputDatum, ValidationDisputeDatumV1);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeGameHandoffRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.timeout.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        destination: "challengerTimeout",
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.timeout.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(gameContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute timeout handoff layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute timeout handoff",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeTimeoutResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeTimeout = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange,
  now = Date.now(),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly now?: number;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeTimeoutResult> => {
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute computation-thread UTxO",
  });
  const disputeContract = contracts.validationTraceDispute.timeout;
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  const range = requireValidityRange(
    validityRange ??
      validationDisputeTimeoutValidityRange(now, dispute.responseDeadline),
  );
  if (
    timeoutMidgardValidationDispute({
      dispute,
      currentTime: range.validFrom,
    }) !== "challenger"
  ) {
    throw new Error(
      "Validation-dispute timeout does not award the fraud proof to the challenger",
    );
  }
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, token.assetName);
  const fraudProofDatum = Data.to(
    { fraud_prover: inputDatum.fraud_prover },
    FraudProofTokenDatum,
  );
  let partialLayout:
    | Omit<FinalizeLayout, "computationThreadMintRedeemerIndex">
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeTimeoutSpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        onLayout: (layout) => {
          partialLayout = layout;
        },
      }),
    )
    .mintAssets(
      { [token.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .attach.SpendingValidator(disputeContract.spendingScript)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    partialLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute timeout layout",
    );
  }
  const layout: FinalizeLayout = {
    ...partialLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "Validation-dispute timeout");
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudProofUnit,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      layout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(layout.fraudProofMintRedeemerIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const makePrepareResolutionRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  resolverIndex,
  preState,
  operatorPost,
  challengerPost,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: bigint;
  readonly preState: ValidationMachineStateV1;
  readonly operatorPost: ValidationTraceProofV1;
  readonly challengerPost: ValidationTraceProofV1;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute prepare resolution",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute prepare resolution",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute prepare resolution",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            PrepareResolution: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              resolver_index: resolverIndex,
              evidence: {
                pre_state: preState,
                operator_post: operatorPost,
                challenger_post: challengerPost,
              },
            },
          },
        ],
      },
      ValidationBoundarySpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

export type SubmitValidationDisputeEnterResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeEnterResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeEnterResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute game UTxO",
  });
  const gameContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== gameContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute game validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before one-step resolution",
    );
  }
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute resolution handoff requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const outputDatum = Data.to(inputDatum, ValidationDisputeDatumV1);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeGameHandoffRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.boundary.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        destination: "resolution",
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.boundary.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(gameContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute resolution handoff layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute resolution handoff",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const VALIDATION_RESOLVER_PHASES_V1 = [
  "CanonicalDecode",
  "CompactBinding",
  "StaticLedgerRules",
  "InputSets",
  "Signatures",
  "PhaseANativeScripts",
  "PhaseAScriptPreconditions",
  "ResolveInputs",
  "ScriptSources",
  "NativeScripts",
  "ScriptIntegrity",
  "Cek",
  "ValueAndMint",
  "LedgerDelta",
] as const satisfies readonly ValidationMachineStateV1["phase"][];

export const validationResolverIndexV1 = (
  phase: ValidationMachineStateV1["phase"],
): number => {
  const resolverIndex = VALIDATION_RESOLVER_PHASES_V1.indexOf(
    phase as (typeof VALIDATION_RESOLVER_PHASES_V1)[number],
  );
  if (resolverIndex < 0) {
    throw new Error(`Validation phase ${phase} has no one-step resolver`);
  }
  return resolverIndex;
};

const validationPrepareResolverDeploymentIndexV1 = (
  resolverIndex: number,
): number => {
  if (resolverIndex >= 0 && resolverIndex <= 8) {
    return resolverIndex;
  }
  if (resolverIndex === 9 || resolverIndex === 10) {
    return resolverIndex;
  }
  if (resolverIndex === 13) {
    return 11;
  }
  throw new Error(
    `Validation resolver ${resolverIndex.toString()} is not staged`,
  );
};

const validationDirectResolverDeploymentIndexV1 = (
  resolverIndex: number,
): number => {
  if (resolverIndex >= 11 && resolverIndex <= 12) {
    return resolverIndex - 11;
  }
  throw new Error(
    `Validation resolver ${resolverIndex.toString()} is not direct`,
  );
};

export type SubmitValidationDisputePrepareResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputePrepareResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  preState,
  operatorPost,
  challengerPost,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly preState: ValidationMachineStateV1;
  readonly operatorPost: ValidationTraceProofV1;
  readonly challengerPost: ValidationTraceProofV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputePrepareResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute boundary UTxO",
  });
  const boundaryContract = contracts.validationTraceDispute.boundary;
  if (threadUtxo.address !== boundaryContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute boundary validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute boundary preparation requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before boundary preparation",
    );
  }
  const resolverIndex = validationResolverIndexV1(preState.phase);
  const resolverContract =
    contracts.validationTraceDispute.resolvers[resolverIndex];
  if (resolverContract === undefined) {
    throw new Error(
      `Validation resolver ${resolverIndex.toString()} is missing from the deployment`,
    );
  }
  if (
    operatorPost.state_hash !== inputDatum.data.dispute.operator_high_hash ||
    challengerPost.state_hash !== inputDatum.data.dispute.challenger_high_hash
  ) {
    throw new Error(
      "Validation boundary successor proofs do not match the authenticated dispute",
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        pre_state: preState,
        operator_successor_hash: operatorPost.state_hash,
        challenger_successor_hash: challengerPost.state_hash,
      },
    },
    ValidationResolutionDatumV1,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makePrepareResolutionRedeemer({
        threadUtxo,
        outputAddress: resolverContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        resolverIndex: BigInt(resolverIndex),
        preState,
        operatorPost,
        challengerPost,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      resolverContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(boundaryContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute boundary layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute boundary preparation",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    resolverIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const requireResolutionDatum = (
  threadUtxo: UTxO,
): ValidationResolutionDatumV1Data & {
  readonly data: NonNullable<ValidationResolutionDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, ValidationResolutionDatumV1);
  if (datum.data === null) {
    throw new Error("Validation resolution requires initialized V1 state");
  }
  return datum as ValidationResolutionDatumV1Data & {
    readonly data: NonNullable<ValidationResolutionDatumV1Data["data"]>;
  };
};

const requirePreparedResolutionDatum = (
  threadUtxo: UTxO,
): PreparedValidationResolutionDatumV1Data & {
  readonly data: NonNullable<
    PreparedValidationResolutionDatumV1Data["data"]
  >;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Prepared validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(
    threadUtxo.datum,
    PreparedValidationResolutionDatumV1,
  );
  if (datum.data === null) {
    throw new Error(
      "Prepared validation resolution requires initialized V1 state",
    );
  }
  return datum as PreparedValidationResolutionDatumV1Data & {
    readonly data: NonNullable<
      PreparedValidationResolutionDatumV1Data["data"]
    >;
  };
};

const requireWinningResolutionDatum = (
  threadUtxo: UTxO,
): WinningValidationResolutionDatumV1Data & {
  readonly data: NonNullable<
    WinningValidationResolutionDatumV1Data["data"]
  >;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Winning validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(
    threadUtxo.datum,
    WinningValidationResolutionDatumV1,
  );
  if (datum.data === null || datum.data.version !== 1n) {
    throw new Error(
      "Winning validation resolution requires canonical V1 state",
    );
  }
  return datum as WinningValidationResolutionDatumV1Data & {
    readonly data: NonNullable<
      WinningValidationResolutionDatumV1Data["data"]
    >;
  };
};

const makePrepareSelectedRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  semanticResolverIndex,
  transition,
  auxiliary,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly semanticResolverIndex: number;
  readonly transition: ValidationOneStepWitnessV1;
  readonly auxiliary: PlutusDataValue;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute prepare selected semantic resolver",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute prepare selected semantic resolver",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute prepare selected semantic resolver",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            semantic_resolver_index: BigInt(
              semanticResolverIndex,
            ),
            transition,
            auxiliary,
          },
        ],
      },
      ValidationPrepareSelectedSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

const semanticActionFieldsV1 = ({
  resolverIndex,
  semanticResolverIndex,
  inputIndex,
  outputIndex,
  transition,
  auxiliary,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly transition: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
}): readonly PlutusDataValue[] => {
  const base: readonly PlutusDataValue[] = [
    inputIndex,
    outputIndex,
    transition,
  ];
  if (resolverIndex === 13) {
    if (
      (semanticResolverIndex === 2 ||
        semanticResolverIndex === 4 ||
        semanticResolverIndex === 6 ||
        semanticResolverIndex === 7) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 0 && auxiliary.index === 40) ||
      (semanticResolverIndex === 1 && auxiliary.index === 32) ||
      (semanticResolverIndex === 3 && auxiliary.index === 33) ||
      (semanticResolverIndex === 5 && auxiliary.index === 39)
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "LedgerDelta auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 7) {
    if (
      (semanticResolverIndex === 0 || semanticResolverIndex === 1) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 2 && auxiliary.index === 9) ||
      (semanticResolverIndex === 3 && auxiliary.index === 37) ||
      (semanticResolverIndex === 4 && auxiliary.index === 38) ||
      (semanticResolverIndex === 5 && auxiliary.index === 10)
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "ResolveInputs auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 8) {
    if (semanticResolverIndex === 0) {
      return [...base, auxiliary];
    }
    if (semanticResolverIndex === 1 && auxiliary.index === 36) {
      return [...base, ...auxiliary.fields];
    }
    if (semanticResolverIndex === 2 && auxiliary.index === 37) {
      return [...base, ...auxiliary.fields];
    }
    if (semanticResolverIndex === 3 && auxiliary.index === 38) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 4 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (semanticResolverIndex === 5 && auxiliary.index === 2) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 6 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (semanticResolverIndex === 7 && auxiliary.index === 41) {
      return [...base, ...auxiliary.fields];
    }
    if (
      (semanticResolverIndex === 8 || semanticResolverIndex === 9) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 10 ||
        semanticResolverIndex === 12) &&
      auxiliary.index === 14 &&
      auxiliary.fields.length === 8
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 11 &&
      auxiliary.index === 14 &&
      auxiliary.fields.length === 8
    ) {
      return [
        ...base,
        auxiliary.fields[0]!,
        auxiliary.fields[1]!,
        auxiliary.fields[2]!,
        auxiliary.fields[4]!,
        auxiliary.fields[5]!,
        auxiliary.fields[6]!,
        auxiliary.fields[7]!,
      ];
    }
    if (
      semanticResolverIndex === 13 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 14 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 15 &&
      auxiliary.index === 34 &&
      auxiliary.fields.length === 2
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 16 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 17 &&
      auxiliary.index === 14 &&
      auxiliary.fields.length === 8
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 18 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 19 &&
      auxiliary.index === 15 &&
      auxiliary.fields.length === 3
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "ScriptSources auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 9) {
    if (
      semanticResolverIndex === 0 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (auxiliary.index === 42 && auxiliary.fields.length === 17) {
      if (semanticResolverIndex === 1) {
        const firstChunk = requireConstr({
          value: auxiliary.fields[15]!,
          index: 0,
          fields: 1,
          label: "validation NativeScripts native first chunk",
        });
        if (auxiliary.fields[1] !== 0n) {
          throw new Error(
            "NativeScripts native semantic route requires language tag 0",
          );
        }
        return [
          ...base,
          auxiliary.fields[0]!,
          ...auxiliary.fields.slice(2, 15),
          firstChunk.fields[0]!,
          auxiliary.fields[16]!,
        ];
      }
      if (semanticResolverIndex === 2) {
        const languageTag = auxiliary.fields[1];
        const noFirstChunk = requireConstr({
          value: auxiliary.fields[15]!,
          index: 1,
          fields: 0,
          label: "validation NativeScripts effectful first chunk",
        });
        const signerPeaks = auxiliary.fields[16];
        if (
          (languageTag !== 3n && languageTag !== 128n) ||
          noFirstChunk.fields.length !== 0 ||
          !Array.isArray(signerPeaks) ||
          signerPeaks.length !== 0
        ) {
          throw new Error(
            "NativeScripts effectful semantic route has native-only evidence",
          );
        }
        return [...base, ...auxiliary.fields.slice(0, 15)];
      }
    }
    throw new Error(
      "NativeScripts auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (
    auxiliary.index === 0 &&
    auxiliary.fields.length === 0
  ) {
    return base;
  }
  if (auxiliary.index === 2 && auxiliary.fields.length === 2) {
    return [...base, ...auxiliary.fields];
  }
  if (auxiliary.index === 3 && auxiliary.fields.length === 3) {
    return [...base, ...auxiliary.fields];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex >= 2 &&
    semanticResolverIndex <= 7 &&
    auxiliary.index === 4 &&
    auxiliary.fields.length === 3
  ) {
    return [...base, auxiliary.fields[0]!, auxiliary.fields[1]!];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex >= 8 &&
    semanticResolverIndex <= 12 &&
    auxiliary.index === 4 &&
    auxiliary.fields.length === 3
  ) {
    return [...base, ...auxiliary.fields];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex === 13 &&
    auxiliary.index === 5 &&
    auxiliary.fields.length === 1
  ) {
    return [...base, auxiliary.fields[0]!];
  }
  if (resolverIndex === 6) {
    if (
      semanticResolverIndex === 0 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 1 &&
      auxiliary.index === 2 &&
      auxiliary.fields.length === 2
    ) {
      return [...base, ...auxiliary.fields];
    }
  }
  throw new Error(
    "Validation auxiliary witness cannot construct the selected semantic redeemer",
  );
};

export const encodeValidationSemanticResolutionRedeemerV1 = ({
  oneStepArgument,
  inputIndex,
  outputIndex,
}: {
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
}): Buffer => {
  if (inputIndex < 0n || outputIndex < 0n) {
    throw new Error(
      "Validation semantic redeemer indexes must be non-negative",
    );
  }
  const staged = requireStagedOneStepArgumentV1(oneStepArgument);
  const fields = semanticActionFieldsV1({
    resolverIndex: oneStepArgument.resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    inputIndex,
    outputIndex,
    transition: staged.transitionData,
    auxiliary: staged.auxiliary,
  });
  return Buffer.from(
    Data.to(new Constr(1, [new Constr(0, [...fields])])),
    "hex",
  );
};

const makeSemanticResolutionRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  resolverIndex,
  semanticResolverIndex,
  transition,
  auxiliary,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transition: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute semantic resolution",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute semantic resolution",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute semantic resolution",
      ),
    };
    onLayout(layout);
    const fields = semanticActionFieldsV1({
      resolverIndex,
      semanticResolverIndex,
      inputIndex: layout.inputIndex,
      outputIndex: layout.outputIndex,
      transition,
      auxiliary,
    });
    return Data.to(
      new Constr(1, [new Constr(0, [...fields])]),
    );
  }) satisfies BuildTxWithRedeemer;

type ValidationFinalizationResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

const makeValidationFinalizingSpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  label,
  encodeRedeemer,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly label: string;
  readonly encodeRedeemer: (
    layout: Omit<
      FinalizeLayout,
      "computationThreadMintRedeemerIndex"
    >,
  ) => string;
  readonly onLayout: (
    layout: Omit<
      FinalizeLayout,
      "computationThreadMintRedeemerIndex"
    >,
  ) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: fraudProofAddress,
          datum: fraudProofDatum,
          unit: fraudProofUnit,
        }),
        `${label} fraud proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        `${label} fraud-proof mint`,
      ),
    };
    onLayout(layout);
    return encodeRedeemer(layout);
  }) satisfies BuildTxWithRedeemer;

const submitValidationFinalizationTransaction = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  threadOutRef,
  token,
  spendingScript,
  spendLabel,
  encodeSpendRedeemer,
  validityRange,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: Awaited<
    ReturnType<
      typeof resolveValidationTraceDisputeDeploymentContracts
    >
  >["contracts"];
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadOutRef: string;
  readonly token: ReturnType<typeof requireComputationThreadToken>;
  readonly spendingScript: {
    readonly spendingScript: Script;
  };
  readonly spendLabel: string;
  readonly encodeSpendRedeemer: (
    layout: Omit<
      FinalizeLayout,
      "computationThreadMintRedeemerIndex"
    >,
  ) => string;
  readonly validityRange: ValidationDisputeValidityRange;
  readonly awaitConfirmation: boolean;
}): Promise<ValidationFinalizationResult> => {
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    token.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  let partialLayout:
    | Omit<
        FinalizeLayout,
        "computationThreadMintRedeemerIndex"
      >
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeValidationFinalizingSpendRedeemer({
        threadUtxo,
        fraudProofAddress:
          contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        label: spendLabel,
        encodeRedeemer: encodeSpendRedeemer,
        onLayout: (layout) => {
          partialLayout = layout;
        },
      }),
    )
    .mintAssets(
      { [token.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId:
          contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId:
          contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .validFrom(validityRange.validFrom)
    .validTo(validityRange.validTo)
    .attach.SpendingValidator(spendingScript.spendingScript)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    partialLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      `BuildTxWithRedeemer did not resolve ${spendLabel} layout`,
    );
  }
  const layout: FinalizeLayout = {
    ...partialLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), spendLabel);
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudProofUnit,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      layout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      layout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputePrepareSelectedResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputePrepareSelected = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputePrepareSelectedResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation prepare-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation semantic preparation requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndexV1(
    inputDatum.data.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the authenticated phase resolver",
    );
  }
  const staged = requireStagedOneStepArgumentV1(oneStepArgument);
  const prepareContract =
    contracts.validationTraceDispute.prepareResolvers[
      validationPrepareResolverDeploymentIndexV1(resolverIndex)
    ];
  const semanticContract =
    contracts.validationTraceDispute.semanticResolvers[
      staged.semanticResolverGlobalIndex
    ];
  if (prepareContract === undefined || semanticContract === undefined) {
    throw new Error(
      "Validation staged resolver deployment is incomplete",
    );
  }
  if (threadUtxo.address !== prepareContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at resolver ${resolverIndex.toString()}`,
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        resolution: inputDatum.data,
        evidence_hash: staged.evidenceHash,
      },
    },
    PreparedValidationResolutionDatumV1,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makePrepareSelectedRedeemer({
        threadUtxo,
        outputAddress: semanticContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        semanticResolverIndex: staged.semanticResolverIndex,
        transition: staged.transition,
        auxiliary: staged.auxiliaryData,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      semanticContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(prepareContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation semantic preparation layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation semantic preparation",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    semanticResolverGlobalIndex:
      staged.semanticResolverGlobalIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeSemanticResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeSemanticResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeSemanticResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "prepared validation semantic-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requirePreparedResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation semantic resolution requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndexV1(
    inputDatum.data.resolution.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the prepared phase resolver",
    );
  }
  const staged = requireStagedOneStepArgumentV1(oneStepArgument);
  if (staged.evidenceHash !== inputDatum.data.evidence_hash) {
    throw new Error(
      "Validation one-step argument does not match the prepared evidence hash",
    );
  }
  const semanticContract =
    contracts.validationTraceDispute.semanticResolvers[
      staged.semanticResolverGlobalIndex
    ];
  if (semanticContract === undefined) {
    throw new Error(
      "Validation semantic resolver deployment is incomplete",
    );
  }
  if (threadUtxo.address !== semanticContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at semantic resolver ${staged.semanticResolverGlobalIndex.toString()}`,
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: { version: 1n },
    },
    WinningValidationResolutionDatumV1,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeSemanticResolutionRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.award.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        resolverIndex,
        semanticResolverIndex: staged.semanticResolverIndex,
        transition: staged.transitionData,
        auxiliary: staged.auxiliary,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.award.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(semanticContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation semantic resolution layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation semantic resolution",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    semanticResolverGlobalIndex:
      staged.semanticResolverGlobalIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeAwardResult =
  ValidationFinalizationResult;

export const submitValidationDisputeAward = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeAwardResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "winning validation award UTxO",
  });
  const awardContract = contracts.validationTraceDispute.award;
  if (threadUtxo.address !== awardContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation award validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireWinningResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation award requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  return await submitValidationFinalizationTransaction({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadOutRef,
    token,
    spendingScript: awardContract,
    spendLabel: "Validation-dispute award",
    encodeSpendRedeemer: (layout) =>
      Data.to(
        {
          Continue: [
            {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              fraud_proof_mint_redeemer_index:
                layout.fraudProofMintRedeemerIndex,
            },
          ],
        },
        ValidationAwardSpendRedeemerV1,
      ),
    validityRange: range,
    awaitConfirmation,
  });
};

export type SubmitValidationDisputeDirectResolutionResult =
  ValidationFinalizationResult & {
    readonly resolverIndex: number;
  };

export const submitValidationDisputeDirectResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeDirectResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation direct-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Direct validation resolution requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndexV1(
    inputDatum.data.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the direct phase resolver",
    );
  }
  const direct = requireDirectOneStepArgumentV1(oneStepArgument);
  const directContract =
    contracts.validationTraceDispute.directResolvers[
      validationDirectResolverDeploymentIndexV1(resolverIndex)
    ];
  if (directContract === undefined) {
    throw new Error(
      "Validation direct resolver deployment is incomplete",
    );
  }
  if (threadUtxo.address !== directContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at direct resolver ${resolverIndex.toString()}`,
    );
  }
  const finalized =
    await submitValidationFinalizationTransaction({
      lucid,
      contracts,
      signer,
      threadUtxo,
      threadOutRef,
      token,
      spendingScript: directContract,
      spendLabel: "Validation-dispute direct resolution",
      encodeSpendRedeemer: (layout) =>
        Data.to(
          {
            Continue: [
              {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                fraud_proof_mint_redeemer_index:
                  layout.fraudProofMintRedeemerIndex,
                challenger_evidence: direct.evidence,
              },
            ],
          },
          ValidationDirectResolveSpendRedeemerV1,
        ),
      validityRange: range,
      awaitConfirmation,
    });
  return { ...finalized, resolverIndex };
};

export const validationDisputeDescriptorData =
  validationTraceDescriptorDataFromCore;
