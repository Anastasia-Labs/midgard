import { selectMidgardFieldCarriageTier } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  AuthenticatedCanonicalDecodeItemDatum,
  ObservedCanonicalDecodeItemDatum,
  PreparedCanonicalDecodeItemDatum,
  PreparedValidationResolutionDatum,
  validationMachineStateDataFromCore,
  ValidationOneStepWitness,
  ValidationResolutionDatum,
  VerifiedCanonicalDecodeItemDatum,
  WinningValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";

import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../step-support.js";
import { witnessSpendingValidatorCarriage } from "../witness-reference-scripts.js";
import { journalJsonDigest } from "../workflow/journal.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionReferenceInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { type FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import { deriveCanonicalDecodeItemStageData } from "./submit/cek-route.js";
import {
  midgardFieldCarriageToData,
  validationOneStepEvidenceHashFromData,
} from "./submit/evidence.js";
import { validationSemanticResolverGlobalIndex } from "./submit/reference-scripts.js";
import { makeIndexedValidationStageRedeemer } from "./submit/semantic-redeemers.js";
import {
  requireL1ProofEnvelope,
  threadAssets,
} from "./submit/transaction-material.js";
import {
  reachOptionalPreSubmitBoundary,
  requireValidityRange,
  validationDisputeValidityRange,
} from "./submit/validity.js";
import { readCanonicalCheckpoint } from "./workflow-canonical-checkpoint.js";
import {
  type ValidationTraceDisputeActuatorAction,
  type ValidationTraceDisputeActuatorConfig,
} from "./workflow-engine.plan-validation-trace-dispute-move.js";
import { type ValidationTraceDisputeActuationMaterial } from "./workflow-engine.plan-validation-trace-dispute-move.js";
import { recoverValidationTraceStateIndex } from "./workflow-engine.recover-validation-trace-state-index.js";
import { type createValidationTraceFieldCarriageProvider } from "./workflow-field-carriage.js";

type Delivery = NonNullable<
  Awaited<
    ReturnType<
      ReturnType<typeof createValidationTraceFieldCarriageProvider>["resolve"]
    >
  >
>;

/** One cold, locally evaluated transaction through the existing Option B stages. */
export const submitCanonicalCheckpoint = async ({
  config,
  input,
  material,
  delivery,
  reference,
  preSubmitBoundary,
}: {
  readonly config: ValidationTraceDisputeActuatorConfig;
  readonly input: UTxO;
  readonly material: ValidationTraceDisputeActuationMaterial;
  readonly delivery: Delivery | undefined;
  readonly reference: UTxO;
  readonly preSubmitBoundary: FraudProofPreSubmitBoundary;
}) => {
  const contracts = config.resolved.contracts;
  const checkpoint = readCanonicalCheckpoint(
    input,
    contracts.validationTraceDispute,
  );
  if (checkpoint === undefined)
    throw new Error("unsupported canonical continuation checkpoint");
  if (checkpoint.fraudProver !== config.signer.paymentKeyHash)
    throw new Error("canonical checkpoint belongs to a different prover");
  const stateIndex = recoverValidationTraceStateIndex({
    trace: material.challengerTrace,
    resolution: checkpoint.resolution,
  });
  const witness = material.challengerTrace.witnesses[stateIndex]!;
  const pre = material.challengerTrace.states[stateIndex]!;
  const successor = material.challengerTrace.states[stateIndex + 1]!;
  if (
    pre.phase !== "canonicalDecode" ||
    witness.auxiliary?.kind !== "transactionFieldItem" ||
    witness.phase !== pre.phase ||
    witness.programCounter !== pre.programCounter ||
    successor.programCounter !== pre.programCounter + 1
  )
    throw new Error(
      "canonical continuation requires its exact complete-item trace step",
    );
  const fieldPreimage = witness.auxiliary.fieldPreimage;
  const transition = {
    work_witness_cbor: witness.cbor.toString("hex"),
    claimed_successor: validationMachineStateDataFromCore(successor),
  };
  const transitionCbor = Data.to(transition, ValidationOneStepWitness);
  const transitionData = Data.from(transitionCbor);
  // This is the existing Option B domain-normalized commitment, not carriage.
  const evidenceHash = validationOneStepEvidenceHashFromData(
    transitionData,
    new Constr(0, []),
  );
  const prepared = checkpoint.prepared ?? {
    version: 1n,
    resolution: checkpoint.resolution,
    evidence_hash: evidenceHash,
  };
  if (prepared.evidence_hash !== evidenceHash)
    throw new Error(
      "canonical continuation differs from its transition-only preparation",
    );
  const states = deriveCanonicalDecodeItemStageData({
    preparedResolution: prepared,
    transition,
    fieldPreimage: fieldPreimage.toString("hex"),
  });
  const fraud_prover = checkpoint.fraudProver;
  const datums = {
    prepare: Data.to(
      { fraud_prover, data: checkpoint.resolution },
      ValidationResolutionDatum,
    ),
    authenticate: Data.to(
      { fraud_prover, data: prepared },
      PreparedValidationResolutionDatum,
    ),
    source: Data.to(
      { fraud_prover, data: states.authenticated },
      AuthenticatedCanonicalDecodeItemDatum,
    ),
    observe: Data.to(
      { fraud_prover, data: states.prepared },
      PreparedCanonicalDecodeItemDatum,
    ),
    proof: Data.to(
      { fraud_prover, data: states.observed },
      ObservedCanonicalDecodeItemDatum,
    ),
    settlement: Data.to(
      { fraud_prover, data: states.verified },
      VerifiedCanonicalDecodeItemDatum,
    ),
    award: Data.to(
      { fraud_prover, data: { version: 1n } },
      WinningValidationResolutionDatum,
    ),
  };
  if (input.datum !== datums[checkpoint.role])
    throw new Error(
      "canonical checkpoint does not equal its authenticated source derivation",
    );
  const chain = contracts.validationTraceDispute;
  const stageContracts = {
    prepare: chain.canonicalDecodePrepare,
    authenticate:
      chain.semanticResolvers[validationSemanticResolverGlobalIndex(0, 1)]!,
    ...chain.canonicalDecodeItemStages,
    award: chain.award,
  };
  const nextRole = {
    prepare: "authenticate",
    authenticate: "source",
    source: "observe",
    observe: "proof",
    proof: "settlement",
    settlement: "award",
  } as const;
  const next = nextRole[checkpoint.role];
  const token = requireComputationThreadToken({
    utxo: input,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: config.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  config.signer.selectWallet(config.lucid);
  const fee = selectFeeInput(await config.lucid.wallet().getUtxos());
  const scriptCarriage = witnessSpendingValidatorCarriage({
    script: stageContracts[checkpoint.role].spendingScript,
    referenceUtxo: reference,
    label: "canonical cold continuation",
  });
  if (
    checkpoint.role === "observe" &&
    delivery === undefined &&
    selectMidgardFieldCarriageTier(fieldPreimage.length) !== "Inline"
  )
    throw new Error(
      "canonical observation omitted authenticated field delivery",
    );
  const proofItemReference =
    delivery !== undefined && "proofItemReference" in delivery
      ? delivery.proofItemReference
      : undefined;
  const refs = [
    reference,
    ...(checkpoint.role === "observe"
      ? (delivery?.material.referenceUtxos ?? [])
      : []),
  ];
  const carriage =
    checkpoint.role === "observe"
      ? midgardFieldCarriageToData(
          delivery?.resolveFieldCarriage({
            fieldIndex: witness.auxiliary.fieldIndex,
            fieldPreimage,
          }) ?? { carriage: "Inline", preimage: fieldPreimage },
        )
      : undefined;
  const outputDatum = datums[next];
  const outputAddress = stageContracts[next].spendingScriptAddress;
  const redeemer = makeIndexedValidationStageRedeemer({
    threadUtxo: input,
    outputAddress,
    outputDatum,
    threadUnit: token.unit,
    ...(proofItemReference === undefined
      ? {}
      : { proofItemReferenceUtxo: proofItemReference }),
    label: "canonical cold continuation",
    onLayout: () => {},
    encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
      Data.to(
        new Constr(1, [
          new Constr(
            checkpoint.role === "observe" && proofItemReference !== undefined
              ? 1
              : 0,
            [
              inputIndex,
              outputIndex,
              ...(checkpoint.role === "prepare"
                ? [1n, transitionData]
                : checkpoint.role === "authenticate"
                  ? [transitionData]
                  : checkpoint.role === "observe"
                    ? [
                        proofItemReference === undefined
                          ? carriage!
                          : referenceInputIndex!,
                      ]
                    : []),
            ],
          ),
        ]),
      ),
  });
  const range = requireValidityRange(
    validationDisputeValidityRange(
      config.lucid.slotToUnixTime(config.lucid.currentSlot()),
    ),
  );
  const tx = scriptCarriage.attach(
    config.lucid
      .newTx()
      .collectFrom([fee])
      .collectFrom([input], redeemer)
      .readFrom(refs)
      .pay.ToContract(
        outputAddress,
        { kind: "inline", value: outputDatum },
        threadAssets(input, token.unit),
      )
      .validFrom(range.validFrom)
      .validTo(range.validTo)
      .addSignerKey(config.signer.paymentKeyHash),
  );
  const unsigned = await tx
    .complete({ localUPLCEval: true })
    .catch((cause: unknown) => {
      throw new Error(
        `canonical ${checkpoint.role} local evaluation failed: ${String(cause)}`,
      );
    });
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "canonical cold continuation");
  await reachOptionalPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates: [
      { role: "canonical cold continuation", utxo: reference },
    ],
  });
  // The production boundary captures before submission; manual invocation stays explicit.
  return await signed.submit();
};

/** Selects only the supported output route; every other route keeps its strict path. */
export const captureCanonicalCheckpoint = async ({
  config,
  material,
  action,
  input,
  publishedThreadScriptReference,
}: {
  readonly config: ValidationTraceDisputeActuatorConfig;
  readonly material: ValidationTraceDisputeActuationMaterial;
  readonly action: ValidationTraceDisputeActuatorAction;
  readonly input: UTxO;
  readonly publishedThreadScriptReference: (
    input: UTxO,
    label: string,
  ) => Promise<UTxO | undefined>;
}) => {
  const canonical = readCanonicalCheckpoint(
    input,
    config.resolved.contracts.validationTraceDispute,
  );
  if (canonical === undefined) return undefined;
  const stateIndex = recoverValidationTraceStateIndex({
    trace: material.challengerTrace,
    resolution: canonical.resolution,
  });
  const auxiliary = material.challengerTrace.witnesses[stateIndex]!.auxiliary;
  if (auxiliary?.kind !== "transactionFieldItem" || auxiliary.fieldIndex !== 2)
    return undefined;
  const delivery =
    canonical.role === "observe"
      ? await config.fieldCarriage?.resolve(action, stateIndex)
      : undefined;
  const reference = await publishedThreadScriptReference(
    input,
    "canonical checkpoint",
  );
  if (reference === undefined)
    throw new Error("canonical checkpoint omitted manifest-bound publication");
  const transaction = await captureLocallyEvaluatedTransaction(
    async (preSubmitBoundary) => {
      await submitCanonicalCheckpoint({
        config,
        input,
        material,
        delivery,
        reference,
        preSubmitBoundary,
      });
    },
  );
  if (
    delivery !== undefined &&
    journalJsonDigest(
      [...workflowTransactionReferenceInputOutRefs(transaction.signed)].sort(),
    ) !== journalJsonDigest(delivery.durableBinding.referenceOutRefs)
  )
    throw new Error(
      "canonical observation changed its authenticated reference set",
    );
  // Option B binds the observation's actual signed transaction and references
  // in its submission intent; it never freezes that carriage in earlier/later
  // checkpoint evidence. Exact source identity stays in the authenticated datum.
  return Object.freeze({ transaction });
};
