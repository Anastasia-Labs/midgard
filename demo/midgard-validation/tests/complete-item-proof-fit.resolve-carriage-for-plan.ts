import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
} from "@al-ft/midgard-sdk";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  type ValidationMachineFieldCarriagePlanInput,
  type ValidationMachineFieldCarriageResolver,
  type ValidationMachineWorkWitness,
} from "../src/index.js";
import {
  fundingLovelaceForOutputs,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

const validationBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");

export const validationDisputeBlueprint = JSON.parse(
  readFileSync(validationBlueprintPath, "utf8"),
) as unknown;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

export const makeExactSizeOutputItem = makeMinAdaFundedExactSizeOutputItem;

export const buildTraceWithOutputs = async (
  outputs: readonly Buffer[],
): Promise<DeterministicValidationMachineTrace> => {
  const spent = outRefFromByte(0x11);
  // The resolved input has to fund every produced output now that each is
  // funded at its own minimum-Ada floor, or stage five would convict this
  // trace with `E_VALUE_NOT_PRESERVED` instead of accepting it. The fee is
  // zero, so the sum is exact.
  const spentOutput = makeOutput(fundingLovelaceForOutputs(outputs));
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

export const findFieldItemStep = (
  trace: DeterministicValidationMachineTrace,
  itemBytes: number,
  phase: "canonicalDecode" | "scriptSources" = "canonicalDecode",
): {
  readonly stateIndex: number;
  readonly witness: ValidationMachineWorkWitness;
  readonly planInput: ValidationMachineFieldCarriagePlanInput;
} => {
  const expectedPreimageBytes = encodeMidgardFieldPreimage([
    Buffer.alloc(itemBytes),
  ]).length;
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    // #600: both constructors carry the carriage *plan input* now — which field
    // and its §5.1 preimage — so a step is located by phase, kind, and the bytes
    // it read, which for these traces is field 2's single-item envelope.
    // Matching on content rather than on a claimed number is the same discipline
    // the door itself keeps, and it is what keeps this from selecting field 0's
    // step, whose complete-item witness comes first in the canonicalDecode walk.
    if (
      witness.phase !== phase ||
      !(
        witness.auxiliary?.kind === "transactionFieldItem" ||
        (phase === "scriptSources" &&
          witness.auxiliary?.kind === "transactionRedeemerItemBegin")
      )
    ) {
      continue;
    }
    const planInput = witness.auxiliary;
    if (planInput.fieldPreimage.length === expectedPreimageBytes) {
      return { stateIndex: index, witness, planInput };
    }
  }
  throw new Error(
    `trace has no ${phase} complete-item witness of ${itemBytes.toString()} bytes`,
  );
};

/**
 * Option B (#620): the item-semantic `Verify` is transition-only, so the
 * redeemer that embeds the item's §8 `FieldCarriageV1` — and therefore the one
 * whose envelope fit the direct frontier measures — is now the observe stage's
 * inline `Observe` arm, `(input_index, output_index, carriage)`. The
 * production submit path constructs this shape inside `submitStage`'s observe
 * encode; this mirror keeps the measured redeemer byte-identical to the
 * deployed ABI without a live transaction context.
 */
export const encodeInlineCompleteItemObserveRedeemer = ({
  auxiliaryCbor,
  inputIndex,
  outputIndex,
}: {
  readonly auxiliaryCbor: Buffer;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
}): Buffer => {
  const auxiliary = Data.from(auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 30 ||
    auxiliary.fields.length !== 1
  ) {
    throw new Error(
      "inline complete-item observation requires a TransactionFieldItemWitness auxiliary",
    );
  }
  return Buffer.from(
    Data.to(
      new Constr(1, [
        new Constr(0, [inputIndex, outputIndex, auxiliary.fields[0]!]),
      ]),
    ),
    "hex",
  );
};

/** A prover key hash — §8.6's `owner`, the min-Ada reclaim authority. */
const CARRIAGE_OWNER = Buffer.alloc(28, 0x7c);

/** The §8.6 certificate minting policy, a validator parameter (#579 rider 2). */
const CERTIFICATE_POLICY_ID = "ab".repeat(28);

const CERTIFICATE_ADDRESS = "addr_test1_field_preimage_certificate";

export const PROVER_KEY_ADDRESS = "addr_test1_prover_key_address";

const carriageUtxo = ({
  txHash,
  outputIndex,
  address,
  datum,
  assets,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly datum: string;
  readonly assets?: Record<string, bigint>;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: { lovelace: 5_000_000n, ...(assets ?? {}) },
  datum,
});

/**
 * The §8 carriage a dispute submitter would resolve for one field, together with
 * the reference-input set it resolved against (#600).
 *
 * Everything here comes from producers: `planMidgardFieldCarriage` decides the
 * tier by §8.4's partition over the preimage's length, the chunk datums are
 * `fieldPreimagePublicationDatumCbor`'s bytes, the manifest is
 * `deriveFieldPreimageCertification`'s, and the indices come back from
 * `resolveMidgardFieldCarriageAgainstReferenceInputs`, which locates each one
 * **by content** against the canonically-sorted list (§8.7). No index is written
 * down anywhere in this file.
 *
 * The reference-input set deliberately contains a **decoy** — the published
 * spending validator a real step reads through `readFrom`, which sorts into the
 * same list and shifts every carriage index that follows it. A resolver that
 * counted only carriage UTxOs would produce indices that are right here and
 * wrong on L1; including it is what makes these vectors measure the real thing
 * (ruling D3-A).
 */
const resolveCarriageForPlan = (
  plan: MidgardFieldCarriagePlan,
): {
  readonly carriage: ReturnType<
    typeof resolveMidgardFieldCarriageAgainstReferenceInputs
  >;
  readonly referenceInputs: readonly UTxO[];
} => {
  const scriptReference = carriageUtxo({
    txHash: "00".repeat(32),
    outputIndex: 0,
    address: "addr_test1_published_validator",
    datum: "d87980",
  });
  const publications = plan.publications.map((publication, offset) =>
    carriageUtxo({
      txHash: `${(offset + 3).toString(16).padStart(2, "0")}`.repeat(32),
      outputIndex: offset,
      address: PROVER_KEY_ADDRESS,
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  );
  const certificate =
    plan.tier === "Certified"
      ? [
          ((): UTxO => {
            const certification = deriveFieldPreimageCertification(plan);
            return carriageUtxo({
              txHash: "f1".repeat(32),
              outputIndex: 0,
              address: CERTIFICATE_ADDRESS,
              datum: certification.datumCbor,
              assets: {
                [`${CERTIFICATE_POLICY_ID}${certification.assetNameHex}`]: 1n,
              },
            });
          })(),
        ]
      : [];
  const referenceInputs = [scriptReference, ...publications, ...certificate];
  return {
    carriage: resolveMidgardFieldCarriageAgainstReferenceInputs({
      plan,
      referenceInputs,
      certificatePolicyId: CERTIFICATE_POLICY_ID,
    }),
    referenceInputs,
  };
};

/**
 * The resolver a submitter hands `buildValidationOneStepArgument` — #600's
 * seam, as the dispute path uses it.
 */
export const carriageResolverForTrace = (
  trace: DeterministicValidationMachineTrace,
): ValidationMachineFieldCarriageResolver => {
  const txId = Buffer.from(trace.states[0]!.transactionId);
  return ({ fieldIndex, fieldPreimage }) =>
    resolveCarriageForPlan(
      planMidgardFieldCarriage({
        owner: CARRIAGE_OWNER,
        txId,
        fieldIndex,
        preimage: fieldPreimage,
      }),
    ).carriage;
};
