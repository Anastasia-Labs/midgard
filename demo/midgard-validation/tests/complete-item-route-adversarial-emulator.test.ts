import "node:fs";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/native-tx-field-access";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/index.js";
import "./validation-fixtures.js";
import "./complete-item-route-adversarial-emulator.submit-stage.js";

import {
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimage,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  deriveCanonicalDecodeItemStageData,
  validationOneStepEvidenceHash,
} from "@al-ft/midgard-fault-proofs";
import {
  AuthenticatedCanonicalDecodeItemDatum,
  buildUnsignedValidationProofItemPublicationProgram,
  buildValidationTraceDisputeFaultProofContracts,
  deriveValidationProofItemPublication,
  ObservedCanonicalDecodeItemDatum,
  parseFaultProofBlueprint,
  PreparedCanonicalDecodeItemDatum,
  PreparedValidationResolutionDatum,
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  validationMachineStateDataFromCore,
  ValidationOneStepWitness,
  type ValidationOneStepWitness as ValidationOneStepWitnessData,
  type ValidationTraceDisputeFaultProofContracts,
  VerifiedCanonicalDecodeItemDatum,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
  encodeValidationOneStepWitnessCbor,
} from "../src/index.js";
import {
  blueprintJson,
  blueprintSpeaksOptionB,
  type Harness,
  sameDatumValue,
  submitAndAwait,
  submitStage,
  walletAddress,
} from "./complete-item-route-adversarial-emulator.submit-stage.js";
import {
  fundingLovelaceForOutputs,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

if (!blueprintSpeaksOptionB) {
  console.warn(
    "SKIPPED (#621): the blueprint at MIDGARD_REAL_BLUEPRINT_PATH (or " +
      "onchain/aiken/plutus.json) predates Option B — " +
      "canonical_decode_item_semantic_v1 still declares the retired carriage " +
      "parameter. Rebuild with the pinned Aiken fork (#617 regeneration) to " +
      "run the adversarial route matrix.",
  );
}

const NETWORK = "Custom" as const;

const HUB_ORACLE_POLICY_ID = "11".repeat(28);

const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);

const THREAD_ASSET_NAME = "aa".repeat(32);

const PROVER_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));

const PROVER_HASH = Buffer.from(
  PROVER_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

/** Wallet B: a continuation driver who is **not** `fraud_prover`. */
const THIRD_PARTY_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 9));

const THIRD_PARTY_HASH = Buffer.from(
  THIRD_PARTY_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

const MAX_L1_TX_BYTES = MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

/** §2.5 field 2 — the outputs field these traces read one complete item of. */
const OUTPUT_FIELD_INDEX = 2;

/** Small on purpose: routing, not size, is what this file is about. */
const FIELD_TWO_PREIMAGE_BYTES = 2_000;

// ## The disputed transaction (borrowed shape: carriage-tiers suite)

/**
 * RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1; R8 of decision 0005). This file
 * used to carry its own copy of the exact-size item builder, producing
 * 10-lovelace items that the ValueAndMint output-descriptor scan now convicts
 * with `E_MIN_ADA`. The shared builder funds each item at its own minimum-Ada
 * floor without moving its length, so every carriage measurement below
 * measures the same number of bytes it did before the wiring.
 */
const makeExactSizeOutputItem = makeMinAdaFundedExactSizeOutputItem;

const outputsForFieldTwoPreimageBytes = (
  targetBytes: number,
): readonly Buffer[] => {
  const payload = targetBytes - 7;
  const first = Math.floor(payload / 2);
  const outputs = [
    makeExactSizeOutputItem(first),
    makeExactSizeOutputItem(payload - first),
  ];
  const measured = encodeMidgardFieldPreimage(outputs).length;
  if (measured !== targetBytes) {
    throw new Error(
      `two-output field-2 envelope measured ${measured.toString()} bytes, wanted ${targetBytes.toString()}`,
    );
  }
  return outputs;
};

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

const buildTraceWithOutputs = async (
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
    outputs: [...outputs],
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

const findFieldTwoCompleteItemStep = (
  trace: DeterministicValidationMachineTrace,
  expectedPreimageBytes: number,
): { readonly stateIndex: number; readonly fieldPreimage: Buffer } => {
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    if (
      witness.phase !== "canonicalDecode" ||
      witness.auxiliary?.kind !== "transactionFieldItem" ||
      witness.auxiliary.fieldIndex !== OUTPUT_FIELD_INDEX ||
      witness.auxiliary.fieldPreimage.length !== expectedPreimageBytes
    ) {
      continue;
    }
    return {
      stateIndex: index,
      fieldPreimage: witness.auxiliary.fieldPreimage,
    };
  }
  throw new Error(
    `trace has no canonicalDecode field-2 complete-item witness of ${expectedPreimageBytes.toString()} preimage bytes`,
  );
};

let cachedContracts: ValidationTraceDisputeFaultProofContracts | undefined;

const loadContracts =
  async (): Promise<ValidationTraceDisputeFaultProofContracts> => {
    cachedContracts ??= await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(
          JSON.parse(JSON.stringify(blueprintJson)),
        ),
        network: NETWORK,
        hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
        fraudProofCataloguePolicyId: FRAUD_PROOF_CATALOGUE_POLICY_ID,
        referenceScriptAuthPolicyId: "33".repeat(28),
      }),
    );
    return cachedContracts;
  };

const THREAD_SEED_COUNT = 3;

const setupEmulator = async (): Promise<Harness> => {
  const contracts = await loadContracts();
  const threadUnit = toUnit(
    contracts.computationThread.policyId,
    THREAD_ASSET_NAME,
  );
  const proverAddress = walletAddress(PROVER_KEY);
  const emulator = new Emulator(
    [
      {
        seedPhrase: "",
        privateKey: PROVER_KEY.to_bech32(),
        address: proverAddress,
        assets: { lovelace: 500_000_000_000n },
      },
      // One seeded thread token per journey thread; minting them would need
      // the real thread policy and a hub oracle, and thread authenticity is
      // the fault-proofs lifecycle suites' scope, not this file's.
      ...Array.from({ length: THREAD_SEED_COUNT }, () => ({
        seedPhrase: "",
        privateKey: PROVER_KEY.to_bech32(),
        address: proverAddress,
        assets: { lovelace: 100_000_000n, [threadUnit]: 1n },
      })),
      {
        seedPhrase: "",
        privateKey: THIRD_PARTY_KEY.to_bech32(),
        address: walletAddress(THIRD_PARTY_KEY),
        assets: { lovelace: 10_000_000_000n },
      },
    ],
    { ...PROTOCOL_PARAMETERS_DEFAULT, maxTxSize: MAX_L1_TX_BYTES },
  );
  const proverLucid = await Lucid(emulator, NETWORK);
  proverLucid.selectWallet.fromPrivateKey(PROVER_KEY.to_bech32());
  const thirdPartyLucid = await Lucid(emulator, NETWORK);
  thirdPartyLucid.selectWallet.fromPrivateKey(THIRD_PARTY_KEY.to_bech32());
  return { emulator, proverLucid, thirdPartyLucid, contracts, threadUnit };
};

const publishReferenceScript = async (
  harness: Harness,
  script: Script,
): Promise<UTxO> => {
  const parkAddress = credentialToAddress(
    NETWORK,
    scriptHashToCredential("2f".repeat(28)),
  );
  const unsigned = await harness.proverLucid
    .newTx()
    .pay.ToAddressWithData(
      parkAddress,
      undefined,
      { lovelace: 60_000_000n },
      script,
    )
    .complete();
  const { txHash, signedCbor } = await submitAndAwait(
    harness.proverLucid,
    unsigned,
  );
  const outputs = CML.Transaction.from_cbor_hex(signedCbor).body().outputs();
  let scriptRefOutputIndex = -1;
  for (let index = 0; index < outputs.len(); index += 1) {
    if (outputs.get(index).script_ref() !== undefined) {
      scriptRefOutputIndex = index;
      break;
    }
  }
  if (scriptRefOutputIndex < 0) {
    throw new Error(
      "reference-script publication omitted its script-ref output",
    );
  }
  const published = await harness.proverLucid.utxosByOutRef([
    { txHash, outputIndex: scriptRefOutputIndex },
  ]);
  const utxo = published[0];
  if (published.length !== 1 || utxo === undefined || utxo.scriptRef == null) {
    throw new Error("published reference script was not found");
  }
  return utxo;
};

// ## The matrix

const NO_AUXILIARY_WITNESS_CBOR = Buffer.from("d87980", "hex");

const exercisedArms = new Set<string>();

describe.skipIf(!blueprintSpeaksOptionB)(
  "complete-item route adversarial matrix (emulator, applied validators, #621)",
  () => {
    it("refuses every hostile probe and completes both routes to identical observed state, one stage by a non-prover", async () => {
      // ### Content and commitments
      const outputs = outputsForFieldTwoPreimageBytes(FIELD_TWO_PREIMAGE_BYTES);
      const trace = await buildTraceWithOutputs(outputs);
      const { stateIndex, fieldPreimage } = findFieldTwoCompleteItemStep(
        trace,
        FIELD_TWO_PREIMAGE_BYTES,
      );
      expect(selectMidgardFieldCarriageTier(fieldPreimage.length)).toBe(
        "Inline",
      );
      const argument = buildValidationOneStepArgument({ trace, stateIndex });
      expect(argument.resolverIndex).toBe(0);
      expect(argument.semanticResolverIndex).toBe(1);
      const auxiliary = Data.from(argument.auxiliaryCbor.toString("hex"));
      if (
        !(auxiliary instanceof Constr) ||
        auxiliary.index !== 30 ||
        auxiliary.fields.length !== 1
      ) {
        throw new Error(
          "complete-item auxiliary witness has an unexpected shape",
        );
      }
      const carriageData = auxiliary.fields[0]!;
      const transitionData = Data.from(argument.transitionCbor.toString("hex"));
      const transition = Data.from(
        argument.transitionCbor.toString("hex"),
        ValidationOneStepWitness,
      ) as ValidationOneStepWitnessData;

      // Option B: the committed evidence is `(transition, NoAuxiliaryWitness)`.
      const evidenceHash = validationOneStepEvidenceHash({
        transitionCbor: argument.transitionCbor,
        auxiliaryCbor: NO_AUXILIARY_WITNESS_CBOR,
      });
      // The retired two-part commitment over the same evidence — genuinely
      // different bytes, or the replay below would be vacuous.
      const retiredEvidenceHash = validationOneStepEvidenceHash({
        transitionCbor: argument.transitionCbor,
        auxiliaryCbor: argument.auxiliaryCbor,
      });
      expect(retiredEvidenceHash).not.toBe(evidenceHash);

      // A well-formed transition that is not the committed one: same work
      // witness, wrong claimed successor (the pre-state itself).
      const wrongTransitionCbor = encodeValidationOneStepWitnessCbor({
        witness: trace.witnesses[stateIndex]!,
        claimedSuccessor: trace.states[stateIndex]!,
      });
      expect(wrongTransitionCbor.toString("hex")).not.toBe(
        argument.transitionCbor.toString("hex"),
      );
      const wrongTransitionData = Data.from(
        wrongTransitionCbor.toString("hex"),
      );
      const wrongTransition = Data.from(
        wrongTransitionCbor.toString("hex"),
        ValidationOneStepWitness,
      ) as ValidationOneStepWitnessData;

      const preState = validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      );
      const claimedSuccessorHash = hashMidgardValidationMachineState(
        trace.states[stateIndex + 1]!,
      ).toString("hex");
      const preparedThreadDatumWith = (hash: string): string =>
        Data.to(
          {
            fraud_prover: PROVER_HASH,
            data: {
              version: 1n,
              resolution: {
                version: 1n,
                pre_state: preState,
                operator_successor_hash: claimedSuccessorHash,
                challenger_successor_hash: claimedSuccessorHash,
              },
              evidence_hash: hash,
            },
          },
          PreparedValidationResolutionDatum,
        );
      const preparedThreadDatum = preparedThreadDatumWith(evidenceHash);
      const retiredHashThreadDatum =
        preparedThreadDatumWith(retiredEvidenceHash);

      const preparedResolutionOf = (datum: string) => {
        const parsed = (
          Data.from(
            datum,
            PreparedValidationResolutionDatum,
          ) as PreparedValidationResolutionDatumData
        ).data;
        if (parsed === null) {
          throw new Error("prepared thread datum is missing its state");
        }
        return parsed;
      };
      const stageData = deriveCanonicalDecodeItemStageData({
        preparedResolution: preparedResolutionOf(preparedThreadDatum),
        transition,
        fieldPreimage: fieldPreimage.toString("hex"),
      });
      const authenticatedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.authenticated },
        AuthenticatedCanonicalDecodeItemDatum,
      );
      const preparedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.prepared },
        PreparedCanonicalDecodeItemDatum,
      );
      const observedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.observed },
        ObservedCanonicalDecodeItemDatum,
      );
      const verifiedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.verified },
        VerifiedCanonicalDecodeItemDatum,
      );
      // The hostile authenticate replays keep their own datums consistent
      // with their own redeemers, so the one disagreement each probe stages
      // is the one the validator is claimed to refuse.
      const retiredHashAuthenticatedDatum = Data.to(
        {
          fraud_prover: PROVER_HASH,
          data: {
            ...deriveCanonicalDecodeItemStageData({
              preparedResolution: preparedResolutionOf(retiredHashThreadDatum),
              transition,
              fieldPreimage: fieldPreimage.toString("hex"),
            }).authenticated,
          },
        },
        AuthenticatedCanonicalDecodeItemDatum,
      );
      const wrongTransitionAuthenticatedDatum = Data.to(
        {
          fraud_prover: PROVER_HASH,
          data: deriveCanonicalDecodeItemStageData({
            preparedResolution: preparedResolutionOf(preparedThreadDatum),
            transition: wrongTransition,
            fieldPreimage: fieldPreimage.toString("hex"),
          }).authenticated,
        },
        AuthenticatedCanonicalDecodeItemDatum,
      );

      // ### Ledger
      const harness = await setupEmulator();
      const prover = { lucid: harness.proverLucid, hash: PROVER_HASH };
      const thirdParty = {
        lucid: harness.thirdPartyLucid,
        hash: THIRD_PARTY_HASH,
      };
      const stages =
        harness.contracts.validationTraceDispute.canonicalDecodeItemStages;
      const semanticContract =
        harness.contracts.validationTraceDispute.semanticResolvers[1];
      if (semanticContract === undefined) {
        throw new Error("canonical-decode item semantic resolver is missing");
      }
      const observeScriptReference = await publishReferenceScript(
        harness,
        stages.observe.spendingScript,
      );

      // Three threads over the same content: A rides inline, C rides the
      // publication, D carries the retired two-part commitment.
      const seedThread = async (datum: string): Promise<UTxO> => {
        const tokenSeed = (await harness.proverLucid.wallet().getUtxos()).find(
          (utxo) => utxo.assets[harness.threadUnit] === 1n,
        );
        if (tokenSeed === undefined) {
          throw new Error("thread token seed was not found");
        }
        const unsigned = await harness.proverLucid
          .newTx()
          .collectFrom([tokenSeed])
          .pay.ToContract(
            semanticContract.spendingScriptAddress,
            { kind: "inline", value: datum },
            { lovelace: 80_000_000n, [harness.threadUnit]: 1n },
          )
          .complete();
        const { txHash } = await submitAndAwait(harness.proverLucid, unsigned);
        const seeded = (
          await harness.proverLucid.utxosAt(
            semanticContract.spendingScriptAddress,
          )
        ).find(
          (utxo) =>
            utxo.txHash === txHash && utxo.assets[harness.threadUnit] === 1n,
        );
        if (seeded === undefined) {
          throw new Error("seeded thread UTxO was not found");
        }
        return seeded;
      };
      const threadA = await seedThread(preparedThreadDatum);
      const threadC = await seedThread(preparedThreadDatum);
      const threadD = await seedThread(retiredHashThreadDatum);

      // ### The authenticate boundary — retired wires refused, Option B green
      const verifyRedeemer = (fields: readonly unknown[]): string =>
        Data.to(new Constr(1, [new Constr(0, [...(fields as never[])])]));
      const authenticateStage = (
        inputUtxo: UTxO,
        outputDatum: string,
        encode: Parameters<typeof submitStage>[0]["encode"],
      ) =>
        submitStage({
          harness,
          driver: prover,
          inputUtxo,
          inputContract: semanticContract,
          outputContract: stages.source,
          outputDatum,
          label: "canonical item authentication",
          encode,
        });

      // The old two-part `(transition, auxiliary)` commitment, replayed on
      // the Option B wire: the applied resolver recomputes
      // `(transition, NoAuxiliaryWitness)` and the datum disagrees.
      await expect(
        authenticateStage(
          threadD,
          retiredHashAuthenticatedDatum,
          ({ inputIndex, outputIndex }) =>
            verifyRedeemer([inputIndex, outputIndex, transitionData]),
        ),
      ).rejects.toThrow(/local evaluation failed/u);
      exercisedArms.add("refused:retired-two-part-evidence-hash");

      // The retired four-field `Verify` wire (#620's fork), replayed with the
      // carriage appended exactly where it used to ride.
      await expect(
        authenticateStage(
          threadA,
          authenticatedDatum,
          ({ inputIndex, outputIndex }) =>
            verifyRedeemer([
              inputIndex,
              outputIndex,
              transitionData,
              carriageData,
            ]),
        ),
      ).rejects.toThrow(/local evaluation failed/u);
      exercisedArms.add("refused:retired-four-field-verify-wire");

      // A well-formed transition that is not the committed one: Option B's
      // remaining on-chain commitment is exactly this equality, so a
      // substituted successor must die here.
      await expect(
        authenticateStage(
          threadA,
          wrongTransitionAuthenticatedDatum,
          ({ inputIndex, outputIndex }) =>
            verifyRedeemer([inputIndex, outputIndex, wrongTransitionData]),
        ),
      ).rejects.toThrow(/local evaluation failed/u);
      exercisedArms.add("refused:transition-substitution");

      // Red then green: the same machinery, the honest wire.
      const authenticateA = await authenticateStage(
        threadA,
        authenticatedDatum,
        ({ inputIndex, outputIndex }) =>
          verifyRedeemer([inputIndex, outputIndex, transitionData]),
      );
      const authenticateC = await authenticateStage(
        threadC,
        authenticatedDatum,
        ({ inputIndex, outputIndex }) =>
          verifyRedeemer([inputIndex, outputIndex, transitionData]),
      );
      exercisedArms.add("green:option-b-verify");

      const sourceStage = (inputUtxo: UTxO) =>
        submitStage({
          harness,
          driver: prover,
          inputUtxo,
          inputContract: stages.source,
          outputContract: stages.observe,
          outputDatum: preparedDatum,
          label: "canonical item source binding",
          encode: ({ inputIndex, outputIndex }) =>
            Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
        });
      const sourceA = await sourceStage(authenticateA.nextThreadUtxo);
      const sourceC = await sourceStage(authenticateC.nextThreadUtxo);

      // ### The §8 publications — one honest, two hostile
      const publishProofItem = async (publication: {
        readonly datumCbor: string;
      }): Promise<UTxO> => {
        const unsigned = await Effect.runPromise(
          buildUnsignedValidationProofItemPublicationProgram(
            harness.proverLucid,
            harness.contracts,
            publication as Parameters<
              typeof buildUnsignedValidationProofItemPublicationProgram
            >[2],
          ),
        );
        const { txHash } = await submitAndAwait(harness.proverLucid, unsigned);
        const published = (
          await harness.proverLucid.utxosAt(
            harness.contracts.validationTraceDispute.proofItem
              .spendingScriptAddress,
          )
        ).find((utxo) => utxo.txHash === txHash);
        if (published === undefined) {
          throw new Error("proof-item publication was not found");
        }
        return published;
      };
      const honestPublication = await publishProofItem(
        deriveValidationProofItemPublication({
          transactionId: preState.transaction_id,
          transactionCommitment: preState.transaction_commitment,
          fieldPreimage: fieldPreimage.toString("hex"),
        }),
      );
      // Hostile: right bytes, wrong dispute — the commitment binding names a
      // different transaction commitment (§8.7's anti-fungibility pin).
      const wrongCommitmentPublication = await publishProofItem(
        deriveValidationProofItemPublication({
          transactionId: preState.transaction_id,
          transactionCommitment: preState.transaction_id,
          fieldPreimage: fieldPreimage.toString("hex"),
        }),
      );
      // Hostile: right dispute, wrong bytes — one byte of the preimage
      // flipped, so the door's field-commitment hash disagrees.
      const perturbedPreimage = Buffer.from(fieldPreimage);
      perturbedPreimage[perturbedPreimage.length - 1]! ^= 0x01;
      const wrongPreimagePublication = await publishProofItem(
        deriveValidationProofItemPublication({
          transactionId: preState.transaction_id,
          transactionCommitment: preState.transaction_commitment,
          fieldPreimage: perturbedPreimage.toString("hex"),
        }),
      );

      // ### The observe boundary — the door is the sole content gate
      const observeStage = ({
        driver,
        inputUtxo,
        encode,
        extraReferences,
      }: {
        readonly driver: { lucid: LucidEvolution; hash: string };
        readonly inputUtxo: UTxO;
        readonly encode: Parameters<typeof submitStage>[0]["encode"];
        readonly extraReferences?: readonly UTxO[];
      }) =>
        submitStage({
          harness,
          driver,
          inputUtxo,
          inputContract: stages.observe,
          outputContract: stages.proof,
          outputDatum: observedDatum,
          label: "canonical item observation",
          scriptReference: observeScriptReference,
          ...(extraReferences === undefined ? {} : { extraReferences }),
          encode,
        });

      // Hostile inline content, and by the third party at that: invalid data
      // fails whoever carries it — the door gates content, not identity.
      const corruptHex = perturbedPreimage.toString("hex");
      await expect(
        observeStage({
          driver: thirdParty,
          inputUtxo: sourceA.nextThreadUtxo,
          encode: ({ inputIndex, outputIndex }) =>
            Data.to(
              new Constr(1, [
                new Constr(0, [
                  inputIndex,
                  outputIndex,
                  new Constr(0, [corruptHex]),
                ]),
              ]),
            ),
        }),
      ).rejects.toThrow(/local evaluation failed/u);
      exercisedArms.add("refused:door-inline-content-mismatch");
      exercisedArms.add("refused:third-party-invalid-data");

      // Hostile publications through the reference door.
      const observeByReferenceC = (publication: UTxO) =>
        observeStage({
          driver: prover,
          inputUtxo: sourceC.nextThreadUtxo,
          extraReferences: [publication],
          encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
            Data.to(
              new Constr(1, [
                new Constr(1, [
                  inputIndex,
                  outputIndex,
                  referenceInputIndex(publication),
                ]),
              ]),
            ),
        });
      await expect(
        observeByReferenceC(wrongCommitmentPublication),
      ).rejects.toThrow(/local evaluation failed/u);
      exercisedArms.add("refused:publication-commitment-mismatch");
      await expect(
        observeByReferenceC(wrongPreimagePublication),
      ).rejects.toThrow(/local evaluation failed/u);
      exercisedArms.add("refused:publication-preimage-mismatch");

      // Green, reference route: the honest publication passes the same door.
      const observeC = await observeByReferenceC(honestPublication);
      expect(
        sameDatumValue(observeC.nextThreadUtxo.datum ?? "", observedDatum),
      ).toBe(true);
      exercisedArms.add("green:observe-reference");

      // Green, inline route, driven by the non-prover: valid data succeeds
      // whoever carries it, and lands on the exact state the off-chain
      // staging derived.
      const observeA = await observeStage({
        driver: thirdParty,
        inputUtxo: sourceA.nextThreadUtxo,
        encode: ({ inputIndex, outputIndex }) =>
          Data.to(
            new Constr(1, [
              new Constr(0, [
                inputIndex,
                outputIndex,
                new Constr(0, [fieldPreimage.toString("hex")]),
              ]),
            ]),
          ),
      });
      expect(
        sameDatumValue(observeA.nextThreadUtxo.datum ?? "", observedDatum),
      ).toBe(true);
      exercisedArms.add("green:observe-inline-third-party");

      // Route determinism: both doors wrote byte-identical observations of
      // the same content — the property that makes #621's build-time routing
      // a cost decision and nothing else.
      expect(
        sameDatumValue(
          observeA.nextThreadUtxo.datum ?? "",
          observeC.nextThreadUtxo.datum ?? "",
        ),
      ).toBe(true);
      exercisedArms.add("pin:route-determinism");

      // ### On to settlement: the mixed-driver thread coheres end to end
      const proofA = await submitStage({
        harness,
        driver: prover,
        inputUtxo: observeA.nextThreadUtxo,
        inputContract: stages.proof,
        outputContract: stages.settlement,
        outputDatum: verifiedDatum,
        label: "canonical item proof verification",
        encode: ({ inputIndex, outputIndex }) =>
          Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
      });
      exercisedArms.add("green:settlement-after-third-party-continuation");

      for (const [stage, bytes] of Object.entries({
        authenticateA: authenticateA.signedBytes,
        authenticateC: authenticateC.signedBytes,
        sourceA: sourceA.signedBytes,
        sourceC: sourceC.signedBytes,
        observeA: observeA.signedBytes,
        observeC: observeC.signedBytes,
        proofA: proofA.signedBytes,
      })) {
        expect(bytes, stage).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
      }
    }, 900_000);

    it("exercised every adversarial arm this file owns", () => {
      expect([...exercisedArms].sort()).toEqual(
        [
          "green:observe-inline-third-party",
          "green:observe-reference",
          "green:option-b-verify",
          "green:settlement-after-third-party-continuation",
          "pin:route-determinism",
          "refused:door-inline-content-mismatch",
          "refused:publication-commitment-mismatch",
          "refused:publication-preimage-mismatch",
          "refused:retired-four-field-verify-wire",
          "refused:retired-two-part-evidence-hash",
          "refused:third-party-invalid-data",
          "refused:transition-substitution",
        ].sort(),
      );
    });
  },
);
