import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimageV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  deriveCanonicalDecodeItemStageDataV1,
  validationOneStepEvidenceHashV1,
} from "@al-ft/midgard-fault-proofs";
import {
  AuthenticatedCanonicalDecodeItemDatumV1,
  buildUnsignedValidationProofItemPublicationV1Program,
  buildValidationTraceDisputeFaultProofContracts,
  deriveValidationProofItemPublicationV1,
  ObservedCanonicalDecodeItemDatumV1,
  parseFaultProofBlueprint,
  PreparedCanonicalDecodeItemDatumV1,
  PreparedValidationResolutionDatumV1,
  type PreparedValidationResolutionDatumV1 as PreparedValidationResolutionDatumV1Data,
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  validationMachineStateDataFromCore,
  ValidationOneStepWitnessV1,
  type ValidationOneStepWitnessV1 as ValidationOneStepWitnessV1Data,
  type ValidationTraceDisputeFaultProofContracts,
  VerifiedCanonicalDecodeItemDatumV1,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
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
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgumentV1,
  type DeterministicValidationMachineTrace,
  encodeValidationOneStepWitnessCborV1,
} from "../src/index.js";
import {
  fundingLovelaceForOutputsV1,
  makeMinAdaFundedExactSizeOutputItemV1,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

/**
 * **The adversarial route matrix for the Option B complete-item chain (#621),
 * against the applied validators** — hostile probes red, honest continuations
 * green, on one emulator ledger.
 *
 * Since Option B (#619/#620) the committed `evidence_hash` is transition-only
 * and the observe stage's §8.8 door is the sole content gate, so two things
 * become claims that need falsifiers rather than assumptions:
 *
 * 1. **The door really is a gate.** Corrupted inline bytes, a publication
 *    whose commitment binding is wrong, and a publication whose preimage is
 *    not the committed field's are each refused by the applied validator —
 *    and then the same machinery passes with honest material, so the reds
 *    are attributable to the mutation and nothing else.
 * 2. **The routes and the drivers are interchangeable.** The staged datums
 *    are route-independent, so the inline door and the reference door must
 *    write byte-identical observations — and because `continue()` ignores
 *    `fraud_prover`, a third party who is not the prover must be able to
 *    drive a stage to that same state with valid data (and must fail with
 *    invalid data). Both are proved here by driving two threads over the
 *    same content, one per route, one stage by a non-prover wallet.
 *
 * The retired wires are replayed too: the pre-#620 four-field `Verify`
 * (transition + carriage) and a thread whose datum still commits the old
 * two-part `(transition, auxiliary)` evidence hash are both refused at the
 * authenticate boundary — the pins that Option B's commitment change is
 * enforced on chain, not merely spoken off chain.
 *
 * Harness notes: like `complete-item-carriage-tiers-emulator-v1.test.ts` the
 * computation-thread tokens are seeded (thread authenticity is the
 * fault-proofs lifecycle suites' job); unlike it, everything here speaks the
 * Option B wire, so against a pre-Option-B blueprint this file skips loudly
 * (see the gate below) instead of manufacturing the recorded rows'
 * unfalsifiable `Spend[0]` red.
 */

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const blueprintJson = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
    readonly parameters?: readonly { readonly title: string }[];
  }[];
};

const ITEM_SEMANTIC_SPEND_TITLE =
  "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend";

/**
 * The Option B gate (#621): #620 removed the carriage parameter from
 * `canonical_decode_item_semantic_v1`, taking its declared parameter list
 * from three entries to two. Against the three-parameter deployed build every
 * journey here would red out exactly like the recorded expected-red rows in
 * the fault-proofs suite, proving nothing — so skip, and say why.
 */
const blueprintSpeaksOptionB = (() => {
  const itemSemantic = blueprintJson.validators.find(
    (validator) => validator.title === ITEM_SEMANTIC_SPEND_TITLE,
  );
  if (itemSemantic === undefined) {
    throw new Error(
      `blueprint has no "${ITEM_SEMANTIC_SPEND_TITLE}" validator to probe`,
    );
  }
  return (itemSemantic.parameters ?? []).length === 2;
})();
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

const MAX_L1_TX_BYTES = MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;

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
const makeExactSizeOutputItem = makeMinAdaFundedExactSizeOutputItemV1;

const outputsForFieldTwoPreimageBytes = (
  targetBytes: number,
): readonly Buffer[] => {
  const payload = targetBytes - 7;
  const first = Math.floor(payload / 2);
  const outputs = [
    makeExactSizeOutputItem(first),
    makeExactSizeOutputItem(payload - first),
  ];
  const measured = encodeMidgardFieldPreimageV1(outputs).length;
  if (measured !== targetBytes) {
    throw new Error(
      `two-output field-2 envelope measured ${measured.toString()} bytes, wanted ${targetBytes.toString()}`,
    );
  }
  return outputs;
};

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
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
  const spentOutput = makeOutput(fundingLovelaceForOutputsV1(outputs));
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [...outputs],
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOpV1({
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

// ## Emulator harness — two wallets, three seeded threads

type Harness = {
  readonly emulator: Emulator;
  readonly proverLucid: LucidEvolution;
  readonly thirdPartyLucid: LucidEvolution;
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
  readonly threadUnit: string;
};

const walletAddress = (key: CML.PrivateKey): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  )
    .to_address()
    .to_bech32();

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

const sameDatumValue = (left: string, right: string): boolean =>
  left === right || Data.to(Data.from(left)) === Data.to(Data.from(right));

const submitAndAwait = async (
  lucid: LucidEvolution,
  unsigned: Awaited<
    ReturnType<ReturnType<LucidEvolution["newTx"]>["complete"]>
  >,
): Promise<{ readonly txHash: string; readonly signedCbor: string }> => {
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return { txHash, signedCbor };
};

const feeInputFor = async (
  lucid: LucidEvolution,
  threadUnit: string,
): Promise<UTxO> => {
  const candidates = (await lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[threadUnit] === undefined,
  );
  return candidates.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
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

// ## Stage submission, parameterised by the driving wallet

type StageContract = {
  readonly spendingScriptAddress: string;
  readonly spendingScript: Script;
};

const submitStage = async ({
  harness,
  driver,
  inputUtxo,
  inputContract,
  outputContract,
  outputDatum,
  label,
  encode,
  scriptReference,
  extraReferences,
}: {
  readonly harness: Harness;
  /** Who builds, funds, and signs — the prover or the third party. */
  readonly driver: { readonly lucid: LucidEvolution; readonly hash: string };
  readonly inputUtxo: UTxO;
  readonly inputContract: StageContract;
  readonly outputContract: StageContract;
  readonly outputDatum: string;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
    readonly referenceInputIndex: (target: UTxO) => bigint;
  }) => string;
  readonly scriptReference?: UTxO;
  readonly extraReferences?: readonly UTxO[];
}): Promise<{
  readonly nextThreadUtxo: UTxO;
  readonly signedBytes: number;
}> => {
  const makeRedeemer: BuildTxWithRedeemer = (ctx) =>
    encode({
      inputIndex: requireInputIndex(ctx, inputUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        (output) =>
          output.address === outputContract.spendingScriptAddress &&
          output.datum != null &&
          sameDatumValue(output.datum, outputDatum) &&
          output.assets[harness.threadUnit] === 1n,
        label,
      ),
      referenceInputIndex: (target) =>
        requireReferenceInputIndex(ctx, target, label),
    });
  let tx = driver.lucid
    .newTx()
    .collectFrom([await feeInputFor(driver.lucid, harness.threadUnit)])
    .collectFrom([inputUtxo], makeRedeemer);
  if (scriptReference !== undefined) {
    tx = tx.readFrom([scriptReference]);
  }
  if (extraReferences !== undefined && extraReferences.length > 0) {
    tx = tx.readFrom([...extraReferences]);
  }
  tx = tx.pay
    .ToContract(
      outputContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: inputUtxo.assets.lovelace ?? 0n,
        [harness.threadUnit]: 1n,
      },
    )
    .addSignerKey(driver.hash);
  if (scriptReference === undefined) {
    tx = tx.attach.SpendingValidator(inputContract.spendingScript);
  }
  let unsigned;
  try {
    unsigned = await tx.complete({ localUPLCEval: true });
  } catch (cause) {
    throw new Error(
      `${label} local evaluation failed: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
    );
  }
  const { txHash, signedCbor } = await submitAndAwait(driver.lucid, unsigned);
  const nextThreadUtxo = (
    await driver.lucid.utxosAt(outputContract.spendingScriptAddress)
  ).find(
    (utxo) => utxo.txHash === txHash && utxo.assets[harness.threadUnit] === 1n,
  );
  if (nextThreadUtxo === undefined) {
    throw new Error(`${label} did not hand the thread on`);
  }
  return { nextThreadUtxo, signedBytes: signedCbor.length / 2 };
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
      expect(selectMidgardFieldCarriageTierV1(fieldPreimage.length)).toBe(
        "Inline",
      );
      const argument = buildValidationOneStepArgumentV1({ trace, stateIndex });
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
        ValidationOneStepWitnessV1,
      ) as ValidationOneStepWitnessV1Data;

      // Option B: the committed evidence is `(transition, NoAuxiliaryWitness)`.
      const evidenceHash = validationOneStepEvidenceHashV1({
        transitionCbor: argument.transitionCbor,
        auxiliaryCbor: NO_AUXILIARY_WITNESS_CBOR,
      });
      // The retired two-part commitment over the same evidence — genuinely
      // different bytes, or the replay below would be vacuous.
      const retiredEvidenceHash = validationOneStepEvidenceHashV1({
        transitionCbor: argument.transitionCbor,
        auxiliaryCbor: argument.auxiliaryCbor,
      });
      expect(retiredEvidenceHash).not.toBe(evidenceHash);

      // A well-formed transition that is not the committed one: same work
      // witness, wrong claimed successor (the pre-state itself).
      const wrongTransitionCbor = encodeValidationOneStepWitnessCborV1({
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
        ValidationOneStepWitnessV1,
      ) as ValidationOneStepWitnessV1Data;

      const preState = validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      );
      const claimedSuccessorHash = hashMidgardValidationMachineStateV1(
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
          PreparedValidationResolutionDatumV1,
        );
      const preparedThreadDatum = preparedThreadDatumWith(evidenceHash);
      const retiredHashThreadDatum =
        preparedThreadDatumWith(retiredEvidenceHash);

      const preparedResolutionOf = (datum: string) => {
        const parsed = (
          Data.from(
            datum,
            PreparedValidationResolutionDatumV1,
          ) as PreparedValidationResolutionDatumV1Data
        ).data;
        if (parsed === null) {
          throw new Error("prepared thread datum is missing its state");
        }
        return parsed;
      };
      const stageData = deriveCanonicalDecodeItemStageDataV1({
        preparedResolution: preparedResolutionOf(preparedThreadDatum),
        transition,
        fieldPreimage: fieldPreimage.toString("hex"),
      });
      const authenticatedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.authenticated },
        AuthenticatedCanonicalDecodeItemDatumV1,
      );
      const preparedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.prepared },
        PreparedCanonicalDecodeItemDatumV1,
      );
      const observedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.observed },
        ObservedCanonicalDecodeItemDatumV1,
      );
      const verifiedDatum = Data.to(
        { fraud_prover: PROVER_HASH, data: stageData.verified },
        VerifiedCanonicalDecodeItemDatumV1,
      );
      // The hostile authenticate replays keep their own datums consistent
      // with their own redeemers, so the one disagreement each probe stages
      // is the one the validator is claimed to refuse.
      const retiredHashAuthenticatedDatum = Data.to(
        {
          fraud_prover: PROVER_HASH,
          data: {
            ...deriveCanonicalDecodeItemStageDataV1({
              preparedResolution: preparedResolutionOf(retiredHashThreadDatum),
              transition,
              fieldPreimage: fieldPreimage.toString("hex"),
            }).authenticated,
          },
        },
        AuthenticatedCanonicalDecodeItemDatumV1,
      );
      const wrongTransitionAuthenticatedDatum = Data.to(
        {
          fraud_prover: PROVER_HASH,
          data: deriveCanonicalDecodeItemStageDataV1({
            preparedResolution: preparedResolutionOf(preparedThreadDatum),
            transition: wrongTransition,
            fieldPreimage: fieldPreimage.toString("hex"),
          }).authenticated,
        },
        AuthenticatedCanonicalDecodeItemDatumV1,
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
          buildUnsignedValidationProofItemPublicationV1Program(
            harness.proverLucid,
            harness.contracts,
            publication as Parameters<
              typeof buildUnsignedValidationProofItemPublicationV1Program
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
        deriveValidationProofItemPublicationV1({
          transactionId: preState.transaction_id,
          transactionCommitment: preState.transaction_commitment,
          fieldPreimage: fieldPreimage.toString("hex"),
        }),
      );
      // Hostile: right bytes, wrong dispute — the commitment binding names a
      // different transaction commitment (§8.7's anti-fungibility pin).
      const wrongCommitmentPublication = await publishProofItem(
        deriveValidationProofItemPublicationV1({
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
        deriveValidationProofItemPublicationV1({
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
