/** Shared real-contract emulator fixtures for the missing-signature family. */
import {
  deriveMidgardNativeTxWitnessSetCompactV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxWitnessSetCompactV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCarriageBoundsV1,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";

import {
  faultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../../src/field-opening-v1.js";
import type {
  MissingSignatureFindingV1,
  MissingSignatureProverDepsV1,
  MissingSignatureProverEventV1,
  MissingSignatureProverPolicyV1,
} from "../../src/missing-signature/index.js";
import {
  MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS_V1,
  MissingSignatureProvabilityV1,
} from "../../src/missing-signature/index.js";
import {
  planMissingSignatureAddressWitnessesOpeningV1,
  requireMissingSignatureStepStateV1,
  requireMissingSignatureThreadUtxoV1,
} from "../../src/missing-signature/index.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import { selectFeeInput } from "../../src/submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import {
  buildDecodingBlockFixtureV1,
  type DecodingBlockFixtureV1,
} from "./native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const MISSING_SIGNATURE_TARGET_VKEY_V1 = "11".repeat(32);
export const MISSING_SIGNATURE_TARGET_HASH_V1 = SDK.missingSignatureVkeyHashV1(
  MISSING_SIGNATURE_TARGET_VKEY_V1,
);

/** First 103-byte-stride field-7 vector that crosses the tier-2 ceiling. */
export const MISSING_SIGNATURE_FIRST_CERTIFIED_WITNESS_COUNT_V1 =
  Math.floor(
    (midgardFieldCarriageBoundsV1.maxPublishableCarriageBytes - 3) / 103,
  ) + 1;

/** First field-7 vector that is too large for tier 1 and must publish. */
export const MISSING_SIGNATURE_FIRST_RAW_WITNESS_COUNT_V1 =
  Math.floor(
    (midgardFieldCarriageBoundsV1.maxTier1RedeemerPreimageBytes - 3) / 103,
  ) + 1;

/** Widest canonical field-7 vector admitted by the 32,768-byte field cap. */
export const MISSING_SIGNATURE_MAX_ADMISSIBLE_WITNESS_COUNT_V1 = Math.floor(
  (midgardFieldCarriageBoundsV1.maxTransactionAggregateFieldBytes - 3) / 103,
);

const decoyWitness = (index: number): SDK.MidgardAddressWitness => {
  const vkey = Buffer.alloc(32);
  vkey.writeUInt32BE(index + 1, 28);
  return {
    verification_key: vkey.toString("hex"),
    signature: Buffer.alloc(64, (index % 254) + 1).toString("hex"),
  };
};

export const buildMissingSignatureSubjectV1 = ({
  honest = false,
  decoyWitnessCount = 0,
}: {
  readonly honest?: boolean;
  readonly decoyWitnessCount?: number;
} = {}): {
  readonly nativeTx: MidgardNativeTxFullV1;
  readonly requiredSignerHashes: readonly string[];
  readonly addrTxWits: readonly SDK.MidgardAddressWitness[];
  readonly witnessSetCompact: SDK.NativeTxWitnessSetCompact;
} => {
  const addrTxWits: SDK.MidgardAddressWitness[] = [
    ...Array.from({ length: decoyWitnessCount }, (_unused, index) =>
      decoyWitness(index),
    ),
    ...(honest
      ? [
          {
            verification_key: MISSING_SIGNATURE_TARGET_VKEY_V1,
            signature: "ff".repeat(64),
          },
        ]
      : []),
  ];
  const nativeTx = materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: encodeCbor([
        Buffer.from(MISSING_SIGNATURE_TARGET_HASH_V1, "hex"),
      ]),
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: SDK.encodeAddressWitnessPreimage(addrTxWits),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet);
  return {
    nativeTx,
    requiredSignerHashes: [MISSING_SIGNATURE_TARGET_HASH_V1],
    addrTxWits,
    witnessSetCompact: {
      addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
    },
  };
};

export const makeMissingSignatureEmulatorHarnessV1 = async ({
  useScalusEvaluator = true,
}: {
  /**
   * Scalus avoids the Aiken/WASM arena leak on the multi-scan frontier. Small
   * adversarial tests may opt out so the legacy shared refusal guard observes
   * the emulator's canonical `failed script execution` submission error.
   */
  readonly useScalusEvaluator?: boolean;
} = {}) => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realMissingSignature: true,
      alwaysFraudProofCatalogue: true,
    },
    // The default Aiken/WASM adapter grows one unreclaimable linear-memory
    // arena per evaluation. The maximum 318-witness lifecycle necessarily
    // evaluates ten step-04 transactions, so use the repository's production
    // Scalus adapter for Lucid's preflight; Emulator still independently runs
    // every submitted script through phase two.
    ...(useScalusEvaluator
      ? { lucidOptions: { evaluator: createScalusEvaluator() } }
      : {}),
  });
  const missingSignature = harness.contracts.missingSignature;
  const category = harness.catalogue.categories.missingSignature;
  if (missingSignature === undefined || category === undefined) {
    throw new Error(
      "missing-signature harness contracts/category were omitted",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingSignature
  ) {
    throw new Error("unexpected missing-signature category id");
  }
  return { ...harness, missingSignature, category };
};

export const publishMissingSignatureReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarnessV1>
  >["missingSignature"];
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const publications: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `missing-signature step-0${(index + 1).toString()}`,
    });
    publications.push(utxo);
  }
  return publications as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};

export type MissingSignatureScenarioV1 = {
  readonly subject: ReturnType<typeof buildMissingSignatureSubjectV1>;
  readonly block: DecodingBlockFixtureV1;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
};

export const setupMissingSignatureScenarioV1 = async ({
  harness,
  honest = false,
  decoyWitnessCount = 0,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarnessV1>
  >;
  readonly honest?: boolean;
  readonly decoyWitnessCount?: number;
}): Promise<MissingSignatureScenarioV1> => {
  const subject = buildMissingSignatureSubjectV1({
    honest,
    decoyWitnessCount,
  });
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixtureV1({
    operatorVkey,
    startTime,
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject: { kind: "normal", nativeTx: subject.nativeTx },
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  return { subject, block, setup };
};

export const missingSignatureFindingV1 = (
  scenario: MissingSignatureScenarioV1,
): MissingSignatureFindingV1 => ({
  headerHash: scenario.setup.headerHash,
  eventKey: {
    L2TransactionEventKey: { tx_id: scenario.block.nativeTxId },
  },
  fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  txId: scenario.block.nativeTxId,
  nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
  accusedRequiredSignerIndex: 0n,
  accusedRequiredSignerHash: MISSING_SIGNATURE_TARGET_HASH_V1,
  resolvedVkey: MISSING_SIGNATURE_TARGET_VKEY_V1,
  committedWitnessSetHash:
    scenario.subject.nativeTx.compact.transactionWitnessSetHash.toString("hex"),
  provability: MissingSignatureProvabilityV1.MissingWitness,
  estimatedThreadTxCount:
    5 +
    Math.floor(
      Math.max(0, scenario.subject.addrTxWits.length - 1) /
        SDK.MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE_V1,
    ),
});

export const MISSING_SIGNATURE_EMULATOR_PROVER_POLICY_V1: MissingSignatureProverPolicyV1 =
  {
    ...MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS_V1,
    minSettlementDepth: 0n,
    maxThreadBudgetLovelace: null,
  };

export const missingSignatureProverDepsV1 = ({
  harness,
  scenario,
  referenceScriptUtxos,
  journal,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarnessV1>
  >;
  readonly scenario: MissingSignatureScenarioV1;
  readonly referenceScriptUtxos: MissingSignatureProverDepsV1["referenceScriptUtxos"];
  readonly journal?: (event: MissingSignatureProverEventV1) => void;
}): MissingSignatureProverDepsV1 => ({
  lucid: harness.proverLucid,
  blueprint: harness.realBlueprint,
  network,
  contracts: harness.missingSignature,
  category: harness.category,
  catalogue: {
    policyId: harness.contracts.fraudProofCatalogue.policyId,
    spendingScriptAddress:
      harness.contracts.fraudProofCatalogue.spendingScriptAddress,
    root: harness.catalogue.root,
  },
  signer: harness.proverSigner,
  evidence: {
    txInclusion: async () => {
      if (scenario.block.txInclusion === null) {
        throw new Error("normal missing-signature fixture has no inclusion");
      }
      return scenario.block.txInclusion;
    },
    subjectTx: async () => ({
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      requiredSignerHashes: scenario.subject.requiredSignerHashes,
      addrTxWits: scenario.subject.addrTxWits,
      witnessSetCompact: scenario.subject.witnessSetCompact,
    }),
  },
  observations: {},
  journal: journal ?? (() => undefined),
  policy: MISSING_SIGNATURE_EMULATOR_PROVER_POLICY_V1,
  referenceScriptUtxos,
});

/** Publish and mint the §8.6 material for a genuinely tier-3 field-7 proof. */
export const publishMissingSignatureField07CertificateV1 = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarnessV1>
  >;
  readonly scenario: MissingSignatureScenarioV1;
}): Promise<UTxO> => {
  const planned = planMissingSignatureAddressWitnessesOpeningV1({
    anchorTxId: scenario.block.nativeTxId,
    nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
    addrTxWits: scenario.subject.addrTxWits,
    witnessSet: scenario.subject.witnessSetCompact,
    anchorWitnessSetHash:
      scenario.subject.nativeTx.compact.transactionWitnessSetHash.toString(
        "hex",
      ),
    owner: harness.proverSigner.paymentKeyHash,
  });
  if (planned.plan.tier !== "Certified") {
    throw new Error(
      `fat missing-signature fixture selected ${planned.plan.tier}, not Certified`,
    );
  }
  harness.proverSigner.selectWallet(harness.proverLucid);
  const chunkUtxos = await publishFaultProofFieldCarriageV1({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.address,
    label: "missing-signature tier-3 field-7",
  });
  const certificate = harness.contracts.fieldPreimageCertificate;
  const witnessSetCompactCbor = encodeMidgardNativeTxWitnessSetCompactV1({
    addrTxWitsHash: Buffer.from(
      scenario.subject.witnessSetCompact.addr_tx_wits_hash,
      "hex",
    ),
    scriptTxWitsHash: Buffer.from(
      scenario.subject.witnessSetCompact.script_tx_wits_hash,
      "hex",
    ),
    redeemerTxWitsHash: Buffer.from(
      scenario.subject.witnessSetCompact.redeemer_tx_wits_hash,
      "hex",
    ),
  }).toString("hex");
  const unsigned = await Effect.runPromise(
    SDK.buildUnsignedFieldPreimageCertificationV1Program(harness.proverLucid, {
      plan: planned.plan,
      certificatePolicyId: certificate.policyId,
      certificateAddress: certificate.spendingScriptAddress,
      certificateScript: certificate.mintingScript,
      chunkUtxos,
      compactCbor: scenario.block.nativeTxCompactCbor,
      witnessSetCompactCbor,
    }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  const certificateUtxo = (
    await harness.proverLucid.utxosAt(certificate.spendingScriptAddress)
  ).find((utxo) => utxo.txHash === txHash);
  if (certificateUtxo === undefined) {
    throw new Error("missing-signature §8.6 certificate output was not found");
  }
  return certificateUtxo;
};

/**
 * Raw guard-bypassing finalizer for the adversarial polarity. It duplicates
 * the production transaction shape but deliberately omits the local absence
 * check, so an honest witness reaches step-04's on-chain fold.
 */
export const submitRawMissingSignatureStep04V1 = async ({
  harness,
  threadOutRef,
  scenario,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarnessV1>
  >;
  readonly threadOutRef: string;
  readonly scenario: MissingSignatureScenarioV1;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } = await requireMissingSignatureThreadUtxoV1(
    {
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      stepIndex: 3,
      threadOutRef,
    },
  );
  const state = requireMissingSignatureStepStateV1({
    threadUtxo,
    signer: harness.proverSigner,
    schema: SDK.MissingSignatureStep04Datum,
    stepIndex: 3,
  });
  const planned = planMissingSignatureAddressWitnessesOpeningV1({
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
    addrTxWits: scenario.subject.addrTxWits,
    witnessSet: scenario.subject.witnessSetCompact,
    anchorWitnessSetHash: state.verified_witness_set_hash,
    owner: harness.proverSigner.paymentKeyHash,
  });
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.address,
    label: "raw missing-signature step-04",
  });
  const opening = faultProofFieldOpeningV1({
    planned,
    referenceInputs: carriageUtxos,
    certificatePolicyId:
      harness.missingSignature.fieldPreimageCertificatePolicyId,
    label: "raw missing-signature step-04",
  });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const walletUtxos = await harness.proverLucid.wallet().getUtxos();
  const candidates = carriageUtxos.reduce<readonly UTxO[]>(
    (utxos, carriage) => excludeUtxo(utxos, carriage),
    walletUtxos,
  );
  const feeInput = selectFeeInput(candidates);
  const proofUnit = toUnit(
    harness.missingSignature.fraudProof.policyId,
    threadToken.assetName,
  );
  const proofDatum = Data.to(
    { fraud_prover: harness.proverSigner.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: harness.missingSignature.fraudProof.spendingScriptAddress,
    datum: proofDatum,
    unit: proofUnit,
  });
  const spend = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            Finalize: {
              input_index: SDK.requireInputIndex(
                ctx,
                threadUtxo,
                "raw missing-signature step-04",
              ),
              output_index: SDK.requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                "raw missing-signature fraud-proof output",
              ),
              fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
                ctx,
                harness.missingSignature.fraudProof.policyId,
                "raw missing-signature fraud-proof mint",
              ),
              addr_tx_wits_opening: opening,
              checkpoint_cbor: null,
            },
          },
        ],
      },
      SDK.MissingSignatureStep04SpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      harness.missingSignature.computationThread.policyId,
      "raw missing-signature thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mint = ((ctx) =>
    Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          harness.missingSignature.computationThread.policyId,
          "raw missing-signature thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const unsigned = await harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .readFrom([referenceScriptUtxo, ...carriageUtxos])
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [proofUnit]: 1n }, mint)
    .pay.ToContract(
      harness.missingSignature.fraudProof.spendingScriptAddress,
      { kind: "inline", value: proofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [proofUnit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash)
    .attach.MintingPolicy(
      harness.missingSignature.computationThread.mintingScript,
    )
    .attach.MintingPolicy(harness.missingSignature.fraudProof.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};
