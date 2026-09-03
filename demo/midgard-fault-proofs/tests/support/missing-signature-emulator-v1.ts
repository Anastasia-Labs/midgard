/** Shared real-contract emulator fixtures for the missing-signature family. */
import {
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCarriageBounds,
  type MidgardNativeTxFull,
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
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening-v1.js";
import type {
  MissingSignatureFinding,
  MissingSignatureProverDeps,
  MissingSignatureProverEvent,
  MissingSignatureProverPolicy,
} from "../../src/missing-signature/index.js";
import {
  MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS,
  MissingSignatureProvability,
} from "../../src/missing-signature/index.js";
import {
  planMissingSignatureAddressWitnessesOpening,
  requireMissingSignatureStepState,
  requireMissingSignatureThreadUtxo,
} from "../../src/missing-signature/index.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import { selectFeeInput } from "../../src/submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts-v1.js";
import {
  buildDecodingBlockFixture,
  type DecodingBlockFixture,
} from "./native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const MISSING_SIGNATURE_TARGET_VKEY = "11".repeat(32);
export const MISSING_SIGNATURE_TARGET_HASH = SDK.missingSignatureVkeyHash(
  MISSING_SIGNATURE_TARGET_VKEY,
);

/** First 103-byte-stride field-7 vector that crosses the tier-2 ceiling. */
export const MISSING_SIGNATURE_FIRST_CERTIFIED_WITNESS_COUNT =
  Math.floor(
    (midgardFieldCarriageBounds.maxPublishableCarriageBytes - 3) / 103,
  ) + 1;

/** First field-7 vector that is too large for tier 1 and must publish. */
export const MISSING_SIGNATURE_FIRST_RAW_WITNESS_COUNT =
  Math.floor(
    (midgardFieldCarriageBounds.maxTier1RedeemerPreimageBytes - 3) / 103,
  ) + 1;

/** Widest canonical field-7 vector admitted by the 32,768-byte field cap. */
export const MISSING_SIGNATURE_MAX_ADMISSIBLE_WITNESS_COUNT = Math.floor(
  (midgardFieldCarriageBounds.maxTransactionAggregateFieldBytes - 3) / 103,
);

const decoyWitness = (index: number): SDK.MidgardAddressWitness => {
  const vkey = Buffer.alloc(32);
  vkey.writeUInt32BE(index + 1, 28);
  return {
    verification_key: vkey.toString("hex"),
    signature: Buffer.alloc(64, (index % 254) + 1).toString("hex"),
  };
};

export const buildMissingSignatureSubject = ({
  honest = false,
  decoyWitnessCount = 0,
}: {
  readonly honest?: boolean;
  readonly decoyWitnessCount?: number;
} = {}): {
  readonly nativeTx: MidgardNativeTxFull;
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
            verification_key: MISSING_SIGNATURE_TARGET_VKEY,
            signature: "ff".repeat(64),
          },
        ]
      : []),
  ];
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: encodeCbor([
        Buffer.from(MISSING_SIGNATURE_TARGET_HASH, "hex"),
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
  const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    nativeTx,
    requiredSignerHashes: [MISSING_SIGNATURE_TARGET_HASH],
    addrTxWits,
    witnessSetCompact: {
      addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
    },
  };
};

export const makeMissingSignatureEmulatorHarness = async ({
  useScalusEvaluator = true,
}: {
  /**
   * Scalus avoids the Aiken/WASM arena leak on the multi-scan frontier. Small
   * adversarial tests may opt out so the legacy shared refusal guard observes
   * the emulator's canonical `failed script execution` submission error.
   */
  readonly useScalusEvaluator?: boolean;
} = {}) => {
  const harness = await makeFaultProofEmulatorHarness({
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

export const publishMissingSignatureReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
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

export type MissingSignatureScenario = {
  readonly subject: ReturnType<typeof buildMissingSignatureSubject>;
  readonly block: DecodingBlockFixture;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
};

export const setupMissingSignatureScenario = async ({
  harness,
  honest = false,
  decoyWitnessCount = 0,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly honest?: boolean;
  readonly decoyWitnessCount?: number;
}): Promise<MissingSignatureScenario> => {
  const subject = buildMissingSignatureSubject({
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
  const block = await buildDecodingBlockFixture({
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

export const missingSignatureFinding = (
  scenario: MissingSignatureScenario,
): MissingSignatureFinding => ({
  headerHash: scenario.setup.headerHash,
  eventKey: {
    L2TransactionEventKey: { tx_id: scenario.block.nativeTxId },
  },
  fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  txId: scenario.block.nativeTxId,
  nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
  accusedRequiredSignerIndex: 0n,
  accusedRequiredSignerHash: MISSING_SIGNATURE_TARGET_HASH,
  resolvedVkey: MISSING_SIGNATURE_TARGET_VKEY,
  committedWitnessSetHash:
    scenario.subject.nativeTx.compact.transactionWitnessSetHash.toString("hex"),
  provability: MissingSignatureProvability.MissingWitness,
  estimatedThreadTxCount:
    5 +
    Math.floor(
      Math.max(0, scenario.subject.addrTxWits.length - 1) /
        SDK.MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
    ),
});

export const MISSING_SIGNATURE_EMULATOR_PROVER_POLICY: MissingSignatureProverPolicy =
  {
    ...MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS,
    minSettlementDepth: 0n,
    maxThreadBudgetLovelace: null,
  };

export const missingSignatureProverDeps = ({
  harness,
  scenario,
  referenceScriptUtxos,
  journal,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly scenario: MissingSignatureScenario;
  readonly referenceScriptUtxos: MissingSignatureProverDeps["referenceScriptUtxos"];
  readonly journal?: (event: MissingSignatureProverEvent) => void;
}): MissingSignatureProverDeps => ({
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
  policy: MISSING_SIGNATURE_EMULATOR_PROVER_POLICY,
  referenceScriptUtxos,
  witnessReferenceScripts: harness.witnessReferenceScripts,
});

/** Publish and mint the §8.6 material for a genuinely tier-3 field-7 proof. */
export const publishMissingSignatureField07Certificate = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly scenario: MissingSignatureScenario;
}): Promise<UTxO> => {
  const planned = planMissingSignatureAddressWitnessesOpening({
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
  const chunkUtxos = await publishFaultProofFieldCarriage({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.address,
    label: "missing-signature tier-3 field-7",
  });
  const certificate = harness.contracts.fieldPreimageCertificate;
  const witnessSetCompactCbor = encodeMidgardNativeTxWitnessSetCompact({
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
    SDK.buildUnsignedFieldPreimageCertificationProgram(harness.proverLucid, {
      plan: planned.plan,
      certificatePolicyId: certificate.policyId,
      certificateAddress: certificate.spendingScriptAddress,
      certificateWitness: {
        kind: "inline_emulator_only",
        certificateScript: certificate.mintingScript,
      },
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
export const submitRawMissingSignatureStep04 = async ({
  harness,
  threadOutRef,
  scenario,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly scenario: MissingSignatureScenario;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } = await requireMissingSignatureThreadUtxo({
    lucid: harness.proverLucid,
    contracts: harness.missingSignature,
    categoryId: harness.category.categoryId,
    stepIndex: 3,
    threadOutRef,
  });
  const state = requireMissingSignatureStepState({
    threadUtxo,
    signer: harness.proverSigner,
    schema: SDK.MissingSignatureStep04Datum,
    stepIndex: 3,
  });
  const planned = planMissingSignatureAddressWitnessesOpening({
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
    addrTxWits: scenario.subject.addrTxWits,
    witnessSet: scenario.subject.witnessSetCompact,
    anchorWitnessSetHash: state.verified_witness_set_hash,
    owner: harness.proverSigner.paymentKeyHash,
  });
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.address,
    label: "raw missing-signature step-04",
  });
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: harness.missingSignature.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw missing-signature step-04 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: harness.missingSignature.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: "raw missing-signature step-04 fraud-proof mint",
  });
  const referenceInputs = [
    referenceScriptUtxo,
    ...computationThreadCarriage.referenceInputs,
    ...fraudProofCarriage.referenceInputs,
    ...carriageUtxos,
  ];
  const opening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId:
      harness.missingSignature.fieldPreimageCertificatePolicyId,
    label: "raw missing-signature step-04",
  });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const walletUtxos = await harness.proverLucid.wallet().getUtxos();
  const candidates = [...carriageUtxos].reduce<readonly UTxO[]>(
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
  const base = harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .readFrom(referenceInputs)
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
    .addSignerKey(harness.proverSigner.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};
