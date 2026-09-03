/**
 * Shared real-contract emulator fixtures for the `invalid-signature` family
 * (Goal task `Q15`).
 *
 * The family is a two-real-step chain — `init` → `step-01` (bind the committed
 * transaction against the block's counted `transactions_root` and forward
 * `WitnessAnchor`) → `step-02` (open §2.5 field 7 through the §8.8 door and
 * finalize) — followed by fraudulent-block removal.
 *
 * Everything here builds *committed* material: the subject transaction's
 * address-witness list is the block's own field-7 preimage, so the fixture's
 * signatures are the ones the on-chain `verify_ed25519_signature` re-tests.
 * That is what makes the honest polarity expressible at all: an honest block
 * commits witnesses that genuinely sign the transaction id, and the only way to
 * accuse it is to bypass the submitter's local guard — which
 * {@link submitRawInvalidSignatureStep02} does, so the refusal comes from the
 * validator rather than from the builder.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  midgardFieldCarriageBounds,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  type Lucid,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening-v1.js";
import {
  requireFaultProofStepReferenceScript,
  resolveInvalidSignatureDeploymentContracts,
} from "../../src/runtime.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import {
  nativeTxFromCoreCompact,
  requireComputationThreadToken,
  selectFeeInput,
} from "../../src/submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts-v1.js";
import { setupFraudulentBlock } from "./submit-init-emulator-fixtures.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

/**
 * Bytes one address witness occupies in a §5.1 field preimage: the canonical
 * item is `[bytes(32), bytes(64)]` = 101 bytes, wrapped by §5.1 as a definite
 * byte string (`0x58 0x65` + 101) = 103. §5.3 fixes the stride, which is what
 * lets the on-chain `field_item_at` reach witness `n` by arithmetic.
 */
export const INVALID_SIGNATURE_ADDRESS_WITNESS_STRIDE = 103;

/**
 * First field-7 vector too large for tier 1, so §8.4 selects a tier-2 `RawUtxo`
 * publication on the preimage's own length. `- 3` is the §5.1 array header
 * allowance the sibling families use; at this count the header is two bytes, so
 * the constant is a floor rather than an exact fit and the resulting preimage
 * (14,422 B) clears the 14,336-byte tier-1 bound by a full item.
 */
export const INVALID_SIGNATURE_FIRST_RAW_WITNESS_COUNT =
  Math.floor(
    (midgardFieldCarriageBounds.maxTier1RedeemerPreimageBytes - 3) /
      INVALID_SIGNATURE_ADDRESS_WITNESS_STRIDE,
  ) + 1;

/**
 * A deterministic Ed25519 keypair. Fixtures are reproducible run to run, which
 * matters here because the committed signatures are the evidence under test.
 */
const witnessKeyPair = (
  index: number,
): {
  readonly verificationKey: string;
  readonly sign: (message: Buffer) => string;
} => {
  const seed = Buffer.alloc(32);
  seed.writeUInt32BE(index + 1, 28);
  const privateKey = CML.PrivateKey.from_normal_bytes(seed);
  return {
    verificationKey: Buffer.from(
      privateKey.to_public().to_raw_bytes(),
    ).toString("hex"),
    sign: (message) =>
      Buffer.from(privateKey.sign(message).to_raw_bytes()).toString("hex"),
  };
};

/** A witness whose signature genuinely verifies against `txId`. */
export const honestAddressWitness = ({
  index,
  txId,
}: {
  readonly index: number;
  readonly txId: string;
}): SDK.MidgardAddressWitness => {
  const key = witnessKeyPair(index);
  return {
    verification_key: key.verificationKey,
    signature: key.sign(Buffer.from(txId, "hex")),
  };
};

/**
 * A witness with a well-formed verification key and a signature that is not
 * one: exactly the shape a block commits when it violates the rule. The key is
 * a real Ed25519 point, so nothing but the signature check can refuse it.
 */
export const invalidAddressWitness = (
  index: number,
): SDK.MidgardAddressWitness => ({
  verification_key: witnessKeyPair(index).verificationKey,
  signature: Buffer.alloc(64, (index % 251) + 1).toString("hex"),
});

export type InvalidSignatureSubject = {
  readonly nativeTx: MidgardNativeTxFull;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly addrTxWits: readonly SDK.MidgardAddressWitness[];
  readonly witnessSetCompact: SDK.NativeTxWitnessSetCompact;
  readonly badAddrTxWitIndex: bigint;
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusion: SubmitStep01TxInclusion;
};

const nativeTxWithWitnesses = ({
  spendInputByte,
  fee,
  addrTxWits,
}: {
  readonly spendInputByte: string;
  readonly fee: bigint;
  readonly addrTxWits: readonly SDK.MidgardAddressWitness[];
}): MidgardNativeTxFull =>
  makeNativeTx({
    spendInputCbors: [
      Buffer.from(
        Data.to(
          { tx_id: spendInputByte.repeat(32), output_index: 0n } as never,
          Data.Object({
            tx_id: Data.Bytes({ minLength: 32, maxLength: 32 }),
            output_index: Data.Integer(),
          }) as never,
        ),
        "hex",
      ),
    ],
    fee,
    addrTxWitsPreimageCbor: SDK.encodeAddressWitnessPreimage(addrTxWits),
  });

/**
 * Commits one canonical native-V1 compact transaction as the sole leaf of a
 * block's raw transactions MPF and returns the step-01 inclusion evidence.
 */
export const buildInvalidSignatureBlockFixture = async (
  nativeTx: MidgardNativeTxFull,
): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly inclusion: SubmitStep01TxInclusion;
}> => {
  const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
  const l2TransactionSourceCbor = l2TransactionSourceCborV1(nativeTx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const proofCbor = proof.toCBOR().toString("hex");
  const transactionsRoot = trieRootHex(trie);
  return {
    transactionsRoot,
    l2TransactionCount: 1n,
    nativeTxId,
    nativeTxCompactCbor: compactCbor.toString("hex"),
    inclusion: {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proofCbor, SDK.Proof),
      txMembershipProofCbor: proofCbor,
    },
  };
};

const witnessSetCompactOf = (
  nativeTx: MidgardNativeTxFull,
): SDK.NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
};

/**
 * One committed transaction whose field-7 preimage is `decoyWitnessCount`
 * genuinely-signing witnesses plus one accused witness, in that order.
 *
 * `accused: "invalid"` builds the real fault: every decoy verifies, and the sole
 * violation is the witness the proof accuses. `accused: "honest"` builds the
 * adversarial subject — a wholly honest commitment whose accused witness signs
 * the transaction correctly.
 *
 * §3's transaction-id preimage is the body alone, so the id is fixed before any
 * witness exists: the builder derives it from a witness-free twin of the same
 * body, signs *that*, and asserts the populated transaction re-derives to it.
 */
export const buildInvalidSignatureSubject = async ({
  accused,
  decoyWitnessCount = 0,
  spendInputByte = "55",
  fee = 13n,
}: {
  readonly accused: "invalid" | "honest";
  readonly decoyWitnessCount?: number;
  readonly spendInputByte?: string;
  readonly fee?: bigint;
}): Promise<InvalidSignatureSubject> => {
  const bodyOnly = nativeTxWithWitnesses({
    spendInputByte,
    fee,
    addrTxWits: [],
  });
  const nativeTxId = computeMidgardNativeTxId(bodyOnly).toString("hex");
  const accusedIndex = decoyWitnessCount;
  const addrTxWits: readonly SDK.MidgardAddressWitness[] = [
    ...Array.from({ length: decoyWitnessCount }, (_unused, index) =>
      honestAddressWitness({ index, txId: nativeTxId }),
    ),
    accused === "honest"
      ? honestAddressWitness({ index: accusedIndex, txId: nativeTxId })
      : invalidAddressWitness(accusedIndex),
  ];
  const nativeTx = nativeTxWithWitnesses({
    spendInputByte,
    fee,
    addrTxWits,
  });
  const fixture = await buildInvalidSignatureBlockFixture(nativeTx);
  if (fixture.nativeTxId !== nativeTxId) {
    throw new Error(
      "address witnesses moved the native transaction id; §3's id preimage should be the body alone",
    );
  }
  return {
    nativeTx,
    nativeTxId: fixture.nativeTxId,
    nativeTxCompactCbor: fixture.nativeTxCompactCbor,
    addrTxWits,
    witnessSetCompact: witnessSetCompactOf(nativeTx),
    badAddrTxWitIndex: BigInt(accusedIndex),
    transactionsRoot: fixture.transactionsRoot,
    l2TransactionCount: fixture.l2TransactionCount,
    inclusion: fixture.inclusion,
  };
};

export const makeInvalidSignatureEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realInvalidSignature: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.fraudProofContracts.invalidSignature;
  const category = harness.catalogue.categories.invalidSignature;
  if (category === undefined) {
    throw new Error("invalid-signature harness category was omitted");
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.invalidSignature
  ) {
    throw new Error("unexpected invalid-signature category id");
  }
  return { ...harness, family, category };
};

export type InvalidSignatureEmulatorHarness = Awaited<
  ReturnType<typeof makeInvalidSignatureEmulatorHarness>
>;

/**
 * Publishes both family steps as plain reference-script UTxOs. Standing owner
 * ruling: a fault proof sources every script witness from a published reference
 * script, never from an inline attachment.
 */
export const publishInvalidSignatureReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: InvalidSignatureEmulatorHarness["family"];
}): Promise<readonly [UTxO, UTxO]> => {
  const publications: UTxO[] = [];
  // Sequential: each publication spends UTxOs the next one selects from.
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript,
      label: `invalid-signature step-0${(index + 1).toString()}`,
    });
    publications.push(utxo);
  }
  const [step01, step02] = publications;
  if (step01 === undefined || step02 === undefined) {
    throw new Error("invalid-signature reference-script publication is short");
  }
  return [step01, step02];
};

export const setupInvalidSignatureScenario = async ({
  harness,
  subject,
}: {
  readonly harness: InvalidSignatureEmulatorHarness;
  readonly subject: InvalidSignatureSubject;
}): Promise<Awaited<ReturnType<typeof setupFraudulentBlock>>> =>
  await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture: {
      transactionsRoot: subject.transactionsRoot,
      l2TransactionCount: subject.l2TransactionCount,
    },
  });

/**
 * Raw guard-bypassing finalizer for the adversarial polarity.
 *
 * It reproduces `submitInvalidSignatureStep02`'s transaction shape exactly —
 * the same §8.8 opening, the same complete reference-input set, the same burn /
 * mint / output layout — and omits one thing: the local
 * `verifyAddressWitness(...)` refusal. An honest witness therefore reaches
 * step-02's on-chain `verify_ed25519_signature(...) == False`, which is the
 * check that must refuse the attack.
 */
export const submitRawInvalidSignatureStep02 = async ({
  harness,
  deploymentInfo,
  threadOutRef,
  subject,
  referenceScriptUtxo,
  badAddrTxWitIndex,
}: {
  readonly harness: InvalidSignatureEmulatorHarness;
  readonly deploymentInfo: unknown;
  readonly threadOutRef: string;
  readonly subject: InvalidSignatureSubject;
  readonly referenceScriptUtxo: UTxO;
  readonly badAddrTxWitIndex?: bigint;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const { invalidSignatureCategory, contracts } =
    await resolveInvalidSignatureDeploymentContracts({
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const [txHashPart, outputIndexPart] = threadOutRef.split("#");
  if (txHashPart === undefined || outputIndexPart === undefined) {
    throw new Error(`Malformed thread out-ref ${threadOutRef}`);
  }
  const [threadUtxo] = await lucid.utxosByOutRef([
    { txHash: txHashPart, outputIndex: Number(outputIndexPart) },
  ]);
  if (threadUtxo === undefined) {
    throw new Error(`Raw step-02 thread UTxO ${threadOutRef} is not live`);
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: invalidSignatureCategory.categoryId,
    categoryLabel: "raw invalid-signature",
  });
  if (threadUtxo.datum == null) {
    throw new Error("Raw step-02 thread UTxO carries no datum");
  }
  const datum = Data.from(threadUtxo.datum, SDK.InvalidSignatureStep02Datum);
  if (datum.data === null) {
    throw new Error("Raw step-02 thread UTxO carries no step state");
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: SDK.MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: datum.data.bad_tx_id,
    nativeTxCompactCbor: subject.nativeTxCompactCbor,
    itemCbors: subject.addrTxWits.map(SDK.encodeMidgardAddressWitnessCanonical),
    owner: signer.paymentKeyHash,
    witnessSet: subject.witnessSetCompact,
    anchorWitnessSetHash: datum.data.bad_tx_witness_set_hash,
    label: "Raw invalid-signature step 02",
  });
  signer.selectWallet(lucid);
  const published = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Raw invalid-signature step 02 field",
  });
  const stepReference = requireFaultProofStepReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.invalidSignature.steps[1].spendingScriptHash,
    label: "raw invalid-signature step 02",
  });
  const referenceInputs = [...published, stepReference];
  const addrTxWitsOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    label: "Raw invalid-signature step 02",
  });
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spend = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(
              ctx,
              threadUtxo,
              "raw invalid-signature step 02",
            ),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw invalid-signature step 02 fraud-proof",
            ),
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw invalid-signature step 02 fraud-proof",
            ),
            addr_tx_wits_opening: addrTxWitsOpening,
            bad_addr_tx_wit_index:
              badAddrTxWitIndex ?? subject.badAddrTxWitIndex,
          },
        ],
      },
      SDK.InvalidSignatureStep02SpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw invalid-signature step 02 thread burn",
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
          contracts.computationThread.policyId,
          "raw invalid-signature step 02 thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;

  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw invalid-signature step 02 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: "raw invalid-signature step 02 fraud-proof mint",
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .readFrom([
      ...referenceInputs,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [fraudProofUnit]: 1n }, mint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
