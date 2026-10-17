import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  midgardFieldCarriageBounds,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type Lucid, type UTxO } from "@lucid-evolution/lucid";

import type { SubmitStep01TxInclusion } from "../../src/step-support.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import { setupFraudulentBlock } from "./submit-init-emulator-fixtures.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
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
  readonly spendInputByte: string | null;
  readonly fee: bigint;
  readonly addrTxWits: readonly SDK.MidgardAddressWitness[];
}): MidgardNativeTxFull =>
  makeNativeTx({
    spendInputCbors:
      spendInputByte === null
        ? []
        : [
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
  readonly spendInputByte?: string | null;
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
